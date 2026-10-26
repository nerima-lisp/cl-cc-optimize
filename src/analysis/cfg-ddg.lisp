(in-package :cl-cc/optimize)

;;; ─── CFG Data Dependence Graph (DDG) ─────────────────────────────────────
;;; FR-200 dependency: software pipelining needs loop-body dependence edges and
;;; a conservative minimum initiation interval (MII) estimate. CFG construction
;;; is in cfg.lisp (loads before this file).

(defstruct (opt-ddg-node (:conc-name opt-ddg-node-))
  "Node in a loop-body data dependence graph.  DISTANCE is stored on edges, so
   loop-carried dependencies can contribute to recurrence MII."
  index
  inst
  (latency 1 :type fixnum)
  (predecessors nil :type list)
  (successors nil :type list))

(defun cfg-ddg-add-edge (nodes from to &key (distance 0))
  "Add a DDG edge FROM → TO over NODES with loop-carried DISTANCE metadata."
  (let ((edge (list :from from :to to :distance distance)))
    (pushnew edge (opt-ddg-node-successors (aref nodes from)) :test #'equal)
    (pushnew edge (opt-ddg-node-predecessors (aref nodes to)) :test #'equal)
    edge))

(defun cfg-build-ddg (instructions &key (latency-fn (lambda (_inst) (declare (ignore _inst)) 1)))
  "Build a conservative DDG for a straight-line loop body.

Edges encode RAW, WAR, and WAW register hazards in program order.  If the last
definition of a register in the body feeds an earlier use/definition in the next
iteration, a loop-carried edge with DISTANCE=1 is added for recurrence-MII."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (nodes (make-array n)))
    (loop for i from 0 below n
          do (setf (aref nodes i)
                   (make-opt-ddg-node :index i
                                      :inst (aref vec i)
                                      :latency (max 1 (funcall latency-fn (aref vec i))))))
    (loop for i from 0 below n do
      (loop for j from (1+ i) below n
            for earlier = (aref vec i)
            for later = (aref vec j)
            for earlier-reads = (opt-inst-read-regs earlier)
            for earlier-writes = (%opt-inst-write-regs earlier)
            for later-reads = (opt-inst-read-regs later)
            for later-writes = (%opt-inst-write-regs later)
            when (or (%opt-reg-intersect-p earlier-writes later-reads)
                     (%opt-reg-intersect-p earlier-reads later-writes)
                     (%opt-reg-intersect-p earlier-writes later-writes))
              do (cfg-ddg-add-edge nodes i j :distance 0)))
    (loop for i from 0 below n do
      (loop for j from 0 below i
            for later = (aref vec i)
            for earlier-next = (aref vec j)
            when (or (%opt-reg-intersect-p (%opt-inst-write-regs later)
                                           (opt-inst-read-regs earlier-next))
                     (%opt-reg-intersect-p (%opt-inst-write-regs later)
                                           (%opt-inst-write-regs earlier-next)))
              do (cfg-ddg-add-edge nodes i j :distance 1)))
    nodes))

(defun cfg-ddg-edges (nodes)
  "Return all unique DDG edges in NODES."
  (remove-duplicates
   (loop for node across nodes append (opt-ddg-node-successors node))
   :test #'equal))

(defun cfg-compute-mii (nodes &key (issue-width 1))
  "Compute a conservative minimum initiation interval for DDG NODES.

MII = max(resource MII, recurrence MII).  Resource MII is the total latency per
iteration divided by ISSUE-WIDTH.  Recurrence MII considers explicit loop-carried
edges and rounds producer latency / dependence distance upward."
  (let* ((resource-mii (max 1 (ceiling (loop for node across nodes
                                             sum (opt-ddg-node-latency node))
                                        (max 1 issue-width))))
         (recurrence-mii
           (loop for edge in (cfg-ddg-edges nodes)
                 for distance = (getf edge :distance)
                 when (and distance (plusp distance))
                   maximize (ceiling (opt-ddg-node-latency
                                      (aref nodes (getf edge :from)))
                                     distance)
                     into best
                 finally (return (or best 1)))))
    (max resource-mii recurrence-mii)))
