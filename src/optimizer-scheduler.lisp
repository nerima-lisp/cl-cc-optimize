(in-package :cl-cc/optimize)

(defun %opt-inst-latency (inst)
  "Return the estimated latency of INST in cycles."
  (or (gethash (type-of inst) *opt-vm-instruction-latencies*) 1))

(defun %opt-scheduler-barrier-p (inst)
  "T when INST must keep its position in local scheduling."
  (or
    (member
      (type-of inst)
      (quote
        (vm-call
          vm-apply
          vm-generic-call
          vm-signal
          vm-signal-error
          vm-error-instruction
          vm-cerror
          vm-warn))
      :test
      (function eq))
    (and
      (not
        (member
          (vm-inst-effect-kind inst)
          (quote (:pure :read-only))
          :test
          (function eq)))
      (not
        (and
          (opt-memory-write-inst-p inst)
          (not (opt-memory-unknown-write-inst-p inst)))))))

(defun %opt-inst-write-regs (inst)
  "Return registers written by INST."
  (let ((dst (opt-inst-dst inst)))
    (and dst (list dst))))

(defun %opt-reg-intersect-p (a b)
  "T when register lists A and B intersect by EQ."
  (and a b (intersection a b :test #'eq)))

(defun %opt-scheduler-depends-p (earlier later metadata)
  "T when EARLIER must precede LATER due to register or TBAA memory hazards."
  (let ((earlier-reads (opt-inst-read-regs earlier))
        (earlier-writes (%opt-inst-write-regs earlier))
        (later-reads (opt-inst-read-regs later))
        (later-writes (%opt-inst-write-regs later))
        (alias-roots (gethash :alias-roots metadata))
        (type-facts (gethash :type-facts metadata)))
    (or
      (%opt-reg-intersect-p earlier-writes later-reads)
      (%opt-reg-intersect-p earlier-reads later-writes)
      (%opt-reg-intersect-p earlier-writes later-writes)
      (and
        (opt-memory-read-inst-p earlier)
        (opt-memory-write-inst-p later)
        (opt-memory-accesses-may-alias-p earlier later alias-roots type-facts))
      (and
        (opt-memory-write-inst-p earlier)
        (opt-memory-read-inst-p later)
        (opt-memory-accesses-may-alias-p later earlier alias-roots type-facts))
      (and (opt-memory-write-inst-p earlier) (opt-memory-write-inst-p later))))) ; WAW

(defun %opt-build-scheduler-graph (insts)
  "Build dependency predecessor/successor vectors for INSTS."
  (let* ((n (length insts))
         (preds (make-array n :initial-element nil))
         (succs (make-array n :initial-element nil))
         (metadata (opt-build-memory-tbaa-metadata insts)))
    (loop for i from 0 below n
          do (loop for j from (1+ i) below n
            when (%opt-scheduler-depends-p (nth i insts) (nth j insts) metadata)
              do (pushnew i (aref preds j)) (pushnew j (aref succs i))))
    (values preds succs)))

(defun %opt-compute-scheduler-priorities (insts succs)
  "Return critical-path priorities for INSTS given successor vector SUCCS."
  (let* ((n (length insts))
         (memo (make-array n :initial-element nil)))
    (labels ((priority (i)
               (or
            (aref memo i)
            (setf (aref memo i) (+
                (%opt-inst-latency (nth i insts))
                (loop for succ in (aref succs i)
                      maximize (priority succ) into best
                      finally (return (or best 0))))))))
      (loop for i from 0 below n
            do (priority i))
      memo)))

(defun %opt-best-ready-node (ready priorities)
  "Select the highest-priority node from READY, preserving original order on ties."
  (reduce
    (lambda (best candidate)
      (let ((best-priority (aref priorities best))
            (candidate-priority (aref priorities candidate)))
        (if (or
            (> candidate-priority best-priority)
            (and (= candidate-priority best-priority) (< candidate best))) candidate
          best)))
    ready))

(defun %opt-run-scheduler-toposort (insts &key node-selector counts-or-nil)
  "Toposort-based list scheduler for a side-effect-free instruction run.

NODE-SELECTOR is a function (ready insts priorities [counts]) -> node-index.
COUNTS-OR-NIL is a pre-built read-count table for pre-RA pressure tracking,
or NIL for post-RA scheduling (where pressure is ignored)."
  (if (< (length insts) 2) insts
    (multiple-value-bind (preds succs) (%opt-build-scheduler-graph insts)
      (let* ((n (length insts))
             (priorities (%opt-compute-scheduler-priorities insts succs))
             (remaining-preds (make-array n))
             (ready nil)
             (emitted nil))
        (loop for i from 0 below n
              do (setf (aref remaining-preds i) (copy-list (aref preds i)))
                 (when (null (aref remaining-preds i))
                   (push i ready)))
        (loop while ready
              do (let* ((node
                (if counts-or-nil (funcall node-selector ready insts priorities counts-or-nil)
                  (funcall node-selector ready priorities)))
                 (inst (nth node insts)))
            (setf ready (remove node ready :test #'eql))
            (push inst emitted)
            (when counts-or-nil
              (%opt-decrement-reg-counts (opt-inst-read-regs inst) counts-or-nil))
            (dolist (succ (aref succs node))
              (setf (aref remaining-preds succ)
                    (remove node (aref remaining-preds succ) :test #'eql))
              (when (null (aref remaining-preds succ))
                (pushnew succ ready :test #'eql)))))
        (if (= (length emitted) n) (nreverse emitted)
          insts)))))

(defun %opt-schedule-run (insts)
  "List-schedule a side-effect-free instruction run."
  (%opt-run-scheduler-toposort insts :node-selector #'%opt-best-ready-node))

(defun %opt-schedule-basic-block (instructions)
  "Schedule each movable run inside one basic block INSTRUCTIONS."
  (let ((result nil)
        (run nil))
    (labels ((flush-run ()
               (when run
            (dolist (inst (%opt-schedule-run (nreverse run)))
              (push inst result))
            (setf run nil))))
      (dolist (inst instructions)
        (if (%opt-scheduler-barrier-p inst) (progn
            (flush-run)
            (push inst result))
          (push inst run)))
      (flush-run)
      (nreverse result))))

(defun %opt-scheduler-segments (instructions)
  "Split INSTRUCTIONS into movable runs and fixed barriers."
  (let ((segments nil)
        (run nil))
    (labels ((flush-run ()
               (when run
            (push (list :run (nreverse run)) segments)
            (setf run nil))))
      (dolist (inst instructions)
        (if (%opt-scheduler-barrier-p inst) (progn
            (flush-run)
            (push (list :barrier inst) segments))
          (push inst run)))
      (flush-run)
      (nreverse segments))))

(defun %opt-flatten-cfg-block-order (cfg)
  "Flatten CFG in original block creation order after local block rewrites."
  (let ((result nil))
    (loop for block across (cfg-blocks cfg)
          do (when (bb-label block)
        (push (bb-label block) result)) (dolist (inst (bb-instructions block))
        (push inst result)))
    (nreverse result)))

(defun opt-pass-schedule-local (instructions)
  "FR-069: Dependency-aware list scheduling within each basic block.

Builds a local DAG with RAW/WAR/WAW register dependencies, computes critical-path
priorities from estimated VM latencies, and emits highest-priority ready nodes.
Scheduling is limited to side-effect-free runs inside a basic block; calls,
stores, signals, and control-flow instructions are barriers."
  (let ((cfg (cfg-build instructions)))
    (loop for block across (cfg-blocks cfg)
          do (setf (bb-instructions block) (%opt-schedule-basic-block (bb-instructions block))))
    (%opt-flatten-cfg-block-order cfg)))
