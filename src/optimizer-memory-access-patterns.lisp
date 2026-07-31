;;;; optimizer-memory-access-patterns.lisp — memory access pattern analysis
;;;;
;;;; Classifies array/vector accesses into strided-access patterns for
;;;; downstream prefetch and vectorization passes, and resolves a register's
;;;; ultimate points-to root for alias analysis.

(in-package :cl-cc/optimize)

(defun %opt-memory-access-index-reg (inst)
  (typecase inst
    ((or vm-aref vm-aset) (vm-index-reg inst))
    (t nil)))

(defun %opt-memory-access-array-reg (inst)
  (typecase inst
    ((or vm-aref vm-aset) (vm-array-reg inst))
    (t nil)))

(defun %opt-memory-access-kind (inst)
  (typecase inst
    (vm-aref :load)
    (vm-aset :store)
    (t nil)))

(defun %opt-update-memory-pattern-constants (inst env)
  (typecase inst
    (vm-const
     (if (integerp (vm-value inst))
         (setf (gethash (vm-dst inst) env) (vm-value inst))
         (remhash (vm-dst inst) env)))
    (vm-move
     (multiple-value-bind (value found-p) (gethash (vm-src inst) env)
       (if found-p
           (setf (gethash (vm-dst inst) env) value)
           (remhash (vm-dst inst) env))))
    (t
     (let ((dst (opt-inst-dst inst)))
       (when dst (remhash dst env))))))

(defun %opt-memory-pattern-class (stride)
  (cond
    ((null stride) :random)
    ((= (abs stride) 1) :sequential)
    (t :strided)))

(defun %opt-memory-pattern-record (metadata inst block array-reg index-reg stride &key memory-entry)
  (let ((entry (list :kind (%opt-memory-access-kind inst)
                     :array-reg array-reg
                     :index-reg index-reg
                     :stride stride
                     :pattern (%opt-memory-pattern-class stride)
                     :block block
                     :memory-ssa memory-entry
                     :prefetch-candidate (and stride (<= (abs stride) 4))
                     :tiling-candidate (and stride (/= stride 0)))))
    (setf (gethash inst metadata) entry)
    (push inst (gethash :accesses metadata))
    entry))

;;; FR-309: Memory Access Pattern Analysis — classifies memory accesses as
;;; sequential/strided/random; provides data for prefetch insertion and
;;; loop tiling
(defun opt-analyze-memory-access-patterns (cfg-or-instructions memory-ssa)
  "Analyze array memory accesses and classify their access patterns.

Returns an EQ hash-table keyed by memory access instruction.  Each value is a
plist containing `:stride', `:pattern' (`:sequential', `:strided', or `:random'),
and metadata flags consumed by future prefetch (FR-289) and loop-tiling (FR-287)
passes.  Constant-stride evidence is derived from consecutive array accesses,
constant index registers, and simple loop induction summaries."
  (let* ((cfg (if (cfg-p cfg-or-instructions)
                  cfg-or-instructions
                  (cfg-build cfg-or-instructions)))
         (instructions (loop for block across (cfg-blocks cfg)
                             append (bb-instructions block)))
         (inductions (opt-compute-loop-inductions cfg))
         (metadata (make-hash-table :test #'eq)))
    (cfg-compute-dominators cfg)
    (cfg-compute-loop-depths cfg)
    (loop for block across (cfg-blocks cfg)
          when block
          do (let ((constants (make-hash-table :test #'eq))
                   (last-by-array (make-hash-table :test #'eq)))
               (dolist (inst (bb-instructions block))
                 (let ((array-reg (%opt-memory-access-array-reg inst))
                       (index-reg (%opt-memory-access-index-reg inst)))
                   (if (and array-reg index-reg)
                       (let* ((last (gethash array-reg last-by-array))
                              (index-value (gethash index-reg constants))
                              (iv-step (loop for ivs being the hash-values of inductions
                                             for iv = (gethash index-reg ivs)
                                             when (and iv (numberp (opt-iv-step iv)))
                                               return (opt-iv-step iv)))
                              (stride (cond
                                        (iv-step iv-step)
                                        ((and index-value last (getf last :index-value))
                                         (- index-value (getf last :index-value)))
                                        (t nil))))
                         (%opt-memory-pattern-record
                          metadata inst block array-reg index-reg stride
                          :memory-entry (and memory-ssa (gethash inst memory-ssa)))
                         (setf (gethash array-reg last-by-array)
                               (list :inst inst :index-reg index-reg :index-value index-value)))
                       (%opt-update-memory-pattern-constants inst constants))))))
    (setf (gethash :cfg metadata) cfg
          (gethash :instructions metadata) instructions)
    metadata))

(defun opt-points-to-root (reg points-to)
  "Return REG's canonical root under POINTS-TO as two values: root and found-p."
  (gethash reg points-to))
