;;;; optimizer-vectorize-slp.lisp — superword-level parallelism vectorization

(in-package :cl-cc/optimize)

;;;; ─── FR-227: SLP Vectorization ──────────────────────────────────────────

(defun %opt-slp-idempotent-p (instructions)
  "Return T when INSTRUCTIONS already contain SIMD vector markers."
  (some (lambda (inst) (typep inst 'vm-simd-vector-op)) instructions))

(defun %opt-slp-op-kind (inst)
  "Return the SIMD op keyword and packed element type for scalar SLP INST."
  (typecase inst
    (vm-float-add
     (when (eq (vm-float-precision inst) :f64)
       (values :add :f64)))
    (vm-float-sub
     (when (eq (vm-float-precision inst) :f64)
       (values :sub :f64)))
    (vm-float-mul
     (when (eq (vm-float-precision inst) :f64)
       (values :mul :f64)))
    (t
     (when-let ((op (%opt-autovec-op-kind inst)))
       (values op :i32)))))

(defun %opt-slp-index-descriptor (reg values offsets)
  "Return a normalized descriptor for index register REG.

The descriptor is `(BASE . OFFSET)`.  BASE is NIL for literal constants, or the
base index register for an affine `(+ base constant)` value found in the block."
  (multiple-value-bind (value value-p) (gethash reg values)
    (if value-p
        (cons nil value)
        (multiple-value-bind (desc offset-p) (gethash reg offsets)
          (if offset-p desc (cons reg 0))))))

(defun %opt-slp-record-index-fact (inst values offsets)
  "Record simple integer/affine index facts produced by INST."
  (cond
   ((typep inst 'vm-const)
    (if (integerp (vm-value inst))
        (setf (gethash (vm-dst inst) values) (vm-value inst))
        (remhash (vm-dst inst) values))
    (remhash (vm-dst inst) offsets))
   ((typep inst 'vm-add)
    (let ((dst (vm-dst inst))
          (lhs (vm-lhs inst))
          (rhs (vm-rhs inst)))
      (multiple-value-bind (lhs-value lhs-const-p) (gethash lhs values)
        (multiple-value-bind (rhs-value rhs-const-p) (gethash rhs values)
          (cond
           ((and lhs-const-p rhs-const-p)
            (setf (gethash dst values) (+ lhs-value rhs-value))
            (remhash dst offsets))
           (rhs-const-p
            (remhash dst values)
            (setf (gethash dst offsets)
                  (let ((base (%opt-slp-index-descriptor lhs values offsets)))
                    (cons (car base) (+ (cdr base) rhs-value)))))
           (lhs-const-p
            (remhash dst values)
            (setf (gethash dst offsets)
                  (let ((base (%opt-slp-index-descriptor rhs values offsets)))
                    (cons (car base) (+ (cdr base) lhs-value)))))
           (t
            (remhash dst values)
            (remhash dst offsets)))))))
   (t
    (when-let ((dst (opt-inst-dst inst)))
      (remhash dst values)
      (remhash dst offsets)))))

(defun %opt-slp-analyze-block (insts)
  "Return per-block producer/index facts used by SLP matching."
  (let ((loads (make-hash-table :test #'eq))
        (ops (make-hash-table :test #'eq))
        (indexes (make-hash-table :test #'eq))
        (values (make-hash-table :test #'eq))
        (offsets (make-hash-table :test #'eq))
        (positions (make-hash-table :test #'eq))
        (reads (make-hash-table :test #'eq)))
    (loop for inst in insts
          for pos from 0
          do (setf (gethash inst positions) pos)
             (dolist (reg (opt-inst-read-regs inst))
               (incf (gethash reg reads 0)))
             (%opt-slp-record-index-fact inst values offsets)
             (when (typep inst '(or vm-aref vm-aset))
               (setf (gethash inst indexes)
                     (%opt-slp-index-descriptor (vm-index-reg inst) values offsets)))
             (cond
               ((typep inst 'vm-aref)
                (setf (gethash (vm-dst inst) loads)
                      (list :inst inst
                            :array (vm-array-reg inst)
                            :index-reg (vm-index-reg inst)
                            :index (gethash inst indexes))))
               (t
                (multiple-value-bind (op element-type) (%opt-slp-op-kind inst)
                  (when op
                    (setf (gethash (vm-dst inst) ops)
                          (list :inst inst
                                :op op
                                :element-type element-type
                                :lhs (vm-lhs inst)
                                :rhs (vm-rhs inst))))))))
    (values loads ops indexes positions reads)))

(defun %opt-slp-store-lane (store loads ops indexes reads)
  "Return a lane plist for STORE when it is a scalar array-map lane."
  (when (typep store 'vm-aset)
    (let* ((producer (gethash (vm-val-reg store) ops))
           (lhs-load (and producer (gethash (getf producer :lhs) loads)))
           (rhs-load (and producer (gethash (getf producer :rhs) loads)))
           (store-index (gethash store indexes)))
      (when (and producer lhs-load rhs-load
                 (= (gethash (getf producer :lhs) reads 0) 1)
                 (= (gethash (getf producer :rhs) reads 0) 1)
                 (= (gethash (vm-val-reg store) reads 0) 1)
                 (equal (getf lhs-load :index) (getf rhs-load :index))
                 (equal (getf lhs-load :index) store-index))
        (list :store store
              :op-inst (getf producer :inst)
              :lhs-load (getf lhs-load :inst)
              :rhs-load (getf rhs-load :inst)
              :op (getf producer :op)
              :element-type (getf producer :element-type)
              :dst-array (vm-array-reg store)
              :lhs-array (getf lhs-load :array)
              :rhs-array (getf rhs-load :array)
              :index-reg (getf lhs-load :index-reg)
              :index (getf lhs-load :index))))))

(defun %opt-slp-lane-key (lane)
  "Return the isomorphism key for LANE, ignoring its lane offset."
  (list (getf lane :op)
        (getf lane :element-type)
        (getf lane :dst-array)
        (getf lane :lhs-array)
        (getf lane :rhs-array)
        (car (getf lane :index))))

(defun %opt-slp-lane-offset (lane)
  "Return the scalar lane offset recorded in LANE."
  (cdr (getf lane :index)))

(defun %opt-slp-supported-lane-count-p (lanes element-type)
  "Return T when LANES and ELEMENT-TYPE are supported by native SIMD emitters."
  (ecase element-type
    (:i32 (member lanes '(4 8) :test #'=))
    (:f64 (= lanes 2))))

(defun %opt-slp-group-contiguous-p (lanes)
  "Return T when LANES form a single contiguous superword."
  (let ((sorted (sort (copy-list lanes) #'< :key #'%opt-slp-lane-offset)))
    (loop for lane in sorted
          for expected from (%opt-slp-lane-offset (first sorted))
          always (= (%opt-slp-lane-offset lane) expected))))

(defun %opt-slp-group-span-safe-p (insts group positions)
  "Return T when replacing GROUP at its first instruction does not cross a barrier."
  (let* ((selected (make-hash-table :test #'eq))
         (indices nil))
    (dolist (lane group)
      (dolist (inst (list (getf lane :lhs-load)
                          (getf lane :rhs-load)
                          (getf lane :op-inst)
                          (getf lane :store)))
        (setf (gethash inst selected) t)
        (push (gethash inst positions) indices)))
    (let ((start (apply #'min indices))
          (end (apply #'max indices)))
      (loop for pos from start to end
            for inst = (nth pos insts)
            always (or (gethash inst selected)
                       (member (vm-inst-effect-kind inst) '(:pure :read-only) :test #'eq))))))

(defun %opt-slp-simd-inst (group)
  "Build a vm-simd-vector-op for GROUP."
  (let* ((lanes (sort (copy-list group) #'< :key #'%opt-slp-lane-offset))
         (first-lane (first lanes)))
    (make-vm-simd-vector-op :op (getf first-lane :op)
                            :dst-array (getf first-lane :dst-array)
                            :lhs-array (getf first-lane :lhs-array)
                            :rhs-array (getf first-lane :rhs-array)
                            :index-reg (getf first-lane :index-reg)
                            :lanes (length lanes)
                            :element-type (getf first-lane :element-type))))

(defun %opt-slp-find-groups (insts)
  "Find independent straight-line SLP groups in INSTS."
  (multiple-value-bind (loads ops indexes positions reads) (%opt-slp-analyze-block insts)
    (let ((buckets (make-hash-table :test #'equal))
          (groups nil))
      (dolist (inst insts)
        (when-let ((lane (%opt-slp-store-lane inst loads ops indexes reads)))
          (push lane (gethash (%opt-slp-lane-key lane) buckets))))
      (loop for lanes being the hash-values of buckets
            do (let* ((ordered (sort (copy-list lanes) #'< :key #'%opt-slp-lane-offset))
                      (element-type (getf (first ordered) :element-type))
                      (want (ecase element-type
                              (:i32 *opt-simd-lane-count*)
                              (:f64 2))))
                 (loop while (>= (length ordered) want)
                       for candidate = (subseq ordered 0 want)
                       do (if (and (%opt-slp-supported-lane-count-p want element-type)
                                   (%opt-slp-group-contiguous-p candidate)
                                   (%opt-slp-group-span-safe-p insts candidate positions))
                              (progn
                                (push candidate groups)
                                (setf ordered (nthcdr want ordered)))
                              (setf ordered (rest ordered))))))
      (nreverse groups))))

(defun %opt-slp-rewrite-block (insts)
  "Rewrite one basic block's scalar superwords into SIMD vector operations."
  (let ((groups (%opt-slp-find-groups insts)))
    (if groups (let ((remove (make-hash-table :test #'eq))
              (insert (make-hash-table :test #'eq)))
          (dolist (group groups)
            (let* ((simd (%opt-slp-simd-inst group))
                   (members (loop for lane in group append
                                  (list (getf lane :lhs-load)
                                        (getf lane :rhs-load)
                                        (getf lane :op-inst)
                                        (getf lane :store))))
                   (anchor (reduce (lambda (a b)
                                     (if (< (position a insts :test #'eq)
                                            (position b insts :test #'eq))
                                         a b))
                                   members)))
              (dolist (inst members) (setf (gethash inst remove) t))
              (setf (gethash anchor insert) simd)))
          (values (loop for inst in insts
                        append (cond
                                 ((gethash inst insert)
                                  (list (gethash inst insert)))
                                 ((gethash inst remove) nil)
                                 (t (list inst))))
                  t)) (values insts nil))))

(defun opt-pass-slp-vectorize (instructions)
  "FR-227: pack straight-line scalar array-map lanes into SIMD vector ops.

The pass builds a CFG, scans each basic block for isomorphic independent scalar
chains of adjacent `vm-aref` → arithmetic/bitwise op → `vm-aset` lanes, and
replaces each full superword with one existing `vm-simd-vector-op` marker.  It is
conservative and idempotent: existing SIMD markers cause the input to be left
unchanged, preventing repeated packing on subsequent optimizer iterations."
  (if (%opt-slp-idempotent-p instructions)
      instructions
      (let ((cfg (cfg-build instructions))
            (changed nil))
        (loop for block across (cfg-blocks cfg)
              do (multiple-value-bind (new-insts block-changed)
                     (%opt-slp-rewrite-block (bb-instructions block))
                   (when block-changed
                     (setf (bb-instructions block) new-insts
                           changed t))))
        (if changed (cfg-flatten cfg) instructions))))
