(in-package :cl-cc/optimize)
;;;; ─── FR-226: Auto-Vectorization ─────────────────────────────────────────

(defparameter *opt-simd-lane-count* 4
  "Default SIMD lane count used by conservative strip-mined vector loops.")

(defparameter *opt-autovec-label-prefix* "__clcc_autovec_"
  "Prefix for generated auto-vectorization remainder labels.")

(defun %opt-autovec-idempotent-p (instructions)
  "Return T when auto-vectorization markers are already present."
  (some (lambda (inst)
          (or (typep inst 'vm-simd-vector-op)
              (and (typep inst 'vm-label)
                   (let ((name (vm-name inst)))
                     (and (stringp name)
                          (<= (length *opt-autovec-label-prefix*) (length name))
                          (string= *opt-autovec-label-prefix* name
                                   :end2 (length *opt-autovec-label-prefix*)))))))
        instructions))

(define-inst-type-predicate %opt-autovec-cmp-inst-p opt-autovec-cmp "T when INST is a supported counted-loop comparison.")

(defun %opt-autovec-clone-cmp (inst dst lhs rhs)
  "Clone supported comparison INST with new DST/LHS/RHS registers.
   Dispatch is data-driven via *opt-autovec-cmp-clone-table*."
  (when-let ((ctor (gethash (type-of inst) *opt-autovec-cmp-clone-table*)))
    (funcall ctor dst lhs rhs)))

(defun %opt-autovec-op-kind (inst)
  "Return a backend-neutral SIMD op keyword for scalar binary INST, or NIL.
   Dispatch is data-driven via *opt-autovec-scalar-to-simd-op*."
  (gethash (type-of inst) *opt-autovec-scalar-to-simd-op*))

(defun %opt-autovec-array-loads (body iv-reg)
  "Return a table mapping load destination registers to source array registers."
  (let ((loads (make-hash-table :test #'eq)))
    (dolist (inst body loads)
      (when (and (typep inst 'vm-aref)
                 (eq (vm-index-reg inst) iv-reg))
        (setf (gethash (vm-dst inst) loads) (vm-array-reg inst))))))

(defun %opt-autovec-find-map-op (body iv-reg)
  "Detect independent array map scalar ops in BODY and return SIMD markers.

Accepted scalar shape inside one counted loop iteration:
  (aref t1 a i) (aref t2 b i) (<binop> v t1 t2) (aset c i v)
Multiple such chains are allowed as long as they use the loop IV only for array
indices and do not carry values between iterations."
  (let ((loads (%opt-autovec-array-loads body iv-reg))
        (producers (make-hash-table :test #'eq))
        (simd nil))
    (dolist (inst body)
      (when-let ((op (%opt-autovec-op-kind inst)))
        (multiple-value-bind (lhs-array lhs-ok) (gethash (vm-lhs inst) loads)
          (multiple-value-bind (rhs-array rhs-ok) (gethash (vm-rhs inst) loads)
            (when (and lhs-ok rhs-ok)
              (setf (gethash (vm-dst inst) producers)
                    (list op lhs-array rhs-array)))))))
    (dolist (inst body)
      (when (and (typep inst 'vm-aset)
                 (eq (vm-index-reg inst) iv-reg))
        (destructuring-bind (&optional op lhs-array rhs-array)
            (gethash (vm-val-reg inst) producers)
          (when op
            (push (make-vm-simd-vector-op :op op
                                          :dst-array (vm-array-reg inst)
                                          :lhs-array lhs-array
                                          :rhs-array rhs-array
                                          :index-reg iv-reg
                                          :lanes *opt-simd-lane-count*)
                  simd)))))
    (nreverse simd)))

(defun %opt-autovec-vector-limit (init limit step lanes cmp-inst)
  "Return strip-mined vector-limit value for compile-time counted loops."
  (let ((trip (and init limit step
                   (= step 1)
                   (%opt-loop-unroll-trip-count cmp-inst init limit step))))
    (when (and trip (>= trip lanes))
      (+ init (* lanes (floor trip lanes))))))

(defun %opt-autovec-emit-vector-loop (header cmp-inst jz-inst body step-inst exit-label
                                             vec-limit-reg lane-reg remainder-label simd-ops result)
  "Emit vector loop plus scalar remainder loop into reversed RESULT."
  (push header result)
  (push (%opt-autovec-clone-cmp cmp-inst (vm-dst cmp-inst) (vm-lhs cmp-inst) vec-limit-reg) result)
  (push (make-vm-jump-zero :reg (vm-reg jz-inst) :label remainder-label) result)
  (dolist (op simd-ops) (push op result))
  (push (make-vm-add :dst (vm-dst step-inst) :lhs (vm-lhs step-inst) :rhs lane-reg) result)
  (push (make-vm-jump :label (vm-name header)) result)
  (push (make-vm-label :name remainder-label) result)
  (push cmp-inst result)
  (push (make-vm-jump-zero :reg (vm-reg jz-inst) :label exit-label) result)
  (dolist (inst body) (push inst result))
  (push (make-vm-jump :label remainder-label) result)
  result)

(defun %opt-autovec-try-vectorize-at (vec i n fresh-reg result serial)
  "Attempt to vectorize a counted loop starting at position I in VEC.

Returns (values new-result new-i new-serial vectorized-p).  When the loop at
position I matches the canonical autovec shape and has profitable SIMD ops,
new-result contains the rewritten instructions and vectorized-p is T.
Otherwise new-result is unchanged, new-i is (1+ I), and vectorized-p is NIL."
  (let* ((cur (aref vec i))
         (header cur)
         (cmp-inst (aref vec (+ i 1)))
         (jz-inst  (aref vec (+ i 2)))
         (header-name (vm-name header)))
    (flet ((fail ()
             ;; Emit the current instruction as-is and advance by one position.
             (push cur result)
             (values result (1+ i) serial nil)))
      (if (and (%opt-autovec-cmp-inst-p cmp-inst)
               (typep jz-inst 'vm-jump-zero)
               (eq (vm-reg jz-inst) (vm-dst cmp-inst)))
          (let* ((exit-name (vm-label-name jz-inst))
                 (exit-pos  (cfg-find-label-position vec n exit-name))
                 (back-pos  (and exit-pos (1- exit-pos)))
                 (back-inst (and back-pos (>= back-pos 0) (aref vec back-pos))))
            (if (and exit-pos
                     (> exit-pos (+ i 4))
                     (typep back-inst 'vm-jump)
                     (equal (vm-label-name back-inst) header-name)
                     (not (%opt-has-external-jump-to-label-p vec header-name i exit-pos)))
                (let* ((body      (loop for j from (+ i 3) below back-pos collect (aref vec j)))
                       (step-inst (car (last body)))
                       (const-env (%opt-build-const-env-up-to vec i)))
                  (if (and (typep step-inst 'vm-add)
                           (eq (vm-dst step-inst) (vm-lhs step-inst))
                           (eq (vm-dst step-inst) (vm-lhs cmp-inst)))
                      (let* ((iv-reg  (vm-lhs cmp-inst))
                             (lim-reg (vm-rhs cmp-inst))
                             (step-reg (vm-rhs step-inst))
                             (init    (gethash iv-reg const-env))
                             (limit   (gethash lim-reg const-env))
                             (step    (gethash step-reg const-env))
                             (vector-limit (%opt-autovec-vector-limit
                                            init limit step *opt-simd-lane-count* cmp-inst))
                             (simd-ops (%opt-autovec-find-map-op (butlast body) iv-reg)))
                        (if (and vector-limit simd-ops)
                            (let ((vec-limit-reg    (funcall fresh-reg))
                                  (lane-reg         (funcall fresh-reg))
                                  (remainder-label
                                    (format nil "~A~D" *opt-autovec-label-prefix* serial)))
                              (push (make-vm-const :dst vec-limit-reg :value vector-limit) result)
                              (push (make-vm-const :dst lane-reg :value *opt-simd-lane-count*)
                                    result)
                              (setf result (%opt-autovec-emit-vector-loop
                                            header cmp-inst jz-inst body step-inst exit-name
                                            vec-limit-reg lane-reg remainder-label simd-ops result))
                              (push (aref vec exit-pos) result)
                              (values result (1+ exit-pos) (1+ serial) t))
                            (fail)))
                      (fail)))
                (fail)))
          (fail)))))

(defun opt-pass-auto-vectorization (instructions)
  "FR-226: vectorize independent scalar array ops in counted loops.

The pass recognizes a conservative one-dimensional array-map loop, emits a SIMD
vector loop strip-mined by `*opt-simd-lane-count*`, and retains a scalar
remainder loop for tail iterations.  Dynamic trip-count loops are left unchanged;
compile-time trip counts make the generated vector-limit constant explicit."
  (if (%opt-autovec-idempotent-p instructions)
      instructions
      (let* ((vec       (coerce instructions 'vector))
             (n         (length vec))
             (fresh-reg (%opt-fresh-register-generator instructions))
             (result    nil)
             (i         0)
             (changed   nil)
             (serial    0))
        (loop while (< i n)
              do (let ((cur (aref vec i)))
                   (if (and (typep cur 'vm-label) (<= (+ i 5) (1- n)))
                       (multiple-value-bind (new-result new-i new-serial vectorized-p)
                           (%opt-autovec-try-vectorize-at vec i n fresh-reg result serial)
                         (setf result new-result
                               i      new-i
                               serial new-serial)
                         (when vectorized-p (setf changed t)))
                       (progn (push cur result) (incf i)))))
        (if changed (nreverse result) instructions))))
