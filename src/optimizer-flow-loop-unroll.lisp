;;;; optimizer-flow-loop-unroll.lisp — conservative counted-loop unrolling

(in-package :cl-cc/optimize)

(defparameter *opt-loop-unroll-max-trip* 8
  "Maximum compile-time trip count for conservative full unrolling.")

(defparameter *opt-loop-unroll-factor* 2
  "Conservative partial-unroll factor for counted loops that are too large or unknown.")

(defparameter *opt-loop-unroll-max-body* 8
  "Maximum loop body instruction count eligible for conservative partial unrolling.")

(defun %opt-build-const-env-up-to (vec end)
  "Return reg->integer constant env from VEC[0,END)."
  (let ((env (make-hash-table :test #'eq)))
    (loop for i from 0 below end
          for inst = (aref vec i)
          do (cond
              ((typep inst 'vm-const)
               (if (integerp (vm-value inst))
                   (setf (gethash (vm-dst inst) env) (vm-value inst))
                   (remhash (vm-dst inst) env)))
              (t
               (when-let ((dst (opt-inst-dst inst)))
                 (remhash dst env)))))
    env))

(defun %opt-has-external-jump-to-label-p (vec label-name start end)
  "Return T when any jump outside [START,END] targets LABEL-NAME."
  (loop for i from 0 below (length vec)
        thereis (and (not (and (<= start i) (<= i end)))
                     (let ((inst (aref vec i)))
                       (and (typep inst '(or vm-jump vm-jump-zero))
                             (equal (vm-label-name inst) label-name))))))

(define-inst-type-predicate %opt-loop-unroll-cmp-inst-p (or vm-lt vm-le vm-gt vm-ge vm-eq) "Return T when INST is a comparison supported by loop unrolling.")

(defun %opt-loop-unroll-trip-count (cmp-inst init limit step)
  "Return conservative trip count for CMP-INST and affine induction values."
  (opt-induction-trip-count init limit step :predicate cmp-inst))

(defun %opt-loop-unroll-emit-partial (body-insts cmp-inst jz-inst result)
  "Emit a guarded partial unroll of BODY-INSTS into RESULT.
Returns the updated RESULT list. Each extra copy is guarded by the original
condition so odd or short trip counts fall through to the original exit."
  (dotimes (_ *opt-loop-unroll-factor* result)
    (push cmp-inst result)
    (push jz-inst result)
    (dolist (b body-insts)
      (push b result))))

(defun opt-pass-loop-unrolling (instructions)
  "Unroll conservative counted loops.

Conservative subset. Matches this linear shape:
  Lh: (<cmp> rcond riv rlim) (vm-jump-zero rcond Lexit) body... step (vm-jump Lh) Lexit:
where STEP is (vm-add riv riv rstep) and riv/rlim/rstep are integer constants
known at compile time before Lh. Full unrolling applies when 0 < trip <=
*opt-loop-unroll-max-trip*. Larger or unknown trips get guarded partial
unrolling when the body is small."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (result nil)
         (i 0))
    (labels ((header-candidate-p (idx)
               (and (vm-label-p (aref vec idx))
                    (<= (+ idx 5) (1- n))))
             (parse-loop-shape (idx)
               "Return (values cmp-inst jz-inst exit-pos back-pos) when IDX begins
                a canonical compare/guard/back-jump/exit shape, else NIL."
               (let* ((cmp-inst (aref vec (1+ idx)))
                      (jz-inst  (aref vec (+ idx 2)))
                      (header-name (vm-name (aref vec idx))))
                 (when (and (%opt-loop-unroll-cmp-inst-p cmp-inst)
                            (typep jz-inst 'vm-jump-zero)
                            (eq (vm-reg jz-inst) (vm-dst cmp-inst)))
                   (let* ((exit-name (vm-label-name jz-inst))
                          (exit-pos  (cfg-find-label-position vec n exit-name))
                          (back-pos  (and exit-pos (1- exit-pos)))
                          (back-inst (and back-pos (>= back-pos 0) (aref vec back-pos))))
                     (when (and exit-pos
                                (> exit-pos (+ idx 4))
                                (typep back-inst 'vm-jump)
                                (equal (vm-label-name back-inst) header-name)
                                (not (%opt-has-external-jump-to-label-p
                                      vec header-name idx exit-pos)))
                       (values cmp-inst jz-inst exit-pos back-pos))))))
             (parse-affine-step (idx cmp-inst back-pos)
               "Return (values body-insts init limit step) when the loop body
                ends in a self-add step on CMP-INST's LHS with compile-time-known
                integer init/limit/step, else NIL."
               (let* ((body-insts (loop for j from (+ idx 3) below back-pos
                                        collect (aref vec j)))
                      (step-inst (car (last body-insts)))
                      (const-env (%opt-build-const-env-up-to vec idx)))
                 (when (and (typep step-inst 'vm-add)
                            (eq (vm-dst step-inst) (vm-lhs step-inst))
                            (eq (vm-dst step-inst) (vm-lhs cmp-inst)))
                   (let* ((iv-reg   (vm-lhs cmp-inst))
                          (lim-reg  (vm-rhs cmp-inst))
                          (step-reg (vm-rhs step-inst)))
                     (values body-insts
                             (gethash iv-reg const-env)
                             (gethash lim-reg const-env)
                             (gethash step-reg const-env))))))
             (emit-full-unroll (body-insts trip exit-pos)
               (dotimes (_ trip)
                 (dolist (b body-insts)
                   (push b result)))
               ;; keep exit label and continue
               (setf i exit-pos)
               (push (aref vec i) result)
               (incf i))
             (emit-partial-unroll (body-insts cmp-inst jz-inst idx exit-pos)
               (setf result (%opt-loop-unroll-emit-partial body-insts cmp-inst jz-inst result))
               (loop for j from idx below (1+ exit-pos)
                     do (push (aref vec j) result))
               (setf i (1+ exit-pos)))
             (emit-header-unchanged (cur)
               (push cur result)
               (incf i))
             (try-unroll-at (idx)
               "Attempt to unroll the counted loop starting at IDX. Returns T and
                advances I/RESULT when a rewrite is emitted. Returns NIL, leaving
                I untouched, when the shape at IDX doesn't match or the trip count
                is not eligible for full or partial unrolling."
               (unless (header-candidate-p idx)
                 (return-from try-unroll-at nil))
               (multiple-value-bind (cmp-inst jz-inst exit-pos back-pos) (parse-loop-shape idx)
                 (unless cmp-inst
                   (return-from try-unroll-at nil))
                 (multiple-value-bind (body-insts init limit step) (parse-affine-step idx cmp-inst back-pos)
                   (unless body-insts
                     (return-from try-unroll-at nil))
                   (let ((trip (and init limit step
                                     (%opt-loop-unroll-trip-count cmp-inst init limit step))))
                     (cond
                       ((and trip (plusp trip) (<= trip *opt-loop-unroll-max-trip*))
                        (emit-full-unroll body-insts trip exit-pos)
                        t)
                       ((and (<= (length body-insts) *opt-loop-unroll-max-body*)
                             (plusp *opt-loop-unroll-factor*))
                        (emit-partial-unroll body-insts cmp-inst jz-inst idx exit-pos)
                        t)
                       (t nil)))))))
      (loop while (< i n)
            do (let ((cur (aref vec i)))
                 (unless (try-unroll-at i)
                   (emit-header-unchanged cur)))))
    (nreverse result)))
