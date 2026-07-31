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
                (let ((dst (opt-inst-dst inst)))
                  (when dst (remhash dst env))))))
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
    (loop while (< i n)
          do (let ((cur (aref vec i)))
               (if (and (vm-label-p cur)
                        (<= (+ i 5) (1- n)))
                   (let* ((header cur)
                          (cmp-inst (aref vec (+ i 1)))
                          (jz-inst  (aref vec (+ i 2)))
                          (header-name (vm-name header)))
                      (if (and (%opt-loop-unroll-cmp-inst-p cmp-inst)
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
                                    (not (%opt-has-external-jump-to-label-p
                                          vec header-name i exit-pos)))
                               (let* ((body-insts (loop for j from (+ i 3) below back-pos
                                                        collect (aref vec j)))
                                      (step-inst (car (last body-insts)))
                                      (const-env (%opt-build-const-env-up-to vec i)))
                                 (if (and (typep step-inst 'vm-add)
                                          (eq (vm-dst step-inst) (vm-lhs step-inst))
                                          (eq (vm-dst step-inst) (vm-lhs cmp-inst)))
                                     (let* ((iv-reg   (vm-lhs cmp-inst))
                                            (lim-reg  (vm-rhs cmp-inst))
                                            (step-reg (vm-rhs step-inst))
                                            (init     (gethash iv-reg const-env))
                                            (limit    (gethash lim-reg const-env))
                                            (step     (gethash step-reg const-env))
                                             (trip
                                               (and init limit step
                                                    (%opt-loop-unroll-trip-count
                                                     cmp-inst init limit step))))
                                        (cond
                                          ((and trip (> trip 0)
                                                (<= trip *opt-loop-unroll-max-trip*))
                                           (dotimes (_ trip)
                                             (dolist (b body-insts)
                                               (push b result)))
                                           ;; keep exit label and continue
                                           (setf i exit-pos)
                                           (push (aref vec i) result)
                                           (incf i))
                                           ((and (<= (length body-insts) *opt-loop-unroll-max-body*)
                                                 (plusp *opt-loop-unroll-factor*))
                                            (setf result
                                                  (%opt-loop-unroll-emit-partial
                                                   body-insts cmp-inst jz-inst result))
                                            (loop for j from i below (1+ exit-pos)
                                                  do (push (aref vec j) result))
                                            (setf i (1+ exit-pos)))
                                          (t
                                           (push cur result)
                                           (incf i))))
                                     (progn
                                       (push cur result)
                                       (incf i))))
                               (progn
                                 (push cur result)
                                 (incf i))))
                         (progn
                           (push cur result)
                           (incf i))))
                   (progn
                     (push cur result)
                     (incf i)))))
    (nreverse result)))
