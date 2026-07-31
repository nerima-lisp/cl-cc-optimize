;;;; packages/optimize/src/optimizer-div-const.lisp — FR-685 division by constant

(in-package :cl-cc/optimize)

(defun %opt-div-const-quotient-seq (dst src divisor interval new-reg-fn)
  "Build a multiply+shift quotient sequence for SRC/DIVISOR when safe."
  (or (%opt-div-by-verified-reciprocal-seq dst src divisor interval new-reg-fn)
      (%opt-div-by-verified-reciprocal-seq-with-bias dst src divisor interval new-reg-fn)
      (%opt-div-by-unsigned-magic-seq dst src divisor interval new-reg-fn)))

(defun %opt-mod-by-const-seq (dst src divisor interval new-reg-fn)
  "Build mod via q = div-const(src), dst = src - divisor*q."
  (let ((q-reg (funcall new-reg-fn)))
    (when-let ((q-seq (%opt-div-const-quotient-seq q-reg src divisor interval new-reg-fn)))
      (let ((divisor-reg (funcall new-reg-fn))
            (prod-reg (funcall new-reg-fn)))
        (append q-seq
                (list (make-vm-const :dst divisor-reg :value divisor)
                      (make-vm-mul :dst prod-reg :lhs q-reg :rhs divisor-reg)
                      (make-vm-sub :dst dst :lhs src :rhs prod-reg)))))))

(defun %opt-pass-div-by-const/fr685 (instructions)
  "FR-685: lower safe integer division/modulo by known non-zero constants.

Power-of-two divisors are intentionally left for opt-pass-strength-reduce's
vm-ash lowering.  Non-power-of-two lowering uses verified reciprocal sequences
for bounded signed/unsigned intervals and unsigned 64-bit magic multiply-high
when range facts prove the dividend is an unsigned word."
  (let* ((env (make-hash-table :test #'eq))
         (intervals (make-hash-table :test #'eq))
         (counter (1+ (opt-max-reg-index instructions)))
         (result nil))
    (labels ((new-reg ()
               (prog1 (intern (format nil "R~A" counter) :keyword)
                 (incf counter)))
             (const-val (reg)
               (gethash reg env))
             (emit-seq (seq)
               (dolist (inst seq) (push inst result)))
             (kill-dst (inst)
               (when-let ((dst (opt-inst-dst inst)))
                 (remhash dst env)))
             (advance (inst)
               (%opt-transfer-interval-inst inst intervals))
             (try-lower-divmod (inst seq-fn)
               (let* ((dst (vm-dst inst))
                      (lhs (vm-lhs inst))
                      (divisor (const-val (vm-rhs inst)))
                      (interval (gethash lhs intervals))
                      (seq (and (integerp divisor)
                                (not (zerop divisor))
                                (> divisor 1)
                                (not (opt-power-of-2-p divisor))
                                (funcall seq-fn dst lhs divisor interval #'new-reg))))
                 (if seq
                     (progn (remhash dst env) (emit-seq seq))
                     (progn (kill-dst inst) (push inst result)))
                 (advance inst))))
      (dolist (inst instructions)
        (typecase inst
          (vm-label
           (clrhash env)
           (clrhash intervals)
           (push inst result))
          (vm-const
           (setf (gethash (vm-dst inst) env) (vm-value inst))
           (push inst result)
           (advance inst))
          (vm-div
           (try-lower-divmod inst #'%opt-div-const-quotient-seq))
          (vm-mod
           (try-lower-divmod inst #'%opt-mod-by-const-seq))
          (t
           (kill-dst inst)
           (push inst result)
           (advance inst)))))
    (nreverse result)))
