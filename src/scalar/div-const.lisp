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

(defun %opt-fr685-kill-dst (inst env)
  "Remove INST's destination register, if any, from ENV's constant bindings."
  (when-let ((dst (opt-inst-dst inst)))
    (remhash dst env)))

(defun %opt-fr685-emit-seq (seq result)
  "Push each instruction in SEQ onto RESULT (reverse order) and return it."
  (dolist (inst seq result)
    (push inst result)))

(defun %opt-fr685-try-lower-divmod (inst env intervals fresh-reg result seq-fn)
  "Try lowering vm-div/vm-mod INST via SEQ-FN using the known constant divisor
and proved dividend interval, falling back to keeping INST unchanged when no
sequence applies. ENV/INTERVALS are mutated in place; returns the updated
RESULT list with the lowered sequence or the unchanged INST pushed on."
  (let* ((dst (vm-dst inst))
         (lhs (vm-lhs inst))
         (divisor (gethash (vm-rhs inst) env))
         (interval (gethash lhs intervals))
         (seq (and (integerp divisor)
                   (not (zerop divisor))
                   (> divisor 1)
                   (not (opt-power-of-2-p divisor))
                   (funcall seq-fn dst lhs divisor interval fresh-reg))))
    (setf result
          (if seq
              (progn (remhash dst env) (%opt-fr685-emit-seq seq result))
              (progn (%opt-fr685-kill-dst inst env) (push inst result))))
    (%opt-transfer-interval-inst inst intervals)
    result))

(defun %opt-pass-div-by-const/fr685 (instructions)
  "FR-685: lower safe integer division/modulo by known non-zero constants.

Power-of-two divisors are intentionally left for opt-pass-strength-reduce's
vm-ash lowering.  Non-power-of-two lowering uses verified reciprocal sequences
for bounded signed/unsigned intervals and unsigned 64-bit magic multiply-high
when range facts prove the dividend is an unsigned word."
  (let* ((env (make-hash-table :test #'eq))
         (intervals (make-hash-table :test #'eq))
         (fresh-reg (%opt-fresh-register-generator instructions))
         (result nil))
    (dolist (inst instructions)
      (typecase inst
        (vm-label
         (clrhash env)
         (clrhash intervals)
         (push inst result))
        (vm-const
         (setf (gethash (vm-dst inst) env) (vm-value inst))
         (push inst result)
         (%opt-transfer-interval-inst inst intervals))
        (vm-div
         (setf result (%opt-fr685-try-lower-divmod
                       inst env intervals fresh-reg result #'%opt-div-const-quotient-seq)))
        (vm-mod
         (setf result (%opt-fr685-try-lower-divmod
                       inst env intervals fresh-reg result #'%opt-mod-by-const-seq)))
        (t
         (%opt-fr685-kill-dst inst env)
         (push inst result)
         (%opt-transfer-interval-inst inst intervals))))
    (nreverse result)))
