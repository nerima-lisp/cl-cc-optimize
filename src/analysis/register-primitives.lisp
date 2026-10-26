(in-package #:cl-cc/optimize/analysis)

(defun %opt-fresh-register-generator (instructions)
  "Return a closure producing fresh VM register keywords for INSTRUCTIONS."
  (let ((next (1+ (opt-max-reg-index instructions))))
    (lambda ()
      (prog1
        (intern (format nil "R~A" next) :keyword)
        (incf next)))))

(defun %opt-generated-label-present-p (instructions prefix)
  "Return T when INSTRUCTIONS already contain a generated label with PREFIX."
  (some (lambda (inst)
          (and (typep inst 'vm-label)
               (let ((name (vm-name inst)))
                 (and (stringp name)
                      (<= (length prefix) (length name))
                      (string= prefix name :end2 (length prefix))))))
        instructions))

;;; %opt-fresh-register-generator is defined in optimizer-inline.lisp.
;;; No local copies needed here.

(define-inst-type-predicate %opt-control-or-label-p opt-control-or-label "Return T when INST should not be part of a straight-line outlined sequence.")

(defun opt-max-reg-index (instructions)
  "Return the maximum integer N where :RN is used in INSTRUCTIONS, or -1."
  (let ((max-idx -1))
    (dolist (inst instructions max-idx)
      (dolist (reg (cons (opt-inst-dst inst) (opt-inst-read-regs inst)))
        (when (and reg (opt-register-keyword-p reg))
          (let* ((name (symbol-name reg))
                 (idx (handler-case (parse-integer name :start 1) (parse-error () nil))))
            (when (and idx (> idx max-idx))
              (setf max-idx idx))))))))
