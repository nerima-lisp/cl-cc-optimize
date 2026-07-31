;;;; optimizer-register-rename.lisp — VM register renaming utilities
;;;;
;;;; Shared register-renaming primitives used when a block of instructions is
;;;; duplicated into a new context: call-site splitting
;;;; (optimizer-inline-call-site-split.lisp) and sealed-GF devirtualization
;;;; (optimizer-inline-devirt.lisp) both need a copy of an existing
;;;; instruction sequence with every register replaced by a fresh one, so the
;;;; copy does not alias the original's live ranges.
;;;;
;;;; Renaming goes through instruction->sexp / sexp->instruction rather than
;;;; slot-by-slot mutation, so it automatically covers every VM instruction
;;;; type's register-valued slots without this file needing to know each
;;;; type's shape.

(in-package :cl-cc/optimize)

(defun opt-max-reg-index (instructions)
  "Return the maximum integer N where :RN is used in INSTRUCTIONS, or -1."
  (let ((max-idx -1))
    (dolist (inst instructions max-idx)
      (dolist (reg (cons (opt-inst-dst inst) (opt-inst-read-regs inst)))
        (when (and reg (opt-register-keyword-p reg))
          (let* ((name (symbol-name reg))
                 (idx (ignore-errors (parse-integer name :start 1))))
            (when (and idx (> idx max-idx))
              (setf max-idx idx))))))))

(defun opt-make-renaming (body-instructions base-index)
  "Build renaming table: existing-register → fresh :R<N> (starting at BASE-INDEX).
   Uses opt-inst-dst + opt-inst-read-regs to discover ALL registers, including
   those not serialized by instruction->sexp (e.g., vm-make-obj initarg-regs)."
  (let ((seen (make-hash-table :test #'eq))
        (counter base-index)
        (renaming (make-hash-table :test #'eq)))
    (flet ((add (r)
             (when (and r (opt-register-keyword-p r))
               (unless (gethash r seen)
                 (setf (gethash r seen) t)
                 (setf (gethash r renaming)
                       (intern (format nil "R~A" counter) :keyword))
                 (incf counter)))))
      (dolist (inst body-instructions)
        (add (opt-inst-dst inst))
        (dolist (r (opt-inst-read-regs inst)) (add r))))
    renaming))

(defun %opt-collect-sexp-regs-into-cell (form cell)
  "Recursively push register keywords from sexp FORM onto (car CELL)."
  (cond ((and (keywordp form) (opt-register-keyword-p form))
         (push form (car cell)))
        ((consp form)
         (%opt-collect-sexp-regs-into-cell (car form) cell)
         (%opt-collect-sexp-regs-into-cell (cdr form) cell))))

(defun opt-can-safely-rename-p (body-instructions)
  "T if all instructions in BODY can be safely register-renamed via sexp roundtrip.
   An instruction is safe when instruction->sexp captures all its registers —
   i.e., opt-inst-read-regs reports the same registers as appear in the sexp.
   Instructions with custom sexp methods (vm-make-obj, vm-slot-read, etc.) that
   omit registers from their sexp representation will cause this to return NIL."
  (dolist (inst body-instructions t)
    (let ((explicit-regs (remove nil
                                 (cons (opt-inst-dst inst)
                                       (opt-inst-read-regs inst))))
          (sexp-regs-cell (list nil)))
      (handler-case (%opt-collect-sexp-regs-into-cell (instruction->sexp inst) sexp-regs-cell)
        (error () (return-from opt-can-safely-rename-p nil)))
      (let ((sexp-regs (car sexp-regs-cell)))
        (unless (every (lambda (r) (member r sexp-regs)) explicit-regs)
          (return-from opt-can-safely-rename-p nil))))))

(defun opt-rename-regs-in-inst (inst renaming)
  "Return INST with all VM register keywords substituted per RENAMING.
   Uses instruction→sexp roundtrip; returns INST unchanged on any error."
  (handler-case
      (sexp->instruction
       (opt-map-tree (lambda (x) (if (and (keywordp x) (opt-register-keyword-p x))
                                     (or (gethash x renaming) x)
                                     x))
                     (instruction->sexp inst)))
    (error () inst)))
