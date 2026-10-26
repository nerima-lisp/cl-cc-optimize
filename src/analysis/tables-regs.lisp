(in-package :cl-cc/optimize)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer Data Tables — Read-Regs Dispatch
;;;
;;; Per-type register-read extractor table and opt-inst-read-regs, split out
;;; of optimizer-tables.lisp. Loads after optimizer-tables-fold-data.lisp,
;;; whose *opt-binary-lhs-rhs-types*/*opt-unary-src-types*/*opt-commutative-
;;; inst-types* tables are used below.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun %build-opt-read-regs-table ()
  "Build the VM instruction type → read-reg extractor hash table.
Must be called at load time (after all VM instruction types are defined).
Returns an EQ hash table mapping each type symbol to a (lambda (inst) ...)
that returns the list of register keywords read by that instruction."
  (let ((ht (make-hash-table :test #'eq)))
    ;; Single vm-reg accessor
    (dolist (tp '(vm-jump-zero vm-print vm-halt vm-ret))
      (setf (gethash tp ht) (lambda (inst) (list (vm-reg inst)))))
    ;; Single vm-src accessor
    (dolist (tp '(vm-set-global vm-register-function vm-ensure-values vm-spread-values))
      (setf (gethash tp ht) (lambda (inst) (list (vm-src inst)))))
    ;; Call-family: func/gf register + args
    (dolist (tp '(vm-call vm-tail-call vm-trampoline vm-apply))
      (setf (gethash tp ht) (lambda (inst) (cons (vm-func-reg inst) (vm-args inst)))))
    (setf (gethash 'vm-generic-call ht)
          (lambda (inst) (cons (vm-gf-reg inst) (vm-args inst))))
    (setf (gethash 'vm-fma ht)
          (lambda (inst) (list (vm-a inst) (vm-b inst) (vm-c inst))))
    ;; Multiple values
    (setf (gethash 'vm-values ht)       (lambda (inst) (vm-src-regs inst)))
    ;; Variadic-ish concatenation packed by optimizer
    (setf (gethash 'vm-concatenate ht)
          (lambda (inst)
            (remove nil (or (vm-parts inst)
                            (list (vm-str1 inst) (vm-str2 inst))))))
    (setf (gethash 'vm-char ht)
          (lambda (inst) (list (vm-string-reg inst) (vm-index inst))))
    ;; Object operations
    (setf (gethash 'vm-closure-ref-idx ht) (lambda (inst) (list (vm-closure-reg inst))))
    (setf (gethash 'vm-slot-read ht)    (lambda (inst) (list (vm-obj-reg inst))))
    (setf (gethash 'vm-slot-write ht)
          (lambda (inst) (list (vm-obj-reg inst) (vm-value-reg inst))))
    (setf (gethash 'vm-register-method ht)
          (lambda (inst) (list (vm-gf-reg inst) (vm-method-reg inst))))
    (setf (gethash 'vm-make-obj ht)
          (lambda (inst) (cons (vm-class-reg inst) (mapcar #'cdr (vm-initarg-regs inst)))))
    ;; Compound instructions with optional registers
    (setf (gethash 'vm-make-string ht)
          (lambda (inst) (remove nil (list (vm-src inst) (vm-char inst)))))
    (setf (gethash 'vm-intern-symbol ht)
          (lambda (inst)
            (remove nil (list (vm-src inst) (cl-cc/vm:vm-intern-pkg inst)))))
    (dolist (tp '(vm-add-package-local-nickname vm-remove-package-local-nickname))
      (setf (gethash tp ht)
            (lambda (inst)
              (remove nil (list (cl-cc/vm:vm-local-nickname-pkg inst)
                                (cl-cc/vm:vm-local-nickname-nick inst)
                                (cl-cc/vm:vm-local-nickname-target inst))))))
    (setf (gethash 'vm-make-array ht)
          (lambda (inst)
            (remove nil (list (vm-size-reg inst) (vm-initial-element inst)
                               (vm-fill-pointer-reg inst) (vm-adjustable-reg inst)
                               (vm-element-type-reg inst) (vm-displaced-to-reg inst)))))
    (setf (gethash 'vm-aref ht)
          (lambda (inst) (list (vm-array-reg inst) (vm-index-reg inst))))
    (setf (gethash 'vm-aset ht)
          (lambda (inst) (list (vm-array-reg inst) (vm-index-reg inst) (vm-val-reg inst))))
    (setf (gethash 'vm-fill ht)
          (lambda (inst) (list (vm-array-reg inst) (vm-val-reg inst))))
    (setf (gethash 'vm-copy-vector ht)
          (lambda (inst) (list (vm-dst-array-reg inst)
                               (vm-src-array-reg inst)
                               (vm-len-reg inst))))
    ht))

(defparameter *opt-read-regs-table*
  (%build-opt-read-regs-table)
  "Maps VM instruction type symbols to (lambda (inst) ...) read-reg extractors.
   Used by opt-inst-read-regs for types not covered by the bulk tables.")

(defun %opt-collect-sexp-regs (x dst acc)
  "Recursively collect register-shaped keywords from sexp X into ACC, excluding DST."
  (cond
    ((and (keywordp x) (opt-register-keyword-p x) (not (eq x dst))) (cons x acc))
    ((consp x) (%opt-collect-sexp-regs (cdr x) dst (%opt-collect-sexp-regs (car x) dst acc)))
    (t acc)))

(defun opt-inst-read-regs (inst)
  "Return a list of all register names read by INST.
   Dispatch: zero-read types → nil, move → src, binop → lhs/rhs,
   binary/unary tables, then *opt-read-regs-table* for specific types,
   finally sexp-reflection fallback."
  (let ((tp (type-of inst)))
    (cond
      ;; Zero-read instructions
      ((member tp '(vm-const vm-func-ref vm-get-global vm-values-to-list) :test #'eq)
       nil)
      ;; Move: single source
      ((eq tp 'vm-move) (list (vm-src inst)))
      ;; vm-binop subclasses (vm-add, vm-sub, vm-mul) — handled by typep
      ((typep inst 'vm-binop) (list (vm-lhs inst) (vm-rhs inst)))
      ;; Non-binop binary: data-driven from *opt-binary-lhs-rhs-types*
      ((member tp *opt-binary-lhs-rhs-types* :test #'eq)
       (list (vm-lhs inst) (vm-rhs inst)))
      ;; Unary: data-driven from *opt-unary-src-types*
      ((member tp *opt-unary-src-types* :test #'eq)
       (list (vm-src inst)))
      ;; Per-type table lookup
      (t (if-let ((handler (gethash tp *opt-read-regs-table*)))
           (funcall handler inst)
           ;; Fallback: serialize to sexp and collect all register-shaped keywords
           (let ((dst (opt-inst-dst inst)))
             (handler-case
                 (%opt-collect-sexp-regs (instruction->sexp inst) dst nil)
               (error (condition)
                 (warn "opt-inst-read-regs: sexp-reflection fallback failed for ~S: ~A -- treating as reading no registers, which may be unsafe for liveness/DCE."
                       (type-of inst) condition)
                 nil))))))))

(defun %opt-commutative-inst-p (inst)
  "Return T if INST is a commutative binary instruction."
  (member (type-of inst) *opt-commutative-inst-types* :test #'eq))

;;; ─── Control/Label Type Set ──────────────────────────────────────────────

(deftype opt-control-or-label ()
  "CL type specifier for instructions that break straight-line sequences."
  '(or vm-label vm-jump vm-jump-zero vm-ret vm-call vm-tail-call vm-apply))
