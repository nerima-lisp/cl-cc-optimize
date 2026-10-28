(in-package #:cl-cc/optimize/analysis)
;;; -----------------------------------------------------------------------------
;;; Optimizer Register Rewrite Metadata
;;; -----------------------------------------------------------------------------

(defvar *opt-register-slot-table* (make-hash-table :test #'eq)
  "Instruction struct type → VM slot names that may contain register references.

The optimizer uses this to rewrite copy-propagated registers by mutating the
instruction's slots directly, avoiding instruction->sexp/sexp->instruction
allocation in the hot optimization loop.")

(defmacro defopt-register-slots (instruction-type slots)
  "Declare which SLOTS of INSTRUCTION-TYPE may contain VM register references."
  `(let* ((class (%opt-vm-instruction-class ',instruction-type))
          (slot-names
            (mapcar (lambda (slot-name)
                      (let ((slot (find (symbol-name slot-name)
                                        (sb-mop:class-slots class)
                                        :key (lambda (definition)
                                               (symbol-name (sb-mop:slot-definition-name definition)))
                                        :test #'string=)))
                        (unless slot
                          (error "No slot named ~S on instruction type ~S" slot-name ',instruction-type))
                        (sb-mop:slot-definition-name slot)))
                    ',slots)))
     (setf (gethash ',instruction-type *opt-register-slot-table*) slot-names)))
(defun %opt-vm-instruction-class (instruction-type)
  "Resolve an optimizer instruction name to its VM class symbol."
  (or (find-class instruction-type nil)
      (let* ((vm-package (find-package :cl-cc/vm))
             (constructor-name
               (concatenate 'string "MAKE-" (symbol-name instruction-type)))
             (constructor
               (and vm-package (find-symbol constructor-name vm-package)))
             (instruction-package
               (and constructor (symbol-package constructor)))
             (instruction-symbol
               (and instruction-package
                    (find-symbol (symbol-name instruction-type)
                                 instruction-package))))
        (and instruction-symbol
             (find-class instruction-symbol nil)))
      (error "No VM instruction class found for ~S" instruction-type)))

(defun %opt-rewrite-register-tree! (value copies)
  "Destructively rewrite register leaves in VALUE using COPIES.
Returns VALUE and a second value indicating whether anything changed."
  (cond
    ((consp value)
     (multiple-value-bind (new-car car-changed-p)
         (%opt-rewrite-register-tree! (car value) copies)
       (multiple-value-bind (new-cdr cdr-changed-p)
           (%opt-rewrite-register-tree! (cdr value) copies)
         (when car-changed-p (setf (car value) new-car))
         (when cdr-changed-p (setf (cdr value) new-cdr))
         (values value (or car-changed-p cdr-changed-p)))))
    ((keywordp value)
     (multiple-value-bind (replacement present-p) (gethash value copies)
       (if present-p
           (values replacement (not (eq replacement value)))
           (values value nil))))
    (t (values value nil))))

(defun %opt-rewrite-inst-regs-direct! (inst copies slots)
  "Rewrite register slots of INST in place using SLOTS metadata.
Single destination slots are preserved to match the historical sexp rewrite."
  (let ((changed-p nil)
        (dst (opt-inst-dst inst)))
    (dolist (slot slots)
      ;; Registered slot symbols come from the effective class slots, including
      ;; slots defined in VM feature packages.
      (unless (and dst (string= (symbol-name slot) "DST"))
        (let ((value (slot-value inst slot)))
          (multiple-value-bind (new-value slot-changed-p)
              (%opt-rewrite-register-tree! value copies)
            (when slot-changed-p
              (setf (slot-value inst slot) new-value
                    changed-p t))))))
    (values inst changed-p)))

(defopt-register-slots vm-move (dst src))
(defopt-register-slots vm-prefetch (base-reg index-reg))
(defopt-register-slots vm-add (dst lhs rhs))
(defopt-register-slots vm-sub (dst lhs rhs))
(defopt-register-slots vm-mul (dst lhs rhs))
(defopt-register-slots vm-integer-add (dst lhs rhs))
(defopt-register-slots vm-integer-sub (dst lhs rhs))
(defopt-register-slots vm-integer-mul (dst lhs rhs))
(defopt-register-slots vm-integer-mul-high-u (dst lhs rhs))
(defopt-register-slots vm-integer-mul-high-s (dst lhs rhs))
(defopt-register-slots vm-add-checked (dst lhs rhs))
(defopt-register-slots vm-sub-checked (dst lhs rhs))
(defopt-register-slots vm-mul-checked (dst lhs rhs))
(defopt-register-slots vm-float-add (dst lhs rhs))
(defopt-register-slots vm-float-sub (dst lhs rhs))
(defopt-register-slots vm-float-mul (dst lhs rhs))
(defopt-register-slots vm-jump-zero (reg))
(defopt-register-slots vm-select (dst cond-reg then-reg else-reg))
(defopt-register-slots vm-print (reg))
(defopt-register-slots vm-halt (reg))
(defopt-register-slots vm-closure (dst captured))
(defopt-register-slots vm-make-closure (dst env-regs))
(defopt-register-slots vm-closure-ref-idx (dst closure))
(defopt-register-slots vm-call (dst func args))
(defopt-register-slots vm-tail-call (dst func args))
(defopt-register-slots vm-trampoline (dst func args))
(defopt-register-slots vm-ret (reg))
(defopt-register-slots vm-func-ref (dst))
(defopt-register-slots vm-values (dst src-regs))
(defopt-register-slots vm-values-typep (dst src))
(defopt-register-slots vm-mv-bind (dst-regs))
(defopt-register-slots vm-values-to-list (dst))
(defopt-register-slots vm-spread-values (dst src))
(defopt-register-slots vm-values-regs (reg0 reg1 reg2))
(defopt-register-slots vm-mv-bind-regs (dst-regs))
(defopt-register-slots vm-ensure-values (src))
(defopt-register-slots vm-call-next-method (dst args-reg))
(defopt-register-slots vm-next-method-p (dst))
(defopt-register-slots vm-apply (dst func args))
(defopt-register-slots vm-register-function (src))
(defopt-register-slots vm-set-global (src))
(defopt-register-slots vm-get-global (dst))

(defun opt-rewrite-inst-regs (inst copies)
  "Return INST with all source registers replaced by their canonical copies.
   Uses registered direct slot metadata when available to avoid sexp allocation.
   Falls back to the historical sexp roundtrip for unregistered instructions."
  (let ((c (lambda (x)
             (multiple-value-bind (replacement present-p) (gethash x copies)
               (if present-p replacement x)))))
    (handler-case
        (if-let ((slots (gethash (type-of inst) *opt-register-slot-table*)))
          (%opt-rewrite-inst-regs-direct! inst copies slots)
          (let* ((sexp     (instruction->sexp inst))
                 (has-dst  (not (null (opt-inst-dst inst))))
                 ;; Rewrite all leaves; for instructions with a dst, the dst sits at
                 ;; position 1 (immediately after the opcode tag) — leave it intact.
                 (new-sexp (if has-dst
                               (list* (first sexp) (second sexp) (opt-map-tree c (cddr sexp)))
                               (cons  (first sexp) (opt-map-tree c (cdr sexp))))))
            (if (equal sexp new-sexp) inst (sexp->instruction new-sexp))))
      (error () inst))))

;; opt-pass-dead-store-elim and opt-pass-store-to-load-forward are in
;; optimizer-memory-dse.lisp (loaded next).
