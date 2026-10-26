(in-package :cl-cc/optimize)

;;; ─── Cross-Cutting Macros and Helpers ───────────────────────────────────
;;;
;;; Macros and small shared functions used across many optimizer pass files,
;;; defined early so every later file in the :serial t load order can use
;;; them.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %single-let-binding (macro-name bindings)
    "Validate BINDINGS is the one-element ((VAR EXPR)) binding list WHEN-LET
and IF-LET expect, and return the (VAR EXPR) pair. Signals a compile-time
error naming MACRO-NAME and the offending BINDINGS otherwise."
    (unless (and (consp bindings) (null (cdr bindings))
                 (consp (first bindings)) (symbolp (first (first bindings)))
                 (consp (rest (first bindings))) (null (cddr (first bindings))))
      (error "~S: expected exactly one (VAR EXPR) binding, got ~S"
             macro-name bindings))
    (first bindings)))

(defmacro when-let (bindings &body body)
  "Bind VAR to EXPR per the single (VAR EXPR) pair in BINDINGS (the
one-binding-only Alexandria WHEN-LET calling convention: BINDINGS is
((VAR EXPR))); when VAR is non-NIL, evaluate BODY with VAR bound and return
the value of its last form, otherwise return NIL without evaluating BODY.

This is a named-binding sibling of the classic anaphoric AWHEN: instead of an
implicit IT, the caller keeps their own descriptive binding name. It exists
to name the extremely common \"bind once, branch on that same binding\" shape
in this codebase's instruction-walking passes:

  (let ((dst (opt-inst-dst inst)))
    (when dst ...))
  =>
  (when-let ((dst (opt-inst-dst inst)))
    ...)

EXPR is evaluated exactly once. No symbol besides VAR (user-supplied) is
introduced into BODY's scope, so there is nothing to gensym."
  (destructuring-bind (var expr) (%single-let-binding 'when-let bindings)
    `(let ((,var ,expr))
       (when ,var ,@body))))

(defmacro if-let (bindings then &optional else)
  "Bind VAR to EXPR per the single (VAR EXPR) pair in BINDINGS (see
WHEN-LET), then evaluate THEN with VAR bound if VAR is non-NIL, or evaluate
ELSE (VAR remains bound, exactly as in the LET this sugars over) otherwise.

Named-binding sibling of the classic anaphoric AIF; see WHEN-LET. EXPR is
evaluated exactly once."
  (destructuring-bind (var expr) (%single-let-binding 'if-let bindings)
    `(let ((,var ,expr))
       (if ,var ,then ,else))))

(defmacro define-inst-type-predicate (name types &optional doc)
  "Define NAME as a one-line instruction-type test: (typep inst 'TYPES)."
  `(defun ,name (inst)
     ,@(when doc (list doc))
     (typep inst ',types)))

(defun %opt-track-known-callee-label (inst name-to-label reg->label)
  "Update REG->LABEL with the known-callee-label fact produced by INST.

Shared by FR-606 dead-argument elimination, call-site splitting, and sealed
generic-function devirtualization: each pass tracks, per register, the
statically-known function label it holds (from a closure/func-ref literal, a
named constant resolved via NAME-TO-LABEL, or propagated through a move), and
forgets the fact on any other definition of the register."
  (typecase inst
    ((or vm-closure vm-func-ref)
     (setf (gethash (vm-dst inst) reg->label) (vm-label-name inst)))
    (vm-const
     (if-let ((label (and (symbolp (vm-value inst))
                          (gethash (vm-value inst) name-to-label))))
       (setf (gethash (vm-dst inst) reg->label) label)
       (remhash (vm-dst inst) reg->label)))
    (vm-move
     (multiple-value-bind (label found-p) (gethash (vm-src inst) reg->label)
       (if found-p
           (setf (gethash (vm-dst inst) reg->label) label)
           (remhash (vm-dst inst) reg->label))))
    (t
     (when-let ((dst (opt-inst-dst inst)))
       (remhash dst reg->label)))))
