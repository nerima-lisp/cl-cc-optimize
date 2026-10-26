(in-package :cl-cc/optimize)

;;; ─── Effect-Kind Query ───────────────────────────────────────────────────

(defun vm-inst-effect-kind (inst)
  "Return the effect-kind of VM instruction INST.
    Effect kinds: :pure :read-only :alloc :io :write-global :control :unknown.
    Unlisted types (vm-call, vm-apply, vm-generic-call, etc.) default to :unknown."
  (or (gethash (type-of inst) *opt-effect-kind-table*) :unknown))

;;; ─── Purity Predicates ───────────────────────────────────────────────────

(defun opt-inst-pure-p (inst)
  "T if INST has no side effects and produces a deterministic result.
   Extended from the original 2-type (vm-const vm-move) whitelist to cover
   100+ instruction types.  Pure instructions are both CSE-eligible and
   DCE-eligible."
  (eq (vm-inst-effect-kind inst) :pure))

(defun opt-inst-dce-eligible-p (inst)
  "T if INST is eligible for dead code elimination when its result is unused.
   Covers :pure (no side effects) and :alloc (allocation only — if the
   allocated object is never used, the allocation can be removed)."
  (member (vm-inst-effect-kind inst) '(:pure :alloc) :test #'eq))

(defun opt-inst-cse-eligible-p (inst)
  "T if INST is eligible for common subexpression elimination.
   Only :pure instructions guarantee the same result for the same inputs
   regardless of intervening instructions.  :alloc creates distinct objects
   even with the same arguments, so it is NOT CSE-eligible."
  (eq (vm-inst-effect-kind inst) :pure))

;;; ─── Effect-Row Bridge ───────────────────────────────────────────────────

(defun effect-row->effect-kind (effect-row)
  "Convert a cl-cc/type system type-effect-row to an optimizer effect-kind.
   Used when callee effect information is available from the HM type system.
   Compares effect names by string= to avoid cross-package symbol mismatch."
  (let* ((effects (cl-cc/type:type-effect-row-effects effect-row))
          (names   (mapcar (lambda (e)
                             (string-upcase (symbol-name (cl-cc/type:type-effect-op-name e))))
                           effects)))
    (cond
      ((null effects)                          :pure)
      ((member "IO"    names :test #'string=)  :io)
      ((member "STATE" names :test #'string=)  :write-global)
      ((member "ERROR" names :test #'string=)  :control)
      (t                                       :unknown))))
