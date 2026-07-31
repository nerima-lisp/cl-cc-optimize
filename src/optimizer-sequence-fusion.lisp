(in-package :cl-cc/optimize)

;;; ─── Pass 1: Constant Folding + Algebraic Simplification ─────────────────

;;;; ─── FR-179: Sequence Operation Fusion ──────────────────────────────────

(defun %opt-sequence-fusion-callee-name (inst)
  "Return the normalized callee name for a direct function reference INST."
  (when (typep inst 'vm-func-ref)
    (let ((label (vm-label-name inst)))
      (cond
        ((symbolp label) (symbol-name label))
        ((stringp label) (string-upcase label))
        (t nil)))))

(defun %opt-sequence-op-name-p (name)
  "Return T when NAME is one of the sequence operations handled by FR-179."
  (member name '("MAPCAR" "REMOVE-IF" "REMOVE-IF-NOT") :test #'string=))

(defun %opt-sequence-fusion-candidate-p (instructions)
  "Return T when INSTRUCTIONS still contain an unfused direct sequence-op chain.

The normal FR-179 implementation is source preserving: compiler macros in the
expander rewrite visible sequence chains before MAPCAR/REMOVE-IF expand into
loops.  This predicate is retained in the optimizer so pass tracing can identify
late direct-call chains produced by non-stdlib frontends without risking an
incorrect closure synthesis at VM level."
  (let ((func-refs (make-hash-table :test #'eq))
        (sequence-call-dsts nil))
    (dolist (inst instructions nil)
      (typecase inst
        (vm-func-ref
         (let ((name (%opt-sequence-fusion-callee-name inst)))
           (when (%opt-sequence-op-name-p name)
             (setf (gethash (vm-dst inst) func-refs) name))))
        (vm-call
         (let ((callee (gethash (vm-func-reg inst) func-refs)))
           (when (%opt-sequence-op-name-p callee)
             (when (some (lambda (arg) (member arg sequence-call-dsts :test #'eq))
                         (vm-args inst))
               (return-from %opt-sequence-fusion-candidate-p t))
             (pushnew (vm-dst inst) sequence-call-dsts :test #'eq))))))))

(defun opt-pass-sequence-fusion (instructions)
  "FR-179: fuse chained sequence operations.

Source-level compiler macros lower MAPCAR/REMOVE-IF chains into one explicit
loop before macro expansion, so the instruction stream normally arrives here
already fused.  The optimizer pass is intentionally conservative: it recognizes
late direct-call chains for reporting, but does not synthesize new closures from
VM bytecode.  Macro-expanded loop streams are therefore preserved exactly after
their intermediate allocation has already been eliminated upstream."
  (when (%opt-sequence-fusion-candidate-p instructions)
    (%opt-report :sequence-fusion
                 "late direct sequence call chain left unchanged; source fusion unavailable"))
  instructions)
