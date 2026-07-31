;;;; effects-purity-inference.lisp — transitive user-function purity inference

(in-package :cl-cc/optimize)

;;; ─── FR-152: Transitive Function Purity Inference ──────────────────────────
;;;
;;; NOTE: The existing optimizer-purity.lisp provides call-graph construction
;;; for inlining eligibility (opt-build-call-graph).  This file provides a
;;; separate purity classification for CSE/DCE — answering "can this function
;;; call be elided?" rather than "should we inline it?".  The two modules
;;; serve different optimization decisions and do not conflict.
;;;
;;; Build a per-function instruction table and propagate purity transitively
;;; through the call graph.  Pure user-defined functions are then eligible for
;;; CSE/DCE the same way builtin pure functions are.

(defvar *function-instruction-table* (make-hash-table :test #'eq)
  "Maps function names (symbols) to their compiled instruction vectors.
Filled by the compile/optimize pipeline so the purity analysis can inspect
function bodies without recompiling.")

(defvar *user-function-purity-cache* (make-hash-table :test #'eq)
  "Maps function names (symbols) to effect-kind keywords.
Populated lazily by COMPUTE-FUNCTION-PURITY.
Can be invalidated via INVALIDATE-FUNCTION-PURITY when a function is redefined.")

(defvar *purity-analysis-in-progress* nil
  "Stack of function names currently being analyzed.
Used to detect mutual recursion / cycles; when a function appears here,
it is conservatively marked :unknown to break the cycle.")

;;; ── Purity effect-kind severity ordering ──────────────────────────────────
;;;
;;; For merging multiple effect kinds, we take the maximum (most side-effecting).
;;;   :pure < :alloc < :read-only < :control < :write-global < :io < :unknown

(defparameter *effect-kind-severity*
  '((:pure        . 0)
    (:alloc       . 1)
    (:read-only   . 2)
    (:control     . 3)
    (:write-global . 4)
    (:io          . 5)
    (:unknown     . 6))
  "Severity ordering for effect kinds. Higher = more side effects.")

(defun effect-kind-max (a b)
  "Return the more severe (higher severity) of two effect kinds."
  (let ((sa (cdr (assoc a *effect-kind-severity*)))
        (sb (cdr (assoc b *effect-kind-severity*))))
    (if (>= sa sb) a b)))

;;; ── Callee name extraction from vm-call / vm-func-ref ─────────────────────

(defun %extract-callee-name (inst)
  "Extract the callee function name from INST if it is a direct call/ref.
Returns a symbol or NIL.
NOTE: vm-call's func field is typically a register, not a direct func-ref.
Only vm-func-ref instructions (used in closure construction) carry a label name
that can be statically resolved without data-flow analysis."
  (cond
    ((typep inst 'cl-cc/vm:vm-func-ref)
     (let ((label (cl-cc/vm:vm-label-name inst)))
       (when (symbolp label) label)))
    ;; vm-call and vm-tail-call use register operands for the callee;
    ;; static callee resolution requires def-use tracking (not done here).
    ;; vm-generic-call and vm-apply are inherently dynamic.
    (t nil)))

;;; ── Main purity analysis ──────────────────────────────────────────────────

(defun compute-function-purity (fn-name &key (instructions nil))
  "Analyze FN-NAME's instruction sequence and return its effect-kind.
Propagates purity transitively: if this function only contains :pure
instructions and calls only :pure callees, it is :pure.

INSTRUCTIONS may be provided (when freshly compiled) or looked up from
*FUNCTION-INSTRUCTION-TABLE*.

Returns one of: :pure :alloc :read-only :control :write-global :io :unknown."

  ;; Check the cache first
  (when (gethash fn-name *user-function-purity-cache*)
    (return-from compute-function-purity
      (gethash fn-name *user-function-purity-cache*)))

  ;; Check builtin function properties
  (let ((builtin-kind (known-function-effect-kind fn-name)))
    (when (not (eq builtin-kind :unknown))
      (setf (gethash fn-name *user-function-purity-cache*) builtin-kind)
      (return-from compute-function-purity builtin-kind)))

  ;; Cycle detection: if already analyzing this function, return :unknown
  (when (member fn-name *purity-analysis-in-progress* :test #'eq)
    (return-from compute-function-purity :unknown))

  ;; Get or look up instructions
  (unless instructions
    (setf instructions (gethash fn-name *function-instruction-table*)))

  ;; If no instructions available, cannot analyze
  (unless instructions
    (setf (gethash fn-name *user-function-purity-cache*) :unknown)
    (return-from compute-function-purity :unknown))

  ;; Analyze
  (let ((worst-kind :pure))
    (push fn-name *purity-analysis-in-progress*)
    (unwind-protect
         (dolist (inst instructions worst-kind)
           (let ((kind (vm-inst-effect-kind inst)))
             ;; For vm-call to user functions, recursively analyze callee
             (when (and (eq kind :unknown)
                        (typep inst '(or vm-call vm-tail-call)))
               (let ((callee (%extract-callee-name inst)))
                 (when callee
                   (setf kind (compute-function-purity callee)))))
             ;; Merge effect kinds (track worst)
             (setf worst-kind (effect-kind-max worst-kind kind))
             ;; Early exit: if already the worst possible, no need to continue
             (when (eq worst-kind :unknown)
               (return))))
      (pop *purity-analysis-in-progress*))
    ;; Cache the result
    (setf (gethash fn-name *user-function-purity-cache*) worst-kind)
    worst-kind))

(defun invalidate-function-purity (fn-name)
  "Remove FN-NAME from the purity cache so it is recomputed on next query.
Also clears the instruction table entry."
  (remhash fn-name *user-function-purity-cache*)
  (remhash fn-name *function-instruction-table*))

(defun register-function-instructions (fn-name instructions)
  "Store INSTRUCTIONS for FN-NAME in the instruction table.
Should be called by the compile/optimize pipeline after generating code for a function."
  (setf (gethash fn-name *function-instruction-table*) instructions)
  ;; Clear any stale purity cache entry
  (remhash fn-name *user-function-purity-cache*))

(defun clear-all-purity-cache ()
  "Reset all purity caches. Used between compilation units or for testing."
  (clrhash *function-instruction-table*)
  (clrhash *user-function-purity-cache*))

(defun call-site-effect-kind (inst)
  "Return the effect-kind of a vm-call/vm-tail-call INST by analyzing the callee.
If the callee is a builtin with known properties, returns that kind.
If the callee is a user function, returns its computed purity.
Otherwise returns :unknown."
  (unless (typep inst '(or vm-call vm-tail-call))
    (return-from call-site-effect-kind :unknown))
  (let ((callee (%extract-callee-name inst)))
    (unless callee
      (return-from call-site-effect-kind :unknown))
    ;; Try builtin properties first
    (let ((builtin-kind (known-function-effect-kind callee)))
      (unless (eq builtin-kind :unknown)
        (return-from call-site-effect-kind builtin-kind)))
    ;; Try user function purity
    (compute-function-purity callee)))

;;; ── Convenience exports for the optimizer ─────────────────────────────────

(defun user-function-pure-p (fn-name)
  "Return T when FN-NAME is a pure function (may be used for CSE/DCE)."
  (eq (compute-function-purity fn-name) :pure))

(defun user-function-cse-eligible-p (fn-name)
  "Return T when FN-NAME is safe for common subexpression elimination."
  (user-function-pure-p fn-name))

(defun user-function-dce-eligible-p (fn-name)
  "Return T when FN-NAME's result may be dropped without side effects."
  (member (compute-function-purity fn-name) '(:pure :alloc) :test #'eq))
