(in-package #:cl-cc/optimize/analysis)

;;; ─── Memory Optimization Passes ─────────────────────────────────────────────
;;;
;;; Extracted from optimizer-memory-alias.lisp.
;;; Depends on: optimizer-memory-alias.lisp (alias analysis: opt-compute-heap-aliases,
;;;   opt-slot-alias-key, opt-rewrite-inst-regs, opt-inst-dst, opt-inst-pure-p).
;;; Load order: immediately after optimizer-memory-alias.lisp.

;;; ─── Shared mem-pass-state struct and helpers ────────────────────────────────

(defvar *opt-bounds-check-eliminable-metadata* (make-hash-table :test #'eq)
  "EQ side table for BCE annotations on existing VM instructions.")

(defun opt-mark-bounds-check-eliminable (inst &key array-reg index-reg length-reg block)
  "Annotate INST as having a proven redundant array bounds check.

The VM has no unchecked array instruction yet, so BCE records conservative
side-table metadata and returns INST unchanged. Downstream codegen can query this
metadata without requiring cross-package instruction-shape changes."
  (setf (gethash inst *opt-bounds-check-eliminable-metadata*)
        (list :bounds-check-eliminable t
              :bce-eliminable t
              :array-reg array-reg
              :index-reg index-reg
              :length-reg length-reg
              :block block))
  inst)

(defun opt-bounds-check-eliminable-metadata (inst)
  "Return BCE metadata for INST, or NIL when INST is not annotated."
  (gethash inst *opt-bounds-check-eliminable-metadata*))

(defun opt-bounds-check-eliminable-marked-p (inst)
  "Return T when INST has been annotated by the BCE pass."
  (let ((metadata (opt-bounds-check-eliminable-metadata inst)))
    (or (getf metadata :bounds-check-eliminable)
        (getf metadata :bce-eliminable))))
