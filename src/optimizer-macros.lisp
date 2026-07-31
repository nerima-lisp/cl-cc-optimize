(in-package :cl-cc/optimize)

;;; ─── Cross-Cutting Macros ────────────────────────────────────────────────
;;;
;;; Macros used across many optimizer pass files, defined early so every
;;; later file in the :serial t load order can expand calls to them.

(defmacro define-inst-type-predicate (name types &optional doc)
  "Define NAME as a one-line instruction-type test: (typep inst 'TYPES)."
  `(defun ,name (inst)
     ,@(when doc (list doc))
     (typep inst ',types)))
