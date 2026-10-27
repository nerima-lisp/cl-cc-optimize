(in-package #:cl-cc/optimize/analysis)

;;; ─── Tree Walker (shared utility) ────────────────────────────────────────

(defun opt-map-tree (fn tree)
  "Apply FN to every leaf of TREE (a possibly-improper nested cons tree)."
  (if (consp tree)
      (cons (opt-map-tree fn (car tree))
            (opt-map-tree fn (cdr tree)))
      (funcall fn tree)))
