(in-package #:cl-cc/optimize/analysis)

(defun %opt-stable-print-string (object)
  (with-standard-io-syntax
    (prin1-to-string object)))
