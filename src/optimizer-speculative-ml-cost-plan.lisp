;;;; optimizer-speculative-ml-cost-plan.lisp — learned/ML-style inline and
;;;; codegen cost planning descriptors

(in-package :cl-cc/optimize)

(defun opt-ml-inline-score-plan (&key features model-version)
  "Return a deterministic MLGO-style inline scoring descriptor."
  (let ((feature-count (length (or features nil))))
    (list :kind :ml-inline-score
          :model-version (or model-version "mlgo-v1")
          :feature-count feature-count
          :score (+ 10 (* 2 feature-count)))))

(defun opt-learned-codegen-cost-plan (&key opcode-features target)
  "Return learned cost descriptor used by backend codegen selection policies."
  (let* ((feature-count (length (or opcode-features nil)))
         (arch (or target :generic))
         (base (ecase arch
                 ((:x86-64) 8)
                 ((:aarch64) 7)
                 ((:generic) 10))))
    (list :kind :learned-codegen-cost
          :target arch
          :feature-count feature-count
          :predicted-cost (+ base feature-count))))
