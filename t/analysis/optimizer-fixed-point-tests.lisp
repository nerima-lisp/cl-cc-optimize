(in-package :cl-cc/optimize/analysis/test)

(it-sequential "ai-compute-fixed-point-derives-add-facts"
  (let* ((program (list (cl-cc/vm:make-vm-const :dst :r0 :value 2)
                        (cl-cc/vm:make-vm-const :dst :r1 :value 5)
                        (cl-cc/vm:make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
         (state (cl-cc/optimize/analysis:ai-compute-fixed-point program))
         (facts (cl-cc/optimize/analysis::ai-state-facts state))
         (r2 (gethash :r2 facts)))
    (expect (getf r2 :interval) :to-equal '(7 . 7))
    (expect (getf r2 :nullness) :to-be :non-null)))
