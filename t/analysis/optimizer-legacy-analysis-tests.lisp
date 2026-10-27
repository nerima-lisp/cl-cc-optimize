(in-package #:cl-cc/optimize/analysis/test)

(it-sequential "optimizer-demand-analysis-records-absent-params"
  (let* ((instrs (list (make-vm-func-ref :dst :f :label 'demand-fixture :params '(:x :y))
                       (make-vm-label :name 'demand-fixture)
                       (make-vm-ret :reg :x)))
         (summaries (cl-cc/optimize/analysis:opt-analyze-program-demand instrs))
         (summary (gethash 'demand-fixture summaries)))
    (expect summary :to-be-truthy)
    (expect (member :x (cl-cc/optimize/analysis:opt-demand-summary-strict-params summary)) :to-be-truthy)
    (expect (member :y (cl-cc/optimize/analysis:opt-demand-summary-absent-params summary)) :to-be-truthy)))

(it-sequential "optimizer-hot-cold-layout-keeps-signal-block-last"
  (let* ((hot   (make-vm-label :name "hot"))
         (jump  (make-vm-jump-zero :reg :r0 :label "cold"))
         (work  (make-vm-const :dst :r1 :value 1))
         (toend (make-vm-jump :label "end"))
         (cold  (make-vm-label :name "cold"))
         (sig   (cl-cc/vm:make-vm-signal-error :error-reg :r1))
         (end   (make-vm-label :name "end"))
         (ret   (make-vm-ret :reg :r1))
         (cfg   (cl-cc/optimize/analysis:cfg-build (list hot jump work toend cold sig end ret)))
         (_idom (cl-cc/optimize/analysis:cfg-compute-dominators cfg))
         (_loop (cl-cc/optimize/analysis:cfg-compute-loop-depths cfg))
         (out   (cl-cc/optimize/analysis:cfg-flatten-hot-cold cfg))
         (labels (loop for inst in out
                        when (typep inst 'cl-cc/vm::vm-label)
                        collect (cl-cc/vm::vm-name inst))))
    (declare (ignore _idom _loop))
    (expect (member "hot" labels :test #'equal) :to-be-truthy)
    (expect (member "cold" labels :test #'equal) :to-be-truthy)
    (expect (member "end" labels :test #'equal) :to-be-truthy)
    (expect (< (position "hot" labels :test #'equal)
                    (position "cold" labels :test #'equal)) :to-be-truthy)
    (expect (< (position "end" labels :test #'equal)
                    (position "cold" labels :test #'equal)) :to-be-truthy)))

(it-sequential "optimizer-control-or-label-deftype label"
  (destructuring-bind (inst expected) (list (make-vm-label :name "l") t)
    (expect (typep inst 'cl-cc/optimize/analysis::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-control-or-label-deftype jump"
  (destructuring-bind (inst expected) (list (make-vm-jump :label "l") t)
    (expect (typep inst 'cl-cc/optimize/analysis::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-control-or-label-deftype jump-zero"
  (destructuring-bind (inst expected) (list (make-vm-jump-zero :reg :r0 :label "l") t)
    (expect (typep inst 'cl-cc/optimize/analysis::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-control-or-label-deftype ret"
  (destructuring-bind (inst expected) (list (make-vm-ret :reg :r0) t)
    (expect (typep inst 'cl-cc/optimize/analysis::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-control-or-label-deftype call"
  (destructuring-bind (inst expected) (list (make-vm-call :dst :r0 :func :r1 :args nil) t)
    (expect (typep inst 'cl-cc/optimize/analysis::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-control-or-label-deftype const"
  (destructuring-bind (inst expected) (list (make-vm-const :dst :r0 :value 42) nil)
    (expect (typep inst 'cl-cc/optimize/analysis::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-control-or-label-deftype add"
  (destructuring-bind (inst expected) (list (make-vm-add  :dst :r0 :lhs :r1 :rhs :r2) nil)
    (expect (typep inst 'cl-cc/optimize/analysis::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-control-or-label-deftype move"
  (destructuring-bind (inst expected) (list (make-vm-move :dst :r0 :src :r1) nil)
    (expect (typep inst 'cl-cc/optimize/analysis::opt-control-or-label) :to-equal expected)))
