(in-package #:cl-cc/optimize/loops/test)

(it-sequential "optimizer-sequence-fusion-recognizes-map-filter-chain"
  (let* ((instrs (list (make-vm-func-ref :dst :f-map :label 'mapcar)
                       (make-vm-func-ref :dst :f-filter :label 'remove-if)
                       (make-vm-call :dst :mapped :func :f-map :args '(:fn :xs))
                       (make-vm-call :dst :filtered :func :f-filter :args '(:pred :mapped))))
         (out (cl-cc/optimize/loops:opt-pass-sequence-fusion instrs)))
    (expect (cl-cc/optimize/loops::%opt-sequence-fusion-candidate-p instrs) :to-be-truthy)
    (expect out :to-be instrs)))

(it-sequential "optimizer-autovec-op-kind-table add"
  (destructuring-bind (inst expected-op) (list (make-vm-add  :dst :r0 :lhs :r1 :rhs :r2) :add)
    (expect (cl-cc/optimize/loops::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-autovec-op-kind-table sub"
  (destructuring-bind (inst expected-op) (list (make-vm-sub  :dst :r0 :lhs :r1 :rhs :r2) :sub)
    (expect (cl-cc/optimize/loops::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-autovec-op-kind-table mul"
  (destructuring-bind (inst expected-op) (list (make-vm-mul  :dst :r0 :lhs :r1 :rhs :r2) :mul)
    (expect (cl-cc/optimize/loops::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-autovec-op-kind-table logand"
  (destructuring-bind (inst expected-op) (list (make-vm-logand :dst :r0 :lhs :r1 :rhs :r2) :logand)
    (expect (cl-cc/optimize/loops::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-autovec-op-kind-table logior"
  (destructuring-bind (inst expected-op) (list (make-vm-logior :dst :r0 :lhs :r1 :rhs :r2) :logior)
    (expect (cl-cc/optimize/loops::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-autovec-op-kind-table logxor"
  (destructuring-bind (inst expected-op) (list (make-vm-logxor :dst :r0 :lhs :r1 :rhs :r2) :logxor)
    (expect (cl-cc/optimize/loops::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-autovec-op-kind-table min"
  (destructuring-bind (inst expected-op) (list (make-vm-min  :dst :r0 :lhs :r1 :rhs :r2) :min)
    (expect (cl-cc/optimize/loops::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-autovec-op-kind-table max"
  (destructuring-bind (inst expected-op) (list (make-vm-max  :dst :r0 :lhs :r1 :rhs :r2) :max)
    (expect (cl-cc/optimize/loops::%opt-autovec-op-kind inst) :to-be expected-op)))
