;;;; Unit tests for FR-681 strength-reduction passes.

(in-package :cl-cc/optimize/loops/test)

(it-sequential "fr-681-div-by-const-skips-power-of-two"
  (let* ((c (cl-cc/vm:make-vm-const :dst :d :value 8))
         (div (cl-cc/vm:make-vm-div :dst :q :lhs :x :rhs :d))
         (out (cl-cc/optimize/loops:opt-pass-div-by-const (list c div))))
    (expect (member div out) :to-be-truthy)
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm::vm-ash)) out) :to-be-falsy)))

(it-sequential "fr-681-div-by-const-lowers-bounded-non-power-divisor"
  (let* ((c4 (cl-cc/vm:make-vm-const :dst :a :value 4))
         (c8 (cl-cc/vm:make-vm-const :dst :b :value 8))
         (sum (cl-cc/vm:make-vm-add :dst :x :lhs :a :rhs :b))
         (c3 (cl-cc/vm:make-vm-const :dst :d :value 3))
         (div (cl-cc/vm:make-vm-div :dst :q :lhs :x :rhs :d))
         (out (cl-cc/optimize/loops:opt-pass-div-by-const (list c4 c8 sum c3 div))))
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm::vm-div)) out) :to-be-falsy)
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm::vm-mul)) out) :to-be-truthy)
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm::vm-ash)) out) :to-be-truthy)))

(it-sequential "fr-681-iv-strength-reduce-replaces-loop-mul-with-derived-iv"
  (let* ((mul (make-vm-mul :dst :prod :lhs :i :rhs :scale))
         (insts (list (cl-cc/vm:make-vm-const :dst :i :value 0)
                      (cl-cc/vm:make-vm-const :dst :limit :value 8)
                      (cl-cc/vm:make-vm-const :dst :one :value 1)
                      (cl-cc/vm:make-vm-const :dst :scale :value 4)
                      (cl-cc/vm:make-vm-label :name "loop")
                      (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                      (cl-cc/vm:make-vm-jump-zero :reg :cond :label "exit")
                      mul
                      (cl-cc/vm:make-vm-add :dst :sum :lhs :sum :rhs :prod)
                      (cl-cc/vm:make-vm-add :dst :i :lhs :i :rhs :one)
                      (cl-cc/vm:make-vm-jump :label "loop")
                      (cl-cc/vm:make-vm-label :name "exit")
                      (cl-cc/vm:make-vm-ret :reg :sum)))
         (out (cl-cc/optimize/loops:opt-pass-iv-strength-reduce insts)))
    (expect (member mul out) :to-be-falsy)
    (expect (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-move)
                              (eq (cl-cc/vm::vm-dst inst) :prod)))
                       out) :to-be-truthy)
    (expect (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-add)
                              (not (eq (cl-cc/vm::vm-dst inst) :i))))
                       out) :to-be-truthy)))
