(in-package :cl-cc/optimize/scalar/test)

(it-sequential "fr-685-div-by-const-bounded-dividend-emits-multiply-shift"
  (let* ((c4 (cl-cc/vm:make-vm-const :dst :r0 :value 4))
         (c8 (cl-cc/vm:make-vm-const :dst :r1 :value 8))
         (sum (cl-cc/vm:make-vm-add :dst :x :lhs :r0 :rhs :r1))
         (d3 (cl-cc/vm:make-vm-const :dst :d :value 3))
         (div (cl-cc/vm:make-vm-div :dst :q :lhs :x :rhs :d))
         (result (cl-cc/optimize:opt-pass-div-by-const (list c4 c8 sum d3 div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-truthy)))

(it-sequential "fr-685-mod-by-const-uses-quotient-times-divisor-subtraction"
  (let* ((c4 (cl-cc/vm:make-vm-const :dst :r0 :value 4))
         (c8 (cl-cc/vm:make-vm-const :dst :r1 :value 8))
         (sum (cl-cc/vm:make-vm-add :dst :x :lhs :r0 :rhs :r1))
         (d3 (cl-cc/vm:make-vm-const :dst :d :value 3))
         (mod (cl-cc/vm:make-vm-mod :dst :m :lhs :x :rhs :d))
         (result (cl-cc/optimize:opt-pass-div-by-const (list c4 c8 sum d3 mod))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mod)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-sub)) result) :to-be-truthy)))

(it-sequential "fr-685-div-by-zero-not-transformed"
  (let* ((d0 (cl-cc/vm:make-vm-const :dst :d :value 0))
         (div (cl-cc/vm:make-vm-div :dst :q :lhs :x :rhs :d))
         (result (cl-cc/optimize:opt-pass-div-by-const (list d0 div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-truthy)))

(it-sequential "fr-685-power-of-two-left-for-strength-reduce"
  (let* ((d8 (cl-cc/vm:make-vm-const :dst :d :value 8))
         (div (cl-cc/vm:make-vm-div :dst :q :lhs :x :rhs :d))
         (result (cl-cc/optimize:opt-pass-div-by-const (list d8 div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-falsy)))
