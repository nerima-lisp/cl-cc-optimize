(in-package #:cl-cc/optimize/scalar/test)

(it-sequential "optimizer-algebraic-type-producer-identity null-of-cons"
  (destructuring-bind (expected-value producer predicate) (list 0 (make-vm-cons :dst :r1 :car-src :r0 :cdr-src :r0) (make-vm-null-p :dst :r2 :src :r1))
    (let ((out (cl-cc/optimize/scalar:opt-pass-fold (list producer predicate))))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r2)
                              (eql (cl-cc/vm::vm-value i) expected-value)))
                       out) :to-be-truthy)
     (expect (some (lambda (i) (typep i (type-of predicate))) out) :to-be-falsy))))

(it-sequential "optimizer-algebraic-type-producer-identity consp-of-cons"
  (destructuring-bind (expected-value producer predicate) (list 1 (make-vm-cons :dst :r1 :car-src :r0 :cdr-src :r0) (make-vm-cons-p :dst :r2 :src :r1))
    (let ((out (cl-cc/optimize/scalar:opt-pass-fold (list producer predicate))))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r2)
                              (eql (cl-cc/vm::vm-value i) expected-value)))
                       out) :to-be-truthy)
     (expect (some (lambda (i) (typep i (type-of predicate))) out) :to-be-falsy))))

(it-sequential "optimizer-algebraic-type-producer-identity listp-of-cons"
  (destructuring-bind (expected-value producer predicate) (list 1 (make-vm-cons :dst :r1 :car-src :r0 :cdr-src :r0) (make-vm-listp :dst :r2 :src :r1))
    (let ((out (cl-cc/optimize/scalar:opt-pass-fold (list producer predicate))))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r2)
                              (eql (cl-cc/vm::vm-value i) expected-value)))
                       out) :to-be-truthy)
     (expect (some (lambda (i) (typep i (type-of predicate))) out) :to-be-falsy))))

(it-sequential "optimizer-algebraic-type-producer-identity stringp-of-concatenate"
  (destructuring-bind (expected-value producer predicate) (list 1 (make-vm-concatenate :dst :r1 :str1 :r0 :str2 :r0) (make-vm-stringp :dst :r2 :src :r1))
    (let ((out (cl-cc/optimize/scalar:opt-pass-fold (list producer predicate))))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r2)
                              (eql (cl-cc/vm::vm-value i) expected-value)))
                       out) :to-be-truthy)
     (expect (some (lambda (i) (typep i (type-of predicate))) out) :to-be-falsy))))

(it-sequential "optimizer-algebraic-type-producer-identity vectorp-of-coerce-to-vector"
  (destructuring-bind (expected-value producer predicate) (list 1 (make-vm-coerce-to-vector :dst :r1 :src :r0) (make-vm-vectorp :dst :r2 :src :r1))
    (let ((out (cl-cc/optimize/scalar:opt-pass-fold (list producer predicate))))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r2)
                              (eql (cl-cc/vm::vm-value i) expected-value)))
                       out) :to-be-truthy)
     (expect (some (lambda (i) (typep i (type-of predicate))) out) :to-be-falsy))))

(it-sequential "optimizer-pass-produces-const fold-rational-unary"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 3/4))
                    (u1  (cl-cc/vm:make-vm-numerator :dst :r1 :src :r0))
                    (ret (make-vm-ret :reg :r1)))
               (cl-cc/optimize/scalar:opt-pass-fold (list c1 u1 ret)))) :r1 3)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-pass-produces-const fold-car-constant-cons"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value '(11 . 12)))
                    (u1  (make-vm-car :dst :r1 :src :r0))
                    (ret (make-vm-ret :reg :r1)))
               (cl-cc/optimize/scalar:opt-pass-fold (list c1 u1 ret)))) :r1 11)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-pass-produces-const fold-cdr-constant-cons"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value '(11 . 12)))
                    (u1  (make-vm-cdr :dst :r1 :src :r0))
                    (ret (make-vm-ret :reg :r1)))
               (cl-cc/optimize/scalar:opt-pass-fold (list c1 u1 ret)))) :r1 12)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-pass-produces-const fold-rational-binary"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 8))
                    (c2  (make-vm-const :dst :r1 :value 12))
                    (g   (cl-cc/vm:make-vm-gcd :dst :r2 :lhs :r0 :rhs :r1))
                    (ret (make-vm-ret :reg :r2)))
               (cl-cc/optimize/scalar:opt-pass-fold (list c1 c2 g ret)))) :r2 4)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-pass-produces-const fold-rational-cl-div"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 3))
                    (c2  (make-vm-const :dst :r1 :value 4))
                    (d   (cl-cc/vm::make-vm-cl-div :dst :r2 :lhs :r0 :rhs :r1))
                    (ret (make-vm-ret :reg :r2)))
               (cl-cc/optimize/scalar:opt-pass-fold (list c1 c2 d ret)))) :r2 3/4)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-pass-produces-const fold-floor-div"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value -7))
                    (c2  (make-vm-const :dst :r1 :value 2))
                    (d   (cl-cc/vm:make-vm-div :dst :r2 :lhs :r0 :rhs :r1))
                    (ret (make-vm-ret :reg :r2)))
               (cl-cc/optimize/scalar:opt-pass-fold (list c1 c2 d ret)))) :r2 -4)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-fold-eligible-predicates-table string-length-string"
  (destructuring-bind (inst value expected) (list (make-vm-string-length  :dst :r0 :src :r1) "hello" t)
    (expect (cl-cc/optimize/analysis:%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table string-length-number"
  (destructuring-bind (inst value expected) (list (make-vm-string-length  :dst :r0 :src :r1) 42 nil)
    (expect (cl-cc/optimize/analysis:%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table vm-not-anything"
  (destructuring-bind (inst value expected) (list (make-vm-not :dst :r0 :src :r1) 99 t)
    (expect (cl-cc/optimize/analysis:%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table vm-not-nil"
  (destructuring-bind (inst value expected) (list (make-vm-not :dst :r0 :src :r1) nil t)
    (expect (cl-cc/optimize/analysis:%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table car-cons"
  (destructuring-bind (inst value expected) (list (make-vm-car :dst :r0 :src :r1) '(a . b) t)
    (expect (cl-cc/optimize/analysis:%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table car-nil"
  (destructuring-bind (inst value expected) (list (make-vm-car :dst :r0 :src :r1) nil t)
    (expect (cl-cc/optimize/analysis:%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table car-number"
  (destructuring-bind (inst value expected) (list (make-vm-car :dst :r0 :src :r1) 42 nil)
    (expect (cl-cc/optimize/analysis:%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table vm-neg-number"
  (destructuring-bind (inst value expected) (list (make-vm-neg :dst :r0 :src :r1) 7 t)
    (expect (cl-cc/optimize/analysis:%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table vm-neg-string"
  (destructuring-bind (inst value expected) (list (make-vm-neg :dst :r0 :src :r1) "x" nil)
    (expect (cl-cc/optimize/analysis:%fold-unary-constant-eligible-p inst value) :to-equal expected)))
