;;;; tests/unit/optimize/optimizer-loop-transforms-tests.lisp
;;;; Unit tests for loop optimization passes: rotation, peeling, unrolling.
;;;;
;;;; Covers: opt-pass-loop-rotation, opt-pass-loop-peel, opt-pass-loop-unrolling.

(cl:in-package :cl-cc/optimize/loops/test)

;;; ─── Helper: build instruction sequences ─────────────────────────────────

(defun %loop-transforms-make-while-loop-insts ()
  "Build a simple while-loop instruction sequence used by loop optimization tests.
Pattern: loop on :r0 while (integer-p :r0), adding :r2 each iteration."
  (list (cl-cc/vm:make-vm-label :name "Lh")
        (cl-cc/vm:make-vm-integer-p :dst :r1 :src :r0)
        (cl-cc/vm:make-vm-jump-zero :reg :r1 :label "Lexit")
        (cl-cc/vm:make-vm-add :dst :r0 :lhs :r0 :rhs :r2)
        (cl-cc/vm:make-vm-jump :label "Lh")
        (cl-cc/vm:make-vm-label :name "Lexit")
        (cl-cc/vm:make-vm-ret :reg :r0)))

(defun %loop-transforms-make-counted-loop-insts (&key (lim 3))
  "Build a counted-loop instruction sequence for loop unrolling tests.
Pattern: count :i from 0 to LIM by 1, accumulating in :sum."
  (list (cl-cc/vm:make-vm-const :dst :i   :value 0)
        (cl-cc/vm:make-vm-const :dst :lim :value lim)
        (cl-cc/vm:make-vm-const :dst :one :value 1)
        (cl-cc/vm:make-vm-label :name "Lh")
        (make-vm-lt :dst :c :lhs :i :rhs :lim)
        (cl-cc/vm:make-vm-jump-zero :reg :c :label "Lexit")
        (cl-cc/vm:make-vm-add :dst :sum :lhs :sum :rhs :i)
        (cl-cc/vm:make-vm-add :dst :i   :lhs :i   :rhs :one)
        (cl-cc/vm:make-vm-jump :label "Lh")
        (cl-cc/vm:make-vm-label :name "Lexit")
        (cl-cc/vm:make-vm-ret :reg :sum)))

;;; ─── Loop rotation ────────────────────────────────────────────────────────

(it-sequential "loop-rotation-rotates-simple-while-shape"
  (let* ((insts (%loop-transforms-make-while-loop-insts))
         (out (cl-cc/optimize/control-flow:opt-pass-loop-rotation insts))
         (first-inst (first out))
         (jumps-to-lh (count-if (lambda (i)
                                  (and (typep i 'cl-cc/vm::vm-jump)
                                       (equal (cl-cc/vm::vm-label-name i) "Lh")))
                                out))
         (guard-jumps (count-if (lambda (i)
                                  (and (typep i 'cl-cc/vm::vm-jump-zero)
                                       (equal (cl-cc/vm::vm-label-name i) "Lexit")))
                                out)))
    (expect (typep first-inst 'cl-cc/vm::vm-jump) :to-be-truthy)
    (expect (= 0 jumps-to-lh) :to-be-truthy)
    (expect (= 1 guard-jumps) :to-be-truthy)))

(it-sequential "loop-passes-noop-on-nonmatching-shape rotation"
  (destructuring-bind (pass) (list #'cl-cc/optimize/control-flow:opt-pass-loop-rotation)
    (let* ((insts (list (cl-cc/vm:make-vm-label :name "A")
                      (cl-cc/vm:make-vm-const :dst :r0 :value 1)
                      (cl-cc/vm:make-vm-jump :label "B")
                      (cl-cc/vm:make-vm-label :name "B")
                      (cl-cc/vm:make-vm-ret :reg :r0)))
         (out (funcall pass insts)))
    (expect (= (length insts) (length out)) :to-be-truthy))))

(it-sequential "loop-passes-noop-on-nonmatching-shape peeling"
  (destructuring-bind (pass) (list #'cl-cc/optimize/loops:opt-pass-loop-peel)
    (let* ((insts (list (cl-cc/vm:make-vm-label :name "A")
                      (cl-cc/vm:make-vm-const :dst :r0 :value 1)
                      (cl-cc/vm:make-vm-jump :label "B")
                      (cl-cc/vm:make-vm-label :name "B")
                      (cl-cc/vm:make-vm-ret :reg :r0)))
         (out (funcall pass insts)))
    (expect (= (length insts) (length out)) :to-be-truthy))))

;;; ─── Loop peeling ─────────────────────────────────────────────────────────

(it-sequential "loop-peeling-duplicates-first-iteration-for-simple-while"
  (let* ((insts (%loop-transforms-make-while-loop-insts))
         (out (cl-cc/optimize/loops:opt-pass-loop-peel insts))
         (jz-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out))
         (add-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out)))
    (expect (= 2 jz-count) :to-be-truthy)
    (expect (= 2 add-count) :to-be-truthy)))

;;; ─── Loop unrolling ───────────────────────────────────────────────────────

(it-sequential "loop-unrolling-fully-unrolls-small-counted-loop"
  (let* ((insts (%loop-transforms-make-counted-loop-insts :lim 3))
         (out (cl-cc/optimize/loops:opt-pass-loop-unrolling insts))
         (jump-to-lh (count-if (lambda (x)
                                (and (typep x 'cl-cc/vm::vm-jump)
                                     (equal (cl-cc/vm::vm-label-name x) "Lh")))
                              out))
         (lt-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-lt)) out))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out)))
    (expect (= 0 jump-to-lh) :to-be-truthy)
    (expect (= 0 lt-count) :to-be-truthy)
    (expect (= 3 step-count) :to-be-truthy)))

(it-sequential "loop-unrolling-partially-unrolls-unknown-trip-with-remainder"
  (let* ((insts (list (cl-cc/vm:make-vm-const :dst :i :value 0)
                      (cl-cc/vm:make-vm-const :dst :one :value 1)
                      (cl-cc/vm:make-vm-label :name "Lh")
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (cl-cc/vm:make-vm-jump-zero :reg :c :label "Lexit")
                      (cl-cc/vm:make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (cl-cc/vm:make-vm-add :dst :i :lhs :i :rhs :one)
                      (cl-cc/vm:make-vm-jump :label "Lh")
                      (cl-cc/vm:make-vm-label :name "Lexit")
                      (cl-cc/vm:make-vm-ret :reg :sum)))
         (out (cl-cc/optimize/loops:opt-pass-loop-unrolling insts))
         (lt-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-lt)) out))
         (jump-to-lh (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-jump)
                                      (equal (cl-cc/vm::vm-label-name x) "Lh")))
                               out)))
    (expect (= 3 lt-count) :to-be-truthy)
    (expect (= 1 jump-to-lh) :to-be-truthy)))

(it-sequential "loop-unrolling-supports-additional-comparisons le"
  (destructuring-bind (init-inst limit-inst step-inst cmp-inst cmp-type expected-steps) (list (cl-cc/vm:make-vm-const :dst :i :value 0) (cl-cc/vm:make-vm-const :dst :lim :value 2) (cl-cc/vm:make-vm-const :dst :step :value 1) (make-vm-le :dst :c :lhs :i :rhs :lim) 'cl-cc/vm::vm-le 3)
    (let* ((insts (list init-inst
                      limit-inst
                      step-inst
                      (cl-cc/vm:make-vm-label :name "Lh")
                      cmp-inst
                      (cl-cc/vm:make-vm-jump-zero :reg :c :label "Lexit")
                      (cl-cc/vm:make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (cl-cc/vm:make-vm-add :dst :i :lhs :i :rhs :step)
                      (cl-cc/vm:make-vm-jump :label "Lh")
                      (cl-cc/vm:make-vm-label :name "Lexit")
                      (cl-cc/vm:make-vm-ret :reg :sum)))
         (out (cl-cc/optimize/loops:opt-pass-loop-unrolling insts))
         (cmp-count (count-if (lambda (x) (typep x cmp-type)) out))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out)))
    (expect (= 0 cmp-count) :to-be-truthy)
    (expect (= expected-steps step-count) :to-be-truthy))))

(it-sequential "loop-unrolling-supports-additional-comparisons ge"
  (destructuring-bind (init-inst limit-inst step-inst cmp-inst cmp-type expected-steps) (list (cl-cc/vm:make-vm-const :dst :i :value 3) (cl-cc/vm:make-vm-const :dst :lim :value 1) (cl-cc/vm:make-vm-const :dst :step :value -1) (make-vm-ge :dst :c :lhs :i :rhs :lim) 'cl-cc/vm::vm-ge 3)
    (let* ((insts (list init-inst
                      limit-inst
                      step-inst
                      (cl-cc/vm:make-vm-label :name "Lh")
                      cmp-inst
                      (cl-cc/vm:make-vm-jump-zero :reg :c :label "Lexit")
                      (cl-cc/vm:make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (cl-cc/vm:make-vm-add :dst :i :lhs :i :rhs :step)
                      (cl-cc/vm:make-vm-jump :label "Lh")
                      (cl-cc/vm:make-vm-label :name "Lexit")
                      (cl-cc/vm:make-vm-ret :reg :sum)))
         (out (cl-cc/optimize/loops:opt-pass-loop-unrolling insts))
         (cmp-count (count-if (lambda (x) (typep x cmp-type)) out))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out)))
    (expect (= 0 cmp-count) :to-be-truthy)
    (expect (= expected-steps step-count) :to-be-truthy))))

(it-sequential "loop-unrolling-supports-additional-comparisons eq"
  (destructuring-bind (init-inst limit-inst step-inst cmp-inst cmp-type expected-steps) (list (cl-cc/vm:make-vm-const :dst :i :value 4) (cl-cc/vm:make-vm-const :dst :lim :value 4) (cl-cc/vm:make-vm-const :dst :step :value 1) (make-vm-eq :dst :c :lhs :i :rhs :lim) 'cl-cc/vm::vm-eq 1)
    (let* ((insts (list init-inst
                      limit-inst
                      step-inst
                      (cl-cc/vm:make-vm-label :name "Lh")
                      cmp-inst
                      (cl-cc/vm:make-vm-jump-zero :reg :c :label "Lexit")
                      (cl-cc/vm:make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (cl-cc/vm:make-vm-add :dst :i :lhs :i :rhs :step)
                      (cl-cc/vm:make-vm-jump :label "Lh")
                      (cl-cc/vm:make-vm-label :name "Lexit")
                      (cl-cc/vm:make-vm-ret :reg :sum)))
         (out (cl-cc/optimize/loops:opt-pass-loop-unrolling insts))
         (cmp-count (count-if (lambda (x) (typep x cmp-type)) out))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out)))
    (expect (= 0 cmp-count) :to-be-truthy)
    (expect (= expected-steps step-count) :to-be-truthy))))

(it-sequential "loop-unrolling-partial-keeps-remainder-loop"
  (let* ((insts (%loop-transforms-make-counted-loop-insts :lim 10))
         (out (cl-cc/optimize/loops:opt-pass-loop-unrolling insts))
         (lt-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-lt)) out))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out))
         (jump-to-lh (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-jump)
                                      (equal (cl-cc/vm::vm-label-name x) "Lh")))
                               out)))
    (expect (= 3 lt-count) :to-be-truthy)
    (expect (= 3 step-count) :to-be-truthy)
    (expect (= 1 jump-to-lh) :to-be-truthy)))

(it-sequential "cfg-natural-loop-transforms-detected rotation"
  (destructuring-bind (pass expected-jumps-to-lh expected-adds) (list #'cl-cc/optimize/control-flow:opt-pass-loop-rotation 0 1)
    (let* ((insts (list (cl-cc/vm:make-vm-const :dst :one :value 1)
                      (cl-cc/vm:make-vm-jump :label "Lh")
                      (cl-cc/vm:make-vm-label :name "Lh")
                      (cl-cc/vm:make-vm-integer-p :dst :c :src :i)
                      (cl-cc/vm:make-vm-jump-zero :reg :c :label "Lexit")
                      (cl-cc/vm:make-vm-add :dst :i :lhs :i :rhs :one)
                      (cl-cc/vm:make-vm-jump :label "Lh")
                      (cl-cc/vm:make-vm-label :name "Lexit")
                      (cl-cc/vm:make-vm-ret :reg :i)))
         (out (funcall pass insts))
         (jumps-to-lh (count-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-jump)
                                       (equal (cl-cc/vm::vm-label-name x) "Lh")))
                                out))
         (add-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-add)) out)))
    (expect (= expected-jumps-to-lh jumps-to-lh) :to-be-truthy)
    (expect (= expected-adds add-count) :to-be-truthy))))

(it-sequential "cfg-natural-loop-transforms-detected peeling"
  (destructuring-bind (pass expected-jumps-to-lh expected-adds) (list #'cl-cc/optimize/loops:opt-pass-loop-peel 1 2)
    (let* ((insts (list (cl-cc/vm:make-vm-const :dst :one :value 1)
                      (cl-cc/vm:make-vm-jump :label "Lh")
                      (cl-cc/vm:make-vm-label :name "Lh")
                      (cl-cc/vm:make-vm-integer-p :dst :c :src :i)
                      (cl-cc/vm:make-vm-jump-zero :reg :c :label "Lexit")
                      (cl-cc/vm:make-vm-add :dst :i :lhs :i :rhs :one)
                      (cl-cc/vm:make-vm-jump :label "Lh")
                      (cl-cc/vm:make-vm-label :name "Lexit")
                      (cl-cc/vm:make-vm-ret :reg :i)))
         (out (funcall pass insts))
         (jumps-to-lh (count-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-jump)
                                       (equal (cl-cc/vm::vm-label-name x) "Lh")))
                                out))
         (add-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-add)) out)))
    (expect (= expected-jumps-to-lh jumps-to-lh) :to-be-truthy)
    (expect (= expected-adds add-count) :to-be-truthy))))
