;;;; Unit tests for FR-682 loop peeling.

(in-package :cl-cc/optimize/loops/test)

(it-sequential "fr-682-loop-peel-peels-array-boundary-first-iteration"
  (let* ((aref (cl-cc/vm:make-vm-aref :dst :elt :array-reg :arr :index-reg :i))
         (insts (list (cl-cc/vm:make-vm-const :dst :i :value 0)
                      (cl-cc/vm:make-vm-const :dst :limit :value 8)
                      (cl-cc/vm:make-vm-const :dst :one :value 1)
                      (cl-cc/vm:make-vm-label :name "loop")
                      (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                      (cl-cc/vm:make-vm-jump-zero :reg :cond :label "exit")
                      aref
                      (cl-cc/vm:make-vm-add :dst :i :lhs :i :rhs :one)
                      (cl-cc/vm:make-vm-jump :label "loop")
                      (cl-cc/vm:make-vm-label :name "exit")
                      (cl-cc/vm:make-vm-ret :reg :elt)))
         (out (cl-cc/optimize/loops:opt-pass-loop-peel insts))
         (loop-pos (%test-label-position out "loop")))
    (expect loop-pos :to-be-truthy)
    (expect (> loop-pos 3) :to-be-truthy)
    (expect (typep (nth (- loop-pos 3) out) 'cl-cc/vm::vm-lt) :to-be-truthy)
    (expect (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-aref)
                              (cl-cc/optimize/analysis:opt-bounds-check-eliminable-marked-p inst)))
                       out) :to-be-truthy)))

(it-sequential "fr-682-loop-peel-skips-call-bearing-loop"
  (let* ((call (cl-cc/vm:make-vm-call :dst :tmp :func :fn :args (list :i)))
         (insts (list (cl-cc/vm:make-vm-const :dst :i :value 0)
                      (cl-cc/vm:make-vm-const :dst :limit :value 8)
                      (cl-cc/vm:make-vm-const :dst :one :value 1)
                      (cl-cc/vm:make-vm-label :name "loop")
                      (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                      (cl-cc/vm:make-vm-jump-zero :reg :cond :label "exit")
                      call
                      (cl-cc/vm:make-vm-add :dst :i :lhs :i :rhs :one)
                      (cl-cc/vm:make-vm-jump :label "loop")
                      (cl-cc/vm:make-vm-label :name "exit")
                      (cl-cc/vm:make-vm-ret :reg :tmp)))
         (out (cl-cc/optimize/loops:opt-pass-loop-peel insts)))
    (expect (mapcar #'cl-cc/vm::instruction->sexp out) :to-equal (mapcar #'cl-cc/vm::instruction->sexp insts))))

;;;; ─── FR-514 loop fusion / fission (optimizer-loop-fusion.lisp) ──────────────
;;;;
;;;; Fusion/fission operate on the optimizer's linear "canonical counted loop"
;;;; shape: LABEL / (vm-lt iv limit → cond) / (vm-jump-zero cond → exit) / body… /
;;;; (vm-add iv iv step) / (vm-jump → head) / exit-LABEL. These tests exercise the
;;;; dependence-analysis helpers directly plus the two pass entry points.

;;; ─── memory-dependence arithmetic helpers ───────────────────────────────────

(it-sequential "fr-514-gcd-test-safe-cases coprime-delta"
  (destructuring-bind (a b expected) (list '(:stride 2 :offset 0) '(:stride 2 :offset 1) t)
    (let ((result (cl-cc/optimize/loops::%loop-fr514-gcd-test-safe-p a b)))
    (if expected (expect result :to-be-truthy) (expect result :to-be-falsy)))))

(it-sequential "fr-514-gcd-test-safe-cases divisible-delta"
  (destructuring-bind (a b expected) (list '(:stride 2 :offset 0) '(:stride 2 :offset 2) nil)
    (let ((result (cl-cc/optimize/loops::%loop-fr514-gcd-test-safe-p a b)))
    (if expected (expect result :to-be-truthy) (expect result :to-be-falsy)))))

(it-sequential "fr-514-gcd-test-safe-cases zero-strides"
  (destructuring-bind (a b expected) (list '(:stride 0 :offset 0) '(:stride 0 :offset 5) t)
    (let ((result (cl-cc/optimize/loops::%loop-fr514-gcd-test-safe-p a b)))
    (if expected (expect result :to-be-truthy) (expect result :to-be-falsy)))))

(it-sequential "fr-514-banerjee-safe-cases far-offset"
  (destructuring-bind (a b trip expected) (list '(:stride 1 :offset 100) '(:stride 1 :offset 0) 3 t)
    (let ((result (cl-cc/optimize/loops::%loop-fr514-banerjee-safe-p a b trip)))
    (if expected (expect result :to-be-truthy) (expect result :to-be-falsy)))))

(it-sequential "fr-514-banerjee-safe-cases same-offset"
  (destructuring-bind (a b trip expected) (list '(:stride 1 :offset 0) '(:stride 1 :offset 0) 3 nil)
    (let ((result (cl-cc/optimize/loops::%loop-fr514-banerjee-safe-p a b trip)))
    (if expected (expect result :to-be-truthy) (expect result :to-be-falsy)))))

;;; ─── instruction classification helpers ─────────────────────────────────────

(it-sequential "fr-514-memory-inst-p-cases aref"
  (destructuring-bind (kind expected) (list 'aref t)
    (let ((inst (ecase kind
                (aref  (cl-cc/vm:make-vm-aref :dst :d :array-reg :a :index-reg :i))
                (aset  (cl-cc/vm:make-vm-aset :array-reg :a :index-reg :i :val-reg :v))
                (const (cl-cc/vm:make-vm-const :dst :d :value 1)))))
    (if expected
        (expect (cl-cc/optimize/loops::%loop-fr514-memory-inst-p inst) :to-be-truthy)
        (expect (cl-cc/optimize/loops::%loop-fr514-memory-inst-p inst) :to-be-falsy)))))

(it-sequential "fr-514-memory-inst-p-cases aset"
  (destructuring-bind (kind expected) (list 'aset t)
    (let ((inst (ecase kind
                (aref  (cl-cc/vm:make-vm-aref :dst :d :array-reg :a :index-reg :i))
                (aset  (cl-cc/vm:make-vm-aset :array-reg :a :index-reg :i :val-reg :v))
                (const (cl-cc/vm:make-vm-const :dst :d :value 1)))))
    (if expected
        (expect (cl-cc/optimize/loops::%loop-fr514-memory-inst-p inst) :to-be-truthy)
        (expect (cl-cc/optimize/loops::%loop-fr514-memory-inst-p inst) :to-be-falsy)))))

(it-sequential "fr-514-memory-inst-p-cases const"
  (destructuring-bind (kind expected) (list 'const nil)
    (let ((inst (ecase kind
                (aref  (cl-cc/vm:make-vm-aref :dst :d :array-reg :a :index-reg :i))
                (aset  (cl-cc/vm:make-vm-aset :array-reg :a :index-reg :i :val-reg :v))
                (const (cl-cc/vm:make-vm-const :dst :d :value 1)))))
    (if expected
        (expect (cl-cc/optimize/loops::%loop-fr514-memory-inst-p inst) :to-be-truthy)
        (expect (cl-cc/optimize/loops::%loop-fr514-memory-inst-p inst) :to-be-falsy)))))

(it-sequential "fr-514-pure-core-p-rejects-control-flow"
  (expect (cl-cc/optimize/loops::%loop-fr514-pure-core-p
                (list (cl-cc/vm:make-vm-const :dst :a :value 1)
                      (cl-cc/vm:make-vm-const :dst :b :value 2))) :to-be-truthy)
  (expect (cl-cc/optimize/loops::%loop-fr514-pure-core-p
                 (list (cl-cc/vm:make-vm-const :dst :a :value 1)
                       (cl-cc/vm:make-vm-jump :label "somewhere"))) :to-be-falsy))

;;; ─── label + register helpers ───────────────────────────────────────────────

(it-sequential "fr-514-label-name-derives-fresh-keyword"
  (expect (cl-cc/optimize/loops::%loop-fr514-label-name "HEAD" "FISSION_A") :to-be (intern "HEAD__FISSION_A" :keyword)))

(it-sequential "fr-514-find-label-index-locates-label-or-nil"
  (let ((vec (vector (cl-cc/vm:make-vm-const :dst :i :value 0)
                     (cl-cc/vm:make-vm-label :name "target")
                     (cl-cc/vm:make-vm-ret :reg :i))))
    (expect (= 1 (cl-cc/optimize/loops::%loop-fr514-find-label-index vec "target")) :to-be-truthy)
    (expect (cl-cc/optimize/loops::%loop-fr514-find-label-index vec "missing") :to-be-null)))

(it-sequential "fr-514-build-envs-tracks-constants-and-defs"
  (let ((insts (list (cl-cc/vm:make-vm-const :dst :a :value 5)
                     (cl-cc/vm:make-vm-const :dst :b :value 7)
                     (cl-cc/vm:make-vm-add :dst :a :lhs :a :rhs :b))))
    (multiple-value-bind (const-env def-env)
        (cl-cc/optimize/loops::%loop-fr514-build-envs insts 3)
      ;; :a was reassigned by a non-const add → dropped from const-env.
      (expect (gethash :a const-env) :to-be-null)
      (expect (= 7 (gethash :b const-env)) :to-be-truthy)
      ;; def-env still records the producing instruction for :a.
      (expect (typep (gethash :a def-env) 'cl-cc/vm::vm-add) :to-be-truthy))))

(it-sequential "fr-514-register-dependencies-detects-cross-core-use"
  (let ((dependent-l (list (cl-cc/vm:make-vm-add :dst :x :lhs :y :rhs :z)))
        (dependent-r (list (cl-cc/vm:make-vm-add :dst :w :lhs :x :rhs :q)))
        (independent-l (list (cl-cc/vm:make-vm-const :dst :x :value 1)))
        (independent-r (list (cl-cc/vm:make-vm-const :dst :y :value 2))))
    (expect (cl-cc/optimize/loops::%loop-fr514-register-dependencies-p
                  dependent-l dependent-r) :to-be-truthy)
    (expect (cl-cc/optimize/loops::%loop-fr514-register-dependencies-p
                   independent-l independent-r) :to-be-falsy)))

;;; ─── loop fusion pass ───────────────────────────────────────────────────────

(defun %fr-514-counted-loop (head exit iv limit step core)
  "Build a canonical counted loop: label / lt / jz / CORE… / add-step / jump / exit."
  (append (list (cl-cc/vm:make-vm-label :name head)
                (make-vm-lt :dst (intern (format nil "COND-~A" head) :keyword)
                            :lhs iv :rhs limit)
                (cl-cc/vm:make-vm-jump-zero :reg (intern (format nil "COND-~A" head) :keyword)
                                   :label exit))
          core
          (list (cl-cc/vm:make-vm-add :dst iv :lhs iv :rhs step)
                (cl-cc/vm:make-vm-jump :label head)
                (cl-cc/vm:make-vm-label :name exit))))

(it-sequential "fr-514-fusion-merges-adjacent-identical-space-loops"
  (let* ((insts (append
                 (list (cl-cc/vm:make-vm-const :dst :i :value 0)
                       (cl-cc/vm:make-vm-const :dst :j :value 0)
                       (cl-cc/vm:make-vm-const :dst :limit :value 8)
                       (cl-cc/vm:make-vm-const :dst :one :value 1))
                 (%fr-514-counted-loop "headA" "exitA" :i :limit :one
                                       (list (cl-cc/vm:make-vm-const :dst :xa :value 7)))
                 (%fr-514-counted-loop "headB" "exitB" :j :limit :one
                                       (list (cl-cc/vm:make-vm-const :dst :xb :value 9)))
                 (list (cl-cc/vm:make-vm-ret :reg :xa))))
         (out (cl-cc/optimize:opt-pass-loop-fusion insts)))
    ;; Fusion collapses two loop headers into one: fewer instructions, one back-edge.
    (expect (< (length out) (length insts)) :to-be-truthy)
    (expect (= 1 (count-if (lambda (x) (typep x 'cl-cc/vm::vm-jump)) out)) :to-be-truthy)
    ;; The surviving exit label is the second loop's exit.
    (expect (some (lambda (x)
                         (and (typep x 'cl-cc/vm::vm-label)
                              (equal (cl-cc/vm::vm-name x) "exitB")))
                       out) :to-be-truthy)))

(it-sequential "fr-514-fusion-skips-loops-with-different-iteration-spaces"
  (let* ((insts (append
                 (list (cl-cc/vm:make-vm-const :dst :i :value 0)
                       (cl-cc/vm:make-vm-const :dst :j :value 0)
                       (cl-cc/vm:make-vm-const :dst :limit :value 8)
                       (cl-cc/vm:make-vm-const :dst :limit2 :value 9)
                       (cl-cc/vm:make-vm-const :dst :one :value 1))
                 (%fr-514-counted-loop "headA" "exitA" :i :limit :one
                                       (list (cl-cc/vm:make-vm-const :dst :xa :value 7)))
                 (%fr-514-counted-loop "headB" "exitB" :j :limit2 :one
                                       (list (cl-cc/vm:make-vm-const :dst :xb :value 9)))))
         (out (cl-cc/optimize:opt-pass-loop-fusion insts)))
    (expect (mapcar #'cl-cc/vm::instruction->sexp out) :to-equal (mapcar #'cl-cc/vm::instruction->sexp insts))))

;;; ─── loop fission pass ──────────────────────────────────────────────────────

(it-sequential "fr-514-fission-splits-large-independent-pure-loop"
  (let* ((core (loop for k from 1 to 8
                     collect (cl-cc/vm:make-vm-const
                              :dst (intern (format nil "C~A" k) :keyword)
                              :value k)))
         (insts (append
                 (list (cl-cc/vm:make-vm-const :dst :i :value 0)
                       (cl-cc/vm:make-vm-const :dst :limit :value 8)
                       (cl-cc/vm:make-vm-const :dst :one :value 1))
                 (%fr-514-counted-loop "headF" "exitF" :i :limit :one core)
                 (list (cl-cc/vm:make-vm-ret :reg :c1))))
         (out (cl-cc/optimize/loops:opt-pass-loop-fission insts)))
    ;; Fission produces two suffixed loops (FISSION_A / FISSION_B).
    (expect (some (lambda (x)
                         (and (typep x 'cl-cc/vm::vm-label)
                              (search "FISSION_A" (string (cl-cc/vm::vm-name x)))))
                       out) :to-be-truthy)
    (expect (some (lambda (x)
                         (and (typep x 'cl-cc/vm::vm-label)
                              (search "FISSION_B" (string (cl-cc/vm::vm-name x)))))
                       out) :to-be-truthy)))

(it-sequential "fr-514-fission-leaves-small-loops-unchanged"
  (let* ((insts (append
                 (list (cl-cc/vm:make-vm-const :dst :i :value 0)
                       (cl-cc/vm:make-vm-const :dst :limit :value 8)
                       (cl-cc/vm:make-vm-const :dst :one :value 1))
                 (%fr-514-counted-loop "headS" "exitS" :i :limit :one
                                       (list (cl-cc/vm:make-vm-const :dst :x :value 1)))
                 (list (cl-cc/vm:make-vm-ret :reg :x))))
         (out (cl-cc/optimize/loops:opt-pass-loop-fission insts)))
    (expect (mapcar #'cl-cc/vm::instruction->sexp out) :to-equal (mapcar #'cl-cc/vm::instruction->sexp insts))))
