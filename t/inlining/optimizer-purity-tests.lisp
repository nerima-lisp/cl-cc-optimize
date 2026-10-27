;;;; optimizer-purity-tests.lisp — Unit tests for optimizer-purity.lisp
;;;;
;;;; Covers: opt-function-body-transitively-pure-p (empty body, pure arithmetic,
;;;; unknown call → impure, known impure call, known pure call),
;;;; opt-infer-transitive-function-purity (empty program, pure leaf,
;;;; recursive → not pure, two-function chain, multi-hop purity).

(in-package :cl-cc/optimize/inlining/test)

;;; ─── Helpers ─────────────────────────────────────────────────────────────────

(defun %make-func-defs (&rest label-body-pairs)
  "Build a func-defs hash-table from alternating label/body-list args."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (label body) on label-body-pairs by #'cddr
          do (setf (gethash label ht)
                   (list :closure nil :params nil :body body)))
    ht))

(defun %make-pure-labels (&rest labels)
  "Build a pure-labels hash-table with each label marked T."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (l labels ht)
      (setf (gethash l ht) t))))

(defun %count-inst-type (instructions type)
  "Count TYPE instances in INSTRUCTIONS."
  (count-if (lambda (inst) (typep inst type)) instructions))

;;; ─── opt-function-body-transitively-pure-p ───────────────────────────────────

(it-sequential "opt-body-pure-empty"
  (expect (cl-cc/optimize::opt-function-body-transitively-pure-p
    nil
    (%make-func-defs)
    (make-hash-table :test #'eq)
    (%make-pure-labels)) :to-be-truthy))

(it-sequential "opt-body-pure-arithmetic-only"
  (let ((body (list (cl-cc/vm:make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                    (cl-cc/vm:make-vm-ret :reg :r2))))
    (expect (cl-cc/optimize::opt-function-body-transitively-pure-p
      body
      (%make-func-defs)
      (make-hash-table :test #'eq)
      (%make-pure-labels)) :to-be-truthy)))

(it-sequential "opt-body-pure-const-and-move"
  (let ((body (list (cl-cc/vm:make-vm-const :dst :r0 :value 42)
                    (cl-cc/vm:make-vm-move  :dst :r1 :src :r0)
                    (cl-cc/vm:make-vm-ret   :reg :r1))))
    (expect (cl-cc/optimize::opt-function-body-transitively-pure-p
      body
      (%make-func-defs)
      (make-hash-table :test #'eq)
      (%make-pure-labels)) :to-be-truthy)))

(it-sequential "opt-body-impure-unknown-call"
  (let ((body (list (cl-cc/vm:make-vm-call :dst :r1 :func :r0 :args nil)
                    (cl-cc/vm:make-vm-ret  :reg :r1))))
    (expect (cl-cc/optimize::opt-function-body-transitively-pure-p
      body
      (%make-func-defs)
      (make-hash-table :test #'eq)
      (%make-pure-labels)) :to-be-null)))

(it-sequential "opt-body-impure-known-call-not-pure"
  (let* ((callee-label "callee-fn")
         (closure (cl-cc/vm:make-vm-closure :dst :r0 :label callee-label :params nil :captured nil))
         (call    (cl-cc/vm:make-vm-call :dst :r1 :func :r0 :args nil))
         (ret     (cl-cc/vm:make-vm-ret :reg :r1))
         (fdefs   (%make-func-defs callee-label (list ret))))
    (expect (cl-cc/optimize::opt-function-body-transitively-pure-p
      (list closure call ret)
      fdefs
      (make-hash-table :test #'eq)
      (%make-pure-labels)) :to-be-null)))   ; callee NOT in pure-labels

(it-sequential "opt-body-pure-known-call-pure"
  (let* ((callee-label "pure-callee")
         (closure (cl-cc/vm:make-vm-closure :dst :r0 :label callee-label :params nil :captured nil))
         (call    (cl-cc/vm:make-vm-call :dst :r1 :func :r0 :args nil))
         (ret     (cl-cc/vm:make-vm-ret :reg :r1))
         (fdefs   (%make-func-defs callee-label (list ret))))
    (expect (cl-cc/optimize::opt-function-body-transitively-pure-p
      (list closure call ret)
      fdefs
      (make-hash-table :test #'eq)
      (%make-pure-labels callee-label)) :to-be-truthy)))   ; callee IS in pure-labels

;;; ─── opt-infer-transitive-function-purity ────────────────────────────────────

(it-sequential "opt-infer-purity-empty-program"
  (let ((pure (cl-cc/optimize/analysis:opt-infer-transitive-function-purity nil)))
    (expect (= 0 (hash-table-count pure)) :to-be-truthy)))

(it-sequential "opt-infer-purity-pure-leaf-function"
  (let* ((body   (list (cl-cc/vm:make-vm-add :dst :r1 :lhs :r0 :rhs :r0)
                       (cl-cc/vm:make-vm-ret :reg :r1)))
         (label  "add-fn")
         (insts  (list (cl-cc/vm:make-vm-closure :dst :r9 :label label
                                        :params '(:r0) :captured nil)
                       (cl-cc/vm:make-vm-label :name label)
                       (cl-cc/vm:make-vm-add   :dst :r1 :lhs :r0 :rhs :r0)
                       (cl-cc/vm:make-vm-ret   :reg :r1))))
    (declare (ignore body))
    (let ((pure (cl-cc/optimize/analysis:opt-infer-transitive-function-purity insts)))
      (expect (gethash label pure) :to-be-truthy))))

(it-sequential "opt-infer-purity-recursive-not-pure"
  (let* ((label "rec-fn")
         (insts (list (cl-cc/vm:make-vm-closure :dst :r0 :label label :params '(:r0) :captured nil)
                      (cl-cc/vm:make-vm-label  :name label)
                      ;; calls itself
                      (cl-cc/vm:make-vm-closure :dst :r1 :label label :params nil :captured nil)
                      (cl-cc/vm:make-vm-call   :dst :r2 :func :r1 :args nil)
                      (cl-cc/vm:make-vm-ret    :reg :r2))))
    (let ((pure (cl-cc/optimize/analysis:opt-infer-transitive-function-purity insts)))
      (expect (gethash label pure) :to-be-null))))

(it-sequential "opt-infer-purity-callee-then-caller"
  (let* ((callee-label "callee")
         (caller-label "caller")
         (insts (list
                 ;; callee: arithmetic-only, pure leaf
                 (cl-cc/vm:make-vm-closure :dst :r9 :label callee-label :params '(:r0) :captured nil)
                 (cl-cc/vm:make-vm-label   :name callee-label)
                 (cl-cc/vm:make-vm-add     :dst :r1 :lhs :r0 :rhs :r0)
                 (cl-cc/vm:make-vm-ret     :reg :r1)
                 ;; caller: calls callee then returns
                 (cl-cc/vm:make-vm-closure :dst :r8 :label caller-label :params '(:r0) :captured nil)
                 (cl-cc/vm:make-vm-label   :name caller-label)
                 (cl-cc/vm:make-vm-closure :dst :r3 :label callee-label :params nil :captured nil)
                 (cl-cc/vm:make-vm-call    :dst :r4 :func :r3 :args nil)
                 (cl-cc/vm:make-vm-ret     :reg :r4))))
    (let ((pure (cl-cc/optimize/analysis:opt-infer-transitive-function-purity insts)))
      (expect (gethash callee-label pure) :to-be-truthy)
      (expect (gethash caller-label pure) :to-be-truthy))))

(it-sequential "fr-152-infer-purity-multi-hop-chain"
  (let* ((leaf-label "fr152-leaf")
         (middle-label "fr152-middle")
         (root-label "fr152-root")
         (insts (list
                 ;; leaf: pure arithmetic-only body
                 (cl-cc/vm:make-vm-closure :dst :r9 :label leaf-label :params '(:r0) :captured nil)
                 (cl-cc/vm:make-vm-label   :name leaf-label)
                 (cl-cc/vm:make-vm-add     :dst :r1 :lhs :r0 :rhs :r0)
                 (cl-cc/vm:make-vm-ret     :reg :r1)
                 ;; middle: only calls leaf, so it becomes pure after leaf
                 (cl-cc/vm:make-vm-closure :dst :r8 :label middle-label :params '(:r0) :captured nil)
                 (cl-cc/vm:make-vm-label   :name middle-label)
                 (cl-cc/vm:make-vm-closure :dst :r2 :label leaf-label :params nil :captured nil)
                 (cl-cc/vm:make-vm-call    :dst :r3 :func :r2 :args '(:r0))
                 (cl-cc/vm:make-vm-ret     :reg :r3)
                 ;; root: only calls middle, so it becomes pure after middle
                 (cl-cc/vm:make-vm-closure :dst :r7 :label root-label :params '(:r0) :captured nil)
                 (cl-cc/vm:make-vm-label   :name root-label)
                 (cl-cc/vm:make-vm-closure :dst :r4 :label middle-label :params nil :captured nil)
                 (cl-cc/vm:make-vm-call    :dst :r5 :func :r4 :args '(:r0))
                 (cl-cc/vm:make-vm-ret     :reg :r5))))
    (let ((pure (cl-cc/optimize/analysis:opt-infer-transitive-function-purity insts)))
      (expect (gethash leaf-label pure) :to-be-truthy)
      (expect (gethash middle-label pure) :to-be-truthy)
      (expect (gethash root-label pure) :to-be-truthy))))

(it-sequential "opt-infer-purity-mutually-recursive-not-pure"
  (let* ((even-label "even-fn")
         (odd-label  "odd-fn")
         (insts (list
                 (cl-cc/vm:make-vm-closure :dst :r9 :label even-label :params '(:r0) :captured nil)
                 (cl-cc/vm:make-vm-label   :name even-label)
                 (cl-cc/vm:make-vm-closure :dst :r1 :label odd-label :params nil :captured nil)
                 (cl-cc/vm:make-vm-call    :dst :r2 :func :r1 :args nil)
                 (cl-cc/vm:make-vm-ret     :reg :r2)
                 (cl-cc/vm:make-vm-closure :dst :r8 :label odd-label :params '(:r0) :captured nil)
                 (cl-cc/vm:make-vm-label   :name odd-label)
                 (cl-cc/vm:make-vm-closure :dst :r3 :label even-label :params nil :captured nil)
                 (cl-cc/vm:make-vm-call    :dst :r4 :func :r3 :args nil)
                 (cl-cc/vm:make-vm-ret     :reg :r4))))
    (let ((pure (cl-cc/optimize/analysis:opt-infer-transitive-function-purity insts)))
      (expect (gethash even-label pure) :to-be-null)
      (expect (gethash odd-label  pure) :to-be-null))))

;;; ─── opt-pass-pure-call-optimization ─────────────────────────────────────────

(it-sequential "opt-pass-pure-call-reuses-repeated-known-direct-call"
  (let* ((callee-label "pure-square")
         (insts (list (cl-cc/vm:make-vm-closure :dst :r9 :label callee-label :params '(:r0) :captured nil)
                      (cl-cc/vm:make-vm-label   :name callee-label)
                      (make-vm-mul     :dst :r1 :lhs :r0 :rhs :r0)
                      (cl-cc/vm:make-vm-ret     :reg :r1)
                      (cl-cc/vm:make-vm-closure :dst :r2 :label callee-label :params '(:r0) :captured nil)
                      (cl-cc/vm:make-vm-const   :dst :r0 :value 7)
                      (cl-cc/vm:make-vm-call    :dst :r3 :func :r2 :args '(:r0))
                      (cl-cc/vm:make-vm-call    :dst :r4 :func :r2 :args '(:r0))
                      (cl-cc/vm:make-vm-ret     :reg :r4)))
         (optimized (cl-cc/optimize/inlining:opt-pass-pure-call-optimization insts)))
    (expect (= 1 (%count-inst-type optimized 'vm-call)) :to-be-truthy)
    (expect (some (lambda (inst)
             (and (typep inst 'vm-move)
                  (eq (vm-dst inst) :r4)
                  (eq (vm-src inst) :r3)))
           optimized) :to-be-truthy)))

(it-sequential "opt-pass-pure-call-keeps-impure-direct-call"
  (let* ((callee-label "impure-fn")
         (insts (list (cl-cc/vm:make-vm-closure    :dst :r9 :label callee-label :params '(:r0) :captured nil)
                      (cl-cc/vm:make-vm-label      :name callee-label)
                      (cl-cc/vm:make-vm-set-global :src :r0 :name 'sink)
                      (cl-cc/vm:make-vm-ret        :reg :r0)
                      (cl-cc/vm:make-vm-closure    :dst :r2 :label callee-label :params '(:r0) :captured nil)
                      (cl-cc/vm:make-vm-const      :dst :r0 :value 7)
                      (cl-cc/vm:make-vm-call       :dst :r3 :func :r2 :args '(:r0))
                      (cl-cc/vm:make-vm-call       :dst :r4 :func :r2 :args '(:r0))
                      (cl-cc/vm:make-vm-ret        :reg :r4)))
         (optimized (cl-cc/optimize/inlining:opt-pass-pure-call-optimization insts)))
    (expect (= 2 (%count-inst-type optimized 'vm-call)) :to-be-truthy)
    (expect (some (lambda (inst)
             (and (typep inst 'vm-move)
                  (eq (vm-dst inst) :r4)
                  (eq (vm-src inst) :r3)))
           optimized) :to-be-falsy)))

(it-sequential "opt-pass-pure-call-removes-dead-known-direct-call"
  (let* ((callee-label "pure-inc")
         (insts (list (cl-cc/vm:make-vm-closure :dst :r9 :label callee-label :params '(:r0) :captured nil)
                      (cl-cc/vm:make-vm-label   :name callee-label)
                      (cl-cc/vm:make-vm-add     :dst :r1 :lhs :r0 :rhs :r0)
                      (cl-cc/vm:make-vm-ret     :reg :r1)
                      (cl-cc/vm:make-vm-closure :dst :r2 :label callee-label :params '(:r0) :captured nil)
                      (cl-cc/vm:make-vm-const   :dst :r0 :value 7)
                      (cl-cc/vm:make-vm-call    :dst :r3 :func :r2 :args '(:r0))
                      (cl-cc/vm:make-vm-ret     :reg :r0)))
         (optimized (cl-cc/optimize/inlining:opt-pass-pure-call-optimization insts)))
    (expect (= 0 (%count-inst-type optimized 'vm-call)) :to-be-truthy)
    (expect (some (lambda (inst)
              (and (typep inst 'vm-ret)
                   (eq (vm-reg inst) :r0)))
            optimized) :to-be-truthy)))

(it-sequential "fr-152-pure-call-pass-reuses-transitively-pure-caller"
  (let* ((callee-label "fr152-pure-callee")
         (caller-label "fr152-pure-caller")
         (insts (list
                 ;; callee: pure leaf
                 (cl-cc/vm:make-vm-closure :dst :r9 :label callee-label :params '(:r0) :captured nil)
                 (cl-cc/vm:make-vm-label   :name callee-label)
                 (cl-cc/vm:make-vm-add     :dst :r1 :lhs :r0 :rhs :r0)
                 (cl-cc/vm:make-vm-ret     :reg :r1)
                 ;; caller: pure only because the callee is inferred pure
                 (cl-cc/vm:make-vm-closure :dst :r8 :label caller-label :params '(:r0) :captured nil)
                 (cl-cc/vm:make-vm-label   :name caller-label)
                 (cl-cc/vm:make-vm-closure :dst :r7 :label callee-label :params nil :captured nil)
                 (cl-cc/vm:make-vm-call    :dst :r6 :func :r7 :args '(:r0))
                 (cl-cc/vm:make-vm-ret     :reg :r6)
                 ;; top-level repeated direct calls to the transitively pure caller
                 (cl-cc/vm:make-vm-closure :dst :r2 :label caller-label :params nil :captured nil)
                 (cl-cc/vm:make-vm-const   :dst :r0 :value 7)
                 (cl-cc/vm:make-vm-call    :dst :r3 :func :r2 :args '(:r0))
                 (cl-cc/vm:make-vm-call    :dst :r4 :func :r2 :args '(:r0))
                 (cl-cc/vm:make-vm-ret     :reg :r4)))
         (optimized (cl-cc/optimize/inlining:opt-pass-pure-call-optimization insts)))
    (expect (= 2 (%count-inst-type optimized 'vm-call)) :to-be-truthy)
    (expect (some (lambda (inst)
             (and (typep inst 'vm-move)
                  (eq (vm-dst inst) :r4)
                  (eq (vm-src inst) :r3)))
           optimized) :to-be-truthy)))

(it-sequential "opt-pass-pure-call-does-not-reuse-when-dst-overwrites-arg-register"
  (let* ((callee-label "pure-self")
         (insts (list (cl-cc/vm:make-vm-closure :dst :r9 :label callee-label :params '(:r0) :captured nil)
                      (cl-cc/vm:make-vm-label   :name callee-label)
                      (cl-cc/vm:make-vm-add     :dst :r1 :lhs :r0 :rhs :r0)
                      (cl-cc/vm:make-vm-ret     :reg :r1)
                      (cl-cc/vm:make-vm-closure :dst :r2 :label callee-label :params '(:r0) :captured nil)
                      (cl-cc/vm:make-vm-const   :dst :r0 :value 7)
                      (cl-cc/vm:make-vm-call    :dst :r0 :func :r2 :args '(:r0))
                      (cl-cc/vm:make-vm-call    :dst :r3 :func :r2 :args '(:r0))
                      (cl-cc/vm:make-vm-ret     :reg :r3)))
         (optimized (cl-cc/optimize/inlining:opt-pass-pure-call-optimization insts)))
    (expect (= 2 (%count-inst-type optimized 'vm-call)) :to-be-truthy)
    (expect (some (lambda (inst)
             (and (typep inst 'vm-move)
                  (eq (vm-dst inst) :r3)
                  (eq (vm-src inst) :r0)))
           optimized) :to-be-falsy)))

(it-sequential "optimize-instructions-pass-pipeline-runs-pure-call-optimization"
  (let* ((callee-label "pure-double")
         (insts (list (cl-cc/vm:make-vm-closure :dst :r9 :label callee-label :params '(:r0) :captured nil)
                      (cl-cc/vm:make-vm-label   :name callee-label)
                      (cl-cc/vm:make-vm-add     :dst :r1 :lhs :r0 :rhs :r0)
                      (cl-cc/vm:make-vm-ret     :reg :r1)
                      (cl-cc/vm:make-vm-closure :dst :r2 :label callee-label :params '(:r0) :captured nil)
                      (cl-cc/vm:make-vm-const   :dst :r0 :value 7)
                      (cl-cc/vm:make-vm-call    :dst :r3 :func :r2 :args '(:r0))
                      (cl-cc/vm:make-vm-call    :dst :r4 :func :r2 :args '(:r0))
                      (cl-cc/vm:make-vm-ret     :reg :r4)))
         (optimized (cl-cc/optimize:optimize-instructions
                     insts
                     :max-iterations 1
                     :pass-pipeline '(:pure-call-optimization))))
    (expect (= 1 (%count-inst-type optimized 'vm-call)) :to-be-truthy)
    (expect (some (lambda (inst)
             (and (typep inst 'vm-move)
                  (eq (vm-dst inst) :r4)
                  (eq (vm-src inst) :r3)))
           optimized) :to-be-truthy)))
