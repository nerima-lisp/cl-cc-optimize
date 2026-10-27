;;;; tests/unit/optimize/optimizer-inline-pass-tests-2.lisp
;;;; Unit tests for src/optimize/optimizer-inline-pass.lisp (continued)
;;;;
;;;; Covers:
;;;;   Inlining policy — opt-inline-inst-cost, opt-inline-body-cost,
;;;;                     opt-adaptive-inline-threshold, opt-inline-eligible-p
;;;;   Full passes     — opt-pass-global-dce, opt-pass-inline

(cl:in-package :cl-cc/optimize/inlining/test)

;;; ─── opt-inline-inst-cost ───────────────────────────────────────────────────

(it-sequential "opt-inline-inst-cost-returns-number"
  (dolist (inst (list (cl-cc/vm:make-vm-const :dst :r0 :value 1)
                      (cl-cc/vm:make-vm-move  :dst :r1 :src :r0)
                      (cl-cc/vm:make-vm-ret   :reg :r0)))
    (expect (>= (cl-cc/optimize/inlining:opt-inline-inst-cost inst) 0) :to-be-truthy)))

(it-sequential "opt-inline-inst-cost-target-difference"
  (let ((inst (cl-cc/vm:make-vm-add :dst :r0 :lhs :r1 :rhs :r2)))
    (let ((cl-cc/optimize/inlining::*opt-learned-cost-target* :x86-64))
      (let ((x86-cost (cl-cc/optimize/inlining:opt-inline-inst-cost inst)))
        (let ((cl-cc/optimize/inlining::*opt-learned-cost-target* :aarch64))
          (let ((a64-cost (cl-cc/optimize/inlining:opt-inline-inst-cost inst)))
            (expect (/= x86-cost a64-cost) :to-be-truthy)))))))

;;; ─── opt-inline-body-cost ───────────────────────────────────────────────────

(it-sequential "opt-inline-body-cost-cases excludes-ret"
  (destructuring-bind (body expected-cost) (list (list (cl-cc/vm:make-vm-const :dst :r0 :value 1) (cl-cc/vm:make-vm-ret :reg :r0)) (cl-cc/optimize/inlining:opt-inline-inst-cost (cl-cc/vm:make-vm-const :dst :r0 :value 1)))
    (expect (= expected-cost (cl-cc/optimize/inlining::opt-inline-body-cost body)) :to-be-truthy)))

(it-sequential "opt-inline-body-cost-cases only-ret"
  (destructuring-bind (body expected-cost) (list (list (cl-cc/vm:make-vm-ret :reg :r0)) 0)
    (expect (= expected-cost (cl-cc/optimize/inlining::opt-inline-body-cost body)) :to-be-truthy)))

;;; ─── opt-adaptive-inline-threshold ──────────────────────────────────────────

(it-sequential "opt-adaptive-threshold-cheap-body"
  (let* ((ci   (cl-cc/vm:make-vm-closure :dst :r9 :label "cheap" :params '(:r0) :captured nil))
         ;; vm-move has cost ≤ 1 — saturate cheap-ratio to 1.0
         (body (loop repeat 5
                     collect (cl-cc/vm:make-vm-move :dst :r1 :src :r0)))
         (def  (list :closure ci :params '(:r0) :body (append body (list (cl-cc/vm:make-vm-ret :reg :r1)))))
         (threshold (cl-cc/optimize/inlining:opt-adaptive-inline-threshold def)))
    ;; Should be > base (15) because cheap-ratio >= 0.75
    (expect (>= threshold 15) :to-be-truthy)))

(it-sequential "opt-adaptive-threshold-has-floor"
  (let* ((ci  (cl-cc/vm:make-vm-closure :dst :r9 :label "empty" :params '(:r0) :captured nil))
         (def (list :closure ci :params '(:r0) :body (list (cl-cc/vm:make-vm-ret :reg :r0))))
         (threshold (cl-cc/optimize/inlining:opt-adaptive-inline-threshold def)))
    (expect (>= threshold 8) :to-be-truthy)))

(it-sequential "opt-adaptive-threshold-ml-bonus-unit"
  (let* ((ci (cl-cc/vm:make-vm-closure :dst :r9 :label "cheap" :params '(:r0) :captured nil))
         (body (append (loop repeat 6 collect (cl-cc/vm:make-vm-move :dst :r1 :src :r0))
                       (list (cl-cc/vm:make-vm-ret :reg :r1))))
         (def (list :closure ci :params '(:r0) :body body)))
    (let ((cl-cc/optimize/inlining::*opt-enable-ml-inline-score* nil))
      (let ((without-ml (cl-cc/optimize/inlining:opt-adaptive-inline-threshold def)))
        (let ((cl-cc/optimize/inlining::*opt-enable-ml-inline-score* t)
              (cl-cc/optimize/inlining::*opt-inline-ml-model-version* "mlgo-v2"))
          (expect (>= (cl-cc/optimize/inlining:opt-adaptive-inline-threshold def)
                           without-ml) :to-be-truthy))))))

;;; ─── opt-inline-eligible-p ──────────────────────────────────────────────────

(defun %make-eligible-def (label params body-insts)
  "Build a plist-style func def suitable for opt-inline-eligible-p."
  (let ((ci (cl-cc/vm:make-vm-closure :dst :r9 :label label :params params :captured nil)))
    (list :closure ci
          :params params
          :body (append body-insts (list (cl-cc/vm:make-vm-ret :reg (first params)))))))

(it-sequential "opt-inline-eligible-simple-function"
  (let* ((def (%make-eligible-def "add1" '(:r0)
                 (list (cl-cc/vm:make-vm-const :dst :r1 :value 1)))))
    (expect (cl-cc/optimize/inlining::opt-inline-eligible-p def 50) :to-be-truthy)))

(it-sequential "opt-inline-eligible-rejects-captured-vars"
  (let* ((ci  (cl-cc/vm:make-vm-closure :dst :r9 :label "cap"
                                :params '(:r0)
                                :captured '((:r5 . 42))))
         (def (list :closure ci :params '(:r0)
                    :body (list (cl-cc/vm:make-vm-ret :reg :r0)))))
    (expect (cl-cc/optimize/inlining::opt-inline-eligible-p def 50) :to-be-null)))

(it-sequential "opt-inline-eligible-rejects-over-threshold"
  (let* ((big-body (loop repeat 20
                         collect (cl-cc/vm:make-vm-move :dst :r1 :src :r0)))
         (def (%make-eligible-def "big" '(:r0) big-body)))
    ;; threshold=0 → any non-empty body exceeds it
    (expect (cl-cc/optimize/inlining::opt-inline-eligible-p def 0) :to-be-null)))

;;; ─── opt-pass-global-dce ────────────────────────────────────────────────────

(it-sequential "opt-pass-global-dce-empty-program"
  (expect (cl-cc/optimize/inlining:opt-pass-global-dce nil) :to-be-null))

(it-sequential "opt-pass-global-dce-preserves-reachable-function"
  (let* (;; Function "fn" takes :r1 as param (non-nil so it gets collected)
         (cl   (cl-cc/vm:make-vm-closure :dst :r0 :label "fn" :params '(:r1) :captured nil))
         (lbl  (cl-cc/vm:make-vm-label :name "fn"))
         (body (cl-cc/vm:make-vm-const :dst :r2 :value 1))
         (ret  (cl-cc/vm:make-vm-ret   :reg :r2))
         ;; Top-level: call the closure held in :r0
         (call (cl-cc/vm:make-vm-call :dst :r3 :func :r0 :args (list :r1)))
         (insts (list cl lbl body ret call))
         (result (cl-cc/optimize/inlining:opt-pass-global-dce insts)))
    ;; All instructions should be preserved since "fn" is reachable
    (expect (= (length insts) (length result)) :to-be-truthy)))

(it-sequential "opt-pass-global-dce-removes-dead-function"
  (let* (;; Dead function: must have non-nil params so opt-collect-function-defs
         ;; picks it up. Without params, it's invisible to the DCE analysis.
         (dead-cl  (cl-cc/vm:make-vm-closure :dst :r9 :label "dead"
                                    :params '(:r0) :captured nil))
         (dead-lbl (cl-cc/vm:make-vm-label :name "dead"))
         (dead-body (cl-cc/vm:make-vm-const :dst :r1 :value 99))
         (dead-ret  (cl-cc/vm:make-vm-ret   :reg :r0))
         ;; Top-level: just a constant, never loads or calls "dead"
         (top-const (cl-cc/vm:make-vm-const :dst :r0 :value 0))
         (insts (list dead-cl dead-lbl dead-body dead-ret top-const))
         (result (cl-cc/optimize/inlining:opt-pass-global-dce insts)))
    ;; Only top-const should survive — 4 dead-function instructions removed
    (expect (= 1 (length result)) :to-be-truthy)
    (expect (cl-cc/vm:vm-const-p (first result)) :to-be-truthy)))

;;; ─── opt-pass-inline ────────────────────────────────────────────────────────

(it-sequential "opt-pass-inline-empty-program"
  (expect (cl-cc/optimize/inlining:opt-pass-inline nil) :to-be-null))

(it-sequential "opt-pass-inline-inlines-eligible-call"
  (let* (;; Define a trivial function: (lambda (:r1) (const :r2 42) (ret :r1))
         (cl   (cl-cc/vm:make-vm-closure :dst :r0 :label "const42" :params '(:r1) :captured nil))
         (lbl  (cl-cc/vm:make-vm-label :name "const42"))
         (body (cl-cc/vm:make-vm-const :dst :r2 :value 42))
         (ret  (cl-cc/vm:make-vm-ret   :reg :r2))
         ;; Call it: (const42 :r5) → :r6
         (call (cl-cc/vm:make-vm-call :dst :r6 :func :r0 :args (list :r5)))
         (insts (list cl lbl body ret call))
         (result (cl-cc/optimize/inlining:opt-pass-inline insts :threshold 50)))
    ;; vm-call should be gone; result should contain vm-move for arg passing
    (expect (find-if #'cl-cc/vm:vm-call-p result) :to-be-null)
    ;; A vm-move or vm-const should appear for argument/return
    (expect (some (lambda (i) (or (cl-cc/vm:vm-move-p i) (cl-cc/vm:vm-const-p i))) result) :to-be-truthy)))

(it-sequential "opt-pass-inline-preserves-non-eligible-call"
  (let* ((cl   (cl-cc/vm:make-vm-closure :dst :r0 :label "fn" :params '(:r1)
                                 :captured '((:r5 . 42))))
         (lbl  (cl-cc/vm:make-vm-label :name "fn"))
         (body (cl-cc/vm:make-vm-const :dst :r2 :value 1))
         (ret  (cl-cc/vm:make-vm-ret   :reg :r2))
         (call (cl-cc/vm:make-vm-call :dst :r3 :func :r0 :args (list :r1)))
         (insts (list cl lbl body ret call))
         (result (cl-cc/optimize/inlining:opt-pass-inline insts :threshold 50)))
    ;; Captured vars → not eligible → vm-call must remain in output
    (expect (find-if #'cl-cc/vm:vm-call-p result) :to-be-truthy)))

(it-sequential "opt-pass-inline-propagates-constant-argument-into-inlined-body"
  (let* ((cl   (cl-cc/vm:make-vm-closure :dst :r0 :label "id" :params '(:r1) :captured nil))
         (lbl  (cl-cc/vm:make-vm-label :name "id"))
         (ret  (cl-cc/vm:make-vm-ret :reg :r1))
         (argc (cl-cc/vm:make-vm-const :dst :r5 :value 0))
         (call (cl-cc/vm:make-vm-call :dst :r6 :func :r0 :args (list :r5)))
         (insts (list cl lbl ret argc call))
         (result (cl-cc/optimize/inlining:opt-pass-inline insts :threshold 50)))
    (expect (find-if #'cl-cc/vm:vm-call-p result) :to-be-null)
    (expect (>= (count-if (lambda (i)
                     (and (cl-cc/vm:vm-const-p i)
                          (eql (cl-cc/vm:vm-const-value i) 0)))
                   result)
         2) :to-be-truthy)))
