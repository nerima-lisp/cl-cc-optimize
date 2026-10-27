;;;; tests/integration/optimizer-lowlevel-dce-tests.lisp — DCE and Jump Threading Tests
;;;;
;;;; Continuation of optimizer-lowlevel-tests.lisp.
;;;; Tests for opt-pass-dce (dead code elimination) and opt-pass-jump (jump threading).

(in-package :cl-cc/optimize/control-flow/test)

;;; ── opt-pass-dce: Dead Code Elimination ──────────────────────────────────

(it-sequential "dce-pass-cases removes-unused-const"
  (destructuring-bind (make-case) (list (lambda ()
             (let* ((i1  (cl-cc/vm:make-vm-const :dst :r0 :value 42))
                    (i2  (cl-cc/vm:make-vm-const :dst :r1 :value 7))
                    (ret (cl-cc/vm:make-vm-ret :reg :r1)))
               (list (list i1 i2 ret) (list i1) (list i2 ret)))))
    (destructuring-bind (instrs absent present) (funcall make-case)
    (let ((out (cl-cc/optimize/control-flow:opt-pass-dce instrs)))
      (dolist (inst absent)  (expect (member inst out) :to-be-falsy))
      (dolist (inst present) (expect (member inst out) :to-be-truthy))))))

(it-sequential "dce-pass-cases keeps-const-read-by-add"
  (destructuring-bind (make-case) (list (lambda ()
             (let* ((i1  (cl-cc/vm:make-vm-const :dst :r0 :value 5))
                    (i2  (cl-cc/vm:make-vm-add  :dst :r1 :lhs :r0 :rhs :r0))
                    (ret (cl-cc/vm:make-vm-ret :reg :r1)))
               (list (list i1 i2 ret) nil (list i1 i2)))))
    (destructuring-bind (instrs absent present) (funcall make-case)
    (let ((out (cl-cc/optimize/control-flow:opt-pass-dce instrs)))
      (dolist (inst absent)  (expect (member inst out) :to-be-falsy))
      (dolist (inst present) (expect (member inst out) :to-be-truthy))))))

(it-sequential "dce-pass-cases removes-unused-move"
  (destructuring-bind (make-case) (list (lambda ()
             (let* ((c   (cl-cc/vm:make-vm-const :dst :r0 :value 1))
                    (m   (cl-cc/vm:make-vm-move  :dst :r2 :src :r0))
                    (ret (cl-cc/vm:make-vm-ret :reg :r0)))
               (list (list c m ret) (list m) (list c)))))
    (destructuring-bind (instrs absent present) (funcall make-case)
    (let ((out (cl-cc/optimize/control-flow:opt-pass-dce instrs)))
      (dolist (inst absent)  (expect (member inst out) :to-be-falsy))
      (dolist (inst present) (expect (member inst out) :to-be-truthy))))))

(it-sequential "dce-pass-cases preserves-impure-call"
  (destructuring-bind (make-case) (list (lambda ()
             (let* ((fn  (cl-cc/vm:make-vm-const :dst :r0 :value 'f))
                    (c   (cl-cc/vm:make-vm-call  :dst :r1 :func :r0 :args nil))
                    (ret (cl-cc/vm:make-vm-ret :reg :r0)))
               (list (list fn c ret) nil (list c)))))
    (destructuring-bind (instrs absent present) (funcall make-case)
    (let ((out (cl-cc/optimize/control-flow:opt-pass-dce instrs)))
      (dolist (inst absent)  (expect (member inst out) :to-be-falsy))
      (dolist (inst present) (expect (member inst out) :to-be-truthy))))))

;;; ── opt-pass-jump: Jump Threading ────────────────────────────────────────

(it-sequential "jump-threading-cases"
  (let* ((j   (cl-cc/vm:make-vm-jump  :label "end"))
         (lbl (cl-cc/vm:make-vm-label :name "end"))
         (ret (cl-cc/vm:make-vm-ret   :reg :r0))
         (out (cl-cc/optimize/control-flow:opt-pass-jump (list j lbl ret))))
    (expect (member j out) :to-be-falsy)
    (expect (member lbl out) :to-be-truthy)
    (expect (member ret out) :to-be-truthy))
  (let* ((j1   (cl-cc/vm:make-vm-jump  :label "mid"))
         (lbl1 (cl-cc/vm:make-vm-label :name "mid"))
         (j2   (cl-cc/vm:make-vm-jump  :label "end"))
         (lbl2 (cl-cc/vm:make-vm-label :name "end"))
         (ret  (cl-cc/vm:make-vm-ret   :reg :r0))
         (out  (cl-cc/optimize/control-flow:opt-pass-jump (list j1 lbl1 j2 lbl2 ret))))
    (let ((j1-out (find-if #'cl-cc/vm:vm-jump-p out)))
      (when j1-out
        (expect "end" :to-equal (cl-cc/vm::vm-label-name j1-out))))))

(it-sequential "jump-threading-follows-long-acyclic-chain"
  (let ((instructions nil))
    (loop for i from 0 below 25 do
      (push (cl-cc/vm:make-vm-jump :label (format nil "L~D" (1+ i))) instructions)
      (push (cl-cc/vm:make-vm-label :name (format nil "L~D" (1+ i))) instructions))
    (push (cl-cc/vm:make-vm-ret :reg :r0) instructions)
    (let* ((out (cl-cc/optimize/control-flow:opt-pass-jump (nreverse instructions)))
           (j0  (find-if #'cl-cc/vm:vm-jump-p out)))
      (expect j0 :to-be-truthy)
      (expect "L25" :to-equal (cl-cc/vm::vm-label-name j0)))))

(it-sequential "jump-zero-threading-updates-label"
  (let* ((c   (cl-cc/vm:make-vm-const     :dst :r0 :value 0))
         (jz  (cl-cc/vm:make-vm-jump-zero :reg :r0 :label "mid"))
         (lbl (cl-cc/vm:make-vm-label     :name "mid"))
         (j2  (cl-cc/vm:make-vm-jump      :label "end"))
         (end (cl-cc/vm:make-vm-label     :name "end"))
         (ret (cl-cc/vm:make-vm-ret       :reg :r0))
         (out (cl-cc/optimize/control-flow:opt-pass-jump (list c jz lbl j2 end ret))))
    ;; The jump-zero should now target "end" directly
    (let ((jz-out (find-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out)))
      (when jz-out
        (expect "end" :to-equal (cl-cc/vm::vm-label-name jz-out))))))
