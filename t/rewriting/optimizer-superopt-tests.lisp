(in-package :cl-cc/optimize/rewriting/test)

(it-sequential "optimizer-superopt-reduces-duplicate-move"
  (let* ((window (list (cl-cc/vm:make-vm-move :dst :r1 :src :r0)
                       (cl-cc/vm:make-vm-move :dst :r1 :src :r0)))
         (optimized (cl-cc/optimize/rewriting:opt-pass-superopt window)))
    (expect (= 1 (length optimized)) :to-be-truthy)
    (expect (typep (first optimized) 'cl-cc/vm:vm-move) :to-be-truthy)
    (expect (cl-cc/vm::vm-dst (first optimized)) :to-be :r1)
    (expect (cl-cc/vm::vm-src (first optimized)) :to-be :r0)))
