(in-package #:cl-cc/optimize/analysis)

(defun %opt-inst-write-regs (inst)
  "Return registers written by INST."
  (let ((dst (opt-inst-dst inst)))
    (and dst (list dst))))

(defun %opt-reg-intersect-p (a b)
  "T when register lists A and B intersect by EQ."
  (and a b (intersection a b :test #'eq)))
