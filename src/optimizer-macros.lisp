(in-package :cl-cc/optimize)

;;; ─── Cross-Cutting Macros and Helpers ───────────────────────────────────
;;;
;;; Macros and small shared functions used across many optimizer pass files,
;;; defined early so every later file in the :serial t load order can use
;;; them.

(defmacro define-inst-type-predicate (name types &optional doc)
  "Define NAME as a one-line instruction-type test: (typep inst 'TYPES)."
  `(defun ,name (inst)
     ,@(when doc (list doc))
     (typep inst ',types)))

(defun %opt-track-known-callee-label (inst name-to-label reg->label)
  "Update REG->LABEL with the known-callee-label fact produced by INST.

Shared by FR-606 dead-argument elimination, call-site splitting, and sealed
generic-function devirtualization: each pass tracks, per register, the
statically-known function label it holds (from a closure/func-ref literal, a
named constant resolved via NAME-TO-LABEL, or propagated through a move), and
forgets the fact on any other definition of the register."
  (typecase inst
    ((or vm-closure vm-func-ref)
     (setf (gethash (vm-dst inst) reg->label) (vm-label-name inst)))
    (vm-const
     (let ((label (and (symbolp (vm-value inst))
                       (gethash (vm-value inst) name-to-label))))
       (if label
           (setf (gethash (vm-dst inst) reg->label) label)
           (remhash (vm-dst inst) reg->label))))
    (vm-move
     (multiple-value-bind (label found-p) (gethash (vm-src inst) reg->label)
       (if found-p
           (setf (gethash (vm-dst inst) reg->label) label)
           (remhash (vm-dst inst) reg->label))))
    (t
     (let ((dst (opt-inst-dst inst)))
       (when dst (remhash dst reg->label))))))
