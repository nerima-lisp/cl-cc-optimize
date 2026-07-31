;;;; optimizer-flow-if-conversion.lisp — branchless diamond selection
;;;;
;;;; Rewrites a simple diamond (one condition, two single-instruction arms
;;;; that each define the same register) into a straight-line vm-select,
;;;; removing the branch entirely when both arms are side-effect-free.

(in-package :cl-cc/optimize)

;;; ─── If-conversion: simple diamond → vm-select ───────────────────────────

(defun %opt-label-reference-counts (instructions)
  "Return a label-name → branch-reference-count table for INSTRUCTIONS."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (inst instructions counts)
      (when (or (typep inst 'vm-jump)
                (typep inst 'vm-jump-zero))
        (incf (gethash (vm-label-name inst) counts 0))))))

(defun %opt-label-ref-count (counts label-name)
  "Return the recorded branch-reference count for LABEL-NAME."
  (gethash label-name counts 0))

(defun %opt-label-position (vec label-name)
  "Return the position of LABEL-NAME in VEC, or NIL when absent."
  (loop for i from 0 below (length vec)
        for inst = (aref vec i)
        when (and (vm-label-p inst)
                  (equal (vm-name inst) label-name))
          return i))

(defun %opt-insts-contain-control-or-label-p (insts)
  "Return T when INSTS contain labels or control transfers."
  (some (lambda (inst)
          (or (vm-label-p inst)
              (typep inst 'vm-jump)
              (typep inst 'vm-jump-zero)
              (vm-ret-p inst)))
        insts))

(defun %opt-simple-select-arm-source (insts dst)
  "Return the source register when INSTS is exactly `(vm-move DST SRC)'."
  (when (and (= (length insts) 1)
             (typep (first insts) 'vm-move)
             (eq (vm-dst (first insts)) dst))
    (vm-src (first insts))))

(defun %opt-if-conversion-else-target (vec jz-inst jz-pos label-counts)
  "Return (values ELSE-POS THEN-JUMP-POS THEN-JUMP) for the else-branch target
of JZ-INST, or NIL when the shape is not a simple diamond's else edge."
  (let* ((else-name (vm-label-name jz-inst))
         (else-pos (%opt-label-position vec else-name))
         (then-jump-pos (and else-pos (1- else-pos)))
         (then-jump (and then-jump-pos (> then-jump-pos jz-pos)
                         (aref vec then-jump-pos))))
    (when (and else-pos
               (typep then-jump 'vm-jump)
               (= (%opt-label-ref-count label-counts else-name) 1))
      (values else-pos then-jump-pos then-jump))))

(defun %opt-if-conversion-join-position (vec then-jump else-pos label-counts)
  "Return the join-label position targeted by THEN-JUMP when it is unique and
located after ELSE-POS, or NIL otherwise."
  (let* ((join-name (vm-label-name then-jump))
         (join-pos (%opt-label-position vec join-name)))
    (when (and join-pos
               (> join-pos else-pos)
               (= (%opt-label-ref-count label-counts join-name) 1))
      join-pos)))

(defun %opt-if-conversion-build-select (vec i then-jump-pos else-pos join-pos
                                        cond-inst cond-reg)
  "Return the vm-select candidate plist for the diamond bounded by I..JOIN-POS,
or NIL when the arms are not simple single-move register copies."
  (let* ((then-insts (loop for j from (+ i 2) below then-jump-pos
                           collect (aref vec j)))
         (else-insts (loop for j from (1+ else-pos) below join-pos
                           collect (aref vec j)))
         (then-dst (and then-insts (opt-inst-dst (first then-insts))))
         (then-src (%opt-simple-select-arm-source then-insts then-dst))
         (else-src (%opt-simple-select-arm-source else-insts then-dst)))
    (when (and then-dst then-src else-src
               (not (%opt-insts-contain-control-or-label-p then-insts))
               (not (%opt-insts-contain-control-or-label-p else-insts)))
      (list :end join-pos
            :cond-inst cond-inst
            :select (make-vm-select :dst then-dst
                                    :cond-reg cond-reg
                                    :then-reg then-src
                                    :else-reg else-src)))))

(defun %opt-if-conversion-candidate (vec i label-counts)
  "Return a vm-select candidate plist starting at VEC[I], or NIL.

Recognized shape:
  <cond-inst> (vm-jump-zero cond Lelse)
  (vm-move dst then-reg) (vm-jump Ljoin)
  Lelse: (vm-move dst else-reg)
  Ljoin:

The transform is intentionally conservative: only register-to-register arms are
converted, and both internal labels must be referenced solely by the diamond."
  (let* ((n (length vec))
         (cond-inst (and (< i n) (aref vec i)))
         (jz-pos (1+ i))
         (jz-inst (and (< jz-pos n) (aref vec jz-pos)))
         (cond-reg (and (typep jz-inst 'vm-jump-zero) (vm-reg jz-inst))))
    (when (and cond-inst
               cond-reg
               (not (vm-label-p cond-inst))
               (not (typep cond-inst 'vm-jump))
               (not (typep cond-inst 'vm-jump-zero))
               (eq (opt-inst-dst cond-inst) cond-reg))
      (multiple-value-bind (else-pos then-jump-pos then-jump)
          (%opt-if-conversion-else-target vec jz-inst jz-pos label-counts)
        (when else-pos
          (let ((join-pos (%opt-if-conversion-join-position vec then-jump else-pos label-counts)))
            (when join-pos
              (%opt-if-conversion-build-select vec i then-jump-pos else-pos join-pos
                                                cond-inst cond-reg))))))))

(defun opt-pass-if-conversion (instructions)
  "Convert simple if-diamond control flow into branchless `vm-select`.

This FR-034 pass lowers only the safe single-move diamond shape, preserving the
condition producer and replacing the conditional/unconditional branch pair with
one select. Later dead-label cleanup can remove the now-unreferenced join label."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (label-counts (%opt-label-reference-counts instructions))
         (result nil)
         (i 0))
    (loop while (< i n)
          do (if-let ((candidate (%opt-if-conversion-candidate vec i label-counts)))
               (progn
                 (push (getf candidate :cond-inst) result)
                 (push (getf candidate :select) result)
                 ;; Keep the join label in-place. It is harmless if dead and
                 ;; preserves layout for diagnostics until dead-label cleanup.
                 (push (aref vec (getf candidate :end)) result)
                 (setf i (1+ (getf candidate :end))))
               (progn
                 (push (aref vec i) result)
                 (incf i))))
    (nreverse result)))
