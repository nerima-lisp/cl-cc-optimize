(in-package :cl-cc/optimize)

;;;; Safepoint optimization foundation (FR-090, FR-091)

(defun %opt-mir-accessor (name)
  "Return MIR accessor NAME when the MIR package is loaded."
  (let* ((pkg (find-package :cl-cc/mir))
         (sym (and pkg (find-symbol name pkg))))
    (and sym (fboundp sym) (symbol-function sym))))

(defun opt-safepoint-inst-p (inst)
  "Return T when INST represents a MIR/sexp safepoint."
  (or (and (consp inst) (eq (first inst) :safepoint))
      (let ((op-accessor (%opt-mir-accessor "MIRI-OP")))
        (and op-accessor (eq (funcall op-accessor inst) :safepoint)))))

(defun %opt-safepoint-default-roots (inst)
  (cond
    ((and (consp inst) (eq (first inst) :safepoint))
     (rest inst))
    (t
     (let ((srcs-accessor (%opt-mir-accessor "MIRI-SRCS")))
       (and srcs-accessor (funcall srcs-accessor inst))))))

(defun %opt-safepoint-roots (inst root-set-analysis)
  "Return a canonical root set for INST."
  (let ((roots (cond
                 ((hash-table-p root-set-analysis)
                  (multiple-value-bind (value found-p) (gethash inst root-set-analysis)
                    (if found-p value (%opt-safepoint-default-roots inst))))
                 ((functionp root-set-analysis)
                  (funcall root-set-analysis inst))
                 (t
                  (%opt-safepoint-default-roots inst)))))
    (sort (copy-list roots) #'string< :key #'prin1-to-string)))

(defun %opt-filter-safepoints (instructions active root-set-analysis removed)
  "Filter dominated safepoints in a single instruction list."
  (let ((result nil)
        (local-active (copy-list active)))
    (dolist (inst instructions (values (nreverse result) local-active))
      (if (opt-safepoint-inst-p inst)
          (let ((key (%opt-safepoint-roots inst root-set-analysis)))
            (if (member key local-active :test #'equal)
                (setf (gethash inst removed) t)
                (progn
                  (push key local-active)
                  (push inst result))))
          (push inst result)))))

;;; FR-090: Safepoint Dominance Pruning — removes safepoints whose root sets
;;; are dominated by other safepoints
(defun opt-prune-dominated-safepoints (cfg root-set-analysis)
  "Remove safepoint B when dominated by safepoint A with the same root set."
  (cfg-compute-dominators cfg)
  (let ((removed (make-hash-table :test #'eq)))
    (labels ((walk (block active)
               (multiple-value-bind (new-insts new-active)
                   (%opt-filter-safepoints (bb-instructions block) active root-set-analysis removed)
                 (setf (bb-instructions block) new-insts)
                 (dolist (child (bb-dom-children block))
                   (walk child new-active)))))
      (when (cfg-entry cfg)
        (walk (cfg-entry cfg) nil)))
    cfg))

(defun %opt-safepoint-motion-barrier-p (inst)
  "Return T when a safepoint must not be moved across INST."
  (or (typep inst '(or vm-set-global vm-slot-write vm-aset))
      (member (vm-inst-effect-kind inst)
              '(:write-global :io :control :unknown)
              :test #'eq)))

(defun %opt-block-terminator-position (instructions)
  (position-if (lambda (inst) (typep inst '(or vm-jump vm-jump-zero vm-ret vm-halt)))
               instructions))

(defun %opt-remove-safepoints-hoistable-to-tail (block)
  "Remove tail-block safepoints that can be reinserted at the back-edge poll site."
  (let* ((insts (bb-instructions block))
         (term-pos (or (%opt-block-terminator-position insts) (length insts)))
         (prefix (subseq insts 0 term-pos))
         (suffix (subseq insts term-pos))
         (kept nil)
         (hoisted nil))
    (loop for rest on prefix
          for inst = (car rest)
          do (cond
               ((and (opt-safepoint-inst-p inst)
                     (notany #'%opt-safepoint-motion-barrier-p (cdr rest)))
                (push inst hoisted))
               (t
                (push inst kept))))
    (setf (bb-instructions block) (append (nreverse kept) suffix))
    (nreverse hoisted)))

(defun %opt-insert-before-terminator (block insts)
  (when insts
    (let* ((old (bb-instructions block))
           (pos (or (%opt-block-terminator-position old) (length old))))
      (setf (bb-instructions block)
            (append (subseq old 0 pos) insts (subseq old pos))))))

;;; FR-091: Safepoint Hoisting to Loop Back-Edges — moves safepoints from
;;; loop bodies to back edges, reducing polling frequency
(defun opt-hoist-safepoints-to-back-edges (cfg)
  "Move safely-hoistable loop safepoints in back-edge blocks to back-edge polls."
  (cfg-compute-dominators cfg)
  (cfg-compute-loop-depths cfg)
  (loop for tail across (cfg-blocks cfg)
        when tail
        do (dolist (header (bb-successors tail))
             (when (and (> (bb-loop-depth tail) 0)
                        (cfg-dominates-p header tail))
               (let ((hoisted (%opt-remove-safepoints-hoistable-to-tail tail)))
                 (%opt-insert-before-terminator tail hoisted)))))
  cfg)
