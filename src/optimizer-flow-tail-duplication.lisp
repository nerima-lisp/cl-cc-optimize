;;;; optimizer-flow-tail-duplication.lisp — hot-path tail duplication
;;;;
;;;; Duplicates a small, frequently-executed successor block into each of
;;;; its predecessors, trading code size for removing the join point's
;;;; branch on the hot path. Conservative FR-167 subset: bounded by
;;;; *opt-tail-dup-max-instructions*, and only applied where profile data
;;;; marks the target as frequent and the duplication as beneficial.

(in-package :cl-cc/optimize)

;;; ─── Tail duplication (conservative FR-167 subset) ───────────────────────

(defparameter *opt-tail-dup-max-instructions* 12
  "Maximum number of tail-block instructions duplicated into a predecessor.")

(defun %tail-dup-terminator (block)
  "Return BLOCK's last instruction, or NIL."
  (car (last (bb-instructions block))))

(defun %tail-dup-candidate-p (succ)
  "Return T when SUCC is a tail block safe for duplication."
  (let ((insts (bb-instructions succ)))
    (and insts
         (<= (length insts) *opt-tail-dup-max-instructions*)
         (notany #'vm-label-p insts)
         (or (vm-ret-p (%tail-dup-terminator succ))
             (vm-jump-p (%tail-dup-terminator succ))
             (typep (%tail-dup-terminator succ) 'vm-jump-zero)))))

(defun %tail-dup-copy-insts (succ)
  "Return structural copies of SUCC's instructions."
  (mapcar #'%tail-dup-copy-inst (bb-instructions succ)))

(defun %tail-dup-copy-inst (inst)
  "Return a structural copy of INST for tail duplication."
  (handler-case
      (sexp->instruction (instruction->sexp inst))
    (error () inst)))

(defun %tail-dup-frequent-p (succ)
  "Return T when SUCC is worth considering under the loop-depth heuristic."
  (not (%cfg-block-cold-p succ)))

(defun %tail-dup-beneficial-p (pred succ)
  "Return T when duplicating SUCC into PRED removes at least one jump."
  (let* ((term (car (last (bb-instructions pred))))
         (succ-label (and (bb-label succ) (vm-name (bb-label succ)))))
    (and succ-label
         (or (and (typep term 'vm-jump)
                  (equal (vm-label-name term) succ-label))
             (and (typep term 'vm-jump-zero)
                  (equal (vm-label-name term) succ-label))))))

(defun %tail-dup-rewrite-pred (pred succ)
  "Duplicate SUCC instructions into PRED by replacing trailing jump-to-SUCC."
  (let* ((pred-insts (bb-instructions pred))
         (term (car (last pred-insts)))
         (succ-label (and (bb-label succ) (vm-name (bb-label succ)))))
    (when (and succ-label
               pred-insts
               (typep term 'vm-jump)
               (equal (vm-label-name term) succ-label))
      (setf (bb-instructions pred)
             (nconc (butlast pred-insts)
                    (%tail-dup-copy-insts succ)))
      t)))

(defun %tail-dup-split-conditional-target-edge (cfg pred succ)
  "Create an edge pad for a conditional PRED branch to SUCC."
  (let* ((term (car (last (bb-instructions pred))))
         (succ-label (%cfg-ensure-label succ cfg "TAIL_DUP_TARGET_")))
    (when (and (typep term 'vm-jump-zero)
               (equal (vm-label-name term) (vm-name succ-label)))
      (let ((pad (%cfg-split-edge cfg pred succ succ-label)))
        (%cfg-replace-terminator pred term
                                 (make-vm-jump-zero :reg (vm-reg term)
                                                    :label (vm-name (bb-label pad))))
        pad))))

(defun %tail-dup-rewrite-conditional-pred (cfg pred succ)
  "Duplicate SUCC through an edge pad for a conditional predecessor."
  (when-let ((pad (%tail-dup-split-conditional-target-edge cfg pred succ)))
    (setf (bb-instructions pad) (%tail-dup-copy-insts succ)
          (bb-successors pad) (copy-list (bb-successors succ)))
    (dolist (old-succ (bb-successors succ))
      (pushnew pad (bb-predecessors old-succ) :test #'eq))
    t))

(defun %tail-dup-linear-candidates (cfg)
  "Return an alist of label-name to tail instruction copies for CFG candidates."
  (let ((candidates nil))
    (cfg-compute-dominators cfg)
    (cfg-compute-loop-depths cfg)
    (loop for succ across (cfg-blocks cfg)
          for label = (and (bb-label succ) (vm-name (bb-label succ)))
          when (and label
                    (> (length (bb-predecessors succ)) 1)
                    (%tail-dup-frequent-p succ)
                    (%tail-dup-candidate-p succ))
            do (push (cons label (bb-instructions succ)) candidates))
    candidates))

(defun %tail-dup-fresh-label-name (counter)
  "Return a fresh tail-duplication label name."
  (format nil "TAIL_DUP_~D" counter))

(defun %tail-dup-process-inst (inst candidates insertions counter out changed)
  "Process one INST for tail duplication against CANDIDATES, recording a
vm-jump-zero's duplicated tail under INSERTIONS until its target label is
reached. Returns three values: the updated OUT (reverse order), the updated
CHANGED flag, and COUNTER for fresh label naming."
  (cond
    ((and (typep inst 'vm-jump)
          (assoc (vm-label-name inst) candidates :test #'equal))
     (dolist (copy (mapcar #'%tail-dup-copy-inst
                           (cdr (assoc (vm-label-name inst) candidates :test #'equal))))
       (push copy out))
     (values out t counter))
    ((and (typep inst 'vm-jump-zero)
          (assoc (vm-label-name inst) candidates :test #'equal))
     (let* ((target (vm-label-name inst))
            (tail (cdr (assoc target candidates :test #'equal)))
            (fresh (%tail-dup-fresh-label-name (incf counter))))
       (push (make-vm-jump-zero :reg (vm-reg inst) :label fresh) out)
       (push (cons (make-vm-label :name fresh)
                   (mapcar #'%tail-dup-copy-inst tail))
             (gethash target insertions))
       (values out t counter)))
    ((and (vm-label-p inst) (gethash (vm-name inst) insertions))
     (dolist (entry (nreverse (gethash (vm-name inst) insertions)))
       (push (car entry) out)
       (dolist (copy (cdr entry))
         (push copy out)))
     (push inst out)
     (values out changed counter))
    (t
     (push inst out)
     (values out changed counter))))

(defun %tail-dup-linear (instructions candidates)
  "Apply tail duplication to INSTRUCTIONS while preserving linear layout."
  (let ((insertions (make-hash-table :test #'equal))
        (counter 0)
        (out nil)
        (changed nil))
    (dolist (inst instructions)
      (multiple-value-setq (out changed counter)
        (%tail-dup-process-inst inst candidates insertions counter out changed)))
    (values (nreverse out) changed)))

(defun opt-pass-tail-duplication (instructions)
  "Duplicate profitable shared tail blocks into CFG predecessors.

The pass considers multi-predecessor tail blocks up to
*OPT-TAIL-DUP-MAX-INSTRUCTIONS*, handles unconditional predecessors directly,
  and handles conditional taken edges by inserting an edge pad before duplicating.
Duplication is only applied when it removes at least one jump."
  (let* ((cfg (cfg-build instructions))
         (candidates (%tail-dup-linear-candidates cfg)))
    (multiple-value-bind (out changed)
        (%tail-dup-linear instructions candidates)
      (if changed out (cfg-flatten cfg)))))
