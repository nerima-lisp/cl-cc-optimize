;;;; optimizer-flow-block-merge.lisp — single-predecessor block fusion
;;;;
;;;; Folds a block into its sole predecessor when that predecessor has no
;;;; other successor, dropping the now-redundant jump between them.

(in-package :cl-cc/optimize)

;;; ─── Block merge helpers ──────────────────────────────────────────────────

(defun %block-mergeable-successor-p (block)
  "Return T when BLOCK has exactly one successor that has BLOCK as its sole predecessor."
  (let ((succs (bb-successors block)))
    (and (= (length succs) 1)
         (let ((succ (first succs)))
           (and (= (length (bb-predecessors succ)) 1)
                (eq (first (bb-predecessors succ)) block))))))

(defun %block-strip-merge-jump (insts target-label)
  "Remove the trailing vm-jump to TARGET-LABEL from INSTS when merging blocks."
  (if (and insts
           (vm-jump-p (car (last insts)))
           (equal (vm-label-name (car (last insts))) target-label))
      (butlast insts)
      insts))

(defun %block-merge-emit (block visited suppress-label)
  "Recursively emit BLOCK and its mergeable successors into a flat instruction list."
  (when (or (null block) (gethash block visited))
    (return-from %block-merge-emit nil))
  (setf (gethash block visited) t)
  (let ((result nil))
    (unless suppress-label
      (when (bb-label block)
        (push (bb-label block) result)))
    (let ((insts (bb-instructions block)))
      (if (%block-mergeable-successor-p block)
          (let* ((succ       (first (bb-successors block)))
                 (succ-label (and (bb-label succ) (vm-name (bb-label succ)))))
            (setf insts (if succ-label
                            (%block-strip-merge-jump insts succ-label)
                            insts))
            (setf result (nconc result insts))
            (setf result (nconc result (%block-merge-emit succ visited t))))
          (progn
            (setf result (nconc result insts))
            (dolist (succ (bb-successors block))
              (setf result (nconc result (%block-merge-emit succ visited nil)))))))
    result))

(defun opt-pass-block-merge (instructions)
  "Merge linear CFG chains where a block has exactly one successor and that
   successor has exactly one predecessor. This removes redundant labels/jumps
   on straight-line code paths without changing branching structure."
  (let ((cfg (cfg-build instructions)))
    (if (cfg-entry cfg)
        (%block-merge-emit (cfg-entry cfg) (make-hash-table :test #'eq) nil)
        instructions)))
