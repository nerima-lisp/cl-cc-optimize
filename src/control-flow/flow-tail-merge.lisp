;;;; optimizer-flow-tail-merge.lisp — duplicate tail block merging
;;;;
;;;; Blocks with structurally identical instruction sequences (a shared
;;;; "tail", e.g. common cleanup code reached from several branches) are
;;;; merged into one, redirecting every predecessor's terminator to the
;;;; surviving block.

(in-package :cl-cc/optimize)

;;; ─── Block terminator rewriter (shared by tail-merge and jump-threading) ───

(defun %opt-rewrite-block-terminator (block old-label new-label)
  "Rewrite matching jump terminator labels in BLOCK.

Returns true when a terminator was rewritten."
  (when-let ((cell (last (bb-instructions block))))
    (let ((term (car cell)))
      (typecase term
        (vm-jump
         (when (equal (vm-label-name term) old-label)
           (setf (car cell) (make-vm-jump :label new-label))
           t))
        (vm-jump-zero
         (when (equal (vm-label-name term) old-label)
           (setf (car cell)
                 (make-vm-jump-zero :reg (vm-reg term) :label new-label))
           t))))))

;;; ─── Tail merge helpers ───────────────────────────────────────────────────

(defun %tail-merge-succ-labels (block)
  "Return list of successor label names for BLOCK."
  (mapcar (lambda (succ) (and (bb-label succ) (vm-name (bb-label succ))))
          (bb-successors block)))

(defun %tail-merge-block-signature (block)
  "Return a structural equality key for BLOCK: (instruction-sexps successor-labels)."
  (list (mapcar #'instruction->sexp (bb-instructions block))
        (%tail-merge-succ-labels block)))

(defun %tail-merge-redirect-predecessors (block canon label canon-label)
  "Redirect every predecessor of BLOCK to CANON instead, rewriting each
predecessor's terminator from LABEL to CANON-LABEL."
  (dolist (pred (copy-list (bb-predecessors block)))
    (%cfg-replace-successor pred block canon)
    (%opt-rewrite-block-terminator pred label canon-label)
    (pushnew pred (bb-predecessors canon) :test #'eq))
  (setf (bb-predecessors block) nil))

(defun %tail-merge-process-block (block canonical-by-sig)
  "Merge BLOCK into its structural twin recorded in CANONICAL-BY-SIG, or
register BLOCK as the canonical block for its signature when it is first."
  (when-let ((label (and (bb-label block) (vm-name (bb-label block)))))
    (let* ((sig   (%tail-merge-block-signature block))
           (canon (gethash sig canonical-by-sig)))
      (if (and canon (not (eq canon block)))
          (let ((canon-label (and (bb-label canon) (vm-name (bb-label canon)))))
            (when (and canon-label label)
              (%tail-merge-redirect-predecessors block canon label canon-label)))
          (setf (gethash sig canonical-by-sig) block)))))

(defun %tail-merge-merge-duplicates (cfg)
  "Merge duplicate labeled blocks in CFG in-place, rewiring predecessors."
  (let ((canonical-by-sig (make-hash-table :test #'equal)))
    (dolist (block (coerce (cfg-blocks cfg) 'list))
      (%tail-merge-process-block block canonical-by-sig))))

(defun opt-pass-tail-merge (instructions)
  "Merge CFG blocks with identical bodies and identical successor labels.

   This is a conservative tail-merging pass: it only merges whole basic blocks
   whose instruction sequences and outgoing edges are exactly the same. That
   keeps the transformation safe while still removing duplicated block tails."
  (let ((cfg (cfg-build instructions)))
    (when (cfg-entry cfg)
      (%tail-merge-merge-duplicates cfg)
      (cfg-flatten cfg))))
