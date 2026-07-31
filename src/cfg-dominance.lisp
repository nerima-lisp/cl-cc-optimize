(in-package :cl-cc/optimize)

;;; ─── CFG Dominance ────────────────────────────────────────────────────────
;;; Reverse post-order, forward dominator tree (Cooper et al. 2001), and
;;; natural-loop depth analysis.  CFG construction is in cfg.lisp (loads
;;; before this file); post-dominators and critical-edge splitting are in
;;; cfg-analysis.lisp (loads after this file).

;;; ─── Reverse Post-Order ──────────────────────────────────────────────────

(defun %cfg-rpo-dfs (b visited post-order-cell)
  "Post-order DFS from B; results are consed onto (car POST-ORDER-CELL)."
  (unless (gethash b visited)
    (setf (gethash b visited) t)
    (dolist (s (bb-successors b))
      (%cfg-rpo-dfs s visited post-order-cell))
    (push b (car post-order-cell))))

(defun cfg-compute-rpo (cfg)
  "Compute reverse post-order (RPO) for CFG blocks starting from entry.
   Sets bb-rpo-index for each reachable block.
   Returns a list of blocks in RPO order."
  (let ((visited        (make-hash-table :test #'eq))
        (post-order-cell (list nil)))
    (when (cfg-entry cfg)
      (%cfg-rpo-dfs (cfg-entry cfg) visited post-order-cell))
    ;; `push` prepends, so the last-pushed node (entry) is at the front.
    ;; post-order already holds blocks in RPO order — no nreverse needed.
    (let ((post-order (car post-order-cell)))
      (loop for b in post-order for i from 0
            do (setf (bb-rpo-index b) i))
      post-order)))

;;; ─── Dominator Tree (Cooper et al. 2001) ─────────────────────────────────

(defun cfg-compute-dominators (cfg)
  "Compute immediate dominators for all blocks in CFG using Cooper et al.'s
   simple iterative algorithm (2001).  Sets bb-idom for each block.
   Returns the entry block (root of the dominator tree)."
  (let* ((rpo    (cfg-compute-rpo cfg))
         (entry  (cfg-entry cfg)))
    (unless entry (return-from cfg-compute-dominators))

    ;; Dominator computation is called by multiple analyses; keep the tree
    ;; idempotent instead of appending duplicate children across recomputes.
    (loop for b across (cfg-blocks cfg)
          do (setf (bb-idom b) nil
                   (bb-dom-children b) nil))

    ;; Initialize: entry dominates itself; all others are undefined (nil)
    (setf (bb-idom entry) entry)

    ;; Iterate until stable
    (let ((changed t))
      (loop while changed
            do (setf changed nil)
               (dolist (b rpo)
                 (unless (eq b entry)
                   (let ((new-idom nil))
                     ;; new-idom = first processed predecessor
                     (dolist (p (bb-predecessors b))
                       (when (bb-idom p)
                         (if new-idom (setf new-idom (cfg-intersect p new-idom)) (setf new-idom p))))
                     (when (and new-idom (not (eq new-idom (bb-idom b))))
                       (setf (bb-idom b) new-idom
                             changed t)))))))

    ;; Build dom-children lists
    (loop for b across (cfg-blocks cfg)
          when (and (bb-idom b) (not (eq b entry)))
          do (push b (bb-dom-children (bb-idom b))))

    entry))

(defun cfg-intersect (b1 b2)
  "Find the common dominator of B1 and B2 using the RPO-indexed finger walk.
   Called during iterative dominator computation."
  (let ((f1 b1) (f2 b2))
    (loop until (eq f1 f2)
          do (loop while (> (bb-rpo-index f1) (bb-rpo-index f2))
                   do (setf f1 (bb-idom f1)))
             (loop while (> (bb-rpo-index f2) (bb-rpo-index f1))
                   do (setf f2 (bb-idom f2))))
    f1))

;;; ─── Dominator-based analysis helpers ───────────────────────────────────────
;;; cfg-post-dominates-p, %cfg-replace-*, %cfg-ensure-label, %cfg-split-edge,
;;; and cfg-split-critical-edges live in cfg-analysis.lisp (loads after this file, too).

(defun %cfg-tree-ancestor-p (a b idom-fn)
  "Return T if A is an ancestor of B in the tree defined by IDOM-FN."
  (or (eq a b)
      (let ((idom (funcall idom-fn b)))
        (and idom (not (eq b idom))
             (%cfg-tree-ancestor-p a idom idom-fn)))))

(defun cfg-dominates-p (a b)
  "T if block A dominates block B (A is an ancestor of B in the dominator tree)."
  (%cfg-tree-ancestor-p a b #'bb-idom))

(defun cfg-collect-natural-loop (header tail)
  "Return the natural loop blocks for a backedge TAIL → HEADER."
  (let ((members (make-hash-table :test #'eq))
        (worklist (list tail)))
    (setf (gethash header members) t)
    (loop while worklist
          do (let ((b (pop worklist)))
               (unless (gethash b members)
                 (setf (gethash b members) t)
                 (dolist (p (bb-predecessors b))
                   (unless (gethash p members)
                     (push p worklist))))))
    (loop for b being the hash-keys of members collect b)))

(defun cfg-compute-loop-depths (cfg)
  "Annotate each block with a simple natural-loop nesting depth.
    A backedge is any edge whose target dominates its source."
  (loop for b across (cfg-blocks cfg)
        do (setf (bb-loop-depth b) 0))
  (loop for b across (cfg-blocks cfg)
        do (dolist (s (bb-successors b))
             (when (cfg-dominates-p s b)
               (dolist (member (cfg-collect-natural-loop s b))
                  (incf (bb-loop-depth member))))))
  cfg)
