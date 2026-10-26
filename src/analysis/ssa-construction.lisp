;;;; packages/optimize/src/ssa-construction.lisp — SSA Construction and Destruction Entry Points
;;;;
;;;; Contains:
;;;;   ssa-construct — build SSA form from flat VM instructions
;;;;   ssa-destroy — deconstruct SSA back to VM instructions
;;;;   ssa-sequentialize-copies — parallel copy sequentialization
;;;;   ssa-round-trip — round-trip utility
;;;;
;;;; Data structures (ssa-rename-state, ssa-phi), phi placement (ssa-place-phis),
;;;; renaming (ssa-rename), and elimination are in ssa.lisp (loads before).
;;;;
;;;; Load order: after ssa.lisp.

(in-package :cl-cc/optimize)

(defun ssa-construct (instructions)
  "Construct SSA form from a flat VM INSTRUCTIONS list.
   Returns (values cfg phi-map renamed-map) where:
     cfg         — the CFG with dominator information
     phi-map     — hash-table block → list of ssa-phi
     renamed-map — hash-table block → list of renamed vm-instructions"
  (let ((cfg (cfg-build instructions)))
    (cfg-compute-dominators cfg)
    (cfg-compute-dominance-frontiers cfg)
    (let ((phi-map (ssa-place-phis cfg)))
      (setf phi-map (ssa-place-lcssa-phis cfg phi-map))
      (multiple-value-bind (renamed phi-map)
          (ssa-rename cfg phi-map)
        (multiple-value-bind (phi-map renamed)
            (ssa-eliminate-trivial-phis phi-map renamed)
          (values cfg phi-map renamed))))))

;;; ─── SSA Destruction ─────────────────────────────────────────────────────
;;;
;;; Converts SSA form back to a conventional flat instruction list by:
;;;   1. Replacing phi-nodes with copy instructions in predecessor blocks
;;;   2. Sequentializing parallel copies (handling swap cycles via temps)
;;;
;;; The result should be semantically equivalent to the original instructions
;;; under any correct SSA construction (used for round-trip testing).

(defun %ssa-destroy-final-terminator (block-insts)
  "Return BLOCK-INSTS's last instruction if it is a control-flow terminator
(jump, conditional jump, return, or halt), else NIL."
  (and block-insts
       (let ((last-inst (car (last block-insts))))
         (and (typep last-inst '(or vm-jump vm-jump-zero vm-ret vm-halt))
              last-inst))))

(defun %ssa-destroy-branch-target-block (cfg term)
  "Return the CFG block that TERM's vm-jump-zero label targets, or NIL when
TERM is not a conditional jump."
  (and (typep term 'vm-jump-zero)
       (cfg-get-block-by-label cfg (vm-label-name term))))

(defun %ssa-destroy-fallthrough-block (block target)
  "Return BLOCK's successor other than TARGET."
  (find-if (lambda (succ) (not (eq succ target)))
           (bb-successors block)))

(defun %ssa-destroy-emit-copies (copies result)
  "Sequentialize COPIES and push each resulting vm-move onto RESULT. Return
the updated RESULT."
  (dolist (copy (ssa-sequentialize-copies copies) result)
    (push copy result)))

(defun %ssa-destroy-edge-copies (copies-to-insert pred succ)
  "Return the pending parallel copies scheduled on the PRED -> SUCC edge, or
NIL."
  (let ((succ-table (gethash pred copies-to-insert)))
    (and succ-table (gethash succ succ-table))))

(defun %ssa-destroy-process-block (b cfg renamed-map copies-to-insert make-target-pad result)
  "Emit B's label, its non-terminator instructions, and then its terminator
(possibly rewritten to target an edge pad), scheduling any pending edge
copies along the way. MAKE-TARGET-PAD allocates edge-pad blocks for
conditional-jump targets that need copies. Return the updated reverse-order
RESULT accumulator."
  (when (bb-label b)
    (push (bb-label b) result))
  (let* ((block-insts (gethash b renamed-map))
         (terminator  (%ssa-destroy-final-terminator block-insts))
         (prefix      (if terminator (butlast block-insts) block-insts)))
    (dolist (inst prefix)
      (push inst result))
    (cond
     ((typep terminator 'vm-jump)
      (let* ((target (cfg-get-block-by-label cfg (vm-label-name terminator)))
             (copies (and target (%ssa-destroy-edge-copies copies-to-insert b target))))
        (when copies
          (setf result (%ssa-destroy-emit-copies copies result)))
        (push terminator result)))
     ((typep terminator 'vm-jump-zero)
      (let* ((target (%ssa-destroy-branch-target-block cfg terminator))
             (fallthrough (%ssa-destroy-fallthrough-block b target))
             (target-copies (and target (%ssa-destroy-edge-copies copies-to-insert b target)))
             (fallthrough-copies (and fallthrough (%ssa-destroy-edge-copies copies-to-insert b fallthrough)))
             (branch-inst terminator))
        (when target-copies
          (setf branch-inst
                (make-vm-jump-zero
                 :reg (vm-reg terminator)
                 :label (funcall make-target-pad b target target-copies))))
        (push branch-inst result)
        (when fallthrough-copies
          (setf result (%ssa-destroy-emit-copies fallthrough-copies result)))))
     ((typep terminator '(or vm-ret vm-halt))
      (push terminator result))
     (t
      (when-let ((successor (first (bb-successors b))))
        (when-let ((copies (%ssa-destroy-edge-copies copies-to-insert b successor)))
          (setf result (%ssa-destroy-emit-copies copies result))))))
    result))

(defun %ssa-destroy-emit-edge-pads (edge-pads result)
  "Append the deferred edge-pad blocks (label, sequentialized copies, jump to
the real target) collected in EDGE-PADS. Return the updated RESULT."
  (dolist (pad (nreverse edge-pads) result)
    (destructuring-bind (pad-name copies target-name) pad
      (push (make-vm-label :name pad-name) result)
      (setf result (%ssa-destroy-emit-copies copies result))
      (push (make-vm-jump :label target-name) result))))

(defun ssa-destroy (cfg phi-map renamed-map)
  "Destroy SSA form: replace phi-nodes with parallel copies in predecessors.
   Returns a flat instruction list in RPO order.
   Uses the same-RPO ordering for deterministic output."
  (let ((copies-to-insert (make-hash-table :test #'eq)) ; pred → succ → list of (dst . src)
        (edge-pads nil))

    (labels ((add-edge-copy (pred succ dst src)
                            (push (cons dst src)
                                  (gethash succ (or (gethash pred copies-to-insert)
                                                    (setf (gethash pred copies-to-insert)
                                                          (make-hash-table :test #'eq))))))
             (make-target-pad (pred succ copies)
                              (let ((pad-name (format nil "SSA_EDGE_~D_~D_~D"
                                                       (bb-id pred) (bb-id succ) (length edge-pads)))
                                    (target-label (bb-label succ)))
                                (push (list pad-name copies (vm-name target-label)) edge-pads)
                                pad-name)))

      ;; Step 1: for each phi-node, schedule copies in predecessor blocks
      (loop for b across (cfg-blocks cfg)
            do (dolist (phi (gethash b phi-map))
                 (dolist (arg (phi-args phi))
                   (let ((pred (car arg))
                         (src  (cdr arg))
                         (dst  (phi-dst phi)))
                     (add-edge-copy pred b dst src)))))

      ;; Step 2: emit flat instruction list in RPO order
      (let ((rpo (cfg-compute-rpo cfg))
            (result nil))
        (dolist (b rpo)
          (setf result (%ssa-destroy-process-block b cfg renamed-map copies-to-insert #'make-target-pad result)))
        (setf result (%ssa-destroy-emit-edge-pads edge-pads result))
        (nreverse result)))))

(defun %ssa-seqcopy-dst (copy) (car copy))

(defun %ssa-seqcopy-src (copy) (cdr copy))

(defun %ssa-seqcopy-register-copy-p (copy)
  (and (symbolp (%ssa-seqcopy-dst copy))
       (symbolp (%ssa-seqcopy-src copy))
       (not (eq (%ssa-seqcopy-dst copy) (%ssa-seqcopy-src copy)))))

(defun %ssa-seqcopy-source-counts (copies)
  (let ((counts (make-hash-table :test #'eq)))
    (dolist (copy copies counts)
      (when (symbolp (%ssa-seqcopy-src copy))
        (incf (gethash (%ssa-seqcopy-src copy) counts 0))))))

(defun %ssa-seqcopy-ready-copies (copies)
  (let ((counts (%ssa-seqcopy-source-counts copies)))
    (loop for copy in copies
          unless (plusp (gethash (%ssa-seqcopy-dst copy) counts 0))
            collect copy)))

(defun %ssa-seqcopy-find-two-register-cycle (copies)
  (when (= (length copies) 2)
    (destructuring-bind (a b) copies
      (when (and (%ssa-seqcopy-register-copy-p a)
                 (%ssa-seqcopy-register-copy-p b)
                 (eq (%ssa-seqcopy-dst a) (%ssa-seqcopy-src b))
                 (eq (%ssa-seqcopy-src a) (%ssa-seqcopy-dst b)))
        (values (%ssa-seqcopy-dst a) (%ssa-seqcopy-src a))))))

(defun %ssa-seqcopy-emit-xor-swap (left right result)
  (push (make-vm-logxor :dst left :lhs left :rhs right) result)
  (push (make-vm-logxor :dst right :lhs left :rhs right) result)
  (push (make-vm-logxor :dst left :lhs left :rhs right) result)
  result)

(defun %ssa-seqcopy-fresh-temp ()
  (intern (symbol-name (gensym "SSATMP")) :keyword))

(defun %ssa-seqcopy-break-cycle (copies result)
  (let* ((copy (find-if #'%ssa-seqcopy-register-copy-p copies))
         (dst (%ssa-seqcopy-dst copy))
         (temp (%ssa-seqcopy-fresh-temp)))
    ;; Preserve the old value of DST, then rewrite all remaining
    ;; reads of DST to read the temp.  DST is now safe to overwrite,
    ;; so the normal ready-copy pass will drain the cycle.
    (push (make-vm-move :dst temp :src dst) result)
    (values (mapcar (lambda (candidate)
                       (if (eq (%ssa-seqcopy-src candidate) dst)
                           (cons (%ssa-seqcopy-dst candidate) temp)
                           candidate))
                     copies)
            result)))

(defun %ssa-seqcopy-drain-ready (ready copies result)
  "Emit a vm-move for every ready copy and drop it from the pending set."
  (dolist (copy ready)
    (push (make-vm-move :dst (%ssa-seqcopy-dst copy) :src (%ssa-seqcopy-src copy)) result)
    (setf copies (remove copy copies :test #'equal)))
  (values copies result))

(defun %ssa-seqcopy-resolve-stuck (copies result)
  "No copy is ready, so the pending set contains at least one cycle.
Emit an XOR-swap for the two-register case, otherwise break one cycle with a
temporary register so a later round's ready-copies pass can drain it."
  (multiple-value-bind (left right)
      (%ssa-seqcopy-find-two-register-cycle copies)
    (if left
        (values nil (%ssa-seqcopy-emit-xor-swap left right result))
        (%ssa-seqcopy-break-cycle copies result))))

(defun ssa-sequentialize-copies (parallel-copies)
  "Convert a list of parallel copies (dst . src) to a sequential list of
    vm-move instructions that produces the same effect.

   Handles the swap problem: if A←B and B←A appear simultaneously and both
   values are integer registers, emit an XOR-swap instead of using a temp.
   Larger cycles still use a temporary register to break the cycle.

   Algorithm: build the copy dependency DAG and repeatedly emit copies whose
   destination is not read by remaining copies.  Cycles are detected when the
   DAG has no ready leaf."
  (unless parallel-copies (return-from ssa-sequentialize-copies))
  (let ((copies (remove-if (lambda (copy)
                             (eq (%ssa-seqcopy-dst copy) (%ssa-seqcopy-src copy)))
                           (copy-list parallel-copies)))
        (result nil))
    (loop while copies
          do (let ((ready (%ssa-seqcopy-ready-copies copies)))
               (if ready
                   (multiple-value-setq (copies result)
                     (%ssa-seqcopy-drain-ready ready copies result))
                   (multiple-value-setq (copies result)
                     (%ssa-seqcopy-resolve-stuck copies result)))))
    (nreverse result)))

;;; ─── Round-Trip Utility ──────────────────────────────────────────────────

(defun ssa-round-trip (instructions)
  "Construct and immediately destruct SSA form.
   Returns a flat instruction list that should be semantically equivalent
   to the input.  Used for integration testing."
  (multiple-value-bind (cfg phi-map renamed)
      (ssa-construct instructions)
    (ssa-destroy cfg phi-map renamed)))
