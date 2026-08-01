(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer — Jump Threading and Constant Propagation Passes
;;;
;;; Contains: opt-build-label-index, opt-thread-label, opt-pass-jump,
;;; jump constant-propagation (edge value forwarding), and
;;; opt-pass-jump-threading-with-propagation.
;;;
;;; Load order: after optimizer-flow-passes.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; ─── Pass 3: Jump Threading + Dead Jump Elimination ─────────────────────

(defun opt-build-label-index (instructions)
  "Return (values vector label→index-ht) for threading analysis."
  (let ((vec (coerce instructions 'vector))
        (idx (make-hash-table :test #'equal)))
    (loop for i from 0 below (length vec)
          when (vm-label-p (aref vec i))
          do (setf (gethash (vm-name (aref vec i)) idx) i))
    (values vec idx)))

(defun opt-thread-label (label idx vec &optional (seen (make-hash-table :test #'equal)))
  "Follow jump chains starting at LABEL. Returns the ultimate jump target label.

   Cycle detection prevents infinite loops in pathological (cyclic) code while
   still allowing long but valid jump chains to fully thread."
  (when (gethash label seen)
    (return-from opt-thread-label label))
  (setf (gethash label seen) t)
  (let ((pos (gethash label idx)))
    (unless pos (return-from opt-thread-label label))
    ;; Scan forward past any labels to find the first real instruction
    (loop for i from pos below (length vec)
          for inst = (aref vec i)
          do (typecase inst
                (vm-label nil) ; skip label markers
                (vm-jump ; found a chained jump → follow it
                 (let ((next (vm-label-name inst)))
                   (return-from opt-thread-label
                     (opt-thread-label next idx vec seen))))
                (t (return-from opt-thread-label label)))))
  label)


(defun opt-falls-through-to-p (vec i target)
  "T if scanning forward from position I+1 we reach TARGET before any non-label."
  (loop for j from (1+ i) below (length vec)
        for inst = (aref vec j)
        if (not (vm-label-p inst)) return nil
        if (equal (vm-name inst) target) return t
        finally (return)))

(defun %opt-thread-jump (inst vec i idx)
  "Thread unconditional INST; return the (possibly relabeled) jump, or NIL if it falls through."
  (let ((threaded (opt-thread-label (vm-label-name inst) idx vec)))
    (unless (opt-falls-through-to-p vec i threaded)
      (if (equal threaded (vm-label-name inst))
          inst
          (make-vm-jump :label threaded)))))

(defun %opt-thread-jump-zero (inst vec i idx)
  "Thread conditional INST; always return an instruction (never elide conditionals)."
  (declare (ignore i))
  (let ((threaded (opt-thread-label (vm-label-name inst) idx vec)))
    (if (equal threaded (vm-label-name inst))
        inst
        (make-vm-jump-zero :reg (vm-reg inst) :label threaded))))

(defparameter *opt-jump-thread-table*
  (list (cons 'vm-jump      #'%opt-thread-jump)
        (cons 'vm-jump-zero #'%opt-thread-jump-zero))
  "Maps jump instruction types to their threading handlers.")

(defun opt-pass-jump (instructions)
  "Thread jump chains, remove fallthrough jumps, and propagate edge facts.

The value-propagation part is deliberately conservative: it only carries facts
derived from vm-jump-zero conditions whose comparison operands are both known
constants in the predecessor block, only into single-predecessor successors,
and never across CFG back-edges."
  (opt-pass-jump-threading-with-propagation instructions))

(defun %opt-pass-jump-chain-only (instructions)
  "Thread jump chains and remove jumps to the immediately following label."
  (multiple-value-bind (vec idx) (opt-build-label-index instructions)
    (let ((result nil)
          (n (length vec)))
      (loop for i from 0 below n
            for inst = (aref vec i)
            do (if-let ((handler (loop for entry in *opt-jump-thread-table*
                 for type = (car entry)
                 for fn = (cdr entry)
                 when (typep inst type) return fn)))
                 (when-let ((new (funcall handler inst vec i idx)))
                   (push new result))
                 (push inst result)))
      (nreverse result))))

(define-inst-type-predicate %opt-jump-comparison-p (or vm-lt vm-le vm-gt vm-ge vm-eq vm-num-eq) "Return T when INST is a comparison supported by jump fact propagation.")

(defun %opt-jump-fact-killed-p (fact dst)
  "Return T when a write to DST invalidates FACT's comparison operands."
  (and fact dst
       (or (eq dst (getf fact :lhs))
           (eq dst (getf fact :rhs)))))

(defun %opt-jump-same-comparison-p (inst fact)
  "Return T when INST repeats the comparison described by FACT."
  (and fact
       (%opt-jump-comparison-p inst)
       (eq (type-of inst) (getf fact :pred))
       (eq (vm-lhs inst) (getf fact :lhs))
       (eq (vm-rhs inst) (getf fact :rhs))
       (opt-inst-dst inst)))

(defun %opt-jump-branch-fact-from-comparison (inst env)
  "Return a comparison fact for INST when it's a comparison whose LHS/RHS
are both known constants in ENV, or NIL otherwise."
  (when (%opt-jump-comparison-p inst)
    (multiple-value-bind (lval lfound) (gethash (vm-lhs inst) env)
      (declare (ignore lval))
      (multiple-value-bind (rval rfound) (gethash (vm-rhs inst) env)
        (declare (ignore rval))
        (when (and lfound rfound)
          (list :pred (type-of inst)
                :lhs (vm-lhs inst)
                :rhs (vm-rhs inst)))))))

(defun %opt-jump-branch-fact-step (inst cond-reg env)
  "Process one instruction INST while scanning for a branch comparison
fact: update ENV's known-constant bindings for INST's destination, and
when INST defines COND-REG, return (values T NEW-FACT), where NEW-FACT is
the comparison fact INST establishes (or NIL to clear any prior fact).
Returns (values NIL NIL) when INST doesn't define COND-REG."
  (let ((dst (opt-inst-dst inst)))
    (when dst
      (if (typep inst 'vm-const)
          (setf (gethash dst env) (vm-value inst))
          (remhash dst env)))
    (if (and dst (eq dst cond-reg))
        (values t (%opt-jump-branch-fact-from-comparison inst env))
        (values nil nil))))

(defun %opt-jump-branch-comparison-fact (block)
  "Return a comparison fact for BLOCK's vm-jump-zero terminator, or NIL.

The comparison must define the branch condition register, and both comparison
operands must be known constants at that point in the same block. The returned
fact does not include the edge value; callers add :VALUE for taken/fallthrough
successors."
  (let* ((insts (bb-instructions block))
         (term (car (last insts))))
    (unless (typep term 'vm-jump-zero)
      (return-from %opt-jump-branch-comparison-fact))
    (let ((env (make-hash-table :test #'eq))
          (cond-reg (vm-reg term))
          (fact nil))
      (dolist (inst insts fact)
        (when (eq inst term)
          (return fact))
        (multiple-value-bind (updated-p new-fact)
            (%opt-jump-branch-fact-step inst cond-reg env)
          (when updated-p
            (setf fact new-fact)))))))

(defun %opt-jump-constant-facts (fact)
  (copy-list (getf fact :constants)))

(defun %opt-jump-known-constant (reg constants)
  (if-let ((cell (assoc reg constants :test #'eq)))
    (values (cdr cell) t)
    (values nil nil)))

(defun %opt-jump-put-constant (reg value constants)
  (acons reg value (remove reg constants :key #'car :test #'eq)))

(defun %opt-jump-kill-constant (reg constants)
  (remove reg constants :key #'car :test #'eq))

(defun %opt-jump-back-edge-p (pred succ)
  "Return T when PRED -> SUCC is a CFG back-edge."
  (and (bb-idom pred)
       (cfg-dominates-p succ pred)))

(defun %opt-jump-edge-fact (pred succ cfg live-fact)
  "Return the fact propagated on edge PRED -> SUCC, or NIL."
  (unless (and (= (length (bb-predecessors succ)) 1)
               (not (%opt-jump-back-edge-p pred succ)))
    (return-from %opt-jump-edge-fact))
  (let* ((term (car (last (bb-instructions pred))))
         (branch-fact (%opt-jump-branch-comparison-fact pred)))
    (if (typep term 'vm-jump-zero)
        (let* ((target (cfg-get-block-by-label cfg (vm-label-name term)))
               (value (if (eq succ target) 0 1))
               (constants (%opt-jump-put-constant
                           (vm-reg term) value
                           (%opt-jump-constant-facts live-fact))))
          (append branch-fact (list :value value :constants constants)))
        live-fact)))

(defun %opt-jump-rewrite-inst-with-fact (inst live-fact constants new-insts)
  "Rewrite INST using LIVE-FACT/CONSTANTS tracked so far, propagating known
constant values through redundant comparisons and copies. Returns three
values: the updated LIVE-FACT, CONSTANTS, and NEW-INSTS (reverse order)."
  (let ((dst (opt-inst-dst inst)))
    (when (%opt-jump-fact-killed-p live-fact dst)
      (setf live-fact nil))
    (when dst
      (setf constants (%opt-jump-kill-constant dst constants)))
    (cond
      ((and (typep inst 'vm-const) dst)
       (setf constants (%opt-jump-put-constant dst (vm-value inst) constants))
       (push inst new-insts))
      ((%opt-jump-same-comparison-p inst live-fact)
       (let ((value (getf live-fact :value)))
         (setf constants (%opt-jump-put-constant dst value constants))
         (push (make-vm-const :dst dst :value value) new-insts)))
      ((and (%opt-jump-comparison-p inst) dst)
       ;; Do not independently fold fresh comparisons here.  This pass only
       ;; propagates branch-edge comparison facts; folding a comparison after
       ;; either source register has been redefined would erase the required
       ;; new comparison and conflate this pass with general constant folding.
       (push inst new-insts))
      ((and (typep inst 'vm-move) dst)
       (multiple-value-bind (value ok) (%opt-jump-known-constant (vm-src inst) constants)
         (if ok
             (progn
               (setf constants (%opt-jump-put-constant dst value constants))
               (push (make-vm-const :dst dst :value value) new-insts))
             (push inst new-insts))))
      (t
       (push inst new-insts)))
    (values live-fact constants new-insts)))

(defun %opt-jump-rewrite-block-with-fact (block fact)
  "Rewrite redundant comparisons in BLOCK using incoming FACT.
Returns the still-live fact after scanning the block."
  (let ((live-fact fact)
        (constants (%opt-jump-constant-facts fact))
        (new-insts nil))
    (dolist (inst (bb-instructions block))
      (multiple-value-setq (live-fact constants new-insts)
        (%opt-jump-rewrite-inst-with-fact inst live-fact constants new-insts)))
    (setf (bb-instructions block) (nreverse new-insts))
    (when (or live-fact constants)
      (append live-fact (list :constants constants)))))

(defun %opt-pass-jump-propagate-edge-values (instructions)
  "Propagate known comparison values from conditional CFG edges."
  (let ((cfg (cfg-build instructions)))
    (cfg-compute-dominators cfg)
    (let ((rpo (cfg-compute-rpo cfg))
          (facts (make-hash-table :test #'eq)))
      (dolist (block rpo)
        (let ((live-fact (%opt-jump-rewrite-block-with-fact
                          block (gethash block facts))))
          (dolist (succ (bb-successors block))
            (when-let ((edge-fact (%opt-jump-edge-fact block succ cfg live-fact)))
              (setf (gethash succ facts) edge-fact)))))
      (cfg-flatten cfg))))

(defun opt-pass-jump-threading-with-propagation (instructions)
  "Run jump-chain threading, then conservative comparison-value propagation."
  (%opt-pass-jump-propagate-edge-values
   (%opt-pass-jump-chain-only instructions)))
