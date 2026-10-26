;;;; optimizer-dataflow-sccp.lisp — Sparse Conditional Constant Propagation (SCCP) pass
(in-package :cl-cc/optimize)

;;; ─── Pass 1b: Sparse Conditional Constant Propagation ────────────────────

(defun %sccp-env-copy (env)
  (let ((copy (make-hash-table :test #'eq)))
    (maphash (lambda (k v) (setf (gethash k copy) v)) env)
    copy))

(defun %sccp-env-equal-p (a b)
  (and (= (hash-table-count a) (hash-table-count b))
       (let ((same t))
         (maphash (lambda (k v)
                    (unless (multiple-value-bind (bv found) (gethash k b)
                              (and found (equal v bv)))
                      (setf same nil)))
                  a)
         same)))

(defun %sccp-env-merge-prune-key (k v merged other-envs)
  "Remove K from MERGED unless every hash-table in OTHER-ENVS also binds K to
a value EQUAL to V."
  (dolist (env other-envs)
    (multiple-value-bind (ov found) (gethash k env)
      (unless (and found (equal ov v))
        (remhash k merged)
        (return)))))

(defun %sccp-env-merge (envs)
  "Intersect constant bindings across all ENVs."
  (cond
    ((null envs) (make-hash-table :test #'eq))
    ((null (cdr envs)) (%sccp-env-copy (car envs)))
    (t (let ((merged (%sccp-env-copy (car envs))))
         (maphash (lambda (k v)
                    (%sccp-env-merge-prune-key k v merged (cdr envs)))
                  merged)
         merged))))

(defun %sccp-fold-binary-op (inst env tp)
  "Fold a binary arithmetic/comparison instruction of type TP when both
operands are known numeric constants in ENV; data-driven via
*opt-binary-fold-table*/*opt-binary-cmp-fold-table*."
  (multiple-value-bind (lval lfound) (gethash (vm-lhs inst) env)
    (multiple-value-bind (rval rfound) (gethash (vm-rhs inst) env)
      (if (and lfound rfound (numberp lval) (numberp rval))
          (multiple-value-bind (folded ok) (opt-fold-binop-value inst lval rval)
            (if ok (make-vm-const :dst (vm-dst inst) :value folded) inst))
          inst))))

(defun %sccp-fold-unary-op (inst env tp)
  "Fold a unary arithmetic instruction of type TP when its operand is a known
constant in ENV eligible for folding; data-driven via *opt-unary-fold-table*."
  (multiple-value-bind (sval found) (gethash (vm-src inst) env)
    (if (and found (%fold-unary-constant-eligible-p inst sval))
        (make-vm-const :dst (vm-dst inst)
                        :value (funcall (gethash tp *opt-unary-fold-table*) sval))
        inst)))

(defun %sccp-fold-type-pred (inst env tp)
  "Fold a type-predicate instruction of type TP when its operand is a known
constant in ENV; data-driven via *opt-type-pred-fold-table*."
  (multiple-value-bind (sval found) (gethash (vm-src inst) env)
    (if found
        (make-vm-const
         :dst (vm-dst inst)
         :value (if (funcall (gethash tp *opt-type-pred-fold-table*) sval) 1 0))
        inst)))

(defun %sccp-fold-via-table (inst env tp)
  "Fold INST using whichever data-driven fold table TP's instruction type
appears in, or return INST unchanged when none applies."
  (cond
    ((or (gethash tp *opt-binary-fold-table*)
         (gethash tp *opt-binary-cmp-fold-table*))
     (%sccp-fold-binary-op inst env tp))
    ((gethash tp *opt-unary-fold-table*)
     (%sccp-fold-unary-op inst env tp))
    ((gethash tp *opt-type-pred-fold-table*)
     (%sccp-fold-type-pred inst env tp))
    (t inst)))

(defun %sccp-fold-inst (inst env)
  (let ((tp (type-of inst)))
    (typecase inst
      (vm-const inst)
      (vm-label inst)
      (vm-jump-zero
       (%sccp-fold-vm-jump-zero inst env))
      (vm-move
       (%sccp-fold-vm-move inst env))
      (vm-concatenate
       (%sccp-fold-vm-concatenate inst env))
      (vm-char
       (%sccp-fold-vm-char inst env))
      (t (%sccp-fold-via-table inst env tp)))))

(defun %sccp-redirect-successors (block new-succs)
  "Update BLOCK's CFG edges to use NEW-SUCCS as its successors."
  (dolist (old (bb-successors block))
    (setf (bb-predecessors old)
          (remove block (bb-predecessors old) :test #'eq)))
  (setf (bb-successors block) new-succs)
  (dolist (succ new-succs)
    (pushnew block (bb-predecessors succ) :test #'eq)))

(defun %sccp-update-env-for-inst (inst env)
  "Update ENV by binding or killing the destination of INST after folding."
  (when-let ((dst (opt-inst-dst inst)))
    (typecase inst
      (vm-const (setf (gethash dst env) (vm-value inst)))
      (t (remhash dst env)))))

(defun %sccp-process-block-inst (inst block env new-insts)
  "Fold INST under ENV within BLOCK, redirecting BLOCK's CFG edges when a
vm-jump-zero resolves to always/never taken, and updating ENV's bindings for
any instruction that is kept. Returns the updated NEW-INSTS (reverse order)."
  (let ((folded (%sccp-fold-inst inst env)))
    (cond
      ((and (typep inst 'vm-jump-zero) (null folded))
       (%sccp-redirect-successors block
                                   (let ((succs (bb-successors block)))
                                     (if (second succs) (list (second succs)))))
       new-insts)
      ((and (typep inst 'vm-jump-zero) (typep folded 'vm-jump))
       (%sccp-redirect-successors block (list (first (bb-successors block))))
       (%sccp-update-env-for-inst folded env)
       (cons folded new-insts))
      ((null folded) new-insts)
      (t
       (%sccp-update-env-for-inst folded env)
       (cons folded new-insts)))))

(defun %sccp-process-block (block in-env)
  "Fold BLOCK's instructions under IN-ENV; return the resulting out-env."
  (let ((env (%sccp-env-copy in-env))
        (new-insts nil))
    (dolist (inst (bb-instructions block))
      (setf new-insts (%sccp-process-block-inst inst block env new-insts)))
    (setf (bb-instructions block) (nreverse new-insts))
    env))

(defun %sccp-merge-in-env (block out-envs)
  "Return the merged in-env for BLOCK from its predecessors' OUT-ENVS."
  (%sccp-env-merge
   (loop for p in (bb-predecessors block)
         for e = (gethash p out-envs)
         when e collect e)))

(defun %sccp-process-worklist-block (block in-envs out-envs worklist)
  "Fold BLOCK under its merged predecessor state when that state changed
since the last visit, propagating to successors on further change. Return
the updated WORKLIST."
  (let* ((new-in (%sccp-merge-in-env block out-envs))
         (old-in (gethash block in-envs)))
    (if (and old-in (gethash block out-envs) (%sccp-env-equal-p old-in new-in))
        worklist
        (progn
          (setf (gethash block in-envs) new-in)
          (let ((new-out (%sccp-process-block block new-in))
                (old-out (gethash block out-envs)))
            (if (and old-out (%sccp-env-equal-p old-out new-out))
                worklist
                (progn
                  (setf (gethash block out-envs) new-out)
                  (dolist (succ (bb-successors block) worklist)
                    (pushnew succ worklist :test #'eq)))))))))

(defun opt-pass-sccp (instructions)
  "Sparse conditional constant propagation over the CFG.
   Propagates constants across blocks and folds constant branches."
  (let ((cfg (cfg-build instructions)))
    (when (cfg-entry cfg)
      (let ((in-envs  (make-hash-table :test #'eq))
            (out-envs (make-hash-table :test #'eq))
            (worklist (list (cfg-entry cfg))))
        (setf (gethash (cfg-entry cfg) in-envs) (make-hash-table :test #'eq))
        (loop while worklist
              do (let ((block (pop worklist)))
                   (setf worklist (%sccp-process-worklist-block block in-envs out-envs worklist))))))
    (let ((linear (loop for b across (cfg-blocks cfg)
                        when b append (append (when (bb-label b) (list (bb-label b)))
                                              (copy-list (bb-instructions b))))))
      (cfg-flatten (cfg-build linear)))))

;;; (opt-map-tree, %opt-copy-prop-* helpers, and opt-pass-copy-prop
;;;  are in optimizer-copyprop.lisp which loads after this file.)

(defun %sccp-fold-vm-char (inst env)
  "Fold a vm-char INST into a vm-const when its string/index operands are
known constants in ENV, otherwise return INST unchanged."
  (multiple-value-bind (string found-string) (gethash (vm-string-reg inst) env)
    (multiple-value-bind (index found-index) (gethash (vm-index inst) env)
      (if (and (gethash 'vm-char *opt-binary-fold-table*)
               found-string found-index
               (stringp string)
               (integerp index)
               (<= 0 index)
               (< index (length string)))
          (make-vm-const :dst (vm-dst inst)
                         :value (funcall (gethash 'vm-char *opt-binary-fold-table*)
                                         string index))
          inst))))

(defun %sccp-fold-vm-concatenate (inst env)
  "Fold a vm-concatenate INST into a vm-const when every part is a known
string constant in ENV, otherwise return INST unchanged."
  (let ((parts (or (vm-parts inst) (list (vm-str1 inst) (vm-str2 inst)))))
    (if (every (lambda (reg)
                 (multiple-value-bind (val found) (gethash reg env)
                   (and found (stringp val))))
               parts)
        (make-vm-const :dst (vm-dst inst)
                        :value (apply #'concatenate 'string
                                      (mapcar (lambda (reg) (gethash reg env)) parts)))
        inst)))

(defun %sccp-fold-vm-move (inst env)
  "Fold a vm-move INST into a vm-const when its source is a known constant
in ENV, otherwise return INST unchanged."
  (multiple-value-bind (val found) (gethash (vm-src inst) env)
    (if found
        (make-vm-const :dst (vm-dst inst) :value val)
        inst)))

(defun %sccp-fold-vm-jump-zero (inst env)
  "Fold a vm-jump-zero INST when its tested register is a known constant in
ENV: rewrite to an unconditional vm-jump when provably taken, drop the
instruction when provably not taken, else return INST unchanged."
  (multiple-value-bind (val found) (gethash (vm-reg inst) env)
    (cond
      ((and found (opt-falsep val)) (make-vm-jump :label (vm-label-name inst)))
      (found nil)
      (t inst))))
