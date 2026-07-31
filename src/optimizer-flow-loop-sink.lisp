;;;; optimizer-flow-loop-sink.lisp — conservative code sinking toward CFG uses

(in-package :cl-cc/optimize)

;;; ─── Conservative code sinking (FR-163 subset) ──────────────────────────

(defun %opt-copy-inst (inst)
  "Return a structural copy of INST when it can be sexp-round-tripped."
  (handler-case
      (sexp->instruction (instruction->sexp inst))
    (error () inst)))

(defun %opt-code-sinking-constant-regs (blocks)
  "Return a hash table of registers defined by vm-const instructions."
  (let ((constants (make-hash-table :test #'eq)))
    (dolist (block blocks constants)
      (dolist (inst (bb-instructions block))
        (when-let ((dst (opt-inst-dst inst)))
          (remhash dst constants))
        (when (typep inst 'vm-const)
          (setf (gethash (vm-dst inst) constants) t))))))

(defun %opt-code-sinking-candidate-p (inst constant-regs sunk-regs)
  "Return T when INST is safe and profitable enough to sink.

The pass keeps allocation sinking for vm-cons, adds constants/copies, admits
constant-operand arithmetic, admits alias-checked read-only loads, and otherwise
allows pure instructions whose operands are known loop-invariant by this local
analysis."
  (let ((reads (opt-inst-read-regs inst)))
    (cond
      ((typep inst 'vm-const) t)
      ((typep inst 'vm-cons) t)
      ((typep inst 'vm-move)
       (or (gethash (vm-src inst) constant-regs)
           (gethash (vm-src inst) sunk-regs)))
      ((typep inst '(or vm-add vm-sub vm-mul))
       (every (lambda (reg) (gethash reg constant-regs)) reads))
      ((opt-inst-pure-p inst)
        (every (lambda (reg)
                 (or (gethash reg constant-regs)
                     (gethash reg sunk-regs)))
               reads))
      ((and (eq (vm-inst-effect-kind inst) :read-only)
            (opt-memory-read-inst-p inst))
       t)
      (t nil))))

(defun %opt-code-sinking-linear-index (linear-insts target)
  "Return TARGET's EQ position in LINEAR-INSTS, or NIL."
  (position target linear-insts :test #'eq))

(defun %opt-code-sinking-use-alias-safe-p (inst use def-pos linear-insts alias-roots type-facts)
  "Return T when no write that may alias INST lies between DEF-POS and USE's
position in LINEAR-INSTS."
  (let* ((use-inst (third use))
         (use-pos (%opt-code-sinking-linear-index linear-insts use-inst)))
    (and use-pos
         (let ((lo (min def-pos use-pos))
               (hi (max def-pos use-pos)))
           (notany (lambda (candidate)
                     (and (not (eq candidate inst))
                          (opt-memory-write-inst-p candidate)
                          (opt-memory-accesses-may-alias-p inst candidate
                                                           alias-roots
                                                           type-facts)))
                   (subseq linear-insts (1+ lo) hi))))))

(defun %opt-code-sinking-alias-safe-p (inst uses linear-insts alias-roots type-facts)
  "Return T when sinking INST to USES does not cross an aliased write."
  (if (opt-memory-read-inst-p inst)
      (let ((def-pos (%opt-code-sinking-linear-index linear-insts inst)))
        (and def-pos
             (every (lambda (use)
                      (%opt-code-sinking-use-alias-safe-p
                       inst use def-pos linear-insts alias-roots type-facts))
                    uses)))
      t))

(defun %opt-block-terminator-index (insts)
  "Return the index of the first terminator in INSTS, or NIL."
  (position-if (lambda (inst)
                 (typep inst '(or vm-jump vm-jump-zero vm-ret vm-halt)))
               insts))

(defun %opt-insert-before-index (list index values)
  "Return LIST with VALUES inserted before INDEX."
  (append (subseq list 0 index) values (nthcdr index list)))

(defun %opt-remove-nth (list index)
  "Return LIST without the element at INDEX."
  (append (subseq list 0 index) (nthcdr (1+ index) list)))

(defun %opt-code-sinking-locations (cfg target-reg)
  "Return (BLOCK INDEX INST) locations where TARGET-REG is read."
  (let ((locations nil))
    (loop for block across (cfg-blocks cfg)
          do (loop for inst in (bb-instructions block)
                   for index from 0
                   when (member target-reg (opt-inst-read-regs inst) :test #'eq)
                     do (push (list block index inst) locations)))
    (nreverse locations)))

(defun %opt-code-sinking-definition-count (cfg target-reg)
  "Count definitions of TARGET-REG in CFG."
  (let ((count 0))
    (loop for block across (cfg-blocks cfg)
          do (dolist (inst (bb-instructions block))
               (when (eq (opt-inst-dst inst) target-reg)
                 (incf count))))
    count))

(defun %opt-common-dominator (blocks)
  "Return the nearest common dominator of BLOCKS."
  (when blocks
    (reduce #'cfg-intersect blocks)))

(defun %opt-first-use-index-in-block (block reg)
  "Return the first instruction index in BLOCK that reads REG, or NIL."
  (position-if (lambda (inst)
                 (member reg (opt-inst-read-regs inst) :test #'eq))
               (bb-instructions block)))

(define-inst-type-predicate %opt-cheap-duplicable-sink-p vm-const "Return T for instructions cheap enough to duplicate onto conditional edges.")

(defun %opt-sink-duplicate-into-conditional-successors (def-block def-index inst reg uses)
  "Duplicate cheap INST into both conditional successors when each edge uses REG."
  (let* ((term (car (last (bb-instructions def-block))))
         (succs (bb-successors def-block)))
    (when (and (%opt-cheap-duplicable-sink-p inst)
               (typep term 'vm-jump-zero)
               (= (length succs) 2)
               (= (length uses) 2)
               (every (lambda (succ)
                        (some (lambda (use)
                                (cfg-dominates-p succ (first use)))
                              uses))
                      succs))
      (dolist (succ succs)
        (when-let ((pos (%opt-first-use-index-in-block succ reg)))
          (setf (bb-instructions succ)
                (%opt-insert-before-index (bb-instructions succ) pos
                                          (list (%opt-copy-inst inst))))))
      (setf (bb-instructions def-block)
            (%opt-remove-nth (bb-instructions def-block) def-index))
      t)))

(defun %opt-sink-inst-before-uses (def-block def-index inst reg uses)
  "Move INST from DEF-BLOCK to the nearest common dominator of USES."
  (let* ((use-blocks (remove-duplicates (mapcar #'first uses) :test #'eq))
         (sink-block (%opt-common-dominator use-blocks)))
    (when (and sink-block
               (not (eq sink-block def-block)))
      (let* ((first-use (%opt-first-use-index-in-block sink-block reg))
             (term-index (%opt-block-terminator-index (bb-instructions sink-block)))
             (insert-index (or first-use term-index (length (bb-instructions sink-block)))))
        (setf (bb-instructions def-block)
              (%opt-remove-nth (bb-instructions def-block) def-index))
        (setf (bb-instructions sink-block)
              (%opt-insert-before-index (bb-instructions sink-block) insert-index (list inst)))
        t))))

(defun %opt-code-sinking-try-sink (cfg block index inst dst constant-regs sunk-regs linear-insts alias-roots type-facts)
  "Attempt to sink INST (at INDEX in BLOCK, defining DST) toward its uses.
Returns T and mutates BLOCK/successor instruction lists and SUNK-REGS when
the sink succeeds, else NIL."
  (when (and dst
             (= (%opt-code-sinking-definition-count cfg dst) 1)
             (%opt-code-sinking-candidate-p inst constant-regs sunk-regs))
    (let ((uses (%opt-code-sinking-locations cfg dst)))
      (when (and uses
                 (%opt-code-sinking-alias-safe-p inst uses linear-insts
                                                 alias-roots type-facts))
        (when (or (%opt-sink-duplicate-into-conditional-successors
                    block index inst dst uses)
                  (and (= (length uses) 1)
                       (%opt-sink-inst-before-uses block index inst dst uses)))
          (setf (gethash dst sunk-regs) t)
          t)))))

(defun %opt-code-sinking-process-block (cfg block constant-regs sunk-regs linear-insts alias-roots type-facts)
  "Try sinking one candidate instruction out of BLOCK, scanning from the last
instruction to the first. Returns T as soon as a sink succeeds, matching the
pass's original one-sink-per-block-per-pass scan."
  (let ((insts (bb-instructions block)))
    (loop for index downfrom (1- (length insts)) to 0
          for inst = (nth index insts)
          for dst = (opt-inst-dst inst)
          thereis (%opt-code-sinking-try-sink cfg block index inst dst
                                              constant-regs sunk-regs
                                              linear-insts alias-roots type-facts))))

(defun opt-pass-code-sinking (instructions)
  "Sink selected pure/read-only values closer to their CFG uses.

The pass builds a CFG, computes dominance, finds all uses of each candidate
definition, and moves it to the nearest common dominator of those uses.  Cheap
constants may be duplicated into both arms of a conditional when both arms use
the value.  Read-only loads are not moved across aliased writes. Side-effecting
instructions are never moved."
  (let ((cfg (cfg-build instructions)))
    (cfg-compute-dominators cfg)
    (cfg-compute-loop-depths cfg)
    (let* ((blocks (coerce (cfg-blocks cfg) 'list))
            (constant-regs (%opt-code-sinking-constant-regs blocks))
            (linear-insts (cfg-flatten cfg))
            (alias-roots (opt-compute-heap-aliases linear-insts))
            (type-facts (opt-compute-heap-type-facts linear-insts alias-roots))
            (sunk-regs (make-hash-table :test #'eq))
            (changed nil))
      (dolist (block blocks)
        (when (%opt-code-sinking-process-block cfg block constant-regs sunk-regs
                                                linear-insts alias-roots type-facts)
          (setf changed t)))
      (if changed
          (cfg-flatten (cfg-build (cfg-flatten cfg)))
          (cfg-flatten cfg)))))
