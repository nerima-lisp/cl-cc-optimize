;;;; packages/optimize/src/optimizer-sroa.lisp — FR-668 SROA
;;;; Scalar Replacement of Aggregates: split struct/array into scalars.
;;;; LLVM SROA pass / mem2reg equivalent.

(in-package :cl-cc/optimize)

(defvar *sroa-enabled* t)

(defstruct (aggregate-access (:conc-name aa-))
  "A single recorded access to a scalar-replaceable aggregate (struct field or
array element).

FIELD-INDEX identifies which field this access targets: a slot-name symbol for
a VM-MAKE-OBJ struct access (read off VM-SLOT-READ/VM-SLOT-WRITE directly,
since the slot name is already a compile-time literal on those instructions),
or an integer element index for a VM-MAKE-ARRAY access (from VM-AREF/VM-ASET,
resolved to a literal via %SROA-SINGLE-DEF-CONSTANT-MAP). It is deliberately
untyped, since both kinds of key are used across this pass."
  (aggregate nil :type symbol)    ; register (keyword) holding the aggregate
  (field-index 0)                 ; slot-name symbol, or integer array index
  (is-read-p nil)                 ; T = read, NIL = write
  (instruction nil))              ; the IR instruction

(progn
  (defun %sroa-control-flow-inst-p (inst)
    "T when INST can transfer control non-locally or serve as a jump target,
i.e. it breaks the straight-line, program-order-is-execution-order property
%SROA-ELIGIBLE-SPAN-P relies on to reason about field values without a real
CFG/dominance analysis."
    (typep inst '(or vm-label vm-jump vm-jump-zero vm-ret vm-halt
                     vm-call vm-tail-call vm-apply vm-generic-call vm-trampoline)))

  (defun %sroa-def-count-table (instructions)
    "Return an EQ hash table: register -> number of instructions in
INSTRUCTIONS whose OPT-INST-DST is that register. A register with exactly one
entry has one static definition site, which is what lets this pass reason
about it as a single value without a real SSA/dataflow analysis."
    (let ((ht (make-hash-table :test #'eq)))
      (dolist (inst instructions ht)
        (when-let ((dst (opt-inst-dst inst)))
          (incf (gethash dst ht 0))))))

  (defun %sroa-single-def-constant-map (instructions def-count)
    "Return an EQ hash table: register -> integer, for every register in
INSTRUCTIONS that has exactly one static definition (per DEF-COUNT) and that
definition is a VM-CONST, or a VM-MOVE chain that bottoms out at one. This is
intentionally control-flow-independent: a register with more than one
definition anywhere is simply absent from the result, so membership alone is
a safe proof that every execution of that register's (unique) defining
instruction produces the same known integer. Used to resolve VM-AREF/VM-ASET
index registers to literal indices."
    (let ((source (make-hash-table :test #'eq))
          (resolved (make-hash-table :test #'eq)))
      (dolist (inst instructions)
        (when-let ((dst (opt-inst-dst inst)))
          (typecase inst
            (vm-const (setf (gethash dst source) (cons :const (vm-value inst))))
            (vm-move  (setf (gethash dst source) (cons :move (vm-src inst))))
            (t        (setf (gethash dst source) (cons :other nil))))))
      (labels ((resolve (reg depth)
                        (when (and reg (< depth 64) (eql (gethash reg def-count) 1))
                          (let ((entry (gethash reg source)))
                            (case (car entry)
                              (:const (cdr entry))
                              (:move (resolve (cdr entry) (1+ depth)))
                              (t nil))))))
        (maphash (lambda (reg count)
                   (declare (ignore count))
                   (let ((value (resolve reg 0)))
                     (when (integerp value)
                       (setf (gethash reg resolved) value))))
                 def-count))
      resolved))

  (defun %sroa-access-value-reg (inst)
    "Return the register whose value INST stores, for a recognized SROA write
instruction (VM-SLOT-WRITE or VM-ASET)."
    (typecase inst
      (vm-slot-write (cl-cc/vm:vm-slot-write-value-reg inst))
      (vm-aset (vm-val-reg inst))))

  (defun %sroa-collect-accesses (alloc-inst instructions def-count const-map)
    "Return an ordered list of AGGREGATE-ACCESS records for every access to
ALLOC-INST's destination register within INSTRUCTIONS, or NIL when that
register is unsuitable for scalar replacement.

ALLOC-INST's destination register is rejected (returns NIL) when: it has more
than one static definition; a VM-MAKE-ARRAY carries adjustable/fill-pointer/
displaced-array metadata this pass's flat per-index model does not account
for; any VM-AREF/VM-ASET index register does not resolve to a literal via
CONST-MAP; a struct or array write stores the aggregate's own register into
itself (a self-referential escape this pass cannot represent once the
allocation is deleted); or the register is read by anything at all other than
a recognized VM-SLOT-READ/VM-SLOT-WRITE (struct) or VM-AREF/VM-ASET (array)
access on itself -- a move, a call argument, a return, storage into another
object, or any other instruction counts as an escape and refuses the whole
aggregate, never a partial rewrite."
    (let ((reg (opt-inst-dst alloc-inst)))
      (when (and reg (eql (gethash reg def-count) 1))
        (let ((struct-p (typep alloc-inst 'vm-make-obj))
              (array-p  (typep alloc-inst 'vm-make-array)))
          (when (and array-p
                     (or (cl-cc/vm:vm-adjustable alloc-inst)
                         (vm-adjustable-reg alloc-inst)
                         (cl-cc/vm:vm-fill-pointer alloc-inst)
                         (vm-fill-pointer-reg alloc-inst)
                         (vm-displaced-to-reg alloc-inst)))
            (return-from %sroa-collect-accesses nil))
          (let ((accesses nil))
            (dolist (inst instructions)
              (when (member reg (opt-inst-read-regs inst) :test #'eq)
                (let ((access
                       (cond
                        ((and struct-p (typep inst 'vm-slot-read)
                              (eq (cl-cc/vm:vm-slot-read-obj-reg inst) reg))
                         (make-aggregate-access
                          :aggregate reg
                          :field-index (cl-cc/vm:vm-slot-read-slot-name inst)
                          :is-read-p t
                          :instruction inst))
                        ((and struct-p (typep inst 'vm-slot-write)
                              (eq (cl-cc/vm:vm-slot-write-obj-reg inst) reg))
                         (when (eq (cl-cc/vm:vm-slot-write-value-reg inst) reg)
                           (return-from %sroa-collect-accesses nil))
                         (make-aggregate-access
                          :aggregate reg
                          :field-index (cl-cc/vm:vm-slot-write-slot-name inst)
                          :is-read-p nil
                          :instruction inst))
                        ((and array-p (typep inst 'vm-aref)
                              (eq (vm-array-reg inst) reg))
                         (let ((idx (gethash (vm-index-reg inst) const-map)))
                           (unless (integerp idx)
                             (return-from %sroa-collect-accesses nil))
                           (make-aggregate-access
                            :aggregate reg :field-index idx :is-read-p t
                            :instruction inst)))
                        ((and array-p (typep inst 'vm-aset)
                              (eq (vm-array-reg inst) reg))
                         (let ((idx (gethash (vm-index-reg inst) const-map)))
                           (unless (integerp idx)
                             (return-from %sroa-collect-accesses nil))
                           (when (eq (vm-val-reg inst) reg)
                             (return-from %sroa-collect-accesses nil))
                           (make-aggregate-access
                            :aggregate reg :field-index idx :is-read-p nil
                            :instruction inst)))
                        (t
                         (return-from %sroa-collect-accesses nil)))))
                  (push access accesses))))
            (nreverse accesses))))))

  (defun sroa-analyze (basic-block)
    "Analyze BASIC-BLOCK (a flat list of VM instructions -- a single basic
block, or any straight-line instruction list) for scalar-replacement
candidates.

Returns a list of (ALLOC-INST . ACCESSES) pairs, one per VM-MAKE-OBJ or
VM-MAKE-ARRAY allocation in BASIC-BLOCK whose destination register is used
only through constant struct-slot or constant-index array accesses within
BASIC-BLOCK -- see %SROA-COLLECT-ACCESSES for exactly which uses qualify.
Every other allocation is simply absent from the result: this analysis never
reports a partially-safe candidate for SROA-ELIGIBLE-P to reconsider, since
by the time an aggregate appears here, every read of its register anywhere in
BASIC-BLOCK is already known to be one of the accesses returned."
    (let ((def-count (%sroa-def-count-table basic-block)))
      (let ((const-map (%sroa-single-def-constant-map basic-block def-count))
            (results nil))
        (dolist (inst basic-block)
          (when (typep inst '(or vm-make-obj vm-make-array))
            (when-let ((accesses (%sroa-collect-accesses inst basic-block def-count const-map)))
              (push (cons inst accesses) results))))
        (nreverse results)))))

(progn
  (defun %sroa-group-accesses-by-field (accesses)
    "Return a list of sublists of ACCESSES grouped by AA-FIELD-INDEX (compared
with EQUAL, since a field key may be a symbol or an integer). ACCESSES is
assumed already in program order (as SROA-ANALYZE produces it), and each
returned sublist preserves that relative order."
    (let ((table (make-hash-table :test #'equal))
          (order nil))
      (dolist (access accesses)
        (let ((key (aa-field-index access)))
          (unless (nth-value 1 (gethash key table)) (push key order))
          (push access (gethash key table))))
      (mapcar (lambda (key) (nreverse (gethash key table))) (nreverse order))))

  (defun %sroa-initial-value-ok-p (alloc-inst group)
    "Return (values OK-P SEED-REG) for one field's GROUP of accesses (in
program order, as produced by %SROA-GROUP-ACCESSES-BY-FIELD).

OK-P is T exactly when this field's value is knowable wherever scalar
replacement would first need it: either ALLOC-INST carries a known initial
value for the field (a VM-MAKE-OBJ initarg register matching the slot name,
or a VM-MAKE-ARRAY initial-element register, which VM semantics apply to
every element), or the field's first access is itself a write, which
establishes the value before anything reads it. A field that is read with no
prior write and no known initial value has an implementation-defined value
this pass cannot reproduce by seeding a fresh register, so OK-P is NIL and
the whole aggregate must be refused by SROA-ELIGIBLE-P.

SEED-REG is the register to move into the field's fresh register at the
allocation site, or NIL when no seed is needed (write-first case)."
    (let ((first (first group)))
      (typecase alloc-inst
        (vm-make-obj
         (if-let ((entry (assoc (aa-field-index first) (vm-initarg-regs alloc-inst))))
           (values t (cdr entry))
           (values (not (aa-is-read-p first)) nil)))
        (vm-make-array
         (if-let ((seed (vm-initial-element alloc-inst)))
           (values t seed)
           (values (not (aa-is-read-p first)) nil)))
        (t (values nil nil)))))

  (defun sroa-promote (aggregate accesses new-reg-fn)
    "Build the concrete scalar-replacement rewrite for AGGREGATE (an
allocation instruction already confirmed eligible by SROA-ELIGIBLE-P) and its
ACCESSES: one fresh register per distinct field, a move seeding each field
register from its known initial value when one exists (see
%SROA-INITIAL-VALUE-OK-P), and one move replacing each access instruction.
NEW-REG-FN is a thunk returning a fresh register keyword, matching the
convention OPT-PASS-STRENGTH-REDUCE and friends use for NEW-REG.

Returns an EQ hash table mapping AGGREGATE to its (possibly empty) list of
seed instructions, and each access's original instruction to the single
VM-MOVE instruction that replaces it. The caller (OPT-PASS-SROA) is
responsible for splicing these into the original instruction list in place."
    (let ((replacements (make-hash-table :test #'eq))
          (seed-insts nil))
      (dolist (group (%sroa-group-accesses-by-field accesses))
        (let ((field-reg (funcall new-reg-fn)))
          (multiple-value-bind (ok-p seed-reg) (%sroa-initial-value-ok-p aggregate group)
            (declare (ignore ok-p))
            (when seed-reg
              (push (make-vm-move :dst field-reg :src seed-reg) seed-insts)))
          (dolist (access group)
            (let ((inst (aa-instruction access)))
              (setf (gethash inst replacements)
                    (if (aa-is-read-p access)
                        (make-vm-move :dst (opt-inst-dst inst) :src field-reg)
                        (make-vm-move :dst field-reg :src (%sroa-access-value-reg inst))))))))
      (setf (gethash aggregate replacements) (nreverse seed-insts))
      replacements)))

(progn
  (defun %sroa-position (inst instructions index-of)
    "Return INST's position in INSTRUCTIONS, preferring the O(1) lookup in
INDEX-OF (an EQ hash table built once per SROA-PASS run) over a linear scan."
    (if index-of
        (gethash inst index-of)
        (position inst instructions :test #'eq)))

  (defun %sroa-eligible-span-p (alloc-inst accesses instructions &optional index-of)
    "T when nothing between ALLOC-INST and the last of ACCESSES (inclusive) can
transfer control into or out of that span -- see %SROA-CONTROL-FLOW-INST-P.

Within such a span, list order is provably execution order: there is no label
another path could jump into partway through, and no jump/call/return inside
it that could skip part of it, so 'earliest access in program order' really
does mean 'earliest access at runtime'. That is exactly what
%SROA-INITIAL-VALUE-OK-P assumes when it treats a field's first access in
program order as dominating every later one. Without this check, a
conditional write followed by an unconditional read could look
write-then-read in list order while, on some path, only ever executing the
read."
    (let ((start (%sroa-position alloc-inst instructions index-of)))
      (and start
           (let ((end (reduce #'max accesses
                               :key (lambda (a) (or (%sroa-position (aa-instruction a) instructions index-of)
                                                    start))
                               :initial-value start)))
             (notany #'%sroa-control-flow-inst-p
                     (subseq instructions (1+ start) (1+ end)))))))

  (defun sroa-eligible-p (aggregate accesses &optional instructions index-of)
    "Return T when AGGREGATE (a VM-MAKE-OBJ/VM-MAKE-ARRAY allocation already
screened by SROA-ANALYZE, so every access to its destination register in
INSTRUCTIONS is a recognized constant struct-slot or constant-index array
access) can safely be scalar replaced given its ACCESSES.

Two further conditions are required on top of SROA-ANALYZE's escape check,
both load-bearing for correctness -- see %SROA-ELIGIBLE-SPAN-P and
%SROA-INITIAL-VALUE-OK-P for why each is necessary:

 1. Straight-line span: AGGREGATE and every access in ACCESSES must lie in one
    label/jump/call/return-free span of INSTRUCTIONS.
 2. Known field values: every field that is ever read must either have a
    known initial value (a VM-MAKE-OBJ initarg, or a VM-MAKE-ARRAY
    initial-element) or have its first access, in program order within that
    span, be a write.

INSTRUCTIONS defaults to NIL, in which case this conservatively returns NIL
(no span to prove safe) rather than guessing; callers that have already
established straight-line safety by other means may still find this useful
purely for the initial-value check by passing an explicit list."
    (and accesses
         instructions
         (%sroa-eligible-span-p aggregate accesses instructions index-of)
         (every (lambda (group) (%sroa-initial-value-ok-p aggregate group))
                (%sroa-group-accesses-by-field accesses)))))

(defun opt-pass-sroa (instructions)
  "FR-668: Scalar Replacement of Aggregates.

Conservatively splits a locally-allocated, non-escaping struct (VM-MAKE-OBJ
plus VM-SLOT-READ/VM-SLOT-WRITE) or fixed-index array (VM-MAKE-ARRAY plus
VM-AREF/VM-ASET) into one fresh scalar register per field/element, then
eliminates the allocation and its accessor instructions -- LLVM SROA / mem2reg
for this VM's aggregate instructions.

Candidates are found by SROA-ANALYZE (which already refuses any aggregate
whose register is read anywhere other than through a recognized constant
accessor on itself) and then re-checked by SROA-ELIGIBLE-P for a
straight-line span and provably-known field values before SROA-PROMOTE builds
the rewrite; anything not provably safe by both is left completely untouched.
This is intentionally conservative: correctness comes from refusing to act
whenever a precondition cannot be proven, not from being clever about the
cases it does handle.

INSTRUCTIONS is the flat instruction list of a single basic block or an
entire function, matching every other OPT-PASS-* pass in this file's
signature. Returns INSTRUCTIONS unchanged (same list, not a copy) when
*SROA-ENABLED* is NIL or nothing qualifies."
  (if (not *sroa-enabled*)
      instructions
      (let* ((candidates (sroa-analyze instructions))
             (index-of (let ((ht (make-hash-table :test #'eq)))
                         (loop for inst in instructions
                               for i from 0
                               do (setf (gethash inst ht) i))
                         ht))
             (counter (1+ (opt-max-reg-index instructions)))
             (replacements (make-hash-table :test #'eq))
             (changed nil))
        (flet ((new-reg ()
                 (prog1 (intern (format nil "R~A" counter) :keyword)
                   (incf counter))))
          (dolist (pair candidates)
            (let ((alloc-inst (car pair))
                  (accesses (cdr pair)))
              (when (sroa-eligible-p alloc-inst accesses instructions index-of)
                (setf changed t)
                (let ((plan (sroa-promote alloc-inst accesses #'new-reg)))
                  (maphash (lambda (k v) (setf (gethash k replacements) v)) plan))))))
        (if (not changed)
            instructions
            (loop for inst in instructions
                  append (multiple-value-bind (value found-p) (gethash inst replacements)
                           (cond
                             ((not found-p) (list inst))
                             ((listp value) value)
                             (t (list value)))))))))
