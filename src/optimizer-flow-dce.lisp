;;;; optimizer-flow-dce.lisp — collection-aware dead code elimination
;;;;
;;;; Global-usedness DCE: a register is live if it is read anywhere in the
;;;; program, so removal is safe across branches without a backward-liveness
;;;; pass. Extends this to collection stores (hash-table/array/cons slot
;;;; writes) by tracking which stores are provably overwritten before any
;;;; aliasing read, since those are dead even though their "destination" is
;;;; a heap location rather than a register.
;;;;
;;;; If-conversion, branch-correlation, block-merge, tail-merge, and
;;;; tail-duplication used to live in this file; they now have one file each
;;;; (optimizer-flow-if-conversion.lisp and siblings).

(in-package :cl-cc/optimize)
;;; ─── Pass 2: Dead Code Elimination ──────────────────────────────────────

(defun %opt-collection-store-target (inst)
  "Return a structural target key for collection STORE INST, or NIL."
  (typecase inst
    (vm-sethash
     (list :sethash (vm-hash-table-reg inst) (vm-hash-key inst)))
    (vm-aset
     (list :aset (vm-array-reg inst) (vm-index-reg inst)))
    (vm-rplaca
     (list :rplaca (vm-cons-reg inst)))
    (vm-rplacd
     (list :rplacd (vm-cons-reg inst)))))

(defun %opt-collection-read-target (inst)
  "Return the collection target read by INST when it is target-specific."
  (typecase inst
    ((or vm-gethash vm-gethash-eq vm-gethash-eql vm-gethash-equal)
     (list :sethash (vm-hash-table-reg inst) (vm-hash-key inst)))
    (vm-aref
     (list :aset (vm-array-reg inst) (vm-index-reg inst)))
    (vm-car
     (list :rplaca (vm-src inst)))
    (vm-cdr
     (list :rplacd (vm-src inst)))))

(defun %opt-collection-target-regs (target)
  "Return registers that define collection TARGET identity."
  (ecase (first target)
    (:sethash (list (second target) (third target)))
    (:aset    (list (second target) (third target)))
    ((:rplaca :rplacd) (list (second target)))))

(defun %opt-pending-target-reg-overwritten-p (target dst)
  "Return T when overwriting DST makes pending TARGET identity ambiguous."
  (and dst (member dst (%opt-collection-target-regs target) :test #'eq)))

(defun %opt-drop-pending-targets-with-reg (pending dst)
  "Forget pending collection stores whose target registers are overwritten."
  (when dst
    (let (stale)
      (maphash (lambda (target inst)
                 (declare (ignore inst))
                 (when (%opt-pending-target-reg-overwritten-p target dst)
                   (push target stale)))
               pending)
      (dolist (target stale)
        (remhash target pending)))))

(defun %opt-clear-pending-aliasing-collection-stores (pending inst)
  "Conservatively forget pending collection stores that INST may observe or alias."
  (when (or (%opt-collection-read-target inst)
            (member (vm-inst-effect-kind inst) '(:read-only :write-global :io :control :unknown)
                    :test #'eq))
    (clrhash pending)))

(defun %opt-collection-dead-store-table (instructions)
  "Return an EQ table of collection stores overwritten before any aliasing read."
  (let ((pending (make-hash-table :test #'equal))
        (dead    (make-hash-table :test #'eq)))
    (dolist (inst instructions dead)
      (%opt-drop-pending-targets-with-reg pending (opt-inst-dst inst))
      (if-let ((store-target (%opt-collection-store-target inst)))
        (let ((previous (gethash store-target pending)))
          (when previous
            (setf (gethash previous dead) t))
          (clrhash pending)
          (setf (gethash store-target pending) inst))
        (%opt-clear-pending-aliasing-collection-stores pending inst)))))

(defun opt-pass-dce (instructions)
  "Dead code elimination via global usedness analysis.
   Pass 1: collect every register that appears as a source operand anywhere
   in the program into a 'used' set (ignoring control flow).
   Pass 2: remove pure instructions (vm-const, vm-move) whose destination
   register never appears in the used set.
   This is safe across branches/labels: a register defined in both branches
   is preserved as long as it is read anywhere -- the linear-order issue that
   plagued the previous backward-liveness DCE is entirely avoided."
  (let ((used (make-hash-table :test #'eq))
        (dead-stores (%opt-collection-dead-store-table instructions))
        (removed 0))
    ;; Pass 1: mark every register that is read by any instruction
    (dolist (inst instructions)
      (dolist (reg (opt-inst-read-regs inst))
        (setf (gethash reg used) t)))
    ;; Pass 2: drop DCE-eligible instructions (pure + alloc) whose dst is never read
    (prog1
        (remove-if (lambda (inst)
                      (let ((dead-p
                              (or (gethash inst dead-stores)
                                  (and (opt-inst-dce-eligible-p inst)
                                       (let ((dst (opt-inst-dst inst)))
                                         (and dst (not (gethash dst used))))))))
                       (when dead-p (incf removed))
                       dead-p))
                   instructions)
      (%opt-report :dce "removed=~D" removed))))

(defun opt-pass-dead-basic-blocks (instructions)
  "Eliminate unreachable basic blocks by round-tripping through the CFG.
   Reachability is computed from the entry block; unreachable blocks and their
   instructions are dropped when the CFG is flattened back to a linear list."
  (cfg-flatten (cfg-build instructions)))
