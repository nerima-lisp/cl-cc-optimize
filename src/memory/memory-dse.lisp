(in-package #:cl-cc/optimize/memory)

(defstruct (mem-pass-state (:conc-name mps-))
  "Mutable state shared by the DSE and store-to-load-forward passes."
  (result        nil                           :type list)
  (pending-order nil                           :type list)
  (pending-by-name (make-hash-table :test #'equal) :type hash-table))

(defun %mps-emit (state inst)
  (push inst (mps-result state)))

(defun %mps-pending-store (state key)
  (gethash key (mps-pending-by-name state)))

(defun %mps-remember-store (state key inst)
  (unless (gethash key (mps-pending-by-name state))
    (push key (mps-pending-order state)))
  (setf (gethash key (mps-pending-by-name state)) inst))

(defun %mps-drop-pending (state key)
  (remhash key (mps-pending-by-name state))
  (setf (mps-pending-order state)
        (remove key (mps-pending-order state) :test #'equal)))

(defun %mps-flush-one (state key)
  (when-let ((inst (%mps-pending-store state key)))
    (%mps-emit state inst)
    (%mps-drop-pending state key)))

(defun %mps-flush-all (state)
  (dolist (key (reverse (mps-pending-order state)))
    (%mps-flush-one state key)))

(defun %mps-pending-uses-reg-p (pending reg)
  "Return T if PENDING store instruction reads REG."
  (typecase pending
    (vm-set-global (eq (cl-cc/vm:vm-set-global-src pending) reg))
    (vm-slot-write (or (eq (cl-cc/vm:vm-slot-write-obj-reg pending) reg)
                       (eq (cl-cc/vm:vm-slot-write-value-reg pending) reg)))))

(defun %mps-flush-if-src-overwritten (state dst)
  "Flush pending stores that read DST (i.e., their source register is DST)."
  (dolist (key (copy-list (mps-pending-order state)))
    (let ((pending (%mps-pending-store state key)))
      (when (and pending (%mps-pending-uses-reg-p pending dst))
        (%mps-flush-one state key)))))

(defun %mps-flush-dependent-on-reg (state reg &key exclude-key)
  "Flush pending stores that reference REG (but not EXCLUDE-KEY)."
  (dolist (key (copy-list (mps-pending-order state)))
    (let ((pending (%mps-pending-store state key)))
      (when (and pending
                  (not (equal key exclude-key))
                  (%mps-pending-uses-reg-p pending reg))
        (%mps-flush-one state key)))))

(defun %mps-flush-slot-writes-aliasing-read
    (state read-inst alias-roots type-facts)
  "Flush pending slot writes that READ-INST may observe under TBAA metadata."
  (dolist (key (copy-list (mps-pending-order state)))
    (let ((pending (%mps-pending-store state key)))
      (when (and (typep pending 'vm-slot-write)
                 (opt-memory-accesses-may-alias-p
                  read-inst pending alias-roots type-facts))
        (%mps-flush-one state key)))))

;;; ─── Cons slot forwarding ─────────────────────────────────────────────────

(defun %opt-cons-slot-kill-dependent-on-reg (facts reg)
  "Remove cons-slot facts made stale when REG is overwritten."
  (when reg
    (let ((dead nil))
      (maphash (lambda (cell-reg entry)
                 (when (or (eq cell-reg reg)
                           (eq (car entry) reg)
                           (eq (cdr entry) reg))
                   (push cell-reg dead)))
               facts)
      (dolist (cell-reg dead)
        (remhash cell-reg facts)))))

(defun %opt-cons-slot-safe-source-p (dst car-reg cdr-reg)
  "Return T when a vm-cons DST write does not destroy either slot source."
  (not (or (eq dst car-reg) (eq dst cdr-reg))))

(defun %opt-cons-slot-mutation-boundary-p (inst)
  "Return T when INST may mutate heap/global state or call arbitrary code."
  (member (vm-inst-effect-kind inst)
          '(:write-global :io :control :unknown)
          :test #'eq))

(defun opt-pass-cons-slot-forward (instructions)
  "Forward car/cdr values from fresh vm-cons instructions to moves from their
original slot registers within a basic block. Clear remembered facts at
control-flow boundaries, calls, unknown effects, and heap mutations. Overwriting
a tracked cell register or slot source kills dependent facts."
  (let ((facts (make-hash-table :test #'eq))
        (result nil))
    (labels ((emit (inst) (push inst result))
             (remember-cons (dst car-reg cdr-reg)
                            (if (%opt-cons-slot-safe-source-p dst car-reg cdr-reg)
                                (setf (gethash dst facts) (cons car-reg cdr-reg))
                                (remhash dst facts)))
             (copy-fact (dst src)
                        (multiple-value-bind (entry found-p) (gethash src facts)
                          (if found-p
                              (setf (gethash dst facts) entry)
                              (remhash dst facts))))
             (handle-cons (inst)
                          (let ((dst (vm-dst inst)))
                            (%opt-cons-slot-kill-dependent-on-reg facts dst)
                            (remember-cons dst (vm-car-reg inst) (vm-cdr-reg inst))
                            (emit inst)))
             (handle-move (inst)
                          (let ((dst (vm-dst inst))
                                (src (vm-src inst)))
                            (unless (eq dst src)
                              (%opt-cons-slot-kill-dependent-on-reg facts dst)
                              (copy-fact dst src))
                            (emit inst)))
             (handle-slot-access (inst accessor)
                          (let* ((dst (vm-dst inst))
                                 (entry (gethash (vm-src inst) facts)))
                            (%opt-cons-slot-kill-dependent-on-reg facts dst)
                            (if entry
                                (emit (make-vm-move :dst dst :src (funcall accessor entry)))
                                (emit inst))))
             (handle-boundary (inst)
                          (clrhash facts)
                          (emit inst))
             (handle-default (inst)
                          (when-let ((dst (opt-inst-dst inst)))
                            (%opt-cons-slot-kill-dependent-on-reg facts dst))
                          (when (%opt-cons-slot-mutation-boundary-p inst)
                            (clrhash facts))
                          (emit inst)))
      (dolist (inst instructions (nreverse result))
        (typecase inst
          (vm-cons (handle-cons inst))
          (vm-move (handle-move inst))
          (vm-car (handle-slot-access inst #'car))
          (vm-cdr (handle-slot-access inst #'cdr))
          ((or vm-label vm-jump vm-jump-zero vm-ret vm-halt) (handle-boundary inst))
          (t (handle-default inst)))))))

(defun opt-pass-dead-store-elim (instructions)
  "Eliminate overwritten stores in straight-line code.

   Conservative rules:
   - a later store to the same global/slot kills the earlier pending store
   - vm-get-global / vm-slot-read force the matching pending store to be emitted first
   - labels and non-pure instructions flush all pending stores"
  (let* ((state (make-mem-pass-state))
         (metadata (opt-build-memory-tbaa-metadata instructions))
         (alias-roots (gethash :alias-roots metadata))
         (type-facts (gethash :type-facts metadata)))
    (dolist (inst instructions)
      (typecase inst
        (vm-label
         (%mps-flush-all state)
         (%mps-emit state inst))
        ((or vm-jump vm-jump-zero vm-ret vm-halt)
         (%mps-flush-all state)
         (%mps-emit state inst))
        (vm-get-global
         (when-let ((dst (cl-cc/vm:vm-get-global-dst inst)))
           (%mps-flush-if-src-overwritten state dst))
         (%mps-flush-one state (cl-cc/vm:vm-get-global-name inst))
         (%mps-emit state inst))
        (vm-slot-read
         (when-let ((dst (cl-cc/vm:vm-slot-read-dst inst)))
           (%mps-flush-if-src-overwritten state dst))
         (%mps-flush-slot-writes-aliasing-read
          state inst alias-roots type-facts)
         (%mps-emit state inst))
        (vm-set-global
         (%mps-remember-store state (cl-cc/vm:vm-set-global-name inst) inst))
        (vm-slot-write
         (%mps-remember-store state (opt-slot-alias-key (cl-cc/vm:vm-slot-write-obj-reg inst)
                                                        (cl-cc/vm:vm-slot-write-slot-name inst)
                                                        alias-roots)
                              inst))
        ;; FR-342. An array read cannot be shown to miss a pending store: the
        ;; index is a register, so two different index registers may still hold
        ;; the same value at run time. Flush every pending store rather than the
        ;; matching key alone.
        ((or vm-aref vm-aref-multi)
         (when-let ((dst (opt-inst-dst inst)))
           (%mps-flush-if-src-overwritten state dst))
         (%mps-flush-all state)
         (%mps-emit state inst))
        (vm-aset
         ;; Keyed on the array *and* index registers together, so a later store
         ;; kills an earlier one only when both are literally the same register —
         ;; distinct index registers may denote the same index, and killing
         ;; across them would drop a live store.
         (%mps-remember-store state (opt-slot-alias-key (cl-cc/vm:vm-array-reg inst)
                                                        (cl-cc/vm:vm-index-reg inst)
                                                        alias-roots)
                              inst))
        (t
         (when-let ((dst (opt-inst-dst inst)))
           (%mps-flush-if-src-overwritten state dst))
         (unless (opt-inst-pure-p inst) (%mps-flush-all state))
         (%mps-emit state inst))))
    (%mps-flush-all state)
    (nreverse (mps-result state))))
