(in-package #:cl-cc/optimize/inlining)

(defparameter *opt-pure-call-memo-max-size* 1024
  "Max entry count used by pure-call optimization memo table.
NIL disables size-based eviction for this optimization pass.")

(defun %opt-pure-call-boundary-p (inst)
  "Return T when INST ends a straight-line pure-call optimization region."
  (or (typep inst 'vm-label)
      (typep inst 'vm-jump)
      (typep inst 'vm-jump-zero)
      (typep inst 'vm-ret)
      (typep inst 'vm-halt)
      (typep inst 'vm-tail-call)))

(defun %opt-pure-call-key (label args)
  "Return the CSE memo key for a pure direct call to LABEL with ARGS."
  (cons label (copy-list args)))

(defun %opt-pure-call-entry-uses-reg-p (key result-reg reg)
  "Return T when KEY/RESULT-REG depends on REG."
  (or (eq result-reg reg)
      (member reg (cdr key) :test #'eq)))

(defun %opt-pure-call-kill-reg (call-cse reg)
  "Drop memoized pure-call entries invalidated by overwriting REG."
  (when reg
    (let ((doomed nil))
      (maphash (lambda (key result-reg)
                 (when (%opt-pure-call-entry-uses-reg-p key result-reg reg)
                   (push key doomed)))
               call-cse)
      (dolist (key doomed)
        (remhash key call-cse)))))

(defun %opt-pure-call-record-if-safe (call-cse key dst)
  "Record KEY -> DST only when DST does not overwrite a key argument register."
  (when (and key
             dst
             (not (member dst (cdr key) :test #'eq)))
    (setf (gethash key call-cse) dst)
    t))

(defun %opt-resolved-pure-direct-call-label (inst reg-track pure-labels)
  "Return INST's direct callee label when it is known and transitively pure."
  (when (typep inst 'vm-call)
    (let ((label (gethash (vm-func-reg inst) reg-track)))
      (and label
           (gethash label pure-labels)
           label))))

(defun %opt-rewrite-pure-call (inst reg-track pure-labels call-cse result)
  "Rewrite one vm-call INST for %OPT-REWRITE-PURE-DIRECT-CALLS's pure-call
CSE: reuse a cached pure result when it's still live in the same register,
emit a move when the cached result lives in another register, or emit and
memoize INST when its result isn't yet known. Return the updated RESULT
accumulator (built most-recent-first, as with PUSH)."
  (let* ((dst (opt-inst-dst inst))
         (label (%opt-resolved-pure-direct-call-label inst reg-track pure-labels))
         (args (vm-args inst))
         (key (and label dst (%opt-pure-call-key label args)))
         (existing (and key
                        (multiple-value-bind (cached found-p)
                            (opt-pure-function-memo-get call-cse pure-labels label args)
                          (and found-p cached)))))
    (cond
      ((and existing (eq existing dst))
       ;; The same register still holds the same pure result.
       result)
      (existing
       (%opt-pure-call-kill-reg call-cse dst)
       (remhash dst reg-track)
       (cons (make-vm-move :dst dst :src existing) result))
      (t
       (%opt-pure-call-kill-reg call-cse dst)
       (let ((new-result (cons inst result)))
         (remhash dst reg-track)
         (let ((recorded-p (%opt-pure-call-record-if-safe call-cse key dst)))
           (when (and recorded-p key dst)
             (opt-pure-function-memo-put call-cse pure-labels label args dst)))
         new-result)))))

(defun %opt-rewrite-pure-direct-calls (instructions name-to-label pure-labels)
  "Rewrite repeated pure direct calls inside straight-line regions.

The pass only reuses known direct `vm-call` sites whose callee label is marked
pure in PURE-LABELS. CSE state is flushed at labels and control-flow
boundaries, and memo entries are killed whenever one of their argument/result
registers is overwritten."
  (let ((reg-track (make-hash-table :test #'eq))
        (call-cse (opt-make-pure-function-memo-table
                   :max-size *opt-pure-call-memo-max-size*))
        (result nil))
    (labels ((flush-state ()
               (clrhash reg-track)
               (clrhash call-cse)))
      (dolist (inst instructions (nreverse result))
        (cond
          ((%opt-pure-call-boundary-p inst)
           (flush-state)
           (push inst result))
          ((typep inst 'vm-call)
           (setf result (%opt-rewrite-pure-call inst reg-track pure-labels call-cse result)))
          (t
           (let ((dst (opt-inst-dst inst)))
             (%opt-pure-call-kill-reg call-cse dst)
             (%opt-track-known-callee-label inst name-to-label reg-track)
             (push inst result))))))))

(defun %opt-remove-dead-pure-direct-calls (instructions name-to-label pure-labels)
  "Remove unused known-pure direct calls conservatively.

Only direct `vm-call` sites with a known pure callee label and an unused
destination register are removed.  Calls with unresolved callees, unknown side
effects, or no destination are preserved."
  (let ((used (make-hash-table :test #'eq))
        (reg-track (make-hash-table :test #'eq))
        (result nil))
    (dolist (inst instructions)
      (dolist (reg (opt-inst-read-regs inst))
        (setf (gethash reg used) t)))
    (labels ((flush-state ()
               (clrhash reg-track)))
      (dolist (inst instructions (nreverse result))
        (cond
          ((%opt-pure-call-boundary-p inst)
           (flush-state)
           (push inst result))
          ((typep inst 'vm-call)
           (let* ((dst (opt-inst-dst inst))
                  (label (%opt-resolved-pure-direct-call-label inst reg-track pure-labels)))
             (unless (and dst label (not (gethash dst used))) (remhash dst reg-track)
                   (push inst result))))
          (t
           (%opt-track-known-callee-label inst name-to-label reg-track)
           (push inst result)))))))

(defun opt-pass-pure-call-optimization (instructions)
  "Conservatively optimize repeated and dead known-pure direct calls.

The pass first infers a transitive pure-label set with
`opt-infer-transitive-function-purity`, then applies two safe rewrites:

1. Reuse repeated straight-line `vm-call` sites that resolve to the same pure
   callee label and the same argument registers.
2. Remove known-pure direct calls whose destination register is never used.

Unknown calls, `vm-apply`, `vm-generic-call`, unresolved indirect calls, and
recursive SCCs remain untouched by construction."
  ;; Fast bailout: skip the whole call-graph fixed-point when the input has
  ;; no VM-CALL instructions at all (the dominant case for tiny test inputs).
  (unless (some (lambda (inst) (typep inst 'vm-call)) instructions)
    (return-from opt-pass-pure-call-optimization instructions))
  (let ((pure-labels (opt-infer-transitive-function-purity instructions)))
    (when (zerop (hash-table-count pure-labels))
      (return-from opt-pass-pure-call-optimization instructions))
    (let* ((name-to-label (opt-build-function-name-map instructions))
           (cse-rewritten (%opt-rewrite-pure-direct-calls instructions name-to-label pure-labels)))
      (%opt-remove-dead-pure-direct-calls cse-rewritten name-to-label pure-labels))))
