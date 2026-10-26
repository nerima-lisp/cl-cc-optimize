;;;; optimizer-trans-validate.lisp — FR-752 Translation Validation
(in-package :cl-cc/optimize)

(defvar *translation-validation-enabled* nil
  "When non-NIL, validate before/after IR for every optimizer pass.")

(define-condition translation-validation-error (error)
  ((reason :initarg :reason :reader translation-validation-error-reason)
    (details :initarg :details :reader translation-validation-error-details)
    (before :initarg :before :reader translation-validation-error-before)
    (after :initarg :after :reader translation-validation-error-after))
  (:report
    (lambda (condition stream)
      (format
        stream
        "Translation validation failed (~A): ~S"
        (translation-validation-error-reason condition)
        (translation-validation-error-details condition)))))

(defun %tv-fail (reason before after &rest details)
  "Signal a structured translation-validation failure."
  (error
    'translation-validation-error
    :reason
    reason
    :details
    details
    :before
    before
    :after
    after))

(defun tv-symbolic-execute-block (instructions)
  "Return the basic outputs/control summary, support status, and a bounded
observable summary."
  (let ((env (make-hash-table :test (function eq)))
        (outputs nil)
        (control nil)
        (effects nil)
        (exit nil)
        (steps 0))
    (labels ((value-of (reg)
               (or (gethash reg env) (list :input reg)))
             (record-const (inst)
               (setf (gethash (vm-dst inst) env) (list :const (vm-value inst))))
             (record-move (inst)
               (setf (gethash (vm-dst inst) env) (value-of (vm-src inst))))
             (record-binop (inst)
               (setf (gethash (vm-dst inst) env)
                     (list (type-of inst) (value-of (vm-lhs inst)) (value-of (vm-rhs inst)))))
             (record-control (inst)
               (push (instruction->sexp inst) control))
             (record-print (inst)
               (push (list :print (value-of (first (opt-inst-read-regs inst)))) effects))
             (record-exit (inst kind)
               (let ((values (mapcar (function value-of) (opt-inst-read-regs inst))))
                 (push values outputs)
                 (setf exit (list kind (first values)))))
             (result ()
               (let ((basic-summary (list :outputs (nreverse outputs) :control (nreverse control))))
                 (values
                   basic-summary
                   t
                   (list
                     :effects
                     (nreverse effects)
                     :exit
                     exit
                     :control
                     (getf basic-summary :control))))))
      (dolist (inst instructions)
        (when (or exit (>= steps 4096))
          (return-from tv-symbolic-execute-block (values nil nil nil)))
        (incf steps)
        (typecase inst
          (vm-label nil)
          (vm-const (record-const inst))
          (vm-move (record-move inst))
          (vm-binop (record-binop inst))
          ((or vm-jump vm-jump-zero) (record-control inst))
          (vm-print (record-print inst))
          (vm-ret (record-exit inst :return))
          (vm-halt (record-exit inst :halt))
          (t (return-from tv-symbolic-execute-block (values nil nil nil)))))
      (result))))

(defun %tv-label-set (instructions)
  (sort
    (loop for inst in instructions
          when (typep inst 'vm-label)
            collect (vm-name inst))
    #'string<))

(defun %tv-observable-shape (instructions)
  "Return a semantic shape used as a safe validation filter."
  (list
    :labels
    (%tv-label-set instructions)
    :returns
    (count-if
      (lambda (inst)
        (typep inst '(or vm-ret vm-halt)))
      instructions)
    :errors
    (count-if
      (lambda (inst)
        (member
          (type-of inst)
          '(vm-signal vm-signal-error vm-error-instruction vm-cerror)
          :test
          #'eq))
      instructions)))

(defun translation-validation-equivalent-p (before after)
  "Conservatively check same observable outputs for same symbolic inputs."
  (multiple-value-bind (before-basic-summary before-supported-p before-summary)
      (tv-symbolic-execute-block before)
    (declare (ignore before-basic-summary))
    (multiple-value-bind (after-basic-summary after-supported-p after-summary)
        (tv-symbolic-execute-block after)
      (declare (ignore after-basic-summary))
      (and
        before-supported-p
        after-supported-p
        (or
          (equal
            (mapcar (function instruction->sexp) before)
            (mapcar (function instruction->sexp) after))
          (equal before-summary after-summary))))))

(defun %tv-defined-labels (instructions before after)
  "Return a label table for INSTRUCTIONS, rejecting duplicates."
  (let ((labels (make-hash-table
               :test
               #'equal)))
    (dolist (inst instructions labels)
      (when (typep inst 'vm-label)
        (let ((name (vm-name inst)))
          (when (gethash name labels)
            (%tv-fail :duplicate-label before after :label name))
          (setf (gethash name labels) t))))))

(defun %tv-jump-target (inst)
  "Return INST's label target when INST is a jump-like instruction."
  (and (typep inst '(or vm-jump vm-jump-zero)) (vm-label-name inst)))

(defun %tv-entry-label (instructions)
  "Return a stable entry-block marker for INSTRUCTIONS."
  (let ((first (first instructions)))
    (if (and first (typep first 'vm-label)) (vm-name first)
      :implicit-entry)))

(defun %tv-exit-kind (inst)
  "Return a compact control/exit signature for INST, or NIL."
  (cond
    ((typep inst 'vm-jump) (list :jump (vm-label-name inst)))
    ((typep inst 'vm-jump-zero) (list :branch (vm-label-name inst)))
    ((typep inst 'vm-ret) (list :return))
    ((typep inst 'vm-halt) (list :halt))
    (t nil)))

(defun %tv-table-increment (key table)
  "Increment KEY in TABLE using EQUAL keys."
  (incf (gethash key table 0)))

(defun %tv-exit-signature-counts (instructions)
  "Return a multiset of block-exit control signatures for INSTRUCTIONS."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (inst instructions counts)
      (when-let ((kind (%tv-exit-kind inst)))
        (%tv-table-increment kind counts)))))

(defun %tv-equal-count-tables-p (left right)
  "T when LEFT and RIGHT contain the same keys and counts."
  (and
    (= (hash-table-count left) (hash-table-count right))
    (loop for key being the hash-keys of left using (hash-value count)
          always (= count (gethash key right 0)))))

(defun %tv-validate-basic-block-structure (before after)
  "Conservatively validate O(n) CFG shape invariants."
  (let ((before-labels (%tv-defined-labels before before after))
        (after-labels (%tv-defined-labels after before after)))
    (unless (equal (%tv-entry-label before) (%tv-entry-label after))
      (%tv-fail
        :entry-edge-changed
        before
        after
        :before
        (%tv-entry-label before)
        :after
        (%tv-entry-label after)))
    (dolist (stream (list before after))
      (let ((labels (if
                   (eq stream
                     before)
                   before-labels
                   after-labels)))
        (dolist (inst stream)
          (let ((target (%tv-jump-target inst)))
            (when (and target (not (gethash target labels)))
              (%tv-fail
                :unknown-label-target
                before
                after
                :target
                target
                :instruction
                (instruction->sexp inst)))))))
    (unless (%tv-equal-count-tables-p
        (%tv-exit-signature-counts before)
        (%tv-exit-signature-counts after))
      (%tv-fail
        :exit-edges-changed
        before
        after
        :before
        (%tv-observable-shape before)
        :after
        (%tv-observable-shape after)))))

(defun %tv-uninitialized-reads (instructions)
  "Return registers read before any definition in INSTRUCTIONS."
  (let ((defined (make-hash-table :test #'eq))
        (uninitialized nil))
    (dolist (inst instructions (nreverse uninitialized))
      (dolist (reg (opt-inst-read-regs inst))
        (when (and reg (not (gethash reg defined)))
          (pushnew reg uninitialized :test #'eq)))
      (when-let ((dst (opt-inst-dst inst)))
        (setf (gethash dst defined) t)))))

(defun %tv-validate-liveness (before after)
  "Reject new use-before-definition registers introduced by AFTER."
  (let ((before-inputs (%tv-uninitialized-reads before)))
    (dolist (reg (%tv-uninitialized-reads after))
      (unless (member reg before-inputs :test #'eq)
        (%tv-fail :new-uninitialized-register before after :register reg)))))

(defun %tv-validate-instruction-count (before after)
  "Reject silent instruction drops unless the lightweight semantic check agrees."
  (when (and
      (< (length after) (length before))
      (not (translation-validation-equivalent-p before after)))
    (%tv-fail
      :instruction-count-decreased
      before
      after
      :before-count
      (length before)
      :after-count
      (length after))))

(defun validate-translation (before after)
  "Validate cheap O(n) IR invariants between BEFORE and AFTER.

Returns T when AFTER is a plausible translation of BEFORE.  Signals
TRANSLATION-VALIDATION-ERROR with diagnostic details when a heuristic invariant
is violated.  This is intentionally lightweight: it checks instruction-count
sanity, basic-block entry/exit integrity, and register liveness consistency, but
does not attempt formal equivalence proof."
  (%tv-validate-instruction-count before after)
  (%tv-validate-basic-block-structure before after)
  (%tv-validate-liveness before after)
  t)

(defun validate-optimizer-translation (pass before after)
  "Warn when PASS violates lightweight IR translation invariants. Never abort compilation."
  (when (and *translation-validation-enabled* (not (eq before after)))
    (handler-case (validate-translation before after)
      (translation-validation-error (condition)
        (warn "Translation validation failed for ~S: ~A" pass condition))))
  after)

(defun opt-pass-translation-validation (instructions)
  "FR-752 registration pass. Per-pass validation is pipeline-integrated."
  instructions)
