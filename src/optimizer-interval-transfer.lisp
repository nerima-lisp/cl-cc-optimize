;;;; optimizer-interval-transfer.lisp — interval dataflow transfer functions
;;;;
;;;; Per-instruction abstract interpretation over the interval arithmetic
;;;; in optimizer-interval-arithmetic.lisp, dispatched through the tables in
;;;; optimizer-interval-data.lisp: merges interval states at join points and
;;;; updates a block's interval state one instruction at a time. Consumed by
;;;; opt-pass-elide-proven-overflow-checks to drop a checked-arithmetic
;;;; instruction's overflow check once its operand ranges prove it can't
;;;; overflow.

(in-package :cl-cc/optimize)

(defun %opt-copy-interval-state (intervals)
  "Return a deep EQ hash-table copy of INTERVALS."
  (let ((copy (make-hash-table :test #'eq)))
    (when intervals
      (maphash (lambda (reg interval)
                 (setf (gethash reg copy)
                       (opt-make-interval (opt-interval-lo interval)
                                          (opt-interval-hi interval))))
               intervals))
    copy))

(defun %opt-interval-equal-p (a b)
  "Return T when intervals A and B have identical closed bounds."
  (and (= (opt-interval-lo a) (opt-interval-lo b))
       (= (opt-interval-hi a) (opt-interval-hi b))))

(defun %opt-interval-state-equal-p (a b)
  "Return T when A and B contain the same reg -> interval facts."
  (and (= (hash-table-count a) (hash-table-count b))
       (loop for reg being the hash-keys of a
             for interval = (gethash reg a)
             always (multiple-value-bind (other found-p) (gethash reg b)
                      (and found-p
                           (%opt-interval-equal-p interval other))))))

(defun %opt-merge-interval-states (states)
  "Meet interval STATES at a CFG join.

Only registers known on every incoming path are preserved. Their intervals are
unioned conservatively as [min lo, max hi]."
  (if (null states)
      (make-hash-table :test #'eq)
      (let* ((merged (%opt-copy-interval-state (first states)))
             (rest-states (rest states))
             (keys (loop for reg being the hash-keys of merged collect reg)))
        (dolist (reg keys merged)
          (let* ((base (gethash reg merged))
                 (lo (opt-interval-lo base))
                 (hi (opt-interval-hi base))
                 (keep-p t))
            (dolist (state rest-states)
              (multiple-value-bind (other found-p) (gethash reg state)
                (unless found-p
                  (setf keep-p nil)
                  (return))
                (setf lo (min lo (opt-interval-lo other))
                      hi (max hi (opt-interval-hi other)))))
            (if keep-p
                (setf (gethash reg merged) (opt-make-interval lo hi))
                (remhash reg merged)))))))

(defun %opt-update-interval-binop (inst intervals fn)
  "Update INTERVALS for binary arithmetic INST using interval combinator FN.
If either operand has no known interval, conservatively kills the destination."
  (let ((lhs (gethash (vm-lhs inst) intervals))
        (rhs (gethash (vm-rhs inst) intervals)))
    (if (and lhs rhs)
        (setf (gethash (vm-dst inst) intervals) (funcall fn lhs rhs))
        (remhash (vm-dst inst) intervals))))

(defun %opt-update-interval-unary (inst intervals fn)
  "Update INTERVALS for unary arithmetic INST using interval transformer FN."
  (let ((src (gethash (vm-src inst) intervals)))
    (if src
        (setf (gethash (vm-dst inst) intervals) (funcall fn src))
        (remhash (vm-dst inst) intervals))))

(defun %opt-update-interval-logand (inst intervals)
  "Update INTERVALS for LOGAND, preserving non-negative mask bounds when known."
  (let* ((lhs (gethash (vm-lhs inst) intervals))
         (rhs (gethash (vm-rhs inst) intervals))
         (interval (opt-interval-logand lhs rhs)))
    (if interval
        (setf (gethash (vm-dst inst) intervals) interval)
        (remhash (vm-dst inst) intervals))))

(defun %opt-update-interval-logior (inst intervals)
  "Update INTERVALS for LOGIOR with non-negative known-bits bounds when provable."
  (let* ((lhs (gethash (vm-lhs inst) intervals))
         (rhs (gethash (vm-rhs inst) intervals))
         (interval (opt-interval-logior lhs rhs)))
    (if interval
        (setf (gethash (vm-dst inst) intervals) interval)
        (remhash (vm-dst inst) intervals))))

(defun %opt-update-interval-logxor (inst intervals)
  "Update INTERVALS for LOGXOR with non-negative known-bits bounds when provable."
  (let* ((lhs (gethash (vm-lhs inst) intervals))
         (rhs (gethash (vm-rhs inst) intervals))
         (interval (opt-interval-logxor lhs rhs)))
    (if interval
        (setf (gethash (vm-dst inst) intervals) interval)
        (remhash (vm-dst inst) intervals))))

(defun %opt-update-interval-ash (inst intervals)
  "Update INTERVALS for ASH when both source and shift ranges are known.

The shift range must be a singleton integer interval."
  (let* ((value (gethash (vm-lhs inst) intervals))
         (shift (gethash (vm-rhs inst) intervals))
         (interval (and value shift (opt-interval-ash value shift))))
    (if interval
        (setf (gethash (vm-dst inst) intervals) interval)
        (remhash (vm-dst inst) intervals))))

(defun %opt-interval-binop-entry (inst)
  "Return the interval binary transformer symbol for INST, or NIL."
  (loop for (type . fn-sym) in *opt-interval-binop-table*
        when (typep inst type)
        return fn-sym))

(defun %opt-interval-unary-entry (inst)
  "Return the interval unary transformer designator for INST, or NIL."
  (loop for (type . fn) in *opt-interval-unary-table*
        when (typep inst type)
        return fn))

(defun %opt-interval-function (designator)
  "Resolve an interval transformer DESIGNATOR to a function."
  (etypecase designator
    (symbol (symbol-function designator))
    (function designator)))

(defun %opt-self-referential-range-update-p (inst)
  "Return T when INST reads and overwrites the same destination register."
  (let ((dst (opt-inst-dst inst)))
    (and dst
         (not (typep inst 'vm-move))
         (member dst (opt-inst-read-regs inst) :test #'eq))))

(defun %opt-transfer-interval-inst (inst intervals &key kill-self-updates)
  "Apply INST to INTERVALS conservatively and return INTERVALS.

With KILL-SELF-UPDATES, instructions that read their own destination kill that
fact instead of expanding ranges indefinitely across loop backedges."
  (cond
    ((typep inst 'vm-const)
     (if (integerp (vm-value inst))
         (setf (gethash (vm-dst inst) intervals)
               (opt-make-interval (vm-value inst) (vm-value inst)))
         (remhash (vm-dst inst) intervals)))
    ((typep inst 'vm-move)
     (let ((src (gethash (vm-src inst) intervals)))
       (if src
           (setf (gethash (vm-dst inst) intervals) src)
           (remhash (vm-dst inst) intervals))))
    ((and kill-self-updates (%opt-self-referential-range-update-p inst))
     (let ((dst (opt-inst-dst inst)))
       (when dst
         (remhash dst intervals))))
    (t
     (let ((binop-entry (%opt-interval-binop-entry inst))
           (unary-entry (%opt-interval-unary-entry inst)))
         (cond
           ((typep inst 'vm-logand)
            (%opt-update-interval-logand inst intervals))
           ((typep inst 'vm-logior)
            (%opt-update-interval-logior inst intervals))
           ((typep inst 'vm-logxor)
            (%opt-update-interval-logxor inst intervals))
           ((typep inst 'vm-ash)
            (%opt-update-interval-ash inst intervals))
           (binop-entry
            (%opt-update-interval-binop inst intervals
                                        (%opt-interval-function binop-entry)))
         (unary-entry
          (%opt-update-interval-unary inst intervals
                                      (%opt-interval-function unary-entry)))
         (t
          (let ((dst (opt-inst-dst inst)))
            (when dst
              (remhash dst intervals))))))))
  intervals)

(defun %opt-checked-arithmetic-elision-entry (inst)
  "Return the checked-arithmetic elision entry for INST, or NIL."
  (loop for (type . entry) in *opt-checked-arithmetic-elision-table*
        when (typep inst type)
        return entry))

(defun %opt-rewrite-checked-arithmetic-if-safe (inst intervals)
  "Rewrite checked arithmetic INST to unchecked integer arithmetic if ranges prove safety."
  (let ((entry (%opt-checked-arithmetic-elision-entry inst)))
    (when entry
      (destructuring-bind (interval-fn . constructor) entry
        (let ((lhs (gethash (vm-lhs inst) intervals))
              (rhs (gethash (vm-rhs inst) intervals)))
          (when (and lhs rhs)
            (let ((result (funcall (symbol-function interval-fn) lhs rhs)))
              (when (opt-interval-fits-fixnum-p result)
                (funcall (symbol-function constructor)
                         :dst (vm-dst inst)
                         :lhs (vm-lhs inst)
                         :rhs (vm-rhs inst))))))))))

(defun opt-pass-elide-proven-overflow-checks (instructions)
  "Elide FR-303 checked arithmetic when interval analysis proves fixnum safety."
  (let ((intervals (make-hash-table :test #'eq))
        (result nil))
    (dolist (inst instructions (nreverse result))
      (let ((replacement (%opt-rewrite-checked-arithmetic-if-safe inst intervals)))
        (push (or replacement inst) result))
      (%opt-transfer-interval-inst inst intervals))))
