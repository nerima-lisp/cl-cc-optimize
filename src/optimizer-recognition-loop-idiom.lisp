;;;; optimizer-recognition-loop-idiom.lisp — Loop Idiom Recognition Passes
;;;;
;;;; Fill and copy loop recognition (FR-055). Scans the instruction stream
;;;; for canonical zero-based array fill/copy loops and replaces them with
;;;; single side-effecting bulk operations (vm-fill, bulk copy).
;;;;
;;;; Load order: after optimizer-recognition.lisp.

(in-package :cl-cc/optimize)

(defun %opt-branch-target-counts (instructions)
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (inst instructions counts)
      (when (or (typep inst 'vm-jump)
                (typep inst 'vm-jump-zero))
        (incf (gethash (vm-label-name inst) counts 0))))))

(defun %opt-distinct-registers-p (&rest regs)
  (loop for tail on (remove nil regs)
        always (not (member (car tail) (cdr tail) :test #'eq))))

(defun %opt-fill-loop-replacement (array-reg val-reg len-reg idx-reg next-reg cond-reg one-reg)
  (list (make-vm-fill :array-reg array-reg :val-reg val-reg)
        (make-vm-move :dst next-reg :src len-reg)
        (make-vm-move :dst idx-reg :src len-reg)
        (make-vm-const :dst cond-reg :value 0)
        (make-vm-const :dst one-reg :value 1)))

(defun %opt-register-read-after-p (reg instructions start)
  "Return T when REG is read by any instruction at or after START."
  (loop for i from start below (length instructions)
        thereis (member reg (opt-inst-read-regs (nth i instructions)) :test #'eq)))

(defun %opt-copy-arrays-provably-distinct-p (dst-reg src-reg alias-roots)
  "Return T when alias facts prove DST-REG and SRC-REG are distinct arrays."
  (and (not (eq dst-reg src-reg))
       (hash-table-p alias-roots)
       (not (opt-may-alias-p dst-reg src-reg alias-roots))))

(defun %opt-copy-loop-replacement (dst-array-reg src-array-reg len-reg idx-reg
                                   next-reg cond-reg one-reg func-reg)
  "Build a bulk COPY-SEQ/REPLACE-style replacement for a full array copy loop.

The final induction, compare, and increment temporaries are restored to the
values produced by the original loop on normal exit."
  (list (make-vm-func-ref :dst func-reg :label "REPLACE")
        (make-vm-const :dst cond-reg :value :end1)
        (make-vm-const :dst one-reg :value :end2)
        (make-vm-call :dst dst-array-reg
                      :func func-reg
                      :args (list dst-array-reg src-array-reg
                                  cond-reg len-reg one-reg len-reg))
        (make-vm-move :dst next-reg :src len-reg)
        (make-vm-move :dst idx-reg :src len-reg)
        (make-vm-const :dst cond-reg :value 0)
        (make-vm-const :dst one-reg :value 1)))

(defun opt-fill-recognition-match-at (instructions pos)
  "Recognize a canonical full-vector fill loop and replace it with vm-fill.

   The recognized form is intentionally narrow: ARRAY-LENGTH immediately before
   the induction initialization, zero-based index, `< length` guard, exactly one
   ASET store, increment by one, and private loop/exit labels."
  (let ((end (+ pos 10)))
    (when (and (> pos 0)
               (<= end (length instructions)))
      (let* ((len-inst   (nth (1- pos) instructions))
             (init       (nth (+ pos 0) instructions))
             (header     (nth (+ pos 1) instructions))
             (cmp        (nth (+ pos 2) instructions))
             (exit-jump  (nth (+ pos 3) instructions))
             (store      (nth (+ pos 4) instructions))
             (one        (nth (+ pos 5) instructions))
             (inc        (nth (+ pos 6) instructions))
             (step       (nth (+ pos 7) instructions))
             (back-jump  (nth (+ pos 8) instructions))
             (exit-label (nth (+ pos 9) instructions)))
        (when (and (typep len-inst 'vm-array-length)
                   (typep init 'vm-const)
                   (eql (vm-value init) 0)
                   (typep header 'vm-label)
                   (typep cmp 'vm-lt)
                   (typep exit-jump 'vm-jump-zero)
                   (typep store 'vm-aset)
                   (typep one 'vm-const)
                   (eql (vm-value one) 1)
                   (typep inc 'vm-add)
                   (typep step 'vm-move)
                   (typep back-jump 'vm-jump)
                   (typep exit-label 'vm-label))
          (let* ((array-reg (vm-src len-inst))
                 (len-reg   (vm-dst len-inst))
                 (idx-reg   (vm-dst init))
                 (cond-reg  (vm-dst cmp))
                 (one-reg   (vm-dst one))
                 (next-reg  (vm-dst inc))
                 (val-reg   (vm-val-reg store))
                 (header-name (vm-name header))
                 (exit-name   (vm-name exit-label))
                 (target-counts (%opt-branch-target-counts instructions)))
            (when (and (eq (vm-lhs cmp) idx-reg)
                       (eq (vm-rhs cmp) len-reg)
                       (eq (vm-reg exit-jump) cond-reg)
                       (equal (vm-label-name exit-jump) exit-name)
                       (eq (vm-array-reg store) array-reg)
                       (eq (vm-index-reg store) idx-reg)
                       (or (and (eq (vm-lhs inc) idx-reg)
                                (eq (vm-rhs inc) one-reg))
                           (and (eq (vm-lhs inc) one-reg)
                                (eq (vm-rhs inc) idx-reg)))
                       (eq (vm-dst step) idx-reg)
                       (eq (vm-src step) next-reg)
                       (equal (vm-label-name back-jump) header-name)
                       (= (gethash header-name target-counts 0) 1)
                       (= (gethash exit-name target-counts 0) 1)
                       (%opt-distinct-registers-p len-reg idx-reg cond-reg one-reg next-reg)
                       (not (member array-reg (list idx-reg cond-reg one-reg next-reg) :test #'eq))
                       (not (member val-reg (list idx-reg cond-reg one-reg next-reg) :test #'eq)))
              (values (%opt-fill-loop-replacement
                       array-reg val-reg len-reg idx-reg next-reg cond-reg one-reg)
                      10))))))))

(defun opt-pass-fill-recognition (instructions)
  "Collapse a canonical zero-based array fill loop into a side-effecting vm-fill.

   This is the conservative FR-055 slice for full-vector fill idioms.  The pass
   preserves final loop temporaries after the bulk fill so later code observing
   the induction or condition registers keeps the same values."
  (loop with n      = (length instructions)
        with result = nil
        with i      = 0
        while (< i n)
        do (multiple-value-bind (rewritten consumed)
               (opt-fill-recognition-match-at instructions i)
             (if (and rewritten (integerp consumed) (plusp consumed))
                 (progn
                   (dolist (inst rewritten)
                     (push inst result))
                   (incf i consumed))
                 (progn
                   (push (nth i instructions) result)
                   (incf i 1))))
        finally (return (nreverse result))))

(defun opt-copy-recognition-match-at (instructions pos alias-roots)
  "Recognize a canonical full-vector copy loop and replace it with a bulk copy.

   The accepted idiom is intentionally narrow and mirrors fill recognition:
   ARRAY-LENGTH immediately precedes the zero initialization, the guard is
   `idx < length`, the body is exactly `(aset dst idx (aref src idx))`, the
   increment is by one, and the loop/exit labels are private.  Source and
   destination must be proved distinct by the existing heap-root alias oracle.
   The bound may come from ARRAY-LENGTH of either vector or from an already
   computed length register used by the loop guard."
  (let ((end (+ pos 11)))
    (when (and (> pos 0)
               (<= end (length instructions)))
      (let* ((len-inst   (nth (1- pos) instructions))
             (init       (nth (+ pos 0) instructions))
             (header     (nth (+ pos 1) instructions))
             (cmp        (nth (+ pos 2) instructions))
             (exit-jump  (nth (+ pos 3) instructions))
             (load       (nth (+ pos 4) instructions))
             (store      (nth (+ pos 5) instructions))
             (one        (nth (+ pos 6) instructions))
             (inc        (nth (+ pos 7) instructions))
             (step       (nth (+ pos 8) instructions))
             (back-jump  (nth (+ pos 9) instructions))
             (exit-label (nth (+ pos 10) instructions)))
        (when (and (typep init 'vm-const)
                   (eql (vm-value init) 0)
                   (typep header 'vm-label)
                   (typep cmp 'vm-lt)
                   (typep exit-jump 'vm-jump-zero)
                   (typep load 'vm-aref)
                   (typep store 'vm-aset)
                   (typep one 'vm-const)
                   (eql (vm-value one) 1)
                   (typep inc 'vm-add)
                   (typep step 'vm-move)
                   (typep back-jump 'vm-jump)
                   (typep exit-label 'vm-label))
          (let* ((src-array-reg (vm-array-reg load))
                 (dst-array-reg (vm-array-reg store))
                  (len-reg       (vm-rhs cmp))
                 (idx-reg       (vm-dst init))
                 (cond-reg      (vm-dst cmp))
                 (one-reg       (vm-dst one))
                 (next-reg      (vm-dst inc))
                 (load-reg      (vm-dst load))
                 (header-name   (vm-name header))
                 (exit-name     (vm-name exit-label))
                 (target-counts (%opt-branch-target-counts instructions)))
             (when (and (or (not (typep len-inst 'vm-array-length))
                            (and (eq (vm-dst len-inst) len-reg)
                                 (member (vm-src len-inst)
                                         (list src-array-reg dst-array-reg)
                                         :test #'eq)))
                        (eq (vm-lhs cmp) idx-reg)
                       (eq (vm-rhs cmp) len-reg)
                       (eq (vm-reg exit-jump) cond-reg)
                       (equal (vm-label-name exit-jump) exit-name)
                       (eq (vm-index-reg load) idx-reg)
                       (eq (vm-index-reg store) idx-reg)
                       (eq (vm-val-reg store) load-reg)
                       (or (and (eq (vm-lhs inc) idx-reg)
                                (eq (vm-rhs inc) one-reg))
                           (and (eq (vm-lhs inc) one-reg)
                                (eq (vm-rhs inc) idx-reg)))
                       (eq (vm-dst step) idx-reg)
                       (eq (vm-src step) next-reg)
                       (equal (vm-label-name back-jump) header-name)
                       (= (gethash header-name target-counts 0) 1)
                       (= (gethash exit-name target-counts 0) 1)
                       (%opt-distinct-registers-p
                        len-reg idx-reg cond-reg one-reg next-reg load-reg)
                       (not (member src-array-reg
                                    (list idx-reg cond-reg one-reg next-reg load-reg) :test #'eq))
                       (not (member dst-array-reg
                                    (list idx-reg cond-reg one-reg next-reg load-reg) :test #'eq))
                       (%opt-copy-arrays-provably-distinct-p
                        dst-array-reg src-array-reg alias-roots)
                       (not (%opt-register-read-after-p load-reg instructions end)))
              (values (%opt-copy-loop-replacement dst-array-reg src-array-reg len-reg idx-reg
                                                  next-reg cond-reg one-reg load-reg)
                      11))))))))

(defun opt-pass-copy-recognition (instructions)
  "Collapse a canonical zero-based array copy loop into a bounded bulk copy.

   This is the copy-seq/memcpy side of FR-055.  It is deliberately conservative:
   it only fires when existing heap-root alias facts prove the source and
   destination arrays cannot be the same object, preserving in-place overlapping
   copy loops for the regular VM executor."
  (let ((alias-roots (opt-compute-heap-aliases instructions)))
    (loop with n      = (length instructions)
          with result = nil
          with i      = 0
          while (< i n)
          do (multiple-value-bind (rewritten consumed)
                 (opt-copy-recognition-match-at instructions i alias-roots)
               (if (and rewritten (integerp consumed) (plusp consumed))
                   (progn
                     (dolist (inst rewritten)
                       (push inst result))
                     (incf i consumed))
                   (progn
                     (push (nth i instructions) result)
                     (incf i 1))))
          finally (return (nreverse result)))))
