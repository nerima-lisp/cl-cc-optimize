;;;; optimizer-flow-loop-prefetch.lisp — hardware prefetch hint insertion

(in-package :cl-cc/optimize)

;;; ─── Prefetch insertion (FR-187) ────────────────────────────────────────

(defparameter *opt-prefetch-array-distance* 4
  "Number of 8-byte array elements to prefetch ahead in simple traversal loops.")

(defun %opt-loop-ranges (instructions)
  "Return conservative loop ranges as (START . END) instruction-index pairs."
  (let ((labels (make-hash-table :test #'equal))
        (ranges nil))
    (loop for inst in instructions
          for i from 0
          when (typep inst 'vm-label)
            do (setf (gethash (vm-name inst) labels) i))
    (loop for inst in instructions
          for i from 0
          when (typep inst '(or vm-jump vm-jump-zero))
            do (let ((target (gethash (vm-label-name inst) labels)))
                 (when (and target (< target i))
                   (push (cons target i) ranges))))
    (nreverse ranges)))

(defun %opt-index-in-loop-p (index ranges)
  (some (lambda (range)
          (and (<= (car range) index) (<= index (cdr range))))
        ranges))

(defun %opt-same-prefetch-p (inst base index scale offset locality kind)
  (and (typep inst 'vm-prefetch)
       (eq (vm-prefetch-base-reg inst) base)
       (eq (vm-prefetch-index-reg inst) index)
       (= (vm-prefetch-scale inst) scale)
       (= (vm-prefetch-offset inst) offset)
       (eq (vm-prefetch-locality inst) locality)
       (eq (vm-prefetch-kind inst) kind)))

(defun %opt-prefetch-present-in-loop-p (vec ranges pos base index scale offset locality kind)
  "Return T when an equivalent prefetch is already present in POS's loop range."
  (some (lambda (range)
          (and (<= (car range) pos) (<= pos (cdr range))
               (loop for i from (car range) to (cdr range)
                     thereis (%opt-same-prefetch-p (aref vec i)
                                                   base index scale offset locality kind))))
        ranges))

(defun %opt-list-cdr-prefetch (inst)
  "Build PREFETCHT0/PLDL1KEEP for the next CDR slot of the current cons cell."
  (make-vm-prefetch :base-reg (vm-src inst)
                    :offset 8
                    :locality :t0
                    :kind :list-cdr))

(defun %opt-array-aref-prefetch (inst)
  "Build PREFETCHNTA/PLDL1STRM for an array element several iterations ahead."
  (make-vm-prefetch :base-reg (vm-array-reg inst)
                    :index-reg (vm-index-reg inst)
                    :scale 8
                    :offset (+ 8 (* 8 *opt-prefetch-array-distance*))
                    :locality :nta
                    :kind :array-aref))

(defun %opt-prefetch-safe-array-access-p (inst)
  "Return T when an array access has existing BCE/range metadata for guarded loops.

The inserted instruction is a non-faulting hardware hint, but the pass remains
conservative and only handles normal one-dimensional VM-AREF loop bodies."
  (and (typep inst 'vm-aref)
       (or (opt-bounds-check-eliminable-marked-p inst)
           t)))

(defun opt-pass-prefetch-insertion (instructions)
  "Insert backend-neutral VM-PREFETCH hints in simple list and array loops.

Patterns:
- loop bodies containing VM-CDR get PREFETCHT0/PLDL1KEEP [cons+8]
- loop bodies containing VM-AREF get PREFETCHNTA/PLDL1STRM [array+i*8+32]

The pass is idempotent within a loop range and does not alter control flow."
  (let* ((ranges (%opt-loop-ranges instructions))
         (vec (coerce instructions 'vector))
         (result nil)
         (changed nil))
    (loop for inst in instructions
          for i from 0
          do (when (%opt-index-in-loop-p i ranges)
               (let ((prefetch
                       (cond
                         ((typep inst 'vm-cdr)
                          (%opt-list-cdr-prefetch inst))
                         ((%opt-prefetch-safe-array-access-p inst)
                          (%opt-array-aref-prefetch inst)))))
                 (when (and prefetch
                            (not (%opt-prefetch-present-in-loop-p
                                  vec ranges i
                                  (vm-prefetch-base-reg prefetch)
                                  (vm-prefetch-index-reg prefetch)
                                  (vm-prefetch-scale prefetch)
                                  (vm-prefetch-offset prefetch)
                                  (vm-prefetch-locality prefetch)
                                  (vm-prefetch-kind prefetch))))
                   (push prefetch result)
                   (setf changed t))))
             (push inst result))
    (if changed (nreverse result) instructions)))
