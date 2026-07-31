;;;; optimizer-recognition.lisp — Tree Pattern Recognition Passes
;;;;
;;;; Bswap (byte-swap) and rotate instruction recognition.
;;;; Scans the instruction stream for multi-instruction idioms and replaces
;;;; them with single specialized instructions (vm-bswap, vm-rotate).
;;;;
;;;; Load order: after optimizer-strength.lisp.

(in-package :cl-cc/optimize)

(defun %opt-bswap-window-shape-valid-p (c0 a0 s0 b0 c1 a1 s1 b1 c2 a2 s2 b2 c3 a3 s3 b3 o0 o1 o2)
  "T when the fixed-role instructions in a candidate bswap window have the
   expected vm instruction types and mask immediates for the canonical
   32-bit reverse-bytes idiom (four masked extracts, four shifts, three ORs)."
  (and (typep c0 'vm-const)
       (eql (vm-value c0) #xFF)
       (typep a0 'vm-logand)
       (typep s0 'vm-const)
       (typep b0 'vm-ash)
       (typep c1 'vm-const)
       (eql (vm-value c1) #xFF00)
       (typep a1 'vm-logand)
       (typep s1 'vm-const)
       (typep b1 'vm-ash)
       (typep c2 'vm-const)
       (eql (vm-value c2) #xFF0000)
       (typep a2 'vm-logand)
       (typep s2 'vm-const)
       (typep b2 'vm-ash)
       (typep c3 'vm-const)
       (eql (vm-value c3) #xFF000000)
       (typep a3 'vm-logand)
       (typep s3 'vm-const)
       (typep b3 'vm-ash)
       (typep o0 'vm-logior)
       (typep o1 'vm-logior)
       (typep o2 'vm-logior)))

(defun opt-bswap-recognition-match-at (instructions pos)
  (let ((end (+ pos 19)))
    (when (<= end (length instructions))
      (destructuring-bind (c0 a0 s0 b0 c1 a1 s1 b1 c2 a2 s2 b2 c3 a3 s3 b3 o0 o1 o2)
          (subseq instructions pos end)
        (when (%opt-bswap-window-shape-valid-p c0 a0 s0 b0 c1 a1 s1 b1 c2 a2 s2 b2 c3 a3 s3 b3 o0 o1 o2)
          (let* ((src (vm-lhs a0))
                 (dst (vm-dst o2)))
            (labels ((mask-extraction-consistent-p ()
                       ;; Each AND reads the same source register and masks it
                       ;; with its own byte-selecting constant, and the shift
                       ;; amounts are the canonical big-to-little permutation.
                       (and (eq (vm-lhs a0) src)
                            (eq (vm-rhs a0) (vm-dst c0))
                            (eq (vm-lhs a1) src)
                            (eq (vm-rhs a1) (vm-dst c1))
                            (eq (vm-lhs a2) src)
                            (eq (vm-rhs a2) (vm-dst c2))
                            (eq (vm-lhs a3) src)
                            (eq (vm-rhs a3) (vm-dst c3))
                            (equal (mapcar #'vm-value (list s0 s1 s2 s3)) '(24 8 -8 -24))))
                     (shift-wiring-consistent-p ()
                       ;; Each shift reads the corresponding masked extract and
                       ;; its corresponding shift-amount constant.
                       (and (eq (vm-lhs b0) (vm-dst a0))
                            (eq (vm-rhs b0) (vm-dst s0))
                            (eq (vm-lhs b1) (vm-dst a1))
                            (eq (vm-rhs b1) (vm-dst s1))
                            (eq (vm-lhs b2) (vm-dst a2))
                            (eq (vm-rhs b2) (vm-dst s2))
                            (eq (vm-lhs b3) (vm-dst a3))
                            (eq (vm-rhs b3) (vm-dst s3))))
                     (combine-wiring-consistent-p ()
                       ;; The three ORs fold the four shifted bytes together
                       ;; pairwise, then combine the two pairs.
                       (and (eq (vm-lhs o0) (vm-dst b0))
                            (eq (vm-rhs o0) (vm-dst b1))
                            (eq (vm-lhs o1) (vm-dst b2))
                            (eq (vm-rhs o1) (vm-dst b3))
                            (eq (vm-lhs o2) (vm-dst o0))
                            (eq (vm-rhs o2) (vm-dst o1)))))
              (when (and (mask-extraction-consistent-p)
                         (shift-wiring-consistent-p)
                         (combine-wiring-consistent-p))
                (values (make-vm-bswap :dst dst :src src) 19)))))))))

;;; Shared skeleton for single-pass recognition passes.
;;; MATCH-FN: (instructions pos) → (values rewritten consumed) or (nil 0)
;;; PUSH-FN:  (rewritten result) → new result with rewritten prepended
(defun %opt-recognition-pass (instructions match-fn push-fn)
  (loop with n      = (length instructions)
        with result = nil
        with i      = 0
        while (< i n)
        do (multiple-value-bind (rewritten consumed)
               (funcall match-fn instructions i)
             (if rewritten
                 (progn (setf result (funcall push-fn rewritten result))
                        (incf i consumed))
                 (progn (push (nth i instructions) result)
                        (incf i))))
        finally (return (nreverse result))))

(defun opt-pass-bswap-recognition (instructions)
  "Collapse explicit byte-swap bit-manipulation trees into vm-bswap.

   Recognizes the canonical 32-bit reverse-bytes tree built from four masked
   extracts, four shifts, and three logior nodes, matching the bswap pattern
   documented for the backend."
  (%opt-recognition-pass instructions
                         #'opt-bswap-recognition-match-at
                         (lambda (rewritten result) (cons rewritten result))))

(defun %opt-rotate-window-shape-valid-p (c0 a0 c1 a1 o0)
  "T when the fixed-role instructions in a candidate rotate window have the
   expected vm instruction types for the two-shift + OR rotate idiom."
  (and (typep c0 'vm-const)
       (typep a0 'vm-ash)
       (typep c1 'vm-const)
       (typep a1 'vm-ash)
       (typep o0 'vm-logior)))

(defun %opt-rotate-build-replacement (k0 k1 c0 c1 src0 out-dst)
  "Build the normalized-count-constant + vm-rotate replacement for a matched
   rotate window, choosing whichever shift amount is the positive rotate
   count.  Returns NIL when neither shift is the complement of the other."
  (cond
    ((and (plusp k0) (= k1 (- k0 64)))
     (values (list (make-vm-const :dst (vm-dst c1)
                                  :value (mod (- 64 k0) 64))
                   (make-vm-rotate :dst out-dst
                                   :lhs src0
                                   :rhs (vm-dst c1)))
             5))
    ((and (plusp k1) (= k0 (- k1 64)))
     (values (list (make-vm-const :dst (vm-dst c0)
                                  :value (mod (- 64 k1) 64))
                   (make-vm-rotate :dst out-dst
                                   :lhs src0
                                   :rhs (vm-dst c0)))
             5))))

(defun opt-rotate-recognition-match-at (instructions pos)
  (let ((end (+ pos 5)))
    (when (<= end (length instructions))
      (destructuring-bind (c0 a0 c1 a1 o0)
          (subseq instructions pos end)
        (when (%opt-rotate-window-shape-valid-p c0 a0 c1 a1 o0)
          (let* ((k0 (vm-value c0))
                 (k1 (vm-value c1))
                 (src0 (vm-lhs a0))
                 (src1 (vm-lhs a1))
                 (count0 (vm-rhs a0))
                 (count1 (vm-rhs a1))
                 (dst0 (vm-dst a0))
                 (dst1 (vm-dst a1))
                 (out-dst (vm-dst o0)))
            (labels ((counts-and-source-valid-p ()
                       ;; Both shifts read the same source register, and the
                       ;; shift amounts trace back to their own constants.
                       (and (integerp k0)
                            (integerp k1)
                            (< 0 k0 64)
                            (< -64 k1 0)
                            (eq src0 src1)
                            (eq count0 (vm-dst c0))
                            (eq count1 (vm-dst c1))))
                     (combine-wiring-consistent-p ()
                       ;; The OR combines the two shifted halves, in either order.
                       (or (and (eq (vm-lhs o0) dst0) (eq (vm-rhs o0) dst1))
                           (and (eq (vm-lhs o0) dst1) (eq (vm-rhs o0) dst0)))))
              (when (and (counts-and-source-valid-p)
                         (combine-wiring-consistent-p))
                (%opt-rotate-build-replacement k0 k1 c0 c1 src0 out-dst)))))))))

(defun opt-pass-rotate-recognition (instructions)
  "Collapse rotate idioms into vm-rotate.

   Recognizes the classic two-shift + OR tree that implements a 64-bit rotate
   and replaces it with a single vm-rotate plus the normalized count constant."
  (%opt-recognition-pass instructions
                          #'opt-rotate-recognition-match-at
                          (lambda (rewritten result)
                            (loop for inst in rewritten
                                  do (push inst result)
                                  finally (return result)))))
