;;;; packages/optimize/src/optimizer-idiom.lisp — FR-684 loop idiom recognition

(in-package :cl-cc/optimize)

(defun %opt-zero-constant-reg-p (reg instructions)
  "Return T when REG is defined by a visible constant zero in INSTRUCTIONS."
  (some (lambda (inst)
          (and (typep inst 'vm-const)
               (eq (vm-dst inst) reg)
               (eql (vm-value inst) 0)))
        instructions))

(defun %opt-idiom-fill-zero-match-at (instructions pos)
  "Recognize DOTIMES zero fill: (dotimes (i n) (setf (aref arr i) 0))."
  (multiple-value-bind (rewritten consumed)
      (opt-fill-recognition-match-at instructions pos)
    (when (and rewritten (plusp (or consumed 0)))
      (let ((window (subseq instructions pos (min (length instructions) (+ pos consumed)))))
        (when (some (lambda (inst)
                      (and (typep inst 'vm-aset)
                           (%opt-zero-constant-reg-p (vm-val-reg inst) instructions)))
                    window)
          (values rewritten consumed))))))

(defun %opt-idiom-copy-match-at (instructions pos alias-roots)
  "Recognize DOTIMES unit-stride memcpy loops and lower them to vm-copy-vector."
  (multiple-value-bind (rewritten consumed)
      (opt-copy-recognition-match-at instructions pos alias-roots)
    (declare (ignore rewritten))
    (let ((end (+ pos 11)))
      (when (and consumed (= consumed 11) (<= end (length instructions)))
        (let* ((cmp   (nth (+ pos 2) instructions))
               (load  (nth (+ pos 4) instructions))
               (store (nth (+ pos 5) instructions))
               (one   (nth (+ pos 6) instructions))
               (inc   (nth (+ pos 7) instructions))
               (step  (nth (+ pos 8) instructions)))
          (when (and (typep cmp 'vm-lt)
                     (typep load 'vm-aref)
                     (typep store 'vm-aset)
                     (typep one 'vm-const)
                     (eql (vm-value one) 1)
                     ;; Unit stride only.  Strided or scaled address forms do not
                     ;; match this canonical add-by-one/update sequence.
                     (or (and (eq (vm-lhs inc) (vm-index-reg load))
                              (eq (vm-rhs inc) (vm-dst one)))
                         (and (eq (vm-rhs inc) (vm-index-reg load))
                              (eq (vm-lhs inc) (vm-dst one))))
                     (eq (vm-dst step) (vm-index-reg load))
                     (eq (vm-src step) (vm-dst inc)))
            (values (list (make-vm-copy-vector :dst-array-reg (vm-array-reg store)
                                               :src-array-reg (vm-array-reg load)
                                               :len-reg (vm-rhs cmp))
                          ;; Preserve the loop's observable exit index.
                          (make-vm-move :dst (vm-index-reg load) :src (vm-rhs cmp)))
                    consumed)))))))

(defun %opt-strlen-window-shape-valid-p (init header char nul cmp body-jump exit-jump body-label one inc step back-jump exit-label)
  "T when the fixed-role instructions in a candidate strlen window have the
   expected vm instruction types and immediate values for the canonical
   zero-based nul-terminated character scan idiom."
  (and (typep init 'vm-const) (eql (vm-value init) 0)
       (typep header 'vm-label)
       (typep char 'vm-char)
       (typep nul 'vm-const) (characterp (vm-value nul))
       (char= (vm-value nul) #\Nul)
       (typep cmp 'vm-char=)
       (typep body-jump 'vm-jump-zero)
       (typep exit-jump 'vm-jump)
       (typep body-label 'vm-label)
       (typep one 'vm-const) (eql (vm-value one) 1)
       (typep inc 'vm-add)
       (typep step 'vm-move)
       (typep back-jump 'vm-jump)
       (typep exit-label 'vm-label)))

(defun %opt-strlen-wiring-consistent-p (idx-reg char nul cmp body-jump body-label
                                         exit-jump exit-label inc one step back-jump header)
  "T when every instruction in the strlen window references the same index
register and jumps to the same labels."
  (and (eq (vm-index char) idx-reg)
       (or (and (eq (vm-char1 cmp) (vm-dst char))
                (eq (vm-char2 cmp) (vm-dst nul)))
           (and (eq (vm-char2 cmp) (vm-dst char))
                (eq (vm-char1 cmp) (vm-dst nul))))
       (eq (vm-reg body-jump) (vm-dst cmp))
       (equal (vm-label-name body-jump) (vm-name body-label))
       (equal (vm-label-name exit-jump) (vm-name exit-label))
       (or (and (eq (vm-lhs inc) idx-reg) (eq (vm-rhs inc) (vm-dst one)))
           (and (eq (vm-rhs inc) idx-reg) (eq (vm-lhs inc) (vm-dst one))))
       (eq (vm-dst step) idx-reg)
       (eq (vm-src step) (vm-dst inc))
       (equal (vm-label-name back-jump) (vm-name header))))

(defun %opt-idiom-strlen-match-at (instructions pos)
  "Recognize a zero-based loop over characters until #\\Nul and use vm-string-length."
  (let ((end (+ pos 13)))
    (when (<= end (length instructions))
      (destructuring-bind (init header char nul cmp body-jump exit-jump body-label one inc step back-jump exit-label)
          (subseq instructions pos end)
        (when (%opt-strlen-window-shape-valid-p init header char nul cmp body-jump exit-jump body-label one inc step back-jump exit-label)
          (let ((idx-reg (vm-dst init)))
            (when (%opt-strlen-wiring-consistent-p idx-reg char nul cmp body-jump body-label
                                                    exit-jump exit-label inc one step back-jump header)
              (values (list (make-vm-string-length :dst idx-reg :src (vm-string-reg char)))
                      13))))))))

(defun %opt-popcount-window-shape-valid-p (init-count header exit-test one dec clear-low inc-count step-src back-jump exit-label)
  "T when the fixed-role instructions in a candidate popcount window have the
   expected vm instruction types and immediate values for the Kernighan
   bit-counting idiom."
  (and (typep init-count 'vm-const) (eql (vm-value init-count) 0)
       (typep header 'vm-label)
       (typep exit-test 'vm-jump-zero)
       (typep one 'vm-const) (eql (vm-value one) 1)
       (typep dec 'vm-sub)
       (typep clear-low 'vm-logand)
       (typep inc-count 'vm-add)
       (typep step-src 'vm-move)
       (typep back-jump 'vm-jump)
       (typep exit-label 'vm-label)))

(defun %opt-popcount-wiring-consistent-p (src-reg count-reg one-reg dec clear-low
                                           inc-count step-src back-jump header exit-test exit-label)
  "T when every instruction in the popcount window references the same
count/source registers and jumps to the same labels."
  (and (eq (vm-lhs dec) src-reg)
       (eq (vm-rhs dec) one-reg)
       (eq (vm-lhs clear-low) src-reg)
       (eq (vm-rhs clear-low) (vm-dst dec))
       (or (and (eq (vm-lhs inc-count) count-reg)
                (eq (vm-rhs inc-count) one-reg))
           (and (eq (vm-rhs inc-count) count-reg)
                (eq (vm-lhs inc-count) one-reg)))
       (eq (vm-dst step-src) src-reg)
       (eq (vm-src step-src) (vm-dst clear-low))
       (equal (vm-label-name back-jump) (vm-name header))
       (equal (vm-label-name exit-test) (vm-name exit-label))))

(defun %opt-idiom-popcount-match-at (instructions pos)
  "Recognize Kernighan bit-counting loop and emit vm-logcount."
  (let ((end (+ pos 10)))
    (when (<= end (length instructions))
      (destructuring-bind (init-count header exit-test one dec clear-low inc-count step-src back-jump exit-label)
          (subseq instructions pos end)
        (when (%opt-popcount-window-shape-valid-p init-count header exit-test one dec clear-low inc-count step-src back-jump exit-label)
          (let ((count-reg (vm-dst init-count))
                (src-reg (vm-reg exit-test))
                (one-reg (vm-dst one)))
            (when (%opt-popcount-wiring-consistent-p src-reg count-reg one-reg dec clear-low
                                                      inc-count step-src back-jump header exit-test exit-label)
              (values (list (make-vm-logcount :dst count-reg :src src-reg)
                            (make-vm-const :dst src-reg :value 0))
                      10))))))))

(defun opt-idiom-recognition-match-at (instructions pos alias-roots)
  "Return an idiom rewrite at POS, or NIL/0 when no FR-684 pattern matches."
  (multiple-value-bind (rewritten consumed)
      (%opt-idiom-fill-zero-match-at instructions pos)
    (if rewritten
        (values rewritten consumed)
        (multiple-value-bind (rewritten consumed)
            (%opt-idiom-copy-match-at instructions pos alias-roots)
          (if rewritten
              (values rewritten consumed)
              (multiple-value-bind (rewritten consumed)
                  (%opt-idiom-strlen-match-at instructions pos)
                (if rewritten
                    (values rewritten consumed)
                    (%opt-idiom-popcount-match-at instructions pos))))))))

(defun opt-pass-idiom-recognition (instructions)
  "FR-684: recognize memset, memcpy, strlen, and popcount loop idioms.

The pass is deliberately conservative: it only rewrites unit-stride, zero-based,
private-loop shapes whose body matches the canonical VM instruction sequence."
  (let ((alias-roots (opt-compute-heap-aliases instructions)))
    (loop with n = (length instructions)
          with result = nil
          with i = 0
          while (< i n)
          do (multiple-value-bind (rewritten consumed)
                 (opt-idiom-recognition-match-at instructions i alias-roots)
               (if (and rewritten (integerp consumed) (plusp consumed))
                   (progn
                     (dolist (inst rewritten) (push inst result))
                     (incf i consumed))
                   (progn
                     (push (nth i instructions) result)
                     (incf i))))
          finally (return (nreverse result)))))
