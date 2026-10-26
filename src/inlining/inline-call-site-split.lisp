;;;; optimizer-inline-call-site-split.lisp — call-site splitting pass
;;;;
;;;; Duplicates a call site's successor block per known-callee, so a
;;;; downstream pass sees a monomorphic call instead of one call reachable
;;;; from several different known functions. Register renaming for the
;;;; duplicated blocks comes from optimizer-register-rename.lisp.

(in-package #:cl-cc/optimize/inlining)

(defun opt-body-has-global-refs-p (body-instructions params)
  "Return T if BODY-INSTRUCTIONS read any register that is neither in PARAMS
   nor defined as a DST by a prior body instruction.  Such 'global registers'
   (e.g., class descriptors set by defclass at the top level) would be renamed
   to fresh uninitialized registers if the function were inlined, breaking
   correctness.  Functions with global refs must not be inlined."
  (let ((safe (make-hash-table :test #'eq)))
    (dolist (p params) (setf (gethash p safe) t))
    (dolist (inst body-instructions nil)
      (dolist (r (opt-inst-read-regs inst))
        (unless (gethash r safe)
          (return-from opt-body-has-global-refs-p t)))
      (when-let ((dst (opt-inst-dst inst)))
        (setf (gethash dst safe) t)))))

(defun opt-known-callee-labels (instructions)
  "Return reg -> known callee label mapping tracked through simple designators." 
  (let ((name-to-label (opt-build-function-name-map instructions))
        (reg-track (make-hash-table :test #'eq)))
    (dolist (inst instructions reg-track)
      (%opt-track-known-callee-label inst name-to-label reg-track))))

(defun %opt-call-site-split-fresh-label (used-labels)
  "Return a fresh after-call label not present in USED-LABELS."
  (loop for i from 0
        for label = (format nil "CALL-SITE-SPLIT-AFTER-~D" i)
        unless (gethash label used-labels)
          do (setf (gethash label used-labels) t)
             (return label)))

(defun %opt-known-callee-before-index (instructions end-index func-reg name-to-label)
  "Return FUNC-REG's known label immediately before END-INDEX, within one block."
  (loop for i downfrom (1- end-index) downto 0
        for inst = (nth i instructions)
        do (cond
             ((or (typep inst 'vm-label)
                  (typep inst 'vm-jump)
                  (typep inst 'vm-jump-zero)
                  (typep inst 'vm-ret)
                  (typep inst 'vm-halt))
              (return))
             ((eq (opt-inst-dst inst) func-reg)
              (return
                (typecase inst
                  ((or vm-closure vm-func-ref)
                   (vm-label-name inst))
                  (vm-const
                   (and (symbolp (vm-value inst))
                        (gethash (vm-value inst) name-to-label)))
                  (t nil)))))))

(defun %opt-copy-vm-call (call)
  "Return a fresh copy of CALL for predecessor-local call-site splitting."
  (make-vm-call :dst (vm-dst call)
                :func (vm-func-reg call)
                :args (copy-list (vm-args call))))

(define-inst-type-predicate %opt-call-like-p (or vm-call vm-tail-call vm-apply) "Return T when INST is a call shape supported by call-site splitting.")

(defun %opt-copy-call-like (call)
  "Return a fresh copy of CALL for predecessor-local call-site splitting."
  (typecase call
    (vm-call (%opt-copy-vm-call call))
    (vm-tail-call
     (make-vm-tail-call :dst (vm-dst call) :func (vm-func-reg call)
                         :args (copy-list (vm-args call))))
    (vm-apply (make-vm-apply :dst (vm-dst call)
                             :func (vm-func-reg call)
                             :args (copy-list (vm-args call))
                             :tail-p (cl-cc/vm:vm-tail-p call)))))

(defun %opt-call-site-split-join-labels (instructions call-index)
  "Return all consecutive labels immediately preceding CALL-INDEX."
  (loop for i downfrom (1- call-index) downto 0
        for inst = (nth i instructions)
        while (typep inst 'vm-label)
        collect (vm-name inst)))

(defun %opt-callable-type-proof-before-index-p (instructions end-index func-reg)
  "Return T when the predecessor locally proves FUNC-REG is function-callable."
  (loop for i downfrom (1- end-index) downto 0
        for inst = (nth i instructions)
        do (cond
             ((or (typep inst 'vm-label) (typep inst 'vm-jump)
                  (typep inst 'vm-ret) (typep inst 'vm-halt))
              (return))
             ((and (typep inst 'vm-typep)
                   (eq (vm-src inst) func-reg)
                   (member (vm-type-name inst) '(function compiled-function) :test #'eq))
              (return t))
             ((and (typep inst 'vm-function-p) (eq (vm-src inst) func-reg))
              (return t)))))

(defun %opt-call-site-split-replacement (call-inst func-reg callee-label after-label)
  "Build replacement instructions for a predecessor split of CALL-INST."
  (append (when callee-label (list (make-vm-func-ref :dst func-reg :label callee-label)))
          (list (%opt-copy-call-like call-inst))
          (list (make-vm-jump :label after-label))))

(defun opt-pass-call-site-splitting (instructions)
  "Duplicate join-block call sites into predecessors with known callees.
Handles consecutive multi-join labels and `vm-call`/`vm-tail-call`/`vm-apply`.
The original join call remains available for fall-through and unknown preds."
  (let* ((len (length instructions))
         (name-to-label (opt-build-function-name-map instructions))
         (used-labels (make-hash-table :test #'equal))
         (replacements (make-hash-table))
         (after-labels (make-hash-table)))
    (dolist (inst instructions)
      (when (typep inst 'vm-label)
        (setf (gethash (vm-name inst) used-labels) t)))
    (loop for call-index from 1 below len
          for call-inst = (nth call-index instructions)
          for join-labels = (%opt-call-site-split-join-labels instructions call-index)
          when (and join-labels (%opt-call-like-p call-inst))
          do (let ((func-reg (vm-func-reg call-inst))
                   (split-jumps nil))
               (dolist (join-label join-labels)
                 (loop for jump-index from 0 below call-index
                       for jump-inst = (nth jump-index instructions)
                       when (and (typep jump-inst 'vm-jump)
                                 (equal (vm-label-name jump-inst) join-label))
                       do (let ((callee-label
                                 (%opt-known-callee-before-index
                                  instructions jump-index func-reg name-to-label)))
                            (when (or callee-label
                                      (%opt-callable-type-proof-before-index-p
                                       instructions jump-index func-reg))
                              (push (list jump-index callee-label) split-jumps)))))
               (when split-jumps
                 (let ((after-label (%opt-call-site-split-fresh-label used-labels)))
                   (setf (gethash call-index after-labels) after-label)
                   (dolist (split split-jumps)
                     (destructuring-bind (jump-index callee-label) split
                       (setf (gethash jump-index replacements)
                             (%opt-call-site-split-replacement
                              call-inst func-reg callee-label after-label))))))))
    (loop for index from 0 below len
          for inst = (nth index instructions)
          append (or (gethash index replacements)
                     (if-let ((after-label (gethash index after-labels)))
                       (list inst (make-vm-label :name after-label))
                       (list inst))))))
