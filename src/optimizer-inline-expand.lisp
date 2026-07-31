;;;; optimizer-inline-expand.lisp — call expansion and the inline pass
;;;;
;;;; Picks candidate functions bottom-up per optimizer-inline-heuristics.lisp's
;;;; cost model, then replaces each eligible vm-call with the callee's body
;;;; (renamed per optimizer-register-rename.lisp) and an argument/result move.
;;;; opt-pass-inline is the driver-facing entry point; DCE and memoization
;;;; infrastructure live in optimizer-inline-pass.lisp.

(in-package :cl-cc/optimize)

(defun %opt-inline-candidate-labels (bottom-up-labels func-defs recursive-labels
                                       threshold profile-data)
  "Return a table of non-recursive small functions eligible for inlining.
BOTTOM-UP-LABELS is consumed in leaf-to-root order so callers see callee bodies
that have already been reduced for the current LTO call graph."
  (let ((candidates (make-hash-table :test #'equal)))
    (dolist (label bottom-up-labels candidates)
      (let* ((def (gethash label func-defs))
             (effective-threshold
               (and def
                    (%opt-inline-effective-threshold def threshold profile-data))))
        (when (and def
                    (not (gethash label recursive-labels))
                    (opt-inline-eligible-p def effective-threshold))
           (setf (gethash label candidates) t))))))

(defun %opt-inline-function-name-for-label (label name-to-label)
  "Return the registered function name for LABEL when NAME-TO-LABEL has one."
  (cond
    ((hash-table-p name-to-label)
     (loop for name being the hash-keys of name-to-label
           using (hash-value mapped-label)
           when (equal mapped-label label)
             return name))
    ((listp name-to-label)
     (car (rassoc label name-to-label :test #'equal)))))

(defun %opt-inline-expand-call (inst label def base-idx name-to-label const-track
                                 effective-threshold)
  "Return replacement instructions for inlining INST, or NIL if it is unsafe.
The second value is the next fresh register base."
  (let* ((body   (getf def :body))
         (ci     (getf def :closure))
         (params (vm-closure-params ci)))
    (unless (= (length params) (length (vm-args inst)))
      (return-from %opt-inline-expand-call (values nil base-idx)))
    (let* ((inst-count (length (butlast body)))
           (body-cost (opt-inline-body-cost body))
           (reason (if (eq (vm-closure-inline-policy ci) :inline)
                       "forced-inline"
                       (format nil "cost=~D threshold=~D" body-cost effective-threshold)))
           (rename (if *block-compile*
                       (opt-make-inline-renaming body params base-idx)
                       (opt-make-renaming body base-idx)))
           (replacement nil))
      (%opt-report :inline "function=~A instructions=~D reason=~A"
                   (or (%opt-inline-function-name-for-label label name-to-label) label)
                   inst-count
                   reason)
      (loop for param in params
            for arg in (vm-args inst)
            do (let ((dst (or (gethash param rename) param))
                     (const-present nil)
                     (const-value nil))
                 (multiple-value-setq (const-value const-present)
                   (gethash arg const-track))
                 (push (if const-present
                           (make-vm-const :dst dst :value const-value)
                           (make-vm-move :dst dst :src arg))
                       replacement)))
      (dolist (bi (butlast body))
        (push (opt-rename-regs-in-inst bi rename) replacement))
      (let* ((ret-inst (car (last body)))
             (ret-src (or (gethash (vm-reg ret-inst) rename)
                          (vm-reg ret-inst))))
        (push (make-vm-move :dst (vm-dst inst) :src ret-src) replacement))
      (values (nreverse replacement) (+ base-idx (hash-table-count rename))))))

(defun %opt-inline-body-bottom-up (body func-defs candidates name-to-label profile-data
                                   threshold base-idx)
  "Inline candidate calls inside BODY using already processed callee bodies."
  (let ((reg-track (make-hash-table :test #'eq))
        (const-track (make-hash-table :test #'eq))
        (result nil))
    (dolist (inst body)
      (typecase inst
        ((or vm-closure vm-func-ref)
         (let ((label (vm-label-name inst)))
           (if (gethash label func-defs)
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track)))
         (remhash (opt-inst-dst inst) const-track)
         (push inst result))
        (vm-const
         (let* ((val (vm-value inst))
                (label (and (symbolp val) (gethash val name-to-label))))
           (if (and label (gethash label func-defs))
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track)))
         (setf (gethash (vm-dst inst) const-track) (vm-value inst))
         (push inst result))
        (vm-move
         (multiple-value-bind (label present-p)
             (gethash (vm-src inst) reg-track)
           (if present-p
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track)))
         (multiple-value-bind (src-const present-p)
             (gethash (vm-src inst) const-track)
           (if present-p
               (setf (gethash (vm-dst inst) const-track) src-const)
               (remhash (vm-dst inst) const-track)))
         (push inst result))
        (vm-call
         (let* ((label (gethash (vm-func-reg inst) reg-track))
                (def (and label (gethash label func-defs))))
           (if (and def (gethash label candidates))
               (multiple-value-bind (replacement next-base)
                   (%opt-inline-expand-call
                    inst label def base-idx name-to-label const-track
                    (%opt-inline-effective-threshold def threshold profile-data))
                 (if replacement
                     (progn
                       (setf base-idx next-base)
                       (setf result (nconc (reverse replacement) result))
                       (let ((dst (vm-dst inst)))
                         (when dst
                           (remhash dst reg-track)
                           (remhash dst const-track))))
                     (push inst result)))
               (push inst result))))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst
             (remhash dst reg-track)
             (remhash dst const-track)))
         (push inst result))))
    (values (nreverse result) base-idx)))

(defun opt-make-inline-renaming (body-instructions params base-index)
  "Build a register renaming table for inlining BODY-INSTRUCTIONS.
PARAMS and registers defined inside the callee receive fresh registers. Registers
that are only read by the callee are treated as block-compilation captures and
left untouched, preserving references to the enclosing compilation unit."
  (let ((counter base-index)
        (renaming (make-hash-table :test #'eq)))
    (labels ((add (reg)
               (when (and reg (opt-register-keyword-p reg)
                          (not (gethash reg renaming)))
                 (setf (gethash reg renaming)
                       (intern (format nil "R~A" counter) :keyword))
                 (incf counter))))
      (dolist (param params)
        (add param))
      (dolist (inst body-instructions)
        (add (opt-inst-dst inst))))
    renaming))

(defun opt-pass-inline (instructions &key (threshold 15))
  "Replace vm-call of a known small function with the inlined body.
  The function must be: captured-var-free, linear, and have body cost
  ≤ THRESHOLD (not counting the final vm-ret)."
  (let* ((func-defs (opt-collect-function-defs instructions))
          (name-to-label (opt-build-function-name-map instructions))
          (call-graph (opt-build-call-graph instructions func-defs name-to-label))
          (bottom-up-labels (opt-call-graph-bottom-up-labels call-graph))
          (recursive-labels (opt-call-graph-recursive-labels call-graph))
          (base-idx  (1+ (opt-max-reg-index instructions)))
           (reg-track (make-hash-table :test #'eq))
           (box-track (make-hash-table :test #'eq))
            (const-track (make-hash-table :test #'eq))
          (profile-data (opt-inline-profile-data instructions))
          (candidates (%opt-inline-candidate-labels bottom-up-labels func-defs recursive-labels
                                                    threshold profile-data))
           (result nil))
    (dolist (label bottom-up-labels)
      (when (gethash label candidates)
        (multiple-value-bind (new-body next-base)
            (%opt-inline-body-bottom-up (getf (gethash label func-defs) :body)
                                        func-defs candidates name-to-label profile-data
                                        threshold base-idx)
          (setf (getf (gethash label func-defs) :body) new-body
                base-idx next-base))))
    (dolist (inst instructions)
      (typecase inst
        ((or vm-closure vm-func-ref)
         (let ((label (vm-label-name inst)))
           (when (gethash label func-defs)
             (setf (gethash (vm-dst inst) reg-track) label)))
         (let ((dst (opt-inst-dst inst)))
           (when dst (remhash dst const-track)))
         (push inst result))
         (vm-const
          (let* ((val (vm-value inst))
                 (label (when (symbolp val) (gethash val name-to-label))))
            (if (and label (gethash label func-defs))
                (setf (gethash (vm-dst inst) reg-track) label)
                (remhash (vm-dst inst) reg-track)))
           (setf (gethash (vm-dst inst) const-track) (vm-value inst))
          (push inst result))
         (vm-move
          (multiple-value-bind (label label-present-p)
              (gethash (vm-src inst) reg-track)
            (if label-present-p
                (setf (gethash (vm-dst inst) reg-track) label)
                (remhash (vm-dst inst) reg-track)))
          (multiple-value-bind (boxed-label box-present-p)
              (gethash (vm-src inst) box-track)
            (if box-present-p
                (setf (gethash (vm-dst inst) box-track) boxed-label)
                (remhash (vm-dst inst) box-track)))
          (multiple-value-bind (src-const present-p)
              (gethash (vm-src inst) const-track)
            (if present-p
                (setf (gethash (vm-dst inst) const-track) src-const)
                (remhash (vm-dst inst) const-track)))
          (push inst result))
         (vm-rplaca
          (let ((label (gethash (vm-val-reg inst) reg-track)))
            (when label
              (setf (gethash (vm-cons-reg inst) box-track) label)))
          (push inst result))
         (vm-car
          (let ((label (gethash (vm-src inst) box-track)))
            (if label
                (setf (gethash (vm-dst inst) reg-track) label)
                (remhash (vm-dst inst) reg-track)))
          (remhash (vm-dst inst) const-track)
          (push inst result))
         (vm-call
          (let* ((label (gethash (vm-func-reg inst) reg-track))
                 (def   (and label (gethash label func-defs)))
                 (effective-threshold
                   (and def
                        (%opt-inline-effective-threshold def threshold profile-data))))
            (if (and def
                     (gethash label candidates))
                 (multiple-value-bind (replacement next-base)
                     (%opt-inline-expand-call inst label def base-idx name-to-label
                                              const-track effective-threshold)
                   (if replacement
                       (progn
                         (setf base-idx next-base)
                         (setf result (nconc (reverse replacement) result))
                         (let ((dst (vm-dst inst)))
                           (when dst
                             (remhash dst reg-track)
                             (remhash dst box-track)
                             (remhash dst const-track))))
                       (push inst result)))
                (push inst result))))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst
              (remhash dst reg-track)
              (remhash dst box-track)
              (remhash dst const-track)))
         (push inst result))))
    (nreverse result)))
