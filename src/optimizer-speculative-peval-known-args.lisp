;;;; optimizer-speculative-peval-known-args.lisp — partial evaluation of
;;;; known-callee call sites with constant call arguments

(in-package :cl-cc/optimize)

(defun %opt-specialize-clear-dst-tracks (dst const-track reg-track)
  "Remove DST's tracked constant value and known-callee label, when DST is
non-NIL."
  (when dst
    (remhash dst const-track)
    (remhash dst reg-track)))

(defun %opt-specialize-emit-clone (callee-label params body const-bindings
                                    specialized-label dynamic-params clone-reg
                                    emitted-labels emit)
  "Emit the first call site's specialized clone: run partial evaluation on
CALLEE-LABEL under CONST-BINDINGS and splice the residual clone's
closure/label/body into the output stream via EMIT, ahead of its first call
site. Record CLONE-REG under SPECIALIZED-LABEL in EMITTED-LABELS."
  (let* ((partial
           (opt-partial-evaluate-function
            callee-label params body
            :constant-bindings const-bindings
            :specialized-name specialized-label))
         (residual-body
           (opt-partial-eval-residual-body partial)))
    (funcall emit (make-vm-closure :dst clone-reg
                                   :label specialized-label
                                   :params dynamic-params
                                   :captured nil))
    (funcall emit (make-vm-label :name specialized-label))
    (dolist (body-inst residual-body)
      (funcall emit body-inst))
    (setf (gethash specialized-label emitted-labels) clone-reg)))

(defun %opt-specialize-handle-call (inst reg-track const-track func-defs
                                     plan-cache emitted-labels new-reg emit)
  "Rewrite INST to call a constant-argument specialization of its callee when
the callee is statically known and a specialization plan can be built;
otherwise emit INST unchanged via EMIT. NEW-REG allocates the clone's
result register."
  (let* ((callee-label (gethash (vm-func-reg inst) reg-track))
         (def (and callee-label (gethash callee-label func-defs))))
    (if def
        (let* ((params (getf def :params))
               (body (getf def :body))
               (const-bindings (%opt-constant-bindings-from-call-args
                                params (vm-args inst) const-track))
               (plan (and const-bindings
                          (opt-build-specialization-plan
                           callee-label params const-bindings :cache plan-cache))))
          (if plan
              (let* ((specialized-label (opt-specialization-plan-specialized-name plan))
                     (dynamic-params (opt-specialization-plan-dynamic-args plan))
                     (dynamic-args
                       (%opt-dynamic-call-args params (vm-args inst) dynamic-params))
                     (clone-reg (funcall new-reg)))
                (unless (gethash specialized-label emitted-labels)
                  (%opt-specialize-emit-clone callee-label params body const-bindings
                                              specialized-label dynamic-params clone-reg
                                              emitted-labels emit))
                (let ((resolved-reg (gethash specialized-label emitted-labels)))
                  (funcall emit (%opt-make-call-like inst resolved-reg dynamic-args))))
              (funcall emit inst)))
        (funcall emit inst))
    (%opt-specialize-clear-dst-tracks (opt-inst-dst inst) const-track reg-track)))

(defun opt-pass-specialize-known-args (instructions)
  "Conservatively clone known-callee functions specialized by constant call args."
  (let* ((func-defs (opt-collect-function-defs instructions))
         (base-idx (1+ (opt-max-reg-index instructions)))
         (reg-track (make-hash-table :test #'eq))
         (const-track (make-hash-table :test #'eq))
         (plan-cache (make-hash-table :test #'equal))
         (emitted-labels (make-hash-table :test #'equal))
         (result nil))
    (labels ((new-reg ()
               (prog1 (intern (format nil "R~A" base-idx) :keyword)
                 (incf base-idx)))
             (emit (inst)
               (push inst result)))
      (dolist (inst instructions)
        (typecase inst
          ((or vm-closure vm-func-ref)
           (setf (gethash (vm-dst inst) reg-track) (vm-label-name inst))
           (remhash (vm-dst inst) const-track)
           (emit inst))
          (vm-const
           (setf (gethash (vm-dst inst) const-track) (vm-value inst))
           (remhash (vm-dst inst) reg-track)
           (emit inst))
          (vm-move
           (multiple-value-bind (src-const present-p)
               (gethash (vm-src inst) const-track)
             (if present-p
                 (setf (gethash (vm-dst inst) const-track) src-const)
                 (remhash (vm-dst inst) const-track)))
           (multiple-value-bind (label found-p)
               (gethash (vm-src inst) reg-track)
             (if found-p
                 (setf (gethash (vm-dst inst) reg-track) label)
                 (remhash (vm-dst inst) reg-track)))
           (emit inst))
          ((or vm-call vm-tail-call)
           (%opt-specialize-handle-call inst reg-track const-track func-defs
                                        plan-cache emitted-labels #'new-reg #'emit))
          (vm-apply
           ;; APPLY spreads the final argument list at runtime, so fixed-arity
           ;; parameter/signature reasoning is not sound here yet.
           (%opt-specialize-clear-dst-tracks (opt-inst-dst inst) const-track reg-track)
           (emit inst))
          (t
           (%opt-specialize-clear-dst-tracks (opt-inst-dst inst) const-track reg-track)
           (emit inst))))
      (nreverse result))))

(defun opt-pass-partial-evaluation (instructions)
  "Pipeline entrypoint for partial evaluation over known constant call arguments.

Current strategy specializes known-callee call sites into residual clones with
only dynamic arguments forwarded, then relies on downstream fold/SCCP cleanup."
  (opt-pass-specialize-known-args instructions))
