;;;; optimizer-speculative-peval-known-args.lisp — partial evaluation of
;;;; known-callee call sites with constant call arguments

(in-package :cl-cc/optimize)

(defun opt-pass-specialize-known-args (instructions)
  "Conservatively clone known-callee functions specialized by constant call args."
  (let* ((func-defs (opt-collect-function-defs instructions))
         (base-idx (1+ (opt-max-reg-index instructions)))
         (reg-track (make-hash-table :test #'eq))
         (const-track (make-hash-table :test #'eq))
         (plan-cache (make-hash-table :test #'equal))
         (emitted-labels (make-hash-table :test #'equal))
         (result nil))
    (labels ((clear-dst-tracks (dst)
               (when dst
                 (remhash dst const-track)
                 (remhash dst reg-track))))
      (dolist (inst instructions)
        (typecase inst
          ((or vm-closure vm-func-ref)
           (setf (gethash (vm-dst inst) reg-track) (vm-label-name inst))
           (remhash (vm-dst inst) const-track)
           (push inst result))
          (vm-const
           (setf (gethash (vm-dst inst) const-track) (vm-value inst))
           (remhash (vm-dst inst) reg-track)
           (push inst result))
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
           (push inst result))
          ((or vm-call vm-tail-call)
            (let* ((callee-label (gethash (vm-func-reg inst) reg-track))
                   (def (and callee-label (gethash callee-label func-defs))))
             (if (null def)
                 (push inst result)
                 (let* ((params (getf def :params))
                        (body (getf def :body))
                        (const-bindings (%opt-constant-bindings-from-call-args
                                         params (vm-args inst) const-track))
                        (plan (and const-bindings
                                   (opt-build-specialization-plan
                                    callee-label params const-bindings :cache plan-cache))))
                   (if (null plan)
                       (push inst result)
                       (let* ((specialized-label (opt-specialization-plan-specialized-name plan))
                              (dynamic-params (opt-specialization-plan-dynamic-args plan))
                              (dynamic-args
                                (%opt-dynamic-call-args params (vm-args inst) dynamic-params))
                              (clone-reg (intern (format nil "R~A" base-idx) :keyword)))
                         (incf base-idx)
                         (unless (gethash specialized-label emitted-labels)
                            (let* ((partial
                                     (opt-partial-evaluate-function
                                      callee-label params body
                                      :constant-bindings const-bindings
                                      :specialized-name specialized-label))
                                   (residual-body
                                     (opt-partial-eval-residual-body partial)))
                              (push (make-vm-closure :dst clone-reg
                                                     :label specialized-label
                                                     :params dynamic-params
                                                    :captured nil)
                                   result)
                             (push (make-vm-label :name specialized-label) result)
                             (dolist (body-inst residual-body)
                               (push body-inst result))
                             (setf (gethash specialized-label emitted-labels) clone-reg)))
                         (let ((resolved-reg (gethash specialized-label emitted-labels)))
                           (push (%opt-make-call-like inst resolved-reg dynamic-args)
                                 result)))))))
              (clear-dst-tracks (opt-inst-dst inst)))
          (vm-apply
           ;; APPLY spreads the final argument list at runtime, so fixed-arity
           ;; parameter/signature reasoning is not sound here yet.
           (clear-dst-tracks (opt-inst-dst inst))
           (push inst result))
          (t
           (clear-dst-tracks (opt-inst-dst inst))
           (push inst result)))))
    (nreverse result)))

(defun opt-pass-partial-evaluation (instructions)
  "Pipeline entrypoint for partial evaluation over known constant call arguments.

Current strategy specializes known-callee call sites into residual clones with
only dynamic arguments forwarded, then relies on downstream fold/SCCP cleanup."
  (opt-pass-specialize-known-args instructions))
