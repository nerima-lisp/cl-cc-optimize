(in-package #:cl-cc/optimize/analysis)

(defun %opt-collect-closure-params (instructions)
  "Return label -> params hash-table, collected from callable reference
instructions (vm-closure/vm-func-ref) that carry a known parameter list."
  (let ((label-to-params (make-hash-table :test #'equal)))
    (dolist (inst instructions label-to-params)
      (when (and (typep inst '(or vm-closure vm-func-ref))
                 (vm-closure-params inst))
        (setf (gethash (vm-label-name inst) label-to-params)
              (vm-closure-params inst))))))

(defun %opt-collect-linear-bodies (instructions)
  "Return label -> instructions hash-table for linear function bodies: a
label followed by straight-line code with no internal jump, ending in a
single vm-ret. A nested label or an internal jump abandons the in-progress
body, so only genuinely linear bodies are recorded."
  (let ((label-to-body (make-hash-table :test #'equal))
        (in-fn nil) (cur-label nil) (cur-body nil) (has-jump nil))
    (dolist (inst instructions label-to-body)
      (typecase inst
        (vm-label
         ;; Nested label: abandon any in-progress body (non-linear)
         (when in-fn
           (setf in-fn nil cur-label nil cur-body nil has-jump nil))
         (setf in-fn t cur-label (vm-name inst) cur-body nil has-jump nil))
        ((or vm-jump vm-jump-zero)
         (setf has-jump t)
         (when in-fn (push inst cur-body)))
        (vm-ret
         (when (and in-fn (not has-jump))
           (push inst cur-body)
           (setf (gethash cur-label label-to-body) (nreverse cur-body)))
         (setf in-fn nil cur-label nil cur-body nil has-jump nil))
        (t (when in-fn (push inst cur-body)))))))

(defun %opt-collect-label-to-closure (instructions)
  "Return label -> preferred callable-reference instruction (vm-closure or
vm-func-ref) for every label referenced in INSTRUCTIONS."
  (let ((label-to-closure (make-hash-table :test #'equal)))
    (dolist (inst instructions label-to-closure)
      (when (typep inst '(or vm-closure vm-func-ref))
        (let* ((label (vm-label-name inst))
               (existing (gethash label label-to-closure)))
          ;; Keep the reference that carries the parameter list. It is also the
          ;; one carrying the capture list and lambda-list metadata that
          ;; OPT-INLINE-ELIGIBLE-P reads off :CLOSURE. Taking the last
          ;; reference instead let a bare VM-FUNC-REF naming the same label
          ;; overwrite the VM-CLOSURE, so the function looked parameterless:
          ;; its own parameters then read as global registers and
          ;; OPT-BODY-HAS-GLOBAL-REFS-P rejected it, silently disabling
          ;; inlining for every function referenced that way.
          (when (or (null existing)
                    (and (vm-closure-params inst)
                         (null (vm-closure-params existing))))
            (setf (gethash label label-to-closure) inst)))))))

(defun opt-collect-function-defs (instructions)
  "Return hash-table label → (:params params :body body-insts).
    Only includes functions that are:
    - Registered via vm-closure/vm-func-ref with known params
    - Linear: no internal jumps; body ends with exactly one vm-ret"
  (let ((label-to-params  (%opt-collect-closure-params instructions))
        (label-to-body    (%opt-collect-linear-bodies instructions))
        (label-to-closure (%opt-collect-label-to-closure instructions))
        (result           (make-hash-table :test #'equal)))
    (maphash (lambda (lbl params)
               (let ((body    (gethash lbl label-to-body))
                     (closure (gethash lbl label-to-closure)))
                 (when (and body closure)
                   (setf (gethash lbl result)
                         (list :closure closure :params params :body body)))))
             label-to-params)
    result))

(defun %opt-find-closure-label-for-reg (instructions reg)
  "Return the label of the vm-closure in INSTRUCTIONS whose destination
register is REG. Used as a fallback whole-list search when a forward
reference means REG-TRACK hasn't recorded REG's closure label yet."
  (dolist (i instructions)
    (when (and (vm-closure-p i)
               (eq (vm-dst i) reg))
      (return (vm-label-name i)))))

(defun opt-build-function-name-map (instructions)
  "Return symbol → function-label mapping for top-level function registrations."
  (let ((reg-track (make-hash-table :test #'eq))
        (name-to-label (make-hash-table :test #'eq)))
    (dolist (inst instructions)
      (typecase inst
        ((or vm-closure vm-func-ref)
         (when-let ((label (vm-label-name inst)))
           (setf (gethash (vm-dst inst) reg-track) label)))
        (vm-register-function
         (when-let ((label (or (gethash (vm-src inst) reg-track)
                                (%opt-find-closure-label-for-reg instructions (vm-src inst)))))
           (setf (gethash (vm-func-name inst) name-to-label) label)))))
    name-to-label))
