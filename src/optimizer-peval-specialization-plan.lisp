;;;; optimizer-peval-specialization-plan.lisp — specialization call planning
;;;;
;;;; Given a call site, decides which arguments are static per
;;;; optimizer-peval-binding-time.lisp and builds the plan
;;;; optimizer-peval-specialize.lisp executes: the specialized name, the
;;;; static/dynamic argument split, and whether a clone is even needed
;;;; (a cache hit reuses an existing specialization).

(in-package :cl-cc/optimize)

(defun opt-build-specialization-plan (callee-label arguments constant-bindings
                                      &key cache)
  "Build a conservative clone/call-redirection plan for known constant arguments.

Returns NIL when ARGUMENTS have no known constants. When CACHE is supplied, the
same `(callee . signature)` pair reuses the earlier specialized name and marks
the plan as a cache hit instead of requesting a new clone."
  (let ((signature (%opt-constant-binding-signature arguments constant-bindings)))
    (when signature
      (let* ((cache-key (list callee-label signature))
             (cached-name nil)
             (cache-hit-p nil))
        (when cache
          (multiple-value-setq (cached-name cache-hit-p)
            (gethash cache-key cache)))
        (let ((specialized-name (or cached-name
                                    (%opt-specialized-name callee-label signature))))
          (when (and cache (not cache-hit-p))
            (setf (gethash cache-key cache) specialized-name))
          (make-opt-specialization-plan
           :callee-label callee-label
           :specialized-name specialized-name
           :signature signature
           :static-args signature
           :dynamic-args (%opt-dynamic-parameters arguments signature)
           :clone-needed-p (not cache-hit-p)
           :cache-hit-p cache-hit-p))))))

(defun %opt-constant-bindings-from-call-args (params args const-track)
  (let ((result nil))
    (loop for param in params
          for arg in args
          do (multiple-value-bind (value present-p)
                 (gethash arg const-track)
               (when present-p
                 (push (cons param value) result))))
    (nreverse result)))

(defun %opt-dynamic-call-args (params args dynamic-params)
  (let ((result nil))
    (loop for param in params
          for arg in args
          do (when (member param dynamic-params :test #'equal)
               (push arg result)))
    (nreverse result)))

(defun %opt-make-call-like (inst func-reg args)
  (cond
    ((typep inst 'vm-tail-call)
     (make-vm-tail-call :dst (vm-dst inst) :func func-reg :args args))
    ((typep inst 'vm-apply)
     (make-vm-apply :dst (vm-dst inst)
                    :func func-reg
                    :args args
                    :tail-p (cl-cc/vm:vm-tail-p inst)))
    (t
     (make-vm-call :dst (vm-dst inst) :func func-reg :args args))))
