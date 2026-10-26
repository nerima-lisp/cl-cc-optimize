;;;; optimizer-peval-binding-time.lisp — binding-time analysis
;;;;
;;;; Classifies each parameter as static (known at specialization time) or
;;;; dynamic, two ways: an SCCP-style lattice fixed-point over call-site
;;;; arguments, and a conservative offline syntactic pass over one function
;;;; body. Feeds optimizer-peval-specialization-plan.lisp's decision of
;;;; which parameters to specialize.

(in-package :cl-cc/optimize)

(defun %opt-lattice-binding-time-kind (lattice)
  (if (and (opt-lattice-value-p lattice)
           (eq (opt-lattice-value-kind lattice) :constant))
      :static
      :dynamic))

(defun opt-sccp-analyze-binding-times (parameters lattice-bindings)
  "Classify PARAMETERS as :STATIC or :DYNAMIC using SCCP lattice bindings."
  (loop for parameter in parameters
        for index from 0
        collect (multiple-value-bind (lattice present-p)
                    (%opt-parameter-constant parameter index lattice-bindings)
                  (let ((kind (if present-p
                                  (%opt-lattice-binding-time-kind lattice)
                                  :dynamic)))
                    (make-opt-binding-time
                     :parameter parameter
                     :kind kind
                     :value (and (eq kind :static)
                                 (opt-lattice-value-value lattice))
                     :lattice (and present-p lattice))))))

(defun opt-run-binding-time-analysis (parameters
                                      &key
                                        (constant-bindings nil)
                                        (lattice-bindings nil))
  "Run a conservative binding-time analysis for PARAMETERS.

Priority:
1) CONSTANT-BINDINGS are treated as compile-time static facts.
2) Remaining parameters are classified from LATTICE-BINDINGS via SCCP.

This provides an explicit BTA entrypoint (FR-210) that can be used by
partial-evaluation passes without requiring callers to manually merge sources."
  (let* ((seed (loop for (name . value) in constant-bindings
                     collect (cons name (opt-lattice-constant value))))
         (merged-lattice (append seed lattice-bindings)))
    (opt-sccp-analyze-binding-times parameters merged-lattice)))

(defun %opt-offline-bta-constant-atom-p (node static-set)
  (cond
    ((symbolp node)
     (or (member node '(nil t) :test #'eq)
         (keywordp node)
         (member node static-set :test #'equal)))
    (t (constantp node))))

(defun %opt-offline-bta-all-static-p (forms static-set)
  "Return T when every form in FORMS classifies as :STATIC under STATIC-SET."
  (every (lambda (f)
           (eq (%opt-offline-bta-classify-form f static-set) :static))
         forms))

(defun %opt-offline-bta-binding-symbol (binding)
  "Return the variable name bound by a let/let* BINDING clause, or NIL."
  (cond
    ((symbolp binding) binding)
    ((and (consp binding) (symbolp (car binding))) (car binding))
    (t nil)))

(defun %opt-offline-bta-let-static-set (bindings static-set)
  "Return STATIC-SET extended with each variable from let/let* BINDINGS whose
initform (if any) also classifies as :static under STATIC-SET."
  (let ((new-static static-set))
    (dolist (binding bindings new-static)
      (let* ((var (%opt-offline-bta-binding-symbol binding))
             (rhs (if (and (consp binding) (cdr binding)) (second binding)))
             (rhs-static-p (or (null rhs)
                               (eq (%opt-offline-bta-classify-form rhs static-set)
                                   :static))))
        (when (and var rhs-static-p)
          (push var new-static))))))

(defun %opt-offline-bta-classify-form (form static-set)
  (cond
    ((atom form)
     (if (%opt-offline-bta-constant-atom-p form static-set) :static :dynamic))
    ((member (car form) '(quote function) :test #'eq)
     :static)
    ((eq (car form) 'if)
     (if (%opt-offline-bta-all-static-p (cdr form) static-set) :static :dynamic))
    ((eq (car form) 'progn)
     (if (%opt-offline-bta-all-static-p (cdr form) static-set) :static :dynamic))
    ((member (car form) '(let let*) :test #'eq)
     (if (%opt-offline-bta-all-static-p
          (cddr form)
          (%opt-offline-bta-let-static-set (second form) static-set))
         :static
         :dynamic))
    ((eq (car form) 'setq)
     (if (%opt-offline-bta-all-static-p
          (loop for (_ v) on (cdr form) by #'cddr
                collect v)
          static-set)
         :static
         :dynamic))
    ((and (symbolp (car form))
          (member (car form) *opt-offline-bta-pure-operators* :test #'eq))
     (if (%opt-offline-bta-all-static-p (cdr form) static-set) :static :dynamic))
    (t :dynamic)))

(defun opt-offline-bta-classify-form (form
                                      &key
                                        (static-bindings nil)
                                        (binding-times nil))
  "Classify FORM as :STATIC or :DYNAMIC using an offline BTA approximation.

STATIC-BINDINGS are explicit compile-time facts `(var . value)`.
BINDING-TIMES may include `opt-binding-time` entries (e.g. from SCCP merge).
Only bindings classified as :static are treated as compile-time-known names."
  (let ((static-set
          (append
           (mapcar #'car static-bindings)
           (loop for bt in binding-times
                 when (and (opt-binding-time-p bt)
                           (eq (opt-binding-time-kind bt) :static))
                 collect (opt-binding-time-parameter bt)))))
    (%opt-offline-bta-classify-form form static-set)))

(defun opt-offline-bta-analyze-body (body
                                     &key
                                       (static-bindings nil)
                                       (binding-times nil))
  "Classify each form in BODY as :STATIC or :DYNAMIC via offline BTA."
  (mapcar (lambda (form)
            (opt-offline-bta-classify-form
             form
             :static-bindings static-bindings
             :binding-times binding-times))
          body))
