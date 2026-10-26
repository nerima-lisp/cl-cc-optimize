;;;; optimizer-peval-specialize.lisp — per-function constant-argument
;;;; specialization
;;;;
;;;; Given a call site where some arguments are compile-time constants,
;;;; builds a specialized clone of the callee with those parameters bound
;;;; and their uses substituted, leaving only the dynamic parameters in the
;;;; residual body. optimizer-peval-program.lisp merges this across a whole
;;;; program; optimizer-peval-binding-time.lisp and
;;;; optimizer-peval-specialization-plan.lisp decide where it applies.

(in-package :cl-cc/optimize)

;;; ─── FR-209/210/211 Partial Evaluation Helper Layer ────────────────────────

(defun %opt-normalize-binding-cell-value (cell)
  (let ((tail (cdr cell)))
    (if (and (consp tail) (null (cdr tail)))
        (car tail)
        tail)))

(defun %opt-binding-value (key bindings)
  (cond
   ((hash-table-p bindings)
    (gethash key bindings))
   (bindings
    (if-let ((cell (assoc key bindings :test #'equal)))
      (values (%opt-normalize-binding-cell-value cell) t)
      (values nil nil)))
   (t (values nil nil))))

(defun %opt-parameter-constant (parameter index constant-bindings)
  (multiple-value-bind (value present-p)
      (%opt-binding-value parameter constant-bindings)
    (if present-p
        (values value t)
        (%opt-binding-value index constant-bindings))))

(defun %opt-constant-binding-signature (parameters constant-bindings)
  (loop for parameter in parameters
        for index from 0
        append (multiple-value-bind (value present-p)
                   (%opt-parameter-constant parameter index constant-bindings)
                 (when present-p
                   (list (cons parameter value))))))

(defun %opt-remove-shadowed-bindings (signature shadowed)
  (remove-if (lambda (cell)
               (member (car cell) shadowed :test #'equal))
             signature))

(defun %opt-lambda-binding-symbol (item)
  (cond
    ((and (symbolp item)
          (not (member item '(&optional &rest &key &aux &body &whole
                              &environment &allow-other-keys)
                       :test #'eq)))
     item)
    ((and (consp item) (symbolp (car item)))
     (car item))
    (t nil)))

(defun %opt-let-binding-symbol (binding)
  (cond
    ((symbolp binding) binding)
    ((and (consp binding) (symbolp (car binding))) (car binding))
    (t nil)))

(defun %opt-substitute-let-bindings (bindings signature sequential-p substitute-fn)
  (let ((lookup-signature signature)
        (eval-signature signature)
        (shadowed nil)
        (result nil))
    (dolist (binding bindings)
      (let ((var (%opt-let-binding-symbol binding)))
        (push (if (and (consp binding) (cdr binding))
                  (multiple-value-bind (value-form updated-signature)
                      (funcall substitute-fn (second binding) lookup-signature)
                    (setf lookup-signature updated-signature
                          eval-signature updated-signature)
                    (list var value-form))
                  binding)
              result)
        (when var
          (push var shadowed)
          (when sequential-p
            (setf lookup-signature
                  (%opt-remove-shadowed-bindings lookup-signature (list var)))))))
    (values (nreverse result)
            (if sequential-p
                lookup-signature
                (%opt-remove-shadowed-bindings lookup-signature shadowed))
            eval-signature
            shadowed)))

(defun %opt-merge-body-effects-into-outer-signature (outer-signature body-signature shadowed)
  (let* ((shadowed-set shadowed)
         (result nil))
    (dolist (cell outer-signature)
      (let* ((var (car cell))
             (body-cell (assoc var body-signature :test #'equal)))
        (cond
          ((member var shadowed-set :test #'equal)
           (push cell result))
          (body-cell
           (push (cons var (cdr body-cell)) result))
          (t
           nil))))
    (dolist (cell body-signature)
      (let ((var (car cell)))
        (unless (or (member var shadowed-set :test #'equal) (assoc var result :test #'equal))
          (push cell result))))
    (nreverse result)))

(defun %opt-tree-substitute-constants (form signature)
  (labels ((contains-setq-p (node)
             (cond
               ((atom node) nil)
               ((eq (car node) 'quote) nil)
               ((eq (car node) 'function) nil)
               ((eq (car node) 'setq) t)
               (t (some #'contains-setq-p node))))
           (substitute-body (forms active-signature)
             (let ((current-signature active-signature)
                   (result nil))
                (dolist (body-form forms)
                  (multiple-value-bind (new-form new-signature)
                      (substitute-node body-form current-signature)
                   (push new-form result)
                   (setf current-signature new-signature)))
               (values (nreverse result) current-signature)))
           (substitute-setq (pairs active-signature)
             (let ((current-signature active-signature)
                   (result nil))
                (loop for (place value) on pairs by #'cddr
                      do (push place result)
                         (multiple-value-bind (value-form updated-signature)
                             (substitute-node value current-signature)
                           (push value-form result)
                           (setf current-signature
                                 (unless (contains-setq-p value) updated-signature)))
                         (when (symbolp place)
                           (setf current-signature
                                 (%opt-remove-shadowed-bindings current-signature
                                                                (list place)))))
               (values (cons 'setq (nreverse result)) current-signature)))
           (substitute-lambda-form (node active-signature)
             "Substitute within a LAMBDA form's body, shadowing bindings introduced
by its lambda-list; the lambda-list itself is left untouched and the
enclosing signature is unaffected by anything the body does."
             (let* ((lambda-list (second node))
                    (shadowed (remove nil
                                      (mapcar #'%opt-lambda-binding-symbol
                                              lambda-list)))
                    (body-signature
                      (%opt-remove-shadowed-bindings active-signature shadowed)))
               (multiple-value-bind (new-body _)
                   (substitute-body (cddr node) body-signature)
                 (declare (ignore _))
                 (values (list* 'lambda lambda-list new-body)
                         active-signature))))
           (substitute-let-form (node active-signature)
             "Substitute within a LET/LET* form: rewrite the bindings, substitute
through the body under the resulting body-signature, then merge whatever
the body did back into the signature visible outside the form."
             (multiple-value-bind (bindings body-signature outward-signature shadowed)
                 (%opt-substitute-let-bindings
                  (second node)
                  active-signature
                  (eq (car node) 'let*)
                  #'substitute-node)
               (multiple-value-bind (new-body body-updated-signature)
                   (substitute-body (cddr node) body-signature)
                 (let ((merged-signature
                         (%opt-merge-body-effects-into-outer-signature
                          outward-signature
                          body-updated-signature
                          shadowed)))
                   (values (list* (car node) bindings new-body)
                           merged-signature)))))
           (substitute-node (node active-signature)
             (cond
               ((symbolp node)
                (let ((cell (assoc node active-signature :test #'equal)))
                  (values (if cell (cdr cell) node)
                          active-signature)))
               ((atom node)
                (values node active-signature))
               ((eq (car node) 'quote)
                (values node active-signature))
               ((eq (car node) 'function)
                (values node active-signature))
               ((eq (car node) 'lambda)
                (substitute-lambda-form node active-signature))
               ((eq (car node) 'progn)
                (multiple-value-bind (new-body updated-signature)
                    (substitute-body (cdr node) active-signature)
                  (values (cons 'progn new-body) updated-signature)))
               ((member (car node) '(let let*) :test #'eq)
                (substitute-let-form node active-signature))
               ((eq (car node) 'setq)
                (substitute-setq (cdr node) active-signature))
               ((symbolp (car node))
                (multiple-value-bind (new-args updated-signature)
                    (substitute-body (cdr node) active-signature)
                  (values (cons (car node) new-args)
                          updated-signature)))
               (t
                (multiple-value-bind (new-seq updated-signature)
                    (substitute-body node active-signature)
                  (values new-seq updated-signature))))))
    (substitute-node form signature)))

(defun %opt-dynamic-parameters (parameters signature)
  (remove-if (lambda (parameter)
               (assoc parameter signature :test #'equal))
             parameters))

(defun %opt-specialized-name (function-name signature)
  (format nil "~A__spec__~A"
          (%opt-stable-print-string function-name)
          (%opt-stable-print-string signature)))

(defun %opt-body-vm-instructions-p (body)
  (every (lambda (node) (typep node 'vm-instruction)) body))

(defun %opt-build-vm-residual-body (body signature)
  "Build residual VM body by materializing static parameter values as vm-const.

When BODY is VM instruction objects (as used by FR-211 clone emission),
tree-level substitution is not sufficient. We conservatively emit one vm-const
per static binding at function entry, preserving original instruction order."
  (append
   (loop for (parameter . value) in signature
         collect (make-vm-const :dst parameter :value value))
   body))

(defun opt-specialize-constant-args (function-name parameters body constant-bindings
                                     &key specialized-name)
  "Build a residual helper copy of BODY with constant PARAMETERS substituted.

This is a helper-level partial-evaluation primitive: it does not execute code or
install a clone in the function registry. It records the static signature and
returns a residual body that later passes can fold safely."
  (let* ((signature (%opt-constant-binding-signature parameters constant-bindings))
          (name (or specialized-name
                    (%opt-specialized-name function-name signature))))
    (make-opt-partial-specialization
     :original-name function-name
     :specialized-name name
     :signature signature
     :static-args signature
     :dynamic-args (%opt-dynamic-parameters parameters signature)
     :residual-body (if (%opt-body-vm-instructions-p body)
                        (%opt-build-vm-residual-body body signature)
                        (cdr (%opt-tree-substitute-constants
                              (cons 'progn body)
                              signature))))))

(defun opt-partial-evaluate-function (function-name parameters body
                                      &key
                                        (constant-bindings nil)
                                        (lattice-bindings nil)
                                        specialized-name)
  "Partially evaluate one function body and return residual + BTA report.

This is a function-level FR-209/210 entrypoint that composes:
1) constant substitution residualization,
2) binding-time analysis merge, and
3) offline BTA classification over residual forms."
  (let* ((specialization
           (opt-specialize-constant-args
            function-name parameters body constant-bindings
            :specialized-name specialized-name))
         (binding-times
           (opt-run-binding-time-analysis
            parameters
            :constant-bindings constant-bindings
            :lattice-bindings lattice-bindings))
         (residual-body (opt-partial-spec-residual-body specialization))
         (form-kinds
           (opt-offline-bta-analyze-body
            residual-body
            :static-bindings (opt-partial-spec-signature specialization)
            :binding-times binding-times))
         (dynamic-body
           (loop for form in residual-body
                 for kind in form-kinds
                 unless (eq kind :static)
                 collect form)))
    (make-opt-partial-eval-result
     :function-name function-name
     :parameters parameters
     :signature (opt-partial-spec-signature specialization)
     :binding-times binding-times
     :form-kinds form-kinds
     :residual-body residual-body
     :dynamic-body dynamic-body
     :specialization specialization)))
