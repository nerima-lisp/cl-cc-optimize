;;;; optimizer-peval-program.lisp — whole-program partial evaluation
;;;;
;;;; Runs optimizer-peval-specialize.lisp's per-function specialization
;;;; across every function in a program and merges the resulting constant
;;;; bindings, so a constant discovered by specializing one function can
;;;; feed the decision to specialize another that calls it.

(in-package :cl-cc/optimize)

(defun %opt-merge-constant-binding (const-map fn param value)
  "Merge inferred (PARAM . VALUE) into CONST-MAP for FN.

Returns T when map changed. Conflicting values conservatively drop knowledge."
  (let* ((fn-bindings (or (gethash fn const-map)
                          (setf (gethash fn const-map) (make-hash-table :test #'equal))))
         (present (nth-value 1 (gethash param fn-bindings))))
    (cond
      ((not present)
       (setf (gethash param fn-bindings) value)
       t)
      ((equal (gethash param fn-bindings) value)
       nil)
      (t
       ;; Conflict => unknown for that parameter.
       (remhash param fn-bindings)
       t))))

(defun %opt-peval-static-const-value (node static-signature)
  "Return the compile-time-known value of NODE under STATIC-SIGNATURE (a
param-name -> value alist), or :UNKNOWN when NODE isn't foldable."
  (cond
    ((symbolp node)
     (if-let ((cell (assoc node static-signature :test #'equal)))
       (cdr cell)
       :unknown))
    ((or (numberp node) (stringp node) (characterp node)
         (keywordp node) (member node '(nil t) :test #'eq))
     node)
    ((and (consp node) (eq (car node) 'quote))
     (second node))
    (t :unknown)))

(defun %opt-extract-constant-call-entry (node static-signature)
  "Return a (HEAD . ((param-index . const-value) ...)) entry for call-shaped
NODE's constant-valued arguments under STATIC-SIGNATURE, or NIL when NODE's
head isn't a symbol or none of its arguments fold to a known constant."
  (let ((head (car node)))
    (when (symbolp head)
      (let ((pairs nil))
        (loop for arg in (cdr node)
              for i from 0
              for v = (%opt-peval-static-const-value arg static-signature)
              unless (eq v :unknown)
              do (push (cons i v) pairs))
        (when pairs
          (list (cons head (nreverse pairs))))))))

(defun %opt-extract-constant-calls-from-form (form static-signature)
  "Collect conservative call-site constant bindings from FORM.

Returns alist entries: (callee . ((param-index . const-value) ...))"
  (labels ((walk (node)
             (cond
               ((atom node) nil)
               ((member (car node) '(quote function) :test #'eq) nil)
               (t
                (append
                 (%opt-extract-constant-call-entry node static-signature)
                 (mapcan #'walk node))))))
    (walk form)))

(defun %opt-record-call-constant-bindings (call params-by-fn const-map)
  "Merge constant-argument facts from one call entry CALL (as returned by
%OPT-EXTRACT-CONSTANT-CALLS-FROM-FORM) into CONST-MAP, using PARAMS-BY-FN
to resolve each constant argument's index to a parameter name."
  (let* ((callee (car call))
         (idx-vals (cdr call))
         (params (gethash callee params-by-fn)))
    (when params
      (dolist (iv idx-vals)
        (let* ((idx (car iv))
               (value (cdr iv))
               (param (nth idx params)))
          (when param
            (%opt-merge-constant-binding const-map callee param value)))))))

(defun %opt-record-report-constant-bindings (entry params-by-fn const-map)
  "Merge constant-argument facts found in one partial-eval report ENTRY's
residual body into CONST-MAP."
  (let* ((report (cdr entry))
         (sig (opt-partial-eval-signature report)))
    (dolist (form (opt-partial-eval-residual-body report))
      (dolist (call (%opt-extract-constant-calls-from-form form sig))
        (%opt-record-call-constant-bindings call params-by-fn const-map)))))

(defun %opt-const-map-to-alist (const-map)
  "Convert CONST-MAP (fn -> (param -> value) hash tables) into the alist
((fn . ((param . value) ...)) ...) result shape."
  (let (result)
    (maphash
     (lambda (fn table)
       (let (pairs)
         (maphash (lambda (k v) (push (cons k v) pairs)) table)
         (push (cons fn (nreverse pairs)) result)))
     const-map)
    (nreverse result)))

(defun %opt-build-inferred-constant-bindings (function-definitions reports)
  "Infer inter-function constant bindings from REPORTS residual signatures.

Produces alist: ((fn . ((param . value) ...)) ...)."
  (let ((params-by-fn (make-hash-table :test #'equal))
        (const-map (make-hash-table :test #'equal)))
    (dolist (def function-definitions)
      (setf (gethash (first def) params-by-fn)
            (coerce (getf (rest def) :params) 'list)))
    (dolist (entry reports)
      (%opt-record-report-constant-bindings entry params-by-fn const-map))
    (%opt-const-map-to-alist const-map)))

(defun %opt-merge-constant-binding-alists (base inferred)
  "Merge INFERRED bindings into BASE conservatively.

BASE values are kept when conflicts arise; INFERRED only adds missing facts."
  (let ((result (copy-tree base)))
    (dolist (entry inferred result)
      (let* ((fn (car entry))
             (new-pairs (cdr entry))
             (cell (assoc fn result :test #'equal)))
        (if cell
            (dolist (pair new-pairs)
              (unless (assoc (car pair) (cdr cell) :test #'equal)
                (setf (cdr cell) (append (cdr cell) (list pair)))))
            (push (cons fn (copy-list new-pairs)) result))))))

(defun opt-partial-evaluate-program (function-definitions
                                     &key
                                       (constant-bindings-by-function nil)
                                       (lattice-bindings-by-function nil)
                                       (max-iterations 64))
  "Run function-level partial evaluation across FUNCTION-DEFINITIONS.

FUNCTION-DEFINITIONS format:
  ((fn-name :params (...) :body (...)) ...)

CONSTANT-BINDINGS-BY-FUNCTION and LATTICE-BINDINGS-BY-FUNCTION are alists:
  ((fn-name . ((param . value) ...)) ...)
  ((fn-name . ((param . lattice) ...)) ...)

Returns OPT-PARTIAL-PROGRAM-RESULT with per-function reports.

Performs a monotone inter-function fixpoint: inferred constants from residual
call-sites are propagated across function boundaries until convergence.
MAX-ITERATIONS is a safety guard for pathological inputs." 
  (let ((current-consts constant-bindings-by-function)
        (reports nil))
    (loop for iter from 1
          while (<= iter (max 1 max-iterations))
          do (setf reports
                   (loop for def in function-definitions
                         for fn = (first def)
                         for params = (getf (rest def) :params)
                         for body = (getf (rest def) :body)
                         for consts = (cdr (assoc fn current-consts :test #'equal))
                         for lattices = (cdr (assoc fn lattice-bindings-by-function :test #'equal))
                         collect (cons fn
                                       (opt-partial-evaluate-function
                                        fn params body
                                        :constant-bindings consts
                                        :lattice-bindings lattices))))
             (let* ((inferred (%opt-build-inferred-constant-bindings function-definitions reports))
                    (next-consts (%opt-merge-constant-binding-alists current-consts inferred)))
               (if (equal next-consts current-consts)
                   (return)
                   (setf current-consts next-consts))))
    (make-opt-partial-program-result :function-results reports)))
