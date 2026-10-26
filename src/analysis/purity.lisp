;;;; optimizer-purity.lisp — Transitive Function Purity Analysis
;;;;
;;;; Call graph construction and recursive SCC detection for inlining eligibility.
;;;; These functions are called by optimizer-inline-pass.lisp to determine which
;;;; functions can be safely inlined (non-recursive, transitively pure).
;;;;
;;;; Load order: after optimizer-inline.lisp.

(in-package #:cl-cc/optimize/analysis)

(defun opt-build-call-graph (instructions func-defs name-to-label)
  "Return label → direct callees graph for all linear bodies in FUNC-DEFS.
The scan is intentionally whole-program: after LTO has concatenated compilation
units, FUNC-DEFS and NAME-TO-LABEL cover functions registered by every unit, so
symbol designators, direct func refs, and simple moves can form cross-module
edges."
  (declare (ignore instructions))
  (let ((graph (make-hash-table :test #'equal)))
    (maphash (lambda (label def)
               (%opt-call-graph-populate-def label def func-defs name-to-label graph))
             func-defs)
    graph))

(defun opt-call-graph-bottom-up-labels (graph)
  "Return GRAPH labels in leaf-to-root order, suitable for bottom-up inlining."
  (let ((seen (make-hash-table :test #'equal))
        (order nil))
    (labels ((visit (label)
               (unless (gethash label seen)
                 (setf (gethash label seen) t)
                 (dolist (callee (gethash label graph))
                   (visit callee))
                 (push label order))))
      (maphash (lambda (label _callees)
                 (visit label))
               graph))
    (nreverse order)))

(defun %opt-call-graph-reaches-self-p (graph start node seen)
  "Return T if NODE can reach START via GRAPH edges, using SEEN to avoid cycles."
  (dolist (next (gethash node graph) nil)
    (cond
      ((equal next start) (return-from %opt-call-graph-reaches-self-p t))
      ((not (gethash next seen))
       (setf (gethash next seen) t)
       (when (%opt-call-graph-reaches-self-p graph start next seen)
         (return-from %opt-call-graph-reaches-self-p t))))))

(defun opt-call-graph-recursive-labels (graph)
  "Return the set of labels that are in a recursive SCC of GRAPH."
  (let ((recursive (make-hash-table :test #'equal)))
    (maphash (lambda (label _callees)
               (when (%opt-call-graph-reaches-self-p
                      graph label label (make-hash-table :test #'equal))
                 (setf (gethash label recursive) t)))
             graph)
    recursive))

(defun opt-function-body-transitively-pure-p (body func-defs name-to-label pure-labels)
  "Return T if BODY is transitively pure under PURE-LABELS.

Known direct calls are allowed only when their callee label is already marked
pure in PURE-LABELS. Unknown calls remain conservative and therefore impure."
  (let ((reg-track (make-hash-table :test #'eq)))
    (dolist (inst body t)
      (typecase inst
        ((or vm-closure vm-func-ref)
         (let ((label (vm-label-name inst)))
           (if (gethash label func-defs)
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track))))
        (vm-const
         (let ((label (and (symbolp (vm-value inst))
                           (gethash (vm-value inst) name-to-label))))
           (if (and label (gethash label func-defs))
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track))))
        (vm-move
         (multiple-value-bind (label present-p)
             (gethash (vm-src inst) reg-track)
           (if present-p
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track))))
        ((or vm-call vm-tail-call)
         (let ((callee (gethash (vm-func-reg inst) reg-track)))
           (unless (and callee (gethash callee pure-labels))
             (return-from opt-function-body-transitively-pure-p))
           (when (opt-inst-dst inst)
             (remhash (opt-inst-dst inst) reg-track))))
        (t
         (unless (or (typep inst 'vm-ret)
                     (typep inst 'vm-label)
                     (opt-inst-pure-p inst))
           (return-from opt-function-body-transitively-pure-p))
         (when-let ((dst (opt-inst-dst inst)))
           (remhash dst reg-track)))))))

(defun opt-infer-transitive-function-purity (instructions)
  "Infer a conservative set of transitively pure function labels.

The inference covers known direct-call edges between collected linear function
bodies. Recursive SCCs remain conservative and are not marked pure."
  (let* ((func-defs (opt-collect-function-defs instructions))
         (name-to-label (opt-build-function-name-map instructions))
         (graph (opt-build-call-graph instructions func-defs name-to-label))
         (recursive-labels (opt-call-graph-recursive-labels graph))
         (pure-labels (make-hash-table :test #'equal))
         (changed t))
    (loop while changed
          do (setf changed nil)
             (maphash (lambda (label def)
                        (unless (or (gethash label pure-labels)
                                    (gethash label recursive-labels))
                          (when (opt-function-body-transitively-pure-p
                                 (getf def :body) func-defs name-to-label pure-labels)
                            (setf (gethash label pure-labels) t
                                  changed t))))
                      func-defs))
     pure-labels))

(defun %opt-call-graph-track-inst (inst func-defs name-to-label reg-track callees)
  "Update REG-TRACK for INST and return CALLEES with any newly discovered
direct callee from a known-target VM-CALL/VM-TAIL-CALL/VM-APPLY added."
  (typecase inst
    ((or vm-closure vm-func-ref)
     (let ((callee (vm-label-name inst)))
       (if (gethash callee func-defs)
           (setf (gethash (vm-dst inst) reg-track) callee)
           (remhash (vm-dst inst) reg-track)))
     callees)
    (vm-const
     (let ((callee (and (symbolp (vm-value inst))
                        (gethash (vm-value inst) name-to-label))))
       (if (and callee (gethash callee func-defs))
           (setf (gethash (vm-dst inst) reg-track) callee)
           (remhash (vm-dst inst) reg-track)))
     callees)
    (vm-move
     (multiple-value-bind (callee present-p)
         (gethash (vm-src inst) reg-track)
       (if present-p
           (setf (gethash (vm-dst inst) reg-track) callee)
           (remhash (vm-dst inst) reg-track)))
     callees)
    ((or vm-call vm-tail-call vm-apply)
     (let ((callee (gethash (vm-func-reg inst) reg-track)))
       (when (and callee (gethash callee func-defs))
         (setf callees (pushnew callee callees :test #'equal))))
     (when-let ((dst (opt-inst-dst inst)))
       (remhash dst reg-track))
     callees)
    (t
     (when-let ((dst (opt-inst-dst inst)))
       (remhash dst reg-track))
     callees)))

(defun %opt-call-graph-populate-def (label def func-defs name-to-label graph)
  "Compute direct callees for LABELs DEF body and record them in GRAPH."
  (let ((body (getf def :body))
        (reg-track (make-hash-table :test #'eq))
        (callees nil))
    (dolist (inst body)
      (setf callees (%opt-call-graph-track-inst inst func-defs name-to-label reg-track callees)))
    (setf (gethash label graph) callees)))
