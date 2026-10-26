(in-package #:cl-cc/optimize/rewriting)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; E-Graph — Advanced Rules and Entry Point
;;;
;;; Extracted from egraph-rules.lisp.
;;; Contains:
;;;   - Advanced rewrite rules (mul-neg-neg, neg-sub) not yet in the optimizer
;;;   - egraph-builtin-rules — registry inspector
;;;   - optimize-with-egraph — main optimization entry point
;;;
;;; Load order: after egraph-rules.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── Advanced Rules (new — not in current optimizer) ─────────────────────

(defrule mul-neg-neg
  (mul (neg ?x) (neg ?y))
  (mul ?x ?y))

(defrule neg-sub
  (neg (sub ?x ?y))
  (sub ?y ?x))

;;; ─── All Built-In Rules ──────────────────────────────────────────────────

(defun egraph-builtin-rules ()
  "Return the list of built-in e-graph rewrite rules.
The primary source of truth is the Prolog `egraph-rule` fact set emitted by
`defrule`; the in-memory guard table is consulted only to attach existing `:when`
predicates to the fact-backed rule records."
  (flet ((%normalize-rule-name (name)
           (if (symbolp name)
               (multiple-value-bind (sym foundp)
                   (find-symbol (symbol-name name) :cl-cc/optimize/rewriting)
                 (if foundp sym name))
               name)))
    (mapcar (lambda (solution)
              (let ((name (%normalize-rule-name (cl-prolog-kit:solution-binding '?name solution)))
                    (lhs  (cl-prolog-kit:solution-binding '?lhs solution))
                    (rhs  (cl-prolog-kit:solution-binding '?rhs solution)))
                (list :name name
                     :lhs lhs
                     :rhs rhs
                     :when (gethash name *egraph-rule-guards*))))
            (cl-prolog-kit:query-prolog *egraph-rulebase* '(egraph-rule ?name ?lhs ?rhs)))))

;;; ─── E-Graph Instruction Rewriter ────────────────────────────────────────

(defun %egraph-rewrite-inst (inst class-id eg class->representative index
                             instruction-blocks)
  "Lower one instruction using an available definition in EG."
  (let ((dst (ignore-errors (vm-dst inst))))
    (cond
      ((null dst) inst)
      ((egraph-class-has-op-p eg class-id (quote const))
       (make-vm-const :dst dst
                      :value (egraph-class-const-value eg class-id)))
      (t
       (let* ((canon (egraph-find eg class-id))
              (representative (gethash canon class->representative))
              (candidate-index (car representative)))
         (if (and representative
                  (< candidate-index index)
                  (or (null instruction-blocks)
                      (= (aref instruction-blocks candidate-index)
                         (aref instruction-blocks index)))
                  (not (eq (cdr representative) dst)))
             (make-vm-move :dst dst :src (cdr representative))
             inst))))))

;;; ─── E-Graph Optimization Entry Point ────────────────────────────────────

(defun %egraph-class-representatives (eg instruction-classes instructions)
  "Map e-classes to their first single-definition register."
  (let ((definition-counts (make-hash-table :test (function eq)))
        (representatives (make-hash-table :test (function equal))))
    (loop for inst in instructions
          for dst = (ignore-errors (vm-dst inst))
          when dst
            do (incf (gethash dst definition-counts 0)))
    (loop for inst in instructions
          for class-id in instruction-classes
          for index from 0
          for dst = (ignore-errors (vm-dst inst))
          when (and dst (= 1 (gethash dst definition-counts 0)))
            do (let ((canon (egraph-find eg class-id)))
                 (unless (gethash canon representatives)
                   (setf (gethash canon representatives)
                         (cons index dst)))))
    representatives))

(defun optimize-with-egraph (instructions &key
                                            (rules (egraph-builtin-rules))
                                            (saturation-limit 30)
                                            (saturation-fuel 10000))
  "Optimize VM INSTRUCTIONS using equality saturation and lower each definition."
  (unless instructions (return-from optimize-with-egraph instructions))
  (let ((eg (make-e-graph)))
    (multiple-value-bind (reg-map instruction-classes)
        (egraph-add-instructions eg instructions)
      (declare (ignore reg-map))
      (egraph-saturate eg rules
                       :limit saturation-limit
                       :fuel saturation-fuel)
      (egraph-rebuild eg)
      (let* ((class->representative
               (%egraph-class-representatives eg instruction-classes instructions))
             (instruction-blocks
               (when (loop for inst in instructions
                             for next in (cdr instructions)
                             thereis (or (typep inst
                                                (quote (or vm-jump vm-jump-zero vm-ret vm-halt)))
                                         (typep next (quote vm-label))))
                 (let ((blocks (make-array (length instructions)))
                       (block-id 0)
                       (previous-boundary nil))
                   (loop for inst in instructions
                         for index from 0
                         do (when (and (plusp index)
                                       (or previous-boundary
                                           (typep inst (quote vm-label))))
                              (incf block-id))
                            (setf (aref blocks index) block-id
                                  previous-boundary
                                  (typep inst
                                         (quote (or vm-jump vm-jump-zero vm-ret vm-halt)))))
                   blocks))))
        (loop for inst in instructions
              for class-id in instruction-classes
              for index from 0
              collect (%egraph-rewrite-inst
                       inst class-id eg class->representative index
                       instruction-blocks))))))

