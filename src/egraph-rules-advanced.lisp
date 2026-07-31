(in-package :cl-cc/optimize)
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
                   (find-symbol (symbol-name name) :cl-cc/optimize)
                 (if foundp sym name))
               name)))
    (mapcar (lambda (solution)
              (let ((name (%normalize-rule-name (cl-prolog:solution-binding '?name solution)))
                    (lhs  (cl-prolog:solution-binding '?lhs solution))
                    (rhs  (cl-prolog:solution-binding '?rhs solution)))
                (list :name name
                     :lhs lhs
                     :rhs rhs
                     :when (gethash name *egraph-rule-guards*))))
            (cl-prolog:query-prolog *egraph-rulebase* '(egraph-rule ?name ?lhs ?rhs)))))

;;; ─── E-Graph Instruction Rewriter ────────────────────────────────────────

(defun %egraph-rewrite-inst (inst eg reg-map class->representative)
  "Lower INST using equality-class information from the saturated e-graph EG.
Constants proven equal are folded; non-representative registers are aliased to
a deterministic representative while its defining instruction is preserved."
  (let ((dst (ignore-errors (vm-dst inst))))
    (cond
      ((null dst) inst)
      ((egraph-class-has-op-p eg (gethash dst reg-map) (quote const))
       (make-vm-const :dst dst
                      :value (egraph-class-const-value eg (gethash dst reg-map))))
      (t
       (let* ((class-id (gethash dst reg-map))
              (canon (and class-id (egraph-find eg class-id)))
              (representative (and canon (gethash canon class->representative))))
         (if (and representative (not (eq representative dst)))
             (make-vm-move :dst dst :src representative)
             inst))))))

;;; ─── E-Graph Optimization Entry Point ────────────────────────────────────

(defun %egraph-class-representatives (eg reg-map instructions)
  "Return canonical e-class IDs mapped to deterministic VM register representatives.
Destination registers are preferred in instruction order so rewritten moves never
refer to a definition that originally appeared later in the instruction stream."
  (let ((representatives (make-hash-table :test (function equal))))
    (dolist (inst instructions)
      (let* ((dst (ignore-errors (vm-dst inst)))
             (class-id (and dst (gethash dst reg-map)))
             (canon (and class-id (egraph-find eg class-id))))
        (when (and canon (null (gethash canon representatives)))
          (setf (gethash canon representatives) dst))))
    (let ((entries nil))
      (maphash (lambda (reg class-id)
                 (push (cons reg class-id) entries))
               reg-map)
      (dolist (entry (sort entries (function string<)
                           :key (lambda (item) (prin1-to-string (car item)))))
        (let ((canon (egraph-find eg (cdr entry))))
          (unless (gethash canon representatives)
            (setf (gethash canon representatives) (car entry))))))
    representatives))

(defun optimize-with-egraph (instructions &key
                                            (rules (egraph-builtin-rules))
                                            (saturation-limit 30)
                                            (saturation-fuel 10000))
  "Optimize a list of VM INSTRUCTIONS using e-graph equality saturation.
   Returns an optimized instruction list.

   Algorithm:
      1. Add instructions to e-graph (building reg→class mapping)
      2. Saturate with RULES until fixed-point or resource limit
      3. Lower destination classes proven equal to constants or register aliases

   This pass is wired into the main optimizer pipeline via :egraph and also
   participates in the broader :prolog-rewrite stage."
  (unless instructions (return-from optimize-with-egraph instructions))
  (let* ((eg (make-e-graph))
         (reg-map (egraph-add-instructions eg instructions)))
    (egraph-saturate eg rules
                     :limit saturation-limit
                     :fuel saturation-fuel)
    (egraph-rebuild eg)
    (let ((class->representative (%egraph-class-representatives eg reg-map instructions)))
      (mapcar (lambda (inst)
                (%egraph-rewrite-inst inst eg reg-map class->representative))
              instructions))))
