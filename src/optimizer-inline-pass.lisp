;;;; optimizer-inline-pass.lisp — Inlining Infrastructure (memo, DCE)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;
;;; Contains: opt-make-pure-function-memo-table,
;;; opt-pure-function-memo-get, opt-pure-function-memo-put,
;;; opt-function-body-instruction-tables,
;;; opt-top-level-function-roots, opt-reachable-function-labels,
;;; opt-pass-global-dce.
;;;
;;; Load order: after optimizer-purity.lisp, before optimizer-inline-cost.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/optimize)

(defvar *opt-pure-memo-metadata* (make-hash-table :test #'eq)
  "EQ map: memo hash-table -> plist metadata (:max-size, :order).")

(defun %opt-memo-metadata (memo-table)
  (or (gethash memo-table *opt-pure-memo-metadata*)
      (setf (gethash memo-table *opt-pure-memo-metadata*)
            (list :max-size nil :order nil))))

(defun %opt-memo-order-touch (memo-table key)
  "Mark KEY as most-recently-used for MEMO-TABLE."
  (let* ((meta (%opt-memo-metadata memo-table))
         (order (getf meta :order)))
    (setf (getf meta :order)
          (cons key (remove key order :test #'equal)))
    meta))

(defun %opt-memo-evict-if-needed (memo-table)
  "Evict least-recently-used entries when MEMO-TABLE exceeds max-size."
  (let* ((meta (%opt-memo-metadata memo-table))
         (max-size (getf meta :max-size)))
    (when (and (integerp max-size) (plusp max-size))
      (loop while (> (hash-table-count memo-table) max-size)
            do (let* ((order (getf meta :order))
                      (victim (car (last order))))
                 (when victim
                   (remhash victim memo-table)
                   (setf (getf meta :order)
                         (remove victim order :test #'equal))))))))

(progn
(defun opt-make-pure-function-memo-table (&key max-size)
  "Create a memo table for pure-function result caching."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash table *opt-pure-memo-metadata*)
          (list :max-size max-size :order nil))
    table))

(defun opt-make-pure-function-runtime-memoizer (function &key max-size)
  "Return a runtime memoizing wrapper for explicitly pure FUNCTION.
All returned values, including NIL and zero values, are preserved."
  (check-type function function)
  (let ((memo (opt-make-pure-function-memo-table :max-size max-size))
        (pure-labels (make-hash-table :test #'eq))
        (label (gensym "PURE-RUNTIME-")))
    (setf (gethash label pure-labels) t)
    (lambda (&rest args)
      (multiple-value-bind (cached found-p)
          (opt-pure-function-memo-get memo pure-labels label args)
        (if found-p
            (values-list cached)
            (let ((values (multiple-value-list (apply function args))))
              (opt-pure-function-memo-put memo pure-labels label args values)
              (values-list values)))))))
)

(defun opt-pure-function-memo-get (memo-table pure-labels label args)
  "Return cached result for pure LABEL/ARGS, or NIL with a miss flag."
  (if (gethash label pure-labels)
      (let ((key (list label args)))
        (multiple-value-bind (value found-p)
            (gethash key memo-table)
          (when found-p
            (%opt-memo-order-touch memo-table key))
          (values value found-p)))
      (values nil nil)))

(defun opt-pure-function-memo-put (memo-table pure-labels label args result)
  "Store RESULT for pure LABEL/ARGS in MEMO-TABLE.
Impure or unknown labels are ignored conservatively."
  (when (gethash label pure-labels)
    (let ((key (list label args)))
      (setf (gethash key memo-table) result)
      (%opt-memo-order-touch memo-table key)
      (%opt-memo-evict-if-needed memo-table)))
  result)

(defun opt-function-body-instruction-tables (func-defs)
  "Single-pass scan of FUNC-DEFS; returns (values body-inst-set inst->label).
body-inst-set: EQ hash-table, every instruction that belongs to a function body.
inst->label:   EQ hash-table, maps each such instruction to its owning label."
  (let ((body-inst-set (make-hash-table :test #'eq))
        (inst->label   (make-hash-table :test #'eq)))
    (maphash (lambda (label def)
               (dolist (inst (getf def :body))
                 (setf (gethash inst body-inst-set) t
                       (gethash inst inst->label)   label)))
             func-defs)
    (values body-inst-set inst->label)))

(defun opt-top-level-function-roots (instructions func-defs name-to-label body-inst-set)
  "Return an EQUAL hash-table of function labels reachable from top-level code.
Only instructions outside collected function bodies are scanned. This keeps the
analysis conservative and avoids treating nested function internals as roots."
  (let ((roots (make-hash-table :test #'equal))
        (reg-track (make-hash-table :test #'eq)))
    (dolist (inst instructions roots)
      (unless (gethash inst body-inst-set)
        (typecase inst
          ((or vm-closure vm-func-ref)
           (let ((label (vm-label-name inst)))
             (when (gethash label func-defs)
               (setf (gethash (vm-dst inst) reg-track) label))))
          (vm-const
           (let ((label (and (symbolp (vm-value inst))
                             (gethash (vm-value inst) name-to-label))))
             (when (and label (gethash label func-defs))
               (setf (gethash (vm-dst inst) reg-track) label))))
          (vm-move
           (multiple-value-bind (label present-p)
               (gethash (vm-src inst) reg-track)
             (if present-p
                 (setf (gethash (vm-dst inst) reg-track) label)
                 (remhash (vm-dst inst) reg-track))))
          ((or vm-call vm-tail-call)
           (when-let ((label (gethash (vm-func-reg inst) reg-track)))
             (setf (gethash label roots) t))
           (when-let ((dst (opt-inst-dst inst)))
             (remhash dst reg-track)))
          (vm-set-global
           (when-let ((label (gethash (vm-src inst) reg-track)))
             (setf (gethash label roots) t)))
          (t
           (when-let ((dst (opt-inst-dst inst)))
             (remhash dst reg-track))))))))

(defun opt-reachable-function-labels (graph roots)
  "Return an EQUAL hash-table of function labels reachable in GRAPH from ROOTS."
  (let ((reachable (make-hash-table :test #'equal))
        (worklist nil))
    (maphash (lambda (label _)
               (push label worklist))
             roots)
    (loop while worklist
          do (let ((label (pop worklist)))
               (unless (gethash label reachable)
                 (setf (gethash label reachable) t)
                 (dolist (callee (gethash label graph))
                   (push callee worklist)))))
    reachable))

(defun opt-pass-global-dce (instructions)
  "Remove unreachable linear function definitions conservatively.
The pass only removes whole vm-closure/vm-register-function/function-body groups
for labels that are unreachable from top-level function roots. Top-level code is
preserved verbatim. Non-linear or otherwise uncollected functions are left in
place by construction."
  (let* ((func-defs (opt-collect-function-defs instructions))
         (name-to-label (opt-build-function-name-map instructions)))
    (when (zerop (hash-table-count func-defs))
      (return-from opt-pass-global-dce instructions))
    (multiple-value-bind (body-inst-set body-inst-labels)
        (opt-function-body-instruction-tables func-defs)
      (let* ((graph (opt-build-call-graph instructions func-defs name-to-label))
             (roots (opt-top-level-function-roots
                     instructions func-defs name-to-label body-inst-set))
             (reachable (opt-reachable-function-labels graph roots))
             (closure-reg->label (make-hash-table :test #'eq))
             (labels-to-drop (make-hash-table :test #'equal)))
        (%opt-global-dce-collect-drop-labels! labels-to-drop reachable func-defs)
        (when (zerop (hash-table-count labels-to-drop))
          (return-from opt-pass-global-dce instructions))
        (%opt-global-dce-track-closure-regs! instructions closure-reg->label)
        (remove-if (lambda (inst)
                     (%opt-global-dce-drop-instruction-p inst labels-to-drop body-inst-set body-inst-labels closure-reg->label))
                   instructions)))))

;;; Cost model + main inline pass → see optimizer-inline-cost.lisp

(defun %opt-global-dce-drop-instruction-p (inst labels-to-drop body-inst-set body-inst-labels closure-reg->label)
  "Return T when INST belongs to a dropped-label group under LABELS-TO-DROP.
Checks direct closure/func-ref definitions, label markers, function-body
instructions (via BODY-INST-SET/BODY-INST-LABELS), and register-function
installs resolved through CLOSURE-REG->LABEL."
  (cond
    ((and (typep inst '(or vm-closure vm-func-ref))
          (gethash (vm-label-name inst) labels-to-drop))
     t)
    ((and (typep inst 'vm-label)
          (gethash (vm-name inst) labels-to-drop))
     t)
    ((and (gethash inst body-inst-set)
          (not (typep inst 'vm-label))
          (gethash (gethash inst body-inst-labels) labels-to-drop))
     t)
    ((and (typep inst 'vm-register-function)
          (let ((label (gethash (vm-src inst) closure-reg->label)))
            (and label (gethash label labels-to-drop))))
     t)
    (t nil)))

(defun %opt-global-dce-collect-drop-labels! (labels-to-drop reachable func-defs)
  "Mark every FUNC-DEFS label not present in REACHABLE as t in LABELS-TO-DROP."
  (maphash (lambda (label _def)
             (unless (gethash label reachable)
               (setf (gethash label labels-to-drop) t)))
           func-defs))

(defun %opt-global-dce-track-closure-regs! (instructions closure-reg->label)
  "Record CLOSURE-REG->LABEL[dst] = label for every closure/func-ref definition
instruction in INSTRUCTIONS."
  (dolist (inst instructions)
    (when (typep inst '(or vm-closure vm-func-ref))
      (setf (gethash (vm-dst inst) closure-reg->label) (vm-label-name inst)))))
