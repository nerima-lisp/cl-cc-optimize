(in-package :cl-cc/optimize)

;;; ─── E-Graph Equality Saturation ─────────────────────────────────────────
;;;
;;; Implements an E-graph (equality graph) for equality saturation.
;;; Core operations:
;;;   egraph-add     — add an e-node, return its e-class ID
;;;   egraph-merge   — assert two e-class IDs are equal
;;;   egraph-rebuild — restore congruence invariant after merges
;;;   egraph-saturate — apply rewrite rules until fixed-point
;;;   egraph-extract — extract optimal term using cost model
;;;
;;; The E-graph subsumes the following existing optimizer passes:
;;;   opt-pass-fold            → constant folding rules
;;;   opt-simplify-binop       → algebraic identity rules
;;;   opt-pass-strength-reduce → strength reduction rules
;;;   opt-pass-copy-prop       → FREE: e-class merge subsumes copy propagation
;;;   opt-pass-cse             → FREE: hash-consing deduplicates automatically
;;;
;;; References:
;;;   Willsey et al. (2021). "egg: Fast and Extensible Equality Saturation"
;;;   Nelson & Oppen (1979). "Simplification by Cooperating Decision Procedures"

;;; ─── Data Structures ─────────────────────────────────────────────────────

(defstruct (e-node (:conc-name en-))
  "An e-node represents a function application over e-class IDs.
   A literal payload distinguishes constants with different values."
  (op nil :type symbol)
  (children nil :type list)
  (data nil)
  (data-p nil :type boolean)
  (eclass -1 :type fixnum))        ; owning e-class ID (-1 = unowned)

(defstruct (e-class (:conc-name ec-))
  "An e-class is an equivalence class of expressions that are all equal.
   It holds a list of e-nodes and analysis data (type, cost bounds, etc.)."
  (id       0    :type fixnum)         ; canonical ID
  (nodes    nil  :type list)           ; list of e-node in this class
  (parents  nil  :type list)           ; list of (e-node . parent-eclass-id) referencing us
  (data     nil))                      ; analysis data (type, cost, etc.)

(defstruct (e-graph (:conc-name eg-))
  "The E-graph maintains a union-find structure over e-classes, a memo table
   for hash-consing, and a worklist of pending merges."
  (classes      (make-hash-table)              :type hash-table)  ; id → e-class
  (memo         (make-hash-table :test #'equal) :type hash-table)  ; canonical-enode-key → class-id
  (literal-memo (make-hash-table :test #'eql)   :type hash-table)
  (union-find   (make-hash-table)              :type hash-table)  ; id → canonical id
  (worklist     nil                            :type list)        ; (id1 . id2) pending merges
  (next-id      0                              :type fixnum))

;;; ─── Union-Find ──────────────────────────────────────────────────────────

(defun egraph-find (eg id)
  "Path-compressed union-find lookup.  Returns the canonical e-class ID for ID."
  (let ((parent (gethash id (eg-union-find eg))))
    (if (or (null parent) (= parent id))
        id
        (let ((root (egraph-find eg parent)))
          ;; Path compression
          (setf (gethash id (eg-union-find eg)) root)
          root))))

(defun egraph-canonical-enode (eg node)
  "Return an e-node with all child IDs canonicalized via union-find."
  (make-e-node :op (en-op node)
               :children (mapcar (lambda (c) (egraph-find eg c))
                                 (en-children node))
               :data (en-data node)
               :data-p (en-data-p node)))

;;; ─── E-Node Memo Key ─────────────────────────────────────────────────────

(defun enode-memo-key (node)
  "Compute a memo key from an e-node's operation, children, and literal data."
  (list (en-op node)
        (en-children node)
        (en-data-p node)
        (en-data node)))

;;; ─── Core Operations ─────────────────────────────────────────────────────

(progn
  (defun %egraph-add-node (eg op child-ids data data-p)
    (let* ((canon-children (mapcar (lambda (child) (egraph-find eg child))
                                   child-ids))
           (node (make-e-node :op op
                              :children canon-children
                              :data data
                              :data-p data-p))
           (literal-p (and data-p (eq op 'const)))
           (memo (if literal-p (eg-literal-memo eg) (eg-memo eg)))
           (key (if literal-p data (enode-memo-key node))))
      (or (gethash key memo)
          (let* ((id (eg-next-id eg))
                 (cls (make-e-class :id id
                                    :nodes (list node)
                                    :data (if data-p data nil))))
            (incf (eg-next-id eg))
            (setf (en-eclass node) id
                  (gethash id (eg-classes eg)) cls
                  (gethash id (eg-union-find eg)) id
                  (gethash key memo) id)
            (dolist (child canon-children)
              (when-let ((child-cls (gethash (egraph-find eg child)
                                             (eg-classes eg))))
                (push (cons node id) (ec-parents child-cls))))
            id))))

  (defun egraph-add (eg op &rest child-ids)
    "Add an e-node with operation OP and CHILD-IDS to the e-graph.
     Returns the e-class ID. Structurally identical nodes share a class."
    (%egraph-add-node eg op child-ids nil nil))

  (defun %egraph-add-constant (eg value)
    (%egraph-add-node eg 'const nil value t)))

(defun egraph-merge (eg id1 id2)
  "Assert that e-classes ID1 and ID2 are equal.
   Queues the merge on the worklist (deferred rebuild for efficiency).
   Returns the canonical ID of the merged class."
  (let ((r1 (egraph-find eg id1))
        (r2 (egraph-find eg id2)))
    (if (= r1 r2)
        r1
        ;; Union: smaller-ID class wins as canonical (keeps canonical ID stable)
        (let ((canon     (min r1 r2))
              (non-canon (max r1 r2)))
          (push (cons r1 r2) (eg-worklist eg))
          (setf (gethash non-canon (eg-union-find eg)) canon)
          canon))))

(defun %egraph-absorb-class (eg r1 r2)
  "Destructively merge e-class R2 into R1: move its nodes and parents."
  (let* ((cls1 (gethash r1 (eg-classes eg)))
         (cls2 (gethash r2 (eg-classes eg)))
         (cls1-has-literal
           (and cls1
                (some (lambda (node)
                        (and (eq (en-op node) 'const)
                             (en-data-p node)))
                      (ec-nodes cls1))))
         (cls2-has-literal
           (and cls2
                (some (lambda (node)
                        (and (eq (en-op node) 'const)
                             (en-data-p node)))
                      (ec-nodes cls2)))))
    (when (and cls1 cls2 cls1-has-literal cls2-has-literal
               (not (eql (ec-data cls1) (ec-data cls2))))
      (error "Cannot merge e-graph constants with different values: ~S and ~S"
             (ec-data cls1)
             (ec-data cls2)))
    (when (and cls1 cls2 cls2-has-literal (not cls1-has-literal))
      (setf (ec-data cls1) (ec-data cls2)))
    (when (and cls1 cls2)
      (dolist (node (ec-nodes cls2))
        (setf (en-eclass node) r1)
        (push node (ec-nodes cls1)))
      (dolist (parent (ec-parents cls2))
        (push parent (ec-parents cls1)))
      (remhash r2 (eg-classes eg)))))

(defun %egraph-repair-parent-node (eg pn)
  "Re-canonicalize parent e-node PN after a class merge; apply congruence merge if needed."
  (let* ((old-key (enode-memo-key pn))
         (new-pn  (egraph-canonical-enode eg pn))
         (new-key (enode-memo-key new-pn)))
    (unless (equal old-key new-key)
      (remhash old-key (eg-memo eg))
      (if-let ((existing (gethash new-key (eg-memo eg))))
        (let ((pn-class (egraph-find eg (en-eclass pn))))
          (unless (= pn-class existing)
            (egraph-merge eg pn-class existing)))
        (progn
          (setf (en-children pn) (en-children new-pn))
          (setf (gethash new-key (eg-memo eg)) (en-eclass pn)))))))

(defun egraph-rebuild (eg)
  "Process queued unions, absorb their classes, and restore congruence."
  (loop while (eg-worklist eg)
        do (let ((worklist (eg-worklist eg))
                 (affected nil))
             (setf (eg-worklist eg) nil)
             (dolist (pair worklist)
               (let ((canonical (egraph-find eg (car pair))))
                 (push canonical affected)
                 (dolist (candidate (list (car pair) (cdr pair)))
                   (when (and (/= candidate canonical)
                              (gethash candidate (eg-classes eg)))
                     (%egraph-absorb-class eg canonical candidate)))))
             (dolist (class-id (remove-duplicates affected))
               (let ((cls (gethash (egraph-find eg class-id) (eg-classes eg))))
                 (when cls
                   (dolist (parent-pair (ec-parents cls))
                     (%egraph-repair-parent-node eg (car parent-pair)))))))))

;;; Pattern matching and rule application are in egraph-match.lisp (loads after).
