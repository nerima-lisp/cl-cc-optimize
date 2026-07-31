;;;; optimizer-flow-branch-correlation.lisp — correlated branch simplification
;;;;
;;;; When a block's incoming edge already proves its own branch condition
;;;; (the predecessor tested the same predicate on the same registers),
;;;; rewrites the block's terminator to the edge's known outcome instead of
;;;; re-testing it.

(in-package :cl-cc/optimize)

(defun %opt-branch-correlation-forwarder-block-p (block)
  "Return T when BLOCK is a trivial forwarder (single unconditional jump)."
  (let ((insts (bb-instructions block)))
    (and (= (length insts) 1)
         (typep (first insts) 'vm-jump))))

(defun %opt-branch-predicate-fact-from-edge
    (pred edge-label &optional (seen (make-hash-table :test #'eq)))
  "Return branch fact seen on PRED -> EDGE-LABEL, recursively through forwarders.

EDGE-LABEL is the successor label on the edge being analyzed."
  (when (gethash pred seen)
    (return-from %opt-branch-predicate-fact-from-edge nil))
  (setf (gethash pred seen) t)
  (let* ((term (car (last (bb-instructions pred))))
         (target-label (and (typep term 'vm-jump-zero)
                            (vm-label-name term))))
    (cond
      (target-label
       (let ((cond-reg (vm-reg term)))
         (loop for inst in (reverse (bb-instructions pred))
               do (let ((dst (opt-inst-dst inst)))
                    (when (eq dst cond-reg)
                      (return
                        (when (or (opt-foldable-type-pred-p inst)
                                  (typep inst 'vm-not))
                          (list :pred (type-of inst)
                                :src (vm-src inst)
                                :value (if (and edge-label
                                                (equal edge-label target-label))
                                           0
                                           1)))))))))
      ((and (typep term 'vm-jump)
            (%opt-branch-correlation-forwarder-block-p pred)
            (= (length (bb-predecessors pred)) 1)
            (equal (vm-label-name term) edge-label)
            (bb-label pred))
       (%opt-branch-predicate-fact-from-edge
        (first (bb-predecessors pred))
        (vm-name (bb-label pred))
        seen))
      (t nil))))

(defun %opt-branch-predicate-fact-from-predecessor (pred block)
  "Return the branch fact carried on edge PRED -> BLOCK, or NIL."
  (let ((block-label (and (bb-label block) (vm-name (bb-label block)))))
    (%opt-branch-predicate-fact-from-edge pred block-label)))

(defun %opt-same-branch-fact-p (a b)
  "Return T when branch facts A and B prove the same replacement value."
  (and a b
       (eq (getf a :pred) (getf b :pred))
       (eq (getf a :src) (getf b :src))
       (eql (getf a :value) (getf b :value))))

(defun %opt-branch-predicate-fact-for-block (block)
  "Return a branch fact plist for BLOCK, or NIL.

Each predecessor edge must carry the same predicate fact. This preserves the
old single-predecessor behavior and extends FR-168 to simple joins where all
incoming edges agree on the predicate outcome. The returned plist contains:
  :pred  instruction type
  :src   predicate source register
  :value replacement constant (1 on fallthrough, 0 on taken branch)."
  (when-let ((preds (bb-predecessors block)))
    (let ((facts (mapcar (lambda (pred)
                           (%opt-branch-predicate-fact-from-predecessor pred block))
                         preds)))
      (when (and (every #'identity facts)
                 (every (lambda (fact)
                          (%opt-same-branch-fact-p fact (first facts)))
                        (rest facts)))
        (first facts)))))

(defun opt-pass-branch-correlation (instructions)
  "Propagate known predicate outcomes from a dominating conditional edge.

This is a conservative FR-168 style pass: when every predecessor edge into a
block carries the same vm-jump-zero fact over a foldable predicate, repeated
tests of the same predicate on the same source register inside the successor
block are replaced with vm-const 1/0."
  (let* ((cfg (cfg-build instructions))
         ;; Collect every block's fact before rewriting any of them. A fact is
         ;; derived from the predicate instruction that feeds a predecessor's
         ;; VM-JUMP-ZERO, and rewriting replaces exactly those instructions with
         ;; VM-CONST — so folding a block first destroyed the evidence its
         ;; successors needed. A join whose predecessors agree lost its fact
         ;; whenever one of them happened to be processed earlier.
         (facts (map 'vector #'%opt-branch-predicate-fact-for-block (cfg-blocks cfg))))
    (loop for block across (cfg-blocks cfg)
          for fact across facts
          do (progn
               (when fact
                 (let ((live-fact fact)
                       (new-insts nil))
                   (dolist (inst (bb-instructions block))
                     (let ((dst (opt-inst-dst inst)))
                       (when (and live-fact dst
                                  (eq dst (getf live-fact :src)))
                         (setf live-fact nil)))
                     (cond
                       ((and live-fact
                             (or (opt-foldable-type-pred-p inst)
                                 (typep inst 'vm-not))
                             (eq (type-of inst) (getf live-fact :pred))
                             (eq (vm-src inst) (getf live-fact :src))
                             (opt-inst-dst inst))
                        (push (make-vm-const :dst (opt-inst-dst inst)
                                             :value (getf live-fact :value))
                              new-insts))
                       (t
                        (push inst new-insts))))
                   (setf (bb-instructions block) (nreverse new-insts))))))
    (cfg-flatten cfg)))
