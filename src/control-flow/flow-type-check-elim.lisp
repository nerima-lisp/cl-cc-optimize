;;;; optimizer-flow-type-check-elim.lisp — dominated redundant type-check elimination

(in-package :cl-cc/optimize)

(defun %type-check-elim-forget-def (facts reg)
  "Remove all facts whose :src or :dst is REG (killed by a def of REG)."
  (remove-if (lambda (fact)
               (or (eq (getf fact :src) reg)
                   (eq (getf fact :dst) reg)))
             facts))

(defun %type-check-elim-lookup-fact (facts pred src)
  "Find the first fact matching predicate PRED applied to source register SRC."
  (find-if (lambda (fact)
             (and (eq (getf fact :pred) pred)
                  (eq (getf fact :src) src)))
           facts))

(defun %type-check-elim-process-block (block facts)
  "Walk BLOCK under dominator FACTS; rewrite redundant type checks; recurse into dom-children."
  (let ((local-facts (copy-list facts))
        (new-insts nil))
    (dolist (inst (bb-instructions block))
      (when-let ((dst (opt-inst-dst inst)))
        (setf local-facts (%type-check-elim-forget-def local-facts dst)))
      (cond
       ((and (or (opt-foldable-type-pred-p inst) (typep inst 'vm-not))
             (vm-src inst) (vm-dst inst))
        (let* ((pred (type-of inst))
               (src  (vm-src inst))
               (dst  (vm-dst inst))
               (fact (%type-check-elim-lookup-fact local-facts pred src)))
          (if (and fact (not (eq dst (getf fact :dst))))
              (push (make-vm-move :dst dst :src (getf fact :dst)) new-insts)
              (progn
                (push inst new-insts)
                (push (list :pred pred :src src :dst dst) local-facts)))))
       (t (push inst new-insts))))
    (setf (bb-instructions block) (nreverse new-insts))
    (dolist (child (bb-dom-children block))
      (%type-check-elim-process-block child local-facts))))

(defun opt-pass-dominated-type-check-elim (instructions)
  "Eliminate redundant pure type predicates dominated by an earlier identical
    predicate on the same source register. Nil checks via vm-not are treated
    the same way. The first check is kept; later checks are replaced with
    vm-move from the dominating result register."
  (let ((cfg (cfg-build instructions)))
    (when (cfg-entry cfg)
      (cfg-compute-dominators cfg)
      (%type-check-elim-process-block (cfg-entry cfg) nil))
    (cfg-flatten cfg)))
