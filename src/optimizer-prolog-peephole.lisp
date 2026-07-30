(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Prolog — Peephole Rewriting Layer
;;;
;;; Contains: %remove-self-move-p, %match-peephole-rule,
;;; %maybe-peephole-rewrite, apply-prolog-peephole.
;;;
;;; Rule data lives in peephole-data.lisp. UNIFY/LOGIC-SUBSTITUTE come from
;;; the external :cl-prolog engine (imported unqualified in package.lisp).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun %remove-self-move-p (instruction)
  "Return true when INSTRUCTION is a redundant self move."
  (and (consp instruction)
       (eq (car instruction) :move)
       (eql (cadr instruction) (caddr instruction))))

(defun %match-peephole-rule (rule current next)
  "Return replacement instructions when RULE matches CURRENT and NEXT."
  (destructuring-bind (cur-pat next-pat result-list) rule
    (multiple-value-bind (env ok) (unify cur-pat current nil)
      (when ok
        (multiple-value-bind (env2 ok2) (unify next-pat next env)
          (when ok2
            (mapcar (lambda (template)
                      (logic-substitute template env2))
                    result-list)))))))

(progn
  (defun %peephole-pattern-candidate-p (pattern instruction)
    "Return true when PATTERN can structurally match INSTRUCTION."
    (or (symbolp pattern)
        (and (consp pattern)
             (consp instruction)
             (eql (car pattern) (car instruction)))))

  (defun %peephole-rule-candidate-p (rule current next)
    "Return true when RULE opcode patterns can match CURRENT and NEXT."
    (and (%peephole-pattern-candidate-p (first rule) current)
         (%peephole-pattern-candidate-p (second rule) next))))

(defun %maybe-peephole-rewrite (current next)
  "Try candidate peephole rules for CURRENT/NEXT and return the first replacement."
  (dolist (rule *peephole-rules*)
    (when (%peephole-rule-candidate-p rule current next)
      (let ((replacements (%match-peephole-rule rule current next)))
        (when replacements
          (return replacements))))))

(defun %peephole-walk (rest out)
  "Scan REST left-to-right in pairs, accumulating rewritten instructions into OUT."
  (cond
    ((null rest)       (nreverse out))
    ((null (cdr rest)) (nreverse (cons (car rest) out)))
    (t
     (let ((replacements (%maybe-peephole-rewrite (car rest) (cadr rest))))
       (if replacements
           (%peephole-walk (cddr rest) (revappend replacements out))
           (%peephole-walk (cdr rest)  (cons (car rest) out)))))))

(defun apply-prolog-peephole (instructions)
  "Apply Prolog-unification peephole rules over two-instruction windows.

   Rule format: each rule in cl-cc/prolog::*peephole-rules* is a three-element list
     (CURRENT-PATTERN NEXT-PATTERN REPLACEMENT-LIST)
   On a match, both instructions are consumed and REPLACEMENT-LIST sexps emitted.
   Self-moves (:move :Rx :Rx) are removed in a pre-pass."
  (%peephole-walk (remove-if #'%remove-self-move-p instructions) nil))
