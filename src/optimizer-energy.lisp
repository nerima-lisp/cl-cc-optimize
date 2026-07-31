;;;; packages/optimize/src/optimizer-energy.lisp — FR-617 Energy-Aware Compilation
;;;; Power-consumption-aware optimization heuristics.
;;;; Research: energy-aware compilation for mobile/IoT.

(in-package :cl-cc/optimize)

(defvar *energy-aware-mode* nil
  "When T, optimizations consider energy cost in addition to speed.")

(defvar *energy-cost-model*
  '((add . 1) (sub . 1) (mul . 3) (div . 10)
    (load . 2) (store . 2) (branch . 1)
    (simd-add . 2) (simd-mul . 4) (simd-fma . 3))
  "Estimated relative energy cost per instruction type.")

(defun instruction-energy-cost (opcode)
  "Return the estimated energy cost of OPCODE."
  (or (cdr (assoc opcode *energy-cost-model*)) 1))

(defun energy-cost-of-block (basic-block)
  "Compute total estimated energy cost of BASIC-BLOCK.
Walks instructions in the block and sums their energy costs.

BASIC-BLOCK must be a list of instructions; this file has no accessor for
any other block representation. (Earlier code looked up a
%BLOCK-INSTRUCTIONS function via FIND-SYMBOL and swallowed the resulting
error with IGNORE-ERRORS -- but that symbol is never defined anywhere in
this system, so the fallback always failed silently. Made explicit here
rather than pretending a non-cons block is supported.)"
  (if basic-block
      (let* ((instrs (and (consp basic-block) basic-block))
             (total 0))
        (when instrs
          (dolist (inst instrs total)
            (incf total (instruction-energy-cost-of-inst inst))))
        total)
      0))

(defun instruction-energy-cost-of-inst (inst)
  "Return the energy cost of a VM instruction by classifying its type."
  (cond
    ((null inst) 0)
    ;; Classify by instruction class name
    ((search "ADD" (symbol-name (type-of inst)) :test #'char-equal) 1)
    ((search "SUB" (symbol-name (type-of inst)) :test #'char-equal) 1)
    ((search "MUL" (symbol-name (type-of inst)) :test #'char-equal) 3)
    ((search "DIV" (symbol-name (type-of inst)) :test #'char-equal) 10)
    ((search "LOAD" (symbol-name (type-of inst)) :test #'char-equal) 2)
    ((search "STORE" (symbol-name (type-of inst)) :test #'char-equal) 2)
    ((search "JUMP" (symbol-name (type-of inst)) :test #'char-equal) 1)
    ((search "BRANCH" (symbol-name (type-of inst)) :test #'char-equal) 1)
    ((search "CALL" (symbol-name (type-of inst)) :test #'char-equal) 3)
    ((search "MOV" (symbol-name (type-of inst)) :test #'char-equal) 0)
    ((search "NOP" (symbol-name (type-of inst)) :test #'char-equal) 0)
    (t 1)))

(defun energy-optimize-block (block)
  "Apply energy-saving optimizations to BLOCK.
Returns modified instruction list or NIL if no changes.

BLOCK must be a list of instructions; see ENERGY-COST-OF-BLOCK's docstring
for why a non-cons block representation is not supported here."
  (let* ((instrs (and (consp block) block))
         (changed nil)
         (result nil))
    (when instrs
      (dolist (inst (reverse instrs))
        (let ((optimized (energy-optimize-instruction inst)))
          (push optimized result)
          (unless (eq optimized inst)
            (setf changed t)))))
    (when changed
      (nreverse result))))

(defun energy-optimize-instruction (inst)
  "Apply an energy-specific rewrite to a single instruction, or return INST
unchanged.

This is a deliberate, verified pass-through, not an unfinished stub. The
transform this hook's docstring originally described -- detecting a
mul-by-power-of-2 and rewriting it to a shift -- is already performed
unconditionally by the E-graph rewrite rules MUL-POW2/MUL-POW2-L in
egraph-rules.lisp (run by %MAYBE-APPLY-PROLOG-REWRITE / the :PROLOG-REWRITE
stage, which is first in *OPT-DEFAULT-CONVERGENCE-PASS-KEYS* and therefore
runs before any other pass sees the instruction stream, independent of
*ENERGY-AWARE-MODE*), and again, redundantly, by the explicit opt-in
OPT-PASS-STRENGTH-REDUCE in optimizer-strength.lisp. By the time any
instruction could reach this hook in the default pipeline, a VM-MUL by a
constant power of two has already become a VM-ASH; re-implementing the same
rewrite here would be dead code shadowed by an earlier pass, not a
policy-specific behavior difference. Duplicating it under a distinct flag
would also invite the two implementations to drift, and the miscompilation
risk of a second, independently-maintained strength-reduction path outweighs
the (nonexistent, since it never fires) benefit.

*ENERGY-AWARE-MODE* remains the intended gate for genuinely energy-specific
rewrites -- ones that trade speed for lower estimated energy cost under
INSTRUCTION-ENERGY-COST in a way no speed-oriented pass would choose on its
own (for example, preferring a scalar sequence over an energy-costlier SIMD
op even when the SIMD op is faster). None have been identified and verified
correct yet, so this hook stays a pass-through until one is."
  inst)

(defun energy-optimize-function (function-instructions)
  "Apply energy-aware optimization pass to FUNCTION-INSTRUCTIONS."
  (mapcar #'energy-optimize-instruction function-instructions))

(defmacro with-energy-aware (&body body)
  `(let ((*energy-aware-mode* t)) ,@body))
