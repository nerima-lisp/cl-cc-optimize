# Quick Start

This walks through optimizing a small instruction stream: two constants, an
add that folds them, and a redundant copy that dead-code elimination removes.

## Build the instruction stream

```lisp
(defparameter *instructions*
  (list (cl-cc/vm:make-vm-const :dst :r0 :value 2)
        (cl-cc/vm:make-vm-const :dst :r1 :value 3)
        (cl-cc/vm:make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
        (cl-cc/vm:make-vm-move :dst :r3 :src :r2)
        (cl-cc/vm:make-vm-halt :reg :r2)))
```

`:r3` is written but never read, so a correct optimizer should drop it.

## Run the full pipeline

```lisp
(cl-cc/optimize:optimize-instructions *instructions*)
;; => (#<VM-CONST :DST :R0 :VALUE 2>
;;     #<VM-CONST :DST :R1 :VALUE 3>
;;     #<VM-ADD :DST :R2 :LHS :R0 :RHS :R1>
;;     #<VM-HALT :REG :R2>)
```

The dead move into `:r3` is gone. Constant folding across `:r0`/`:r1` is left
to a downstream pass in `cl-cc`'s own pipeline; `optimize-instructions` here
runs the passes this repository owns (CFG/SSA construction, e-graph
saturation, dataflow, DCE, and friends) in dependency order.

## Run a single pass directly

Most consumers call `optimize-instructions`, but every pass is independently
exported for testing or for building a custom pipeline:

```lisp
(cl-cc/optimize:opt-pass-fold *instructions*)
;; => the same stream with any provably-constant binops folded
```

## Inspect the CFG a pass sees

```lisp
(let ((cfg (cl-cc/optimize:cfg-build *instructions*)))
  (cl-cc/optimize:cfg-block-count cfg))
;; => 1
```

A single basic block, since nothing here branches.

## Extend with equality saturation

```lisp
(cl-cc/optimize:optimize-with-egraph
 (list (cl-cc/vm:make-vm-const :dst :zero :value 0)
       (cl-cc/vm:make-vm-add :dst :r2 :lhs :r1 :rhs :zero)))
;; => (#<VM-CONST :DST :ZERO :VALUE 0> #<VM-MOVE :DST :R2 :SRC :R1>)
```

`x + 0` rewrites to a move, one of the algebraic identities registered as an
e-graph rewrite rule.

## The complete example

```lisp
(cl-cc/optimize:optimize-instructions
 (list (cl-cc/vm:make-vm-const :dst :r0 :value 2)
       (cl-cc/vm:make-vm-const :dst :r1 :value 3)
       (cl-cc/vm:make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
       (cl-cc/vm:make-vm-move :dst :r3 :src :r2)
       (cl-cc/vm:make-vm-halt :reg :r2)))
```

For what each pass does and how to read `api-reference.md`'s categories, see
[API Reference](api-reference.md).
