# cl-cc-optimize

`cl-cc-optimize` is the optimizer subsystem of the [cl-cc](https://github.com/nerima-lisp/cl-cc)
Common Lisp compiler. It builds control-flow graphs and SSA form over `cl-cc-vm`
instruction streams, runs equality saturation through an e-graph, and drives a
multi-pass pipeline covering constant folding, inlining, devirtualization,
memory/alias analysis, loop transforms, vectorization, and speculative
optimization.

```lisp
(cl-cc/optimize:optimize-instructions
 (list (cl-cc/vm:make-vm-const :dst :r0 :value 2)
       (cl-cc/vm:make-vm-const :dst :r1 :value 3)
       (cl-cc/vm:make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
       (cl-cc/vm:make-vm-halt :reg :r2)))
;; => a shorter, folded instruction list
```

Where to go next:

- [Getting Started](getting-started.md) to add this system as a dependency
  and run the optimizer on a small program.
- [API Reference](reference/api.md) for every exported symbol.
- [Development](project/development.md) to build, test, and measure coverage
  locally.
- [Releases](https://github.com/nerima-lisp/cl-cc-optimize/releases) for
  release history.

This project follows the nerima-lisp org's
[coding](https://github.com/nerima-lisp/.github/blob/main/CODING_STANDARD.md),
[package](https://github.com/nerima-lisp/.github/blob/main/PACKAGE_STANDARD.md),
and [test](https://github.com/nerima-lisp/.github/blob/main/TEST_STANDARD.md)
standards. For contribution, security, and support policy, see the
[nerima-lisp/.github](https://github.com/nerima-lisp/.github) repository,
which GitHub serves as the default for every repository in the org.
