# Changelog

### Removal of the ML register allocation hint pass

Removed `OPT-PASS-ML-REGALLOC` and the `:ml-regalloc` pipeline option. The pass
computed register pressure values but never attached them to instructions or
returned them to the register allocator, and no other optimizer feature used
the pass.

### Removal of unreferenced roadmap evidence helpers from the public API

The `optimizer-roadmap-*` files are test support and now load only with
`cl-cc-optimize/test`. Six helpers with no direct symbol references or
unqualified uses in the umbrella or child repositories are no longer exported
from `cl-cc/optimize` or `cl-cc/optimize/reporting`, and their API reference
entries are removed. The 14 helpers remain exported because
`packages/optimize/tests/optimizer-roadmap-backend-tests.lisp` and
`packages/optimize/tests/optimizer-roadmap-tests.lisp` remain in the umbrella
test suite. Removed exports: `opt-roadmap-evidence-summary`,
`optimize-backend-roadmap-all-fr-complete-p`,
`optimize-backend-roadmap-implementation-evidence-complete-p`,
`optimize-roadmap-doc-fr-ids`, `optimize-roadmap-evidence-well-formed-p`, and
`optimize-roadmap-register-doc-evidence`.

### Cross-repository optimizer API exports

Export `opt-collect-function-defs` from the root facade for ThinLTO consumers.
Export `make-vm-branch-weighted-jump-zero` and `opt-vm-path-profile-record` for
pipeline and native backend consumers. Each symbol is also exported by its
owning feature package.
