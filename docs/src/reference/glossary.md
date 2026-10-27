# Glossary

The API lists are examples, not complete inventories.

## analysis

- Definition: Builds facts that optimization passes use about instruction order, values reaching each program point, function side effects, and memory locations that may refer to the same storage.
- Package: `cl-cc/optimize/analysis`
- Key types: `basic-block`, `cfg`, `opt-dataflow-result`
- Key functions: `cfg-build`, `cfg-compute-dominators`, `compute-function-purity`, `opt-run-dataflow`
- Other contexts: None.

## scalar

- Definition: Simplifies individual values and arithmetic operations, such as replacing a known expression with its result or using narrower integers when that preserves behavior.
- Package: `cl-cc/optimize/scalar`
- Key types: None.
- Key functions: `opt-pass-fold`, `opt-pass-bitwidth-reduction`, `opt-pass-overflow-check-elimination`
- Other contexts: None.

## control-flow

- Definition: Estimates which branch a program is more likely to take. It can remove a type check when earlier checks already establish the type, or duplicate code to simplify a branch that is likely to run.
- Package: `cl-cc/optimize/control-flow`
- Key types: None.
- Key functions: `opt-analyze-branch-weights`, `opt-pass-dominated-type-check-elim`, `opt-pass-tail-duplication`
- Other contexts: None.

## loops

- Definition: Tracks induction variables, which change predictably on each loop iteration, and estimates how many times a loop runs. Polyhedral scheduling models the valid combinations of loop indexes (the iteration domain) and the memory locations each iteration accesses. Integer constraints on those values show when swapping loop order or grouping iterations into tiles preserves required memory order. Superword-level parallelism (SLP) combines independent operations on adjacent values into one instruction that processes several values at once.
- Package: `cl-cc/optimize/loops`
- Key types: `polyhedral-access`, `polyhedral-domain`, `polyhedral-statement`
- Key functions: `opt-compute-loop-inductions`, `opt-induction-trip-count`, `opt-pass-loop-unroll`, `opt-pass-polyhedral`, `opt-pass-slp-vectorize`
- Other contexts: None.

## memory

- Definition: Analyzes memory reads and writes and whether allocated data can escape the current function. It uses those facts to remove bounds checks that cannot fail and move each allocation into the branch where its result is used.
- Package: `cl-cc/optimize/memory`
- Key types: None.
- Key functions: `opt-analyze-memory-access-patterns`, `opt-pass-bounds-check-elimination`, `opt-pass-escape-analysis`, `opt-sink-allocations`
- Other contexts: None.

## inlining

- Definition: Optimizes function calls. Devirtualization replaces an indirect call with a known target. When several branches reach the same call and each branch has a known target, call-site splitting copies the call into each branch so each copy can use that target. Dead-argument elimination removes unused parameters. Interprocedural constant propagation (IPCP) uses constant arguments at known calls to create specialized function versions. Inlining puts a callee's body in its caller, while outlining moves repeated code into a separate function.
- Package: `cl-cc/optimize/inlining`
- Key types: None.
- Key functions: `opt-pass-call-site-splitting`, `opt-pass-devirtualize`, `opt-pass-dead-argument-elimination`, `opt-pass-function-outlining`, `opt-pass-ipcp`
- Other contexts: None.

## partial-evaluation

- Definition: Marks values as known during compilation or known only at runtime, then specializes a function or program using values already known during compilation.
- Package: `cl-cc/optimize/partial-evaluation`
- Key types: None.
- Key functions: `opt-partial-evaluate-function`, `opt-partial-evaluate-program`, `opt-run-binding-time-analysis`, `opt-specialize-constant-args`
- Other contexts: None.

## speculation

- Definition: Chooses faster operations using runtime profiles or assumptions, with checks or fallback behavior when an assumption is false. An inline cache remembers recently seen call targets; an atomic operation is indivisible to other threads. For thread-local storage (TLS), it uses the target architecture and whether accesses are hot to choose inline access and its base register (FS on x86-64 or TPIDR_EL0 on AArch64). It also selects atomic opcodes for a target architecture.
- Package: `cl-cc/optimize/speculation`
- Key types: None.
- Key functions: `opt-build-tls-plan`, `opt-ic-resolve-target`, `opt-ic-transition`, `opt-select-atomic-opcode`, `opt-speculation-allowed-p`
- Other contexts: None.

## rewriting

- Definition: Replaces expressions with equivalent forms using rewrite rules. An e-node represents an operation applied to child e-classes; an e-class groups e-nodes for expressions known to be equivalent. An e-graph stores these groups so equality saturation can add alternatives before selecting a form, while peephole rules transform small instruction sequences.
- Package: `cl-cc/optimize/rewriting`
- Key types: `e-class`, `e-graph`, `e-node`
- Key functions: `apply-prolog-peephole`, `apply-rewrite-rules`, `egraph-saturate`, `opt-pass-superopt`
- Other contexts: None.

## pipeline

- Definition: Configures optimization policy and runs optimization passes in order. When enabled, automatic tuning uses detected CPU cache sizes to choose loop-tile sizes and SIMD lane counts.
- Package: `cl-cc/optimize/pipeline`
- Key types: None.
- Key functions: `apply-optimization-level`, `opt-configure-optimization-policy`, `opt-pass-autotune-simd`, `optimize-instructions`
- Other contexts: None.

## reporting

- Definition: Records execution counts for control-flow paths and presents profile summaries and explanations of optimization decisions. Ball-Larus profiling assigns values to control-flow edges. Instrumentation adds those values along a path and records the total when the path exits; that total identifies the path whose execution is counted.
- Package: `cl-cc/optimize/reporting`
- Key types: `opt-ball-larus-edge`, `opt-ball-larus-profile`, `opt-path-profile-block`
- Key functions: `opt-build-ball-larus-profile`, `opt-compute-path-profile`, `opt-pass-optimization-remarks`
- Other contexts: None.

## verification

- Definition: Checks optimizer intermediate representation (IR) invariants. Translation validation checks inexpensive invariants between the instruction streams before and after an optimization, including basic-block structure and register liveness. It does not prove semantic equivalence. When enabled, the optimizer integration warns about a failed check and continues compilation.
- Package: `cl-cc/optimize/verification`
- Key types: `translation-validation-error`
- Key functions: `opt-verify-ir`, `validate-translation`, `validate-optimizer-translation`, `opt-pass-translation-validation`
- Other contexts: None.
