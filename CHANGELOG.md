# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!--
Heading format is fixed across the org:

    ## [X.Y.Z] - YYYY-MM-DD

The version is bracketed, the separator is an ASCII hyphen (not an em dash),
and the date is ISO 8601. release.yml extracts the section matching the pushed
tag as the GitHub Release body, so a heading that deviates makes the release
fail. Keep `## [Unreleased]` at the top at all times.

Use only these subsection names, and omit the ones that are empty:
Added / Changed / Deprecated / Removed / Fixed / Security
-->

## [Unreleased]

## [0.5.0] - 2026-08-01

### Changed

- Readability refactoring rounds 3-11 (9 commits, 112 functions total
  across the series): systematic depth-first pass over
  `paredit inspect complexity --output json`, sorted by `max_depth`,
  extracting deeply-nested `defun` bodies into named top-level helpers.
  Pure structural refactor, no behavior change -- every function
  independently verified via `nix build .#checks.aarch64-darwin.default`
  after each edit.
  - Depth-15, depth-14, and depth-13 complexity tiers now fully cleared
    codebase-wide (round 11 corrected an inaccurate round-10 claim that
    these tiers were already clear: 12 functions had been missed by
    every prior survey).
  - Depth-12 tier cut from 30 to 17 remaining functions.
  - Established technique: prefer top-level `defun` extraction over
    `labels`/`flet` when the goal is depth reduction (local-function
    nesting adds a depth level itself); thread mutated accumulators
    through return values (`(values ...)`) or pass small closures as
    explicit function parameters, rather than closing over outer
    mutable state by reference, to avoid silently dropping updates
    across extracted loop bodies.
  - Two real correctness hazards caught and avoided during extraction
    (not shipped as bugs): `%ssa-rename-block`'s `pushed-regs` pop
    order (round 10) and `opt-build-call-graph`'s `callees` accumulator
    scope (round 6).
  - Convergence not yet reached: depth-11 (54 functions) still shows
    the same nesting-pyramid shapes fixed at every higher tier; no
    natural floor confirmed above depth-9. Below depth-9, remaining
    nesting looks like ordinary control flow for this codebase's style,
    not a readability problem.
- `paredit-lint` CI gate now covers 17 additional files touched by this
  round; `paredit inspect check` clean across all 165 `src/*.lisp` files.

## [0.4.3] - 2026-07-31

### Fixed

- Two regressions from this session's own earlier automated `paredit fix
  apply` passes in `optimizer-trmc.lisp`: a dropped `progn` case-clause
  key (silently mistaken for a redundant-`progn` wrapper) and a stripped
  quote that turned `eval-when` into an unbound-variable reference.
- `optimizer-energy.lisp`'s `energy-cost-of-block`/`energy-optimize-block`
  called a function (`%block-instructions`) that does not exist anywhere
  in this system; their `ignore-errors` fallback always silently failed.
- `opt-inst-read-regs`'s sexp-reflection fallback silently
  under-approximated read-registers on failure (unsafe for DCE/liveness);
  now warns instead of failing silently.
- Added `*read-eval* nil` around the one genuine untrusted-input `read`
  call in `optimizer-speculative-ic.lisp` (a persisted speculation log).

### Changed

- First `paredit inspect lint` run this session (distinct from the
  parse-validity check already gated in CI): 421 findings (37
  error-severity) found, 226 (29 error-severity, verified false
  positives) remain after ~212 mechanical style auto-fixes
  (`paredit fix apply`) plus targeted manual fixes for
  `handler-case-swallows-error`, `identity-arithmetic`, and
  `implementation-package-symbol` findings. 22
  `handler-case-swallows-error` instances reviewed and confirmed
  intentional graceful degradation, documented as such.

## [0.4.2] - 2026-07-31

### Added

- `paredit-lint` CI gate (`flake.nix`): fails `nix flake check` on any
  structural (unbalanced-parenthesis) parse error across the tree, via
  `nerima-lisp/paredit-cli`'s `mkLintCheck`.
- 9 more real behavioral tests (reassociate, jump-threading,
  batch-concatenate, global DCE, cons-slot-forward, dominated-type-check-
  elim, fill/bswap-recognition, safepoint-polling), bringing the suite to
  147 tests (146 passing, 1 pre-existing tracked todo).
- `vm-program` test DSL macro and 4 named assertion-predicate helpers in
  `t/optimize-boundary-test.lisp`, replacing 36 hand-written instruction-
  list constructions and 28 repeated notany/some/find-if/count-if-over-
  typep assertions -- a pure test-abstraction refactor, no behavior change.

### Fixed

- `optimize-with-egraph`'s `:cost-fn` parameter, dead on arrival since its
  introducing commit, removed.

### Investigated, not applied

- A `cl-weave` `--coverage-min-expression`/`--coverage-min-branch` CI gate
  (the pattern `cl-dataflow`/`cl-weave`'s own flakes use): confirmed
  empirically that `run-all`'s `:coverage` keyword arrives too late once
  `cl-cc-optimize` is already loaded via the test system's `:depends-on`
  chain, producing 0/0 stats. Doing this properly needs either the
  packaged `cl-weave` CLI (reverting `cl-weave`'s `flake = false`) or a
  `run-tests.lisp` load-order restructure -- left as a follow-up.

## [0.4.1] - 2026-07-31

### Fixed

- `optimize-with-egraph`'s `:cost-fn` keyword argument was dead on arrival
  (`(declare (ignore cost-fn))` since its introducing commit); removed,
  since no call site ever passed it.

### Changed

- Systematic `cl-weave` `:timeout-ms` coverage: added to the 9 test cases
  that loop, run symbolic execution, or drive the full
  `optimize-instructions`/`optimize-with-egraph` pipeline; confirmed
  `it-each`/`it-property` have no options slot in the pinned `cl-weave`
  version to add it to.

## [0.4.0] - 2026-07-31

### Changed

- Introduced `when-let`/`if-let` (Alexandria-style named-binding anaphoric
  macros) and converted 194 `(let ((x e)) (when x ...))`/`(let ((x e)) (if
  x ...))` sites across ~90 files to use them, found via a structural
  (not textual) search for this codebase's dominant "bind once, branch on
  that binding" idiom. Investigated 3 more macro categories from the On
  Lisp catalog (forced dynamic-variable save/restore, a gethash-cache
  idiom, a generic walk/dispatch/accumulate skeleton) and found no
  instance count or shape uniform enough to justify a macro for any of
  them — reported plainly rather than forced.

## [0.3.2] - 2026-07-31

### Changed

- Readability: extracted named local helpers (pure behavior-preserving
  refactor, no logic changes) in the 7 most complex functions flagged by
  this session's code-quality audit —
  `opt-copy-recognition-match-at` (replaced 12 positional `nth` lookups
  with one `destructuring-bind`), `%opt-track-sealed-gf-facts`,
  `%opt-compute-simple-inductions-with-constants`,
  `opt-pass-specialize-known-args`, `opt-pass-strength-reduce`,
  `opt-pass-inline` (split into its implicit two phases:
  per-candidate-body inlining, then whole-instruction-list rewrite), and
  `ssa-destroy`. `%opt-tree-substitute-constants`'s existing mutual-
  recursion `labels` structure was reviewed and correctly left as-is — it
  is already the right idiom for a tree walker.
- Investigated a `cl-dataflow` (nerima-lisp org) integration for the
  pass-pipeline trace/reporting machinery; found no genuine fit without
  building the "weird adapter" the project's own conventions reject (its
  event/context vocabulary has no analog to this codebase's Chrome-Trace-
  JSON timing fields) — no dependency added, no changes made.

## [0.3.1] - 2026-07-31

### Added

- `it-fuzz` crash-safety check for `optimize-instructions` on short
  generated instruction streams, using `cl-weave`'s own generators
  (`gen-integer`/`gen-list`/`gen-map`/`gen-tuple`) — deliberately narrower
  than the pre-existing, still-blocked FR-753 semantic-equivalence fuzz
  test, and confirmed not to collide with that test's known unresolved bug.
- Test coverage: 7 more behavioral tests (LICM hoisting, PRE compensation,
  GVN commutative merge, idiom recognition, if-conversion, dead-label
  removal, tail-merge), bringing the suite to 138 tests (137 passing, 1
  pre-existing tracked todo), up from 130.

### Changed

- Consolidated 4 more duplicated patterns found by a deeper
  `paredit inspect duplicates`/`similarity` pass: a byte-identical
  known-callee-label-tracking `typecase` shared across 3 files (now
  `%opt-track-known-callee-label` in `optimizer-macros.lisp`), a
  `define-interval-log-transfer` macro for the logand/logior/logxor
  interval-update trio, a `define-cfg-list-replace` macro for the
  successor/predecessor/terminator replacement trio, and one deleted
  function that was identical to an existing shared helper.
- Investigated `cl-weave`'s `:timeout-ms`/`:retry`/`it-concurrent`/CPS
  continuation-helper features against this codebase; none had a genuine
  fit without forcing a mismatch (documented in the corresponding commit),
  so none were force-applied.

### Fixed

- Two backward-compat/rename leftovers: `optimizer-trans-validate.lisp`
  bindings still named `before-legacy`/`after-legacy` after the prior
  round's rename, and a dead duplicate declaration of
  `*opt-enable-sealed-gf-devirtualization*` in
  `optimizer-pipeline-policy.lisp` with no forward-reference reason to
  exist (unlike `*sroa-enabled*`'s documented one).

## [0.3.0] - 2026-07-31

### Added

- FR-668 Scalar Replacement of Aggregates (`opt-pass-sroa`): conservative
  scalar replacement for `vm-make-obj`/`vm-slot-read`/`vm-slot-write`
  structs and `vm-make-array`/`vm-aref`/`vm-aset` fixed-index arrays. Wired
  in as an opt-in `:sroa` pipeline stage (`*sroa-enabled*`, not in the
  default convergence list — same precedent as `:path-profiling`).
- Test coverage: the "every optimizer pass tolerates an empty instruction
  stream" smoke check grew from 19 to 91 passes (every `*opt-pass-table*`
  entry whose signature fits), plus 12 new behavioral tests asserting
  concrete before/after transformations for DCE, CSE, strength reduction,
  dead-store elimination, store-to-load forwarding, block merging,
  unreachable-code removal, copy propagation, devirtualization, SCCP,
  rotate recognition, and an `it-property` soundness check for value-range
  propagation.

### Fixed

- FR-676 TLAB: `tlab-refill` no longer silently hands out address 0 (a
  null pointer) when no real heap is configured; it now requires an
  explicit `:new-chunk-address` from the caller or signals an error.

### Changed

- `flake.nix` bumps three nerima-lisp sibling flake inputs to their latest
  tagged releases, each verified independently with
  `nix build .#checks.aarch64-darwin.default`:
  - `cl-cc-ast`: `v0.1.0` -> `v0.2.0`. This is `cl-cc-optimize`'s only
    direct (non-transitive) bump of the three; `cl-cc-optimize.asd` depends
    on it directly. Upstream's 0.2.0 is an internal CPS/refactor release
    (closure-analysis helpers gained an `&optional (k #'identity)`
    continuation argument, defaulting to direct-style behavior) with no
    call-site changes required here.
  - `cl-concurrent-kit`: `v0.1.0` -> `v0.2.0`. Only a transitive input here
    (pulled in for `cl-log-kit` 2.0.0's dependency graph, not called
    directly by this codebase). Upstream added `promise-then`,
    `promise-race`, `promise-all-settled`, further promise combinators
    (`promise-catch`/`-finally`/`-all`/`-any`/`-timeout`), countdown
    latches/barriers, executor observability/backpressure, and a reactive
    stream layer — all additive, nothing consumed here.
  - `cl-boundary-kit`: `v1.0.0` -> `v2.0.0` (major, breaking upstream
    changes: `make-environment`'s `:set-fn`/`:unset-fn` now default to a
    real `cl-host-kit`-backed implementation instead of `nil`, and the
    optional `cl-boundary-kit/process-kit` and `cl-boundary-kit/json`
    systems were removed). Also only a transitive input; `cl-cc-optimize`
    does not call `cl-boundary-kit` directly, and none of the removed
    exports or changed defaults are referenced anywhere in `src/` or `t/`,
    so no source changes were needed.
  - These are maintenance-only version bumps: no new capability from any of
    the three packages is exercised by `cl-cc-optimize` yet, since none is
    a direct dependency of the optimizer's own code paths (only
    `cl-cc-ast` is a direct dependency, and its 0.2.0 release added no new
    public API surface — only internal refactoring).

## [0.2.0] - 2026-07-31

### Added

- Nerima-lisp org package conformance: string `defsystem` name, pinned
  sibling flake inputs (including the transitive `cl-log-kit` 2.0.0
  dependencies `cl-date-kit`, `cl-concurrent-kit`, `cl-host-kit`),
  `checks.docs`, `.github/actions/nix-setup`, the four standard workflows,
  and this `docs/` site.
- `:verify-ir` pipeline stage: FR-642 IR/VM invariant verification
  (`opt-pass-verify-ir`) was implemented but never reachable from the
  optimizer pipeline. It is now wired in as the pipeline's final stage,
  gated by `*verify-ir*` (default `nil`, so it costs nothing unless a
  frontend opts in). `*verify-ir*` and `opt-verify-ir` are now exported.

### Changed

- Upgraded sibling dependencies to their latest release tags: `cl-weave`
  1.0.0 → 1.1.0, `cl-log-kit` 1.0.0 → 2.0.0, `cl-process-kit` 1.0.1 → 2.0.0,
  `cl-json-kit` 1.0.0 → 1.0.1, `cl-parser-kit` 1.0.1 → 1.0.2, `cl-host-kit`
  0.2.0 → 0.2.1, and pinned the previously-untagged `cl-cc-ast`, `cl-cc-type`,
  `cl-cc-runtime`, `cl-prolog`, `cl-parser-kit`, and `cl-boundary-kit` inputs
  to their release tags (`cl-cc-bootstrap` has no tags yet, so it is pinned
  to a commit SHA instead).
- `optimizer-driver.lisp`'s per-pass `opt-verify-instructions` now delegates
  to `optimizer-verify-ir.lisp`'s `opt-verify-vm-instructions` instead of
  re-implementing the same duplicate/unknown-jump-target/use-before-define
  checks; it also gains that function's basic-block-terminator check for
  free. Public API (name, signature, export) unchanged.
- `cl-weave`'s flake input is now `flake = false`: this repository only ever
  used it as a source tree, so pulling in cl-weave's own flake graph
  (`cl-nix-forge`, `treefmt-nix`, and their nixpkgs) only bloated
  `flake.lock`.
- Split oversized source files (`cfg.lisp`, `effects.lisp`,
  `optimizer-tables.lisp`, `optimizer-scheduler.lisp`, `optimizer-driver.lisp`,
  `optimizer-recognition.lisp`, `optimizer-vectorize.lisp`,
  `optimizer-speculative-passes.lisp`, `optimizer.lisp`, and
  `optimizer-flow-loop.lisp`) into topic-cohesive files, following the
  project's existing data/logic separation convention (`*-data.lisp` pairs).
  Biggest splits: `optimizer-inline.lisp` → `optimizer-inline-cost-data.lisp` /
  `optimizer-inline-devirt-data.lisp` / `optimizer-inline-devirt.lisp` /
  `optimizer-inline-expand.lisp` / `optimizer-inline-heuristics.lisp` /
  `optimizer-inline-call-site-split.lisp`; `optimizer-flow-loop.lisp` →
  `optimizer-flow-loop-unroll.lisp` / `optimizer-flow-loop-prefetch.lisp` /
  `optimizer-flow-loop-sink.lisp` / `optimizer-flow-type-check-elim.lisp`;
  `optimizer-memory-interval.lisp` → `optimizer-interval-arithmetic.lisp` /
  `optimizer-interval-transfer.lisp` / `optimizer-interval-data.lisp`;
  `cfg.lisp` → `cfg-dominance.lisp` / `cfg-ddg.lisp`; `effects.lisp` →
  `effects-data.lisp` / `effects-known-functions.lisp` /
  `effects-purity-inference.lisp`.
- New `optimizer-macros.lisp` holds `define-inst-type-predicate`,
  consolidating a repeated instruction-type-predicate definition pattern
  used across 17 files.

### Removed

- Dead code: 23 unreferenced functions/variables across 19 files, found by
  cross-referencing `paredit inspect unused-definitions` against
  `package.lisp`'s export list and every `(intern (format nil "OPT-PASS-~A"
  ...))`-style dynamic pass dispatch site, so nothing reachable only through
  the optional-pass registration macro was removed. Includes a fully
  no-op `emit-cpu-dispatch` stub and three orphaned constant-folding/loop
  policy variables (`*opt-foldable-binary-types*`, `*opt-foldable-unary-types*`,
  `*opt-enable-loop-unswitch*`) that nothing ever read.
- `opt-compute-path-profile`'s dual block-label/block-id `:counts` lookup
  ("for compatibility with older callers", with zero in-repo callers) is
  now block-label-only, its one canonical key format.

## [0.1.0] - 2026-07-30

### Added

- Initial release: the optimizer subsystem extracted from the `cl-cc`
  monorepo, covering CFG/SSA construction, E-graph equality saturation, and
  the multi-pass optimization pipeline (constant folding, inlining,
  devirtualization, memory/alias analysis, loop transforms, vectorization,
  and speculative optimization).
- `t/` asserts the extraction's core invariant directly: no source file
  under `src/` names a `cl-cc/vm::` internal symbol, since an external
  repository cannot reach another package's internal symbols.
- FMA fusion now preserves the operands' float precision (`f32`/`f64`)
  instead of assuming `f64`, and SLP vectorization recognizes two-lane
  `f64` array maps.
- Translation validation's symbolic executor treats unsupported opcodes,
  streams beyond its symbolic step bound, and duplicate identical unknown
  instructions as non-equivalent rather than assuming equivalence.
- A runtime memoizer and tier/PGO handoff record for pure functions crossing
  from the baseline to the optimized tier.
- TBAA (type-based alias analysis) metadata is threaded into both the
  instruction scheduler and dead store elimination.

### Fixed

- `opt-analyze-branch-weights` returns the cold-label table it actually
  analyzed, rather than a stale one from a prior call.
- The optimizer roadmap evidence registered against this system is now
  validated for completeness at registration time instead of silently
  accepting partial evidence.
