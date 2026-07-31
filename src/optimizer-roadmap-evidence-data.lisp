;;;; optimizer-roadmap-evidence-data.lisp — roadmap evidence records
;;;;
;;;; The two evidence record types (optimizer and backend roadmaps) and
;;;; their global registries: a feature-ID keyed table of which module,
;;;; test anchors, and API symbols back a roadmap doc's claim that a
;;;; feature is implemented. optimizer-roadmap-evidence-check.lisp verifies
;;;; a registered record still holds; optimizer-roadmap-query.lisp is the
;;;; public read API.

(in-package :cl-cc/optimize)

;;; ─── Optimize roadmap implementation evidence and speculative helpers ─────

(defstruct (opt-roadmap-feature (:conc-name opt-roadmap-feature-))
  "One FR heading parsed from docs/notes/optimize-passes.md."
  (id "" :type string)
  (title "" :type string)
  (line 0 :type integer)
  (status :unknown :type keyword)
  (marked-complete-p nil :type boolean))

(defstruct (opt-roadmap-evidence (:conc-name opt-roadmap-evidence-))
  "Concrete implementation evidence for one optimize roadmap FR."
  (feature-id "" :type string)
  (status :tracked :type keyword)
  (modules nil :type list)
  (api-symbols nil :type list)
  (test-anchors nil :type list)
  (summary "" :type string))

(defvar *opt-roadmap-evidence-registry* (make-hash-table :test #'equal)
  "Maps docs/notes/optimize-passes.md FR ids to implementation evidence.")

(defvar *opt-backend-roadmap-evidence-registry* (make-hash-table :test #'equal)
  "Maps docs/notes/optimize-backend.md FR ids to implementation evidence.")

(defparameter *opt-roadmap-extracted-package-systems*
  '(("packages/ast/"     . :cl-cc-ast)
    ("packages/type/"    . :cl-cc-type)
    ("packages/binary/"  . :cl-cc-binary)
    ("packages/runtime/" . :cl-cc-runtime)
    ("packages/vm/"      . :cl-cc-vm)
    ("packages/bootstrap/" . :cl-cc-bootstrap)
    ("packages/ir/"      . :cl-cc-ir)
    ("packages/mir/"     . :cl-cc-mir)
    ("packages/target/"  . :cl-cc-target)
    ("packages/bytecode/" . :cl-cc-bytecode)
    ;; This system too, now that it is one. Roadmap evidence naming
    ;; packages/optimize/src/... is read from cl-cc, where that directory no
    ;; longer exists -- the sources are here.
    ("packages/optimize/" . :cl-cc-optimize)
    ;; The native backend is one repository holding three systems, each with
    ;; its own .asd, so each prefix resolves against its own system rather than
    ;; against a single cl-cc-codegen-native root.
    ("packages/regalloc/" . :cl-cc-regalloc)
    ("packages/codegen/"  . :cl-cc-codegen)
    ("packages/emit/"     . :cl-cc-emit))
  "Roadmap evidence prefixes for packages that now live in their own repository.

Evidence strings still name the logical module -- packages/type/src/inference.lisp
is where that code belonged when the entry was written, and renaming every entry
would lose the continuity the roadmap is for. The source moved to a standalone
repository, so the prefix is stripped and the remainder resolved against that
system instead of this checkout. The check stays a real file-existence check
rather than being weakened to a pattern match.")

(defparameter +opt-roadmap-evidence-profile-ranges+
  ;; Each entry: (lo hi modules api-symbols test-anchors)
  ;; lo/hi are inclusive bounds; NIL lo/hi means exact match (eql) or open-ended.
  ;; Ordering matters: first matching entry wins (most-specific first).
  `((23 23
     ("packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (opt-ic-transition opt-profile-record-edge
      opt-profile-record-value)
     (optimizer-roadmap-pic-evidence-is-runtime-backed))
    (38 38
     ("packages/optimize/src/optimizer-value-ranges.lisp"
      "packages/optimize/src/optimizer-memory-interval.lisp"
      "packages/optimize/tests/optimizer-memory-tests.lisp")
     (opt-compute-path-sensitive-ranges
      opt-block-reg-range
      opt-interval-widen)
     (path-sensitive-ranges-narrow-jump-target-branch-from-lt
      path-sensitive-ranges-narrow-fallthrough-branch-from-lt
      path-sensitive-ranges-join-unions-narrowed-predecessors
      path-sensitive-ranges-expose-block-local-query-api
      interval-widen-expands-moving-bound-to-sentinel
      path-sensitive-ranges-widen-loop-header-and-converge))
    (329 329
     ("packages/codegen/src/x86-64-codegen-core.lisp"
      "packages/codegen/src/x86-64-codegen-dispatch.lisp"
      "packages/regalloc/src/regalloc.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (("CL-CC/CODEGEN" . "X86-64-USED-CALLEE-SAVED-REGS")
      ("CL-CC/REGALLOC" . "COMPUTE-LIVE-INTERVALS"))
     (optimizer-roadmap-callee-saved-evidence-is-native-backed))
    (34 34
     ("packages/optimize/src/optimizer-flow-passes.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/codegen/src/x86-64-emit-ops-bits.lisp"
      "packages/codegen/src/aarch64-emitters.lisp"
      "packages/optimize/tests/optimizer-flow-tests.lisp"
      "packages/emit/tests/x86-64-codegen-insn-tests.lisp"
      "packages/emit/tests/aarch64-codegen-tests.lisp")
     (opt-pass-if-conversion
      ("CL-CC/CODEGEN" . "EMIT-VM-SELECT")
      ("CL-CC/CODEGEN" . "EMIT-A64-VM-SELECT"))
     (if-conversion-simple-diamond-emits-vm-select
      if-conversion-skips-externally-referenced-diamond-label
      x86-64-select-emitter-encoding
      aarch64-select-emitter-encoding))
    (1 22
     ("packages/optimize/src/optimizer.lisp"
      "packages/optimize/src/optimizer-licm.lisp"
      "packages/optimize/src/optimizer-pre.lisp"
      "packages/optimize/src/optimizer-dataflow.lisp"
      "packages/optimize/src/optimizer-induction.lisp"
      "packages/optimize/tests/optimizer-tests.lisp"
      "packages/optimize/tests/optimizer-memory-tests.lisp")
     (opt-pass-fold opt-pass-licm opt-pass-pre opt-pass-sccp
      opt-compute-simple-inductions opt-induction-trip-count)
     (optimizer-roadmap-core-passes-have-evidence))
    (24 56
     ("packages/optimize/src/optimizer-inline.lisp"
      "packages/optimize/src/optimizer-inline-pass.lisp"
      "packages/optimize/src/optimizer-value-ranges.lisp"
      "packages/optimize/src/optimizer-recognition.lisp"
      "packages/optimize/tests/optimizer-inline-tests.lisp"
      "packages/optimize/tests/optimizer-strength-tests.lisp")
     (opt-known-callee-labels opt-pass-call-site-splitting
      opt-pass-devirtualize opt-pass-global-dce
      opt-array-bounds-check-eliminable-p opt-pass-fill-recognition)
     (optimizer-roadmap-inline-and-memory-evidence
      ;; FR-037 call-site-splitting specific tests
      opt-pass-call-site-splitting-duplicates-known-predecessor-call
      opt-pass-call-site-splitting-noops-without-known-callee
      opt-pass-call-site-splitting-handles-multi-join-labels
      opt-pass-call-site-splitting-handles-vm-apply
      opt-pass-call-site-splitting-handles-vm-tail-call))
    (115 115
     ("packages/optimize/src/optimizer-memory-alias.lisp"
      "packages/optimize/src/optimizer-licm.lisp"
      "packages/optimize/src/optimizer-flow-loop.lisp"
      "packages/optimize/tests/optimizer-licm-tests.lisp"
      "packages/optimize/tests/optimizer-flow-tests.lisp")
     (opt-compute-heap-aliases opt-must-alias-p opt-may-alias-p
      opt-compute-heap-type-facts opt-tbaa-must-not-alias-p
      opt-memory-accesses-may-alias-p opt-pass-licm opt-pass-code-sinking)
     (licm-does-not-hoist-slot-read-across-aliased-slot-write
      licm-hoists-slot-read-across-tbaa-disjoint-slot-write
      licm-unknown-call-invalidates-slot-read-hoist
      code-sinking-does-not-sink-slot-read-across-aliased-write
      code-sinking-sinks-slot-read-across-tbaa-disjoint-write))
    (74 118
     ("packages/optimize/src/optimizer-flow-core.lisp"
      "packages/optimize/src/optimizer-flow-passes.lisp"
      "packages/optimize/src/optimizer-flow-loop.lisp"
      "packages/optimize/src/optimizer-closure.lisp"
      "packages/optimize/src/optimizer-strength.lisp"
      "packages/optimize/src/cfg.lisp"
      "packages/optimize/src/ssa.lisp"
      "packages/optimize/tests/optimizer-flow-tests.lisp")
     (opt-pass-loop-rotation opt-pass-loop-peel
      opt-pass-loop-unrolling opt-pass-branch-correlation
      opt-pass-tail-duplication cfg-split-critical-edges
      ssa-eliminate-trivial-phis
      opt-pass-closure-capture-dedup opt-pass-closure-thunk-sharing)
     (optimizer-roadmap-flow-and-ssa-evidence
      ;; FR-022 loop-unrolling specific tests (names match optimizer-flow-tests.lisp)
      loop-unrolling-fully-unrolls-small-counted-loop
      loop-unrolling-non-lt-comparisons
      loop-unrolling-partially-unrolls-when-trip-count-too-large
      loop-unrolling-partially-unrolls-unknown-trip-with-remainder
      loop-unrolling-partial-keeps-remainder-loop
      ;; FR-079 closure-thunk-sharing specific tests
      closure-thunk-sharing-deduplicates-safe-siblings
      closure-thunk-sharing-noops-on-register-overwrite
      closure-thunk-sharing-preserves-different-capture
      closure-thunk-sharing-noops-on-env-reg-write
      closure-thunk-sharing-noops-across-cfg-boundary
      ;; FR-080 cons-slot-forward tests (covered here)
      closure-capture-dedup-shares-duplicate-environments
      closure-capture-dedup-preserves-non-shareable
      closure-capture-dedup-noops-across-cfg-boundary))
    (148 170
     ("packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/src/optimizer-flow-core.lisp"
      "packages/optimize/src/optimizer-flow-passes.lisp"
      "packages/optimize/src/optimizer-flow-loop.lisp"
      "packages/optimize/src/optimizer-inline-cost.lisp"
      "packages/optimize/tests/optimizer-flow-tests.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp"
      "packages/optimize/tests/optimizer-inline-tests.lisp"
      "packages/optimize/tests/optimizer-inline-pass-tests-2.lisp"
      "packages/optimize/tests/optimizer-strength-inline-tests.lisp")
     (opt-adaptive-inline-threshold opt-adaptive-max-iterations
      opt-pass-code-sinking opt-pass-tail-duplication
      opt-pass-branch-correlation)
     (optimizer-roadmap-code-motion-evidence
      ;; FR-150 adaptive-inline-threshold tests
      opt-adaptive-inline-threshold-uses-profile-and-size-hints
      opt-adaptive-inline-threshold-cases
      opt-adaptive-inline-threshold-respects-pgo-scale
      opt-adaptive-inline-threshold-ml-bonus-is-applied
      ;; FR-163 code-sinking specific tests
      code-sinking-moves-const-into-target-block
      code-sinking-noop-when-value-is-read-multiple-times
      code-sinking-moves-cons-into-target-block
      code-sinking-does-not-sink-impure-random
      code-sinking-duplicates-cheap-const-into-conditional-targets
      code-sinking-noop-for-cons-read-multiple-times
      code-sinking-moves-arithmetic-and-move-into-target-block))
    (287 287
     ("packages/optimize/src/optimizer-licm.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-licm-tests.lisp"
      "packages/optimize/tests/optimizer-cfg-inline-tests.lisp"
      "packages/optimize/tests/optimizer-dataflow-passes-tests.lisp")
     (opt-pass-licm opt-inst-loop-invariant-p opt-licm-emit-with-preheaders)
     (constant-hoist-moves-loop-constant-to-preheader
      licm-does-not-hoist-loop-defined-value
      licm-pass-returns-straight-line-code-unchanged
      licm-collect-invariants-finds-pure-const))
    (261 261
     ("packages/optimize/src/optimizer-speculative-ic.lisp"
      "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
     (make-opt-profile-data opt-profile-record-value
      opt-profile-top-values opt-profile-value-range)
     (optimizer-roadmap-value-profiling-top-k-and-range-behavior))
    (283 283
     ("packages/optimize/src/optimizer-speculative-ic.lisp"
      "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
     (make-opt-speculation-log opt-record-speculation-failure
      opt-speculation-failed-p opt-speculation-allowed-p
      opt-clear-speculation-log opt-save-speculation-log
      opt-load-speculation-log)
     (optimizer-roadmap-speculation-log-gating-and-persistence-behavior))
    (271 271
     ("packages/optimize/src/ssa-phi-elim.lisp"
      "packages/optimize/src/ssa.lisp"
      "packages/optimize/src/ssa-construction.lisp"
      "packages/optimize/tests/ssa-tests.lisp")
     (ssa-eliminate-trivial-phis)
     (optimizer-roadmap-ssa-phi-elim-evidence
      ssa-phi-elim-all-same-arg-multi-pred
      ssa-phi-elim-phi-of-phi-chain-deep
      ssa-phi-elim-unused-phi
      ssa-phi-elim-idempotent
      ssa-trivial-phi-elimination-rewrites-uses
      ssa-trivial-phi-elimination-shortcuts-phi-of-phi-chain
      ssa-trivial-phi-elimination-runs-all-passes-together))
    (223 305
     ("packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (opt-ic-transition opt-record-speculation-failure
      opt-profile-record-edge opt-profile-record-value
      opt-profile-record-call-chain opt-profile-record-allocation
      opt-guard-record opt-jit-cache-select-eviction
      opt-adaptive-compilation-threshold)
     (optimize-roadmap-runtime-helpers-have-concrete-behavior))
    (326 463
     ("packages/optimize/src/optimizer-pipeline.lisp"
      "packages/cps/src/cps-ast.lisp"
      "packages/vm/src/vm-run.lisp"
      "packages/vm/src/vm-dispatch.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (opt-materialize-deopt-state opt-shape-slot-offset
       opt-stack-map-live-root-p opt-merge-module-summaries
       opt-sea-node-schedulable-p)
      (optimize-roadmap-support-helpers-have-conservative-behavior))
    (547 547
     ("packages/optimize/src/effects.lisp"
      "packages/mir/src/mir.lisp"
      "packages/emit/tests/mir-tests.lisp")
     (("CL-CC/MIR" . "MIR-INST-EFFECT-KIND")
      ("CL-CC/MIR" . "MIR-INST-PURE-P")
      ("CL-CC/MIR" . "MIR-INST-DCE-ELIGIBLE-P"))
     (mir-op-effect-kind-classifies-core-ops
      mir-inst-effect-kind-allows-meta-override
      mir-inst-effect-kind-rejects-malformed-meta))
    (548 548
     ("packages/mir/src/mir.lisp"
      "packages/emit/tests/mir-tests.lisp"
      "packages/type/src/checker.lisp"
      "packages/type/src/inference.lisp")
     (("CL-CC/MIR" . "MIR-PROPAGATE-TYPES")
      ("CL-CC/MIR" . "MIR-INFER-INST-TYPE")
      ("CL-CC/MIR" . "MIR-JOIN-TYPES"))
     (mir-propagate-types-updates-instructions-and-values
      mir-propagate-types-joins-phi-input-types-conservatively
      mir-propagate-types-reaches-fixed-point-for-late-producers))
    (530 nil
     ("packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/src/ssa.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (opt-lattice-meet opt-function-summary-safe-to-inline-p
      opt-stack-map-live-root-p opt-materialize-deopt-state
      opt-shape-slot-offset)
     (optimize-roadmap-support-helpers-have-conservative-behavior)))
  "Alist of (lo hi modules api-symbols test-anchors) range entries for
`%opt-roadmap-evidence-profile'.  Entries are checked in order; NIL hi means
open-ended (>= lo).  The default fallback is handled by the function itself.")
