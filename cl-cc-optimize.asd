;;;; This form comes FIRST, before any defsystem. ASDF binds *package* to
;;;; ASDF-USER only for a file it loads itself; read any other way — a REPL
;;;; `load`, an editor evaluating the buffer, flake.nix parsing :version — the
;;;; file is read in whatever package happens to be current.
(in-package #:asdf-user)

(asdf:defsystem "cl-cc-optimize"
  :description "Optimizer subsystem: CFG, SSA, E-graph, peephole, pipeline"
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.5.2"
  :homepage "https://github.com/nerima-lisp/cl-cc-optimize"
  :bug-tracker "https://github.com/nerima-lisp/cl-cc-optimize/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-cc-optimize.git")
  :depends-on (:cl-cc-vm :cl-prolog-kit :cl-cc-type :cl-cc-ast :cl-parser-kit :cl-host-kit)
  :pathname "src"
  :serial t
  :components
  (
   (:module "package-analysis"
    :pathname "analysis"
    :serial t
    :components ((:file "package")))
   (:module "package-reporting"
    :pathname "reporting"
    :serial t
    :components ((:file "package")))
   (:module "package-partial-evaluation"
    :pathname "partial-evaluation"
    :serial t
    :components ((:file "package")))
   (:module "package-rewriting"
    :pathname "rewriting"
    :serial t
    :components ((:file "package")))
   (:module "package-verification"
    :pathname "verification"
    :serial t
    :components ((:file "package")))
   (:module "package-scalar"
    :pathname "scalar"
    :serial t
    :components ((:file "package")))
   (:module "package-control-flow"
    :pathname "control-flow"
    :serial t
    :components ((:file "package")))
   (:module "package-loops"
    :pathname "loops"
    :serial t
    :components ((:file "package")))
   (:module "package-memory"
    :pathname "memory"
    :serial t
    :components ((:file "package")))
   (:module "package-inlining"
    :pathname "inlining"
    :serial t
    :components ((:file "package")))
   (:module "package-speculation"
    :pathname "speculation"
    :serial t
    :components ((:file "package")))
   (:module "package-pipeline"
    :pathname "pipeline"
    :serial t
    :components ((:file "package")))
   (:file "facade-package")
   (:module "implementation-analysis"
    :pathname "analysis"
    :serial t
    :components
    ((:file "macros")
     (:file "bounds-check-metadata")
     (:file "function-collection")
     (:file "register-primitives")
     (:file "scheduler-primitives")
     (:file "stable-print")
     (:file "tree-walk")
     (:file "effects-data")
     (:file "effects")
     (:file "effects-known-functions")
     (:file "effects-purity-inference")
     (:file "cfg")
     (:file "cfg-dominance")
     (:file "cfg-ddg")
     (:file "cfg-analysis")
     (:file "cfg-layout")
     (:file "ssa")
     (:file "ssa-phi-elim")
     (:file "ssa-construction")
     (:file "tables")
     (:file "tables-fold-data")
     (:file "folding")
     (:file "tables-regs")
     (:file "purity")
     (:file "dataflow")
     (:file "dataflow-sccp")
     (:file "memory-alias")
     (:file "interval-arithmetic")
     (:file "interval-data")
     (:file "interval-transfer")
     (:file "value-ranges")
     (:file "memory-alias-basic")
     (:file "register-rewrite")
     (:file "demand")
     (:file "abstract-interp")
     (:file "vrp")
     (:file "static-analysis-135")))
   (:module "implementation-reporting"
    :pathname "reporting"
    :serial t
    :components
    ((:file "report")
     (:file "path-profile")
     (:file "remarks")))
   (:module "implementation-partial-evaluation"
    :pathname "partial-evaluation"
    :serial t
    :components
    ((:file "lattice")
     (:file "peval-result-data")
     (:file "peval-specialize")
     (:file "peval-binding-time")
     (:file "peval-specialization-plan")
     (:file "peval-program")))
   (:module "implementation-rewriting"
    :pathname "rewriting"
    :serial t
    :components
    ((:file "egraph")
     (:file "egraph-match")
     (:file "egraph-saturation")
     (:file "peephole-data")
     (:file "egraph-rules")
     (:file "egraph-rules-advanced")
     (:file "prolog-peephole")
     (:file "superopt")
     (:file "cps-reduce")
     (:file "delimited-cont")
     (:file "ghc-transforms-136")
     (:file "lisp-opt-140")))
   (:module "implementation-verification"
    :pathname "verification"
    :serial t
    :components
    ((:file "trans-validate")
     (:file "verify-ir")))
   (:module "implementation-scalar"
    :pathname "scalar"
    :serial t
    :components
    ((:file "algebraic")
     (:file "copyprop")
     (:file "strength-data")
     (:file "strength")
     (:file "recognition")
     (:file "strength-ext")
     (:file "cse-gvn")
     (:file "fold-pass")
     (:file "fma")
     (:file "div-const")
     (:file "overflow")
     (:file "bitwidth")
     (:file "energy")))
   (:module "implementation-control-flow"
    :pathname "control-flow"
    :serial t
    :components
    ((:file "flow-core")
     (:file "flow-loop-sink")
     (:file "flow-type-check-elim")
     (:file "flow-if-conversion")
     (:file "flow-branch-correlation")
     (:file "flow-block-merge")
     (:file "flow-tail-merge")
     (:file "flow-tail-duplication")
     (:file "flow-dce")
     (:file "jump-threading")
     (:file "branch-weights")))
   (:module "implementation-loops"
    :pathname "loops"
    :serial t
    :components
    ((:file "canonical-loop-data")
     (:file "induction")
     (:file "loop-unroll-flow")
     (:file "recognition-loop-idiom")
     (:file "trmc")
     (:file "licm")
     (:file "pre")
     (:file "sequence-fusion")
     (:file "vectorize")
     (:file "vectorize-slp")
     (:file "swp")
     (:file "loop-peel")
     (:file "scheduler-data")
     (:file "scheduler")
     (:file "scheduler-pre-ra")
     (:file "loop-rotate")
     (:file "dead-loop")
     (:file "loop-unroll")
     (:file "loop-unswitch")
     (:file "strength-reduction")
     (:file "idiom")
     (:file "loop-fusion")
     (:file "loop-tile")
     (:file "polyhedral")))
   (:module "implementation-memory"
    :pathname "memory"
    :serial t
    :components
    ((:file "closure")
     (:file "memory-ssa")
     (:file "memory-access-patterns")
     (:file "memory-safepoints")
     (:file "memory-dse")
     (:file "memory-forward")
     (:file "memory-sink")
     (:file "memory-prefetch")
     (:file "bce")
     (:file "escape")
     (:file "load-store-coalesce")
     (:file "sroa")
     (:file "tlab")))
   (:module "implementation-inlining"
    :pathname "inlining"
    :serial t
    :components
    ((:file "pure-call-optimization")
     (:file "register-rename")
     (:file "inline-devirt-data")
     (:file "inline-call-site-split")
     (:file "inline-devirt")
     (:file "inline-pass")
     (:file "inline-cost-data")
     (:file "inline-heuristics")
     (:file "inline-expand")
     (:file "outlining")
     (:file "devirt")
     (:file "dae")
     (:file "ipcp")
     (:file "defunctionalize")
     (:file "mlgo")
     (:file "speculative-ml-cost-plan")))
   (:module "implementation-speculation"
    :pathname "speculation"
    :serial t
    :components
    ((:file "speculative-inline")
     (:file "speculative-ic-data")
     (:file "speculative-ic")
     (:file "speculative-profile")
     (:file "speculative-shape-data")
     (:file "speculative-shape")
     (:file "speculative-concurrency-data")
     (:file "speculative-concurrency")
     (:file "speculative-security")
     (:file "speculative-wasm")
     (:file "speculative-debug")
     (:file "speculative-atomics-data")
     (:file "speculative-atomics")
     (:file "speculative-peval-data")
     (:file "speculative-passes-data")
     (:file "speculative-peval-known-args")
     (:file "speculative-passes")))
   (:module "implementation-pipeline"
    :pathname "pipeline"
    :serial t
    :components
    ((:file "pipeline-policy")
     (:file "pipeline")
     (:file "driver-pipeline-spec")
     (:file "driver-trace")
     (:file "driver")
     (:file "cpu-dispatch")
     (:file "autotune")
     (:file "fuzz")))
   )
  :in-order-to ((test-op (test-op "cl-cc-optimize/test"))))

(asdf:defsystem "cl-cc-optimize/test"
  :description "Module boundary tests for cl-cc-optimize"
  :author "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :depends-on ("cl-cc-optimize" "cl-weave")
  :pathname "t"
  :serial t
  :components (
   (:file "package")
   (:file "optimize-boundary-test")
   (:module "analysis"
    :pathname "analysis"
    :serial t
    :components ((:file "package")))
   (:module "scalar"
    :pathname "scalar"
    :serial t
    :components ((:file "package")))
   (:module "control-flow"
    :pathname "control-flow"
    :serial t
    :components ((:file "package")))
   (:module "loops"
    :pathname "loops"
    :serial t
    :components ((:file "package")))
   (:module "memory"
    :pathname "memory"
    :serial t
    :components ((:file "package")))
   (:module "inlining"
    :pathname "inlining"
    :serial t
    :components ((:file "package")))
   (:module "partial-evaluation"
    :pathname "partial-evaluation"
    :serial t
    :components ((:file "package")))
   (:module "speculation"
    :pathname "speculation"
    :serial t
    :components ((:file "package")))
   (:module "rewriting"
    :pathname "rewriting"
    :serial t
    :components ((:file "package")))
   (:module "pipeline"
    :pathname "pipeline"
    :serial t
    :components ((:file "package")))
   (:module "reporting"
    :pathname "reporting"
    :serial t
    :components ((:file "package")))
   (:module "verification"
    :pathname "verification"
    :serial t
    :components ((:file "package")))
   (:file "optimizer-roadmap-evidence-data")
   (:file "optimizer-roadmap-doc-parse")
   (:file "optimizer-roadmap-query")
   (:file "optimizer-roadmap-evidence-check")
   (:file "optimizer-roadmap-backend-data-foundation")
   (:file "optimizer-roadmap-backend-data-baseline")
   (:file "optimizer-roadmap-backend-data-memory-numeric")
   (:file "optimizer-roadmap-backend-data-lowering")
   (:file "optimizer-roadmap-backend-data-codegen")
   (:file "optimizer-roadmap-backend-data-speculative")
   (:file "optimizer-roadmap-backend-data")
   (:file "optimizer-roadmap-backend")
  )
  :perform (asdf:test-op
    (op system)
    (declare (ignore op system))
    ;; Not HOST-KIT:SYMBOL-CALL: a .asd is read before :depends-on is ever
    ;; consulted, so a CL-HOST-KIT-prefixed token here would be a read-time
    ;; PACKAGE-DOES-NOT-EXIST error regardless of what the system depends
    ;; on. FIND-SYMBOL/FIND-PACKAGE/FUNCALL are CL, always present.
    (unless (funcall (find-symbol "RUN-ALL" (find-package "CL-WEAVE"))
                     :reporter :spec :pass-with-no-tests nil)
      (error "cl-cc-optimize tests failed"))))
