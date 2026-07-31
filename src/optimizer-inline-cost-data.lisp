;;;; optimizer-inline-cost-data.lisp — inlining cost model configuration
;;;;
;;;; Tunables read by optimizer-inline-heuristics.lisp's cost/profile scoring
;;;; and optimizer-inline-expand.lisp's pass: threshold scale, the size cap,
;;;; whether the ML-learned inline score is enabled, and which model version
;;;; and cost target it targets.

(in-package :cl-cc/optimize)

(defparameter *opt-inline-threshold-scale* 1
  "PGO-guided multiplier for adaptive inline thresholds.
1 means no change; values >1 make inlining more aggressive for hot profiles.")

(defvar *block-compile* nil
  "When non-NIL, optimize a source file as one block-compilation unit.
This permits module-local function bodies with lexical captures to be inlined at
known direct call sites, while recursive callees remain protected by the normal
call-graph guard.")

(defparameter *max-inline-size* 30
  "Maximum raw instruction count for automatic call-graph based inlining.
The count excludes the final vm-ret.  This cap is deliberately independent of
the adaptive cost threshold so LTO inlining stays bounded across modules.")

(defparameter *opt-enable-ml-inline-score* t
  "When non-NIL, adaptive inlining also consults ML-guided score helpers.")

(defparameter *opt-inline-ml-model-version* "mlgo-v2"
  "Model version tag passed to `opt-ml-inline-score-plan`.")

(defparameter *opt-learned-cost-target* :generic
  "Target architecture hint for learned inline cost adjustment.")
