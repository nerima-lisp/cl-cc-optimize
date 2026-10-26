(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer Driver — Convergence, Policy, and Public Entry Point
;;;
;;; Reporting/trace-state structs and emission are in optimizer-driver-trace.lisp;
;;; pass-pipeline string parsing is in optimizer-driver-pipeline-spec.lisp
;;; (both load before this file).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── Dynamic Variables ───────────────────────────────────────────────────

(defvar *skip-optimizer-passes* nil
  "When non-NIL, optimize-instructions returns its input unchanged.")

(defvar *opt-bisect-limit* nil
  "Maximum number of optimization pass invocations allowed to change the instruction stream.
NIL disables optimization bisection.")

(defvar *opt-bisect-count* 0
  "Number of optimization pass invocations that changed the instruction
   stream in the current dynamic scope.")

(defvar *verify-optimizer-instructions* nil
  "When non-NIL, run opt-verify-instructions after every convergence pass to
catch ill-formed sequences (duplicate labels, unknown jump targets, use-before-define).")

;;; ─── Convergence and Verification ────────────────────────────────────────

(defun opt-converged-p (prev next)
  "T if a pass-cycle produced no semantic change in the instruction stream.
Fast-path: pointer equality (most passes return EQ input when nothing changed)
short-circuits before the expensive sexp comparison."
  (or (eq prev next)
      (and (= (length prev) (length next))
           (loop for lhs in prev
                 for rhs in next
                 always (or (eq lhs rhs)
                            (equal (instruction->sexp lhs)
                                   (instruction->sexp rhs)))))))

(defparameter *opt-iteration-budget-thresholds*
  '((50  . -12)
    (150 . -6)
    (400 . 0)
    (800 . 8))
  "Instruction-count thresholds used by opt-adaptive-max-iterations.")

(defun opt-adaptive-max-iterations
    (instructions &key (base-iterations 20) (min-iterations 6) (max-iterations 50))
  "Return a conservative adaptive convergence budget for INSTRUCTIONS."
  (let* ((n (length instructions))
         (delta (or (cdr (find-if (lambda (entry) (< n (car entry)))
                                  *opt-iteration-budget-thresholds*))
                    15)))
    (min max-iterations
         (max min-iterations
              (+ base-iterations delta)))))

(defun opt-verify-instructions (instructions &key pass-name)
  "Conservative VM-level verifier for optimizer/debugging use.

Delegates to OPT-VERIFY-VM-INSTRUCTIONS (optimizer-verify-ir.lisp), the more
complete VM-stream checker: duplicate labels, unknown jump targets, and
use-before-define registers, plus block-terminator well-formedness."
  (opt-verify-vm-instructions instructions :pass-name (or pass-name "<unknown-pass>")))

;;; ─── Bisection Helpers ───────────────────────────────────────────────────

(defun %opt-bisect-limit-reached-p ()
  "Return T when optimization bisection should skip further pass invocations."
  (and (integerp *opt-bisect-limit*)
       (<= 0 *opt-bisect-limit*)
       (>= *opt-bisect-count* *opt-bisect-limit*)))

(defun %opt-note-bisect-change (changed)
  "Record one changed optimization pass for the bisection counter."
  (when (and changed (integerp *opt-bisect-limit*) (<= 0 *opt-bisect-limit*))
    (incf *opt-bisect-count*)))

(defun %opt-run-passes-once (prog passes reporting trace)
  "Apply PASSES once, mutating TRACE in place and emitting via REPORTING.
Returns the final instruction list."
  (let ((current prog))
    (dolist (f passes current)
      (when (%opt-bisect-limit-reached-p)
        (return current))
      (let* ((before    current)
             (start     (get-internal-real-time))
             (next      (funcall f current))
             (elapsed-s (/ (- (get-internal-real-time) start)
                           internal-time-units-per-second))
             (dur-us    (round (* elapsed-s 1000000)))
             (changed   (not (opt-converged-p before next))))
        (%opt-emit-pass-report f before next elapsed-s dur-us reporting trace)
        (when (and (boundp '*translation-validation-enabled*)
                    *translation-validation-enabled*
                    (fboundp 'validate-optimizer-translation))
          (validate-optimizer-translation f before next))
        (%opt-note-bisect-change changed)
        (setf current next)))))

;;; ─── Optimization Policy ─────────────────────────────────────────────────

(defun opt-configure-optimization-policy (&key speed)
  "Configure optimizer feature gates from a coarse optimization SPEED level.

Current policy:
- SPEED >= 2: enable sealed+satiated generic-function devirtualization
- SPEED >= 3: enable pure-call optimization gate
- SPEED <= 2: disable pure-call optimization gate

Returns the resulting gate value for convenience."
  (when speed
    (setf *opt-enable-sealed-gf-devirtualization* (>= speed 2))
    (setf *opt-enable-pure-call-optimization* (>= speed 3)))
  *opt-enable-pure-call-optimization*)

;;; ─── FR-276: Optimization Levels (-O0 to -O3) ─────────────────────────────
;;;
;;; Each level maps to a pre-configured set of optimizer parameters.
;;; The level can be set via the CLI (-O0/-O1/-O2/-O3) or via
;;; (declare (optimize ...)) in source code.

(defparameter *optimization-level-params*
  '((0 :inline-threshold-scale 0   :max-iterations 1  :pass-pipeline :o0
       :speed 0 :enable-egraph nil :description "Fold + DCE only (fast debug build)")
    (1 :inline-threshold-scale 0.5 :max-iterations 5  :pass-pipeline :o1
       :speed 1 :enable-egraph nil :description "Fold + Jump + DCE + basic inline")
    (2 :inline-threshold-scale 1.0 :max-iterations 20 :pass-pipeline :o2
       :speed 2 :enable-egraph t   :description "Full pipeline (production default)")
    (3 :inline-threshold-scale 2.0 :max-iterations 40 :pass-pipeline :o3
       :speed 3 :enable-egraph t   :description "Aggressive: full pipeline + e-graph saturation"))
  "FR-276: Pre-configured optimizer parameters for each -O level.")

(defun opt-level-params (level)
  "Return the parameter plist for optimization LEVEL (0-3).
LEVEL is clamped to 0..3."
  (let* ((lvl (max 0 (min 3 (or level 2))))
         (entry (assoc lvl *optimization-level-params*)))
    (cdr entry)))

(defun apply-optimization-level (level)
  "Configure the global optimizer state for optimization LEVEL (0-3).
Returns the parameter plist that was applied."
  (let ((params (opt-level-params level)))
    (setf *opt-inline-threshold-scale* (getf params :inline-threshold-scale)
          *opt-enable-pure-call-optimization* (>= (getf params :speed) 3)
          *opt-enable-sealed-gf-devirtualization* (>= (getf params :speed) 2)
          *enable-prolog-peephole* (getf params :enable-egraph))
    params))

;;; ─── Pipeline Context Builder ────────────────────────────────────────────

(defun %opt-build-pipeline-context (instructions
                                    &key speed opt-bisect-limit block-compile
                                         inline-threshold-scale
                                         print-pass-timings timing-stream
                                         print-pass-stats   stats-stream
                                         print-opt-remarks  opt-remarks-stream
                                         (opt-remarks-mode :all)
                                         trace-json-stream
                                         (max-iterations 20)
                                         pass-pipeline)
  "Construct the reporting, trace, and dynamic-binding context for a pipeline run.
Returns (values reporting trace max-iter passes dynamic-bindings-alist)."
  (let* ((reporting (make-opt-reporting-options
                     :print-pass-timings print-pass-timings
                     :timing-stream      (or timing-stream *standard-output*)
                     :print-pass-stats   print-pass-stats
                     :stats-stream       (or stats-stream *standard-output*)
                     :print-opt-remarks  print-opt-remarks
                     :opt-remarks-stream (or opt-remarks-stream *standard-output*)
                     :opt-remarks-mode   opt-remarks-mode))
         (trace     (make-opt-trace-state
                     :enabled     (not (null trace-json-stream))
                     :json-stream trace-json-stream))
         (max-iter  (if (eq max-iterations :adaptive)
                        (opt-adaptive-max-iterations instructions)
                        max-iterations))
         (passes    (opt-resolve-pass-pipeline pass-pipeline)))
    (values reporting trace max-iter passes
            (list (cons '*opt-inline-threshold-scale*
                        (or inline-threshold-scale 1))
                  (cons '*opt-bisect-limit*
                        (or opt-bisect-limit *opt-bisect-limit*))
                  (cons '*opt-bisect-count* 0)
                  (cons '*block-compile*
                        (or block-compile *block-compile*))
                  (cons '*opt-enable-pure-call-optimization*
                        (if speed (>= speed 3) *opt-enable-pure-call-optimization*))
                  (cons '*opt-enable-sealed-gf-devirtualization*
                        (if speed (>= speed 2) *opt-enable-sealed-gf-devirtualization*))))))

;;; ─── Public Entry Point ──────────────────────────────────────────────────

(defun optimize-instructions (instructions &key (max-iterations 20) pass-pipeline
                                                 print-pass-timings timing-stream
                                                 print-pass-stats   stats-stream
                                                 print-opt-remarks  opt-remarks-stream
                                                 (opt-remarks-mode :all)
                                                 speed
                                                 opt-bisect-limit
                                                 (inline-threshold-scale 1)
                                                 trace-json-stream
                                                 block-compile
                                                 &allow-other-keys)
  "Run the full multi-pass optimization pipeline on a VM instruction sequence.
Iterates until convergence or MAX-ITERATIONS. Returns optimized instructions.
When *skip-optimizer-passes* is non-NIL, returns instructions unchanged.

Security-mitigation keywords (retpoline, spectre-mitigations, stack-protector,
shadow-stack, asan, msan, tsan, ubsan, hwasan) are accepted and ignored via
&allow-other-keys."
  (when *skip-optimizer-passes*
    (return-from optimize-instructions (values instructions nil)))
  (multiple-value-bind (reporting trace max-iter passes bindings)
      (%opt-build-pipeline-context
       instructions
       :speed speed
       :opt-bisect-limit opt-bisect-limit
       :block-compile block-compile
       :inline-threshold-scale inline-threshold-scale
       :print-pass-timings print-pass-timings
       :timing-stream timing-stream
       :print-pass-stats print-pass-stats
       :stats-stream stats-stream
       :print-opt-remarks print-opt-remarks
       :opt-remarks-stream opt-remarks-stream
       :opt-remarks-mode opt-remarks-mode
       :trace-json-stream trace-json-stream
       :max-iterations max-iterations
       :pass-pipeline pass-pipeline)
    (progv (mapcar #'car bindings) (mapcar #'cdr bindings)
      (let ((prog instructions))
        (loop repeat max-iter
              for prev = prog
              do (setf prog (%opt-run-passes-once prog passes reporting trace))
                 (when *verify-optimizer-instructions*
                   (opt-verify-instructions prog))
              when (opt-converged-p prev prog)
              return prog)
        (when trace-json-stream
          (%opt-write-trace-json trace-json-stream (nreverse (opt-trace-events trace))))
        (opt-pass-leaf-detect prog)))))
