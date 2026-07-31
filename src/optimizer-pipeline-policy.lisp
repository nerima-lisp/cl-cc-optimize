(in-package :cl-cc/optimize)
;;; -----------------------------------------------------------------------------
;;; Optimizer Pipeline Policy and Optional Passes
;;; -----------------------------------------------------------------------------

(defun opt-pass-inline-iterative (instructions)
  "Thresholded inline pass used inside the convergence loop."
  (opt-pass-inline instructions :threshold :adaptive))

(defun %opt-backedge-count (instructions)
  "Return a simple count of backward jumps in INSTRUCTIONS."
  (let ((label-pos (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for i from 0
          when (typep inst 'vm-label)
            do (setf (gethash (vm-name inst) label-pos) i))
    (loop for inst in instructions
          for i from 0
          count (and (typep inst '(or vm-jump vm-jump-zero))
                     (let ((target (gethash (vm-label-name inst) label-pos)))
                       (and target (< target i)))))))

(defun opt-adaptive-loop-unroll-factor (instructions &key call-count hotness)
  "Return adaptive loop-unroll factor and max trip values for INSTRUCTIONS."
  (let* ((n (length instructions))
         (score (or hotness (+ (or call-count 0) (* 10 (%opt-backedge-count instructions))))))
    (cond
      ((>= score 100) (values 4 16))
      ((>= score 30) (values 3 12))
      ((and (> n 800) (zerop score)) (values 1 4))
      (t (values 2 8)))))

(defun opt-pass-loop-unrolling-adaptive (instructions)
  "Run loop unrolling with adaptive loop thresholds scoped to this pass."
  (multiple-value-bind (factor max-trip)
      (opt-adaptive-loop-unroll-factor instructions)
    (let ((*opt-loop-unroll-factor* factor)
          (*opt-loop-unroll-max-trip* max-trip))
      (opt-pass-loop-unrolling instructions))))

(defvar *enable-prolog-peephole* t
  "When non-NIL, run the Prolog peephole and e-graph rewrite stages.

This is an optimizer policy gate, not Prolog engine state.")

(defun %maybe-apply-prolog-rewrite (instructions)
  "Apply the Prolog rewrite stage when enabled, preserving INSTRUCTIONS otherwise.
The stage first applies the instruction-level Prolog peephole rules and then runs
the e-graph rewrite engine whose builtin rule set is also sourced from Prolog rules."
  (if *enable-prolog-peephole*
      (optimize-with-egraph
       (mapcar #'sexp->instruction
               (apply-prolog-peephole (mapcar #'instruction->sexp instructions))))
      instructions))

(defvar *opt-enable-pure-call-optimization* t
  "When NIL, disable pure-call optimization regardless of selected pass pipeline.

This hook is used as an optimization-policy gate so frontends can couple the
pass to `(optimize (speed 3))`-style policy decisions without changing the
optimizer's pass table wiring.")

(defvar *opt-enable-sealed-gf-devirtualization* t
  "When NIL, keep sealed generic calls as dynamic `vm-generic-call` instructions.

This optimization is policy-gated because it trades compilation effort and a
closed-world proof for direct method invocation.  `opt-configure-optimization-policy`
enables it for SPEED >= 2.")

(defun %maybe-run-pure-call-optimization (instructions)
  "Run pure-call optimization only when policy gate permits it."
  (if *opt-enable-pure-call-optimization*
      (opt-pass-pure-call-optimization instructions)
      instructions))

(defun %maybe-run-verify-ir (instructions)
  "Run FR-642 IR/VM invariant verification only when *VERIFY-IR* is enabled.

Debugging-only pass: it never transforms INSTRUCTIONS, so leaving it disabled
(the default) costs nothing in the convergence loop."
  (if *verify-ir*
      (opt-pass-verify-ir instructions)
      instructions))

(progn
  (defun %opt-run-pass-if-fbound (pass-symbol instructions)
    "Run PASS-SYMBOL on INSTRUCTIONS when it is fbound, otherwise no-op."
    (if (fboundp pass-symbol)
        (funcall (symbol-function pass-symbol) instructions)
        instructions))

  (defvar *sroa-enabled* t
    "When NIL, %MAYBE-RUN-SROA leaves instructions untouched regardless of
whether FR-668's OPT-PASS-SROA is loaded.

This is a forward declaration: the canonical DEFVAR lives in
optimizer-sroa.lisp, which -- because :SROA needs a pipeline-policy gate
function defined before OPTIMIZER-PIPELINE.LISP's *OPT-PASS-TABLE* builds --
loads after this file in cl-cc-optimize.asd. DEFVAR only initializes a
still-unbound variable, so declaring it here and again in optimizer-sroa.lisp
is safe and idempotent; it just lets %MAYBE-RUN-SROA below reference the flag
without a compile-time forward reference.")

  (defun %maybe-run-sroa (instructions)
    "Run FR-668 Scalar Replacement of Aggregates (OPT-PASS-SROA) only when
*SROA-ENABLED* permits it.

Written by hand, like %MAYBE-RUN-VERIFY-IR and
%MAYBE-RUN-PURE-CALL-OPTIMIZATION above, because DEFINE-OPTIONAL-PASS's
generated wrapper only checks FBOUNDP, not a policy flag. Dispatch still goes
through %OPT-RUN-PASS-IF-FBOUND (by symbol, not a direct call) because
OPT-PASS-SROA is defined in optimizer-sroa.lisp, which loads after this file;
a direct `(opt-pass-sroa instructions)' call here would be a compile-time
forward reference to a not-yet-defined function."
    (if *sroa-enabled*
        (%opt-run-pass-if-fbound 'opt-pass-sroa instructions)
        instructions)))

(defmacro define-optional-pass (name &key pass doc)
  "Define a %maybe-run-NAME wrapper that delegates to opt-pass-PASS (or opt-pass-NAME).
Data: the pass name. Logic: fbound check and dispatch via %opt-run-pass-if-fbound."
  (let* ((n         (symbol-name name))
         (fn-name   (intern (format nil "%MAYBE-RUN-~A" n)))
         (pass-sym  (if pass
                        (intern (format nil "OPT-PASS-~A" (symbol-name pass)))
                        (intern (format nil "OPT-PASS-~A" n)))))
    `(defun ,fn-name (instructions)
       ,@(when doc (list doc))
       (%opt-run-pass-if-fbound ',pass-sym instructions))))

;;; Optional pass wrappers — data-driven with define-optional-pass.
;;; Each entry: (wrapper-name [:pass pass-name] [:doc "docstring"])
;;; Logic (fbound check + dispatch) lives in the macro; only data varies.

(define-optional-pass fr523-affine-loop-analysis :pass affine-loop-analysis)
(define-optional-pass fr524-loop-interchange     :pass loop-interchange)
(define-optional-pass fr525-polyhedral-schedule  :pass polyhedral-schedule)
(define-optional-pass fr526-loop-fusion-fission  :pass loop-fusion-fission)
(define-optional-pass superopt
  :doc "Run FR-750 superoptimization when its pass file is loaded.")
(define-optional-pass speculative-inline
  :doc "Run FR-523 speculative inlining when the optional pass is loaded.")


(define-optional-pass loop-rotate)
(define-optional-pass dead-loop-elimination)
(define-optional-pass loop-unroll)
(define-optional-pass loop-unswitch)
(define-optional-pass dead-argument-elimination)
(define-optional-pass tail-duplication)
(define-optional-pass iv-strength-reduce)
(define-optional-pass div-by-const)
(define-optional-pass loop-peel)
(define-optional-pass idiom-recognition)
(define-optional-pass trmc)
(define-optional-pass value-range-propagation)
(define-optional-pass bounds-check-elimination)
(define-optional-pass overflow-check-elimination)
(define-optional-pass bitwidth-reduction)
(define-optional-pass cps-reduce)
(define-optional-pass defunctionalize)
(define-optional-pass delimited-continuations
  :doc "Run FR-677 delimited-continuation lowering only when explicitly selected.")
(define-optional-pass escape-analysis)
(define-optional-pass path-profiling
  :doc "Run FR-662 Basic Block Versioning / Path Profiling when loaded.")
(define-optional-pass load-store-coalescing :pass load-widening-store-coalescing
  :doc "Run FR-723 Load Widening / Store Coalescing when loaded.")
(define-optional-pass optimization-remarks)
(define-optional-pass abstract-interpretation
  :doc "Run FR-751 abstract interpretation when loaded and enabled.")
(define-optional-pass translation-validation
  :doc "Register FR-752 translation validation; per-pass checks are pipeline-integrated.")
(define-optional-pass loop-fusion)
(define-optional-pass loop-fission)
(define-optional-pass loop-tile)
(define-optional-pass autotune-simd)
(define-optional-pass polyhedral
  :doc "Run explicit FR-513 polyhedral pass only when its implementation is loaded.")
(define-optional-pass mlgo-inline
  :doc "Run explicit FR-580 MLGO inline pass only when its implementation is loaded.")
(define-optional-pass ml-regalloc
  :doc "Run explicit ML register-allocation hint pass only when loaded.")
