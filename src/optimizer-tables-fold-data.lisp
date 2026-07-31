(in-package :cl-cc/optimize)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer Data Tables — Fold, Type-Predicate, and Auto-Vectorization Tables
;;;
;;; Pure data tables split out of optimizer-tables.lisp (loads before this
;;; file, which defines %alist->eq-hash-table used throughout below).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── Fold Tables ─────────────────────────────────────────────────────────
;;;
;;; Each table maps a VM instruction struct-type symbol to a CL function
;;; that performs the compile-time fold.  Adding a new foldable instruction
;;; requires only ONE entry here — opt-fold-binop-value, opt-pass-fold,
;;; and opt-pass-cse all derive their behavior from these tables.

(defparameter *opt-binary-fold-table*
  (%alist->eq-hash-table
   `((vm-add         . ,#'+)
      (vm-integer-add . ,#'+)
      (vm-add-checked . ,#'+)
     (vm-float-add   . ,#'+)
     (vm-sub         . ,#'-)
      (vm-integer-sub . ,#'-)
      (vm-sub-checked . ,#'-)
     (vm-float-sub   . ,#'-)
     (vm-mul         . ,#'*)
      (vm-integer-mul . ,#'*)
      (vm-mul-checked . ,#'*)
     (vm-float-mul   . ,#'*)
     (vm-mod         . ,#'mod)
     (vm-rem         . ,#'rem)
     (vm-min         . ,#'min)
     (vm-max         . ,#'max)
     (vm-logand      . ,#'logand)
     (vm-logior      . ,#'logior)
     (vm-logxor      . ,#'logxor)
     (vm-logeqv      . ,#'logeqv)
     (vm-ash         . ,#'ash)
      (vm-rotate      . ,#'rotate-right)
      (vm-bswap       . ,#'bswap)
      (vm-div         . ,#'floor)
      (vm-cl-div      . ,#'/)
      (vm-float-div   . ,#'/)
       (vm-gcd         . ,#'gcd)
       (vm-lcm         . ,#'lcm)
       (vm-char        . ,#'char)))
  "Maps binary VM instruction types to their CL fold functions.
   Used by opt-fold-binop-value for constant folding.")

(defparameter *opt-binary-cmp-fold-table*
  (%alist->eq-hash-table
   `((vm-lt     . ,#'<)
     (vm-gt     . ,#'>)
     (vm-le     . ,#'<=)
     (vm-ge     . ,#'>=)
     (vm-num-eq . ,#'=)))
  "Maps comparison VM instruction types to their CL predicate functions.
   Comparisons fold to 1/0 (not t/nil) for VM register semantics.")

(defparameter *opt-binary-zero-guard-types*
  '(vm-div vm-cl-div vm-float-div vm-mod vm-rem)
  "Binary instruction types that must guard against zero divisor before folding.")

(defparameter *opt-binary-no-fold-types*
  '(vm-floor-inst vm-ceiling-inst vm-truncate vm-round-inst)
  "Binary instruction types that set vm-values-list side-channel and must NOT be folded.")

(defparameter *opt-unary-fold-table*
  (%alist->eq-hash-table
   `((vm-neg         . ,#'-)
     (vm-abs         . ,#'abs)
     (vm-inc         . ,#'1+)
     (vm-dec         . ,#'1-)
     (vm-lognot      . ,#'lognot)
     (vm-rational    . ,#'rational)
      (vm-rationalize . ,#'rationalize)
      (vm-numerator   . ,#'numerator)
      (vm-denominator . ,#'denominator)
      (vm-string-length . ,#'length)
      (vm-length      . ,#'length)
      (vm-car         . ,#'car)
      (vm-cdr         . ,#'cdr)
      (vm-string-upcase . ,#'string-upcase)
      (vm-string-downcase . ,#'string-downcase)
      ;; NOT host CL's NOT. VM-NOT executes as (if (vm-falsep src) t nil) and
      ;; the language treats numeric zero as false, so #'not folded (not 0) to
      ;; NIL while the same instruction interpreted at run time yields T. Any
      ;; constant reaching a VM-NOT was therefore miscompiled — most visibly
      ;; through COMPLEMENT, whose (not (apply pred args)) body made every
      ;; -IF-NOT sequence function report "nothing matched".
      (vm-not         . ,(lambda (value) (if (opt-falsep value) t nil)))))
  "Maps unary VM instruction types to their CL fold functions.")

(defparameter *opt-type-pred-fold-table*
  (%alist->eq-hash-table
   `((vm-null-p          . ,#'null)
     (vm-cons-p          . ,#'consp)
     (vm-symbol-p        . ,#'symbolp)
     (vm-number-p        . ,#'numberp)
      (vm-integer-p       . ,#'integerp)
      (vm-function-p      . ,(constantly nil))
      (vm-stringp         . ,#'stringp)
      (vm-listp           . ,#'listp)
      (vm-vectorp         . ,#'vectorp)
      (vm-simple-vector-p . ,#'simple-vector-p)))
  "Maps type-predicate VM instruction types to their CL predicate functions.
   Predicates fold to 1/0.  vm-function-p always returns 0 for constants.")

(defparameter *opt-binary-lhs-rhs-types*
  '(vm-lt vm-gt vm-le vm-ge vm-num-eq vm-eq
    vm-gcd vm-lcm
    vm-mod vm-rem vm-min vm-max
    vm-truncate vm-floor-inst vm-ceiling-inst vm-round-inst
    vm-logand vm-logior vm-logxor vm-logeqv vm-ash vm-rotate
    vm-div vm-cl-div vm-ffloor vm-fceiling vm-ftruncate vm-fround)
  "Non-binop-subclass instruction types that have vm-lhs/vm-rhs accessors.
   vm-binop (parent of vm-add/vm-sub/vm-mul) is handled by typecase inheritance.
   Used by opt-inst-read-regs, opt-pass-fold, opt-pass-cse, and WASM register collection.")

(defparameter *opt-unary-src-types*
  '(vm-neg vm-abs vm-inc vm-dec vm-lognot vm-bswap vm-not
    vm-string-length vm-length
    vm-car vm-cdr
    vm-string-upcase vm-string-downcase
    vm-rational vm-rationalize vm-numerator vm-denominator
    vm-cons-p vm-null-p vm-symbol-p vm-number-p
    vm-integer-p vm-function-p vm-stringp vm-listp vm-vectorp)
  "Instruction types that have vm-src/vm-dst unary accessors.
   Used by opt-inst-read-regs, opt-pass-fold, opt-pass-cse, and WASM register collection.")

(defparameter *opt-commutative-inst-types*
  '(vm-add vm-integer-add vm-add-checked vm-float-add
    vm-mul vm-integer-mul vm-mul-checked vm-float-mul
    vm-logand vm-logior vm-logxor vm-logeqv
    vm-num-eq vm-eq vm-min vm-max)
  "VM binary instruction struct-type symbols where operand order is irrelevant.
   Used by opt-pass-cse to produce a canonical key for commutative expressions,
   enabling (+ a b) and (+ b a) to share the same CSE memo entry.")

;;; ─── Unary Fold Eligibility ──────────────────────────────────────────────

(defparameter *opt-unary-fold-eligible-predicates*
  (%alist->eq-hash-table
   `((vm-string-length  . ,#'stringp)
     (vm-string-upcase  . ,#'stringp)
     (vm-string-downcase . ,#'stringp)
     (vm-length         . ,(lambda (v) (or (vectorp v)
                                           (and (listp v) (ignore-errors (length v) t)))))
     (vm-car            . ,(lambda (v) (or (consp v) (null v))))
     (vm-cdr            . ,(lambda (v) (or (consp v) (null v))))
     (vm-not            . ,(constantly t))))
  "Maps unary VM instruction types to their constant-eligibility predicates.
   Types absent from this table use NUMBERP as the default eligibility check.")

;;; ─── Auto-Vectorization Comparison Clone Table ───────────────────────────

(deftype opt-autovec-cmp ()
  "CL type specifier for supported counted-loop comparison instructions."
  '(or vm-lt vm-le))

(defparameter *opt-autovec-cmp-clone-table*
  (%alist->eq-hash-table
   `((vm-lt . ,(lambda (dst lhs rhs) (make-vm-lt :dst dst :lhs lhs :rhs rhs)))
     (vm-le . ,(lambda (dst lhs rhs) (make-vm-le :dst dst :lhs lhs :rhs rhs)))))
  "Maps comparison instruction types to clone constructors.
   Used by %opt-autovec-clone-cmp.")

;;; ─── Auto-Vectorization SIMD Op Mapping ─────────────────────────────────

(defparameter *opt-autovec-scalar-to-simd-op*
  (%alist->eq-hash-table
   '((vm-add          . :add)
     (vm-integer-add  . :add)
     (vm-add-checked  . :add)
     (vm-sub          . :sub)
     (vm-integer-sub  . :sub)
     (vm-sub-checked  . :sub)
     (vm-mul          . :mul)
     (vm-integer-mul  . :mul)
     (vm-mul-checked  . :mul)
     (vm-logand       . :logand)
     (vm-logior       . :logior)
     (vm-logxor       . :logxor)
     (vm-min          . :min)
     (vm-max          . :max)))
  "Maps scalar binary VM instruction types to backend-neutral SIMD op keywords.
   Used by %opt-autovec-op-kind and the SLP vectorizer.")
