;;;; optimizer-interval-arithmetic.lisp — closed-interval value arithmetic
;;;;
;;;; A [lo, hi] integer interval abstract domain and its arithmetic:
;;;; add/sub/mul/neg/abs, bitwise ops, and bit-width/fixnum-fit queries.
;;;; Dispatched by instruction type through the tables in
;;;; optimizer-interval-data.lisp and driven per-instruction by
;;;; optimizer-interval-transfer.lisp's dataflow transfer functions.

(in-package :cl-cc/optimize)

(defun opt-make-interval (lo hi)
  "Construct a closed integer interval [LO, HI]."
  (cons lo hi))

(defun opt-interval-lo (interval)
  (car interval))

(defun opt-interval-hi (interval)
  (cdr interval))

(defun opt-interval-singleton-p (interval)
  "Return T when INTERVAL is a singleton [N, N]."
  (and interval
       (= (opt-interval-lo interval)
          (opt-interval-hi interval))))

(defun opt-interval-singleton-value (interval)
  "Return INTERVAL's single value, or NIL when it is not a singleton."
  (when (opt-interval-singleton-p interval)
    (opt-interval-lo interval)))

(defun opt-interval-add (a b)
  "Add two intervals conservatively."
  (opt-make-interval (+ (opt-interval-lo a) (opt-interval-lo b))
                     (+ (opt-interval-hi a) (opt-interval-hi b))))

(defun opt-interval-sub (a b)
  "Subtract interval B from A conservatively."
  (opt-make-interval (- (opt-interval-lo a) (opt-interval-hi b))
                     (- (opt-interval-hi a) (opt-interval-lo b))))

(defun opt-interval-mul (a b)
  "Multiply two intervals conservatively."
  (let* ((p1 (* (opt-interval-lo a) (opt-interval-lo b)))
         (p2 (* (opt-interval-lo a) (opt-interval-hi b)))
         (p3 (* (opt-interval-hi a) (opt-interval-lo b)))
         (p4 (* (opt-interval-hi a) (opt-interval-hi b))))
    (opt-make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(defun opt-interval-neg (a)
  "Negate interval A conservatively."
  (opt-make-interval (- (opt-interval-hi a))
                     (- (opt-interval-lo a))))

(defun opt-interval-abs (a)
  "Return a conservative interval for ABS over A."
  (cond
    ((>= (opt-interval-lo a) 0) a)
    ((<= (opt-interval-hi a) 0) (opt-interval-neg a))
    (t (opt-make-interval 0 (max (abs (opt-interval-lo a))
                                 (abs (opt-interval-hi a)))))))

(defun opt-interval-bit-width (interval)
  "Return a conservative unsigned bit-width upper bound for INTERVAL.

Only non-negative intervals are assigned a width. Mixed-sign or fully-negative
intervals return NIL rather than pretending the result is narrower than proven."
  (when (opt-interval-nonnegative-p interval)
    (integer-length (max 0 (opt-interval-hi interval)))))

(defun opt-interval-known-bits-mask (interval)
  "Return a conservative bit mask covering every bit that may be set.

For a non-negative interval with width W, the result is 2^W-1. Bits outside
that mask are therefore known zero for every value in INTERVAL. Returns NIL
when INTERVAL has no proven non-negative width bound."
  (let ((width (opt-interval-bit-width interval)))
    (when width
      (1- (ash 1 width)))))

(defun opt-interval-fits-fixnum-width-p
    (interval &optional (limit-width (integer-length +opt-range-positive-infinity+)))
  "Return T when INTERVAL's unsigned width is strictly below LIMIT-WIDTH."
  (let ((width (opt-interval-bit-width interval)))
    (and width (< width limit-width))))

(defun opt-interval-fits-fixnum-p (interval)
  "Return T when INTERVAL is proven to stay within the host fixnum range."
  (and interval
       (<= +opt-range-negative-infinity+ (opt-interval-lo interval))
       (<= (opt-interval-hi interval) +opt-range-positive-infinity+)))

(defun %opt-nonnegative-mask-value (interval)
  "Return INTERVAL's non-negative singleton value, or NIL."
  (let ((value (opt-interval-singleton-value interval)))
    (when (and (integerp value) (not (minusp value)))
      value)))

(defun opt-interval-logand (a b)
  "Return a conservative interval for LOGAND over A and B.

If either operand is a known non-negative singleton mask, the result is bounded
to [0, mask] even when the other operand is unknown. When both operands are
proven non-negative, the result is also bounded above by the smaller upper
bound. Returns NIL when no safe bound is known."
  (let ((mask-a (%opt-nonnegative-mask-value a))
        (mask-b (%opt-nonnegative-mask-value b)))
    (cond
      (mask-a
       (opt-make-interval 0 (if (opt-interval-nonnegative-p b)
                                (min mask-a (opt-interval-hi b))
                                mask-a)))
      (mask-b
       (opt-make-interval 0 (if (opt-interval-nonnegative-p a)
                                (min mask-b (opt-interval-hi a))
                                mask-b)))
      ((and (opt-interval-nonnegative-p a)
            (opt-interval-nonnegative-p b))
       (opt-make-interval 0 (min (opt-interval-hi a)
                                 (opt-interval-hi b))))
       (t nil))))

(defun opt-interval-logior (a b)
  "Return a conservative interval for LOGIOR over A and B.

When both operands are proven non-negative, the result is non-negative and
bounded above by OR-ing their known-bits masks."
  (when (and (opt-interval-nonnegative-p a)
             (opt-interval-nonnegative-p b))
    (let ((mask-a (opt-interval-known-bits-mask a))
          (mask-b (opt-interval-known-bits-mask b)))
      (when (and mask-a mask-b)
        (opt-make-interval 0 (logior mask-a mask-b))))))

(defun opt-interval-logxor (a b)
  "Return a conservative interval for LOGXOR over A and B.

For proven non-negative operands, every set bit in the result must come from a
set bit in either operand, so the same known-bits upper mask as LOGIOR applies."
  (when (and (opt-interval-nonnegative-p a)
             (opt-interval-nonnegative-p b))
    (let ((mask-a (opt-interval-known-bits-mask a))
          (mask-b (opt-interval-known-bits-mask b)))
      (when (and mask-a mask-b)
        (opt-make-interval 0 (logior mask-a mask-b))))))

(defun opt-interval-ash (value-interval shift-interval)
  "Return a conservative interval for ASH over VALUE-INTERVAL by SHIFT-INTERVAL.

Only singleton integer shifts are handled. Unknown shifts return NIL.
For a fixed integer shift K, ASH is monotone over integers, so bounds can be
shifted directly."
  (let ((k (opt-interval-singleton-value shift-interval)))
    (when (integerp k)
      (opt-make-interval (ash (opt-interval-lo value-interval) k)
                         (ash (opt-interval-hi value-interval) k)))))

(defun opt-interval-nonnegative-p (interval)
  "Return T when INTERVAL is proven to contain only non-negative integers."
  (and interval (<= 0 (opt-interval-lo interval))))

(defun opt-interval-widen (old new &key
                                 (negative-infinity +opt-range-negative-infinity+)
                                 (positive-infinity +opt-range-positive-infinity+))
  "Widen OLD toward NEW for monotone interval fixpoint convergence.

When NEW extends below OLD, the lower bound becomes NEGATIVE-INFINITY.  When
NEW extends above OLD, the upper bound becomes POSITIVE-INFINITY.  Bounds that
do not move remain unchanged.  NIL OLD is treated as bottom and returns NEW."
  (cond
    ((null old) new)
    ((null new) nil)
    (t
     (opt-make-interval
      (if (< (opt-interval-lo new) (opt-interval-lo old))
          negative-infinity
          (opt-interval-lo old))
      (if (> (opt-interval-hi new) (opt-interval-hi old))
          positive-infinity
          (opt-interval-hi old))))))

(defun opt-interval-valid-index-p (index-interval length-interval)
  "Return T when INDEX-INTERVAL is proven valid for any length in LENGTH-INTERVAL.

This is the conservative BCE predicate used by FR-039 style array bounds checks:
the index lower bound must be non-negative, and the index upper bound must be
strictly below the minimum possible array length."
  (and (opt-interval-nonnegative-p index-interval)
       length-interval
       (< (opt-interval-hi index-interval)
          (opt-interval-lo length-interval))))
