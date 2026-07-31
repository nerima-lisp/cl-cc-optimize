(in-package :cl-cc/optimize)

;;; -----------------------------------------------------------------------------
;;; Optimizer Local List Scheduling
;;; -----------------------------------------------------------------------------
;;; FR-069: Dependency-Aware Peephole Scheduling
;;;
;;; Schedule movable instruction runs inside each basic block.  A run ends at any
;;; instruction with side effects or control flow, so calls/stores/signals and
;;; labels/terminators keep their original relative position.
;;;
;;; Shared latency tables, dependency-graph construction, and the generic
;;; toposort list scheduler live here; pressure-aware pre-RA scheduling
;;; (FR-067) is in optimizer-scheduler-pre-ra.lisp (loads after this file).
(defparameter *opt-vm-instruction-latency-alist* '((vm-move . 1)
    (vm-const . 1)
    (vm-add . 1)
    (vm-integer-add . 1)
    (vm-add-checked . 1)
    (vm-float-add . 1)
    (vm-sub . 1)
    (vm-integer-sub . 1)
    (vm-sub-checked . 1)
    (vm-float-sub . 1)
    (vm-neg . 1)
    (vm-abs . 1)
    (vm-inc . 1)
    (vm-dec . 1)
    (vm-logand . 1)
    (vm-logior . 1)
    (vm-logxor . 1)
    (vm-logeqv . 1)
    (vm-lognot . 1)
    (vm-ash . 1)
    (vm-rotate . 1)
    (vm-bswap . 1)
    (vm-lt . 1)
    (vm-gt . 1)
    (vm-le . 1)
    (vm-ge . 1)
    (vm-eq . 1)
    (vm-num-eq . 1)
    (vm-min . 1)
    (vm-max . 1)
    (vm-not . 1)
    (vm-cons-p . 1)
    (vm-null-p . 1)
    (vm-symbol-p . 1)
    (vm-number-p . 1)
    (vm-integer-p . 1)
    (vm-function-p . 1)
    (vm-mul . 4)
    (vm-integer-mul . 4)
    (vm-mul-checked . 4)
    (vm-float-mul . 4)
    (vm-fma . 4)
    (vm-div . 40)
    (vm-cl-div . 40)
    (vm-float-div . 40)
    (vm-mod . 30)
    (vm-rem . 30)
    (vm-truncate . 40)
    (vm-floor-inst . 40)
    (vm-ceiling-inst . 40)
    (vm-round-inst . 40)
    (vm-ffloor . 40)
    (vm-fceiling . 40)
    (vm-ftruncate . 40)
    (vm-fround . 40)
    (vm-car . 4)
    (vm-cdr . 4)
    (vm-slot-read . 5)
    (vm-closure-ref-idx . 4)
    (vm-get-global . 5)
    (vm-func-ref . 4)
    (vm-values-to-list . 4))
  "Raw alist of VM instruction type → estimated latency in cycles.")

(defparameter *opt-vm-instruction-latencies*
  (%alist->eq-hash-table *opt-vm-instruction-latency-alist*)
  "Estimated VM instruction latencies in cycles for local list scheduling.
Derived from *opt-vm-instruction-latency-alist*.")
