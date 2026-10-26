;;;; optimizer-inline-devirt-data.lisp — sealed-GF devirtualization data
;;;;
;;;; The fact record threaded through optimizer-inline-devirt.lisp's
;;;; forward analysis: per-register compile-time knowledge (bound global,
;;;; class, constant, closure label, or literal generic function) collected
;;;; as the pass walks an instruction stream. Pure data; the collection and
;;;; devirtualization logic that populates and consumes it lives in
;;;; optimizer-inline-devirt.lisp.

(in-package :cl-cc/optimize)

;;; --- Devirtualization Fact Struct ---

(defstruct opt-devirt-facts
  "Bundle of 6 hash tables that track per-register compile-time facts for
   sealed generic-function devirtualization.  Allocated once in opt-pass-devirtualize
   and threaded as a single argument through all helper functions."
  reg-name          ; reg → global-variable name symbol
  reg-class         ; reg → class name symbol (from vm-class-def)
  reg-object-class  ; reg → class name symbol (from vm-make-obj with sealed class)
  reg-const         ; reg → constant value
  reg-closure-label ; reg → closure entry-label string
  reg-gf-literal)

(defun %make-devirt-facts ()
  "Allocate a fresh opt-devirt-facts with one empty eq hash-table per slot."
  (make-opt-devirt-facts
   :reg-name          (make-hash-table :test #'eq)
   :reg-class         (make-hash-table :test #'eq)
   :reg-object-class  (make-hash-table :test #'eq)
   :reg-const         (make-hash-table :test #'eq)
   :reg-closure-label (make-hash-table :test #'eq)
   :reg-gf-literal    (make-hash-table :test #'eq)))

;;; Each entry is (reader writer) where reader extracts the slot HT from a facts
;;; struct and writer sets a key in it.  Used in the vm-move arm of
;;; %opt-track-sealed-gf-facts to propagate facts from src to dst without
;;; building a runtime cons list at each invocation.
(defparameter *devirt-fact-slot-accessors*
  (list #'opt-devirt-facts-reg-name
        #'opt-devirt-facts-reg-class
        #'opt-devirt-facts-reg-object-class
        #'opt-devirt-facts-reg-const
        #'opt-devirt-facts-reg-closure-label
        #'opt-devirt-facts-reg-gf-literal)
  "List of reader closures for each slot of opt-devirt-facts, used to iterate
   over all fact tables when propagating register facts across vm-move.")

(defvar *opt-enable-sealed-gf-devirtualization* t
  "When NIL, keep sealed generic calls as dynamic `vm-generic-call` instructions.

This optimization is policy-gated because it trades compilation effort and a
closed-world proof for direct method invocation.  `opt-configure-optimization-policy`
enables it for SPEED >= 2.")
