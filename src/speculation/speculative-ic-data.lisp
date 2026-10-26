(in-package #:cl-cc/optimize/speculation)

(defstruct (opt-megamorphic-cache (:conc-name opt-mega-cache-))
  "Shared megamorphic dispatch cache used after IC state reaches :megamorphic."
  (entries (make-hash-table :test #'equal))
  (order nil :type list)
  (max-size 64 :type integer))

(defstruct (opt-ic-patch-plan (:conc-name opt-ic-patch-))
  "Plan for patching one IC call site at runtime.

This helper is backend-agnostic and only models *what* to patch, not machine
encoding details."
  site-id
  old-state
  new-state
  patch-kind
  target)

(defstruct (opt-speculation-log (:conc-name opt-spec-log-))
  "Failure log preventing repeated harmful speculative optimizations."
  (failures (make-hash-table :test #'equal))
  (threshold 1 :type integer))

(defparameter *opt-speculation-log* (make-opt-speculation-log)
  "Process-global speculation failure log used by conservative roadmap helpers.")

(defstruct (opt-ic-site (:conc-name opt-ic-site-)) "Polymorphic inline-cache state for one call site."
  (state :uninitialized :type keyword)
  (entries nil :type list)
  (max-polymorphic-entries 4 :type integer)
  (misses 0 :type integer)
  (megamorphic-fallback nil))
