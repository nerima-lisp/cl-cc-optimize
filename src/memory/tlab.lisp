;;;; packages/optimize/src/optimizer-tlab.lisp — FR-676 TLAB
;;;; Thread-Local Allocation Buffers for lock-free allocation.
;;;; HotSpot TLAB / Go per-P allocation cache equivalent.

(in-package :cl-cc/optimize)

(defvar *tlab-enabled* t)

(defstruct (tlab (:conc-name tlab-))
  "Thread-Local Allocation Buffer: per-thread pre-allocated heap region."
  (start 0 :type integer)      ; start address
  (current 0 :type integer)    ; current allocation pointer
  (end 0 :type integer)        ; end address
  (size (* 64 1024) :type integer)) ; 64 KB default

(defvar *per-thread-tlab* nil
  "Thread-local TLAB for the current thread.")

(defun tlab-allocate (tlab size-words)
  "Bump-allocate SIZE-WORDS words from TLAB.
Returns the address or NIL if TLAB is exhausted."
  (let* ((byte-size (* size-words 8))
         (new-ptr (+ (tlab-current tlab) byte-size)))
    (if (<= new-ptr (tlab-end tlab))
        (prog1 (tlab-current tlab)
          (setf (tlab-current tlab) new-ptr))
        ;; TLAB exhausted: refill from global heap
        )))

(defun tlab-refill (tlab heap &key new-chunk-address)
  "Refill TLAB with a fresh chunk so TLAB-ALLOCATE has room to bump-allocate
from again.

TLAB-ALLOCATE's bump-pointer math is already correct and complete; this
function's only job is to hand it a new [START, START + SIZE) range. This
module has no real heap/memory-region abstraction of its own -- there is
nothing else in this codebase it could call to obtain a genuine address --
so its contract is deliberately narrowed from the previous stub: it never
invents an address.

Callers that own a real heap pass the freshly obtained chunk's start address
as NEW-CHUNK-ADDRESS. HEAP is accepted (and currently ignored) so a future
caller with a concrete heap/region object can be threaded through this
function's signature without another change to callers; today nothing in
this codebase provides one. When NEW-CHUNK-ADDRESS is not supplied, this
signals an ERROR rather than silently defaulting to address 0: address 0 is
a null pointer, and hand it out as a live TLAB range would let
TLAB-ALLOCATE dole out overlapping [0, TLAB-SIZE) addresses to every caller
that hits an exhausted TLAB with no real backing store configured -- exactly
the silent memory corruption this module must never produce."
  (declare (ignore heap))
  (unless new-chunk-address
    (error "TLAB-REFILL requires a real backing chunk address via ~
:NEW-CHUNK-ADDRESS; this module has no heap of its own to allocate one ~
from, and refilling with a fabricated address (e.g. 0) would silently ~
hand out overlapping/null memory ranges."))
  (setf (tlab-start tlab) new-chunk-address
        (tlab-current tlab) new-chunk-address
        (tlab-end tlab) (+ new-chunk-address (tlab-size tlab)))
  tlab)

(defun tlab-init (size)
  "Initialize a new TLAB with SIZE bytes."
  (make-tlab :size size))

;; Thread-local TLAB access
(defmacro with-tlab ((&optional (size (* 64 1024))) &body body)
  "Execute BODY with a thread-local TLAB of SIZE bytes."
  `(let ((*per-thread-tlab* (tlab-init ,size)))
     (unwind-protect ,@body
       ;; Return remaining TLAB to global heap
       (setf *per-thread-tlab* nil))))
