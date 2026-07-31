(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer Data Tables — DST Accessor Dispatch
;;;
;;; struct-type → dst-accessor dispatch table plus small generic hash-table
;;; helpers used across the optimizer data tables.
;;;
;;; Fold/comparison/type-predicate tables are in optimizer-tables-fold-data.lisp;
;;; the read-regs dispatch table and opt-inst-read-regs are in
;;; optimizer-tables-regs.lisp (both load after this file).
;;;
;;; Load order: before optimizer.lisp (tables must exist before passes).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── Classification Helpers ──────────────────────────────────────────────

(defun opt-falsep (value)
  "Compile-time analog of vm-falsep: T if VALUE is falsy.

The optimizer uses the same language-level truthiness as the VM and CPS layers:
both NIL and numeric zero are false."
  (or (null value)
      (and (numberp value)
           (zerop value))))

(defun opt-register-keyword-p (x)
  "T if X is a VM register keyword of the form :Rn (e.g. :R0, :R15).
   Used by the sexp-reflection fallback in opt-inst-read-regs."
  (and (keywordp x)
       (let ((name (symbol-name x)))
         (and (>= (length name) 2)
              (char= (char name 0) #\R)
              (every #'digit-char-p (subseq name 1))))))

(defun %build-vm-dst-table ()
  "Build a struct-type-name → dst-accessor-fn table from vm-dst CLOS methods.
Must be called in the main thread (at load time) before workers are spawned.
Maps each instruction struct type (including inherited subclasses) to its
struct accessor, eliminating PCL dispatch-cache updates in worker threads.

Handles defstruct inheritance: when vm-add inherits dst from vm-binop,
vm-binop-dst works on vm-add instances, so all vm-binop subclasses are
registered with vm-binop-dst unless they define a more-specific accessor."
  (let ((ht (make-hash-table :test #'eq))
        (visited (make-hash-table :test #'eq)))
    (dolist (method (sb-mop:generic-function-methods #'vm-dst))
      (handler-case
          (let* ((spec  (car (sb-mop:method-specializers method)))
                 (cname (class-name spec))
                 (acc   (find-symbol
                         (concatenate 'string (symbol-name cname) "-DST")
                         :cl-cc/vm)))
            (when (and acc (fboundp acc))
              (let ((fn (symbol-function acc)))
                (unless (gethash cname ht)
                  (setf (gethash cname ht) fn))
                (labels ((visit (class)
                           (unless (gethash class visited)
                             (setf (gethash class visited) t)
                             (dolist (sub (sb-mop:class-direct-subclasses class))
                               (let ((sub-name (class-name sub)))
                                 (unless (gethash sub-name ht)
                                   (setf (gethash sub-name ht) fn))
                                 (visit sub))))))
                  (visit spec)))))
        (error nil)))
    ht))

(defvar *vm-dst-table* nil
  "Pre-built struct-type → dst-accessor mapping; NIL until %init-vm-dst-table! is called.
When NIL, opt-inst-dst falls back to vm-dst CLOS dispatch (always correct).
Call %init-vm-dst-table! from the main thread before spawning parallel workers
to enable O(1) thread-safe dispatch without PCL dispatch-cache contention.")

(defun %init-vm-dst-table! ()
  "Initialize *vm-dst-table* from the live vm-dst CLOS methods.
Must be called from the main thread before parallel workers are spawned.
Safe to call multiple times; subsequent calls rebuild the table."
  (setf *vm-dst-table* (ignore-errors (%build-vm-dst-table))))

(defun opt-inst-dst (inst)
  "Return the single destination register written by INST, or NIL.
Returns NIL for instructions that do not write a destination (jump, halt,
ret, set-global, slot-write, etc.) or for unrecognised types."
  (let ((fn (and *vm-dst-table* (gethash (type-of inst) *vm-dst-table*))))
    (if fn
        (funcall fn inst)
        (ignore-errors (vm-dst inst)))))

;;; opt-inst-pure-p is defined in effects.lisp (loaded first in the optimize module).
;;; It replaces the former 2-type whitelist with a 100+-type data-driven table.

;;; ─── Pure Helper Utilities ───────────────────────────────────────────────

(defun %alist->eq-hash-table (alist)
  "Build an EQ hash-table from ALIST of (key . value) pairs."
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (pair alist ht)
      (setf (gethash (car pair) ht) (cdr pair)))))

(defun %hash-table-keys (ht)
  "Return all keys of HT as a list."
  (loop for k being the hash-keys of ht collect k))

;;; *opt-algebraic-identity-rules*, classification predicates
;;; (opt-binary-lhs-rhs-p, opt-unary-src-p, opt-foldable-unary-arith-p,
;;; opt-foldable-type-pred-p), and *opt-label-ref-table*
;;; are in optimizer-algebraic.lisp (loaded after optimizer-tables-regs.lisp).
