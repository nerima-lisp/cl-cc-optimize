;;;; effects-known-functions.lisp — known CL builtin function effect/purity properties

(in-package :cl-cc/optimize)

;;; ─── Known Function Property Database (FR-183) ───────────────────────────

(defparameter *known-function-property-groups*
  '(((:pure :foldable :no-escape)
     + - * 1+ 1- abs min max evenp oddp zerop plusp minusp
     ash logand logior logxor logeqv lognot logtest logbitp logcount
     integer-length bswap expt exp sin cos tan asin acos atan sinh cosh
     tanh asinh acosh float float-precision float-radix float-sign
     float-digits rational rationalize numerator denominator gcd lcm realpart
     imagpart conjugate phase complex
     char= char< char> char<= char>= char/= char-equal char-not-equal
     char-lessp char-greaterp char-not-greaterp char-not-lessp
     both-case-p graphic-char-p standard-char-p digit-char alpha-char-p
     alphanumericp upper-case-p lower-case-p char-upcase char-downcase char-code
     char-int code-char digit-char-p type-of sxhash)
    ((:pure :foldable :always-returns :no-escape)
     eq eql not null atom consp symbolp numberp integerp functionp stringp
     characterp vectorp hash-table-p keywordp)
    ((:pure :foldable :no-escape)
     / mod rem sqrt log acosh atanh)
    ((:read-only)
     car cdr first second third fourth fifth sixth seventh eighth ninth tenth rest
     last butlast nth nthcdr member assoc get hash-table-count hash-table-keys
     hash-table-values hash-table-test hash-table-size hash-table-rehash-size
     hash-table-rehash-threshold array-length array-rank array-total-size
     array-dimensions array-dimension array-has-fill-pointer-p array-adjustable-p
     array-displacement row-major-aref svref aref bit sbit char symbol-name
     symbol-plist find-package find-symbol class-name class-of find-class
     string= string< string> string<= string>= string-equal string-lessp
     string-greaterp string/= string-not-equal string-not-greaterp
     string-not-lessp string-length string-upcase string-downcase
     string-capitalize string-concat coerce-to-string string equal listp endp)
    ((:read-only :nonneg-result)
     length string-length list-length array-length array-rank array-total-size
     hash-table-count)
    ((:alloc)
     values-list copy-list copy-tree reverse append cons acons subst subseq complement
     string-trim string-left-trim string-right-trim parse-integer make-symbol
     make-list make-random-state make-string-output-stream
     lisp-implementation-type lisp-implementation-version machine-type
     machine-version machine-instance software-type software-version
     short-site-name long-site-name get-output-stream-string read-from-string)
    ((:io)
     princ prin1 print terpri fresh-line read read-char read-line read-byte
     unread-char peek-char listen write-char write-byte write-line force-output
     finish-output clear-input clear-output file-position file-length close
     get-universal-time get-internal-real-time get-internal-run-time
     decode-universal-time encode-universal-time sleep random gensym)
    ((:write-global)
     set makunbound fdefinition symbol-function nstring-upcase nstring-downcase
     nstring-capitalize nreverse nbutlast nconc nreconc rplaca rplacd remhash
     clrhash remprop vector-pop vector-push vector-push-extend adjust-array
     aset setf-gethash rt-string-set rt-bit-set bit-and bit-ior bit-or bit-xor
     bit-not intern %set-symbol-prop %set-symbol-plist %set-fill-pointer
     %svset %progv-enter %progv-exit)
    ((:control)
     error cerror signal warn eval load macroexpand macroexpand-1 next-method-p))
  "Groups of known CL function properties used by optimizer and compiler metadata.
Properties follow FR-183: :pure, :foldable, :nonneg-result, :always-returns,
:no-escape. Conservative effect tags (:read-only, :alloc, :io, :write-global,
:control) keep impure or heap-reading builtins classified without pretending
they are CSE-safe pure functions.")

(defparameter *known-function-property-table*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (group *known-function-property-groups*)
      (destructuring-bind (properties &rest names) group
        (dolist (name names)
          (let* ((key (string-upcase (symbol-name name)))
                 (existing (gethash key table)))
            (setf (gethash key table)
                  (remove-duplicates (append existing properties) :test #'eq))))))
    table)
  "Maps uppercase CL function names to known function property keywords.")

(defun %known-function-name-key (function-name)
  "Normalize FUNCTION-NAME to the uppercase key used by the property database."
  (etypecase function-name
    (symbol (string-upcase (symbol-name function-name)))
    (string (string-upcase function-name))))

(defun register-known-function-properties (function-name properties)
  "Register or extend known PROPERTIES for FUNCTION-NAME."
  (let* ((key (%known-function-name-key function-name))
         (existing (gethash key *known-function-property-table*)))
    (setf (gethash key *known-function-property-table*)
          (remove-duplicates (append existing properties) :test #'eq))))

(defun known-function-properties (function-name)
  "Return known property keywords for FUNCTION-NAME.
Unknown functions return NIL so callers can remain conservative."
  (copy-list (gethash (%known-function-name-key function-name)
                      *known-function-property-table*)))

(defun known-function-property-p (function-name property)
  "Return true when FUNCTION-NAME is known to have PROPERTY."
  (member property (known-function-properties function-name) :test #'eq))

(defun known-function-effect-kind (function-name)
  "Return the optimizer effect-kind implied by FUNCTION-NAME properties."
  (let ((properties (known-function-properties function-name)))
    (cond
      ((member :control properties :test #'eq) :control)
      ((member :write-global properties :test #'eq) :write-global)
      ((member :io properties :test #'eq) :io)
      ((member :read-only properties :test #'eq) :read-only)
      ((member :alloc properties :test #'eq) :alloc)
      ((member :pure properties :test #'eq) :pure)
      (t :unknown))))
