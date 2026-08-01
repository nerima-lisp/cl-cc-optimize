;;;; optimizer-roadmap-evidence-check.lisp — roadmap evidence verification
;;;;
;;;; Checks that a feature's registered evidence (optimizer-roadmap-evidence-
;;;; data.lisp's registry) actually holds: the module it claims still exists,
;;;; its test anchors are registered, and its API symbols are fbound. Doc
;;;; parsing lives in optimizer-roadmap-doc-parse.lisp; the query API other
;;;; code calls is in optimizer-roadmap-query.lisp.
(in-package :cl-cc/optimize)

(defun %opt-roadmap-extracted-module-pathname (path)
  "Resolve PATH against the standalone system that now owns it, or NIL.
Legacy cl-cc-optimize test paths resolve to its consolidated boundary test."
  (loop for (prefix . system) in *opt-roadmap-extracted-package-systems*
        when (and
              (>= (length path) (length prefix))
              (string= prefix path :end2 (length prefix)))
        return
        (ignore-errors
         (let* ((relative-path (subseq path (length prefix)))
                (candidate
                 (if (and (eq system :cl-cc-optimize)
                          (>= (length relative-path) 6)
                          (string= "tests/" relative-path :end2 6))
                     "t/optimize-boundary-test.lisp"
                     relative-path)))
           (probe-file (asdf:system-relative-pathname system candidate))))))

(defun %opt-roadmap-module-present-p (path)
  "Return T when PATH identifies a checkout file."
  (labels ((present-p (candidate)
             (or
          (ignore-errors (probe-file (asdf:system-relative-pathname :cl-cc candidate)))
          (probe-file (merge-pathnames candidate (host-kit:getcwd)))
          (%opt-roadmap-extracted-module-pathname candidate))))
    (and
      (stringp path)
      (or
        (present-p path)
        (and
          (string= path "packages/optimize/src/optimizer-speculative-ic.lisp")
          (every
            #'present-p
            '("packages/optimize/src/optimizer-speculative-ic.lisp"
              "packages/optimize/src/optimizer-speculative-profile.lisp"
              "packages/optimize/src/optimizer-speculative-shape.lisp"
              "packages/optimize/src/optimizer-speculative-concurrency.lisp"
              "packages/optimize/src/optimizer-speculative-security.lisp"
              "packages/optimize/src/optimizer-speculative-wasm.lisp"
              "packages/optimize/src/optimizer-speculative-debug.lisp"
              "packages/optimize/src/optimizer-speculative-atomics.lisp"
              "packages/optimize/src/optimizer-speculative-peval.lisp"
              "packages/optimize/src/optimizer-speculative-passes.lisp")))))))

(defun %opt-roadmap-test-anchor-registered-p (anchor)
  "Return T when ANCHOR exactly names a test in the loaded cl-weave plan, or
when a plan path element starts with ANCHOR followed by a space.
Returns NIL when cl-weave or its public test-plan inspection API is unavailable,
so cl-weave remains an optional dependency."
  (when-let
    ((cl-weave-package (find-package :cl-weave)))
    (let* ((list-tests (find-symbol "LIST-TESTS" cl-weave-package))
           (test-plan-entry-path (find-symbol "TEST-PLAN-ENTRY-PATH" cl-weave-package))
           (anchor-name (symbol-name anchor))
           (anchor-length (length anchor-name)))
      (when (and
          list-tests
          test-plan-entry-path
          (fboundp list-tests)
          (fboundp test-plan-entry-path))
        (loop for entry in (funcall list-tests :stream (make-broadcast-stream) :name-filter nil)
              thereis (some
            (lambda (path-element)
              (%opt-roadmap-test-anchor-matches-element-p
                anchor-name
                anchor-length
                path-element))
            (funcall test-plan-entry-path entry)))))))

(defun %opt-roadmap-api-entry-fbound-p (entry)
  "Return T when ENTRY names a checkable API evidence target.
ENTRY may be a symbol in the current image or a cons of
(package-name-string . symbol-name-string) for external packages.
Evidence anchors include functions and special variables."
  (cond
    ((symbolp entry) (or (fboundp entry) (boundp entry)))
    ((and (consp entry) (stringp (car entry)) (stringp (cdr entry)))
      (let* ((pkg (find-package (car entry)))
             (sym (and pkg (find-symbol (cdr entry) pkg))))
        (and sym (or (fboundp sym) (boundp sym)))))
    (t nil)))

(defun optimize-roadmap-evidence-well-formed-p (evidence)
  "Return T when EVIDENCE is a checkable roadmap implementation record."
  (and
    evidence
    (member
      (opt-roadmap-evidence-status evidence)
      '(:implemented :partial :planned))
    (consp (opt-roadmap-evidence-modules evidence))
    (every #'%opt-roadmap-module-present-p (opt-roadmap-evidence-modules evidence))
    (consp (opt-roadmap-evidence-api-symbols evidence))
    (every
      #'%opt-roadmap-api-entry-fbound-p
      (opt-roadmap-evidence-api-symbols evidence))
    (every
      #'%opt-roadmap-test-anchor-registered-p
      (opt-roadmap-evidence-test-anchors evidence))
    (plusp (length (opt-roadmap-evidence-summary evidence)))))

(defun %opt-roadmap-feature-number (feature-id)
  "Return numeric part of FEATURE-ID, or NIL when it is malformed."
  (when (and (stringp feature-id) (>= (length feature-id) 4))
    (parse-integer feature-id :start 3 :junk-allowed t)))

(defun %opt-roadmap-evidence-status (feature)
  "Return FEATURE evidence status, mapping unmarked roadmap headings to :planned."
  (let ((status (opt-roadmap-feature-status feature)))
    (case status
      (:unknown :planned)
      (otherwise status))))

(defun %opt-roadmap-evidence-profile (feature-id)
  "Return module/API/test profile for FEATURE-ID.
The profile is intentionally coarse-grained by subsystem, but no longer uses a
single all-FR placeholder: each roadmap cluster points at the subsystem that
currently carries its implementation evidence."
  (let ((n (%opt-roadmap-feature-number feature-id)))
    (if-let
      ((entry
          (and
            n
            (find-if
              (lambda (e)
                (%opt-roadmap-range-covers-p e n))
              +opt-roadmap-evidence-profile-ranges+))))
      (destructuring-bind (_lo _hi modules api-symbols test-anchors) entry
        (declare (ignore _lo _hi))
        (values modules api-symbols test-anchors))
      (values
        '("packages/optimize/src/optimizer-pipeline.lisp"
          "packages/optimize/tests/optimizer-pipeline-tests.lisp")
        '(optimize-roadmap-doc-features optimize-roadmap-register-doc-evidence)
        '(optimize-roadmap-evidence-covers-doc-fr-list)))))

(defun %opt-roadmap-test-anchor-matches-element-p (anchor-name anchor-length path-element)
  (let* ((path-element-name (string path-element))
         (path-length (length path-element-name)))
    (and
      (<= anchor-length path-length)
      (string-equal anchor-name path-element-name :end2 anchor-length)
      (or
        (= anchor-length path-length)
        (char= #\Space (char path-element-name anchor-length))))))

(defun %opt-roadmap-range-covers-p (e n)
  (destructuring-bind (lo hi . _rest) e
    (declare (ignore _rest))
    (and (<= lo n) (or (null hi) (<= n hi)))))
