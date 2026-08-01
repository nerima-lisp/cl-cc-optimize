;;;; optimizer-roadmap-query.lisp — roadmap document query API
;;;;
;;;; Public read API over optimizer-roadmap-doc-parse.lisp's parsed roadmap
;;;; documents: the feature list, FR-ID list, and (for the backend roadmap)
;;;; status summaries and completion queries.

(in-package :cl-cc/optimize)

(defun optimize-roadmap-doc-features (&optional (pathname (%opt-roadmap-doc-pathname)))
  "Parse docs/notes/optimize-passes.md and return all FR features in document order."
  (let ((features nil))
    (loop for line in (host-kit:split-string (host-kit:read-file-string pathname)
                                             :separator '(#\Newline))
          for line-no from 1
          when (%opt-roadmap-heading-p line)
          do (when-let ((feature-id (%opt-roadmap-fr-id-from-line line)))
               (push (make-opt-roadmap-feature
                      :id feature-id
                      :title (%opt-roadmap-title-from-line line)
                      :line line-no
                      :status (%opt-roadmap-status-from-line line)
                      :marked-complete-p (not (null (search "✅" line))))
                     features)))
    (nreverse features)))

(defun optimize-roadmap-doc-fr-ids (&optional (pathname (%opt-roadmap-doc-pathname)))
  "Return all optimize roadmap FR ids in document order."
  (mapcar #'opt-roadmap-feature-id (optimize-roadmap-doc-features pathname)))

(defun optimize-backend-roadmap-doc-features
    (&optional (pathname (%opt-backend-roadmap-doc-pathname)))
  "Parse docs/notes/optimize-backend.md and return all FR features in document order."
  (optimize-roadmap-doc-features pathname))

(defun optimize-backend-roadmap-doc-fr-ids
    (&optional (pathname (%opt-backend-roadmap-doc-pathname)))
  "Return all optimize-backend roadmap FR ids in document order."
  (mapcar #'opt-roadmap-feature-id
          (optimize-backend-roadmap-doc-features pathname)))

(defun optimize-backend-roadmap-status-summary
    (&optional (pathname (%opt-backend-roadmap-doc-pathname)))
  "Return status counts for docs/notes/optimize-backend.md FR headings.

Returned plist keys:
  :total        total FR heading count
  :implemented  count of ✅ headings
  :partial      count of 🔶 headings
  :planned      count of explicit ❌ headings
  :unknown      count of unmarked headings"
  (let ((implemented 0)
        (partial 0)
        (planned 0)
        (unknown 0)
        (total 0))
    (dolist (feature (optimize-backend-roadmap-doc-features pathname))
      (incf total)
      (case (opt-roadmap-feature-status feature)
        (:implemented (incf implemented))
        (:partial (incf partial))
        (:planned (incf planned))
        (otherwise (incf unknown))))
    (list :total total
          :implemented implemented
          :partial partial
          :planned planned
          :unknown unknown)))

(defun optimize-backend-roadmap-all-fr-complete-p
    (&optional (pathname (%opt-backend-roadmap-doc-pathname)))
  "Return T only when every optimize-backend FR is marked ✅ and has complete evidence."
  (let* ((summary (optimize-backend-roadmap-status-summary pathname))
         (features (optimize-backend-roadmap-doc-features pathname)))
    (and (plusp (getf summary :total 0))
         (= (getf summary :implemented 0)
            (getf summary :total 0))
         (every (lambda (feature)
                  (and (eq (opt-roadmap-feature-status feature) :implemented)
                       (optimize-backend-roadmap-implementation-evidence-complete-p
                        (lookup-opt-backend-roadmap-evidence
                         (opt-roadmap-feature-id feature)))))
                features))))

(defun optimize-backend-roadmap-fr-ids-by-status
    (status &optional (pathname (%opt-backend-roadmap-doc-pathname)))
  "Return optimize-backend FR IDs filtered by STATUS.

Accepted STATUS keywords: :implemented, :partial, :planned, :unknown."
  (check-type status (member :implemented :partial :planned :unknown))
  (let ((ids nil))
    (dolist (feature (optimize-backend-roadmap-doc-features pathname))
      (when (eq (opt-roadmap-feature-status feature) status)
        (push (opt-roadmap-feature-id feature) ids)))
    (nreverse ids)))
