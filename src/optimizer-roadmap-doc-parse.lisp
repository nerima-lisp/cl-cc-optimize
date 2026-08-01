;;;; optimizer-roadmap-doc-parse.lisp — roadmap markdown doc parsing
;;;;
;;;; Line-oriented parsing of the optimizer and backend roadmap markdown
;;;; documents: heading detection, and extracting each feature's FR-ID,
;;;; title, and status from a heading line. Consumed by
;;;; optimizer-roadmap-query.lisp.

(in-package :cl-cc/optimize)

(defun %opt-roadmap-doc-pathname ()
  "Return the canonical docs/notes/optimize-passes.md pathname when available."
  (let ((checkout-path (merge-pathnames #P"docs/notes/optimize-passes.md" (host-kit:getcwd))))
    (or (probe-file checkout-path)
        (ignore-errors (asdf:system-relative-pathname :cl-cc "docs/notes/optimize-passes.md"))
        checkout-path)))

(defun %opt-backend-roadmap-doc-pathname ()
  "Return the canonical docs/notes/optimize-backend.md pathname when available."
  (let ((checkout-path (merge-pathnames #P"docs/notes/optimize-backend.md" (host-kit:getcwd))))
    (or (probe-file checkout-path)
        (ignore-errors (asdf:system-relative-pathname :cl-cc "docs/notes/optimize-backend.md"))
        checkout-path)))

(defun %opt-roadmap-heading-p (line)
  "Return T when LINE is an optimize roadmap FR heading."
  (and (>= (length line) 7)
       (string= "####" (subseq line 0 4))
       (search "FR-" line)))

(defun %opt-roadmap-fr-id-from-line (line)
  "Extract the FR-#### id from LINE."
  (when-let ((pos (search "FR-" line)))
    (let ((end (+ pos 3)))
      (loop while (and (< end (length line))
                       (digit-char-p (char line end)))
            do (incf end))
      (when (> end (+ pos 3))
        (subseq line pos end)))))

(defun %opt-roadmap-trim-title (text)
  "Normalize a roadmap heading title."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (let* ((marks (remove nil
                                      (mapcar (lambda (marker) (search marker text))
                                              '("✅" "🔶" "❌"))))
                      (mark (and marks (reduce #'min marks))))
                 (if mark (subseq text 0 mark) text))))

(defun %opt-roadmap-status-from-line (line)
  "Return the implementation status marker encoded in roadmap heading LINE."
  (cond
    ((search "✅" line) :implemented)
    ((search "🔶" line) :partial)
    ((search "❌" line) :planned)
    (t :unknown)))

(defun %opt-roadmap-title-from-line (line)
  "Extract the human-readable title from an FR heading LINE."
  (let* ((colon (position #\: line))
         (start (and colon (1+ colon))))
    (%opt-roadmap-trim-title (if start (subseq line start) line))))
