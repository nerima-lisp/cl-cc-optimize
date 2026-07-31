;;;; optimizer-report.lisp — shared optimizer pass debug-reporting utility

(in-package :cl-cc/optimize)

(defvar *optimization-report-stream* nil
  "When non-NIL, optimizer passes emit one-line optimization reports here.")

(defun %opt-report (kind control &rest args)
  "Emit one optimizer debugging report line when reporting is enabled."
  (when *optimization-report-stream*
    (format *optimization-report-stream* "~&opt-report ~A " kind)
    (apply #'format *optimization-report-stream* control args)
    (terpri *optimization-report-stream*)))
