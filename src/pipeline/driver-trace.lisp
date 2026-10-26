(in-package :cl-cc/optimize)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer Driver — Reporting and Trace State
;;;
;;; Split out of optimizer-driver.lisp; the convergence loop, policy, and
;;; public entry point (optimize-instructions) are in optimizer-driver.lisp
;;; (loads after this file).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── Reporting / Trace State ─────────────────────────────────────────────

(defstruct (opt-reporting-options (:conc-name opt-report-))
  "Read-only bundle of side-channel reporting flags for the optimizer pipeline."
  (print-pass-timings nil)
  (timing-stream      nil)
  (print-pass-stats   nil)
  (stats-stream       nil)
  (print-opt-remarks  nil)
  (opt-remarks-stream nil)
  (opt-remarks-mode   :all))

(defstruct (opt-trace-state (:conc-name opt-trace-))
  "Mutable accumulator for Chrome-trace-compatible events."
  (enabled     nil)
  (json-stream nil)
  (events      nil)
  (ts-us        0))

;;; ─── Chrome Trace / FR-703 Analytics ────────────────────────────────────

(defun %opt-write-trace-json (stream events)
  "Write Chrome-trace-compatible JSON EVENTS to STREAM."
  (format stream "{\"traceEvents\":[")
  (loop for event in events
        for i from 0
        do (when (plusp i) (format stream ","))
           (format stream
                   "{\"name\":~S,\"ph\":\"X\",\"pid\":1,\"tid\":1,\"ts\":~D,\"dur\":~D}"
                   (getf event :name)
                   (getf event :ts-us)
                   (getf event :dur-us)))
  (format stream "]}~%"))

(defun compiler-self-profiling-capabilities ()
  "Return FR-703 Compiler Self-Profiling / Build Analytics capabilities."
  '(:fr-id :fr-703
    :time-passes t
    :stats t
    :trace-emit :chrome-trace-json
    :build-analytics t))

(defun build-analytics-summary (&key pass-count instruction-count elapsed-us changed-count)
  "Build a compact FR-703 build analytics summary plist."
  (list :fr-id :fr-703
        :pass-count (or pass-count 0)
        :instruction-count (or instruction-count 0)
        :elapsed-us (or elapsed-us 0)
        :changed-count (or changed-count 0)
        :capabilities (compiler-self-profiling-capabilities)))

;;; ─── Per-Pass Reporting ──────────────────────────────────────────────────

(defun %opt-pass-name-string (f)
  (string-upcase (princ-to-string f)))

(defun %opt-remarks-applies-p (changed mode)
  "T when a remarks entry should be emitted given CHANGED status and MODE."
  (or (eq mode :all)
      (and changed      (eq mode :changed))
      (and (not changed)(eq mode :missed))))

(defun %opt-emit-pass-report (f before next elapsed-s dur-us reporting trace)
  "Emit timing, stats, remarks, and trace events for one pass application.
Separated from the core loop so reporting concerns are independently extensible.
F is the pass function; BEFORE/NEXT are instruction lists; ELAPSED-S is wall time
in seconds; DUR-US is duration in microseconds; REPORTING is an opt-reporting-options;
TRACE is an opt-trace-state (mutated in place)."
  (let* ((before-count (length before))
         (after-count  (length next))
         (changed      (not (opt-converged-p before next)))
         (name         (%opt-pass-name-string f)))
    (when (opt-report-print-pass-timings reporting)
      (format (opt-report-timing-stream reporting) "~A: ~,6Fs~%" f elapsed-s))
    (when (opt-report-print-pass-stats reporting)
      (format (opt-report-stats-stream reporting)
              "~A: before=~D after=~D delta=~D changed=~A~%"
              f before-count after-count (- after-count before-count)
              (if changed "yes" "no")))
    (when (and (opt-report-print-opt-remarks reporting)
               (%opt-remarks-applies-p changed (opt-report-opt-remarks-mode reporting)))
      (format (opt-report-opt-remarks-stream reporting)
              "~A: ~A~%" f (if changed "changed" "missed")))
    (when (opt-trace-enabled trace)
      (push (list :name name :ts-us (opt-trace-ts-us trace) :dur-us dur-us)
            (opt-trace-events trace))
      (incf (opt-trace-ts-us trace) dur-us))))
