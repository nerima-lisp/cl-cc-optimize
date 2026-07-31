(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer Driver — Pass Pipeline Spec Parsing
;;;
;;; Split out of optimizer-driver.lisp, which resolves the parsed pipeline
;;; via opt-resolve-pass-pipeline (loads after this file).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── Pass Pipeline Parsing ───────────────────────────────────────────────

;;; The --pass-pipeline spec ("sccp,cse,dce") is tokenized and parsed with the
;;; external cl-parser-kit library instead of a hand-rolled comma split: a
;;; whitespace-skipping tokenizer emits pass-name and comma tokens, and a
;;; sep-by combinator parses the comma-separated pass list.  This makes the
;;; grammar explicit and whitespace-tolerant (" sccp , cse " parses cleanly).

(defparameter *opt-pass-pipeline-tokenizer*
  (cl-parser-kit:make-tokenizer
   :rules (list (cl-parser-kit:make-whitespace-rule :skip-p t)
                (cl-parser-kit:make-literal-rule :comma ",")
                (cl-parser-kit:make-identifier-rule
                 :type :pass
                 :start-predicate (lambda (c) (or (alpha-char-p c) (char= c #\_)))
                 :continue-predicate (lambda (c)
                                       (or (alphanumericp c)
                                           (char= c #\-)
                                           (char= c #\_))))))
  "cl-parser-kit tokenizer for optimizer pipeline specs: skips whitespace and
emits :pass identifier tokens (letters/digits/-/_) separated by :comma.")

(defparameter *opt-pass-pipeline-parser*
  (cl-parser-kit:sep-by (cl-parser-kit:type-token-text :pass)
                        (cl-parser-kit:literal "," :type :comma))
  "cl-parser-kit parser: a comma-separated list of pass-name texts.")

(defun opt-parse-pass-pipeline-string (text)
  "Parse a comma-separated optimizer pipeline string into keyword pass names,
tokenized and parsed with cl-parser-kit."
  (multiple-value-bind (ok value)
      (cl-parser-kit:parse-source *opt-pass-pipeline-parser*
                                  text
                                  *opt-pass-pipeline-tokenizer*)
    (when ok
      (remove nil
              (mapcar (lambda (name)
                        (and (plusp (length name))
                             (intern (string-upcase name) :keyword)))
                      value)))))

(defun opt-resolve-pass-pipeline (pipeline)
  "Resolve PIPELINE into a list of pass functions."
  (cond
    ((null pipeline) *opt-convergence-passes*)
    ((stringp pipeline) (opt-resolve-pass-pipeline (opt-parse-pass-pipeline-string pipeline)))
    ((every #'functionp pipeline) pipeline)
    (t
     (mapcar (lambda (entry)
               (or (and (keywordp entry) (gethash entry *opt-pass-registry*))
                   (error "Unknown optimizer pass ~S" entry)))
             pipeline))))
