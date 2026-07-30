;;;; t/package.lisp — test package for cl-cc-optimize
(defpackage :cl-cc-optimize/test (:use :cl)
  (:import-from :cl-weave #:describe-sequential #:it #:expect))
