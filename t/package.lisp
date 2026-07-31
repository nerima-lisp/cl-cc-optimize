;;;; t/package.lisp — test package for cl-cc-optimize
(defpackage #:cl-cc-optimize/test
  (:use #:cl)
  (:import-from #:cl-weave
    #:describe-sequential #:it #:it-todo #:expect
    #:it-each #:it-property #:it-fuzz
    #:gen-integer #:gen-list #:gen-such-that))
