(defpackage #:cl-cc/optimize/rewriting/test
  (:shadowing-import-from #:cl-weave #:describe)
  (:use #:cl #:cl-weave #:cl-cc/optimize/rewriting)
  (:import-from #:cl-cc/bootstrap #:const #:var)
  (:import-from #:cl-cc/vm #:vm-const #:vm-move #:vm-add))
