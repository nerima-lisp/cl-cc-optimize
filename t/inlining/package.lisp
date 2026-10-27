(defpackage #:cl-cc/optimize/inlining/test
            (:shadowing-import-from #:cl-weave #:describe)
            (:use #:cl #:cl-weave #:cl-cc/optimize/inlining)
            (:import-from #:cl-cc/vm
                          #:instruction->sexp
                          #:make-vm-mul
                          #:vm-call
                          #:vm-dst
                          #:vm-move
                          #:vm-reg
                          #:vm-ret
                          #:vm-src))
