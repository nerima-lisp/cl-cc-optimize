(defpackage #:cl-cc/optimize/speculation/test
            (:shadowing-import-from #:cl-weave #:describe)
            (:use #:cl #:cl-weave #:cl-cc/optimize/speculation)
            (:import-from #:cl-cc/vm
                          #:instruction->sexp
                          #:make-vm-lt
                          #:make-vm-mul
                          #:vm-label
                          #:vm-move
                          #:vm-mul))
