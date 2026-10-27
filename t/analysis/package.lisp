(defpackage #:cl-cc/optimize/analysis/test
            (:shadowing-import-from #:cl-weave #:describe)
            (:use #:cl #:cl-weave #:cl-cc/optimize/analysis)
            (:import-from #:cl-cc/vm #:make-vm-lt
                          #:make-vm-add
                          #:make-vm-call
                          #:make-vm-const
                          #:make-vm-func-ref
                          #:make-vm-jump
                          #:make-vm-jump-zero
                          #:make-vm-label
                          #:make-vm-move
                          #:make-vm-ret))
