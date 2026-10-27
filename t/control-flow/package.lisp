(defpackage #:cl-cc/optimize/control-flow/test
            (:shadowing-import-from #:cl-weave #:describe)
            (:use #:cl #:cl-weave #:cl-cc/optimize/control-flow)
            (:import-from #:cl-cc/vm
                          #:make-vm-lt
                          #:make-vm-const
                          #:make-vm-jump-zero
                          #:make-vm-label
                          #:make-vm-not
                          #:make-vm-null-p
                          #:make-vm-ret
                          #:vm-lt
                          #:vm-le)
            (:import-from #:cl-cc-optimize-test-support
                          #:%make-test-basic-block))
