(defpackage #:cl-cc/optimize/loops/test
            (:shadowing-import-from #:cl-weave #:describe)
            (:use #:cl #:cl-weave #:cl-cc/optimize/loops)
            (:import-from #:cl-cc/vm
                          #:make-vm-eq
                          #:make-vm-ge
                          #:make-vm-le
                          #:make-vm-lt
                          #:make-vm-mul
                          #:make-vm-array-length
                          #:make-vm-ash
                          #:make-vm-logand
                          #:make-vm-print
                          #:make-vm-add
                          #:make-vm-call
                          #:make-vm-func-ref
                          #:make-vm-logior
                          #:make-vm-logxor
                          #:make-vm-max
                          #:make-vm-min
                          #:make-vm-sub)
            (:import-from #:cl-cc-optimize-test-support #:%test-label-position))
