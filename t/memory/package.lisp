(defpackage #:cl-cc/optimize/memory/test
            (:shadowing-import-from #:cl-weave #:describe)
            (:use #:cl #:cl-weave #:cl-cc/optimize/memory)
            (:import-from #:cl-cc/vm
                          #:make-vm-lt
                          #:make-vm-ash
                          #:make-vm-car
                          #:make-vm-cdr
                          #:make-vm-dec
                          #:make-vm-logand
                          #:make-vm-logior
                          #:make-vm-logxor
                          #:make-vm-make-closure
                          #:make-vm-num-eq
                          #:make-vm-rplaca
                          #:vm-set-global-p
                          #:vm-get-global-p
                          #:make-vm-aset
                          #:make-vm-ret
                          #:vm-cons
                          #:vm-make-array
                          #:vm-closure
                          #:vm-make-closure
                          #:vm-add
                          #:vm-integer-add
                          #:vm-sub
                          #:vm-integer-sub
                          #:vm-mul
                          #:vm-integer-mul
                          #:vm-le
                          #:vm-ge
                          #:vm-eq)
            (:import-from #:cl-cc/optimize/analysis
                          #:opt-interval-add
                          #:opt-interval-sub))
