(defpackage #:cl-cc/optimize/pipeline/test
  (:shadowing-import-from #:cl-weave #:describe)
  (:use #:cl #:cl-weave #:cl-cc/optimize/pipeline)
  (:import-from #:cl-cc-optimize-test-support
                #:%make-test-basic-block
                #:%test-label-position
                #:make-bswap-tree-instructions)
  (:import-from #:cl-cc/vm
                #:instruction->sexp
                #:make-vm-ash
                #:make-vm-bswap
                #:make-vm-car
                #:make-vm-cdr
                #:make-vm-cons-p
                #:make-vm-dec
                #:make-vm-eq
                #:make-vm-format-inst
                #:make-vm-ge
                #:make-vm-gt
                #:make-vm-le
                #:make-vm-logand
                #:make-vm-logior
                #:make-vm-lognot
                #:make-vm-lt
                #:make-vm-make-string
                #:make-vm-mul
                #:make-vm-null-p
                #:make-vm-print
                #:make-vm-rplaca
                #:make-vm-rplacd
                #:vm-label
                #:vm-move
                #:vm-mul
                #:vm-jump
                #:vm-jump-zero))
