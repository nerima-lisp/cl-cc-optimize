(defpackage #:cl-cc-optimize-test-support
  (:nicknames #:cl-cc/optimize/test-support)
  (:use #:cl)
  (:export #:%make-test-basic-block
           #:%test-label-position
           #:make-bswap-tree-instructions))
