(in-package #:cl-cc/optimize/memory/test)

(it-sequential "optimizer-dead-store-elim-collections-removes-overwritten-aset"
  (let* ((store1 (make-vm-aset :array-reg :arr :index-reg :idx :val-reg :v1))
         (store2 (make-vm-aset :array-reg :arr :index-reg :idx :val-reg :v2))
         (ret (make-vm-ret :reg :arr))
         (out (cl-cc/optimize/memory:opt-pass-dead-store-elim (list store1 store2 ret))))
    (expect (member store1 out :test #'eq) :to-be-falsy)
    (expect (member store2 out :test #'eq) :to-be-truthy)))
