(in-package #:cl-cc-optimize-test-support)

;;; ─── Test helpers ────────────────────────────────────────────────────────

(defun %make-test-basic-block (&key (successors nil) (predecessors nil)
                                     (loop-depth 0) (rpo-index 0)
                                     (instructions nil))
  "Build a minimal basic-block for testing."
  (cl-cc/optimize/analysis:make-basic-block
   :successors successors
   :predecessors predecessors
   :loop-depth loop-depth
    :rpo-index rpo-index
    :instructions instructions))

(defun %test-label-position (instructions label-name)
  "Return LABEL-NAME's position in INSTRUCTIONS, or NIL."
  (position-if (lambda (inst)
                 (and (typep inst 'cl-cc/vm::vm-label)
                      (equal (cl-cc/vm::vm-name inst) label-name)))
               instructions))

(defun make-bswap-tree-instructions ()
  "Build the canonical 32-bit byte-swap instruction tree."
  (list (cl-cc/vm:make-vm-const :dst :mask0 :value #xFF)
        (cl-cc/vm:make-vm-logand :dst :and0 :lhs :src :rhs :mask0)
        (cl-cc/vm:make-vm-const :dst :shift0 :value 24)
        (cl-cc/vm:make-vm-ash :dst :byte0 :lhs :and0 :rhs :shift0)
        (cl-cc/vm:make-vm-const :dst :mask1 :value #xFF00)
        (cl-cc/vm:make-vm-logand :dst :and1 :lhs :src :rhs :mask1)
        (cl-cc/vm:make-vm-const :dst :shift1 :value 8)
        (cl-cc/vm:make-vm-ash :dst :byte1 :lhs :and1 :rhs :shift1)
        (cl-cc/vm:make-vm-const :dst :mask2 :value #xFF0000)
        (cl-cc/vm:make-vm-logand :dst :and2 :lhs :src :rhs :mask2)
        (cl-cc/vm:make-vm-const :dst :shift2 :value -8)
        (cl-cc/vm:make-vm-ash :dst :byte2 :lhs :and2 :rhs :shift2)
        (cl-cc/vm:make-vm-const :dst :mask3 :value #xFF000000)
        (cl-cc/vm:make-vm-logand :dst :and3 :lhs :src :rhs :mask3)
        (cl-cc/vm:make-vm-const :dst :shift3 :value -24)
        (cl-cc/vm:make-vm-ash :dst :byte3 :lhs :and3 :rhs :shift3)
        (cl-cc/vm:make-vm-logior :dst :pair0 :lhs :byte0 :rhs :byte1)
        (cl-cc/vm:make-vm-logior :dst :pair1 :lhs :byte2 :rhs :byte3)
        (cl-cc/vm:make-vm-logior :dst :dst :lhs :pair0 :rhs :pair1)))
