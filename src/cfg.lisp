(in-package :cl-cc/optimize)

;;; ─── Control Flow Graph (CFG) ────────────────────────────────────────────
;;;
;;; Constructs a CFG from a flat list of VM instructions by splitting at
;;; leaders (first instruction, jump targets, fall-through after branches).
;;;
;;; Reverse post-order, forward dominators, and loop-depth analysis are in
;;; cfg-dominance.lisp; the data dependence graph is in cfg-ddg.lisp (both
;;; load after this file).
;;;
;;; References:
;;;   Cooper, Harvey, Kennedy (2001). "A Simple, Fast Dominance Algorithm"
;;;   Cytron, Ferrante et al. (1991). "Efficiently Computing Static Single
;;;     Assignment Form and the Control Dependence Graph"

;;; ─── Data Structures ─────────────────────────────────────────────────────

(defstruct (basic-block (:conc-name bb-))
  "A maximal straight-line sequence of VM instructions with a single entry
   and single exit.  The block's control flow edges are stored as predecessor
   and successor lists of basic-block structs."
  (id       0   :type fixnum)          ; unique block ID (0 = entry)
  (label    nil)                        ; vm-label instruction opening this block, or NIL
  (instructions nil :type list)        ; list of vm-instruction (excluding the opening label)
  (predecessors nil :type list)        ; list of basic-block
  (successors   nil :type list)        ; list of basic-block
  (idom         nil)                   ; immediate dominator (basic-block or NIL for entry)
  (dom-children nil :type list)        ; blocks dominated by this block
  (dom-frontier nil :type list)        ; dominance frontier blocks
  (post-idom    nil)                   ; immediate post-dominator
  (post-children nil :type list)       ; blocks post-dominated by this block
  (vm-osr-entry nil)                   ; FR-521 OSR loop-header metadata/plist
  (loop-depth   0   :type fixnum)      ; nesting depth (0 = not in any loop)
  (rpo-index    0   :type fixnum))     ; index in reverse post-order

(defstruct (cfg (:conc-name cfg-))
  "Control Flow Graph for a single function / compilation unit.
   Blocks are identified by integer IDs.  Entry is always block 0."
  (blocks     (make-array 0 :adjustable t :fill-pointer 0) :type (vector *))
  (entry      nil)                     ; entry basic-block
  (exit       nil)                     ; exit basic-block (last ret/halt block)
  (label->block (make-hash-table :test #'equal) :type hash-table)
  (next-id    0   :type fixnum))

;;; ─── CFG Construction ────────────────────────────────────────────────────
;;; FR-017 (dependency): CFG construction provides the control-flow graph
;;; needed for alias analysis and memory disambiguation

(defun cfg-new-block (cfg &key label)
  "Allocate a new basic-block in CFG and return it."
  (let ((b (make-basic-block :id (cfg-next-id cfg) :label label)))
    (incf (cfg-next-id cfg))
    (vector-push-extend b (cfg-blocks cfg))
    (when label
      (setf (gethash (vm-name label) (cfg-label->block cfg)) b))
    b))

(defun cfg-add-edge (from to)
  "Add a directed edge FROM → TO in the CFG."
  (pushnew to (bb-successors from) :test #'eq)
  (pushnew from (bb-predecessors to) :test #'eq))

(defun %cfg-mark-leaders (vec n)
  "Single pass: return a bit array marking every leader in VEC.
   Leaders: index 0, any vm-label, fall-through after branch/ret/halt,
   and every explicit jump target."
  (let ((leader (make-array n :element-type 'bit :initial-element 0)))
    (setf (aref leader 0) 1)
    (loop for i from 0 below n
          for inst = (aref vec i)
          do (typecase inst
               (vm-label
                (setf (aref leader i) 1))
               ((or vm-jump vm-jump-zero)
                (when (< (1+ i) n) (setf (aref leader (1+ i)) 1))
                (let ((tgt (cfg-find-label-position vec n (vm-label-name inst))))
                  (when tgt (setf (aref leader tgt) 1))))
               ((or vm-ret vm-halt)
                (when (< (1+ i) n) (setf (aref leader (1+ i)) 1)))))
    leader))

(defun %cfg-fallthrough-edge (b next-start blocks-by-start)
  "Add a fall-through edge from B to the block starting at NEXT-START, if any."
  (let ((fall (and next-start (gethash next-start blocks-by-start))))
    (when fall (cfg-add-edge b fall))))

(defun %cfg-jump-target-edge (b inst g)
  "Add an unconditional jump edge from B to the explicit target of INST."
  (let ((tgt (cfg-get-block-by-label g (vm-label-name inst))))
    (when tgt (cfg-add-edge b tgt))))

(defun %cfg-connect-block (b insts g blocks-by-start next-start)
  "Wire outgoing edges for block B whose instructions are INSTS."
  (let ((term (find-if (lambda (i) (typep i '(or vm-jump vm-jump-zero vm-ret vm-halt)))
                       (reverse insts))))
    (typecase term
      (vm-jump
       (%cfg-jump-target-edge b term g))
      (vm-jump-zero
       (%cfg-jump-target-edge b term g)
       (%cfg-fallthrough-edge b next-start blocks-by-start))
      ((or vm-ret vm-halt) nil)
      (t
       (%cfg-fallthrough-edge b next-start blocks-by-start)))))

(defun %cfg-build-create-blocks (g vec n blocks-by-start leader)
  "Pass 2: allocate a basic block for each leader position in VEC."
  (loop for i from 0 below n
        when (= (aref leader i) 1)
        do (let ((cur-label (and (vm-label-p (aref vec i)) (aref vec i))))
             (unless (gethash i blocks-by-start)
               (setf (gethash i blocks-by-start) (cfg-new-block g :label cur-label))))))

(defun %cfg-build-collect-instructions (vec s e)
  "Collect instructions in VEC for the block spanning [S, E), skipping any opening label."
  (loop for j from s below e
        for inst = (aref vec j)
        unless (and (= j s) (vm-label-p inst))
        collect inst))

(defun %cfg-build-connect-blocks (g vec n blocks-by-start)
  "Pass 3: populate each block's instruction list and wire CFG edges."
  (let ((starts (sort (loop for k being the hash-keys of blocks-by-start collect k) #'<)))
    (loop for (s . rest) on starts
          do (let* ((e     (or (car rest) n))
                    (b     (gethash s blocks-by-start))
                    (insts (%cfg-build-collect-instructions vec s e)))
               (setf (bb-instructions b) insts)
               (%cfg-connect-block b insts g blocks-by-start (car rest))))))

(defun cfg-build (instructions)
  "Build a CFG from a flat list of VM INSTRUCTIONS.
Returns a cfg struct with all basic blocks, edges, entry, and exit set.

Algorithm:
  1. Mark leaders: index 0, every jump target, fall-throughs after branches.
  2. Allocate a basic block per leader.
  3. Populate each block's instruction list and wire fall-through / jump edges."
  (when (null instructions)
    (let* ((g (make-cfg)) (entry (cfg-new-block g)))
      (setf (cfg-entry g) entry (cfg-exit g) entry)
      (return-from cfg-build g)))
  (let* ((vec             (coerce instructions 'simple-vector))
         (n               (length vec))
         (leader          (%cfg-mark-leaders vec n))
         (g               (make-cfg))
         (blocks-by-start (make-hash-table)))
    (%cfg-build-create-blocks  g vec n blocks-by-start leader)
    (%cfg-build-connect-blocks g vec n blocks-by-start)
    (let* ((all-blocks (loop for b across (cfg-blocks g) collect b))
           (entry-b    (gethash 0 blocks-by-start))
           (exit-b     (or (find-if (lambda (b) (null (bb-successors b))) all-blocks)
                           (car (last all-blocks)))))
      (setf (cfg-entry g) entry-b (cfg-exit g) exit-b))
    g))

(defun cfg-find-label-position (vec n label-name)
  "Find the index of a vm-label with name LABEL-NAME in instruction vector VEC."
  (loop for i from 0 below n
        when (and (vm-label-p (aref vec i))
                  (equal (vm-name (aref vec i)) label-name))
        return i))

(defun cfg-get-block-by-label (cfg label-name)
  "Return the basic-block for the given LABEL-NAME, or NIL."
  (gethash label-name (cfg-label->block cfg)))

(defun cfg-mark-osr-loop-headers (cfg)
  "Annotate loop-header blocks with FR-521 VM OSR entry metadata.

The metadata is kept on BASIC-BLOCK via BB-VM-OSR-ENTRY so lower pipeline stages
can generate interpreter-vreg to JIT-preg mappings and OSR stubs only when
CL-CC/VM:*OSR-ENABLED* is true."
  (when cl-cc/vm:*osr-enabled*
    (let ((headers (make-hash-table :test #'eq)))
      (dolist (b (loop for block across (cfg-blocks cfg) collect block))
        (dolist (succ (bb-successors b))
          (when (cfg-dominates-p succ b)
            (setf (gethash succ headers) t))))
      (maphash (lambda (header _)
                 (declare (ignore _))
                 (setf (bb-vm-osr-entry header)
                       (list :block-id (bb-id header)
                             :label (and (bb-label header) (vm-name (bb-label header))))))
               headers)))
  cfg)
