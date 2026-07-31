(in-package :cl-cc/optimize)

;;; FR-523..FR-528 planning helpers (backend roadmap evidence anchors)

(defun %opt-find-label-index (vec name &optional (start 0))
  (loop for i from start below (length vec)
        for inst = (aref vec i)
        when (and (typep inst 'vm-label)
                  (equal (vm-name inst) name))
        do (return i)))

(defun %opt-parse-canonical-loop-at (vec i)
  "Parse canonical loop shape at label index I.

Expected shape:
  Lh: cmp/jz body step jump Lh Lexit:
where cmp is vm-lt and step is self-update vm-add on induction variable.
Returns OPT-CANONICAL-LOOP or NIL."
  (when (and (< (+ i 5) (length vec))
             (typep (aref vec i) 'vm-label)
             (typep (aref vec (1+ i)) 'vm-lt)
             (typep (aref vec (+ i 2)) 'vm-jump-zero))
    (let* ((head (aref vec i))
           (cmp  (aref vec (1+ i)))
           (jz   (aref vec (+ i 2)))
           (head-label (vm-name head))
           (exit-label (vm-label-name jz))
           (exit-idx (%opt-find-label-index vec exit-label (+ i 3))))
      (when (and exit-idx (> exit-idx (+ i 4)))
        (let* ((back-idx (1- exit-idx))
               (back (aref vec back-idx)))
          (when (and (typep back 'vm-jump)
                     (equal (vm-label-name back) head-label))
            (let* ((body (loop for k from (+ i 3) below back-idx
                               collect (aref vec k)))
                   (step (car (last body))))
              (when (and (typep step 'vm-add)
                         (eq (vm-dst step) (vm-lhs step))
                         (eq (vm-dst step) (vm-lhs cmp))
                         (eq (vm-reg jz) (vm-dst cmp)))
                (make-opt-canonical-loop
                 :head-index i
                 :cmp-index (1+ i)
                 :jz-index (+ i 2)
                 :back-index back-idx
                 :exit-index exit-idx
                 :head-label head-label
                 :exit-label exit-label
                 :iv-reg (vm-lhs cmp)
                 :limit-reg (vm-rhs cmp)
                 :step-reg (vm-rhs step)
                 :cond-reg (vm-dst cmp)
                 :body body)))))))))

(defun %opt-find-canonical-loops (instructions)
  (let* ((vec (coerce instructions 'vector))
         (loops nil)
         (i 0)
         (n (length vec)))
    (loop while (< i n)
          do (if-let ((lp (%opt-parse-canonical-loop-at vec i)))
               (progn
                 (push lp loops)
                 (setf i (1+ (opt-loop-exit-index lp))))
               (incf i)))
    (nreverse loops)))

(defun opt-build-affine-loop-summary (&key induction-vars bounds accesses)
  "Build a conservative affine-loop summary descriptor."
  (list :kind :affine-loop-summary
        :induction-vars (copy-list induction-vars)
        :bounds (copy-list bounds)
        :accesses (copy-list accesses)))

(defun %opt-access-kind (inst)
  (typecase inst
    (vm-get-global :read-global)
    (vm-set-global :write-global)
    (vm-slot-read :read-slot)
    (vm-slot-write :write-slot)
    (t nil)))

(defun %opt-loop-core-and-step (lp)
  (let* ((body (opt-loop-body lp))
         (step (car (last body)))
         (core (butlast body)))
    (values core step)))

(defun %opt-loop-constant-init (vec lp)
  "Return last dominating integer init for loop IV, or NIL when uncertain."
  (let* ((iv (opt-loop-iv-reg lp))
         (value nil))
    (loop for i from 0 below (opt-loop-head-index lp)
          for inst = (aref vec i)
          do (cond
               ((and (typep inst 'vm-const)
                     (eq (vm-dst inst) iv)
                     (integerp (vm-value inst)))
                (setf value (vm-value inst)))
               ((and (opt-inst-dst inst) (eq (opt-inst-dst inst) iv))
                (setf value nil))))
    value))

(defun %opt-inst-depends-on-p (producer consumer)
  (let ((dst (opt-inst-dst producer)))
    (and dst (member dst (opt-inst-read-regs consumer) :test #'eq))))

(defun %opt-schedule-core-with-deps (core)
  "Dependency-aware local reordering: only swap adjacent independent ops by cost." 
  (let ((vec (coerce (copy-list core) 'vector))
        (changed nil))
    (loop for i from 0 below (1- (length vec))
          do (let* ((a (aref vec i))
                    (b (aref vec (1+ i))))
               (when (and (not (%opt-inst-depends-on-p a b))
                          (not (%opt-inst-depends-on-p b a))
                          (< (opt-inline-inst-cost b) (opt-inline-inst-cost a)))
                 (rotatef (aref vec i) (aref vec (1+ i)))
                 (setf changed t))))
    (values (coerce vec 'list) changed)))

(defun %opt-affine-loop-access (inst)
  "Return an access descriptor plist for INST when it is a recognized
global/slot read or write, else NIL."
  (let ((kind (%opt-access-kind inst)))
    (and kind (list :kind kind :inst inst))))

(defun %opt-affine-loop-summary-for (lp)
  "Build an affine-loop summary for canonical loop LP."
  (let ((accesses (remove nil (mapcar #'%opt-affine-loop-access (opt-loop-body lp)))))
    (opt-build-affine-loop-summary
     :induction-vars (list (opt-loop-iv-reg lp))
     :bounds (list (list :lt (opt-loop-iv-reg lp) (opt-loop-limit-reg lp)))
     :accesses accesses)))

(defun opt-pass-affine-loop-analysis (instructions)
  "Analyze canonical loops and cache affine summaries for later passes.

This pass preserves instructions but computes real summaries from detected loop
regions (not from caller-provided payload lists)."
  (let ((loops (%opt-find-canonical-loops instructions)))
    (setf *opt-last-affine-loop-summaries* (mapcar #'%opt-affine-loop-summary-for loops))
    instructions))

(defun opt-loop-interchange-plan (&key loops cache-locality-score dependence-safe-p)
  "Return an interchange plan when dependence safety is proven."
  (list :kind :loop-interchange
        :applied-p (and dependence-safe-p (plusp (or cache-locality-score 0)))
        :loops (copy-list loops)
        :dependence-safe-p (not (null dependence-safe-p))
        :cache-locality-score (or cache-locality-score 0)))

(defun %opt-loop-interchange-swapped-core (core)
  "Return (values REWRITTEN CHANGED-P): CORE with its first two instructions
swapped when both are CSE-eligible and independent of each other, or CORE
unchanged (CHANGED-P NIL) otherwise."
  (if (and (>= (length core) 2)
           (opt-inst-cse-eligible-p (first core))
           (opt-inst-cse-eligible-p (second core))
           (not (%opt-inst-depends-on-p (first core) (second core)))
           (not (%opt-inst-depends-on-p (second core) (first core))))
      (values (cons (second core) (cons (first core) (cddr core))) t)
      (values core nil)))

(defun %opt-loop-interchange-emit-loop (vec i lp emit)
  "Emit LP's canonical loop found at position I in VEC via the EMIT
function, swapping its core operations when independent. Return (values
NEXT-I SWAPPED-P): the index just past the loop, and whether the core was
swapped."
  (multiple-value-bind (core step) (%opt-loop-core-and-step lp)
    (multiple-value-bind (rewritten swapped-p) (%opt-loop-interchange-swapped-core core)
      (funcall emit (aref vec i))
      (funcall emit (aref vec (1+ i)))
      (funcall emit (aref vec (+ i 2)))
      (dolist (inst rewritten) (funcall emit inst))
      (funcall emit step)
      (funcall emit (aref vec (opt-loop-back-index lp)))
      (funcall emit (aref vec (opt-loop-exit-index lp)))
      (values (1+ (opt-loop-exit-index lp)) swapped-p))))

(defun opt-pass-loop-interchange (instructions)
  "Apply conservative loop-body interchange via independent core-op swap.

This is intentionally strict: only pure/independent core operations are swapped.
Control instructions and IV update remain untouched."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (out nil)
         (changed nil)
         (i 0))
    (labels ((emit (x) (push x out)))
      (loop while (< i n)
            do (let ((lp (%opt-parse-canonical-loop-at vec i)))
                 (if lp
                     (multiple-value-bind (new-i swapped-p)
                         (%opt-loop-interchange-emit-loop vec i lp #'emit)
                       (setf i new-i)
                       (when swapped-p (setf changed t)))
                     (progn (emit (aref vec i)) (incf i)))))
      (if changed (nreverse out) instructions))))

(defun opt-polyhedral-schedule-plan (&key statements constraints objective)
  "Return a conservative polyhedral schedule planning descriptor."
  (list :kind :polyhedral-schedule
        :statements (copy-list statements)
        :constraints (copy-list constraints)
        :objective (or objective :latency-min)))

(defun opt-pass-polyhedral-schedule (instructions)
  "Apply a conservative schedule optimization inside canonical loops.

Current subset: reorder loop body pure operations by ascending static cost,
leaving control-flow and induction update in place."
  (let* ((vec (coerce instructions 'vector))
         (out nil)
         (changed nil)
         (i 0)
         (n (length vec)))
    (labels ((emit (x) (push x out)))
      (loop while (< i n)
            do (let ((lp (%opt-parse-canonical-loop-at vec i)))
                 (if lp (let* ((body (opt-loop-body lp))
                            (step (car (last body)))
                            (body-core (butlast body))
                            (sortable (every #'opt-inst-cse-eligible-p body-core))
                            (sorted body-core)
                            (sorted-changed nil)
                            (plan (opt-polyhedral-schedule-plan
                                   :statements body-core
                                   :constraints (list :canonical-loop)
                                   :objective :latency-min)))
                        (declare (ignore plan))
                       (when sortable
                         (multiple-value-setq (sorted sorted-changed)
                           (%opt-schedule-core-with-deps body-core)))
                       (when sorted-changed
                         (setf changed t))
                       (emit (aref vec i))
                       (emit (aref vec (1+ i)))
                       (emit (aref vec (+ i 2)))
                       (dolist (inst sorted)
                         (emit inst))
                       (emit step)
                       (emit (aref vec (opt-loop-back-index lp)))
                       (emit (aref vec (opt-loop-exit-index lp)))
                       (setf i (1+ (opt-loop-exit-index lp)))) (progn
                       (emit (aref vec i))
                       (incf i)))))
      (if changed (nreverse out) instructions))))

(defun opt-loop-fusion-fission-plan (&key loops register-pressure instruction-budget)
  "Choose loop fusion/fission strategy from simple pressure/budget heuristics."
  (let* ((pressure (or register-pressure 0))
         (budget (or instruction-budget 0))
         (strategy (cond ((and (> pressure 32) (plusp budget)) :fission)
                         ((and (<= pressure 32) (plusp budget)) :fusion)
                         (t :none))))
    (list :kind :loop-fusion-fission
          :strategy strategy
          :loops (copy-list loops)
          :register-pressure pressure
          :instruction-budget budget)))

(defun %opt-loop-fusion-loop-seq (vec lp)
  "Return the list of VEC instructions spanning LP's canonical loop, from its
header through its exit index, inclusive."
  (loop for k from (opt-loop-head-index lp) to (opt-loop-exit-index lp)
        collect (aref vec k)))

(defun %opt-loop-fusion-pure-core (lp)
  "Return LP's pure core instructions, discarding the accompanying induction
step instruction."
  (multiple-value-bind (core step) (%opt-loop-core-and-step lp)
    (declare (ignore step))
    core))

(defun %opt-loop-fusion-pure-loop-p (lp)
  "True when every instruction in LP's pure core is CSE-eligible, i.e. LP is
safe to fuse or fission."
  (every #'opt-inst-cse-eligible-p (%opt-loop-fusion-pure-core lp)))

(defun %opt-loop-fusion-same-iter-space-p (vec a b)
  "True when canonical loops A and B iterate the same space via distinct
induction registers: same limit register, step register, and constant
initial value."
  (and (not (eq (opt-loop-iv-reg a) (opt-loop-iv-reg b)))
       (equal (opt-loop-limit-reg a) (opt-loop-limit-reg b))
       (equal (opt-loop-step-reg a) (opt-loop-step-reg b))
       (equal (%opt-loop-constant-init vec a) (%opt-loop-constant-init vec b))))

(defun %opt-loop-fusion-fuse-adjacent (vec lp lp2 emit)
  "Fuse canonical loops LP and LP2, which share an iteration space, into one
loop whose body is LP's core followed by LP2's core, with LP2's induction
register renamed to LP's. Emit the fused loop via EMIT and return the index
just past LP2's exit."
  (multiple-value-bind (core-a step-a) (%opt-loop-core-and-step lp)
    (multiple-value-bind (core-b step-b) (%opt-loop-core-and-step lp2)
      (declare (ignore step-b))
      (let ((m (make-hash-table :test #'eq)))
        (setf (gethash (opt-loop-iv-reg lp2) m) (opt-loop-iv-reg lp))
        (funcall emit (aref vec (opt-loop-head-index lp)))
        (funcall emit (aref vec (opt-loop-cmp-index lp)))
        (funcall emit (aref vec (opt-loop-jz-index lp)))
        (dolist (inst core-a) (funcall emit inst))
        (dolist (inst core-b) (funcall emit (opt-rewrite-inst-regs inst m)))
        (funcall emit step-a)
        (funcall emit (aref vec (opt-loop-back-index lp)))
        (funcall emit (aref vec (opt-loop-exit-index lp2)))
        (1+ (opt-loop-exit-index lp2))))))

(defun %opt-loop-fusion-fission-oversized (vec lp core emit)
  "When LP's pure CORE exceeds the fission size threshold, split it in half
at a synthetic split label and emit both halves; otherwise emit LP
unchanged. Return (values NEXT-I CHANGED-P): the index just past LP's exit,
and whether a split occurred."
  (if (and (%opt-loop-fusion-pure-loop-p lp) (> (length core) 24))
      (let* ((half (floor (length core) 2))
             (core-a (subseq core 0 half))
             (core-b (subseq core half))
             (split-label
               (make-vm-label
                :name (intern (format nil "~A__SPLIT"
                                       (vm-name (aref vec (opt-loop-head-index lp))))
                               :keyword))))
        (funcall emit (aref vec (opt-loop-head-index lp)))
        (funcall emit (aref vec (opt-loop-cmp-index lp)))
        (funcall emit (aref vec (opt-loop-jz-index lp)))
        (dolist (inst core-a) (funcall emit inst))
        (funcall emit split-label)
        (dolist (inst core-b) (funcall emit inst))
        (funcall emit (car (last (opt-loop-body lp))))
        (funcall emit (aref vec (opt-loop-back-index lp)))
        (funcall emit (aref vec (opt-loop-exit-index lp)))
        (values (1+ (opt-loop-exit-index lp)) t))
      (progn
        (dolist (inst (%opt-loop-fusion-loop-seq vec lp)) (funcall emit inst))
        (values (1+ (opt-loop-exit-index lp)) nil))))

(defun %opt-loop-fusion-fission-step (vec lp lp2 emit)
  "Advance past canonical loop LP (with successor LP2, if any): fuse LP and
LP2 when both are pure and share an iteration space, otherwise fission LP
alone if its core is oversized, otherwise emit LP unchanged via EMIT.
Return (values NEXT-I CHANGED-P)."
  (if (and lp2
           (%opt-loop-fusion-pure-loop-p lp)
           (%opt-loop-fusion-pure-loop-p lp2)
           (%opt-loop-fusion-same-iter-space-p vec lp lp2))
      (values (%opt-loop-fusion-fuse-adjacent vec lp lp2 emit) t)
      (%opt-loop-fusion-fission-oversized vec lp (%opt-loop-fusion-pure-core lp) emit)))

(defun opt-pass-loop-fusion-fission (instructions)
  "Apply conservative loop fusion/fission on canonical loops.

Fusion: adjacent loops with identical headers are merged into one loop body.
Fission: oversized loop body is split into two core regions in the same loop
         using a conservative split marker label."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (out nil)
         (changed nil)
         (i 0))
    (labels ((emit (x) (push x out)))
      (loop while (< i n)
            do (let ((lp (%opt-parse-canonical-loop-at vec i)))
                 (if lp
                     (let* ((next-i (1+ (opt-loop-exit-index lp)))
                            (lp2 (and (< next-i n)
                                      (%opt-parse-canonical-loop-at vec next-i))))
                       (multiple-value-bind (new-i change-p)
                           (%opt-loop-fusion-fission-step vec lp lp2 #'emit)
                         (setf i new-i)
                         (when change-p (setf changed t))))
                     (progn (emit (aref vec i)) (incf i)))))
      (if changed (nreverse out) instructions))))
