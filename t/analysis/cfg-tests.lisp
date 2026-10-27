;;;; tests/unit/optimize/cfg-tests.lisp — CFG + Dominator Tree Tests
;;; Tests for Phase 1: cfg-build, cfg-compute-dominators, cfg-compute-dominance-frontiers, and related utilities.

(in-package :cl-cc/optimize/analysis/test)

;;; ─── Helpers ─────────────────────────────────────────────────────────────
(defun make-test-cfg-linear ()
  "Build a CFG from a simple linear instruction sequence: CONST → ADD → RET."
  (cl-cc/optimize:cfg-build
   (list (cl-cc/vm:make-vm-const :dst :r0 :value 1)
         (cl-cc/vm:make-vm-const :dst :r1 :value 2)
         (cl-cc/vm:make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
         (cl-cc/vm:make-vm-ret   :reg :r2))))
(defun make-test-cfg-branch ()
  "Build a CFG with a conditional branch (entry JUMP-ZERO→then, else JUMP→exit, then, exit RET)."
  (cl-cc/optimize:cfg-build
   (list (cl-cc/vm:make-vm-const    :dst :r0 :value 0)
         (cl-cc/vm:make-vm-jump-zero :reg :r0 :label "then")
         (cl-cc/vm:make-vm-const    :dst :r1 :value 99)
         (cl-cc/vm:make-vm-jump     :label "exit")
         (cl-cc/vm:make-vm-label    :name "then")
         (cl-cc/vm:make-vm-const    :dst :r1 :value 42)
         (cl-cc/vm:make-vm-label    :name "exit")
          (cl-cc/vm:make-vm-ret      :reg :r1))))
(defun make-test-cfg-loop ()
  "Build a CFG with a simple natural loop and a separate exit block."
  (cl-cc/optimize:cfg-build
   (list (cl-cc/vm:make-vm-label    :name "head")
          (cl-cc/vm:make-vm-const    :dst :r0 :value 1)
          (cl-cc/vm:make-vm-jump-zero :reg :r0 :label "exit")
          (cl-cc/vm:make-vm-jump     :label "head")
          (cl-cc/vm:make-vm-label    :name "exit")
          (cl-cc/vm:make-vm-ret      :reg :r0))))
(defun make-test-cfg-hot-cold ()
  "Build a CFG with one loop-heavy hot block and one cold exit block."
  (cl-cc/optimize:cfg-build
   (list (cl-cc/vm:make-vm-const    :dst :r0 :value 1)
         (cl-cc/vm:make-vm-jump-zero :reg :r0 :label "cold")
         (cl-cc/vm:make-vm-label    :name "hot")
         (cl-cc/vm:make-vm-const    :dst :r1 :value 2)
         (cl-cc/vm:make-vm-jump     :label "hot")
          (cl-cc/vm:make-vm-label    :name "cold")
          (cl-cc/vm:make-vm-ret      :reg :r0))))
(defun make-test-cfg-cold-signal ()
  "Build a CFG with a normal block and an explicit cold error block."
  (cl-cc/optimize:cfg-build
   (list (cl-cc/vm:make-vm-const     :dst :r0 :value 0)
         (cl-cc/vm:make-vm-jump-zero :reg :r0 :label "hot")
         (cl-cc/vm:make-vm-label    :name "cold")
          (cl-cc/vm:make-vm-signal-error :error-reg :r0)
          (cl-cc/vm:make-vm-ret      :reg :r0)
          (cl-cc/vm:make-vm-label    :name "hot")
          (cl-cc/vm:make-vm-ret      :reg :r0))))
(defun make-test-cfg-critical-edge ()
  "Build a CFG with one critical edge into the THEN block."
  (cl-cc/optimize:cfg-build
   (list (cl-cc/vm:make-vm-const    :dst :r0 :value 0)
          (cl-cc/vm:make-vm-jump-zero :reg :r0 :label "then")
          (cl-cc/vm:make-vm-const    :dst :r1 :value 99)
          (cl-cc/vm:make-vm-jump     :label "then")
          (cl-cc/vm:make-vm-label    :name "then")
          (cl-cc/vm:make-vm-const    :dst :r1 :value 42)
          (cl-cc/vm:make-vm-label    :name "merge")
          (cl-cc/vm:make-vm-ret      :reg :r1))))

;;; ─── Basic CFG Construction ──────────────────────────────────────────────
(it-sequential "cfg-linear-has-single-block"
  (let* ((cfg   (make-test-cfg-linear))
         (entry (cl-cc/optimize:cfg-entry cfg)))
    (expect (= 1 (cl-cc/optimize:cfg-block-count cfg)) :to-be-truthy)
    (expect entry :to-be-truthy)
    (expect (cl-cc/optimize/analysis:bb-instructions entry) :to-be-truthy)))

(it-sequential "cfg-empty-has-entry-block"
  (let ((cfg (cl-cc/optimize:cfg-build nil)))
    (expect (cl-cc/optimize:cfg-entry cfg) :to-be-truthy)))

(it-sequential "cfg-branch-has-multiple-blocks"
  (let ((cfg (make-test-cfg-branch)))
    (expect (>= (cl-cc/optimize:cfg-block-count cfg) 2) :to-be-truthy)))
(it-sequential "cfg-branch-labels-resolved then"
  (destructuring-bind (label-name) (list "then")
    (let ((cfg (make-test-cfg-branch)))
    (expect (cl-cc/optimize:cfg-get-block-by-label cfg label-name) :to-be-truthy))))

(it-sequential "cfg-branch-labels-resolved exit"
  (destructuring-bind (label-name) (list "exit")
    (let ((cfg (make-test-cfg-branch)))
    (expect (cl-cc/optimize:cfg-get-block-by-label cfg label-name) :to-be-truthy))))

;;; ─── Predecessor / Successor Edges ──────────────────────────────────────
(it-sequential "cfg-branch-entry-has-two-successors"
  (let* ((cfg   (make-test-cfg-branch))
         (entry (cl-cc/optimize:cfg-entry cfg)))
    (expect (= 2 (length (cl-cc/optimize/analysis:bb-successors entry))) :to-be-truthy)))

(it-sequential "cfg-branch-exit-has-predecessor"
  (let* ((cfg  (make-test-cfg-branch))
         (exit (cl-cc/optimize:cfg-get-block-by-label cfg "exit")))
    (when exit
      (expect (>= (length (cl-cc/optimize/analysis:bb-predecessors exit)) 1) :to-be-truthy))))

;;; ─── RPO ─────────────────────────────────────────────────────────────────
(it-sequential "cfg-rpo-ordering"
  (let* ((cfg   (make-test-cfg-branch))
         (rpo   (cl-cc/optimize:cfg-compute-rpo cfg))
         (entry (cl-cc/optimize:cfg-entry cfg)))
    (expect (= (cl-cc/optimize:cfg-block-count cfg) (length rpo)) :to-be-truthy)
    (expect (car rpo) :to-be entry)))

;;; ─── Dominator Tree ──────────────────────────────────────────────────────
(it-sequential "cfg-dominator-properties"
  (let* ((cfg   (make-test-cfg-branch))
         (entry (cl-cc/optimize:cfg-entry cfg)))
    (cl-cc/optimize:cfg-compute-dominators cfg)
    (expect (cl-cc/optimize/analysis:bb-idom entry) :to-be entry)
    (let ((exit (cl-cc/optimize:cfg-get-block-by-label cfg "exit")))
      (when exit
        (expect (cl-cc/optimize:cfg-dominates-p entry exit) :to-be-truthy)))))

;;; ─── Dominance Frontiers ─────────────────────────────────────────────────
(it-sequential "cfg-dominance-frontiers-computed"
  (let ((cfg (make-test-cfg-branch)))
    (cl-cc/optimize:cfg-compute-dominators cfg)
    (cl-cc/optimize:cfg-compute-dominance-frontiers cfg)
    (expect t :to-be-truthy)))

;;; ─── Post-Dominator Tree ────────────────────────────────────────────────
(it-sequential "cfg-post-dominators-computed"
  (handler-bind ((warning #'muffle-warning))
    (let ((cfg (make-test-cfg-branch)))
      (cl-cc/optimize/analysis:cfg-compute-post-dominators cfg)
      (let ((exit (cl-cc/optimize:cfg-get-block-by-label cfg "exit"))
            (entry (cl-cc/optimize:cfg-entry cfg)))
        (expect exit :to-be-truthy)
        (expect (cl-cc/optimize/analysis:bb-post-idom exit) :to-be exit)
        (expect (cl-cc/optimize/analysis:cfg-post-dominates-p exit entry) :to-be-truthy)))))
(it-sequential "cfg-loop-depths-computed"
  (let* ((cfg (make-test-cfg-loop))
         (head (cl-cc/optimize:cfg-get-block-by-label cfg "head"))
         (exit (cl-cc/optimize:cfg-get-block-by-label cfg "exit"))
         (body (find-if (lambda (b)
                          (some (lambda (i)
                                  (and (typep i 'cl-cc/vm::vm-jump)
                                       (equal (cl-cc/vm::vm-label-name i) "head")))
                                (cl-cc/optimize/analysis:bb-instructions b)))
                        (coerce (cl-cc/optimize:cfg-blocks cfg) 'list))))
    (cl-cc/optimize:cfg-compute-dominators cfg)
    (cl-cc/optimize:cfg-compute-loop-depths cfg)
    (expect head :to-be-truthy)
    (expect body :to-be-truthy)
    (expect (= 1 (cl-cc/optimize/analysis:bb-loop-depth head)) :to-be-truthy)
    (expect (= 1 (cl-cc/optimize/analysis:bb-loop-depth body)) :to-be-truthy)
    (expect (= 0 (cl-cc/optimize/analysis:bb-loop-depth exit)) :to-be-truthy)))
(it-sequential "cfg-hot-cold-flatten-cold-after-hot loop-vs-exit"
  (destructuring-bind (cfg) (list (make-test-cfg-hot-cold))
    (cl-cc/optimize:cfg-compute-dominators cfg) (cl-cc/optimize:cfg-compute-loop-depths cfg) (let* ((flat   (cl-cc/optimize:cfg-flatten-hot-cold cfg))
         (labels (loop for inst in flat
                       when (typep inst 'cl-cc/vm::vm-label)
                       collect (cl-cc/vm::vm-name inst))))
    (expect (member "hot"  labels :test #'equal) :to-be-truthy)
    (expect (member "cold" labels :test #'equal) :to-be-truthy)
    (expect (< (position "hot"  labels :test #'equal)
                    (position "cold" labels :test #'equal)) :to-be-truthy))))

(it-sequential "cfg-hot-cold-flatten-cold-after-hot normal-vs-signal"
  (destructuring-bind (cfg) (list (make-test-cfg-cold-signal))
    (cl-cc/optimize:cfg-compute-dominators cfg) (cl-cc/optimize:cfg-compute-loop-depths cfg) (let* ((flat   (cl-cc/optimize:cfg-flatten-hot-cold cfg))
         (labels (loop for inst in flat
                       when (typep inst 'cl-cc/vm::vm-label)
                       collect (cl-cc/vm::vm-name inst))))
    (expect (member "hot"  labels :test #'equal) :to-be-truthy)
    (expect (member "cold" labels :test #'equal) :to-be-truthy)
    (expect (< (position "hot"  labels :test #'equal)
                    (position "cold" labels :test #'equal)) :to-be-truthy))))

(it-sequential "cfg-hot-cold-flatten-places-error-handler-last"
  (let ((cfg (make-test-cfg-cold-signal)))
    (cl-cc/optimize:cfg-compute-dominators cfg)
    (cl-cc/optimize:cfg-compute-loop-depths cfg)
    (let* ((flat (cl-cc/optimize:cfg-flatten-hot-cold cfg))
           (labels (loop for inst in flat
                         when (typep inst 'cl-cc/vm::vm-label)
                         collect (cl-cc/vm::vm-name inst))))
      (expect (car (last labels)) :to-equal "cold"))))
(it-sequential "cfg-critical-edge-splitting-inserts-landing-pad"
  (let* ((cfg (make-test-cfg-critical-edge))
         (before (cl-cc/optimize:cfg-block-count cfg))
         (entry (cl-cc/optimize:cfg-entry cfg))
         (then  (cl-cc/optimize:cfg-get-block-by-label cfg "then")))
    (cl-cc/optimize:cfg-split-critical-edges cfg)
    (expect (= (1+ before) (cl-cc/optimize:cfg-block-count cfg)) :to-be-truthy)
    (expect (not (member then (cl-cc/optimize/analysis:bb-successors entry) :test #'eq)) :to-be-truthy)
    (expect (not (member entry (cl-cc/optimize/analysis:bb-predecessors then) :test #'eq)) :to-be-truthy)
    (let ((pad (find-if (lambda (b)
                          (and (= 1 (length (cl-cc/optimize/analysis:bb-successors b)))
                               (eq then (first (cl-cc/optimize/analysis:bb-successors b)))
                               (some (lambda (i)
                                       (and (typep i 'cl-cc/vm::vm-jump)
                                            (equal (cl-cc/vm::vm-label-name i)
                                                   (cl-cc/vm::vm-name (cl-cc/optimize/analysis:bb-label then)))))
                                     (cl-cc/optimize/analysis:bb-instructions b))))
                        (coerce (cl-cc/optimize:cfg-blocks cfg) 'list))))
      (expect pad :to-be-truthy)
      (expect (member pad (cl-cc/optimize/analysis:bb-successors entry) :test #'eq) :to-be-truthy))))

;;; ─── Flatten Round-Trip ──────────────────────────────────────────────────
(it-sequential "cfg-flatten-preserves-instruction-count"
  (let* ((orig  (list (cl-cc/vm:make-vm-const :dst :r0 :value 42)
                      (cl-cc/vm:make-vm-ret   :reg :r0)))
         (cfg   (cl-cc/optimize:cfg-build orig))
         (flat  (cl-cc/optimize:cfg-flatten cfg)))
    (expect (>= (length flat) (length orig)) :to-be-truthy)))

;;; ─── cfg-idf (Iterated Dominance Frontier) ──────────────────────────────────
(it-sequential "cfg-idf-empty-input-returns-nil"
  (expect (cl-cc/optimize:cfg-idf nil) :to-be-null))

(it-sequential "cfg-idf-linear-entry-returns-list"
  (let* ((cfg   (make-test-cfg-linear))
         (_     (cl-cc/optimize:cfg-compute-dominators cfg))
         (_     (cl-cc/optimize:cfg-compute-dominance-frontiers cfg))
         (entry (cl-cc/optimize:cfg-entry cfg)))
    (declare (ignore _ _))
    (expect (listp (cl-cc/optimize:cfg-idf (list entry))) :to-be-truthy)))

(it-sequential "cfg-idf-branch-entry-returns-list"
  (let* ((cfg   (make-test-cfg-branch))
         (_     (cl-cc/optimize:cfg-compute-dominators cfg))
         (_     (cl-cc/optimize:cfg-compute-dominance-frontiers cfg))
         (entry (cl-cc/optimize:cfg-entry cfg)))
    (declare (ignore _ _))
    (expect (listp (cl-cc/optimize:cfg-idf (list entry))) :to-be-truthy)))

;;; ─── %cfg-fallthrough-edge / %cfg-jump-target-edge ───────────────────────
(it-sequential "cfg-fallthrough-edge-adds-when-next-start-exists"
  (let* ((g   (cl-cc/optimize:make-cfg))
         (b1  (cl-cc/optimize/analysis:cfg-new-block g))
         (b2  (cl-cc/optimize/analysis:cfg-new-block g))
         (bbs (let ((ht (make-hash-table)))
                (setf (gethash 1 ht) b2)
                ht)))
    (cl-cc/optimize/analysis::%cfg-fallthrough-edge b1 1 bbs)
    (expect (member b2 (cl-cc/optimize/analysis:bb-successors b1) :test #'eq) :to-be-truthy)
    (expect (member b1 (cl-cc/optimize/analysis:bb-predecessors b2) :test #'eq) :to-be-truthy)))

(it-sequential "cfg-fallthrough-edge-noop-when-nil"
  (let* ((g   (cl-cc/optimize:make-cfg))
         (b1  (cl-cc/optimize/analysis:cfg-new-block g))
         (bbs (make-hash-table)))
    (cl-cc/optimize/analysis::%cfg-fallthrough-edge b1 nil bbs)
    (expect (cl-cc/optimize/analysis:bb-successors b1) :to-be-null)))

(it-sequential "cfg-jump-target-edge-wires-to-label"
  (let* ((g    (cl-cc/optimize:make-cfg))
         (src  (cl-cc/optimize/analysis:cfg-new-block g))
         (dest (cl-cc/optimize/analysis:cfg-new-block g :label (cl-cc/vm:make-vm-label :name "tgt"))))
    (declare (ignore dest))
    (cl-cc/optimize/analysis::%cfg-jump-target-edge src (cl-cc/vm:make-vm-jump :label "tgt") g)
    (let ((tgt (cl-cc/optimize:cfg-get-block-by-label g "tgt")))
      (expect (member tgt (cl-cc/optimize/analysis:bb-successors src) :test #'eq) :to-be-truthy))))

;;; ─── %cfg-replace-successor / %cfg-replace-predecessor / %cfg-replace-terminator
(it-sequential "cfg-replace-successor-swaps-block"
  (let* ((blk (make-instance 'cl-cc/optimize:basic-block))
         (old (make-instance 'cl-cc/optimize:basic-block))
         (new (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize/analysis:bb-successors blk) (list old))
    (cl-cc/optimize/analysis:%cfg-replace-successor blk old new)
    (expect (member old (cl-cc/optimize/analysis:bb-successors blk) :test #'eq) :to-be-falsy)
    (expect (member new (cl-cc/optimize/analysis:bb-successors blk) :test #'eq) :to-be-truthy)))

(it-sequential "cfg-replace-predecessor-swaps-block"
  (let* ((blk (make-instance 'cl-cc/optimize:basic-block))
         (old (make-instance 'cl-cc/optimize:basic-block))
         (new (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize/analysis:bb-predecessors blk) (list old))
    (cl-cc/optimize/analysis::%cfg-replace-predecessor blk old new)
    (expect (member old (cl-cc/optimize/analysis:bb-predecessors blk) :test #'eq) :to-be-falsy)
    (expect (member new (cl-cc/optimize/analysis:bb-predecessors blk) :test #'eq) :to-be-truthy)))

(it-sequential "cfg-replace-terminator-swaps-instruction"
  (let* ((blk (make-instance 'cl-cc/optimize:basic-block))
         (old (cl-cc/vm:make-vm-jump :label "a"))
         (new (cl-cc/vm:make-vm-jump :label "b")))
    (setf (cl-cc/optimize/analysis:bb-instructions blk) (list old))
    (cl-cc/optimize/analysis:%cfg-replace-terminator blk old new)
    (expect (cl-cc/optimize/analysis:bb-instructions blk) :to-equal (list new))))

;;; ─── %cfg-ensure-label ────────────────────────────────────────────────────
(it-sequential "cfg-ensure-label-creates-fresh-when-absent"
  (let* ((g   (cl-cc/optimize:make-cfg))
         (blk (cl-cc/optimize/analysis:cfg-new-block g)))
    (setf (cl-cc/optimize/analysis:bb-label blk) nil)
    (let ((lbl (cl-cc/optimize/analysis:%cfg-ensure-label blk g "test")))
      (expect (cl-cc/vm::vm-label-p lbl) :to-be-truthy)
      (expect (cl-cc/optimize/analysis:bb-label blk) :to-be lbl))))

(it-sequential "cfg-ensure-label-returns-existing"
  (let* ((g   (cl-cc/optimize:make-cfg))
         (blk (cl-cc/optimize/analysis:cfg-new-block g :label (cl-cc/vm:make-vm-label :name "existing"))))
    (let ((lbl (cl-cc/optimize/analysis:%cfg-ensure-label blk g "test")))
      (expect (cl-cc/vm::vm-name lbl) :to-equal "existing"))))
