;;; -----------------------------------------------------------------------------
;;; Optimizer Pre-RA Pressure-Aware Scheduling
;;; -----------------------------------------------------------------------------
;;; FR-067: pressure-aware list scheduling before register allocation.
;;;
;;; Builds on the latency tables, dependency-graph construction, and generic
;;; toposort list scheduler in optimizer-scheduler.lisp (loads before this
;;; file), adding live-range/register-pressure tracking so the pre-RA
;;; scheduler prefers instructions that reduce live-register pressure.

(in-package :cl-cc/optimize)

(defun %opt-add-regs (regs live)
  "Return LIVE with REGS added using EQ identity."
  (let ((result live))
    (dolist (reg regs result)
      (when reg
        (pushnew reg result :test #'eq)))))

(defun %opt-remove-regs (regs live)
  "Return LIVE with REGS removed using EQ identity."
  (let ((result live))
    (dolist (reg regs result)
      (setf result (remove reg result :test #'eq)))))

(defun %opt-live-before-instruction (inst live-after)
  "Return registers live immediately before INST given LIVE-AFTER."
  (%opt-add-regs
    (opt-inst-read-regs inst)
    (%opt-remove-regs (%opt-inst-write-regs inst) live-after)))

(defun %opt-live-before-instructions (instructions live-after)
  "Return registers live before INSTRUCTIONS given LIVE-AFTER."
  (let ((live live-after))
    (dolist (inst (reverse instructions) live)
      (setf live (%opt-live-before-instruction inst live)))))

(defun %opt-increment-reg-counts (regs counts)
  "Increment COUNTS for each REG in REGS."
  (dolist (reg regs counts)
    (when reg
      (incf (gethash reg counts 0)))))

(defun %opt-decrement-reg-counts (regs counts)
  "Decrement COUNTS for each REG in REGS, removing zero entries."
  (dolist (reg regs counts)
    (when reg
      (let ((next (1- (gethash reg counts 0))))
        (if (plusp next) (setf (gethash reg counts) next)
          (remhash reg counts))))))

(defun %opt-build-read-counts (insts live-out)
  "Return a read-count table for unscheduled INSTS plus LIVE-OUT pseudo-uses."
  (let ((counts (make-hash-table :test #'eq)))
    (%opt-increment-reg-counts live-out counts)
    (dolist (inst insts counts)
      (%opt-increment-reg-counts (opt-inst-read-regs inst) counts))))

(defun %opt-pre-ra-pressure-delta (inst counts)
  "Estimate live-register pressure delta if INST is scheduled next.

Negative values reduce pressure: operands whose last unscheduled use is this
instruction die.  Positive values increase pressure: destinations that are still
needed by unscheduled instructions or live-out become live."
  (let ((reads (opt-inst-read-regs inst))
        (writes (%opt-inst-write-regs inst))
        (delta 0))
    (dolist (reg reads)
      (when (= (gethash reg counts 0) 1)
        (decf delta)))
    (dolist (reg writes)
      (when (plusp (gethash reg counts 0))
        (incf delta)))
    delta))

(defun %opt-best-pre-ra-ready-node (ready insts priorities counts)
  "Select pressure-aware best node from READY.

Prefer instructions that reduce register pressure, then longer critical paths,
then original order for deterministic output."
  (reduce
    (lambda (best candidate)
      (let ((best-delta (%opt-pre-ra-pressure-delta (nth best insts) counts))
            (candidate-delta (%opt-pre-ra-pressure-delta (nth candidate insts) counts))
            (best-priority (aref priorities best))
            (candidate-priority (aref priorities candidate)))
        (if (or
            (< candidate-delta best-delta)
            (and (= candidate-delta best-delta) (> candidate-priority best-priority))
            (and
              (= candidate-delta best-delta)
              (= candidate-priority best-priority)
              (< candidate best))) candidate
          best)))
    ready))

(defun %opt-schedule-pre-ra-run (insts live-out)
  "Pressure-aware list-schedule a side-effect-free pre-RA instruction run."
  (%opt-run-scheduler-toposort
    insts
    :node-selector
    #'%opt-best-pre-ra-ready-node
    :counts-or-nil
    (%opt-build-read-counts insts live-out)))

(defun %opt-schedule-pre-ra-basic-block (instructions)
  "Pressure-aware scheduling for one basic block, preserving all barriers."
  (let ((live-after nil)
        (scheduled-segments nil))
    (dolist (segment (reverse (%opt-scheduler-segments instructions)))
      (ecase (first segment)
        (:barrier
          (let ((inst (second segment)))
            (push (list inst) scheduled-segments)
            (setf live-after (%opt-live-before-instruction inst live-after))))
        (:run
          (let* ((run (second segment))
                 (scheduled (%opt-schedule-pre-ra-run run live-after)))
            (push scheduled scheduled-segments)
            (setf live-after (%opt-live-before-instructions scheduled live-after))))))
    (apply #'append scheduled-segments)))

(defun schedule-pre-ra (instructions)
  "FR-067: pressure-aware list scheduling before register allocation.

Builds a dependency DAG for each basic block, computes critical-path priorities,
and schedules ready instructions with a register-pressure tie-breaker.  The pass
never moves instructions across labels, control-flow instructions, calls, stores,
signals, or other side-effecting barriers, so basic-block boundaries and codegen
semantics are preserved.  FR-067 is complete and this function is the backend
entry point used before register allocation."
  (let ((cfg (cfg-build instructions)))
    (loop for block across (cfg-blocks cfg)
          do (setf (bb-instructions block)
                   (%opt-schedule-pre-ra-basic-block (bb-instructions block))))
    (%opt-flatten-cfg-block-order cfg)))
