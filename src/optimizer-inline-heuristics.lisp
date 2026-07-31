;;;; optimizer-inline-heuristics.lisp — inline cost and eligibility scoring
;;;;
;;;; Estimates a callee body's inlining cost (instruction weights, adjusted
;;;; by profile-guided size/hotness and an optional learned ML score) and
;;;; decides eligibility against the thresholds in
;;;; optimizer-inline-cost-data.lisp. optimizer-inline-expand.lisp consumes
;;;; this to pick which candidates to actually inline.

(in-package :cl-cc/optimize)

(defun %opt-inline-call-heavy-p (body)
  "Return T when BODY contains call-like instructions that make inlining risky."
  (some (lambda (inst)
          (typep inst '(or vm-call vm-generic-call vm-tail-call vm-apply)))
        body))

(defun %opt-inline-profile-value (profile-data label key default)
  "Read KEY from PROFILE-DATA for LABEL, accepting either a plist or hash table."
  (cond
    ((hash-table-p profile-data) (getf (gethash label profile-data) key default))
    ((listp profile-data) (getf profile-data key default))
    (t default)))

(defun %opt-inline-def-label (def)
  "Return DEF's label when it has a closure entry."
  (let ((closure (getf def :closure)))
    (and (typep closure '(or vm-closure vm-func-ref))
         (vm-label-name closure))))

(defun %opt-inline-captured-vars (inst)
  "Return captured vars for callable reference INST. vm-func-ref never captures."
  (if (vm-closure-p inst)
      (vm-captured-vars inst)
      nil))

(defun %opt-inline-size-adjustment (inst-count body-cost)
  "Return a conservative inline threshold adjustment for function size."
  (cond
    ((<= inst-count 6) 4)
    ((or (> inst-count 60) (> body-cost 120)) -10)
    ((or (> inst-count 30) (> body-cost 70)) -5)
    (t 0)))

(defun %opt-inline-hotness-adjustment (call-count loop-depth)
  "Return inline threshold bonus from profile call count and loop depth hints."
  (+ (cond
       ((>= call-count 100) 10)
       ((>= call-count 20) 5)
       ((>= call-count 5) 2)
       (t 0))
     (min 12 (* 4 loop-depth))))

(defun %opt-inline-loop-depths (instructions)
  "Return index -> loop-depth table derived from backward branch ranges."
  (let ((label-pos (make-hash-table :test #'equal))
        (depths (make-hash-table :test #'eql)))
    (loop for inst in instructions
          for i from 0
          when (typep inst 'vm-label)
            do (setf (gethash (vm-name inst) label-pos) i))
    (loop for inst in instructions
          for i from 0
          when (typep inst '(or vm-jump vm-jump-zero))
            do (let ((target (gethash (vm-label-name inst) label-pos)))
                 (when (and target (< target i))
                   (loop for j from target to i do (incf (gethash j depths 0))))))
    depths))

(defun opt-inline-profile-data (instructions)
  "Return label -> plist profile hints for adaptive inlining."
  (let ((name-to-label (opt-build-function-name-map instructions))
        (reg-track (make-hash-table :test #'eq))
        (depths (%opt-inline-loop-depths instructions))
        (profile (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for i from 0
          do (typecase inst
               ((or vm-call vm-tail-call vm-apply)
                (let ((label (gethash (vm-func-reg inst) reg-track)))
                  (when label
                    (let ((entry (or (gethash label profile)
                                     (setf (gethash label profile)
                                           (list :call-count 0 :loop-depth 0)))))
                      (incf (getf entry :call-count))
                      (setf (getf entry :loop-depth)
                            (max (getf entry :loop-depth 0) (gethash i depths 0))))))
                (let ((dst (opt-inst-dst inst)))
                  (when dst (remhash dst reg-track))))
               (t (%opt-track-known-callee-label inst name-to-label reg-track))))
    profile))

(defun %opt-inline-score-features (def)
  "Build coarse feature tags for ML-guided inline score estimation."
  (let* ((body (butlast (getf def :body)))
         (inst-count (length body))
          (call-heavy-p (%opt-inline-call-heavy-p body))
         (cheap-count (count-if (lambda (inst)
                                  (<= (egraph-default-cost (vm-inst-to-enode-op inst) nil) 1))
                                body))
         (cheap-ratio (if (zerop inst-count) 1.0 (/ cheap-count inst-count))))
    (remove nil
            (list (if (<= inst-count 6) :small-body :large-body)
                  (when (>= cheap-ratio 0.75) :cheap-body)
                  (when call-heavy-p :call-heavy)))))

(defun %opt-ml-inline-threshold-bonus (def)
  "Translate ML inline score into a small threshold bonus."
  (if (and *opt-enable-ml-inline-score*
           (fboundp 'opt-ml-inline-score-plan))
      (let ((features (%opt-inline-score-features def)))
        ;; OPT-ML-INLINE-SCORE-PLAN scores by feature *count*, without regard to
        ;; what each feature means. A body tagged :CALL-HEAVY therefore outscored
        ;; a small cheap one and earned a threshold bonus for being a poor inline
        ;; candidate — enough to push a call-heavy body back over the base
        ;; threshold that %OPT-INLINE-SIZE-ADJUSTMENT had just pulled it under.
        ;; Only the favourable tags may raise the threshold.
        (if (intersection '(:call-heavy :large-body) features)
            0
            (let* ((plan (opt-ml-inline-score-plan
                          :features features
                          :model-version *opt-inline-ml-model-version*))
                   (score (or (getf plan :score) 0)))
              (cond ((>= score 16) 6)
                    ((>= score 12) 3)
                    (t 0)))))
      0))

(defun opt-inline-inst-cost (inst)
  "Return the inline cost of INST using the shared e-graph opcode table.
This keeps inlining policy aligned with the optimizer's existing cost model
instead of relying on raw instruction count."
  (let* ((base (egraph-default-cost (vm-inst-to-enode-op inst) nil))
         (learned (and (fboundp 'opt-learned-codegen-cost-plan)
                       (opt-learned-codegen-cost-plan
                        :opcode-features (list (vm-inst-to-enode-op inst))
                        :target *opt-learned-cost-target*)))
         (predicted (and learned (getf learned :predicted-cost))))
    ;; The learned adjustment applies only where the shared model charges
    ;; something. OPT-LEARNED-CODEGEN-COST-PLAN is opcode-blind — it answers
    ;; (+ arch-base feature-count) — so adding it unconditionally put a floor of
    ;; (max 1 (ceiling 11 4)) = 3 under every instruction. A VM-CONST the e-graph
    ;; table prices at 0 then cost as much as three adds, flattening the very
    ;; model this function exists to reuse: a body of 17 constants scored 51 and
    ;; was refused at a threshold of 15. Free instructions stay free; the
    ;; target-specific difference stays visible on the rest.
    (if (and predicted (plusp base))
        (+ base (max 1 (ceiling predicted 4)))
        base)))

(defun opt-inline-body-cost (body)
  "Return the total inline cost of BODY, excluding the final vm-ret."
  (reduce #'+ (mapcar #'opt-inline-inst-cost (butlast body)) :initial-value 0))

(defun opt-adaptive-inline-threshold (def &key (base-threshold 15) (max-threshold 50)
                                            profile-data call-count loop-depth function-size)
  "Compute a conservative adaptive inline threshold for DEF.
Cheap bodies dominated by low-cost instructions get a larger threshold, while
call-heavy bodies are kept near the base threshold. Optional profile hints make
hot loop-local callees more aggressive without weakening structural checks."
  (let* ((body (butlast (getf def :body)))
          (inst-count (length body))
          (body-cost (opt-inline-body-cost (getf def :body)))
          (label (%opt-inline-def-label def))
          (effective-call-count (or call-count
                                    (%opt-inline-profile-value profile-data label :call-count 0)))
          (effective-loop-depth (or loop-depth
                                    (%opt-inline-profile-value profile-data label :loop-depth 0)))
          (effective-size (or function-size inst-count))
          (cheap-count (count-if (lambda (inst)
                                   (<= (egraph-default-cost (vm-inst-to-enode-op inst) nil) 1))
                                 body))
          (call-heavy-p (%opt-inline-call-heavy-p body))
          (cheap-ratio (if (zerop inst-count) 1.0 (/ cheap-count inst-count))))
    (let* ((raw (max 8
                      (+ base-threshold
                         (if call-heavy-p -5 0)
                         (%opt-inline-hotness-adjustment effective-call-count effective-loop-depth)
                         (%opt-inline-size-adjustment effective-size body-cost)
                         (cond
                          ((>= cheap-ratio 0.90) 35)
                          ((>= cheap-ratio 0.75) 20)
                          ((>= cheap-ratio 0.50) 8)
                          (t 0)))))
            (scaled (if (and (integerp *opt-inline-threshold-scale*)
                             (> *opt-inline-threshold-scale* 1))
                        (* raw *opt-inline-threshold-scale*)
                        raw))
            (with-ml (+ scaled (%opt-ml-inline-threshold-bonus def))))
      (min max-threshold with-ml))))

(defun opt-inline-eligible-p (def threshold)
  "Return T if DEF satisfies all inlining preconditions:
1. Has a vm-closure with zero captured variables
2. Has only required params (no &optional/&rest/&key)
3. Body cost ≤ THRESHOLD (excluding the final vm-ret), unless forced INLINE
4. All body instructions support lossless register renaming
5. Body reads no global registers not defined by params or body.
NOTINLINE always blocks inlining; INLINE only bypasses the cost threshold and
does not weaken any structural safety checks."
  (let ((ci   (getf def :closure))
         (body (getf def :body)))
    (and (typep ci '(or vm-closure vm-func-ref))
         (<= (length (butlast body)) *max-inline-size*)
         (not (eq (vm-closure-inline-policy ci) :notinline))
          (or *block-compile*
              (null (%opt-inline-captured-vars ci)))
         (null (vm-closure-optional-params ci))
         (null (vm-closure-rest-param ci))
         (null (vm-closure-key-params ci))
         (or (eq (vm-closure-inline-policy ci) :inline)
             (<= (opt-inline-body-cost body) threshold))
          (opt-can-safely-rename-p body)
           (or *block-compile*
               (not (opt-body-has-global-refs-p body (vm-closure-params ci)))))))

(defun %opt-inline-effective-threshold (def threshold profile-data)
  "Resolve THRESHOLD for DEF, honoring the adaptive policy when requested."
  (cond
    ((eq threshold :adaptive)
     (opt-adaptive-inline-threshold def :profile-data profile-data))
    ((and (eq threshold :mlgo)
          (boundp '*mlgo-enabled*)
          (symbol-value '*mlgo-enabled*)
          (fboundp 'opt-mlgo-inline-threshold))
     (opt-mlgo-inline-threshold def :profile-data profile-data))
    ((eq threshold :mlgo)
     (opt-adaptive-inline-threshold def :profile-data profile-data))
    (t threshold)))
