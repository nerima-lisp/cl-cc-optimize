(in-package :cl-cc/optimize/speculation/test)

(it-sequential "optimize-roadmap-ic-transitions-track-misses-and-fallbacks"
  (let ((site (cl-cc/optimize/speculation:make-opt-ic-site :max-polymorphic-entries 2)))
    (expect (cl-cc/optimize/speculation:opt-ic-site-state site) :to-be :uninitialized)
    (cl-cc/optimize/speculation:opt-ic-transition site :shape-a :target-a)
    (expect (cl-cc/optimize/speculation:opt-ic-site-state site) :to-be :monomorphic)
    (cl-cc/optimize/speculation:opt-ic-transition site :shape-b :target-b)
    (expect (cl-cc/optimize/speculation:opt-ic-site-state site) :to-be :polymorphic)
    (cl-cc/optimize/speculation:opt-ic-transition site :shape-c :target-c)
    (expect (cl-cc/optimize/speculation:opt-ic-site-state site) :to-be :megamorphic)
    (expect (= 3 (cl-cc/optimize/speculation:opt-ic-site-misses site)) :to-be-truthy)
    (expect (= 3 (length (cl-cc/optimize/speculation:opt-ic-site-megamorphic-fallback site))) :to-be-truthy)))

(it-sequential "optimize-roadmap-speculation-failure-threshold"
  (let ((spec-log (cl-cc/optimize/speculation:make-opt-speculation-log :threshold 2)))
    (expect (cl-cc/optimize/speculation:opt-speculation-failed-p spec-log :site :guard) :to-be-falsy)
    (expect (= 1 (cl-cc/optimize/speculation:opt-record-speculation-failure spec-log :site :guard)) :to-be-truthy)
    (expect (cl-cc/optimize/speculation:opt-speculation-failed-p spec-log :site :guard) :to-be-falsy)
    (expect (= 2 (cl-cc/optimize/speculation:opt-record-speculation-failure spec-log :site :guard)) :to-be-truthy)
    (expect (cl-cc/optimize/speculation:opt-speculation-failed-p spec-log :site :guard) :to-be-truthy)))

(it-sequential "optimize-roadmap-profile-recorders-track-runtime-data"
  (let ((profile (cl-cc/optimize/speculation:make-opt-profile-data)))
    (expect (= 3 (cl-cc/optimize/speculation:opt-profile-record-edge profile :entry :exit 3)) :to-be-truthy)
    (expect (= 2 (cl-cc/optimize/speculation:opt-profile-record-value profile :ic-site 42 2)) :to-be-truthy)
    (expect (= 4 (cl-cc/optimize/speculation:opt-profile-record-call-chain profile '(:main :callee) 4)) :to-be-truthy)
    (expect (cl-cc/optimize/speculation:opt-profile-record-allocation profile :alloc-site 64 2)
            :to-equal (cons 2 64))))

(it-sequential "optimize-roadmap-summary-safe-to-inline"
  (let ((pure-summary (cl-cc/optimize/speculation:make-opt-function-summary
                       :name 'pure-helper :pure-p t :effects nil))
        (effectful-summary (cl-cc/optimize/speculation:make-opt-function-summary
                            :name 'effectful-helper :pure-p t :effects '(:heap-write))))
    (expect (cl-cc/optimize/speculation:opt-function-summary-safe-to-inline-p pure-summary) :to-be-truthy)
    (expect (cl-cc/optimize/speculation:opt-function-summary-safe-to-inline-p effectful-summary) :to-be-falsy)))

(it-sequential "optimize-roadmap-bump-region-alignment-overflow-and-reset"
  (let ((region (cl-cc/optimize/speculation:make-opt-bump-region :limit 8)))
    (cl-cc/optimize/speculation:opt-bump-allocate region 3)
    (cl-cc/optimize/speculation:opt-bump-mark region)
    (expect (= 4 (cl-cc/optimize/speculation:opt-bump-allocate region 2 :alignment 4)) :to-be-truthy)
    (expect (cl-cc/optimize/speculation:opt-bump-allocate region 4) :to-be-null)
    (cl-cc/optimize/speculation:opt-bump-reset region)
    (expect (= 3 (cl-cc/optimize/speculation:opt-bump-region-cursor region)) :to-be-truthy)))

(it-sequential "optimize-roadmap-stack-map-live-root-membership"
  (let ((stack-map (cl-cc/optimize/speculation:make-opt-stack-map :pc 42 :roots '(:r0 :r2))))
    (expect (cl-cc/optimize/speculation:opt-stack-map-live-root-p stack-map :r0) :to-be-truthy)
    (expect (cl-cc/optimize/speculation:opt-stack-map-live-root-p stack-map :r1) :to-be-falsy)))

(it-sequential "optimize-roadmap-guard-recording-strength"
  (let ((guard (cl-cc/optimize/speculation:make-opt-guard-state :executions 9)))
    (expect (cl-cc/optimize/speculation:opt-guard-record guard t) :to-be :tag-bit-test)
    (expect (cl-cc/optimize/speculation:opt-guard-record guard nil) :to-be :full-type-check)))

(it-sequential "optimize-roadmap-jit-cache-selects-cold-entry-for-eviction"
  (let* ((cold (cl-cc/optimize/speculation:make-opt-jit-cache-entry :id :cold :size 8 :warmth 1))
         (warm (cl-cc/optimize/speculation:make-opt-jit-cache-entry :id :warm :size 8 :warmth 9))
         (evicted (cl-cc/optimize/speculation:opt-jit-cache-select-eviction
                   (list warm cold)
                   :current-size 90
                   :max-size 100)))
    (expect evicted :to-be cold)))

(it-sequential "optimize-roadmap-sea-node-schedulability"
  (expect (cl-cc/optimize/speculation:opt-sea-node-schedulable-p
           (cl-cc/optimize/speculation:make-opt-sea-node :id :n1 :op :add :controls '(:entry)))
          :to-be-truthy)
  (expect (cl-cc/optimize/speculation:opt-sea-node-schedulable-p
           (cl-cc/optimize/speculation:make-opt-sea-node :id :n2 :controls '(:entry)))
          :to-be-falsy))

(it-sequential "optimize-roadmap-shape-missing-slot-has-no-offset"
  (let ((shape (cl-cc/optimize/speculation:make-opt-shape-descriptor-for-slots 9 '(:class :slots))))
    (expect (cl-cc/optimize/speculation:opt-shape-slot-offset shape :missing) :to-be-null)))

(it-sequential "optimizer-roadmap-value-profiling-top-k-and-range-behavior"
  (let ((profile (cl-cc/optimize/speculation:make-opt-profile-data :value-limit 2)))
    (expect (= 2 (cl-cc/optimize/speculation:opt-profile-record-value profile :site 10 2)) :to-be-truthy)
    (expect (= 3 (cl-cc/optimize/speculation:opt-profile-record-value profile :site 20 3)) :to-be-truthy)
    (expect (= 1 (cl-cc/optimize/speculation:opt-profile-record-value profile :site 30 1)) :to-be-truthy)
    (expect (cl-cc/optimize/speculation:opt-profile-top-values profile :site) :to-equal '((20 . 3) (10 . 2)))
    (expect (cl-cc/optimize/speculation:opt-profile-top-values profile :site 1) :to-equal '((20 . 3)))
    (expect (cl-cc/optimize/speculation:opt-profile-value-range profile :site) :to-equal '(10 . 30))))

(it-sequential "optimizer-roadmap-speculation-log-gating-and-persistence-behavior"
  (let* ((cl-cc/optimize/speculation:*opt-speculation-log*
           (cl-cc/optimize/speculation:make-opt-speculation-log :threshold 2))
         (temp-path (merge-pathnames
                     (make-pathname :name (format nil "opt-spec-log-~D-~D"
                                                  (get-universal-time)
                                                  (random 1000000))
                                    :type "prof")
                     (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (expect (cl-cc/optimize/speculation:opt-speculation-allowed-p :site :guard) :to-be-truthy)
           (expect (= 1 (cl-cc/optimize/speculation:opt-record-speculation-failure
                         cl-cc/optimize/speculation:*opt-speculation-log*
                         :site :guard))
                   :to-be-truthy)
           (expect (cl-cc/optimize/speculation:opt-speculation-allowed-p :site :guard) :to-be-truthy)
           (expect (= 2 (cl-cc/optimize/speculation:opt-record-speculation-failure
                         cl-cc/optimize/speculation:*opt-speculation-log*
                         :site :guard))
                   :to-be-truthy)
           (expect (cl-cc/optimize/speculation:opt-speculation-allowed-p :site :guard) :to-be-falsy)
           (cl-cc/optimize/speculation:opt-save-speculation-log temp-path)
           (expect (cl-cc/optimize/speculation:opt-clear-speculation-log)
                   :to-be cl-cc/optimize/speculation:*opt-speculation-log*)
           (expect (cl-cc/optimize/speculation:opt-speculation-allowed-p :site :guard) :to-be-truthy)
           (setf (cl-cc/optimize/speculation::opt-spec-log-threshold
                  cl-cc/optimize/speculation:*opt-speculation-log*) 99)
           (cl-cc/optimize/speculation:opt-load-speculation-log temp-path)
           (expect (= 2 (cl-cc/optimize/speculation::opt-spec-log-threshold
                         cl-cc/optimize/speculation:*opt-speculation-log*))
                   :to-be-truthy)
           (expect (cl-cc/optimize/speculation:opt-speculation-failed-p
                    cl-cc/optimize/speculation:*opt-speculation-log* :site :guard)
                   :to-be-truthy)
           (expect (cl-cc/optimize/speculation:opt-speculation-allowed-p :site :guard) :to-be-falsy))
      (when (probe-file temp-path)
        (delete-file temp-path)))))
