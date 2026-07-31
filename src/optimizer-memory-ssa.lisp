(in-package :cl-cc/optimize)

(defun opt-compute-memory-ssa-snapshot (instructions)
  "Compute a straight-line Memory-SSA snapshot table for INSTRUCTIONS.

Returns an EQ hash-table mapping each modeled instruction to a plist:
  :kind     one of :def or :use
  :location canonical location key
  :in       incoming memory version
  :out      outgoing memory version

This intentionally models only straight-line versioning (no MemoryPhi)."
  (let ((annotations (make-hash-table :test #'eq))
        (version 0)
        (alias-roots (opt-compute-heap-aliases instructions)))
    (dolist (inst instructions annotations)
      (let ((loc (%opt-memory-location-key inst alias-roots)))
        (cond
          ((and loc (opt-memory-def-inst-p inst))
           (let ((vin version))
             (incf version)
             (setf (gethash inst annotations)
                   (list :kind :def :location loc :in vin :out version))))
          ((and loc (opt-memory-use-inst-p inst))
            (setf (gethash inst annotations)
                  (list :kind :use :location loc :in version :out version))))))))

(defun %opt-memory-ssa-copy-state (state)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (k v)
               (setf (gethash k copy) v))
             state)
    copy))

(defun %opt-memory-ssa-const-value-before-terminator (block reg)
  "Resolve REG to a block-local integer constant before BLOCK terminator, if known." 
  (let ((env (make-hash-table :test #'eq)))
    (dolist (inst (bb-instructions block))
      (when (typep inst '(or vm-jump vm-jump-zero vm-ret vm-halt))
        (return))
      (typecase inst
        (vm-const
         (let ((dst (vm-dst inst))
               (value (vm-value inst)))
           (when (and dst (integerp value))
             (setf (gethash dst env) value))))
        (vm-move
         (let ((dst (vm-dst inst))
               (src (vm-src inst)))
           (when dst
             (if src
                 (multiple-value-bind (value found-p) (gethash src env)
                   (if found-p
                       (setf (gethash dst env) value)
                       (remhash dst env)))
                 (remhash dst env)))))
        (t
         (when-let ((dst (opt-inst-dst inst)))
           (remhash dst env)))))
    (gethash reg env)))

(defun %opt-memory-ssa-edge-feasible-p (pred succ)
  "Return T when edge PRED->SUCC is feasible under local constant branch facts." 
  (let ((term (car (last (bb-instructions pred)))))
    (cond
      ((typep term 'vm-jump-zero)
       (let* ((cond-reg (vm-reg term))
              (target-label (vm-label-name term))
              (succ-label (and (bb-label succ) (vm-name (bb-label succ))))
              (const-value (%opt-memory-ssa-const-value-before-terminator pred cond-reg))
              (zero-edge-p (and succ-label (equal succ-label target-label))))
         (if const-value
             (if (zerop const-value)
                 zero-edge-p
                 (not zero-edge-p))
             t)))
      (t t))))

(defun %opt-memory-ssa-reachable-blocks (cfg)
  "Return EQ hash-table of blocks reachable under local constant branch pruning." 
  (let ((reachable (make-hash-table :test #'eq))
        (work nil))
    (when (cfg-entry cfg)
      (setf (gethash (cfg-entry cfg) reachable) t)
      (push (cfg-entry cfg) work))
    (loop while work
          do (let ((block (pop work)))
               (dolist (succ (bb-successors block))
                 (when (and (%opt-memory-ssa-edge-feasible-p block succ)
                            (not (gethash succ reachable)))
                   (setf (gethash succ reachable) t)
                   (push succ work)))))
    reachable))

(defun %opt-memory-ssa-merge-states (states)
  "Keep location->version facts only when all incoming states agree."
  (cond
    ((null states)
     (make-hash-table :test #'equal))
    ((null (cdr states))
     (%opt-memory-ssa-copy-state (car states)))
    (t
     (let* ((first (car states))
            (merged (%opt-memory-ssa-copy-state first))
            (dead nil))
       (maphash (lambda (key value)
                  (unless (every (lambda (state)
                                   (multiple-value-bind (other found-p)
                                       (gethash key state)
                                     (and found-p (eql value other))))
                                 (cdr states))
                    (push key dead)))
                first)
       (dolist (key dead)
         (remhash key merged))
       merged))))

(defun %opt-memory-ssa-state-equal (a b)
  (and (= (hash-table-count a) (hash-table-count b))
       (loop for key being the hash-keys of a
             always (multiple-value-bind (bv found-p)
                        (gethash key b)
                      (and found-p (eql (gethash key a) bv))))))

(defstruct (opt-memory-phi-node (:conc-name opt-memory-phi-))
  "Explicit MemoryPhi node metadata for a block-entry location join.

LOCATION is the canonical memory location key.
VERSION is the synthetic joined version assigned at block entry.
INCOMING is an alist of (pred-block . pred-version)."
  location
  version
  incoming)

(defun %opt-memory-ssa-loc-disagrees-p (loc first-version rest-states)
  "Return T when every state in REST-STATES defines an integer version for
LOC (making it a valid phi candidate) but at least one disagrees with
FIRST-VERSION."
  (and (every (lambda (state)
                (multiple-value-bind (version found-p)
                    (gethash loc state)
                  (and found-p (integerp version))))
              rest-states)
       (not (every (lambda (state)
                      (eql first-version (gethash loc state)))
                    rest-states))))

(defun %opt-memory-ssa-phi-version-for (block loc phi-version-table next-version)
  "Return (values VERSION NEW-NEXT-VERSION): the existing or freshly
allocated synthetic MemoryPhi version for BLOCK/LOC in PHI-VERSION-TABLE."
  (let* ((phi-key (list block loc))
         (version (gethash phi-key phi-version-table)))
    (if version
        (values version next-version)
        (let ((fresh (1+ next-version)))
          (setf (gethash phi-key phi-version-table) fresh)
          (values fresh fresh)))))

(defun %opt-memory-ssa-synthesize-entry-phis
    (block predecessor-states phi-version-table next-version)
  "Return synthetic MemoryPhi versions for BLOCK entry.

When BLOCK has multiple predecessors and all predecessor states define a
location but disagree on its version, synthesize (or reuse) a fresh version id
for that BLOCK/location pair. Returns two values:
  1) hash-table location -> synthesized version
  2) updated NEXT-VERSION"
  (let ((phis (make-hash-table :test #'equal)))
    (when (and predecessor-states (cdr predecessor-states))
      (let* ((first-state (car predecessor-states))
             (rest-states (cdr predecessor-states)))
        (when first-state
          (loop for loc being the hash-keys of first-state using (hash-value first-version)
                when (%opt-memory-ssa-loc-disagrees-p loc first-version rest-states)
                do (multiple-value-bind (version new-next)
                       (%opt-memory-ssa-phi-version-for block loc phi-version-table next-version)
                     (setf next-version new-next)
                     (setf (gethash loc phis) version))))))
    (values phis next-version)))

(defun opt-memory-ssa-version-at (inst annotations &key (point :in))
  "Return memory version for INST in ANNOTATIONS at POINT (:in or :out)."
  (when-let ((entry (gethash inst annotations)))
    (ecase point
      (:in (getf entry :in))
      (:out (getf entry :out)))))
