(in-package :cl-cc/optimize)

;;; ─── Static Single Assignment (SSA) Form ────────────────────────────────
;;;
;;; Constructs and destructs SSA form for the CL-CC VM instruction set.
;;;
;;; Construction (Cytron et al. 1991):
;;;   1. Compute CFG, dominator tree, dominance frontiers (in cfg.lisp)
;;;   2. Place phi-nodes at dominance frontiers of definition sites
;;;   3. Rename registers via DFS over the dominator tree
;;;
;;; Destruction (Sreedhar & Gao / Briggs-Cooper):
;;;   1. Replace each phi-node with parallel copy instructions in predecessors
;;;   2. Sequentialize parallel copies (handle swap cycles via temporaries)
;;;
;;; SSA values use the convention: original register :R5, version 3 → :R5.3
;;; Phi-node temporaries use #:PHI-<id> (uninterned symbols / gensyms)
;;;
;;; References:
;;;   Cytron, Ferrante, Rosen, Wegman, Zadeck (1991). TOPLAS.
;;;   Braun et al. (2013). "Simple and Efficient Construction of SSA Form."

;;; ─── SSA Value Naming ────────────────────────────────────────────────────

(defun ssa-versioned-reg (base-reg version)
  "Return an SSA-versioned register keyword :R<n>.<v> for BASE-REG and VERSION."
  (intern (format nil "~A.~A" (symbol-name base-reg) version) :keyword))

;;; ─── SSA Rename State ────────────────────────────────────────────────────

(defstruct (ssa-rename-state (:conc-name ssr-))
  "Per-register renaming state during SSA construction.
   Counters track how many versions each register has received.
   Stacks track the current live version at any point in the DFS."
  (counters (make-hash-table :test #'eq) :type hash-table) ; reg → next-version
  (stacks   (make-hash-table :test #'eq) :type hash-table)); reg → stack of versions

(defun ssr-push-new-version (state reg)
  "Assign a new version number to REG, push it onto the stack, return the version."
  (let* ((i   (or (gethash reg (ssr-counters state)) 0))
         (new  (ssa-versioned-reg reg i)))
    (setf (gethash reg (ssr-counters state)) (1+ i))
    (push new (gethash reg (ssr-stacks state)))
    new))

(defun ssr-current-version (state reg)
  "Return the current (most recent) SSA name for REG, or REG if not renamed."
  (or (car (gethash reg (ssr-stacks state))) reg))

(defun ssr-pop-version (state reg)
  "Pop the top version of REG from the rename stack."
  (when-let ((stack (gethash reg (ssr-stacks state))))
    (setf (gethash reg (ssr-stacks state)) (cdr stack))))

;;; ─── Phi-Node Representation ─────────────────────────────────────────────

(defstruct (ssa-phi (:conc-name phi-))
  "A phi-function inserted at the entry of a basic block.
   DST is the SSA-versioned destination register.
   ARGS is an alist of (predecessor-block . versioned-source-register).
   KIND distinguishes normal Cytron phis from LCSSA loop-boundary phis."
  (dst  nil)                           ; versioned SSA register keyword
  (args nil :type list)                ; ((bb . versioned-reg) ...)
  (reg  nil)                           ; original (pre-SSA) register name
  (kind :normal))                      ; :normal or :lcssa

(defun %ssa-block-use-def (block)
  "Return two lists: registers used before local definition, and definitions."
  (let ((uses nil)
        (defs nil))
    (dolist (inst (bb-instructions block))
      (dolist (r (opt-inst-read-regs inst))
        (when (and r (not (member r defs :test #'eq)))
          (pushnew r uses :test #'eq)))
      (when-let ((dst (ignore-errors (vm-dst inst))))
        (pushnew dst defs :test #'eq)))
    (values uses defs)))

(defun %ssa-init-live-maps (cfg)
  "Return (values USE-MAP DEF-MAP LIVE-IN LIVE-OUT), each an EQ hash-table
keyed by CFG block, with USE-MAP/DEF-MAP seeded from each block's local
use/def sets and LIVE-IN/LIVE-OUT initialized empty."
  (let ((use-map (make-hash-table :test #'eq))
        (def-map (make-hash-table :test #'eq))
        (live-in (make-hash-table :test #'eq))
        (live-out (make-hash-table :test #'eq)))
    (loop for b across (cfg-blocks cfg)
          do (multiple-value-bind (uses defs) (%ssa-block-use-def b)
               (setf (gethash b use-map) uses
                     (gethash b def-map) defs
                     (gethash b live-in) nil
                     (gethash b live-out) nil)))
    (values use-map def-map live-in live-out)))

(defun %ssa-live-sets-differ-p (old-in new-in old-out new-out)
  "Return T when either the in-set or the out-set changed from the
previous fixed-point iteration."
  (or (set-difference old-in new-in :test #'eq)
      (set-difference new-in old-in :test #'eq)
      (set-difference old-out new-out :test #'eq)
      (set-difference new-out old-out :test #'eq)))

(defun %ssa-recompute-block-live (b live-in live-out use-map def-map)
  "Recompute block B's live-in/live-out sets from its successors' live-in
sets. Updates LIVE-IN/LIVE-OUT in place and returns T when either set
changed."
  (let* ((old-in (gethash b live-in))
         (old-out (gethash b live-out))
         (new-out nil))
    (dolist (succ (bb-successors b))
      (setf new-out (union new-out (gethash succ live-in) :test #'eq)))
    (let ((new-in (union (gethash b use-map)
                         (set-difference new-out (gethash b def-map) :test #'eq)
                         :test #'eq)))
      (when (%ssa-live-sets-differ-p old-in new-in old-out new-out)
        (setf (gethash b live-in) new-in
              (gethash b live-out) new-out)
        t))))

(defun %ssa-compute-live-in (cfg)
  "Compute block live-in sets for pruned SSA phi placement."
  (multiple-value-bind (use-map def-map live-in live-out) (%ssa-init-live-maps cfg)
    (let ((changed t)
          (order (reverse (cfg-compute-rpo cfg))))
      (loop while changed
            do (setf changed nil)
               (dolist (b order)
                 (when (%ssa-recompute-block-live b live-in live-out use-map def-map)
                   (setf changed t)))))
    live-in))

;;; ─── Phi Placement (Cytron Algorithm) ───────────────────────────────────

(defun %ssa-collect-def-sites-and-uses (cfg)
  "Return (values def-sites used-regs): DEF-SITES maps register -> list of
defining blocks; USED-REGS maps register -> T when read anywhere in CFG."
  (let ((def-sites (make-hash-table :test #'eq))
        (used-regs (make-hash-table :test #'eq)))
    (loop for b across (cfg-blocks cfg)
          do (dolist (inst (bb-instructions b))
               (dolist (r (opt-inst-read-regs inst))
                 (when r
                   (setf (gethash r used-regs) t)))
               (when-let ((dst (ignore-errors (vm-dst inst))))
                 (pushnew b (gethash dst def-sites) :test #'eq))))
    (values def-sites used-regs)))

(defun %ssa-place-phis-for-reg (reg blocks live-in phi-map)
  "Insert a phi stub for REG at each block in IDF(BLOCKS) where REG is
live-in and not already covered by an existing phi in PHI-MAP."
  (let ((idf (cfg-idf blocks)))
    (dolist (f idf)
      (when (and (member reg (gethash f live-in) :test #'eq)
                 (not (find-if (lambda (p) (eq (phi-reg p) reg))
                               (gethash f phi-map))))
        (push (make-ssa-phi :reg reg) (gethash f phi-map))))))

(defun ssa-place-phis (cfg)
  "Insert phi-node stubs at dominance frontiers of definition sites.
   Returns a hash-table: block → list of ssa-phi stubs (with nil DST/ARGS,
   to be filled during renaming).

   Algorithm:
     For each variable v:
       defsites(v) = blocks where v is defined (has a dst write)
       Insert phi(v) at each block in IDF(defsites(v))
       if v is not already live-in there"
  ;; Step 1: collect all registers, their definition blocks, and live-in uses.
  ;; Pruned SSA: if a variable is not live-in at a frontier block, skip phi placement.
  (let ((live-in  (%ssa-compute-live-in cfg))
        (phi-map  (make-hash-table :test #'eq))) ; block → list of ssa-phi
    ;; Step 2: for each register, place phis at IDF(def-sites)
    (multiple-value-bind (def-sites used-regs) (%ssa-collect-def-sites-and-uses cfg)
      (maphash
       (lambda (reg blocks)
         (when (gethash reg used-regs)
           (%ssa-place-phis-for-reg reg blocks live-in phi-map)))
       def-sites))
    phi-map))

(defun %ssa-ensure-phi (phi-map block reg &optional (kind :normal))
  (unless (find-if (lambda (p) (eq (phi-reg p) reg))
                   (gethash block phi-map))
    (push (make-ssa-phi :reg reg :kind kind) (gethash block phi-map))))

(defun %ssa-loop-defined-regs (loop-blocks)
  (let ((defined (make-hash-table :test #'eq)))
    (dolist (b loop-blocks defined)
      (dolist (inst (bb-instructions b))
        (when-let ((dst (ignore-errors (vm-dst inst))))
          (setf (gethash dst defined) t))))))

(defun %ssa-loop-exit-blocks (loop-blocks)
  (let ((members (make-hash-table :test #'eq))
        (exits nil))
    (dolist (b loop-blocks)
      (setf (gethash b members) t))
    (dolist (b loop-blocks)
      (dolist (s (bb-successors b))
        (unless (gethash s members)
          (pushnew s exits :test #'eq))))
    (nreverse exits)))

(defun ssa-place-lcssa-phis (cfg phi-map)
  "Insert LCSSA phi stubs at natural-loop exits.

For each backedge tail -> header, place a phi for every loop-defined register
that is live-in at an exit block. This keeps loop-carried values represented at
the loop boundary instead of delaying the phi to a later outside-loop use."
  (let ((live-in (%ssa-compute-live-in cfg)))
    (loop for tail across (cfg-blocks cfg)
          do (dolist (header (bb-successors tail))
               (when (cfg-dominates-p header tail)
                 (let* ((loop-blocks (cfg-collect-natural-loop header tail))
                        (defined-regs (%ssa-loop-defined-regs loop-blocks))
                        (exit-blocks (%ssa-loop-exit-blocks loop-blocks)))
                   (dolist (exit exit-blocks)
                     (let ((exit-live-in (gethash exit live-in)))
                       (loop for reg being the hash-keys of defined-regs
                             do (when (member reg exit-live-in :test #'eq)
                                  (%ssa-ensure-phi phi-map exit reg :lcssa)))))))))
    phi-map))

;;; ─── SSA Renaming (DFS over dominator tree) ──────────────────────────────

(defun %ssa-rename-block-phis (b state phi-map)
  "Push a fresh SSA version for every phi at block B, setting its PHI-DST.
Return the list of original phi registers pushed (most-recently-pushed
first), for later popping."
  (let ((pushed-regs nil))
    (dolist (phi (gethash b phi-map) pushed-regs)
      (let ((new-dst (ssr-push-new-version state (phi-reg phi))))
        (setf (phi-dst phi) new-dst)
        (push (phi-reg phi) pushed-regs)))))

(defun %ssa-rename-instruction (inst state)
  "Return (values RENAMED-INST PUSHED-REG): INST with its read registers
substituted for their current SSA versions and its destination register (if
any) renamed to a fresh version. PUSHED-REG is that destination's original
register, or NIL when INST has no destination."
  (let* ((reads (opt-inst-read-regs inst))
         (renamed-inst
           (if reads
               (let ((subst (make-hash-table :test #'eq)))
                 (dolist (r reads)
                   (setf (gethash r subst) (ssr-current-version state r)))
                 (opt-rewrite-inst-regs inst subst))
               inst))
         (dst (ignore-errors (vm-dst inst))))
    (if dst
        (let ((new-dst (ssr-push-new-version state dst)))
          (values (ssa-rewrite-dst renamed-inst dst new-dst) dst))
        (values renamed-inst nil))))

(defun %ssa-rename-block-instructions (b state)
  "Rename every instruction in block B. Return (values NEW-INSTS
PUSHED-REGS): the renamed instructions in order, and the destination
registers pushed during renaming (most-recently-pushed first), for later
popping."
  (let ((new-insts nil)
        (pushed-regs nil))
    (dolist (inst (bb-instructions b))
      (multiple-value-bind (renamed-inst pushed-reg) (%ssa-rename-instruction inst state)
        (when pushed-reg (push pushed-reg pushed-regs))
        (push renamed-inst new-insts)))
    (values (nreverse new-insts) pushed-regs)))

(defun %ssa-rename-fill-successor-phi-args (b state phi-map)
  "For each phi in each successor of B, record B's current SSA version of
the phi's register as an incoming argument."
  (dolist (succ (bb-successors b))
    (dolist (phi (gethash succ phi-map))
      (push (cons b (ssr-current-version state (phi-reg phi))) (phi-args phi)))))

(defun %ssa-rename-block (b state phi-map renamed)
  "DFS rename pass for one block B, mutating STATE, PHI-MAP, and RENAMED."
  (let ((phi-pushed (%ssa-rename-block-phis b state phi-map)))
    (multiple-value-bind (new-insts inst-pushed) (%ssa-rename-block-instructions b state)
      (setf (gethash b renamed) new-insts)
      (%ssa-rename-fill-successor-phi-args b state phi-map)
      (dolist (child (bb-dom-children b))
        (%ssa-rename-block child state phi-map renamed))
      ;; Pop in reverse push order: instruction destinations were pushed
      ;; after the block's phis, so they must be popped first.
      (dolist (r (append inst-pushed phi-pushed))
        (ssr-pop-version state r)))))

(defun ssa-rename (cfg phi-map)
  "Rename all register references in CFG to SSA form.
   Fills in phi-node DST registers and argument registers.
   Returns (values renamed-instructions phi-map)."
  (let ((state   (make-ssa-rename-state))
        (renamed (make-hash-table :test #'eq)))
    (when (cfg-entry cfg)
      (%ssa-rename-block (cfg-entry cfg) state phi-map renamed))
    (values renamed phi-map)))

;;; Trivial phi elimination (ssa-eliminate-trivial-phis) and ssa-rewrite-dst
;;; are in ssa-phi-elim.lisp.
