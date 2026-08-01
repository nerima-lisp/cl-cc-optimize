# API Reference

Every symbol below is exported from `cl-cc/optimize`. Sections mirror the concept groups `src/package.lisp` uses to organize its `:export` form.

Struct accessors and constructors are documented from their `defstruct` slot and options; passes and other functions are documented from their own docstring, unedited except for light rewrapping.


## effects.lisp — effect-kind classification

### `vm-inst-effect-kind`

```lisp
(cl-cc/optimize:vm-inst-effect-kind (inst))
```

Return the effect-kind of VM instruction INST.
Effect kinds: :pure :read-only :alloc :io :write-global :control :unknown.
Unlisted types (vm-call, vm-apply, vm-generic-call, etc.) default to :unknown.

### `opt-inst-pure-p`

```lisp
(cl-cc/optimize:opt-inst-pure-p (inst))
```

T if INST has no side effects and produces a deterministic result.
Extended from the original 2-type (vm-const vm-move) whitelist to cover
100+ instruction types.  Pure instructions are both CSE-eligible and
DCE-eligible.

### `opt-inst-dce-eligible-p`

```lisp
(cl-cc/optimize:opt-inst-dce-eligible-p (inst))
```

T if INST is eligible for dead code elimination when its result is unused.
Covers :pure (no side effects) and :alloc (allocation only — if the
allocated object is never used, the allocation can be removed).

### `opt-inst-cse-eligible-p`

```lisp
(cl-cc/optimize:opt-inst-cse-eligible-p (inst))
```

T if INST is eligible for common subexpression elimination.
Only :pure instructions guarantee the same result for the same inputs
regardless of intervening instructions.  :alloc creates distinct objects
even with the same arguments, so it is NOT CSE-eligible.

### `effect-row->effect-kind`

```lisp
(cl-cc/optimize:effect-row->effect-kind (effect-row))
```

Convert a cl-cc/type system type-effect-row to an optimizer effect-kind.
Used when callee effect information is available from the HM type system.
Compares effect names by string= to avoid cross-package symbol mismatch.


## effects-known-functions.lisp — known function property registry

### `*known-function-property-table*`

Maps uppercase CL function names to known function property keywords.

### `register-known-function-properties`

```lisp
(cl-cc/optimize:register-known-function-properties (function-name properties))
```

Register or extend known PROPERTIES for FUNCTION-NAME.

### `known-function-properties`

```lisp
(cl-cc/optimize:known-function-properties (function-name))
```

Return known property keywords for FUNCTION-NAME.
Unknown functions return NIL so callers can remain conservative.

### `known-function-property-p`

```lisp
(cl-cc/optimize:known-function-property-p (function-name property))
```

Return true when FUNCTION-NAME is known to have PROPERTY.

### `known-function-effect-kind`

```lisp
(cl-cc/optimize:known-function-effect-kind (function-name))
```

Return the optimizer effect-kind implied by FUNCTION-NAME properties.


## cfg.lisp — basic blocks and CFG (also cfg-analysis.lisp, cfg-dominance.lisp, cfg-ddg.lisp, cfg-layout.lisp)

### `basic-block`

A maximal straight-line sequence of VM instructions with a single entry
and single exit.  The block's control flow edges are stored as predecessor
and successor lists of basic-block structs.

Constructed with `make-basic-block`; slots use the `bb-` accessor prefix: `id`, `label`, `instructions`, `predecessors`, `successors`, `idom`, `dom-children`, `dom-frontier`, `post-idom`, `post-children`, `vm-osr-entry`, `loop-depth`, `rpo-index`.

### `make-basic-block`

Construct a `basic-block` -- A maximal straight-line sequence of VM instructions with a single entry
and single exit.  The block's control flow edges are stored as predecessor
and successor lists of basic-block structs.

### `basic-block-p`

Type predicate: true when its argument is a `basic-block`.

### `bb-id`

Accessor for the `id` slot of `basic-block`.

### `bb-label`

Accessor for the `label` slot of `basic-block`.

### `bb-instructions`

Accessor for the `instructions` slot of `basic-block`.

### `bb-predecessors`

Accessor for the `predecessors` slot of `basic-block`.

### `bb-successors`

Accessor for the `successors` slot of `basic-block`.

### `bb-idom`

Accessor for the `idom` slot of `basic-block`.

### `bb-dom-children`

Accessor for the `dom-children` slot of `basic-block`.

### `bb-dom-frontier`

Accessor for the `dom-frontier` slot of `basic-block`.

### `bb-post-idom`

Accessor for the `post-idom` slot of `basic-block`.

### `bb-post-children`

Accessor for the `post-children` slot of `basic-block`.

### `bb-vm-osr-entry`

Accessor for the `vm-osr-entry` slot of `basic-block`.

### `bb-loop-depth`

Accessor for the `loop-depth` slot of `basic-block`.

### `bb-rpo-index`

Accessor for the `rpo-index` slot of `basic-block`.

### `opt-ddg-node`

Node in a loop-body data dependence graph.  DISTANCE is stored on edges, so
loop-carried dependencies can contribute to recurrence MII.

Constructed with `make-opt-ddg-node`; slots use the `opt-ddg-node-` accessor prefix: `index`, `inst`, `latency`, `predecessors`, `successors`.

### `make-opt-ddg-node`

Construct a `opt-ddg-node` -- Node in a loop-body data dependence graph.  DISTANCE is stored on edges, so
loop-carried dependencies can contribute to recurrence MII.

### `opt-ddg-node-p`

Type predicate: true when its argument is a `opt-ddg-node`.

### `opt-ddg-node-index`

Accessor for the `index` slot of `opt-ddg-node`.

### `opt-ddg-node-inst`

Accessor for the `inst` slot of `opt-ddg-node`.

### `opt-ddg-node-latency`

Accessor for the `latency` slot of `opt-ddg-node`.

### `opt-ddg-node-predecessors`

Accessor for the `predecessors` slot of `opt-ddg-node`.

### `opt-ddg-node-successors`

Accessor for the `successors` slot of `opt-ddg-node`.

### `cfg`

Control Flow Graph for a single function / compilation unit.
Blocks are identified by integer IDs.  Entry is always block 0.

Constructed with `make-cfg`; slots use the `cfg-` accessor prefix: `blocks`, `entry`, `exit`, `label->block`, `next-id`.

### `make-cfg`

Construct a `cfg` -- Control Flow Graph for a single function / compilation unit.
Blocks are identified by integer IDs.  Entry is always block 0.

### `cfg-p`

Type predicate: true when its argument is a `cfg`.

### `cfg-blocks`

Accessor for the `blocks` slot of `cfg`.

### `cfg-entry`

Accessor for the `entry` slot of `cfg`.

### `cfg-exit`

Accessor for the `exit` slot of `cfg`.

### `cfg-label->block`

Accessor for the `label->block` slot of `cfg`.

### `cfg-next-id`

Accessor for the `next-id` slot of `cfg`.

### `cfg-build`

```lisp
(cl-cc/optimize:cfg-build (instructions))
```

Build a CFG from a flat list of VM INSTRUCTIONS.
Returns a cfg struct with all basic blocks, edges, entry, and exit set.

Algorithm:
1. Mark leaders: index 0, every jump target, fall-throughs after branches.
2. Allocate a basic block per leader.
3. Populate each block's instruction list and wire fall-through / jump edges.

### `cfg-block-count`

```lisp
(cl-cc/optimize:cfg-block-count (cfg))
```

Return the number of basic blocks in CFG.

### `cfg-get-block-by-label`

```lisp
(cl-cc/optimize:cfg-get-block-by-label (cfg label-name))
```

Return the basic-block for the given LABEL-NAME, or NIL.

### `cfg-compute-rpo`

```lisp
(cl-cc/optimize:cfg-compute-rpo (cfg))
```

Compute reverse post-order (RPO) for CFG blocks starting from entry.
Sets bb-rpo-index for each reachable block.
Returns a list of blocks in RPO order.

### `cfg-compute-dominators`

```lisp
(cl-cc/optimize:cfg-compute-dominators (cfg))
```

Compute immediate dominators for all blocks in CFG using Cooper et al.'s
simple iterative algorithm (2001).  Sets bb-idom for each block.
Returns the entry block (root of the dominator tree).

### `cfg-compute-post-dominators`

```lisp
(cl-cc/optimize:cfg-compute-post-dominators (cfg))
```

Compute immediate post-dominators for all blocks in CFG.
Traverses the reverse CFG starting from CFG's exit block. Sets bb-post-idom
for each reachable block and populates bb-post-children lists.
Returns the exit block (root of the post-dominator tree).

### `cfg-compute-dominance-frontiers`

```lisp
(cl-cc/optimize:cfg-compute-dominance-frontiers (cfg))
```

Compute dominance frontiers for all blocks in CFG.
Sets bb-dom-frontier for each block.
DF(b) = { y | ∃ x ∈ pred(y) such that b dom x and b !strictdom y }

Algorithm (Cytron et al.):
For each block y with ≥2 predecessors:
For each predecessor x of y:
Walk up dominator tree from x to idom(y) (exclusive),
adding y to DF(runner) at each step.

### `cfg-dominates-p`

```lisp
(cl-cc/optimize:cfg-dominates-p (a b))
```

T if block A dominates block B (A is an ancestor of B in the dominator tree).

### `cfg-post-dominates-p`

```lisp
(cl-cc/optimize:cfg-post-dominates-p (a b))
```

T if block A post-dominates block B (A is an ancestor of B in the post-dominator tree).

### `cfg-idf`

```lisp
(cl-cc/optimize:cfg-idf (def-blocks))
```

Compute the iterated dominance frontier (IDF) of DEF-BLOCKS.
Returns the set of join points where phi-nodes must be placed.
DEF-BLOCKS themselves are NOT included unless they appear in a frontier.

Algorithm (Cytron et al.):
visited tracks processed nodes to prevent infinite loops.
result contains only blocks that are in some dominance frontier.

### `cfg-flatten`

```lisp
(cl-cc/optimize:cfg-flatten (cfg))
```

Emit a flat instruction list from the CFG (for round-trip testing).
Blocks are emitted in RPO order.  Each block's opening label (if any)
is prepended to the block's instruction list.

### `cfg-mark-osr-loop-headers`

```lisp
(cl-cc/optimize:cfg-mark-osr-loop-headers (cfg))
```

Annotate loop-header blocks with FR-521 VM OSR entry metadata.

The metadata is kept on BASIC-BLOCK via BB-VM-OSR-ENTRY so lower pipeline stages
can generate interpreter-vreg to JIT-preg mappings and OSR stubs only when
CL-CC/VM:*OSR-ENABLED* is true.

### `cfg-split-critical-edges`

```lisp
(cl-cc/optimize:cfg-split-critical-edges (cfg))
```

Split critical edges by inserting empty landing-pad blocks.

A critical edge is an edge from a block with multiple successors to a block
with multiple predecessors.  This pass inserts a fresh block on each such
edge and rewires the CFG so later SSA / code-motion passes can place code on
the split edge without duplicating it along other incoming paths.

### `cfg-build-ddg`

```lisp
(cl-cc/optimize:cfg-build-ddg (instructions &key (latency-fn (lambda (_inst) (declare (ignore _inst)) 1))))
```

Build a conservative DDG for a straight-line loop body.

Edges encode RAW, WAR, and WAW register hazards in program order.  If the last
definition of a register in the body feeds an earlier use/definition in the next
iteration, a loop-carried edge with DISTANCE=1 is added for recurrence-MII.

### `cfg-ddg-add-edge`

```lisp
(cl-cc/optimize:cfg-ddg-add-edge (nodes from to &key (distance 0)))
```

Add a DDG edge FROM → TO over NODES with loop-carried DISTANCE metadata.

### `cfg-ddg-edges`

```lisp
(cl-cc/optimize:cfg-ddg-edges (nodes))
```

Return all unique DDG edges in NODES.

### `cfg-compute-mii`

```lisp
(cl-cc/optimize:cfg-compute-mii (nodes &key (issue-width 1)))
```

Compute a conservative minimum initiation interval for DDG NODES.

MII = max(resource MII, recurrence MII).  Resource MII is the total latency per
iteration divided by ISSUE-WIDTH.  Recurrence MII considers explicit loop-carried
edges and rounds producer latency / dependence distance upward.


## ssa.lisp — SSA data structures + phi placement

### `ssa-rename-state`

Per-register renaming state during SSA construction.
Counters track how many versions each register has received.
Stacks track the current live version at any point in the DFS.

Constructed with `make-ssa-rename-state`; slots use the `ssr-` accessor prefix: `counters`, `stacks`.

### `make-ssa-rename-state`

Construct a `ssa-rename-state` -- Per-register renaming state during SSA construction.
Counters track how many versions each register has received.
Stacks track the current live version at any point in the DFS.

### `ssa-rename-state-p`

Type predicate: true when its argument is a `ssa-rename-state`.

### `ssr-counters`

Accessor for the `counters` slot of `ssa-rename-state`.

### `ssr-stacks`

Accessor for the `stacks` slot of `ssa-rename-state`.

### `ssr-push-new-version`

```lisp
(cl-cc/optimize:ssr-push-new-version (state reg))
```

Assign a new version number to REG, push it onto the stack, return the version.

### `ssr-current-version`

```lisp
(cl-cc/optimize:ssr-current-version (state reg))
```

Return the current (most recent) SSA name for REG, or REG if not renamed.

### `ssr-pop-version`

```lisp
(cl-cc/optimize:ssr-pop-version (state reg))
```

Pop the top version of REG from the rename stack.

### `ssa-phi`

A phi-function inserted at the entry of a basic block.
DST is the SSA-versioned destination register.
ARGS is an alist of (predecessor-block . versioned-source-register).
KIND distinguishes normal Cytron phis from LCSSA loop-boundary phis.

Constructed with `make-ssa-phi`; slots use the `phi-` accessor prefix: `dst`, `args`, `reg`, `kind`.

### `make-ssa-phi`

Construct a `ssa-phi` -- A phi-function inserted at the entry of a basic block.
DST is the SSA-versioned destination register.
ARGS is an alist of (predecessor-block . versioned-source-register).
KIND distinguishes normal Cytron phis from LCSSA loop-boundary phis.

### `ssa-phi-p`

Type predicate: true when its argument is a `ssa-phi`.

### `phi-dst`

Accessor for the `dst` slot of `ssa-phi`.

### `phi-args`

Accessor for the `args` slot of `ssa-phi`.

### `phi-reg`

Accessor for the `reg` slot of `ssa-phi`.

### `phi-kind`

Accessor for the `kind` slot of `ssa-phi`.

### `ssa-versioned-reg`

```lisp
(cl-cc/optimize:ssa-versioned-reg (base-reg version))
```

Return an SSA-versioned register keyword :R<n>.<v> for BASE-REG and VERSION.

### `ssa-construct`

```lisp
(cl-cc/optimize:ssa-construct (instructions))
```

Construct SSA form from a flat VM INSTRUCTIONS list.
Returns (values cfg phi-map renamed-map) where:
cfg         — the CFG with dominator information
phi-map     — hash-table block → list of ssa-phi
renamed-map — hash-table block → list of renamed vm-instructions

### `ssa-destroy`

```lisp
(cl-cc/optimize:ssa-destroy (cfg phi-map renamed-map))
```

Destroy SSA form: replace phi-nodes with parallel copies in predecessors.
Returns a flat instruction list in RPO order.
Uses the same-RPO ordering for deterministic output.

### `ssa-round-trip`

```lisp
(cl-cc/optimize:ssa-round-trip (instructions))
```

Construct and immediately destruct SSA form.
Returns a flat instruction list that should be semantically equivalent
to the input.  Used for integration testing.

### `ssa-place-phis`

```lisp
(cl-cc/optimize:ssa-place-phis (cfg))
```

Insert phi-node stubs at dominance frontiers of definition sites.
Returns a hash-table: block → list of ssa-phi stubs (with nil DST/ARGS,
to be filled during renaming).

Algorithm:
For each variable v:
defsites(v) = blocks where v is defined (has a dst write)
Insert phi(v) at each block in IDF(defsites(v))
if v is not already live-in there

### `ssa-rename`

```lisp
(cl-cc/optimize:ssa-rename (cfg phi-map))
```

Rename all register references in CFG to SSA form.
Fills in phi-node DST registers and argument registers.
Returns (values renamed-instructions phi-map).

### `ssa-sequentialize-copies`

```lisp
(cl-cc/optimize:ssa-sequentialize-copies (parallel-copies))
```

Convert a list of parallel copies (dst . src) to a sequential list of
vm-move instructions that produces the same effect.

Handles the swap problem: if A←B and B←A appear simultaneously and both
values are integer registers, emit an XOR-swap instead of using a temp.
Larger cycles still use a temporary register to break the cycle.

Algorithm: build the copy dependency DAG and repeatedly emit copies whose
destination is not read by remaining copies.  Cycles are detected when the
DAG has no ready leaf.

### `ssa-eliminate-trivial-phis`

```lisp
(cl-cc/optimize:ssa-eliminate-trivial-phis (phi-map renamed-map))
```

Collapse trivial phi-nodes and remove dead phi nodes.


## egraph.lisp — E-graph data structures

### `e-node`

An e-node represents a function application f(c1, c2, ...) where each
argument ci is an e-class ID (not a concrete value).  Structurally
identical e-nodes (same op + same canonicalized child IDs) are the same.

Constructed with `make-e-node`; slots use the `en-` accessor prefix: `op`, `children`, `eclass`.

### `make-e-node`

Construct a `e-node` -- An e-node represents a function application f(c1, c2, ...) where each
argument ci is an e-class ID (not a concrete value).  Structurally
identical e-nodes (same op + same canonicalized child IDs) are the same.

### `e-node-p`

Type predicate: true when its argument is a `e-node`.

### `en-op`

Accessor for the `op` slot of `e-node`.

### `en-children`

Accessor for the `children` slot of `e-node`.

### `en-eclass`

Accessor for the `eclass` slot of `e-node`.

### `e-class`

An e-class is an equivalence class of expressions that are all equal.
It holds a list of e-nodes and analysis data (type, cost bounds, etc.).

Constructed with `make-e-class`; slots use the `ec-` accessor prefix: `id`, `nodes`, `parents`, `data`.

### `make-e-class`

Construct a `e-class` -- An e-class is an equivalence class of expressions that are all equal.
It holds a list of e-nodes and analysis data (type, cost bounds, etc.).

### `e-class-p`

Type predicate: true when its argument is a `e-class`.

### `ec-id`

Accessor for the `id` slot of `e-class`.

### `ec-nodes`

Accessor for the `nodes` slot of `e-class`.

### `ec-parents`

Accessor for the `parents` slot of `e-class`.

### `ec-data`

Accessor for the `data` slot of `e-class`.

### `e-graph`

The E-graph maintains a union-find structure over e-classes, a memo table
for hash-consing, and a worklist of pending merges.

Constructed with `make-e-graph`; slots use the `eg-` accessor prefix: `classes`, `memo`, `union-find`, `worklist`, `next-id`.

### `make-e-graph`

Construct a `e-graph` -- The E-graph maintains a union-find structure over e-classes, a memo table
for hash-consing, and a worklist of pending merges.

### `e-graph-p`

Type predicate: true when its argument is a `e-graph`.

### `eg-classes`

Accessor for the `classes` slot of `e-graph`.

### `eg-memo`

Accessor for the `memo` slot of `e-graph`.

### `eg-union-find`

Accessor for the `union-find` slot of `e-graph`.

### `eg-worklist`

Accessor for the `worklist` slot of `e-graph`.

### `eg-next-id`

Accessor for the `next-id` slot of `e-graph`.


## egraph-saturation.lisp — saturation + extraction

### `egraph-find`

```lisp
(cl-cc/optimize:egraph-find (eg id))
```

Path-compressed union-find lookup.  Returns the canonical e-class ID for ID.

### `egraph-add`

```lisp
(cl-cc/optimize:egraph-add (eg op &rest child-ids))
```

Add an e-node with operation OP and CHILD-IDS to the e-graph.
Returns the e-class ID.
If a structurally identical node already exists (memo hit), returns its class.

### `egraph-merge`

```lisp
(cl-cc/optimize:egraph-merge (eg id1 id2))
```

Assert that e-classes ID1 and ID2 are equal.
Queues the merge on the worklist (deferred rebuild for efficiency).
Returns the canonical ID of the merged class.

### `egraph-rebuild`

```lisp
(cl-cc/optimize:egraph-rebuild (eg))
```

Process the worklist of pending merges to restore the congruence invariant.

### `egraph-saturate`

```lisp
(cl-cc/optimize:egraph-saturate (eg rules &key (limit 30) (fuel 10000)))
```

Apply RULES to EG until saturation (no new merges) or resource limits.
LIMIT: maximum number of full-pass iterations.
FUEL:  maximum total merges across all iterations.
Returns (values saturated-p iterations fuel-remaining).

### `egraph-extract`

```lisp
(cl-cc/optimize:egraph-extract (eg root-id &optional (cost-fn #'egraph-default-cost)))
```

Bottom-up extraction: for each e-class reachable from ROOT-ID,
pick the e-node with the minimum cost.
Returns an s-expression (nested list) representing the cheapest term.

COST-FN: (op children-costs) → numeric cost.

### `egraph-default-cost`

```lisp
(cl-cc/optimize:egraph-default-cost (op children-costs))
```

Default cost model: base latency + sum of children costs.
Constants are free; simple arithmetic costs 1; calls cost 10+.
Compares by symbol-name so cross-package symbol variants work.

### `egraph-stats`

```lisp
(cl-cc/optimize:egraph-stats (eg))
```

Return a plist of e-graph statistics for debugging.

### `egraph-pattern-var-p`

```lisp
(cl-cc/optimize:egraph-pattern-var-p (x))
```

T if X is a pattern variable: a symbol whose name starts with '?'.

### `egraph-match-pattern`

```lisp
(cl-cc/optimize:egraph-match-pattern (eg pattern class-id &optional bindings))
```

Match PATTERN against the e-class CLASS-ID in e-graph EG.
Returns a list of all binding alists that satisfy the match, or NIL.
Pattern variables (?x) bind to e-class IDs.


## egraph-rules.lisp — rewrite rules

### `defrule`

```lisp
(cl-cc/optimize:defrule (name pattern replacement &key when))
```

Define an e-graph rewrite rule guard and assert a matching Prolog fact.

### `egraph-rule-register`

```lisp
(cl-cc/optimize:egraph-rule-register (name lhs rhs when-fn))
```

Register the guard function for NAME.
Rule structure itself is sourced from the Prolog rule database emitted by `defrule`.

### `egraph-builtin-rules`

```lisp
(cl-cc/optimize:egraph-builtin-rules ())
```

Return the list of built-in e-graph rewrite rules.
The primary source of truth is the Prolog `egraph-rule` fact set emitted by
`defrule`; the in-memory guard table is consulted only to attach existing `:when`
predicates to the fact-backed rule records.


## egraph-rules-advanced.lisp — entry point

### `optimize-with-egraph`

```lisp
(cl-cc/optimize:optimize-with-egraph (instructions &key (rules (egraph-builtin-rules)) (saturation-limit 30) (saturation-fuel 10000) (cost-fn (function egraph-default-cost))))
```

Optimize a list of VM INSTRUCTIONS using e-graph equality saturation.
Returns an optimized instruction list.

Algorithm:
1. Add instructions to e-graph (building reg→class mapping)
2. Saturate with RULES until fixed-point or resource limit
3. Lower destination classes proven equal to constants or register aliases

This pass is wired into the main optimizer pipeline via :egraph and also
participates in the broader :prolog-rewrite stage.


## cfg-dominance.lisp — additional CFG utilities

### `cfg-compute-loop-depths`

```lisp
(cl-cc/optimize:cfg-compute-loop-depths (cfg))
```

Annotate each block with a simple natural-loop nesting depth.
A backedge is any edge whose target dominates its source.


## cfg-layout.lisp — code layout

### `cfg-flatten-hot-cold`

```lisp
(cl-cc/optimize:cfg-flatten-hot-cold (cfg))
```

Emit a branch-aware hot/cold flat instruction list from CFG.

For vm-jump-zero, the fall-through successor is treated as the hot path and is
placed immediately after the conditional block.  Explicit jump targets and
condition/error blocks are placed later, with cold blocks deferred to the end of
the function.  Any non-branch fall-through edge broken by the new order is made
explicit with a vm-jump so label references and control flow remain valid.

### `opt-pass-hot-cold-layout`

```lisp
(cl-cc/optimize:opt-pass-hot-cold-layout (instructions))
```

Reorder basic blocks for I-cache locality using a hot/cold CFG layout.

This is a layout-only optimization: it preserves instruction objects and label
targets, builds a CFG, emits vm-jump-zero fall-through paths contiguously, and
moves cold error/signalling blocks to the function tail.


## optimizer-tables.lisp — instruction introspection

### `opt-inst-dst`

```lisp
(cl-cc/optimize:opt-inst-dst (inst))
```

Return the single destination register written by INST, or NIL.
Returns NIL for instructions that do not write a destination (jump, halt,
ret, set-global, slot-write, etc.) or for unrecognised types.

## optimizer-tables-regs.lisp — read-register dispatch

### `opt-inst-read-regs`

```lisp
(cl-cc/optimize:opt-inst-read-regs (inst))
```

Return a list of all register names read by INST.
Dispatch: zero-read types → nil, move → src, binop → lhs/rhs,
binary/unary tables, then *opt-read-regs-table* for specific types,
finally sexp-reflection fallback.


## optimizer-tables.lisp — instruction introspection

### `opt-falsep`

```lisp
(cl-cc/optimize:opt-falsep (value))
```

Compile-time analog of vm-falsep: T if VALUE is falsy.

The optimizer uses the same language-level truthiness as the VM and CPS layers:
both NIL and numeric zero are false.


## optimizer-dataflow.lisp — generic dataflow helpers

### `opt-dataflow-result`

Stores per-block IN and OUT states for a dataflow analysis.

Constructed with `make-opt-dataflow-result`; slots use the `opt-dataflow-result-` accessor prefix: `cfg`, `direction`, `in`, `out`.

### `make-opt-dataflow-result`

Construct a `opt-dataflow-result` -- Stores per-block IN and OUT states for a dataflow analysis.

### `opt-dataflow-result-p`

Type predicate: true when its argument is a `opt-dataflow-result`.

### `opt-dataflow-result-cfg`

Accessor for the `cfg` slot of `opt-dataflow-result`.

### `opt-dataflow-result-direction`

Accessor for the `direction` slot of `opt-dataflow-result`.

### `opt-dataflow-result-in`

Accessor for the `in` slot of `opt-dataflow-result`.

### `opt-dataflow-result-out`

Accessor for the `out` slot of `opt-dataflow-result`.

### `opt-run-dataflow`

```lisp
(cl-cc/optimize:opt-run-dataflow (cfg &key (direction :forward) meet transfer (state-equal #'equal) (initial-state nil) (boundary-state initial-state) (copy-state #'identity)))
```

Run a generic worklist dataflow analysis over CFG.

MEET merges a list of predecessor/successor states. TRANSFER computes the new
output state for BLOCK from its incoming state. STATE-EQUAL compares states for
convergence. INITIAL-STATE seeds unreachable edges; BOUNDARY-STATE is used at
the entry block for forward analyses and exit block for backward analyses.

### `define-dataflow-pass`

```lisp
(cl-cc/optimize:define-dataflow-pass (name lambda-list &rest options))
```

Define NAME as a thin wrapper around OPT-RUN-DATAFLOW.

### `opt-compute-available-expressions`

```lisp
(cl-cc/optimize:opt-compute-available-expressions (cfg-or-instructions))
```

No docstring; behavior documented only by its call sites and tests.

### `opt-compute-reaching-definitions`

```lisp
(cl-cc/optimize:opt-compute-reaching-definitions (cfg-or-instructions))
```

No docstring; behavior documented only by its call sites and tests.


## prolog-peephole.lisp — Prolog peephole rewriting

### `apply-prolog-peephole`

```lisp
(cl-cc/optimize:apply-prolog-peephole (instructions))
```

Apply Prolog-unification peephole rules over two-instruction windows.

Rule format: each rule in cl-cc/prolog::*peephole-rules* is a three-element list
(CURRENT-PATTERN NEXT-PATTERN REPLACEMENT-LIST)
On a match, both instructions are consumed and REPLACEMENT-LIST sexps emitted.
Self-moves (:move :Rx :Rx) are removed in a pre-pass.

### `opt-pass-superopt`

```lisp
(cl-cc/optimize:opt-pass-superopt (instructions))
```

FR-750 peephole superoptimizer.

Enumerates move-instruction sequences up to *OPT-SUPEROPT-MAX-LENGTH* (default
N=3), exhaustively tests them over *OPT-SUPEROPT-INPUT-SPACE*, and replaces a
window with the shortest equivalent sequence found.  The implementation is
deliberately infrastructure-focused and side-effect-safe: only move-only windows
are interpreted, so labels, jumps, calls, allocations, and VM semantics outside
register copies are never changed.

### `*opt-superopt-max-length*`

Maximum peephole window length enumerated by OPT-PASS-SUPEROPT.

### `*opt-superopt-input-space*`

Exhaustive scalar input values used for peephole equivalence testing.


## optimizer-memory-alias.lisp — alias analysis

### `opt-compute-heap-aliases`

```lisp
(cl-cc/optimize:opt-compute-heap-aliases (instructions))
```

Compute a conservative EQ hash-table reg -> canonical heap root.
This is a small FR-115 style oracle intended for downstream passes.

### `opt-compute-points-to`

```lisp
(cl-cc/optimize:opt-compute-points-to (instructions))
```

Compute conservative flow-sensitive points-to roots for INSTRUCTIONS.

This FR-018 helper intentionally models a single canonical fresh heap root per
register in straight-line code. Fresh heap producers create roots, vm-move
propagates them, and later non-heap writes kill stale facts. Branch joins and
field-sensitive object graphs remain out of scope for this helper.

### `opt-points-to-root`

```lisp
(cl-cc/optimize:opt-points-to-root (reg points-to))
```

Return REG's canonical root under POINTS-TO as two values: root and found-p.

### `opt-compute-simple-inductions`

```lisp
(cl-cc/optimize:opt-compute-simple-inductions (instructions))
```

Return reg -> opt-induction-var summaries for simple affine updates.

Recognized patterns are intentionally conservative: affine self-updates,
geometric self-updates `(mul dst dst const)`, two-instruction affine recurrences
`dst = dst * c + d`, and derived variables `j = i + c` where `i` is already an
induction variable. Existing affine callers continue to read OPT-IV-STEP.

### `opt-compute-loop-inductions`

```lisp
(cl-cc/optimize:opt-compute-loop-inductions (cfg-or-instructions))
```

Return header-block -> (reg -> opt-induction-var) for CFG natural loops.

The analysis uses CFG backedges (`tail -> header` where HEADER dominates TAIL)
and `cfg-collect-natural-loop` to keep induction facts scoped to each loop.
Constants from non-loop predecessor blocks seed the per-loop SCEV scan.

### `opt-induction-trip-count`

```lisp
(cl-cc/optimize:opt-induction-trip-count (init limit step &key inclusive-p predicate))
```

Return a conservative integer trip count for an affine induction variable.

The loop condition is interpreted as `< limit` for positive STEP and `> limit`
for negative STEP. With INCLUSIVE-P, the corresponding boundary is `<=` or `>=`.
PREDICATE may be a comparison instruction class/symbol such as `vm-le`, `vm-ge`,
or `vm-eq`; it overrides INCLUSIVE-P when supplied. Returns NIL when STEP is
zero except equality predicates, which are single-test guards.

### `opt-iv-reg`

Accessor for the `reg` slot of `opt-induction-var`.

### `opt-iv-init`

Accessor for the `init` slot of `opt-induction-var`.

### `opt-iv-step`

Accessor for the `step` slot of `opt-induction-var`.

### `opt-iv-update-inst`

Accessor for the `update-inst` slot of `opt-induction-var`.

### `opt-compute-cfg-value-ranges`

```lisp
(cl-cc/optimize:opt-compute-cfg-value-ranges (cfg-or-instructions))
```

Compute conservative CFG-aware integer value ranges.

Returns an OPT-DATAFLOW-RESULT with per-block IN/OUT maps. Join points keep
only registers known on every incoming path and union their intervals.
Self-updating destinations are killed conservatively to guarantee termination.

### `opt-compute-path-sensitive-ranges`

```lisp
(cl-cc/optimize:opt-compute-path-sensitive-ranges (instructions))
```

Compute value ranges with branch predicate narrowing.
Returns a hash-table mapping (block . reg) to (lo . hi) interval.

### `opt-block-reg-range`

```lisp
(cl-cc/optimize:opt-block-reg-range (block reg))
```

Return the latest path-sensitive entry interval for REG at BLOCK as (LO . HI).

The table is populated by OPT-COMPUTE-PATH-SENSITIVE-RANGES.  NIL is returned
when BLOCK has no recorded fact for REG.

### `opt-compute-value-ranges`

```lisp
(cl-cc/optimize:opt-compute-value-ranges (instructions))
```

Compute conservative integer value ranges.

Straight-line callers keep the existing reg -> interval hash-table API. When
INSTRUCTIONS contain control flow, this wrapper runs CFG-aware analysis and
returns the merged exit-state interval table for convenience callers.

### `opt-array-bounds-check-eliminable-p`

```lisp
(cl-cc/optimize:opt-array-bounds-check-eliminable-p (index-reg length-reg intervals &optional block))
```

Return T when INTERVALS prove INDEX-REG is within LENGTH-REG bounds.

INTERVALS may be the classic reg -> interval table, or the path-sensitive
(block . reg) -> interval table returned by OPT-COMPUTE-PATH-SENSITIVE-RANGES
when BLOCK is supplied.

### `opt-mark-bounds-check-eliminable`

```lisp
(cl-cc/optimize:opt-mark-bounds-check-eliminable (inst &key array-reg index-reg length-reg block))
```

Annotate INST as having a proven redundant array bounds check.

The VM has no unchecked array instruction yet, so BCE records conservative
side-table metadata and returns INST unchanged. Downstream codegen can query this
metadata without requiring cross-package instruction-shape changes.

### `opt-bounds-check-eliminable-metadata`

```lisp
(cl-cc/optimize:opt-bounds-check-eliminable-metadata (inst))
```

Return BCE metadata for INST, or NIL when INST is not annotated.

### `opt-bounds-check-eliminable-marked-p`

```lisp
(cl-cc/optimize:opt-bounds-check-eliminable-marked-p (inst))
```

Return T when INST has been annotated by the BCE pass.

## optimizer-interval-arithmetic.lisp — interval arithmetic

### `opt-interval-add`

```lisp
(cl-cc/optimize:opt-interval-add (a b))
```

Add two intervals conservatively.

### `opt-interval-sub`

```lisp
(cl-cc/optimize:opt-interval-sub (a b))
```

Subtract interval B from A conservatively.

### `opt-interval-bit-width`

```lisp
(cl-cc/optimize:opt-interval-bit-width (interval))
```

Return a conservative unsigned bit-width upper bound for INTERVAL.

Only non-negative intervals are assigned a width. Mixed-sign or fully-negative
intervals return NIL rather than pretending the result is narrower than proven.

### `opt-interval-known-bits-mask`

```lisp
(cl-cc/optimize:opt-interval-known-bits-mask (interval))
```

Return a conservative bit mask covering every bit that may be set.

For a non-negative interval with width W, the result is 2^W-1. Bits outside
that mask are therefore known zero for every value in INTERVAL. Returns NIL
when INTERVAL has no proven non-negative width bound.

### `opt-interval-fits-fixnum-width-p`

```lisp
(cl-cc/optimize:opt-interval-fits-fixnum-width-p (interval &optional (limit-width (integer-length +opt-range-positive-infinity+))))
```

Return T when INTERVAL's unsigned width is strictly below LIMIT-WIDTH.

### `opt-interval-fits-fixnum-p`

```lisp
(cl-cc/optimize:opt-interval-fits-fixnum-p (interval))
```

Return T when INTERVAL is proven to stay within the host fixnum range.

### `opt-interval-widen`

```lisp
(cl-cc/optimize:opt-interval-widen (old new &key (negative-infinity +opt-range-negative-infinity+) (positive-infinity +opt-range-positive-infinity+)))
```

Widen OLD toward NEW for monotone interval fixpoint convergence.

When NEW extends below OLD, the lower bound becomes NEGATIVE-INFINITY.  When
NEW extends above OLD, the upper bound becomes POSITIVE-INFINITY.  Bounds that
do not move remain unchanged.  NIL OLD is treated as bottom and returns NEW.

### `opt-interval-logand`

```lisp
(cl-cc/optimize:opt-interval-logand (a b))
```

Return a conservative interval for LOGAND over A and B.

If either operand is a known non-negative singleton mask, the result is bounded
to [0, mask] even when the other operand is unknown. When both operands are
proven non-negative, the result is also bounded above by the smaller upper
bound. Returns NIL when no safe bound is known.

## optimizer-interval-transfer.lisp — interval transfer functions

### `opt-pass-elide-proven-overflow-checks`

```lisp
(cl-cc/optimize:opt-pass-elide-proven-overflow-checks (instructions))
```

Elide FR-303 checked arithmetic when interval analysis proves fixnum safety.


## optimizer-memory-alias.lisp — alias analysis

### `opt-may-alias-by-type-p`

```lisp
(cl-cc/optimize:opt-may-alias-by-type-p (reg-a reg-b points-to heap-kinds))
```

Return T if REG-A and REG-B may alias under type-based heap classification.

If both roots and kinds are known and the kinds differ, return NIL. Otherwise
stay conservative and return T.

### `opt-may-alias-p`

```lisp
(cl-cc/optimize:opt-may-alias-p (reg-a reg-b alias-roots &optional type-facts))
```

Return T when REG-A and REG-B may alias under ALIAS-ROOTS.

Unknown roots remain conservative and therefore return T.

### `opt-must-alias-p`

```lisp
(cl-cc/optimize:opt-must-alias-p (reg-a reg-b alias-roots))
```

Return T when REG-A and REG-B definitely alias under ALIAS-ROOTS.

### `opt-compute-heap-type-facts`

```lisp
(cl-cc/optimize:opt-compute-heap-type-facts (instructions &optional alias-roots))
```

Compute conservative heap type facts for TBAA.

Returns an EQ hash-table mapping both fresh allocation roots and currently-known
register aliases to one of `:cons', `:array', or `:closure'.  The table is built
from the same root facts used by `opt-compute-heap-aliases', so downstream
callers can pass it directly to `opt-tbaa-must-not-alias-p'.

### `opt-tbaa-must-not-alias-p`

```lisp
(cl-cc/optimize:opt-tbaa-must-not-alias-p (obj1-reg obj2-reg type-facts))
```

Return T when TBAA type facts prove OBJ1-REG and OBJ2-REG cannot alias.

Heap identities with different concrete heap kinds (`:cons', `:array',
`:closure') are distinct runtime object families.  Unknown or equal kinds remain
conservative and return NIL.

### `opt-memory-tbaa-metadata`

```lisp
(cl-cc/optimize:opt-memory-tbaa-metadata (inst metadata))
```

Return the TBAA entry for INST from METADATA, or NIL.

### `opt-build-memory-tbaa-metadata`

```lisp
(cl-cc/optimize:opt-build-memory-tbaa-metadata (instructions))
```

Build reusable TBAA metadata for memory instructions in INSTRUCTIONS.

### `opt-may-alias-with-tbaa-p`

```lisp
(cl-cc/optimize:opt-may-alias-with-tbaa-p (reg-a reg-b alias-roots type-facts))
```

Return T when REG-A and REG-B may alias after points-to and TBAA checks.

## optimizer-memory-safepoints.lisp — safepoint placement

### `opt-prune-dominated-safepoints`

```lisp
(cl-cc/optimize:opt-prune-dominated-safepoints (cfg root-set-analysis))
```

Remove safepoint B when dominated by safepoint A with the same root set.

### `opt-hoist-safepoints-to-back-edges`

```lisp
(cl-cc/optimize:opt-hoist-safepoints-to-back-edges (cfg))
```

Move safely-hoistable loop safepoints in back-edge blocks to back-edge polls.


## optimizer-memory-alias.lisp — alias analysis (continued)

### `opt-sink-allocations`

```lisp
(cl-cc/optimize:opt-sink-allocations (instructions cfg alias-facts))
```

Sink branch-local allocations into the conditional branch that uses them.

Currently handles `vm-cons' and `vm-make-array'.  An allocation is moved from a
branching block into the unique successor branch that dominates all uses of its
destination register.  The pass is intentionally conservative: operands must be
available at the target entry and all uses must remain dominated by the sunk
location.  ALIAS-FACTS is accepted for pipeline integration and future escape
checks; this foundation does not weaken existing alias safety.

### `opt-analyze-memory-access-patterns`

```lisp
(cl-cc/optimize:opt-analyze-memory-access-patterns (cfg-or-instructions memory-ssa))
```

Analyze array memory accesses and classify their access patterns.

Returns an EQ hash-table keyed by memory access instruction.  Each value is a
plist containing `:stride', `:pattern' (`:sequential', `:strided', or `:random'),
and metadata flags consumed by future prefetch (FR-289) and loop-tiling (FR-287)
passes.  Constant-stride evidence is derived from consecutive array accesses,
constant index registers, and simple loop induction summaries.

### `opt-pass-cons-slot-forward`

```lisp
(cl-cc/optimize:opt-pass-cons-slot-forward (instructions))
```

Forward car/cdr of a fresh vm-cons to moves from the original slot registers.

Within a single basic block the pass is fully precise: an vm-cons followed by
vm-car/vm-cdr on the same destination register is replaced by a vm-move from the
original slot source.  Across control-flow boundaries (labels, jumps, calls,
unknown effects, and heap mutations) all facts are cleared conservatively.
Overwrites of a tracked cell register or slot source kill only the dependent facts.

CFG-level join-point forwarding is handled by opt-pass-cons-slot-forward-cfg in
optimizer-memory-forward.lisp, which runs on the full CFG and requires all
predecessors to agree on the same slot fact.

### `opt-pass-bounds-check-elimination`

```lisp
(cl-cc/optimize:opt-pass-bounds-check-elimination (instructions))
```

Annotate array accesses whose bounds checks are provably redundant.

This FR-039 pass is intentionally non-rewriting until unchecked VM array
instructions exist. It builds CFG facts, computes dominators and loop depths,
uses path-sensitive integer ranges, and marks proven `vm-aref` / `vm-aset`
instructions with BCE metadata. The returned instruction list is valid and keeps
the original instruction objects/order.


## optimizer-inline-call-site-split.lisp — call-site splitting

### `opt-known-callee-labels`

```lisp
(cl-cc/optimize:opt-known-callee-labels (instructions))
```

Return reg -> known callee label mapping tracked through simple designators.

### `opt-pass-call-site-splitting`

```lisp
(cl-cc/optimize:opt-pass-call-site-splitting (instructions))
```

Duplicate join-block call sites into predecessors with known callees.
Handles consecutive multi-join labels and `vm-call`/`vm-tail-call`/`vm-apply`.
The original join call remains available for fall-through and unknown preds.

## optimizer-devirt.lisp — devirtualization

### `opt-pass-devirtualize`

```lisp
(cl-cc/optimize:opt-pass-devirtualize (instructions))
```

Run whole-program CHA devirtualization, then the local devirt pass.


## optimizer-closure.lisp — closure allocation sharing

### `opt-pass-closure-capture-dedup`

```lisp
(cl-cc/optimize:opt-pass-closure-capture-dedup (instructions))
```

FR-330: share duplicate closure environments with one vm-make-closure.

Closures are grouped with cl-cc/ast:group-shareable-closures by code label and
capture set.  The first closure in each safe group performs the allocation;
later closure sites become vm-move aliases to the first closure register.

### `opt-pass-closure-thunk-sharing`

```lisp
(cl-cc/optimize:opt-pass-closure-thunk-sharing (instructions))
```

FR-079 closure thunk sharing: eliminate redundant closure allocations.

When two simple closure instructions share both entry-label and capture set, and
the first closure register + its captured environment registers are not
overwritten between the two sites, the second allocation is replaced by a
vm-move to the first closure register.  This is a conservative partial
implementation; same-code-body-with-different-environments requires VM-level
support for separate code-pointer + environment-record (FR-079 extension).


## optimizer-demand.lisp — VM parameter demand analysis

### `opt-demand-summary`

Per-function demand summary for VM parameters.
DEMANDS is an alist of (param-reg . (:strict | :lazy | :absent)).  STRICT-PARAMS
are safe candidates for unboxed/stack-local calling-convention treatment;
ABSENT-PARAMS let call-site cleanup replace unused argument values, enabling DCE
of pure argument computations.

Constructed with `make-opt-demand-summary`; slots use the `opt-demand-summary-` accessor prefix: `function`, `params`, `demands`, `strict-params`, `absent-params`.

### `make-opt-demand-summary`

Construct a `opt-demand-summary` -- Per-function demand summary for VM parameters.
DEMANDS is an alist of (param-reg . (:strict | :lazy | :absent)).  STRICT-PARAMS
are safe candidates for unboxed/stack-local calling-convention treatment;
ABSENT-PARAMS let call-site cleanup replace unused argument values, enabling DCE
of pure argument computations.

### `opt-demand-summary-p`

Type predicate: true when its argument is a `opt-demand-summary`.

### `opt-demand-summary-function`

Accessor for the `function` slot of `opt-demand-summary`.

### `opt-demand-summary-params`

Accessor for the `params` slot of `opt-demand-summary`.

### `opt-demand-summary-demands`

Accessor for the `demands` slot of `opt-demand-summary`.

### `opt-demand-summary-strict-params`

Accessor for the `strict-params` slot of `opt-demand-summary`.

### `opt-demand-summary-absent-params`

Accessor for the `absent-params` slot of `opt-demand-summary`.

### `*opt-demand-summary-table*`

Latest FR-182 demand summaries keyed by function label.

### `opt-analyze-function-demand`

```lisp
(cl-cc/optimize:opt-analyze-function-demand (function params body-instructions))
```

Analyze PARAM usage in BODY-INSTRUCTIONS for FUNCTION label.

### `opt-analyze-program-demand`

```lisp
(cl-cc/optimize:opt-analyze-program-demand (instructions))
```

Analyze all VM closures/functions in INSTRUCTIONS and return a summary table.

### `opt-pass-demand-analysis`

```lisp
(cl-cc/optimize:opt-pass-demand-analysis (instructions))
```

Run FR-182 demand analysis and expose call-site cleanup for absent params.

The pass is conservative: it preserves arity, replacing only absent argument
registers with a fresh NIL constant at known direct call sites. This lets later
DCE remove pure computations that fed those now-unused registers, while effectful
argument computations remain in the instruction stream.


## optimizer-recognition-loop-idiom.lisp — loop idiom recognition

### `opt-pass-fill-recognition`

```lisp
(cl-cc/optimize:opt-pass-fill-recognition (instructions))
```

Collapse a canonical zero-based array fill loop into a side-effecting vm-fill.

This is the conservative FR-055 slice for full-vector fill idioms.  The pass
preserves final loop temporaries after the bulk fill so later code observing
the induction or condition registers keeps the same values.

### `opt-pass-copy-recognition`

```lisp
(cl-cc/optimize:opt-pass-copy-recognition (instructions))
```

Collapse a canonical zero-based array copy loop into a bounded bulk copy.

This is the copy-seq/memcpy side of FR-055.  It is deliberately conservative:
it only fires when existing heap-root alias facts prove the source and
destination arrays cannot be the same object, preserving in-place overlapping
copy loops for the regular VM executor.

## optimizer-vectorize.lisp — auto-vectorization

### `opt-pass-auto-vectorization`

```lisp
(cl-cc/optimize:opt-pass-auto-vectorization (instructions))
```

FR-226: vectorize independent scalar array ops in counted loops.

The pass recognizes a conservative one-dimensional array-map loop, emits a SIMD
vector loop strip-mined by `*opt-simd-lane-count*`, and retains a scalar
remainder loop for tail iterations.  Dynamic trip-count loops are left unchanged;
compile-time trip counts make the generated vector-limit constant explicit.

## optimizer-vectorize-slp.lisp — SLP vectorization

### `opt-pass-slp-vectorize`

```lisp
(cl-cc/optimize:opt-pass-slp-vectorize (instructions))
```

FR-227: pack straight-line scalar array-map lanes into SIMD vector ops.

The pass builds a CFG, scans each basic block for isomorphic independent scalar
chains of adjacent `vm-aref` → arithmetic/bitwise op → `vm-aset` lanes, and
replaces each full superword with one existing `vm-simd-vector-op` marker.  It is
conservative and idempotent: existing SIMD markers cause the input to be left
unchanged, preventing repeated packing on subsequent optimizer iterations.

### `vm-simd-vector-op`

A VM instruction type defined by `cl-cc-vm`, inherited here through `:use`
rather than owned by this package. Represents a lane-wise SIMD operation
(`op`) over `lhs-array`/`rhs-array` at `index-reg`, writing `dst-array`, with
a fixed `lanes` count and `element-type`. Produced by
[`opt-pass-slp-vectorize`](#opt-pass-slp-vectorize) and
[`opt-pass-auto-vectorization`](#opt-pass-auto-vectorization) when scalar
array accesses in adjacent loop iterations can be fused into one vector op.

### `make-vm-simd-vector-op`

Construct a `vm-simd-vector-op`. See
[`cl-cc-vm`](https://github.com/nerima-lisp/cl-cc-vm)'s own API reference for
the full keyword-argument list.

### `vm-simd-vector-op-p`

Type predicate: true when its argument is a `vm-simd-vector-op`.

## optimizer-outlining.lisp — function outlining + safepoint polling

### `opt-pass-function-outlining`

```lisp
(cl-cc/optimize:opt-pass-function-outlining (instructions))
```

FR-294: outline duplicate pure straight-line sequences into shared helpers.

### `opt-pass-safepoint-polling`

```lisp
(cl-cc/optimize:opt-pass-safepoint-polling (instructions))
```

FR-233: insert safepoint flag checks at function entries and loop backedges.

## optimizer-swp.lisp — software pipelining

### `opt-pass-software-pipelining`

```lisp
(cl-cc/optimize:opt-pass-software-pipelining (instructions))
```

FR-200: modulo-schedule canonical loop bodies.

The pass builds a DDG for each pure counted-loop body, computes MII, and emits a
prologue/kernel/epilogue layout.  It is intentionally conservative: side-effecting
or non-canonical loops are preserved, and generated labels make the pass
idempotent in the convergence pipeline.

### `opt-modulo-schedule-loop-body`

```lisp
(cl-cc/optimize:opt-modulo-schedule-loop-body (body &key (issue-width 1)))
```

Build the DDG for BODY, compute MII, and return a modulo schedule.

Values are (scheduled-core ddg mii).  BODY may include the induction update as
its final instruction; the scheduler excludes that update and lets callers emit
it at the end of the kernel.

## optimizer-branch-weights.lisp — branch prediction metadata

### `opt-analyze-branch-weights`

```lisp
(cl-cc/optimize:opt-analyze-branch-weights (instructions))
```

Annotate conditional branches with static branch-prediction metadata.

The analysis is intentionally non-transforming: instruction order and control-flow
targets are preserved.  It marks branches to blocks containing VM condition/error
signaling instructions as cold/unlikely, loop back-edge branches as likely taken,
and branches directly targeting return/exit blocks as not-taken.

### `opt-branch-weight`

```lisp
(cl-cc/optimize:opt-branch-weight (inst))
```

Return branch prediction metadata for INST, or NIL when it is unannotated.


## optimizer-flow-type-check-elim.lisp

### `opt-pass-dominated-type-check-elim`

```lisp
(cl-cc/optimize:opt-pass-dominated-type-check-elim (instructions))
```

Eliminate redundant pure type predicates dominated by an earlier identical
predicate on the same source register. Nil checks via vm-not are treated
the same way. The first check is kept; later checks are replaced with
vm-move from the dominating result register.

## optimizer-flow-loop-prefetch.lisp

### `opt-pass-prefetch-insertion`

```lisp
(cl-cc/optimize:opt-pass-prefetch-insertion (instructions))
```

Insert backend-neutral VM-PREFETCH hints in simple list and array loops.

Patterns:
- loop bodies containing VM-CDR get PREFETCHT0/PLDL1KEEP [cons+8]
- loop bodies containing VM-AREF get PREFETCHNTA/PLDL1STRM [array+i*8+32]

The pass is idempotent within a loop range and does not alter control flow.


## extended optimizer pass files

### `opt-pass-loop-rotate`

```lisp
(cl-cc/optimize:opt-pass-loop-rotate (instructions))
```

FR-683 loop rotation pass.

Only single-latch while-header loops are rotated. Multi-back-edge loops and
loops with internal labels are left untouched to avoid correctness hazards.

### `opt-pass-dead-loop-elimination`

```lisp
(cl-cc/optimize:opt-pass-dead-loop-elimination (instructions))
```

FR-686 dead loop elimination.

Runs after ordinary DCE in the default pipeline and removes pure loops whose
loop-defined values are unused after the loop. Any volatile marker or side
effect keeps the loop intact.

### `opt-pass-loop-unroll`

```lisp
(cl-cc/optimize:opt-pass-loop-unroll (instructions))
```

FR-601: unroll conservative counted loops.

Full unroll is applied when the trip count is known at compile time and is in
the small range 1..8. Unknown or larger loops get a factor-4 guarded prefix plus
the original loop as the residual loop. Expanded bodies are cleaned with constant
folding and CSE before returning.

### `opt-pass-loop-unswitch`

```lisp
(cl-cc/optimize:opt-pass-loop-unswitch (instructions))
```

FR-602: hoist loop-invariant conditionals out of conservative counted loops.

The pass duplicates one loop into true/false specialized versions and emits an
outer conditional dispatch. It is intentionally code-size increasing and is
therefore policy-gated in the default pipeline by SPEED=3 and SPACE=0.

### `opt-pass-dead-argument-elimination`

```lisp
(cl-cc/optimize:opt-pass-dead-argument-elimination (instructions))
```

FR-606: create specialized functions with unused positional parameters removed.

This pass is closed-world within the current instruction stream.  Calls whose
callee register is statically known are rewritten to a generated specialized
label and receive a shorter argument list.  Original functions are retained for
unknown call sites and future cross-module/LTO use.  Parameters captured by an
inner closure are treated as used and are never removed.

### `opt-pass-ipcp`

```lisp
(cl-cc/optimize:opt-pass-ipcp (instructions))
```

Create constant-specialized function versions for direct call sites.

Specialized labels use the required ORIGINAL-FUNCTION-IPCP-{hash} spelling.  The
pass is closed-world/LTO friendly but remains safe outside LTO: if no direct
target or constant argument is known, it leaves the stream unchanged.

### `opt-pass-tail-duplication`

```lisp
(cl-cc/optimize:opt-pass-tail-duplication (instructions))
```

Duplicate profitable shared tail blocks into CFG predecessors.

The pass considers multi-predecessor tail blocks up to
*OPT-TAIL-DUP-MAX-INSTRUCTIONS*, handles unconditional predecessors directly,
and handles conditional taken edges by inserting an edge pad before duplicating.
Duplication is only applied when it removes at least one jump.

### `opt-pass-iv-strength-reduce`

```lisp
(cl-cc/optimize:opt-pass-iv-strength-reduce (instructions))
```

FR-681 induction-variable strength reduction.

Recognizes simple counted loops and converts loop-body `i * constant` uses into
a derived induction variable initialized before the loop and advanced by
`step * constant` at the latch.  The pass is deliberately conservative: it only
handles single-latch label/cmp/jump-zero/body/step/jump loops with known integer
step and multiplier constants, and leaves other loops unchanged.

### `opt-pass-div-by-const`

```lisp
(cl-cc/optimize:opt-pass-div-by-const (instructions))
```

FR-681 division-by-constant lowering for non-power-of-two integer divisors.

This pass exposes the non-power-of-two subset separately from
`opt-pass-strength-reduce`.  Divisors that are powers of two are intentionally
left unchanged here because the existing shift lowering owns that case.  The
implementation delegates to the FR-685 helper, which also handles modulo by
constant through the same quotient sequence.

### `opt-pass-loop-peel`

```lisp
(cl-cc/optimize:opt-pass-loop-peel (instructions))
```

FR-682 loop peeling pass.

Peels the first-iteration boundary-check slice of conservative single-latch
counted loops whose body contains array boundary checks or null/type checks.
The peeled copy is emitted before the original header with the original guard,
and array accesses in the peeled copy are annotated for BCE.  Loops with labels,
calls, internal branches, or other shapes that could make the first iteration
semantically distinct are left unchanged.

### `opt-pass-idiom-recognition`

```lisp
(cl-cc/optimize:opt-pass-idiom-recognition (instructions))
```

FR-684: recognize memset, memcpy, strlen, and popcount loop idioms.

The pass is deliberately conservative: it only rewrites unit-stride, zero-based,
private-loop shapes whose body matches the canonical VM instruction sequence.

### `opt-pass-value-range-propagation`

```lisp
(cl-cc/optimize:opt-pass-value-range-propagation (instructions))
```

FR-610: compute path-sensitive value ranges for virtual registers.

The interval domain is the existing optimizer interval representation `(LO . HI)`,
with `*MIN-INF*` and `*MAX-INF*` as conservative finite sentinels.  The transfer
function handles constants, moves, arithmetic (including `vm-add`), bitwise masks,
and branch-condition refinement through `vm-jump-zero` fed by comparison
instructions such as `(> x 0)` (`vm-gt`).  This pass records facts for following
passes and leaves the instruction stream unchanged.

### `opt-pass-overflow-check-elimination`

```lisp
(cl-cc/optimize:opt-pass-overflow-check-elimination (instructions))
```

FR-613: replace checked integer arithmetic when VRP proves fixnum safety.

`vm-add-checked` and `vm-mul-checked` are rewritten to their unchecked integer
forms only when interval arithmetic proves the result stays within the host fixnum
range.  Otherwise the checked instruction is preserved.

### `opt-pass-bitwidth-reduction`

```lisp
(cl-cc/optimize:opt-pass-bitwidth-reduction (instructions))
```

FR-614: record safe i64→i32→i8 narrowing decisions after VRP.

The VM instruction set has no separate i8/i32 arithmetic opcodes, so this pass
attaches conservative metadata consumed by backends.  No type is narrowed unless
the proven interval is non-negative and fits the narrower range at runtime.

### `opt-pass-cps-reduce`

```lisp
(cl-cc/optimize:opt-pass-cps-reduce (instructions))
```

FR-674 optimizer pass entry point.
VM instruction streams are returned unchanged; CPS S-expression pipelines can
call the same pass function directly and receive reduced CPS output.

### `opt-pass-defunctionalize`

```lisp
(cl-cc/optimize:opt-pass-defunctionalize (instructions))
```

FR-676: convert constant-proven higher-order calls to direct vm-call sites.

### `opt-pass-delimited-continuations`

```lisp
(cl-cc/optimize:opt-pass-delimited-continuations (instructions))
```

FR-677 explicit pass for delimited continuations.

VM instruction streams are returned unchanged.  Source-like S-expression callers
can run the pass explicitly to lower RESET/SHIFT into continuation-passing direct
forms.  This pass is registered by ASDF/package exports only and is not part of
the default optimizer convergence pipeline.

### `opt-pass-escape-analysis`

```lisp
(cl-cc/optimize:opt-pass-escape-analysis (instructions))
```

FR-516: mark heap allocations that do not escape function scope.

Non-escaping allocations are annotated for downstream lowering as
`:replacement-op :vm-stack-alloc'.  The pass leaves bytecode unchanged unless a
future VM stack-allocation instruction is present, preserving correctness for
all unproven or potentially escaping objects.

### `opt-path-profile-block`

Ball-Larus metadata for one basic block.

Constructed with `make-opt-path-profile-block`; slots use the `opt-path-profile-block-` accessor prefix: `block-id`, `label`, `path-id`, `successor-count`, `execution-count`, `path-count`.

### `make-opt-path-profile-block`

Construct a `opt-path-profile-block` -- Ball-Larus metadata for one basic block.

### `opt-path-profile-block-p`

Type predicate: true when its argument is a `opt-path-profile-block`.

### `opt-path-profile-block-block-id`

Accessor for the `block-id` slot of `opt-path-profile-block`.

### `opt-path-profile-block-label`

Accessor for the `label` slot of `opt-path-profile-block`.

### `opt-path-profile-block-path-id`

Accessor for the `path-id` slot of `opt-path-profile-block`.

### `opt-path-profile-block-successor-count`

Accessor for the `successor-count` slot of `opt-path-profile-block`.

### `opt-path-profile-block-execution-count`

Accessor for the `execution-count` slot of `opt-path-profile-block`.

### `opt-path-profile-block-path-count`

Accessor for the `path-count` slot of `opt-path-profile-block`.

### `opt-ball-larus-edge`

Instrumentable CFG edge with assigned Ball-Larus VALUE.

Constructed with `make-opt-ball-larus-edge`; slots use the `opt-ball-larus-edge-` accessor prefix: `from`, `to`, `value`, `backedge-p`, `exit-p`.

### `make-opt-ball-larus-edge`

Construct a `opt-ball-larus-edge` -- Instrumentable CFG edge with assigned Ball-Larus VALUE.

### `opt-ball-larus-edge-p`

Type predicate: true when its argument is a `opt-ball-larus-edge`.

### `opt-ball-larus-edge-from`

Accessor for the `from` slot of `opt-ball-larus-edge`.

### `opt-ball-larus-edge-to`

Accessor for the `to` slot of `opt-ball-larus-edge`.

### `opt-ball-larus-edge-value`

Accessor for the `value` slot of `opt-ball-larus-edge`.

### `opt-ball-larus-edge-backedge-p`

Accessor for the `backedge-p` slot of `opt-ball-larus-edge`.

### `opt-ball-larus-edge-exit-p`

Accessor for the `exit-p` slot of `opt-ball-larus-edge`.

### `opt-ball-larus-profile`

Complete Ball-Larus profile plan for one instruction stream.

Constructed with `make-opt-ball-larus-profile`; slots use the `opt-ball-larus-profile-` accessor prefix: `cfg`, `blocks`, `edges`, `paths`, `instrumented-instructions`, `function-id`.

### `make-opt-ball-larus-profile`

Construct a `opt-ball-larus-profile` -- Complete Ball-Larus profile plan for one instruction stream.

### `opt-ball-larus-profile-p`

Type predicate: true when its argument is a `opt-ball-larus-profile`.

### `opt-ball-larus-profile-cfg`

Accessor for the `cfg` slot of `opt-ball-larus-profile`.

### `opt-ball-larus-profile-blocks`

Accessor for the `blocks` slot of `opt-ball-larus-profile`.

### `opt-ball-larus-profile-edges`

Accessor for the `edges` slot of `opt-ball-larus-profile`.

### `opt-ball-larus-profile-paths`

Accessor for the `paths` slot of `opt-ball-larus-profile`.

### `opt-ball-larus-profile-instrumented-instructions`

Accessor for the `instrumented-instructions` slot of `opt-ball-larus-profile`.

### `opt-block-version-plan`

Basic block versioning plan derived from Ball-Larus path counters.

Constructed with `make-opt-block-version-plan`; slots use the `opt-block-version-plan-` accessor prefix: `hot-threshold`, `versions`.

### `make-opt-block-version-plan`

Construct a `opt-block-version-plan` -- Basic block versioning plan derived from Ball-Larus path counters.

### `opt-block-version-plan-p`

Type predicate: true when its argument is a `opt-block-version-plan`.

### `opt-block-version-plan-hot-threshold`

Accessor for the `hot-threshold` slot of `opt-block-version-plan`.

### `opt-block-version-plan-versions`

Accessor for the `versions` slot of `opt-block-version-plan`.

### `opt-compute-path-profile`

```lisp
(cl-cc/optimize:opt-compute-path-profile (instructions &key (counts nil)))
```

Compute Ball-Larus path metadata for INSTRUCTIONS.

COUNTS may be keyed by block label/id for compatibility with older callers; path
counter tables are consumed by OPT-BUILD-BLOCK-VERSION-PLAN.

### `opt-build-ball-larus-profile`

```lisp
(cl-cc/optimize:opt-build-ball-larus-profile (instructions &key (function-id :anonymous)))
```

Build the Ball-Larus analysis and instrumented instruction stream.

### `opt-instrument-path-profile`

```lisp
(cl-cc/optimize:opt-instrument-path-profile (instructions &key (function-id :anonymous)))
```

Return INSTRUCTIONS instrumented with real Ball-Larus path profiling code.

### `opt-identify-hot-paths`

```lisp
(cl-cc/optimize:opt-identify-hot-paths (profile counts &key (hot-threshold 1) (limit nil)))
```

Return hot Ball-Larus paths sorted by decreasing execution count.

### `opt-build-block-version-plan`

```lisp
(cl-cc/optimize:opt-build-block-version-plan (path-profile &key (hot-threshold 1) counts limit))
```

Build a hot-path block-versioning plan from Ball-Larus counters.

### `opt-duplicate-hot-paths`

```lisp
(cl-cc/optimize:opt-duplicate-hot-paths (instructions counts &key (hot-threshold 1) (function-id :anonymous) (limit 4)))
```

Append path-specific superblock clones for hot Ball-Larus paths.

The clones are emitted as independent basic-block versions.  Later layout or tier
selection can redirect execution to them safely because each superblock preserves
the original instructions for its path while removing internal branch terminators
whose outcomes are known on that path.

### `opt-pass-path-profiling`

```lisp
(cl-cc/optimize:opt-pass-path-profiling (instructions))
```

Optimizer pipeline hook: insert Ball-Larus path profiling instrumentation.

### `opt-load-widening-candidate-p`

```lisp
(cl-cc/optimize:opt-load-widening-candidate-p (a b &optional alias-roots))
```

Return true when A and B are adjacent loads eligible for FR-723 widening.

A candidate is two byte-lane loads from the same memory family and definitely
the same base object, with integer offsets N and N+1.  The two-byte widened
access must be naturally aligned because the current VM has no unaligned access
marker.

### `opt-pass-store-coalescing`

```lisp
(cl-cc/optimize:opt-pass-store-coalescing (instructions &key alias-roots))
```

Combine adjacent narrow stores into packed naturally-aligned wider stores.

This is store coalescing, not dead-store elimination: no overwritten store is
dropped.  Consecutive byte stores to offsets N, N+1, ... are packed with
LOGAND/ASH/LOGIOR and replaced by one store at offset N when the resulting
power-of-two byte access is naturally aligned.  Groups are never formed across
intervening instructions, so alias boundaries and side effects remain intact.

### `opt-pass-load-widening-store-coalescing`

```lisp
(cl-cc/optimize:opt-pass-load-widening-store-coalescing (instructions &key alias-roots))
```

Run FR-723 adjacent load widening and adjacent store coalescing.

Load widening replaces adjacent byte loads with one wider memory read and a set
of extraction instructions that preserve the original destination registers.
Store coalescing replaces adjacent byte stores with packing instructions and one
wider memory write.  Both rewrites operate on VM/MIR memory instructions
(`vm-aref'/`vm-aset' and `vm-slot-read'/`vm-slot-write'), require immediate
integer offsets, use alias-analysis roots when supplied, and reject unaligned
groups until the VM grows an explicit unaligned access marker.

### `opt-pass-optimization-remarks`

```lisp
(cl-cc/optimize:opt-pass-optimization-remarks (instructions))
```

Emit an analysis optimization remark when remark reporting is enabled.

This framework pass intentionally leaves INSTRUCTIONS unchanged.  The main
pipeline owns per-pass changed/missed remarks; this pass provides a stable pass
entry so users can request a final optimization-remarks stage explicitly.

### `opt-pass-loop-fusion`

```lisp
(cl-cc/optimize:opt-pass-loop-fusion (instructions))
```

FR-514: fuse adjacent canonical loops with identical iteration spaces.

Fusion is applied only when both loop bodies are side-effect-free and dependency
legality is proven by register checks plus conservative GCD/Banerjee memory tests.

### `opt-pass-loop-fission`

```lisp
(cl-cc/optimize:opt-pass-loop-fission (instructions))
```

FR-514: split large independent loop bodies into separate loops.

The pass only fissions side-effect-free canonical loops whose core can be split
into two register-independent regions and whose IV can be reset to a known
constant initializer before the second loop.  This creates vectorization-friendly
single-purpose loops without changing loops with unknown state or memory effects.

### `opt-pass-loop-tile`

```lisp
(cl-cc/optimize:opt-pass-loop-tile (instructions))
```

FR-515: cache-size adaptive loop tiling/blocking for affine nested loops.

Tiling is gated on a successful L1/L2 cache-size probe; when cache information is
unknown this pass is a no-op.  For recognized 2D/3D canonical nested loops with
affine aref/aset-style access patterns, the pass emits tile-plan labels derived
from L1/L2 tile sizes and preserves the original executable loop body.

### `*autotune-simd-enabled*`

When true, auto-select SIMD/loop-tile sizes from detected CPU cache geometry.

Default NIL preserves the existing optimizer output and the 7746/0 baseline.

### `autotune-simd-cache-info`

```lisp
(cl-cc/optimize:autotune-simd-cache-info ())
```

Return cached FR-582 cache geometry.

### `autotune-simd-tile-sizes`

```lisp
(cl-cc/optimize:autotune-simd-tile-sizes (&optional (cache-info (autotune-simd-cache-info))))
```

Return three tile sizes derived from CACHE-INFO.

Defaults and thresholds follow FR-582: L1=32KiB -> 32, L2=256KiB -> 256,
L3=8MiB -> 512.  Larger caches keep the same conservative upper bounds.

### `opt-pass-autotune-simd`

```lisp
(cl-cc/optimize:opt-pass-autotune-simd (instructions))
```

FR-582: auto-tune SIMD tile sizes from CPU cache geometry.

The pass derives L1/L2/L3 tile sizes, runs the existing loop-tiling pass under
those defaults when available, and retunes SIMD vector markers to the L1 tile.
With *AUTOTUNE-SIMD-ENABLED* NIL this is a no-op.

### `*abstract-interp-enabled*`

When non-NIL, run the FR-751 abstract interpretation pass.

### `*abstract-interp-last-state*`

Latest FR-751 abstract interpretation result for range/null/bounds consumers.

### `ai-alpha`

```lisp
(cl-cc/optimize:ai-alpha (value))
```

Galois abstraction α: concrete VALUE → product abstract value.

### `ai-gamma`

```lisp
(cl-cc/optimize:ai-gamma (abstract))
```

Galois concretization γ: return a conservative descriptor.

### `ai-compute-fixed-point`

```lisp
(cl-cc/optimize:ai-compute-fixed-point (instructions &key (max-iterations 24)))
```

Compute a forward abstract fixed point over INSTRUCTIONS with widening/narrowing.

### `opt-pass-abstract-interpretation`

```lisp
(cl-cc/optimize:opt-pass-abstract-interpretation (instructions))
```

FR-751 analysis-only pass for bounds, null pointer, and range analysis facts.

### `*translation-validation-enabled*`

When non-NIL, validate before/after IR for every optimizer pass.

### `tv-symbolic-execute-block`

```lisp
(cl-cc/optimize:tv-symbolic-execute-block (instructions))
```

Return the legacy summary, support status, and a bounded observable summary.

### `translation-validation-error`

Condition type; see its `:report` for the signaled message.

### `validate-translation`

```lisp
(cl-cc/optimize:validate-translation (before after))
```

Validate cheap O(n) IR invariants between BEFORE and AFTER.

Returns T when AFTER is a plausible translation of BEFORE.  Signals
TRANSLATION-VALIDATION-ERROR with diagnostic details when a heuristic invariant
is violated.  This is intentionally lightweight: it checks instruction-count
sanity, basic-block entry/exit integrity, and register liveness consistency, but
does not attempt formal equivalence proof.

### `validate-optimizer-translation`

```lisp
(cl-cc/optimize:validate-optimizer-translation (pass before after))
```

Warn when PASS violates lightweight IR translation invariants. Never abort compilation.

### `opt-pass-translation-validation`

```lisp
(cl-cc/optimize:opt-pass-translation-validation (instructions))
```

FR-752 registration pass. Per-pass validation is pipeline-integrated.

### `*polyhedral-enabled*`

When non-NIL, enable explicit polyhedral loop transforms.

### `polyhedral-domain`

Struct with slots: dimensions, constraints.

Constructed with `make-polyhedral-domain`; slots use the `poly-domain-` accessor prefix: `dimensions`, `constraints`.

### `make-polyhedral-domain`

Construct a `polyhedral-domain`.

### `polyhedral-domain-p`

Type predicate: true when its argument is a `polyhedral-domain`.

### `poly-domain-dimensions`

Accessor for the `dimensions` slot of `polyhedral-domain`.

### `poly-domain-constraints`

Accessor for the `constraints` slot of `polyhedral-domain`.

### `polyhedral-access`

Struct with slots: array-reg, write-p, coefficients, offset.

Constructed with `make-polyhedral-access`; slots use the `poly-access-` accessor prefix: `array-reg`, `write-p`, `coefficients`, `offset`.

### `make-polyhedral-access`

Construct a `polyhedral-access`.

### `polyhedral-access-p`

Type predicate: true when its argument is a `polyhedral-access`.

### `poly-access-array-reg`

Accessor for the `array-reg` slot of `polyhedral-access`.

### `poly-access-write-p`

Accessor for the `write-p` slot of `polyhedral-access`.

### `poly-access-coefficients`

Accessor for the `coefficients` slot of `polyhedral-access`.

### `poly-access-offset`

Accessor for the `offset` slot of `polyhedral-access`.

### `polyhedral-statement`

Struct with slots: loops, domain, accesses, schedule, body.

Constructed with `make-polyhedral-statement`; slots use the `poly-stmt-` accessor prefix: `loops`, `domain`, `accesses`, `schedule`, `body`.

### `make-polyhedral-statement`

Construct a `polyhedral-statement`.

### `polyhedral-statement-p`

Type predicate: true when its argument is a `polyhedral-statement`.

### `poly-stmt-loops`

Accessor for the `loops` slot of `polyhedral-statement`.

### `poly-stmt-domain`

Accessor for the `domain` slot of `polyhedral-statement`.

### `poly-stmt-accesses`

Accessor for the `accesses` slot of `polyhedral-statement`.

### `poly-stmt-schedule`

Accessor for the `schedule` slot of `polyhedral-statement`.

### `poly-stmt-body`

Accessor for the `body` slot of `polyhedral-statement`.

### `polyhedral-build-domain`

```lisp
(cl-cc/optimize:polyhedral-build-domain (loops constraints))
```

Build a lightweight polyhedral domain descriptor.

### `polyhedral-loop-interchange`

```lisp
(cl-cc/optimize:polyhedral-loop-interchange (statement))
```

Interchange two-deep affine loops when cache-locality improves.

STATEMENT may be either a POLYHEDRAL-STATEMENT descriptor or a VM instruction
list.  Descriptor schedules are rewritten directly.  Instruction streams are
transformed only for strict, read-only, rectangular two-deep canonical nests with
affine array reads; all other inputs are returned unchanged.

### `polyhedral-tile`

```lisp
(cl-cc/optimize:polyhedral-tile (statement &key tile-sizes))
```

Apply a basic 2D tile schedule descriptor to STATEMENT.

The current VM instruction set has no min/bound primitive suitable for safe
general strip-mining, so instruction lists are annotated with tile-plan labels by
OPT-PASS-POLYHEDRAL.  Descriptor inputs receive an explicit tiled schedule.

### `polyhedral-fuse`

```lisp
(cl-cc/optimize:polyhedral-fuse (statements))
```

Fuse compatible polyhedral statement descriptors into one descriptor.

Statements are compatible when their first two loops and domains match.  The
fused descriptor concatenates bodies and access summaries while preserving the
shared schedule.  Non-descriptor or incompatible inputs are returned unchanged.

### `opt-pass-polyhedral`

```lisp
(cl-cc/optimize:opt-pass-polyhedral (instructions))
```

FR-513 explicit polyhedral pass.

This pass is intentionally not part of the default optimizer pipeline.  When
*POLYHEDRAL-ENABLED* is NIL it is a no-op.  When enabled it performs strict
two-deep affine loop interchange and emits 2D tile-plan metadata for remaining
eligible nests.

### `opt-run-compiler-fuzz`

```lisp
(cl-cc/optimize:opt-run-compiler-fuzz (&key (trials 100) (seed 753) (max-program-length 16) (optimizer #'optimize-instructions)))
```

Run FR-753 compiler fuzzing and return a result plist.

Each trial generates a random valid straight-line IR program, interprets it,
optimizes it with OPTIMIZER, and interprets the optimized program.  Any optimizer
error or semantic mismatch is reported in the returned plist instead of signaling,
so the harness can be used from CI or the REPL without destabilizing the default
test plan.  This is an explicit tool and is not wired into the default optimizer
pipeline.

### `*mlgo-enabled*`

When non-NIL, enable MLGO-inspired inline benefit prediction.

### `*mlgo-inline-weights*`

Hardcoded linear-model weights for inline benefit prediction.

### `opt-mlgo-function-features`

```lisp
(cl-cc/optimize:opt-mlgo-function-features (def &key profile-data))
```

Return a feature vector for MLGO inline decisions.
Features are instruction count, call count, loop depth, and argument count.

### `opt-mlgo-inline-benefit`

```lisp
(cl-cc/optimize:opt-mlgo-inline-benefit (features))
```

Predict inline benefit from FEATURES using a weighted linear model.

### `opt-mlgo-inline-threshold`

```lisp
(cl-cc/optimize:opt-mlgo-inline-threshold (def &key profile-data (base-threshold 15) (max-threshold 80)))
```

Return an ML-predicted inline threshold for DEF, replacing a static cutoff.

### `opt-ml-inline-score-plan`

```lisp
(cl-cc/optimize:opt-ml-inline-score-plan (&key features model-version))
```

Return a deterministic MLGO-style inline scoring descriptor.

### `opt-pass-mlgo-inline`

```lisp
(cl-cc/optimize:opt-pass-mlgo-inline (instructions))
```

Run inlining with the MLGO benefit model when explicitly enabled.

### `opt-pass-ml-regalloc`

```lisp
(cl-cc/optimize:opt-pass-ml-regalloc (instructions))
```

FR-581: Compute register pressure hints for INSTRUCTIONS.
Performs liveness-based register pressure analysis and returns instructions
annotated for downstream register allocation. Full ML-driven register allocation
awaits training data, but this heuristic pass provides actionable pressure
estimates that significantly improve spill code generation.


## optimizer-driver.lisp — top-level entry point

### `optimize-instructions`

```lisp
(cl-cc/optimize:optimize-instructions (instructions &key (max-iterations 20) pass-pipeline print-pass-timings timing-stream print-pass-stats stats-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) speed opt-bisect-limit (inline-threshold-scale 1) trace-json-stream block-compile &allow-other-keys))
```

Run the full multi-pass optimization pipeline on a VM instruction sequence.
Iterates until convergence or MAX-ITERATIONS. Returns optimized instructions.
When *skip-optimizer-passes* is non-NIL, returns instructions unchanged.

Security-mitigation keywords (retpoline, spectre-mitigations, stack-protector,
shadow-stack, asan, msan, tsan, ubsan, hwasan) are accepted and ignored via
&allow-other-keys.

## optimizer.lisp — forward fold pass

### `opt-pass-fold`

```lisp
(cl-cc/optimize:opt-pass-fold (instructions))
```

Forward pass: constant folding, algebraic simplification, constant branch elimination.


## FR-152: transitive function purity

### `*function-instruction-table*`

Maps function names (symbols) to their compiled instruction vectors.
Filled by the compile/optimize pipeline so the purity analysis can inspect
function bodies without recompiling.

### `*user-function-purity-cache*`

Maps function names (symbols) to effect-kind keywords.
Populated lazily by COMPUTE-FUNCTION-PURITY.
Can be invalidated via INVALIDATE-FUNCTION-PURITY when a function is redefined.

### `compute-function-purity`

```lisp
(cl-cc/optimize:compute-function-purity (fn-name &key (instructions nil)))
```

Analyze FN-NAME's instruction sequence and return its effect-kind.
Propagates purity transitively: if this function only contains :pure
instructions and calls only :pure callees, it is :pure.

INSTRUCTIONS may be provided (when freshly compiled) or looked up from
*FUNCTION-INSTRUCTION-TABLE*.

Returns one of: :pure :alloc :read-only :control :write-global :io :unknown.

### `invalidate-function-purity`

```lisp
(cl-cc/optimize:invalidate-function-purity (fn-name))
```

Remove FN-NAME from the purity cache so it is recomputed on next query.
Also clears the instruction table entry.

### `register-function-instructions`

```lisp
(cl-cc/optimize:register-function-instructions (fn-name instructions))
```

Store INSTRUCTIONS for FN-NAME in the instruction table.
Should be called by the compile/optimize pipeline after generating code for a function.

### `clear-all-purity-cache`

```lisp
(cl-cc/optimize:clear-all-purity-cache ())
```

Reset all purity caches. Used between compilation units or for testing.

### `call-site-effect-kind`

```lisp
(cl-cc/optimize:call-site-effect-kind (inst))
```

Return the effect-kind of a vm-call/vm-tail-call INST by analyzing the callee.
If the callee is a builtin with known properties, returns that kind.
If the callee is a user function, returns its computed purity.
Otherwise returns :unknown.

### `user-function-pure-p`

```lisp
(cl-cc/optimize:user-function-pure-p (fn-name))
```

Return T when FN-NAME is a pure function (may be used for CSE/DCE).

### `user-function-cse-eligible-p`

```lisp
(cl-cc/optimize:user-function-cse-eligible-p (fn-name))
```

Return T when FN-NAME is safe for common subexpression elimination.

### `user-function-dce-eligible-p`

```lisp
(cl-cc/optimize:user-function-dce-eligible-p (fn-name))
```

Return T when FN-NAME's result may be dropped without side effects.


## FR-276: optimization levels

### `*optimization-level-params*`

FR-276: Pre-configured optimizer parameters for each -O level.

### `opt-level-params`

```lisp
(cl-cc/optimize:opt-level-params (level))
```

Return the parameter plist for optimization LEVEL (0-3).
LEVEL is clamped to 0..3.

### `apply-optimization-level`

```lisp
(cl-cc/optimize:apply-optimization-level (level))
```

Configure the global optimizer state for optimization LEVEL (0-3).
Returns the parameter plist that was applied.

### `compiler-self-profiling-capabilities`

```lisp
(cl-cc/optimize:compiler-self-profiling-capabilities ())
```

Return FR-703 Compiler Self-Profiling / Build Analytics capabilities.

### `build-analytics-summary`

```lisp
(cl-cc/optimize:build-analytics-summary (&key pass-count instruction-count elapsed-us changed-count))
```

Build a compact FR-703 build analytics summary plist.

### `*optimization-report-stream*`

When non-NIL, optimizer passes emit one-line optimization reports here.

### `*block-compile*`

When non-NIL, optimize a source file as one block-compilation unit.
This permits module-local function bodies with lexical captures to be inlined at
known direct call sites, while recursive callees remain protected by the normal
call-graph guard.

### `*max-inline-size*`

Maximum raw instruction count for automatic call-graph based inlining.
The count excludes the final vm-ret.  This cap is deliberately independent of
the adaptive cost threshold so LTO inlining stays bounded across modules.

### `*skip-optimizer-passes*`

When non-NIL, optimize-instructions returns its input unchanged.

### `*verify-optimizer-instructions*`

When non-NIL, run opt-verify-instructions after every convergence pass to
catch ill-formed sequences (duplicate labels, unknown jump targets, use-before-define).

### `*verify-ir*`

When non-NIL, verify IR/VM invariants before and after each optimizer pass.

Unlike `*verify-optimizer-instructions*`, which only understands flat VM
instruction streams, `opt-verify-ir` also accepts `cl-cc/ir` structured IR and
`cl-cc/mir` functions/modules (resolved dynamically, so this package never
names another package's internals). It is wired in as the pipeline's final
stage (`:verify-ir` in `*opt-pass-table*`), as one last invariant check on the
fully-optimized instruction stream.

### `opt-verify-ir`

```lisp
(cl-cc/optimize:opt-verify-ir (ir &key pass-name))
```

Verify optimizer IR invariants. Currently supports flat VM instruction streams;
the function is intentionally non-transforming so it can be registered as a pass.

### `*opt-bisect-limit*`

Maximum number of optimization pass invocations allowed to change the instruction stream.
NIL disables optimization bisection.

### `*opt-bisect-count*`

Number of optimization pass invocations that changed the instruction stream in the current dynamic scope.

### `*opt-enable-pure-call-optimization*`

When NIL, disable pure-call optimization regardless of selected pass pipeline.

This hook is used as an optimization-policy gate so frontends can couple the
pass to `(optimize (speed 3))`-style policy decisions without changing the
optimizer's pass table wiring.

### `*opt-enable-sealed-gf-devirtualization*`

When NIL, keep sealed generic calls as dynamic `vm-generic-call` instructions.

This optimization is policy-gated because it trades compilation effort and a
closed-world proof for direct method invocation.  `opt-configure-optimization-policy`
enables it for SPEED >= 2.

### `opt-pass-schedule-local`

```lisp
(cl-cc/optimize:opt-pass-schedule-local (instructions))
```

FR-069: Dependency-aware list scheduling within each basic block.

Builds a local DAG with RAW/WAR/WAW register dependencies, computes critical-path
priorities from estimated VM latencies, and emits highest-priority ready nodes.
Scheduling is limited to side-effect-free runs inside a basic block; calls,
stores, signals, and control-flow instructions are barriers.

### `opt-configure-optimization-policy`

```lisp
(cl-cc/optimize:opt-configure-optimization-policy (&key speed))
```

Configure optimizer feature gates from a coarse optimization SPEED level.

Current policy:
- SPEED >= 2: enable sealed+satiated generic-function devirtualization
- SPEED >= 3: enable pure-call optimization gate
- SPEED <= 2: disable pure-call optimization gate

Returns the resulting gate value for convenience.

### `optimize-roadmap-doc-features`

```lisp
(cl-cc/optimize:optimize-roadmap-doc-features (&optional (pathname (%opt-roadmap-doc-pathname))))
```

Parse docs/notes/optimize-passes.md and return all FR features in document order.

### `optimize-roadmap-doc-fr-ids`

```lisp
(cl-cc/optimize:optimize-roadmap-doc-fr-ids (&optional (pathname (%opt-roadmap-doc-pathname))))
```

Return all optimize roadmap FR ids in document order.

### `optimize-roadmap-register-doc-evidence`

```lisp
(cl-cc/optimize:optimize-roadmap-register-doc-evidence (&optional (pathname (%opt-roadmap-doc-pathname))))
```

Populate `*opt-roadmap-evidence-registry*` from docs/notes/optimize-passes.md.

### `lookup-opt-roadmap-evidence`

```lisp
(cl-cc/optimize:lookup-opt-roadmap-evidence (feature-id))
```

Return implementation evidence for FEATURE-ID, populating the registry lazily.

### `optimize-backend-roadmap-doc-features`

```lisp
(cl-cc/optimize:optimize-backend-roadmap-doc-features (&optional (pathname (%opt-backend-roadmap-doc-pathname))))
```

Parse docs/notes/optimize-backend.md and return all FR features in document order.

### `optimize-backend-roadmap-doc-fr-ids`

```lisp
(cl-cc/optimize:optimize-backend-roadmap-doc-fr-ids (&optional (pathname (%opt-backend-roadmap-doc-pathname))))
```

Return all optimize-backend roadmap FR ids in document order.

### `optimize-backend-roadmap-status-summary`

```lisp
(cl-cc/optimize:optimize-backend-roadmap-status-summary (&optional (pathname (%opt-backend-roadmap-doc-pathname))))
```

Return status counts for docs/notes/optimize-backend.md FR headings.

Returned plist keys:
:total        total FR heading count
:implemented  count of ✅ headings
:partial      count of 🔶 headings
:planned      count of explicit ❌ headings
:unknown      count of unmarked headings

### `optimize-backend-roadmap-all-fr-complete-p`

```lisp
(cl-cc/optimize:optimize-backend-roadmap-all-fr-complete-p (&optional (pathname (%opt-backend-roadmap-doc-pathname))))
```

Return T only when every optimize-backend FR is marked ✅ and has complete evidence.

### `optimize-backend-roadmap-fr-ids-by-status`

```lisp
(cl-cc/optimize:optimize-backend-roadmap-fr-ids-by-status (status &optional (pathname (%opt-backend-roadmap-doc-pathname))))
```

Return optimize-backend FR IDs filtered by STATUS.

Accepted STATUS keywords: :implemented, :partial, :planned, :unknown.

### `optimize-backend-roadmap-register-doc-evidence`

```lisp
(cl-cc/optimize:optimize-backend-roadmap-register-doc-evidence (&optional (pathname (%opt-backend-roadmap-doc-pathname))))
```

Populate `*opt-backend-roadmap-evidence-registry*` from docs/notes/optimize-backend.md.

### `lookup-opt-backend-roadmap-evidence`

```lisp
(cl-cc/optimize:lookup-opt-backend-roadmap-evidence (feature-id))
```

Return optimize-backend implementation evidence for FEATURE-ID.

### `optimize-roadmap-evidence-well-formed-p`

```lisp
(cl-cc/optimize:optimize-roadmap-evidence-well-formed-p (evidence))
```

Return T when EVIDENCE is a checkable roadmap implementation record.

### `optimize-roadmap-implementation-evidence-complete-p`

```lisp
(cl-cc/optimize:optimize-roadmap-implementation-evidence-complete-p (evidence))
```

Return T when EVIDENCE references concrete modules, APIs, and tests.

### `optimize-backend-roadmap-implementation-evidence-complete-p`

```lisp
(cl-cc/optimize:optimize-backend-roadmap-implementation-evidence-complete-p (evidence))
```

Return T only when optimize-backend EVIDENCE is marked implemented and its anchors resolve.

### `opt-roadmap-evidence-feature-id`

Accessor for the `feature-id` slot of `opt-roadmap-evidence`.

### `opt-roadmap-evidence-status`

Accessor for the `status` slot of `opt-roadmap-evidence`.

### `opt-roadmap-evidence-modules`

Accessor for the `modules` slot of `opt-roadmap-evidence`.

### `opt-roadmap-evidence-api-symbols`

Accessor for the `api-symbols` slot of `opt-roadmap-evidence`.

### `opt-roadmap-evidence-test-anchors`

Accessor for the `test-anchors` slot of `opt-roadmap-evidence`.

### `opt-roadmap-evidence-summary`

Accessor for the `summary` slot of `opt-roadmap-evidence`.

### `make-opt-ic-site`

Construct a `opt-ic-site` -- Polymorphic inline-cache state for one call site.

### `opt-ic-site-state`

Accessor for the `state` slot of `opt-ic-site`.

### `opt-ic-site-misses`

Accessor for the `misses` slot of `opt-ic-site`.

### `opt-ic-site-megamorphic-fallback`

Accessor for the `megamorphic-fallback` slot of `opt-ic-site`.

### `opt-ic-transition`

```lisp
(cl-cc/optimize:opt-ic-transition (site receiver-key target))
```

Record RECEIVER-KEY → TARGET in SITE and return SITE.
State transitions follow uninitialized → monomorphic → polymorphic → megamorphic.

### `make-opt-megamorphic-cache`

Construct a `opt-megamorphic-cache` -- Shared megamorphic dispatch cache used after IC state reaches :megamorphic.

### `opt-mega-cache-put`

```lisp
(cl-cc/optimize:opt-mega-cache-put (cache receiver-key target))
```

Insert RECEIVER-KEY -> TARGET into CACHE with simple LRU eviction.

### `opt-mega-cache-get`

```lisp
(cl-cc/optimize:opt-mega-cache-get (cache receiver-key))
```

Lookup RECEIVER-KEY in CACHE. Returns (values target found-p).

### `opt-ic-resolve-target`

```lisp
(cl-cc/optimize:opt-ic-resolve-target (site receiver-key &optional megamorphic-cache))
```

Resolve dispatch target for RECEIVER-KEY from SITE and optional shared cache.

Lookup order:
1) site-local IC entries
2) shared megamorphic cache (only when SITE is :megamorphic)

Returns (values target source-keyword), where source is one of
:site-local, :megamorphic-shared, or :miss.

### `make-opt-ic-patch-plan`

Construct a `opt-ic-patch-plan` -- Plan for patching one IC call site at runtime.

This helper is backend-agnostic and only models *what* to patch, not machine
encoding details.

### `opt-ic-make-patch-plan`

```lisp
(cl-cc/optimize:opt-ic-make-patch-plan (site-id old-state new-state target))
```

Build a conservative IC patch plan from OLD-STATE to NEW-STATE.

PATCH-KIND is one of:
:install-monomorphic
:promote-polymorphic
:promote-megamorphic
:no-op

### `opt-build-inline-polymorphic-dispatch`

```lisp
(cl-cc/optimize:opt-build-inline-polymorphic-dispatch (entries receiver-reg))
```

Build a simple PIC-style dispatch chain from ENTRIES.

ENTRIES is an alist of (shape-key . target). Returns a list of plists with
fields :shape, :receiver, :target representing sequential guards.

### `*opt-speculative-inline-dominance-threshold*`

Minimum IC type-frequency dominance required for speculative inlining.

### `opt-ic-dominant-type`

```lisp
(cl-cc/optimize:opt-ic-dominant-type (counter-table &key (threshold *opt-speculative-inline-dominance-threshold*)))
```

Return (values TYPE COUNT TOTAL RATIO) when one IC type dominates.

Flat distributions deliberately return NIL so the optimizer does not speculate
when multiple receiver types have similar frequency.

### `opt-speculative-inline-eligible-p`

```lisp
(cl-cc/optimize:opt-speculative-inline-eligible-p (inst))
```

Return dominant IC key for INST when FR-523 speculation is profitable.

### `opt-annotate-speculative-inline`

```lisp
(cl-cc/optimize:opt-annotate-speculative-inline (inst key pc))
```

Annotate INST with a guarded inline-cache fast path.

The VM's IC fast path already emits the runtime type guard by comparing the
current specializer key to VM-PGO-SPECIALIZER and directly calling the cached
method on success; guard failure falls back to the normal IC slow path, which is
also a safe FR-522 deopt/fallback boundary.

### `opt-pass-speculative-inline`

```lisp
(cl-cc/optimize:opt-pass-speculative-inline (instructions))
```

FR-523: use IC type frequencies to install guarded speculative inlines.

Only monomorphic-dominant sites (>90%) are annotated.  Sites with flat type
distributions are left untouched.

### `make-opt-speculation-log`

Construct a `opt-speculation-log` -- Failure log preventing repeated harmful speculative optimizations.

### `*opt-speculation-log*`

Process-global speculation failure log used by conservative roadmap helpers.

### `opt-record-speculation-failure`

```lisp
(cl-cc/optimize:opt-record-speculation-failure (log site-id reason))
```

Record a failed speculation for SITE-ID and REASON.

### `opt-speculation-failed-p`

```lisp
(cl-cc/optimize:opt-speculation-failed-p (log site-id reason))
```

Return T when SITE-ID/REASON has crossed LOG's failure threshold.

### `opt-speculation-allowed-p`

```lisp
(cl-cc/optimize:opt-speculation-allowed-p (site-id reason &optional (log *opt-speculation-log*)))
```

Return T when SITE-ID/REASON is still allowed under LOG.

### `opt-clear-speculation-log`

```lisp
(cl-cc/optimize:opt-clear-speculation-log (&optional (log *opt-speculation-log*)))
```

Clear LOG's recorded failures and return LOG.

### `opt-save-speculation-log`

```lisp
(cl-cc/optimize:opt-save-speculation-log (pathname &optional (log *opt-speculation-log*)))
```

Persist LOG to PATHNAME as a simple S-expression and return PATHNAME.

### `opt-load-speculation-log`

```lisp
(cl-cc/optimize:opt-load-speculation-log (pathname &optional (log *opt-speculation-log*)))
```

Load a persisted speculation log from PATHNAME into LOG and return LOG.

### `make-opt-profile-data`

Construct a `opt-profile-data` -- Small profile container for edge, value, call-chain, and allocation data.

### `opt-profile-record-edge`

```lisp
(cl-cc/optimize:opt-profile-record-edge (profile from to &optional (delta 1)))
```

Increment the execution count for CFG edge FROM → TO.

### `opt-profile-record-value`

```lisp
(cl-cc/optimize:opt-profile-record-value (profile site-id value &optional (delta 1)))
```

Increment a top-k style value counter for SITE-ID.

### `opt-profile-top-values`

```lisp
(cl-cc/optimize:opt-profile-top-values (profile site-id &optional limit))
```

Return SITE-ID's retained top values as sorted (VALUE . COUNT) pairs.

### `opt-profile-value-range`

```lisp
(cl-cc/optimize:opt-profile-value-range (profile site-id))
```

Return SITE-ID's numeric value range as (MIN . MAX), or NIL when absent.

### `opt-profile-record-call-chain`

```lisp
(cl-cc/optimize:opt-profile-record-call-chain (profile chain &optional (delta 1)))
```

Increment a context-sensitive call-chain sample.

### `opt-profile-record-allocation`

```lisp
(cl-cc/optimize:opt-profile-record-allocation (profile site-id bytes &optional (count 1)))
```

Record allocation COUNT and BYTES for SITE-ID.

### `opt-pgo-best-successor`

```lisp
(cl-cc/optimize:opt-pgo-best-successor (block successors edge-counts &optional visited))
```

Return BLOCK's hottest unvisited successor according to EDGE-COUNTS.

### `opt-pgo-build-hot-chain`

```lisp
(cl-cc/optimize:opt-pgo-build-hot-chain (entry successors-alist edge-counts))
```

Build a greedy Pettis-Hansen-style hot block chain from ENTRY.

### `opt-pgo-rotate-loop`

```lisp
(cl-cc/optimize:opt-pgo-rotate-loop (loop-chain preferred-exit))
```

Rotate LOOP-CHAIN so PREFERRED-EXIT becomes the loop bottom.

### `opt-pgo-build-counter-plan`

```lisp
(cl-cc/optimize:opt-pgo-build-counter-plan (entry successors-alist))
```

Build an explicit BB/edge counter plan from CFG successor relations.

Returns plist:
:bb-counters   ((BLOCK . BB-ID) ...)
:edge-counters ((((FROM . TO) . EDGE-ID) ...)
:total-bb      integer
:total-edge    integer

### `opt-pgo-make-profile-template`

```lisp
(cl-cc/optimize:opt-pgo-make-profile-template (counter-plan))
```

Build a zero-initialized profile payload from COUNTER-PLAN.

### `opt-lattice-bottom`

```lisp
(cl-cc/optimize:opt-lattice-bottom ())
```

Return the unknown-bottom lattice element.

### `opt-lattice-constant`

```lisp
(cl-cc/optimize:opt-lattice-constant (value))
```

Return a constant lattice element for VALUE.

### `opt-lattice-overdefined`

```lisp
(cl-cc/optimize:opt-lattice-overdefined ())
```

Return the overdefined lattice element.

### `opt-lattice-meet`

```lisp
(cl-cc/optimize:opt-lattice-meet (left right))
```

Conservatively meet LEFT and RIGHT in the SCCP lattice.

### `opt-lattice-value-kind`

Accessor for the `kind` slot of `opt-lattice-value`.

### `opt-lattice-value-value`

Accessor for the `value` slot of `opt-lattice-value`.

### `make-opt-function-summary`

Construct a `opt-function-summary` -- Small interprocedural summary used by conservative IPO helpers.

### `opt-function-summary-safe-to-inline-p`

```lisp
(cl-cc/optimize:opt-function-summary-safe-to-inline-p (summary &key (max-effects 0)))
```

Return T when SUMMARY is pure enough for conservative inlining/IPSCCP use.

### `opt-thinlto-should-import-p`

```lisp
(cl-cc/optimize:opt-thinlto-should-import-p (summary caller-callees &key (budget 50)))
```

Return T when SUMMARY is a conservative ThinLTO import candidate.

### `make-opt-slab-pool`

Construct a `opt-slab-pool` -- Fixed-size object pool for cons/slab allocation modelling.

### `opt-slab-allocate`

```lisp
(cl-cc/optimize:opt-slab-allocate (pool))
```

Allocate one fixed-size object id from POOL.

### `opt-slab-free`

```lisp
(cl-cc/optimize:opt-slab-free (pool object))
```

Return OBJECT to POOL's freelist.

### `make-opt-bump-region`

Construct a `opt-bump-region` -- Bump-pointer allocation region used by allocation planning helpers.

### `opt-bump-region-cursor`

Accessor for the `cursor` slot of `opt-bump-region`.

### `opt-bump-allocate`

```lisp
(cl-cc/optimize:opt-bump-allocate (region words &key (alignment 1)))
```

Allocate WORDS from REGION and return the start cursor, or NIL on overflow.

### `opt-bump-mark`

```lisp
(cl-cc/optimize:opt-bump-mark (region))
```

Record REGION's current cursor and return it.

### `opt-bump-reset`

```lisp
(cl-cc/optimize:opt-bump-reset (region &optional mark))
```

Reset REGION to MARK, or to the most recent saved mark.

### `make-opt-stack-map`

Construct a `opt-stack-map` -- Safepoint stack-map metadata: VM PC and live roots.

### `opt-stack-map-live-root-p`

```lisp
(cl-cc/optimize:opt-stack-map-live-root-p (stack-map root))
```

Return T when ROOT is live at STACK-MAP's safepoint.

### `make-opt-guard-state`

Construct a `opt-guard-state` -- Speculative guard state for guard weakening and deopt accounting.

### `opt-guard-record`

```lisp
(cl-cc/optimize:opt-guard-record (guard success-p))
```

Record one GUARD execution and return its current strength.

### `opt-weaken-guard`

```lisp
(cl-cc/optimize:opt-weaken-guard (guard &key (execution-threshold 10)))
```

Weaken GUARD when it has enough successful executions and no failures.

### `make-opt-jit-cache-entry`

Construct a `opt-jit-cache-entry` -- One JIT code-cache entry for conservative eviction decisions.

### `opt-jit-cache-select-eviction`

```lisp
(cl-cc/optimize:opt-jit-cache-select-eviction (entries &key (pressure-threshold 0.8) current-size max-size))
```

Select the coldest active entry when cache pressure exceeds PRESSURE-THRESHOLD.

### `make-opt-module-summary`

Construct a `opt-module-summary` -- ThinLTO-style module summary for parallel whole-program planning.

### `opt-merge-module-summaries`

```lisp
(cl-cc/optimize:opt-merge-module-summaries (summaries))
```

Merge SUMMARIES into a small global summary plist.

### `make-opt-sea-node`

Construct a `opt-sea-node` -- Schedule-free Sea-of-Nodes placeholder used by MIR/SSA bridge planning.

### `opt-sea-node-schedulable-p`

```lisp
(cl-cc/optimize:opt-sea-node-schedulable-p (node))
```

Return T when NODE has an operator and explicit control dependencies.

### `make-opt-deopt-frame`

Construct a `opt-deopt-frame` -- VM materialization metadata for a deoptimization point.

### `opt-materialize-deopt-state`

```lisp
(cl-cc/optimize:opt-materialize-deopt-state (frame machine-registers))
```

Return a VM register alist reconstructed from MACHINE-REGISTERS using FRAME.

### `make-opt-osr-point`

Construct a `opt-osr-point` -- On-Stack Replacement metadata at loop back-edge safe points.

### `opt-osr-trigger-p`

```lisp
(cl-cc/optimize:opt-osr-trigger-p (osr-point &key (threshold 1000)))
```

Return T when OSR-POINT hotness reaches THRESHOLD.

### `opt-osr-materialize-entry`

```lisp
(cl-cc/optimize:opt-osr-materialize-entry (osr-point machine-registers))
```

Materialize live VM register state for OSR entry from MACHINE-REGISTERS.

Each entry in OPT-OSR-POINT-LIVE-REGISTERS is (machine-reg . vm-reg).

### `make-opt-shape-descriptor-for-slots`

```lisp
(cl-cc/optimize:make-opt-shape-descriptor-for-slots (shape-id slots))
```

Create a shape descriptor whose slot offsets follow SLOTS order.

### `opt-shape-slot-offset`

```lisp
(cl-cc/optimize:opt-shape-slot-offset (shape slot))
```

Return SLOT's fixed offset in SHAPE, or NIL when absent.

### `make-opt-shape-transition-cache`

Construct a `opt-shape-transition-cache` -- Forward-only shape transition cache (parent-shape, slot) -> child-shape.

### `opt-shape-transition-put`

```lisp
(cl-cc/optimize:opt-shape-transition-put (cache parent-shape-id slot child-shape-id))
```

Register one forward shape transition and return CHILD-SHAPE-ID.

### `opt-shape-transition-get`

```lisp
(cl-cc/optimize:opt-shape-transition-get (cache parent-shape-id slot))
```

Lookup forward shape transition. Returns (values child-shape-id found-p).

### `opt-adaptive-compilation-threshold`

```lisp
(cl-cc/optimize:opt-adaptive-compilation-threshold (&key (base 1000) warmup-p cache-pressure failures))
```

Return an adaptive tiering/recompilation threshold.
Warmup lowers the threshold, cache pressure raises it, and failures suppress
speculative recompilation.

### `opt-tier-transition`

```lisp
(cl-cc/optimize:opt-tier-transition (current-tier hotness &key (baseline-threshold 100) (optimized-threshold 1000)))
```

Return the next tier for CURRENT-TIER and HOTNESS, or CURRENT-TIER.

**Example**:

```lisp
(cl-cc/optimize:opt-tier-transition :interpreter 150)
;; => :baseline
```

### `opt-tier-runtime-state`

Per-call-site tiering state: which tier (`:interpreter`/`:baseline`/`:optimized`)
a call site currently runs at, its call and back-edge counts, and whether a
one-shot PGO handoff to the optimized tier is pending. Constructed with
`make-opt-tier-runtime-state`; slots use the `opt-tier-runtime-state-` accessor
prefix: `tier`, `call-count`, `backedge-count`, `pgo-handoff-p`.

### `make-opt-tier-runtime-state`

```lisp
(cl-cc/optimize:make-opt-tier-runtime-state (&key (tier :interpreter) (call-count 0) (backedge-count 0) (pgo-handoff-p nil)))
```

Construct a tier runtime-state record: interpreter/baseline/optimized tier, call count, back-edge count, and whether a PGO handoff is pending.

### `opt-tier-runtime-state-tier`

Accessor for the `tier` slot of `opt-tier-runtime-state`.

### `opt-tier-runtime-state-call-count`

Accessor for the `call-count` slot of `opt-tier-runtime-state`.

### `opt-tier-runtime-state-backedge-count`

Accessor for the `backedge-count` slot of `opt-tier-runtime-state`.

### `opt-tier-runtime-state-pgo-handoff-p`

Accessor for the `pgo-handoff-p` slot of `opt-tier-runtime-state`.

### `opt-tier-record-runtime-event`

```lisp
(cl-cc/optimize:opt-tier-record-runtime-event (state event &key (count 1) (baseline-threshold 100) (optimized-threshold 1000)))
```

Record a call or backedge and return STATE plus a one-shot PGO handoff payload.

**Returns**: `(values state handoff-plist-or-nil)`. `handoff-plist-or-nil` is
non-nil exactly once, the event that first crosses into the `:optimized` tier.

### `opt-make-pure-function-runtime-memoizer`

```lisp
(cl-cc/optimize:opt-make-pure-function-runtime-memoizer (function &key max-size))
```

Return a runtime memoizing wrapper for explicitly pure FUNCTION. All returned
values, including NIL and zero values, are preserved.

### `make-opt-async-state-machine`

Construct a `opt-async-state-machine` -- Minimal async/await lowering plan as an explicit state machine graph.

### `opt-build-async-state-machine`

```lisp
(cl-cc/optimize:opt-build-async-state-machine (await-labels))
```

Build a linear async state machine skeleton from AWAIT-LABELS.

Each await label creates one state and a transition to the next state.
Returns an OPT-ASYNC-STATE-MACHINE descriptor for planning/testing.

### `opt-choose-coroutine-lowering-strategy`

```lisp
(cl-cc/optimize:opt-choose-coroutine-lowering-strategy (&key supports-call/cc deep-yield-p))
```

Choose coroutine lowering strategy.

Returns :stackful when deep yield or call/cc compatibility is required,
otherwise returns :stackless for state-machine lowering.

### `make-opt-channel-site`

Construct a `opt-channel-site` -- Channel/CSP optimization metadata for one send/recv site.

### `opt-channel-select-path`

```lisp
(cl-cc/optimize:opt-channel-select-path (site))
```

Select channel fast/sync path based on SITE characteristics.

Returns one of:
:fast-buffered      buffered channel with available queue slots/items
:synchronous-rendezvous  unbuffered channel hand-off path
:contended-fallback heavy contention -> conservative fallback

### `opt-channel-should-jump-table-select-p`

```lisp
(cl-cc/optimize:opt-channel-should-jump-table-select-p (site &key (threshold 4)))
```

Return T when select-arity is large enough to prefer jump-table lowering.

### `make-opt-stm-plan`

Construct a `opt-stm-plan` -- STM lowering plan for one `(atomically ...)` region.

### `opt-stm-build-plan`

```lisp
(cl-cc/optimize:opt-stm-build-plan (&key reads writes pure-p))
```

Build a conservative STM plan from read/write sets and purity.

When PURE-P is true, transaction logging can be skipped.

### `opt-stm-needs-log-p`

```lisp
(cl-cc/optimize:opt-stm-needs-log-p (plan))
```

Return T when PLAN requires transactional logging.

### `make-opt-lockfree-plan`

Construct a `opt-lockfree-plan` -- Lock-free lowering support plan for CAS-based data structures.

### `opt-lockfree-select-reclamation`

```lisp
(cl-cc/optimize:opt-lockfree-select-reclamation (&key aba-risk-p contention))
```

Choose memory reclamation strategy for lock-free lowering.

Rules:
- no ABA risk -> :none
- high contention (>= 4) -> :epoch
- otherwise -> :hazard-pointer

### `opt-lockfree-build-plan`

```lisp
(cl-cc/optimize:opt-lockfree-build-plan (&key operation aba-risk-p contention))
```

Build a lock-free support plan with reclamation strategy.

### `make-opt-cfi-plan`

Construct a `opt-cfi-plan` -- Control-Flow Integrity planning record.

### `opt-build-cfi-plan`

```lisp
(cl-cc/optimize:opt-build-cfi-plan (&key target has-indirect-calls-p))
```

Build a conservative CFI insertion plan for TARGET (:x86-64 or :aarch64).

### `opt-cfi-entry-opcode`

```lisp
(cl-cc/optimize:opt-cfi-entry-opcode (plan))
```

Return the function-entry CFI marker opcode selected by PLAN.

### `opt-should-use-retpoline-p`

```lisp
(cl-cc/optimize:opt-should-use-retpoline-p (&key target has-indirect-branch-p supports-ibrs-p))
```

Return T when retpoline mitigation should be enabled.

Retpoline is x86-64 specific and unnecessary when IBRS/eIBRS is available.

### `opt-retpoline-thunk-name`

```lisp
(cl-cc/optimize:opt-retpoline-thunk-name (target-reg))
```

Return the module-local retpoline thunk name for TARGET-REG.

### `opt-needs-stack-canary-p`

```lisp
(cl-cc/optimize:opt-needs-stack-canary-p (&key has-stack-buffer-p))
```

Return T when stack protector instrumentation should be inserted.

### `opt-stack-canary-emit-plan`

```lisp
(cl-cc/optimize:opt-stack-canary-emit-plan (&key has-stack-buffer-p guard-slot failure-target))
```

Return a backend-neutral stack canary prologue/epilogue emission plan.

### `opt-stack-canary-prologue-seq`

```lisp
(cl-cc/optimize:opt-stack-canary-prologue-seq (plan &key (temp-reg :stack-canary-temp)))
```

Return abstract prologue operations for PLAN's stack canary setup.

### `opt-stack-canary-epilogue-seq`

```lisp
(cl-cc/optimize:opt-stack-canary-epilogue-seq (plan &key (temp-reg :stack-canary-temp)))
```

Return abstract epilogue operations for PLAN's stack canary verification.

### `make-opt-shadow-stack-plan`

Construct a `opt-shadow-stack-plan` -- Shadow Stack (CET SS) planning record for return-address verification.

### `opt-shadow-stack-plan-enabled-p`

Accessor for the `enabled-p` slot of `opt-shadow-stack-plan`.

### `opt-shadow-stack-plan-target`

Accessor for the `target` slot of `opt-shadow-stack-plan`.

### `opt-shadow-stack-plan-needs-incsssp-p`

Accessor for the `needs-incsssp-p` slot of `opt-shadow-stack-plan`.

### `opt-shadow-stack-plan-needs-save-restore-p`

Accessor for the `needs-save-restore-p` slot of `opt-shadow-stack-plan`.

### `opt-build-shadow-stack-plan`

```lisp
(cl-cc/optimize:opt-build-shadow-stack-plan (&key target supports-cet-ss-p has-nonlocal-control-p has-setjmp-longjmp-p))
```

Build a conservative Shadow Stack plan for TARGET.

CET Shadow Stack is x86-64 specific. Non-local control transfers require
explicit save/restore planning before backend instruction emission is safe.

### `make-opt-wasm-tailcall-plan`

Construct a `opt-wasm-tailcall-plan` -- Wasm tail-call lowering decision for one call site.

### `opt-wasm-select-tailcall-opcode`

```lisp
(cl-cc/optimize:opt-wasm-select-tailcall-opcode (&key tail-position-p indirect-p enabled-p))
```

Select wasm call opcode with tail-call proposal support.

Tail-position calls require ENABLED-P; the backend does not silently lower them
to non-tail calls.

Returns one of :call, :call-indirect, :return-call, :return-call-indirect.

### `opt-wasm-select-direct-tailcall-opcode`

```lisp
(cl-cc/optimize:opt-wasm-select-direct-tailcall-opcode (&key tail-position-p enabled-p))
```

Select opcode for direct wasm calls.

Tail-position calls require ENABLED-P.

### `opt-build-wasm-tailcall-plan`

```lisp
(cl-cc/optimize:opt-build-wasm-tailcall-plan (&key tail-position-p indirect-p (enabled-p t)))
```

Build tail-call lowering plan and chosen opcode for one wasm call site.

### `make-opt-wasm-gc-layout`

Construct a `opt-wasm-gc-layout` -- Wasm GC layout descriptor for struct/array-backed CL objects.

### `opt-build-wasm-gc-layout`

```lisp
(cl-cc/optimize:opt-build-wasm-gc-layout (&key kind fields nullable-p))
```

Build a wasm-gc layout descriptor for planning/testing purposes.

### `opt-wasm-gc-layout-valid-p`

```lisp
(cl-cc/optimize:opt-wasm-gc-layout-valid-p (layout))
```

Return T when LAYOUT is a structurally valid wasm-gc descriptor.

Accepted kinds are :STRUCT and :ARRAY.
For :STRUCT, fields must be a list of (name . type) pairs.
For :ARRAY, fields must contain exactly one element type descriptor.

### `opt-wasm-gc-runtime-host-compatible-p`

```lisp
(cl-cc/optimize:opt-wasm-gc-runtime-host-compatible-p (layout &key host-supports-wasm-gc-p))
```

Return T if LAYOUT can be safely emitted for current host/runtime settings.

### `opt-build-wasm-gc-optimization-plan`

```lisp
(cl-cc/optimize:opt-build-wasm-gc-optimization-plan (layout))
```

Build optimization hints for wasm-gc lowering from LAYOUT.

Returns plist:
:layout-valid-p           -- structural validity
:inline-field-access-p    -- enable direct struct.get/set lowering
:bounds-check-elision-p   -- enable array bounds-check elision candidates

### `make-opt-debug-loc`

Construct a `opt-debug-loc` -- Source-level location record for debug info planning.

### `opt-build-dwarf-line-row`

```lisp
(cl-cc/optimize:opt-build-dwarf-line-row (address debug-loc))
```

Build a minimal DWARF-like line row plist from ADDRESS and DEBUG-LOC.

### `opt-build-wasm-source-map-entry`

```lisp
(cl-cc/optimize:opt-build-wasm-source-map-entry (offset debug-loc))
```

Build a source-map entry plist for wasm OFFSET and DEBUG-LOC.

### `opt-build-wasm-source-map-v3`

```lisp
(cl-cc/optimize:opt-build-wasm-source-map-v3 (entries &key file))
```

Build a backend-neutral Source Map v3 payload plist from ENTRIES.

This helper normalizes entry ordering and source lists but does not write files
or emit VLQ-encoded mappings.

### `opt-format-diagnostic-reason`

```lisp
(cl-cc/optimize:opt-format-diagnostic-reason (pass outcome reason))
```

Format optimization diagnostic reason in Rpass-like style.

### `opt-build-diagnostic-caret-line`

```lisp
(cl-cc/optimize:opt-build-diagnostic-caret-line (line-text column &key (caret #\^)))
```

Return a two-line caret diagnostic snippet for LINE-TEXT at 1-based COLUMN.

### `opt-diagnostic-did-you-mean`

```lisp
(cl-cc/optimize:opt-diagnostic-did-you-mean (unknown candidates &key (limit 3)))
```

Return up to LIMIT ranked suggestion strings for UNKNOWN from CANDIDATES.

### `opt-format-type-trace`

```lisp
(cl-cc/optimize:opt-format-type-trace (steps))
```

Format type-inference rationale STEPS as a human-readable trace string.

### `make-opt-tls-plan`

Construct a `opt-tls-plan` -- Thread-local access lowering plan.

### `opt-tls-plan-target`

Accessor for the `target` slot of `opt-tls-plan`.

### `opt-tls-plan-base-register`

Accessor for the `base-register` slot of `opt-tls-plan`.

### `opt-tls-plan-model`

Exported, but `opt-tls-plan` (`optimizer-speculative-atomics-data.lisp`) has
no `model` slot -- only `target`, `uses-inline-tls-p`, and `base-register`.
Calling this accessor signals `undefined-function`. Tracked as a known gap.

### `opt-tls-plan-notes`

Exported, but `opt-tls-plan` has no `notes` slot either. Same gap as
[`opt-tls-plan-model`](#opt-tls-plan-model).

### `opt-build-tls-plan`

```lisp
(cl-cc/optimize:opt-build-tls-plan (&key target hot-access-p))
```

Build TLS access plan for TARGET architecture.

When HOT-ACCESS-P is true, choose inline segment/thread-pointer based access.

### `make-opt-atomic-plan`

Construct a `opt-atomic-plan` -- Atomic lowering plan for architecture + memory ordering.

### `opt-atomic-plan-target`

Accessor for the `target` slot of `opt-atomic-plan`.

### `opt-atomic-plan-operation`

Accessor for the `operation` slot of `opt-atomic-plan`.

### `opt-atomic-plan-memory-order`

Accessor for the `memory-order` slot of `opt-atomic-plan`.

### `opt-atomic-plan-opcode`

Accessor for the `opcode` slot of `opt-atomic-plan`.

### `opt-select-atomic-opcode`

```lisp
(cl-cc/optimize:opt-select-atomic-opcode (&key target operation memory-order))
```

Select a representative atomic opcode for OPERATION and MEMORY-ORDER.

This helper captures lowering intent only; exact instruction encoding remains
backend responsibility.

### `opt-build-atomic-plan`

```lisp
(cl-cc/optimize:opt-build-atomic-plan (&key target operation memory-order))
```

Build atomic lowering plan with selected opcode.

### `make-opt-htm-plan`

Construct a `opt-htm-plan` -- Hardware Transactional Memory lock-elision plan.

### `opt-build-htm-plan`

```lisp
(cl-cc/optimize:opt-build-htm-plan (&key target supports-htm-p low-contention-p))
```

Build an HTM lock-elision plan.

HTM path is enabled only when hardware support exists and contention is low.
Fallback lock path remains enabled conservatively.

### `make-opt-concurrent-gc-plan`

Construct a `opt-concurrent-gc-plan` -- Concurrent GC planning record.

### `opt-build-concurrent-gc-plan`

```lisp
(cl-cc/optimize:opt-build-concurrent-gc-plan (&key latency-sensitive-p heap-size))
```

Build conservative concurrent-GC plan for tri-color marking.

LATENCY-SENSITIVE-P keeps concurrent marking + SATB barrier.
Small heaps may disable mutator assist to avoid overhead.

### `make-opt-partial-specialization`

Construct a `opt-partial-specialization` -- Residual helper result for conservative constant-argument specialization.

### `opt-partial-spec-original-name`

Accessor for the `original-name` slot of `opt-partial-specialization`.

### `opt-partial-spec-specialized-name`

Accessor for the `specialized-name` slot of `opt-partial-specialization`.

### `opt-partial-spec-signature`

Accessor for the `signature` slot of `opt-partial-specialization`.

### `opt-partial-spec-static-args`

Accessor for the `static-args` slot of `opt-partial-specialization`.

### `opt-partial-spec-dynamic-args`

Accessor for the `dynamic-args` slot of `opt-partial-specialization`.

### `opt-partial-spec-residual-body`

Accessor for the `residual-body` slot of `opt-partial-specialization`.

### `make-opt-partial-eval-result`

Construct a `opt-partial-eval-result` -- Function-level partial-evaluation report used by FR-209/210 orchestration.

### `opt-partial-eval-function-name`

Accessor for the `function-name` slot of `opt-partial-eval-result`.

### `opt-partial-eval-parameters`

Accessor for the `parameters` slot of `opt-partial-eval-result`.

### `opt-partial-eval-signature`

Accessor for the `signature` slot of `opt-partial-eval-result`.

### `opt-partial-eval-binding-times`

Accessor for the `binding-times` slot of `opt-partial-eval-result`.

### `opt-partial-eval-form-kinds`

Accessor for the `form-kinds` slot of `opt-partial-eval-result`.

### `opt-partial-eval-residual-body`

Accessor for the `residual-body` slot of `opt-partial-eval-result`.

### `opt-partial-eval-dynamic-body`

Accessor for the `dynamic-body` slot of `opt-partial-eval-result`.

### `opt-partial-eval-specialization`

Accessor for the `specialization` slot of `opt-partial-eval-result`.

### `make-opt-partial-program-result`

Construct a `opt-partial-program-result` -- Program/module-level partial-evaluation report keyed by function name.

FUNCTION-RESULTS is an alist of:
(function-name . opt-partial-eval-result)

### `opt-partial-program-function-results`

Accessor for the `function-results` slot of `opt-partial-program-result`.

### `opt-specialize-constant-args`

```lisp
(cl-cc/optimize:opt-specialize-constant-args (function-name parameters body constant-bindings &key specialized-name))
```

Build a residual helper copy of BODY with constant PARAMETERS substituted.

This is a helper-level partial-evaluation primitive: it does not execute code or
install a clone in the function registry. It records the static signature and
returns a residual body that later passes can fold safely.

### `opt-partial-evaluate-function`

```lisp
(cl-cc/optimize:opt-partial-evaluate-function (function-name parameters body &key (constant-bindings nil) (lattice-bindings nil) specialized-name))
```

Partially evaluate one function body and return residual + BTA report.

This is a function-level FR-209/210 entrypoint that composes:
1) constant substitution residualization,
2) binding-time analysis merge, and
3) offline BTA classification over residual forms.

### `opt-partial-evaluate-program`

```lisp
(cl-cc/optimize:opt-partial-evaluate-program (function-definitions &key (constant-bindings-by-function nil) (lattice-bindings-by-function nil) (max-iterations 64)))
```

Run function-level partial evaluation across FUNCTION-DEFINITIONS.

FUNCTION-DEFINITIONS format:
((fn-name :params (...) :body (...)) ...)

CONSTANT-BINDINGS-BY-FUNCTION and LATTICE-BINDINGS-BY-FUNCTION are alists:
((fn-name . ((param . value) ...)) ...)
((fn-name . ((param . lattice) ...)) ...)

Returns OPT-PARTIAL-PROGRAM-RESULT with per-function reports.

Performs a monotone inter-function fixpoint: inferred constants from residual
call-sites are propagated across function boundaries until convergence.
MAX-ITERATIONS is a safety guard for pathological inputs.

### `opt-partial-evaluate-modules`

Exported but not yet implemented in this repository -- calling it signals `undefined-function`. Tracked as a known gap; see `optimizer-peval-specialize.lisp` and `optimizer-peval-program.lisp` for the sibling functions (`opt-partial-evaluate-function`, `opt-partial-evaluate-program`) that are implemented.

### `opt-partial-evaluate-modules-cached`

Exported but not yet implemented in this repository -- calling it signals `undefined-function`. Tracked as a known gap; see `optimizer-peval-specialize.lisp` and `optimizer-peval-program.lisp` for the sibling functions (`opt-partial-evaluate-function`, `opt-partial-evaluate-program`) that are implemented.

### `opt-partial-evaluate-modules-incremental`

Exported but not yet implemented in this repository -- calling it signals `undefined-function`. Tracked as a known gap; see `optimizer-peval-specialize.lisp` and `optimizer-peval-program.lisp` for the sibling functions (`opt-partial-evaluate-function`, `opt-partial-evaluate-program`) that are implemented.

### `opt-partial-evaluate-functions-incremental`

Exported but not yet implemented in this repository -- calling it signals
`undefined-function`. Tracked as a known gap alongside
`opt-partial-evaluate-modules`, `opt-partial-evaluate-modules-cached`, and
`opt-partial-evaluate-modules-incremental`.

### `make-opt-binding-time`

Construct a `opt-binding-time` -- Binding-time classification for one parameter under the SCCP lattice.

### `opt-binding-time-parameter`

Accessor for the `parameter` slot of `opt-binding-time`.

### `opt-binding-time-kind`

Accessor for the `kind` slot of `opt-binding-time`.

### `opt-binding-time-value`

Accessor for the `value` slot of `opt-binding-time`.

### `opt-binding-time-lattice`

Accessor for the `lattice` slot of `opt-binding-time`.

### `opt-sccp-analyze-binding-times`

```lisp
(cl-cc/optimize:opt-sccp-analyze-binding-times (parameters lattice-bindings))
```

Classify PARAMETERS as :STATIC or :DYNAMIC using SCCP lattice bindings.

### `opt-run-binding-time-analysis`

```lisp
(cl-cc/optimize:opt-run-binding-time-analysis (parameters &key (constant-bindings nil) (lattice-bindings nil)))
```

Run a conservative binding-time analysis for PARAMETERS.

Priority:
1) CONSTANT-BINDINGS are treated as compile-time static facts.
2) Remaining parameters are classified from LATTICE-BINDINGS via SCCP.

This provides an explicit BTA entrypoint (FR-210) that can be used by
partial-evaluation passes without requiring callers to manually merge sources.

### `opt-offline-bta-classify-form`

```lisp
(cl-cc/optimize:opt-offline-bta-classify-form (form &key (static-bindings nil) (binding-times nil)))
```

Classify FORM as :STATIC or :DYNAMIC using an offline BTA approximation.

STATIC-BINDINGS are explicit compile-time facts `(var . value)`.
BINDING-TIMES may include `opt-binding-time` entries (e.g. from SCCP merge).
Only bindings classified as :static are treated as compile-time-known names.

### `opt-offline-bta-analyze-body`

```lisp
(cl-cc/optimize:opt-offline-bta-analyze-body (body &key (static-bindings nil) (binding-times nil)))
```

Classify each form in BODY as :STATIC or :DYNAMIC via offline BTA.

### `make-opt-specialization-plan`

Construct a `opt-specialization-plan` -- Known-callee specialization plan keyed by a constant-argument signature.

### `opt-specialization-plan-callee-label`

Accessor for the `callee-label` slot of `opt-specialization-plan`.

### `opt-specialization-plan-specialized-name`

Accessor for the `specialized-name` slot of `opt-specialization-plan`.

### `opt-specialization-plan-signature`

Accessor for the `signature` slot of `opt-specialization-plan`.

### `opt-specialization-plan-static-args`

Accessor for the `static-args` slot of `opt-specialization-plan`.

### `opt-specialization-plan-dynamic-args`

Accessor for the `dynamic-args` slot of `opt-specialization-plan`.

### `opt-specialization-plan-clone-needed-p`

Accessor for the `clone-needed-p` slot of `opt-specialization-plan`.

### `opt-specialization-plan-cache-hit-p`

Accessor for the `cache-hit-p` slot of `opt-specialization-plan`.

### `opt-build-specialization-plan`

```lisp
(cl-cc/optimize:opt-build-specialization-plan (callee-label arguments constant-bindings &key cache))
```

Build a conservative clone/call-redirection plan for known constant arguments.

Returns NIL when ARGUMENTS have no known constants. When CACHE is supplied, the
same `(callee . signature)` pair reuses the earlier specialized name and marks
the plan as a cache hit instead of requesting a new clone.

### `opt-pass-specialize-known-args`

```lisp
(cl-cc/optimize:opt-pass-specialize-known-args (instructions))
```

Conservatively clone known-callee functions specialized by constant call args.

### `opt-pass-partial-evaluation`

```lisp
(cl-cc/optimize:opt-pass-partial-evaluation (instructions))
```

Pipeline entrypoint for partial evaluation over known constant call arguments.

Current strategy specializes known-callee call sites into residual clones with
only dynamic arguments forwarded, then relies on downstream fold/SCCP cleanup.

### `make-opt-cow-object`

Construct a `opt-cow-object` -- Copy-on-write wrapper for planner/runtime-independent optimization metadata.

PAYLOAD is treated as immutable by readers; writers must call OPT-COW-WRITE to
ensure uniqueness before mutation.  REFCOUNT models shared aliases.

### `opt-cow-object-payload`

Accessor for the `payload` slot of `opt-cow-object`.

### `opt-cow-object-refcount`

Accessor for the `refcount` slot of `opt-cow-object`.

### `opt-cow-copy`

```lisp
(cl-cc/optimize:opt-cow-copy (cow))
```

Return a logical copy of COW by incrementing shared refcount.

This operation is O(1): no payload duplication occurs here.

### `opt-cow-write`

```lisp
(cl-cc/optimize:opt-cow-write (cow write-fn))
```

Apply WRITE-FN under copy-on-write semantics and return the resulting object.

When COW is uniquely owned (REFCOUNT <= 1), WRITE-FN mutates its payload in
place.  When shared, this function decrements the old object's REFCOUNT and
materializes a detached copy using COPY-TREE before applying WRITE-FN.

### `schedule-pre-ra`

```lisp
(cl-cc/optimize:schedule-pre-ra (instructions))
```

FR-067: pressure-aware list scheduling before register allocation.

Builds a dependency DAG for each basic block, computes critical-path priorities,
and schedules ready instructions with a register-pressure tie-breaker.  The pass
never moves instructions across labels, control-flow instructions, calls, stores,
signals, or other side-effecting barriers, so basic-block boundaries and codegen
semantics are preserved.  FR-067 is complete and this function is the backend
entry point used before register allocation.

### `opt-verify-instructions`

```lisp
(cl-cc/optimize:opt-verify-instructions (instructions &key pass-name))
```

Conservative VM-level verifier for optimizer/debugging use.
