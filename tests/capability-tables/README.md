# Capability handle table (CAP-002 step 1)

`nix develop -c make -C kernel test-capability-tables prove-capability-tables`

`Capability_Tables` (`kernel/src/capability_tables.ad[sb]`) is the per-process
handle table from docs/capability-handles.md: kernel-chosen placement,
least-recently-freed reuse, a generation per slot, a typed limit
(`Default_Limit` = 4096, `Kernel_Limit_Handles` spelled out), and storage that
grows by charged pages (a directory page plus 64-entry leaf pages). It is a
generic with no kernel dependencies; this directory instantiates it twice
(`test_tables.ads`):

- `Small`: 4 leaves = 256 slots, `Generation_Limit => 3`, so capacity and
  generation retirement are reached quickly;
- `Kernel`: the kernel's element (`Capabilities.Capability`) and geometry
  (512 leaves = 32768 slots, 32-bit generations).

Both use `Fake_Pages`, a stand-in for `Charged_Metadata_Pages` that hands out
real 4 KiB blocks, counts pages per owner, refuses on demand, and detects
double or foreign releases.

## Proved

GNATprove level 2 (`-u test_tables.ads`, both instances): 511 checks, none
unproved, none justified, no `pragma Assume` (level 1 leaves 2 assertions). The table
invariant `Valid` holds after every operation. Properties:

- No run-time errors.
- Insert never overwrites a live entry and changes no other entry
  (`Opened_Into`); the new entry's generation is greater than any the slot
  had.
- Lookup returns `Current` only for the live entry's own generation; Close
  and Revoke act only on a `Current` handle and leave the handle `Stale` (or
  `Revoked`) afterwards. A slot keeps its generation when vacated and gets
  a strictly greater one when reused, so no earlier handle resolves again.
  A slot at `Generation_Limit` is retired instead of reused: a generation
  never wraps.
- `Live_Count` equals the number of live entries (a ghost recursive count)
  and never exceeds the bound; `Limit_Reached` exactly when a bounded limit
  is reached; refusals (`Limit_Reached`, `Table_Full`, `Out_Of_Memory`)
  change nothing, including the pages held.
- Growth, close and revoke preserve every other entry (frame conditions
  over a ghost model of all slots).
- Pages: `Pages_Held` = directory + leaves held, every held leaf is dense
  (no leaf held before it is needed), and `Release_All` returns every page.
  SPARK ownership proves no page pointer is dropped or duplicated.
- `From_Word (W)` decodes every word, and `To_Word (From_Word (W)) = W`.

Not proved: free-list completeness and FIFO (least-recently-freed) order;
they affect which slot is chosen, not safety. Tested below.

Assumed: the page adapter (`Acquire_*`/`Return_*`, SPARK_Mode Off: page
address to owning pointer and back) and `Allocate_Page`/`Release_Page`
honour their contracts; the caller serializes operations on a table (the
kernel's mailbox lock).

## Tested

`build/main` (contracts and `Valid` executed, `-gnata`): scenarios for every
result kind (Invalid, Stale, Revoked, Current; Opened, Limit_Reached,
Table_Full, Out_Of_Memory), stale after close and after reuse, revoked then
stale after reuse, LRF order, generation retirement (also by revocation),
limit and refusal, capacity under `Kernel_Limit`, allocator refusal at the
directory, at a leaf and at the second leaf (table and pages unchanged), a
free slot needing no memory, release on exit and a new incarnation,
iteration, handle words; then four randomized differential runs (fixed
seeds, 50,000 operations each) against an independent reference model,
comparing every result and every slot after each operation, with injected
allocation failures and random limit changes.

`build/geometry` (kernel geometry, no `-gnata`: `Valid` is quadratic in
32768 slots): default limit at 4096 (65 pages), `Kernel_Limit` to 32768
(`Table_Full`, 513 pages), release, then a 300,000-operation differential
run.

## Mutation checks

Each mutation applied alone to a private copy; "tests" = `main` and
`geometry`, "proof" = level 2. All seven caught by the tests; six by proof.

| Mutation | Tests | Proof |
|---|---|---|
| m1: `Check` ignores an older generation (stale resolves) | caught (postcondition at run time; differential) | caught (`Lookup` post: generation equal) |
| m2b: limit check `>` instead of `>=` in Insert | caught | caught (`not At_Limit` on Opened/Table_Full) |
| m3a: free list LIFO instead of FIFO | caught (LRF order, differential) | caught (frame/`Vacated` posts) |
| m3b: fresh slot preferred over a freed one | caught | caught (`Insert_Fresh` precondition) |
| m4a: adapter does not release the page (uncharge) | caught (pages held, allocator count) | not caught: trusted adapter, as documented |
| m4b: failed growth drops the directory pointer | caught | caught (memory leak) |
| m4c: Release_All keeps the charge count | caught | caught (`Is_Empty` post) |

FIFO order itself is not specified, so the proof catches m3a and m3b only
incidentally (the LIFO edit also rewrites the vacated entry; the reordering
violates a helper's precondition). A cleaner reordering would be caught by
the tests alone. A further m2 mutated the specification's own `At_Limit` (`>`): the tests
catch it; the proof cannot, since the contract is defined by it (the bound
`Live <= Bound` still holds; only the refusal's label changes).
