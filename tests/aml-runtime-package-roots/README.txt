Source-only public Collecting GC/lifetime fixture; no hosted compile or run yet.

The Program constant is the exact common AML body from two normally compiled
SSDTs (header revisions1/2); all source/disassembly/raw ACPICA output is retained.
The oracle observes sixteen package returns and sixteen MARK=4 values. Warnings,
remarks and four cache teardown allocations per process remain evidence.
ACPICA supplied its own revision2 DSDT: this oracle validates byte structure and
small counts, not global32bit arithmetic. Hosted tests explicitly run both widths.

NAM0: runtime Name of nested package, method-local name retirement.
RET0: unnamed nested package returned directly.
CAL0: package first actual, later PRES actual allocates before PASS returns it.
CEL0: nested RET0 return -> Local -> PASS, later actual allocation.
Four CNUM calls per construction each allocate64byte temporary garbage. Full
recursive package sizes3/2/1 and all four buffers are checked after collection;
previous returned tree remains retained during the next construction. Two passes
per width exercise reuse. Completed+FreedObjects+FreedBytes increase, zero new
Rejected, explicit root count/release, and subsequent pressure reclamation are
required. Module_Transient_Free is checked-profile-only ghost evidence.

This public fixture observes collection during count/argument allocation, not an
injected callback at every individual Append instruction. It does not establish
private builder token replay/abort semantics or same-live-lease failure recovery;
those require the separate protocol seam. No guessed failure expectations added.
Production copied byte-for-byte from frozen rck2; implementation overlay and
bounded hosted runner must be composed/reviewed before compiling this fixture.

Final cleanup requires at least six reclaimed package element slots (3+2+1),
positive reclaimed object/byte counts, and zero remaining element storage. PRES
allocates only buffers, so its garbage cannot satisfy the package witness.
