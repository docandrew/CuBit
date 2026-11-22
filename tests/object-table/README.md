# Object table: dynamic process and thread tables

`nix develop -c make -C kernel test-object-table`

`Object_Table` (`kernel/src/object_table.ads`) is the dynamic, two-level table
behind the process table and, later, the thread table (docs/threads.md).
- **Records** live in pages allocated on demand. A directory maps page
  indexes to pages, so `Lookup` is two loads and takes no lock, and records
  never move while their page is live.
- **IDs and generations** come from the proved `Id_Ledger`
  (`tests/id-ledger`).
- **Emptied pages** are unlinked and freed only after a quiescent-state
  grace period, using the proved `Quiescent_Reclamation` rule
  (`tests/quiescent-reclamation`).
- **Lookups of unused IDs** return a shared `Absent` record in its reset
  state.

## Hosted concurrency test

Four reader tasks (real OS threads) look up random IDs lock-free and check
every record's tag (0 or its ID's value), passing a quiescent point between
lookups. A writer allocates, tags, releases and reclaims 2,000,000 times.
Freed pages are poisoned before being returned to the C allocator, so a
reader touching freed memory fails the run. The `Absent` record is checked
for stray writes.

A typical run: about 628,000 allocations, 1,975 pages mapped, 1,943 freed
after their grace period, and 360–540 million concurrent lock-free reads,
none of freed memory.

**What it found.** The first version marked an ID used (in the ledger)
before resetting its record. A lock-free reader in that window, on a page
kept live by other records, read an uninitialized record: poison from a
recycled page. Records are now published (a per-ID atomic flag) only after
reset, and unpublished before release. Removing the flag makes the test fail.

**What it does not show.** The adapter issues a full fence after unlinking
a page (before snapshotting the counters) and after each quiescent counter
update. x86 lets a later load pass an earlier store, and without the fences
a reader could take a page pointer after its quiescent point while the
snapshot records its old count. The test passes with the fences removed: the
window is too narrow to hit on the host. The fences are reasoned from the
memory model, not demonstrated.

**Not proved or tested here:**
- the kernel instance (buddy-allocated pages, the kernel lock, placement of
  quiescent points in the scheduler);
- ID width beyond the test's 256;
- `Reset` of a kernel `Process` record.

Those come with the process-table migration and the native suites.
