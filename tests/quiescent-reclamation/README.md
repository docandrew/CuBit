# Quiescent-state reclamation for table pages

`nix develop -c make -C kernel test-quiescent-reclamation prove-quiescent-reclamation`

`Quiescent_Reclamation` (`kernel/src/quiescent_reclamation.ads`) decides when
a page of process or thread records, removed from its table's directory, may
be freed while lock-free readers on other CPUs might still hold pointers into
it (docs/threads.md). Each CPU advances a counter at quiescent points, where
it holds no record pointer. A page retired with a snapshot of the counters is
freed once every online CPU's counter has passed its snapshot value. Nothing
waits: unlike `TLB_Reclamation`, retired pages are freed later.

The rules are functions over plain counter arrays. The kernel adapter keeps
one counter per CPU (atomic components) that only that CPU writes, so CPUs
never contend or overwrite each other's progress. An earlier version bundled
all counters in one record passed `in out`, which could copy the record and
lose a concurrent update from another CPU.

**Proved (GNATprove):** 15 checks, none unproved or justified, no `Assume`:
- `Next_Count` advances by exactly one and saturates at the maximum.
- `Advanced` (ghost) changes only the advancing CPU's counter.
- `Prove_Idle_CPU_Blocks`: an online CPU that has not advanced since the
  snapshot keeps the grace period from elapsing.
- `Prove_Monotonic`: advancing never moves counters before an earlier
  snapshot.
- `Prove_Everyone_Advanced`: once every CPU has advanced past the snapshot
  without saturating, the grace period has elapsed, so reclamation makes
  progress.
- A snapshot taken at the maximum can never satisfy the grace condition, so
  such pages leak instead of being freed early.

**Hosted simulation:** 500,000 random steps of readers taking and dropping
pointers, quiescent points, retirements and reclamation. It asserts that no
page is freed while any reader holds it, and checks offline versus online
CPUs. A mutation that ignores one CPU in the grace condition makes the
simulation fail ("page freed while a reader holds it"), so the check
discriminates.

**Not proved, and required of the kernel adapter:**
- quiescent points are placed only where the CPU holds no record pointer;
- the snapshot is taken after the page is unlinked from every lookup path;
- online-CPU tracking is accurate;
- counters are published with the ordering the adapter's atomics provide.
