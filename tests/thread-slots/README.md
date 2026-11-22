# Thread-count slots

`nix develop -c make -C kernel test-thread-slots prove-thread-slots`
and `tests/thread-slots/mutations.sh` (from the Nix shell).

Each process has two read-only thread-count pages after its readiness page
(`shared/publications/readiness_publication.ads`): 128 lines of 64 bytes,
one per thread slot. System-call entry adds one to the calling thread's
line (`Process.threadtab.callCounter`), so a process reads a thread's count
with a load instead of the retired READ_STATISTICS. The kernel hands out
slots with `Thread_Slots` (`kernel/src/thread_slots.ads`, SPARK): the main
thread holds slot 0 from process creation, `THREAD_CREATE` takes the lowest
free slot (returned in the result above the thread ID, and in the new
thread's RSI) and `THREAD_EXIT` gives it back; the next thread to take a
slot finds its line zeroed.

**Proved (GNATprove, level 2):** 24 checks, none unproved or justified, no
`Assume`. `Thread_Slots`: `Take` returns the lowest free slot and marks only
it, or changes nothing when all are taken; `Give_Back` frees only the given
slot. `Slot_Protocol` (this directory, test-only) models one process's
thread table over them and proves the invariant, kept by every create and
exit: the main thread is live in slot 0, every live thread's slot is taken,
and no two live threads share a slot (`Prove_One_Writer`), so every count
line has a single writer.

**Mutants** (`mutations.sh`, each must fail to prove; all five do): `Take`
that does not mark the slot, `Take` that hands out a taken slot,
`Give_Back` that frees nothing, a create recorded in the main thread's slot,
and an exit that frees the main thread's slot instead of its own.

**Hosted simulation** (`main.adb`): fill all 128 slots (8 more creates
refused), then one million random creates and exits with contracts checked,
asserting after each step the invariant, the lowest-free rule and that the
slots taken equal the live threads.

**Native** (`tests/headless/run.sh --test secondary-stacks`):
`secondary-stack-check` starts two batches of four threads through
`CuBit.Thread_Areas`. Each worker checks the slot it received in RSI against
the one in `THREAD_CREATE`'s result, reads its line before its first call
(0, including the second batch's reuse of the first batch's slots) and that
one YIELD adds exactly one; the main thread checks the same for itself.
`capability-test` (`--test capability-security`) checks that the retired
READ_STATISTICS (134) writes nothing and that `Owned_Bytes`, `Free_Memory_Bytes`
read sensibly; `storage-check` checks `Owned_Bytes` page by page against its
commits and drops.

**Not proved, required of the kernel:**
- `createThread` takes the slot and raises `threadCount` in one critical
  section of `Process.lock`, and `exitThread` gives it back and lowers
  `threadCount` in one; a refused `Take` refuses the thread (the limit and
  the slot set agreeing is shown by the simulation, not proved);
- `callCounter` is set to the slot's line before the thread first runs, and
  to `uncountedCalls` before its slot is given back;
- the pages are freed only after every thread of the process is gone.
