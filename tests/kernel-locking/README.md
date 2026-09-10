# Linux-hosted kernel locking regressions

Run `nix develop -c make -C kernel test-locking`.

The test compiles production `Locks`, `Spinlocks`, and `Process.Queues`.
The fixture provides host task-local CPU/exclusion state, a TLB-service counter,
disabled tracing, and minimal process storage with a lock-checking ready adapter.
The actual x86 compare/exchange and PAUSE instructions execute on Linux.

Four workers contend on a shared lock, perform 400,000 nested-lock updates, and
check visibility, ownership, and balanced exclusion. Queue tests check sleep
wakeup serialization, duplicate wakeup, delta preservation, and queue endpoints.
A 30-second timeout makes a deadlocked host test fail.

Ready-queue tests simulate 300 quantum expirations and compare 10,000 seeded
arrival/blocking operations against an independent stable-array ordering model.
They check descending priority, FIFO ties, forward/back links, empty endpoints,
and removal cleanup using the **production** queue code. The original `>=`
insertion failed the first FIFO assertion; strict `>` insertion passes.
This is deterministic policy testing, not host wall-clock scheduling measurement.

Assertions are enabled **only in this host executable**. No fixture is compiled
into CuBit and no kernel runtime assertion policy is changed.

These tests are not an SMP memory-model proof and do not exercise the real
scheduler, hardware interrupt masking, process retirement, or TLB invalidation.
Use the focused GNATprove targets and QEMU integration tests alongside them.
