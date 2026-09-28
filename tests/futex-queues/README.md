# Futex checks

Hosted checks for the kernel's futexes (`docs/threads.md`). These are
Linux-hosted: they exercise the kernel's `Futex_Queues` package and models of
the protocol, not a running CuBit kernel. The live check is
`userspace/apps/futex-check` (headless case `futex`).

| Check | What it establishes | How |
| --- | --- | --- |
| `prove-futex-queues` | `Futex_Queues` (kernel code, generic; proved at both kernel instances, 32-slot buckets and the 1023-slot overflow set): every operation keeps the bucket well formed (distinct waiters, distinct tickets); a wake takes only waiters of its key, oldest first; removal takes exactly the named waiter; no other slot changes. `Prove_FIFO`, `Prove_Key_Isolation`. | SPARK proof |
| `prove-futex-queues` | `Futex_Protocol` (model): FUTEX_WAIT as load and commit under the bucket lock, user stores at any time, FUTEX_WAKE under the lock. Invariant: every sleeper, and every waiter between load and commit, expects the word's current value or is covered by a pending wake. So a sleeper never waits forever on a stale value when every changing store is followed by a wake. | SPARK proof |
| `mutations.sh` | The proofs are not vacuous: seven mutants (newest-first wake, key ignored, ticket reuse, unchecked removal, no compare, unlocked wake, store without a wake obligation) each fail to prove. | mutation |
| `test-futex-queues` | `Futex_Queues` against an independent FIFO reference over 400,000 random waits, wakes and removals, contracts checked at run time. | hosted test |
| `explore.py` | Every interleaving of the Rust std futex mutex (2–3 threads) and the THREAD_EXIT join protocol, with the kernel's real WAIT/WAKE steps: mutual exclusion, and no deadlock or lost wakeup. Four mutants (unlocked kernel wait, twice; unlock without wake; exit word cleared after the wake) must be caught. | exhaustive model check |

Assumptions (not proved here): the bucket spinlock gives mutual exclusion;
`Process.User_Memory.Load_Word32` reads the word atomically; the scheduler
does not lose a `ready`.

Run from the Nix shell:

    make -C kernel test-futex-queues prove-futex-queues
    tests/futex-queues/mutations.sh
    python3 tests/futex-queues/explore.py
