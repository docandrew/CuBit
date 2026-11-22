# Buddy bitmap layout and indexing

These tests compile the actual `kernel/src/buddy_bitmap` package used by the
physical allocator, not a substitute model.

```sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/buddy-bitmap/buddy_bitmap_tests.gpr && ../tests/buddy-bitmap/build/main'
nix develop -c bash tests/buddy-bitmap/prove.sh
nix develop -c bash -lc 'make -C kernel cubit_kernel && python3 tests/buddy-bitmap/check-codegen.py'
```

The host test enables assertions and overflow checks; the kernel does not.
It checks every frame for inclusive maxima 0 through 1025 and bitmap orders
0 through 11. Additional tests cover all supported orders through 39,
power-of-two boundaries and the maximum 40-bit physical frame number.
An additional 400,000 deterministic full-width cases compare shift indexing
against an independent division expression across all 40 supported orders.

The historical regression is explicit: with highest frame 15, the old order-0
last-pair bit and order-1 first-pair bit both had index 7. The new layout gives
the first slice eight bits, placing order 1 at bit 8.

The focused proof completes 87 obligations with zero unproved or justified
checks: 35 runtime checks, 12 assertions, 27 functional contracts and 13
termination checks. In particular, the ghost procedures prove separation
between arbitrary distinct orders and coverage of every valid bit by allocated
64-bit words. The constructor establishes the layout invariant and lookup
preserves the per-order and enclosing bounds. These ghost routines are absent
from the kernel object; no `Assume` or SPARK-Off section exists in the core.

Pair indices retain their bounded machine-word type until converted once to
integer bitmap offsets. Ghost lemmas establish word-shift monotonicity, the
word-to-integer ordering bridge and their composition. `Pair_Span`, `Limit_Bit`
and `Consistent` are ghost observations too; the host test's running boundary
value is ghost state. Actual index/count and storage-size operations remain
non-ghost because the allocator needs them.

The script adds CVC5's `--solve-bv-as-int=sum` encoding alongside the default
CVC5 and Z3 configurations. The default encoding stalls on the mixed-theory
ordering bridge; the integer encoding proves that actual obligation. The
checked-in Why3 configuration uses the CVC5 already pinned by Nix. No assumed
lemmas or manual proof-file edits are required. The absolute configuration path
avoids GNATprove 15's relative-path
handling issue.

The codegen check inspects the real kernel object: all proof helpers must be
absent, and `Locate` must contain a right shift with neither division nor a
subprogram call. This establishes the intended instruction shape, not a measured
allocator throughput or latency guarantee.

The raw adapter validates the physical frame number and supplies the initialized
layout. It still owns physical overlays, storage reservation and locking. The
proof does not certify those operations, intrusive free-list membership,
allocation non-overlap, double-free rejection, split/coalesce state transitions
or SMP correctness. These are tracked in
[the allocator verification plan](../../docs/allocator-verification.md).

Native regression:

```sh
nix develop -c tests/headless/run.sh --test capability-security --disk kernel/capability_security_disk.img --accel kvm --cpus 4 --timeout 90
```

This exercises the integrated bitmap through ordinary allocation, stack/heap
growth and eight repeated partial-load rollbacks followed by successful PID
reuse. It is not a randomized allocator or physical-exhaustion test.
