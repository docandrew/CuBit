# Physical split/coalesce geometry

This tests and proves `kernel/src/buddy_geometry.adb`, which the production
buddy allocator now uses to construct split children and merged parent spans.
There is no separate model substituted for the production arithmetic.

Run from the repository root, sequentially, using Nix:

```sh
nix develop -c bash tests/buddy-geometry/prove.sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/buddy-geometry/buddy_geometry_tests.gpr && ../tests/buddy-geometry/build/main'
nix develop -c make -C kernel cubit_kernel
nix develop -c python3 tests/buddy-geometry/check-codegen.py
nix develop -c python3 tests/buddy-bitmap/check-codegen.py
nix develop -c tests/headless/run.sh --test capability-security --disk kernel/capability_security_disk.img --accel kvm --cpus 4 --timeout 90 --serial /tmp/cubit-geometry-native-serial.log --keep-logs
nix develop -c env CUBIT_DOOM_MULTIAPP=1 tests/headless/run.sh --test desktop-doom --accel kvm --cpus 4 --timeout 90 --serial /tmp/cubit-geometry-desktop-serial.log --keep-logs
```

GNATprove passes **46 analysis obligations**: 16 runtime checks, 15 functional
contract checks, three assertions, four initialization checks and eight
termination checks. Zero unproved or justified obligations. Both arithmetic
proof helpers are Ghost; the core has no SPARK-Off or Assume. The proof uses
the default CVC5/Z3 configuration, not additional axioms or an extended timeout.

Properties:

- A valid block is an aligned span inside the 40-bit physical-frame space.
- Splitting an even span preserves alignment, creates equal non-overlapping
  adjacent children, and exactly covers the original span.
- Merge requires equal-sized neighbors with alignment at the combined size;
  mere adjacency is insufficient. Its result exactly covers both children.
- Splitting and then merging those children restores the original block,
  including both start and length (proved by the Ghost composition check).

The core deliberately uses explicit lengths rather than embedding allocation
orders/exponent arithmetic. The kernel supplies power-of-two lengths from its
orders. The arithmetic properties also hold for aligned non-dyadic even spans;
the host suite exercises these too. This is not permission for the allocator
to admit arbitrary-sized allocations.

Host coverage includes inclusive maximum boundaries, exhaustive small extent
admission, invalid/reversed/duplicate merge pairs, maximum-sized blocks, and
390,000 deterministic full-width dyadic split cases checked against an
independent XOR oracle. Codegen checks inspect the actual kernel object:
no Ghost helpers, no undefined runtime dependencies, and no division or helper
calls in `Split`. This is not a whole-allocator performance benchmark.

Native four-vCPU KVM results: `capability-security` passed its eight partial-load
rollbacks and subsequent PID reuse; multi-app `desktop-doom` passed
Workbench/NetSurf launch and closure, DOOM game pixels and responsive Apps-menu
input. The kernel's 2048-byte per-function stack limit also passed.

The physical-memory adapter, order-to-span correspondence, free-list link/count
invariants, firmware reservation handling, and SMP/TLB lifetime boundaries are
not proved by this geometry core. In particular, local partition preservation
does not yet establish non-overlap among all live allocations.
