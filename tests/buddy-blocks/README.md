# Production buddy block-state core

Run from the repository root; all tools use the Nix environment:

```sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/buddy-blocks/buddy_blocks_tests.gpr && ../tests/buddy-blocks/build/main'
nix develop -c bash tests/buddy-blocks/prove.sh
nix develop -c make -C kernel cubit_kernel
nix develop -c tests/headless/run.sh --test capability-security --disk kernel/capability_security_disk.img --accel kvm --cpus 4 --timeout 90 --serial /tmp/cubit-block-native-serial.log --keep-logs
nix develop -c env CUBIT_DOOM_MULTIAPP=1 tests/headless/run.sh --test desktop-doom --accel kvm --cpus 4 --timeout 90 --serial /tmp/cubit-block-desktop-serial.log --keep-logs
```

Run builds and proofs for this project sequentially; run native fixtures
sequentially because they stage shared boot artifacts.

The host test executes `kernel/src/buddy_blocks.adb`, the same transition core
used by the real allocator. It covers all six states and forty orders against
every transition and requested order, plus 4,608,000 split/merge combinations.
Enabled contracts check exact preservation on failure as well as success.
Additional sequences cover wrong-order and duplicate releases, retirement,
final reclamation, and split/coalesce round trips. Reserved and interior states
are canonical order zero; test constructor inputs cover these redundantly.

GNATprove: 18 analysis obligations, zero unproved/justified. Nine are discharged
by flow/termination analysis and nine by solvers (including all four transition
postconditions). No Assume, SPARK-Off core, or runtime kernel assertion checks.

Native results: `capability-security` passed eight partial-load rollbacks and
subsequent PID reuse. Multi-app `desktop-doom` passed Workbench/NetSurf launch
and closure, DOOM game pixels and responsive Apps-menu input. Kernel stack-limit
and existing bitmap codegen checks passed; `nm -u kernel/build/buddy_blocks.o`
reported no undefined symbols (including no assertion-runtime dependencies).

The physical adapter is tested by native fixtures but is **not yet proved**.
Host tests do not establish adjacency, free-list pointer integrity, locking,
firmware reservation correctness, memory-map aliases or SMP/TLB safety. See
[the proof boundary and next properties](../../docs/allocator-verification.md).
