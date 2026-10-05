# Retained grant-record storage

The kernel's grant records, forwarding scopes and parent links use
`Retained_Record_Blocks` for stable, lazy page-block storage. These hosted tests
compile that same production adapter.
The namespace now supports 4096 grants per owner, while records are committed
in 64-record blocks on demand. This is a capacity bound, not an allocation of
4096 buffers per process. Each grant still has a 16 MiB maximum extent.

Run in the Nix development environment:

```sh
nix develop -c gprbuild -p -P tests/grant-storage/storage.gpr
nix develop -c tests/grant-storage/build/main
nix develop -c tests/grant-storage/build/forwarding
```

The fixture covers absent blocks, partial final blocks, sparse iteration,
typed initialization from dirty backing, existing-record access after quota
reduction, quota rejection, allocator failure at every growth step, alignment
and address-overflow rejection, and stable pointers across sixteen blocks
containing 1,001 addressable records. Assertions and overflow checks are enabled.

The allocator contract still requires fresh, disjoint, writable backing of
the requested size. Address checks do not establish that contract. The caller
must serialize access and retain published backing for the store lifetime.
There is no implicit record retirement or memory reclamation: those remain
the responsibility of the grant lifetime protocol. These are hosted regression
tests, not SPARK proofs or native kernel/hardware results.

The second fixture uses the actual kernel forwarding-state type, retaining a
configured parent while allocating ten blocks and injecting failure before
each growth. It checks typed initialization and preservation of the live parent.
Initialization writes one record at a time: a whole-block aggregate introduced
a 3,872-byte temporary rejected by the native kernel stack gate. The in-place
version's compiled `Ensure` frame is 80 bytes with the current native toolchain;
this is a per-frame measurement, not a proof of total call-chain stack usage.

The production instance preserves grant-lock serialization and allocation-before-
hold ordering. Native forwarding-retention and mapping-growth fixtures must
also pass under the shared build lock. The namespace expansion changes kernel
and runtime bounds together; consumers must be rebuilt rather than mixing
old binaries with the new slot geometry.

2026-10-02 integration evidence: kernel build and both native CuBit QEMU gates
passed (`build/native-integration-r2.log`). Forwarding stress:
`../intel-gpu/demand-backing.bWBHgD/serial.log`; mapping growth:
`../intel-gpu/demand-backing.XzbODe/serial.log`. Each directory records exact
kernel and fixture hashes. These test kernel IPC and lifetimes, not Intel GPU
execution, and do not inject native allocator failure.

The native view fixture also covers the legacy owner-revoke syscall boundary:
wrong-owner rejection, successful revocation with a held reader, closed new
admission, retained readable contents, final-reader retirement, and rejection
of a retired slot. The syscall no longer inspects the grant table outside the
IPC lock. Evidence: `build/locked-revoke-native-r2.log` and
`../intel-gpu/demand-backing.CaZCpV/serial.log`. This sequential regression is
not an exhaustive concurrency proof. An initial run failed the oracle because
its expected marker order was wrong; the corrected oracle passed a fresh run.

## Native owner retirement and exact PID reuse

With a built kernel, run this disposable guest under the shared build lock:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash tests/grant-storage/native/run.sh
```

The bootstrap spawns its own ELF as an unprivileged child at PID 42. It acquires
the child's page, requests exit and waits for the parent's child-retirement
event. In `Process.reclaimProcess` this notification follows endpoint
invalidation and grant-protected teardown. Only the two test processes run;
this is a controlled phase oracle, not a claim that arbitrary IPC events are
authenticated. There is no sleep-based inference of completed retirement.

The same PID cannot be spawned while its reader holds the page. All 4,096 bytes
remain readable after retirement, new admission fails, and returning the held
acquisition succeeds. After the return, the identical ELF is successfully
spawned at exactly PID 42. Its first grant reuses the same global slot with a
different generation. The fresh endpoint rejects the old grant; the old
endpoint rejects the fresh grant. An old return cannot release the replacement
acquisition. Fresh contents, acquisition return and replacement retirement are
checked. The host requires ordered success markers, rejects failure markers,
bounds execution to 120 seconds and terminates only its own QEMU process.

This tests actual CuBit process/grant syscalls with four emulated CPUs, not GPU
execution, native allocation-failure injection or an exhaustive SMP proof.
The fixture records exact kernel and executable hashes in its evidence folder.

2026-10-02: the final strengthened fixture passed in
`build/lifetime.xjkUSY/serial.log`, with its build/run log at
`build/lifetime-native-r3.log`. Kernel SHA-256:
`6de2e3085d0d85c98bf9debaeca8c98cc26e42f536835dfaff8408a19a090acb`.
No kernel change was needed for this gate. An initial sandbox invocation could
not access the Nix cache; the subsequent permitted Nix runs completed.

## Expanded-capacity native gate

`native/run.sh capacity` creates and acquires all 4096 owner slots, crossing
64-record metadata block boundaries. It verifies readable content after growth
and exhaustion, denies a 4097th grant, retains a revoked slot until its reader
returns, reuses that exact slot with a different generation, rejects an old
return and retires every acquisition. The fixture spreads aliases over 128
physical pages to respect the independent 127-pin-per-frame bound; it does not
raise that bound to make the test pass.

2026-10-02 capacity and owner/PID reuse PASS with expanded kernel
`109b21724d28b3c6fecb8e41a4cab2f702266736bec61c5aa59760ba2c25e5ea`:
`build/lifetime.jSzmRZ/serial.log` and `build/lifetime.Si3tId/serial.log`.
Forwarding and mapping growth PASS in
`../intel-gpu/demand-backing.S4jWUQ/serial.log` and
`../intel-gpu/demand-backing.1UalJu/serial.log`. Build logs are
`build/capacity-native*.log`. The first fixture compile needed an operator
visibility clause; the first execution aliased one page and hit its pin bound.
The old forwarding fixture expected the seventeenth grant to fail; it now
requires admission and clean retirement, while the new capacity test checks
actual namespace exhaustion.

The hosted codec checks every global slot and the initrd/owned-memory aperture
boundaries. Its focused SPARK gate passes 20 checks with zero unproved. The
production Mesa bridge compiles natively and its hosted C/Ada regression accepts
the expanded maximum and rejects the next slot (`build/capacity-bridge-r2.log`).
No new physical NUC image or GPU execution claim accompanies these results.
