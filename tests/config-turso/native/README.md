# Native Turso bring-up

This runs the real Rust Turso engine **inside CuBit**, not Linux. It is an
isolated test application, **not a persistent replacement for `config.svc`**.
The probe and Config storage worker now use the shared CuBit Rust std runtime;
normal desktop Config persistence remains separately opt-in.

There is now a separate [native storage-worker startup test](../../../userspace/services/config-storage/README.md)
(`config-storage` headless case). Public native Create/Get/Set and read-only
recovery across boots are also covered by `tests/config-object-client/native-app`.
Do not confuse those live Config tests with this probe's direct database tests.

## Transaction I/O attribution

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c \
  bash tests/config-turso/native/run-sql-profile.sh /tmp/cubit-sql-profile-results
```

The opt-in `sql-bench` feature wraps the actual scoped transport with a bounded
counter snapshot. Normal builds contain no metrics calls, clocks or locks from
this wrapper. It forwards every operation and its exact result (including
errors/short transfers), counts acknowledged read/write bytes, and times the
inner filesystem call. The hosted test checks forwarding, actual byte counts,
failure accounting, vector counts and snapshot differences.

The native workload creates a **separate test database** under the existing
test-only volume scope and commits integer values 1..129 through
`Store::commit_declared_object`, the same storage library path used by the
worker. Each commit is read back outside its timing interval. No serial output
occurs inside the measured loop; final samples include the total commit time
and per-operation transport totals. Wrapper accounting overhead remains in
the outside-transport remainder, not falsely attributed to SQL alone.

The host verifies sample completeness/accounting, the checkpointed database's
exact schema/all 129 revisions via independent SQLite, and read-only e2fsck.
This diagnostic isolates the storage engine/adapter/filesystem side; it does
not include the public Config endpoint or Ada worker codec. Use the separate
public Config benchmark for end-to-end results. The TSC/host-load caveats and
nonjournaled-ext2 durability limits still apply. Results and attribution are
recorded in `../io-findings.md`. The script restores the regular smoke-test
binary after a successful run; after a failed run rebuild `--features turso`
before using the ordinary native test again.

## Reproduce

Build the normal kernel/services/disk first if this is a fresh checkout:

```sh
nix develop -c make -C kernel world
flock --exclusive coordination/build.lock nix develop -c bash -c '
  make -C kernel filesystem &&
  bash tests/config-turso/native/build.sh --features turso &&
  bash tests/headless/run.sh --test turso-native \
    --accel tcg,thread=multi --timeout 60 --serial /tmp/turso-native.log --keep-logs'
```

KVM may replace TCG when available; use `QEMU_CPU_MODEL=host` with KVM on AMD
hosts to avoid mixed AMD-vendor/Intel-model identification tripping getrandom's
old-AMD RDRAND safety exclusion. No entropy safety check is bypassed.
The tested guest has 128 MiB and four CPUs;
the application also exercises four concurrent storage-client threads. The runner copies its executable
and Clock into a disposable test disk. It does not install a new Config backend
in the interactive desktop. Boot tests share staging: run them sequentially.

## Native I/O measurements

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c \
  bash tests/config-turso/native/run-benchmark.sh /tmp/cubit-native-io-results
```

Use a new results directory. This builds the opt-in `bench` feature, then runs
three independent KVM boots on disposable disks. KVM and a compatible host CPU
are required; it never silently substitutes TCG. The output retains serial
measurements, independent SQLite/ext2 validation logs and host information.

The same verified File workload runs on Linux and CuBit, but the native version
injects a calibrated, serialized local TSC clock. The normal provisional Rust
`Instant` bridge uses millisecond Clock IPC and is unsuitable for measuring
short I/O. Calibration uses the existing Ada benchmark helper (three 200 ms
intervals, at most 2% spread); cross-vCPU TSC agreement remains an assumption.
This changes benchmark instrumentation, not the OS clock or Rust std API.

Eighteen phases exercise 4 KiB/64 KiB reads, overwrites, vectored writes and
write-plus-flush; batch sizes 1/8 are **not** device queue depth. The baseline
adapter completes synchronously (`peak_deferred=0`). Every phase verifies
exact content/counts outside timing and excludes setup/serial logging. The 64
measured samples per phase are exploratory: nearest-rank p99 equals the maximum,
not strong evidence of a latency bound. Guest flush completion is not ext2
power-cut atomicity, and virtual-device timings are not physical NVMe results.

`report-benchmark.py` rejects missing/duplicate phases, malformed timing rows,
failed calibration, incomplete benchmark/persistence runs and unexpected deferred
operations. Its unit fixtures are synthetic and never performance evidence.
Linux/native performance comparisons still need controlled, matched storage,
sample counts, host contention and scheduling conditions.

The probe now uses one generated Ada standalone-library initializer, called
once before any Ada exports. It covers both storage and worker dependencies;
the redundant unbound storage archive was removed. This matches the production
worker's foreign-main elaboration discipline.

## Filesystem adapter

`src/native_io.rs` in the parent workspace implements Turso's `IO`/`File`
interfaces; `userspace/lib/storage/native/bridge.rs` and the Ada `Native_Storage` library
connect them to existing CuBit filesystem messages. This is **not** a POSIX
shim and does not enable ambient `std::fs` access.

The probe's manifest limits filesystem access to `@nvme:0/turso-native/`.
The adapter further accepts only its database, WAL, and workload paths. Each
open obtains a real `OPEN_DENY_SHARING` handle, including the WAL; `NoLock`
never weakens that hold. Explicit shared/unlock operations are unsupported.
Engine-local path hashes are not security identities: the filesystem enforces
inode exclusivity and per-process authority independently.

One mutex serializes calls and the single 64 KiB, page-aligned Ada grant buffer.
The transport reports its nonzero capacity from Ada; Rust chunks larger buffers
at that reported boundary. It no longer splits a 64 KiB request into sixteen
round trips. The grant costs 60 KiB more resident memory per channel, not per
file or I/O request. Ordinary 4 KiB requests still copy/request only their byte
range. The reusable mapping covers the entire isolated grant; acquisition
checks bounds/lifetime but is not hardware-enforced per-request subrange
isolation. No private Rust allocation is exposed to the filesystem.
The shared `userspace/lib/storage/Storage_Channel` submits through capability
async IPC; its separate completion/result operations let an owning dispatcher
continue other work. Rust buffers are copied through that page; their allocation
pages are never granted to another process or retained by the channel.

The probe's FFI wrapper still presents a **copying, synchronous** interface to
Turso: it owns the completion queue and sleeps in `waitCompletion`, rather than
polling. This wrapper belongs in a dedicated worker, not Config's IPC dispatcher.
It is not yet pipelined/zero-copy I/O or a demonstrated performance gain. The `direct` cache
hint uses the same native transfer path; it does not imply durability. `sync`
actually sends the filesystem flush request. Vectored writes explicitly complete
their parent once, including short/error cases; callbacks run outside the mutex.

Vectored writes now gather at most 32 borrowed segments per batch, capped by the
reported transfer capacity. One-segment batches retain the scalar path; others
pass a bounded C-layout descriptor array to Ada, which validates all lengths
before copying each segment directly into the existing grant. No concatenated
Rust payload is allocated, no segment pointer enters IPC, and no extra payload
copy is added. Empty segments are skipped; partial segments continue in the next
batch. A short/error completion stops the parent and poisons the backend as
before—batching is not atomicity or retry permission.

The benchmark header records `vector_layout=packed`. Historical segmented
results remain readable, but the reporter refuses to combine different layouts
or transfer capacities into one median.

Uncertain open/data/flush failures retire the backend from further data I/O.
Definite open denials do not. Close is still allowed for handle cleanup; it is
not an implicit flush retry. Grant shutdown requires confirmed retirement, and
the static buffer is never recycled even if revocation remains pending.

The Linux-hosted tests use this exact generic adapter with a model transport:
SQL/Config close-reopen, duplicate-open rejection, each of four failed/short
vectored-write chunks, short reads/zero tail, offset overflow, callback reentry,
flush failure and uncertain open failure. Model flush completion is **not**
durability. No new SPARK proof is claimed for this adapter or FFI.

The [Ada channel fault tests](../../storage-channel/README.md) exercise delayed,
stale and malformed completions, target death, page ownership and confirmed
retirement. The channel does not accept ordinary incoming IPC as a completion.

Async-path validation (2026-09-24): 36 hosted channel scenarios pass; two CuBit
boots restore revision 1 and commit revision 2, with independent SQLite and ext2
checks. The final retirement refinement also passes a fresh native seed run.
Logs: `/tmp/cubit-turso-async-final.log`, `/tmp/cubit-turso-async-final-seed.log`;
exported disks/databases: `/tmp/cubit-turso-async-final-20260924/{seed,reopen}/`.

`check-disk.py` independently checks a stopped guest's test image with read-only
`e2fsck` and Python SQLite, including integrity, the revision head and exact CBOR
bytes. It extracts the file with `debugfs`, without a privileged loopback mount
or modifying the image. The guest checkpoints and closes the database first;
the independent reader deliberately opens just that database, without its WAL.

For the smaller standard-library probe, omit `--features turso` and select
`--test turso-native-std`. The larger test deliberately rejects an executable
built without its SQL/Config checks by requiring additional serial markers.

### Two independent boots

```sh
flock --exclusive coordination/build.lock nix develop -c \
  bash tests/config-turso/native/run-reboot.sh /tmp/cubit-config-reboot-results
```

The result directory must not exist. The script first saves revision 1, stops
QEMU and checks/exports its raw disk. It builds the `reopen` probe and boots
a copy of that saved disk in a new VM. That probe must restore the old values
before committing revision 2 (scale 125 → 150); it cannot silently reseed an
empty database. Linux SQLite checks both immutable revisions and the new head.
This tests clean-close persistence across a full guest reboot, not abrupt
process death, torn sectors, power loss, or filesystem service resurrection.

`seed/` and `reopen/` contain `disk.img` and extracted `profile.sqlite` for manual
inspection. Neither boot writes to the user's base image in place. The script
restores the normal seed probe afterward so ordinary `turso-native` runs still
use a fresh database. To inspect the checker independently:

```sh
nix develop -c python3 tests/config-turso/native/test-check-disk.py
```

These three Linux-hosted oracle tests reject wrong boot phases, missing history,
altered payloads/schema and inconsistent revision heads. They test the checker;
the two actual QEMU runs are the evidence for native persistence.

The Turso harness checks ext2 both before and after each boot. It reuses an
existing fixture directory without calling `debugfs mkdir` again: a repeated
mkdir was found to leave orphaned directory metadata in e2fsprogs 1.47.4, even
though the tool reports that the directory already exists. This was reproduced
on a Linux-only disposable copy, independently of CuBit execution.

Validated 2026-09-24: both independent CuBit/TCG boots, all four pre/post ext2
checks, and Linux SQLite checks of revision 1 and the complete revision-1/2
history pass. The original cleanup rebuild encountered another session's
in-progress runtime source after both tests completed; cleanup now restores
saved seed binaries rather than rebuilding shared sources. The normal seed
probe was restaged separately against the already-tested Ada archives.

Validated on 2026-09-24, Nix / QEMU TCG (four CPUs, 128 MiB):

```text
TURSO-NATIVE: std probe PASS
TURSO-NATIVE: volatile SQL transaction PASS
TURSO-NATIVE: typed Config CBOR/revision/reopen PASS (volatile)
TURSO-NATIVE: shared File workload PASS (volatile)
TURSO-NATIVE: shared File workload PASS (filesystem)
TURSO-NATIVE: database and WAL exclusion PASS
TURSO-NATIVE: typed Config CBOR/revision/reopen PASS (filesystem)
STORAGE: filesystem grant retired
TURSO-NATIVE: Linux SQLite integrity/typed payload/ext2 PASS
```

The checks exercise the actual shared `Store` and CBOR codec: string, integer,
boolean, a complete revision, stale-revision conflict, and reopen with the same
MemoryIO device, followed by the same profile through actual filesystem IPC.
Linux then reads the database after the guest process and QEMU have exited:
`config_format`, `collections`, `revisions`, `object_types`, revision 1 of
`com.cubit.desktop/laptop`, and exact CBOR for theme `Alloy`, scale `125`, enabled
`true`. SQLite integrity and read-only ext2 checks both pass.
The Ada worker probe additionally creates and recovers its native Integer type
declaration and commits the corresponding typed value. The independent oracle
checks the exact canonical declaration and all value revisions, not just row
counts. The two-boot script verifies declaration recovery before the second
value commit. Current experimental database format is 3; older formats fail
closed instead of being silently reseeded.
This is clean-close persistence, **not** power-cut recovery or in-place
filesystem service resurrection.
Config's authority checks are not bypassed or replaced; this application is
not exposed as Config and has manifest-requested Clock/filesystem endpoints,
with the filesystem scope restricted to its disposable test directory.

The same `io_workload` module used by the Linux backend comparison also runs
here against MemoryIO and the native filesystem: sequential/random reads, changing overwrites, two-segment
vectored writes and write-plus-flush, with bounded batches and content checks.
This validates the shared workload on native Rust/std/allocator and filesystem
facilities, **not a performance target or power-loss durability**. Its `Instant` implementation
uses the kernel's millisecond clock; do not use these timings for
microsecond performance claims. Native FS timings come from `bench-storage`.

## Runtime boundary

Unified-runtime validation (2026-09-25): the initial native KVM probe and then
the expanded four-thread File probe pass, including independent SQLite/ext2
checks. Logs: `/tmp/cubit-unified-std-probe-fixed.log` and
`/tmp/cubit-unified-std-threaded.log`. These are native tests, not hosted model
results or measurements of parallel SQL throughput. The allocator's native
backing cfg now covers `os=cubit` as well as existing no-std `os=none` apps;
selecting hosted System backing for the new target would recurse through the
linked allocation hook. No allocator metadata algorithm was changed.

The Nix-pinned Rust sources are copied into an ignored generated directory
and patched locally; no Cargo registry or Nix store sources are modified.
Cargo's private `__CARGO_TESTS_ONLY_SRC_ROOT` and bootstrap build-std switches
make this an experimental platform port, not a stable Rust target distribution.
Both this probe and `config-storage.svc` build through
`userspace/rust/std/cargo-cubit.sh` using `x86_64-unknown-cubit.json`.
The obsolete probe-specific std patch, modules, builder and target were removed;
there is no alternate single-threaded std build path. Shared hooks in
`userspace/services/config-storage/std_hooks.rs` preserve these requirements:

* `System` allocation uses the existing CuBit Rust/Ada bounded allocator and
  kernel heap growth, not libc, a static payload BSS, or a new allocator.
* Monotonic time uses the kernel millisecond clock; wall time calls the
  manifest-granted Clock endpoint. Neither is a microsecond benchmark clock.
* Entropy uses checked RDRAND through getrandom; absence/failure is fatal, not
  a predictable fallback. This is an explicit experimental hardware trust
  assumption, not CuBit's eventual audited system-entropy service.
* Thread creation, thread-local storage and synchronization use the shared
  CuBit std runtime. The probe checks main/child thread-local independence.
  Four threads share one native File for two rounds of writes larger than the
  grant capacity, exact readback and 16 callbacks. Write callbacks re-enter
  `size`, checking release-before-callback ordering. The dedicated test file
  avoids changing the scalar I/O benchmark's dimensions. This exercises real
  CuBit thread/futex/IPC paths, not a shared SQL connection or parallel device
  queue depth. The adapter still serializes complete operations.
* Ordinary std file/network/process facilities remain unsupported. There is
  no POSIX layer, ambient stdio, environment configuration, or dynamic loader.
  Panic diagnostics use CuBit's debug route separately from ordinary stdout.
* The native target uses an SSE2 userspace ABI, not the upstream bare-metal
  soft-float kernel ABI. CuBit already eagerly saves/restores FXSAVE state.
  AVX is not enabled; the probe rejects AVX/AVX-512 runtime detection. Extending
  register-state support is separate kernel work. All Rust dependencies and
  std are rebuilt together for this target; do not mix prebuilt soft-float
  Rust libraries into it. The Ada bridge passes scalar/pointer arguments only.

Turso's pinned no-fs configuration does not compile, so `fs` enables its I/O
abstractions. It does **not** grant access to CuBit files. `turso.patch` makes
dynamic extension loading explicitly fail on this target. Database I/O is
injected via `Store::open_with_io`; tests use MemoryIO and NativeIO. Upstream's
generic std filesystem backend is not a suitable native authority/locking
adapter and cannot open files through this std port.

Pure-Rust crypto is selected; optional C SIMD/mimalloc/FTS are not. The native
executable links the Rust archive, Ada allocator/storage bridge/runtime and generated ELF manifest,
without a C runtime. This does not constitute a complete dependency security
or license audit. Upstream unsafe Rust remains unproved.

The executable is about 20 MiB after stripping debug sections (about 89 MiB with
debug information). `build.sh` retains `turso-native-probe.debug.app` for address
diagnosis and stages the smaller ELF. Two simultaneously retained databases
can exhaust the bounded allocator's large arena; the test explicitly releases
the first database before opening the next. Do not silently enlarge bounds to
hide ownership mistakes. Memory quotas/cache tuning need further measurement.

## Required before persistent Config

1. The synchronous native Turso I/O adapter now uses **exclusively owned,
   scoped** CuBit file handles and checked grants, with real position/size/
   truncate/flush semantics and an explicit `pwritev` implementation.
   The pinned upstream default `File::pwritev` fails to complete its parent
   when a child completion reports an error; MemoryIO/UnixIO override it.
   The hosted fault adapter also overrides it. Do not inherit that default
   in the native backend. No fake locking or successful no-op sync.
   The filesystem now supplies `OPEN_DENY_SHARING` on write-authorized handles,
   backed by a SPARK-proved ownership model and native conflict/close tests.
   It is distinct from create-if-absent. Database and WAL ownership are tested;
   parent-namespace protection and trusted cleanup after app death remain integration requirements;
   see [the contract](../../../docs/filesystem-exclusive-ownership.md).
2. End-to-end device flush now exists through FS/ext2/NVMe and passes native
   `storage-grants`. Unsupported media, including RAM, return an explicit
   durability-unsupported result. NVMe timeout/wrong-completion retires its I/O
   queue until reset, preventing late-command acknowledgement/reuse. This is
   tested success/authorization behavior, not a formal driver proof or tested
   timeout fault injection. ATA does not yet offer this guarantee.
3. Ext2 now exposes checked nonzero resize through authorized file handles,
   including double-indirect allocation/reclamation, coherent aliases, and
   quarantine after uncertain mutation. Hosted fault injection, Linux content/
   fsck round-trips and native IPC tests pass. Ext2 metadata still lacks
   crash-atomic allocation updates. A device flush alone is **not** sufficient
   evidence for Config power-loss durability. Test allocation/metadata crashes,
   torn writes, full disk, failed flush, service death and restart ownership.
4. Current devmgr starts/waits for filesystem before Config, then starts
   procmgr later. Keep validated system.ccl seeds independent of persistent
   attachment; service readiness alone does not ensure backing-media availability.
   The backing location must come from the boot plan,
   not a mutable key inside the database. Reconcile saved intent with seeds
   explicitly; do not replay stored authority or resurrect live handles.
5. A narrow owned-value Ada/Rust boundary behind Config's existing policy
   checks. Keep cached reads out of the disk path; acknowledge durable revision
   commits separately from live activation. Bound history and total storage.

The shared Store now enters `RecoveryRequired` after a failed transaction
boundary or checkpoint I/O. Subsequent read/write/checkpoint calls fail before
using the connection. `data_sync_retry=ON` is explicitly selected because this
pinned engine otherwise panics on sync failure; the adapter **does not retry**.
Hosted fault injection covers write failure, flush failure, no further adapter
I/O, and fresh-connection recovery to a complete old/new revision. It models
completion failures over MemoryIO, not torn sectors or actual power loss.

No new SPARK proofs are claimed for Rust std, Turso, the FFI, or the storage
drivers. Existing pure Config model proofs remain a separate assurance boundary.

## Typed Ada worker probe

The native application now also links `Config_Native_Probe`, which drives the
actual Config_Objects publication model, Config_Worker executor, shared CCL
codec and direct Config_Database Ada/Rust ABI. Trusted startup opens the Store
over the same scoped NativeIO, then exclusively lends a Rust Database pointer
for the call. No pointer arrives from client IPC, and Ada retains none.

The seed boot stores CCL integer 41 in `org.cubit.publication/test`; the second
boot must load that exact value at revision 1 before committing 42 at revision
2. It then closes/reopens again for read-only verification. Explicit checks
remain enabled without `-gnata`. The disk oracle checks both this exact CBOR
history and the original scalar profile, and the headless test requires the
new `Ada typed worker publication PASS (filesystem)` marker.

Validated 2026-09-24 on the thread-split kernel: both independent TCG boots,
exact CCL revision history, original scalar-profile history and pre/post ext2
checks pass. The normal seed executable is restored after the run. Artifacts:
`/tmp/cubit-config-worker-native-20260924/{seed,reopen}/`; combined log:
`/tmp/cubit-config-worker-native-20260924.log`.

This is native component integration, not a running persistent `config.svc`.
See the [integration checklist](../../../docs/config-worker-integration-checklist.md)
for remaining launch, authority, lifetime and asynchronous-dispatch gates.

## Optional NVMe wait attribution

Under Nix and the shared build lock:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash \
  tests/config-turso/native/run-nvme-profile.sh /tmp/cubit-nvme-profile-new
```

This enables `CUBIT_NVME_IO_PROFILE=on`, runs the existing real-CuBit Store/SQL
profile, independently checks the SQLite/ext2 result, and reports NVMe command
counts, wait ticks, exhausted spin budgets and sleep calls. Three diagnostic
lines are printed every 64 device flushes. The first interval includes boot
work, the incomplete tail is omitted, and the boundaries differ from the timed
SQL loop. Do not subtract one profile's totals from the other. Waiting includes
guest descheduling; it is not a device-only latency measurement. Serial output
perturbs the diagnostic run, so production latency must be measured separately.

The driver diagnostics are removed by preprocessing in the default `off` build,
not merely disabled at runtime. The wrapper restores the ordinary NVMe driver,
initrd/ISO and uninstrumented Turso probe on exit. It does not modify the user's
base disk. Run the public Config benchmark separately for client-visible timing.
The strict parser has malformed/truncated-report tests in `test-nvme-waits.py`.
