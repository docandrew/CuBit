# Native Turso bring-up

This runs the real Rust Turso engine **inside CuBit**, not Linux. It is an
isolated test application, **not a persistent replacement for `config.svc`**.
Normal Config, desktop images, and the existing no_std Rust workspace are not
switched to this runtime.

## Reproduce

Build the normal kernel/services/disk first if this is a fresh checkout:

```sh
nix develop -c make -C kernel world
nix develop -c bash tests/config-turso/native/build.sh --features turso
nix develop -c bash tests/headless/run.sh --test turso-native \
  --accel tcg,thread=multi --timeout 60 --serial /tmp/turso-native.log --keep-logs
```

KVM may replace TCG when available. The tested guest has 128 MiB and four CPUs;
the application itself executes on one thread. The runner copies its executable
and Clock into a disposable test disk. It does not install a new Config backend
in the interactive desktop. Boot tests share staging: run them sequentially.

For the smaller standard-library probe, omit `--features turso` and select
`--test turso-native-std`. The larger test deliberately rejects an executable
built without its SQL/Config checks by requiring additional serial markers.

Validated on 2026-09-23, Nix / QEMU TCG:

```text
TURSO-NATIVE: std probe PASS
TURSO-NATIVE: volatile SQL transaction PASS
TURSO-NATIVE: typed Config CBOR/revision/reopen PASS (volatile)
TURSO-NATIVE: shared File workload PASS (volatile)
```

The checks exercise the actual shared `Store` and CBOR codec: string, integer,
boolean, a complete revision, stale-revision conflict, and reopen with the same
MemoryIO device. This is **not** persistence across process exit or reboot.
Config's authority checks are not bypassed or replaced; this application is
not exposed as Config and has only its manifest-requested Clock endpoint.

The same `io_workload` module used by the Linux backend comparison also runs
here against MemoryIO: sequential/random reads, changing overwrites, two-segment
vectored writes and write-plus-flush, with bounded batches and content checks.
This validates the shared workload on native Rust/std/allocator facilities,
**not native filesystem performance or durability**. Its `Instant` implementation
uses the Clock service's millisecond interface; do not use these timings for
microsecond performance claims. Native FS timings come from `bench-storage`.

## Runtime boundary

The Nix-pinned Rust 1.98.1 sources are copied into an ignored generated directory
and patched locally; no Cargo registry or Nix store sources are modified.
Cargo's private `__CARGO_TESTS_ONLY_SRC_ROOT` and bootstrap build-std switches
make this an experimental platform port, not a stable Rust target distribution.
`std.patch` plus the small modules in `std/` are the whole platform adaptation:

* `System` allocation uses the existing CuBit Rust/Ada bounded allocator and
  kernel heap growth, not libc, a static payload BSS, or a new allocator.
* Monotonic and wall time call the manifest-granted Clock endpoint. This is a
  synchronous IPC clock bridge, not an optimized shared-clock fast path.
* Entropy uses checked RDRAND through getrandom; absence/failure is fatal, not
  a predictable fallback. This is an explicit experimental hardware trust
  assumption, not CuBit's eventual audited system-entropy service.
* TLS uses Rust's single-thread implementation. Thread creation fails. This
  port is **not valid for a multithreaded process**, and does not establish
  Turso's concurrent safety. Process exit reclaims TLS allocations.
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
injected via `Store::open_with_io`; it currently receives MemoryIO. Upstream's
generic std filesystem backend is not a suitable native authority/locking
adapter and cannot open files through this std port.

Pure-Rust crypto is selected; optional C SIMD/mimalloc/FTS are not. The native
executable links the Rust archive, Ada allocator and generated ELF manifest,
without a C runtime. This does not constitute a complete dependency security
or license audit. Upstream unsafe Rust remains unproved.

The executable is about 20 MiB after stripping debug sections (about 89 MiB with
debug information). `build.sh` retains `turso-native-probe.debug.app` for address
diagnosis and stages the smaller ELF. Two simultaneously retained databases
can exhaust the bounded allocator's large arena; the test explicitly releases
the first database before opening the next. Do not silently enlarge bounds to
hide ownership mistakes. Memory quotas/cache tuning need further measurement.

## Required before persistent Config

1. A native Turso I/O adapter using **exclusively owned, scoped** CuBit file
   handles and checked grants. Supply real position/size/truncate semantics,
   exact short-I/O/error completion, and an explicit `pwritev` implementation.
   The pinned upstream default `File::pwritev` fails to complete its parent
   when a child completion reports an error; MemoryIO/UnixIO override it.
   The hosted fault adapter also overrides it. Do not inherit that default
   in the native backend. No fake locking or successful no-op sync.
2. End-to-end device flush now exists through FS/ext2/NVMe and passes native
   `storage-grants`. Unsupported media, including RAM, return an explicit
   durability-unsupported result. NVMe timeout/wrong-completion retires its I/O
   queue until reset, preventing late-command acknowledgement/reuse. This is
   tested success/authorization behavior, not a formal driver proof or tested
   timeout fault injection. ATA does not yet offer this guarantee.
3. Ext2's existing internal truncate ignores some I/O errors and is not ready
   to expose as the database's truncate contract. Ext2 metadata also lacks
   crash-atomic allocation updates. A device flush alone is **not** sufficient
   evidence for Config power-loss durability. Test allocation/metadata crashes,
   torn writes, full disk, failed flush, service death and restart ownership.
4. Config starts before the ordinary filesystem. Keep validated system.ccl
   seeds available at boot, then explicitly attach an authorized backing store
   once storage is ready. The backing location must come from the boot plan,
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
