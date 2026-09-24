# Turso Config backend experiment

This is a working **storage evaluation**, not a dependency of `config.svc`, a
production native backend, a policy engine, or an importer for untrusted databases.
No hosted Turso account, network relay, or cloud service is involved.

The main workspace below is **Linux hosted**. An isolated
[native CuBit probe](native/README.md) now runs the same typed Config/CBOR
adapter on real Turso with volatile MemoryIO. Persistent native Config is not
enabled; the probe documents the runtime port and remaining storage requirements.

```sh
nix develop -c bash tests/config-turso/run.sh
nix develop -c bash tests/config-turso/run.sh --bench
```

The separate Cargo workspace pins `turso_core = 0.8.0-pre.12` and checks in its
lockfile. It uses the repository's Nix Rust toolchain; the first run downloads
the locked crates. It does not add this dependency closure to CuBit's native
freestanding workspace or the general development shell. Generated databases,
dependency inventories, and measurement logs are ignored under `results/`.

## What works

* Bounded string/integer/boolean values, independently named collections and
  profiles, immutable historical revisions, and expected-revision updates.
* Each revision stores one deterministic CBOR profile and its payload-schema
  identity, rather than an SQL row per setting. SQL is only the transaction and
  indexing layer; it does not define the future CCL object model.
* A whole profile revision plus its new head is one transaction. Rollback to old
  settings creates a new revision; it never rewinds the revision counter.
* SQL stays private to the adapter. Names/values are parameter-bound, including
  Unicode and SQL-looking text. These names are **not authenticated ownership**.
* Tests cover stale revisions across two connections, size/duplicate rejection,
  failure partway through a transaction, invalid stored types, unknown format
  versions, and close/reopen.
* Injected write/flush completion failures retire the adapter connection:
  subsequent reads, writes and checkpoints require recovery. A new connection
  recovers a complete old/new revision. This uses MemoryIO, not a disk-failure
  simulator. A failed commit acknowledgement is never blindly retried.
* Four process-exit fixtures skip destructors after revision insertion, payload
  replacement, head replacement, and successful COMMIT. Recovery retains
  the previous complete revision or the committed complete revision as expected.
* A checkpointed, closed `.sqlite` file copied **without its WAL** opens in
  Python's independent SQLite reader, passes its integrity check, and reopens in
  Turso with the expected settings. This is a compatibility fixture, not a
  scoped export/import product. Do not copy an arbitrary live database file.

Process exit is not power loss: the
host page cache survives. We have not injected torn writes, failed flushes,
power loss, or disk exhaustion on a real disk. Nothing here is SPARK-proved.
On 2026-09-23, all 13 Rust tests, the shared Ada/Rust CBOR fixture, formatting
check and dependency-feature inventory passed through `nix develop`.

## Experimental CBOR payload

`payload.rs` deliberately supports only the existing scalar profile. It is not
a general-purpose CCL serializer. The envelope is:

```text
[1, payload-schema-sha256 : bytes32, {setting-name : text | i64 | bool, ...}]
```

The schema identity hashes the exact, newline-free UTF-8 `SCHEMA` descriptor in
that module. It identifies the experimental codec, **not** a publisher, an
authority, a complete per-application schema, or an existing CCL interface.
The SQL `schema_version` is application metadata; this prototype does not yet
validate it against a registered application schema or run migrations.

All containers are definite-length; lengths and integers use their shortest
encoding. Map keys sort by byte length, then bytes. Names are nonempty ASCII
dot-separated components of letters/digits/underscore/hyphen, at most 128 bytes.
Profiles contain at most 256 entries; text values are at most 4096 UTF-8 bytes.
The decoder checks total size before parsing and map count before allocating.
It rejects duplicate/out-of-order keys, invalid UTF-8, out-of-range integers,
unknown schemas/versions, tags, floats, nested values and trailing data.
Re-encoding verifies canonical bytes; this bounded extra copy is intentional
for the experiment, not an optimized native read path.

There is no deserialization of executable code, authority or live handles.
Future records/variants/collections must come from the shared CCL persistable
type model described in the [roadmap](../../docs/ccl-unified-documents-roadmap.md),
not grow here as unrelated Config-only types.

`fixtures/scalar-profile.hex` is shared by Rust and a hosted Ada test using the
existing Nix-pinned `cbor_ada` library. Both encode/decode the same scalar fixture.
This is cross-language regression evidence, not a full Ada profile validator or
a proof of this Rust adapter. Upstream `Decode_All_Strict` has a 128-item tree
bound, so the fixture does not establish that API supports a maximum-size
256-entry Config profile; a native codec needs bounded streaming or a deliberately
sized owned representation. The existing CCL wire profile is not changed.

Database format **2** removes the prototype `settings` table and little-endian
scalar blobs. Format 1 is rejected; these disposable hosted fixtures have no
migration or compatibility path. No live CuBit files or native ABI are changed.

## Measurements

Nix release build, O2, full synchronization (also checked by a test), 20 entries
per profile, 1000 complete revisions with history, 5000 full-profile reads.
Statements are prepared per operation; these are not optimized cached reads.

| Operation | Median | p95 | p99 |
| --- | ---: | ---: | ---: |
| Read a complete 20-setting profile | 117 µs | 159 µs | 205 µs |
| Commit a complete revision with history | 3,829 µs | 6,126 µs | 8,051 µs |

These first numbers used format 1 (one SQL row per setting), before CBOR.
Initial open/schema setup was 25.6 ms; the checkpointed database with all history
was 3,248,128 bytes. These are one host run, not CuBit IPC numbers, scheduler
guarantees, or a comparison with another database. The benchmark creates an
exclusive new output file and never overwrites a previous database.

Format 2 (one CBOR payload per revision), same benchmark on 2026-09-23:

| Operation | Median | p95 | p99 |
| --- | ---: | ---: | ---: |
| Read/decode a complete 20-setting profile | 86 µs | 118 µs | 138 µs |
| Commit a complete revision with history | 3,492 µs | 4,073 µs | 7,506 µs |

Initial open/schema setup was 25.2 ms; the checkpointed file was 2,109,440 bytes.
The run is recorded in ignored `results/run.HABeQR/latency.txt`. This is a
before/after observation, not controlled performance attribution: host load and
storage latency can vary. Whole-profile reads perform less SQL work despite
CBOR decoding and canonical re-encoding. Durable writes are still millisecond
scale; the intended native design must cache owned live values and keep durable
commit acknowledgement separate from routine reads/UI painting.

The example executable's text+data+BSS totals about 17.2 MB, excluding debug
sections; its debug-bearing file is about 96 MiB. This is a significant backend,
not a tiny settings parser. Retention and aggregate quotas are not implemented.

## I/O backend comparison

```sh
nix develop -c bash tests/config-turso/run-benchmarks.sh --repeats 3
# Explicit alternative if the host disallows io_uring:
nix develop -c bash tests/config-turso/run-benchmarks.sh --syscall-only
```

This builds the optional `linux-uring` feature, uses fresh private directories,
alternates backend order, and records the toolchain, binary/lockfile hashes,
CPU/affinity, filesystem and complete results in ignored `results/io.*/`.
An unavailable or failing io_uring backend is an error, never a silent fallback;
the runner does not alter host security settings. The normal test build and
native CuBit dependency do not enable io_uring.

Both backends execute the same two levels of workload:

* Real SQL/Config operations: 1000 measured FULL-sync commits and 5000 complete
  profile reads after 32 warmup revisions, then checkpoint/close/reopen checks.
* Shared `io_workload` through Turso's `File`/`IO` traits: 4 KiB and 64 KiB
  sequential/random reads, changing overwrites and two-buffer vectored writes
  at batch depths 1/8/32, plus serial write-then-flush. Each phase has 1024
  measured operations after two warmup batches, a 4 MiB initialized working
  set, exact completion-length checks and content verification. Write phases
  reinitialize outside timing and verify changed bytes after timing.

These are **buffered, warm-cache** measurements, not direct/cold storage tests.
Raw buffers are ordinary heap buffers, not registered io_uring buffers. Depth
is the bounded batch size: UnixIO completes synchronously and cannot obtain
device concurrency by increasing it. `peak_deferred` counts unfinished
completions observed after submission, **not hardware queue depth**; SQL reports
`na` because that concurrency is not instrumented.

Request latency includes submission/completion overhead. Write-then-flush also
includes flush acknowledgement. Batch elapsed includes submission and draining
but excludes allocation, preparation, verification and gaps between batches.
Consequently the reported **active-phase MiB/s is not sustained end-to-end
throughput**. No logging occurs inside a timed phase. Warmups, generated data
and strict checks are shared; timer resolution is backend/platform dependent.

`summarize-benchmarks.py` rejects incomplete runs, missing/duplicate phases,
bad sample counts and incompatible settings. It reports the median of each
run's percentiles and the run-to-run p99 range, not pooled percentiles. Its
negative fixtures run with `run.sh`.

The native probe runs this same workload against **MemoryIO**, validating
native execution but not filesystem performance. The separate native
[`bench-storage`](../performance/README.md) measures real FS IPC, including
write-plus-NVMe-flush. These are not yet like-for-like Linux/CuBit benchmarks.
See [I/O findings and next steps](io-findings.md).

## Dependency and porting findings

The [upstream core](https://github.com/tursodatabase/turso/tree/main/core) is
MIT-licensed; the resolved dependency closure has its own licenses. `audit.py`
inventories declared licenses and selected features in `results/dependencies.md`.
This is not a completed legal or supply-chain audit. The CBOR experiment adds
`minicbor = 2.3.0` (`std`, no derive) and `sha2 = 0.10.9`; these now also run in
the isolated native probe, not as replacements for the existing Ada CBOR library.

Defaults are disabled; selected features are `fs`, `uuid`, `json`, and
`pure-rust-crypto`. Without UUID and JSON this pinned release fails to compile:
incremental code still references UUID and no-JSON branches have error-type
mismatches. The hosted build applies no upstream patches or warning suppressions;
the native probe has the explicitly documented dynamic-loader patch.
Optional mimalloc, SIMD C code, full-text search, and networking SDK bindings are
not selected. The AEGIS build dependency still includes the Rust `cc` helper,
but its build script returns before C compilation with `pure-rust` enabled.
Linux `std`/libc remain host dependencies; this is not a C-free host executable.

The original native check with `--target x86_64-unknown-none` failed first in `getrandom`
because that target has no supported entropy backend. Source inspection also
shows substantial `std` dependencies; CuBit currently exposes `core`/`alloc`
and native Rust bindings. The isolated probe now supplies a limited single-thread
std port without changing that normal workspace. Adding an entropy hook alone
was not sufficient; see its runtime assumptions and limitations.

The [I/O interface](https://github.com/tursodatabase/turso/blob/main/core/io/mod.rs)
offers caller-driven completion, reads/writes, sync, truncate, size and locks.
A native adapter must provide real semantics for these through scoped CuBit
filesystem authority. Filesystem IPC now has a flush operation backed by NVMe;
RAM and unsupported backends explicitly reject durability. Arbitrary file-size
truncate and exclusive database ownership still need sound native contracts.
Rename/flush success is not an ext2 power-loss consistency guarantee. Do not
implement fake sync/lock success to get a demo.

## Before adoption

1. Establish a scoped native storage contract: position-based I/O or exclusively
   owned serialized handles, buffer/grant completion lifetime, size/truncate,
   meaningful flush/ordering, and exclusive database ownership. No other app
   should receive backing-file access just because it can read Config.
   Provision the backend location/handle through the declarative boot plan and
   trusted launch path, not a mutable `config.store` key inside the database.
2. Harden the experimental Rust `std` port, or work upstream on a smaller
   embedded engine surface. Account for locks, clocks, entropy, allocation,
   file identity, temporary storage, and dynamic loading; the current
   single-threaded native probe is not general-purpose Rust platform support.
3. Connect only through a narrow owned-value Ada/Rust boundary. Keep collection
   schemas, authenticated ownership, context-bound authority and activation in
   Config's model. This experiment deliberately does not duplicate those checks.
4. Stage validated updates; publish the live cached revision only after the
   chosen durability acknowledgement. An ambiguous commit failure needs recovery,
   not a blind retry. Reads and UI painting must not wait for disk sync.
5. Add authenticated provenance, approved migrations, bounded history/quotas,
   scoped snapshots, preview/rebinding on import, and storage fault injection.
   Format-v2 tables here are experimental, not a committed CuBit disk ABI.

**Recommendation:** keep Turso as a credible candidate and this reproducible
testbed. Do not enable native persistent Config until the runtime and storage
contracts exist. The native SPARK store can proceed independently meanwhile.
