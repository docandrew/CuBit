# Native Config storage worker

This executable runs Rust Turso inside CuBit over the native filesystem IPC
adapter. It is a single-owner storage worker, not a second public Config API.
There is no POSIX filesystem shim, ambient `std::fs`, or C implementation.

## Current integration

Trusted Stage 2 startup can select exactly one worker:

```lisp
(startup v1
  (start "clock.svc" (priority 5))
  (start "config-storage.svc" (priority 5) (role config-storage)))
```

The boot `system-config` supplies `cubit.config.storage.database`. The worker
reads it once from bootstrap Config before opening storage. The sample manifest
permits only `@nvme:0/system-config.sqlite` and its `-wal` file. Choosing a path
in Config does **not** grant access to it: the manifest-installed filesystem
policy still applies. The adapter owns an exact two-path allow-list as additional
narrowing. There is no default path or fallback volume.

After normal manifest/policy installation, procmgr mints a private endpoint
into Config's reserved slot 61 and sends the attachment control message. Only
the registered procmgr can attach it, and only once. Public spawn callers cannot
select this role; an ELF claiming the worker's application identity is not
enough. No global driver role or public worker discovery is added.

Config's event loop now drains native-object requests and authenticated worker
completions separately. Its existing byte settings/inspection API remains
available during storage work. Attachment acknowledges transport setup, **not**
database readiness; the worker logs `CONFIG-STORAGE: ready` after opening it.

The worker authenticates Config before acquiring any grant, accepts schema
provisioning only from that source, and copies/releases incoming mappings before
Turso I/O. Its saved reply slot 62 survives nested filesystem/clock IPC. Raw
database pointers stay inside this process, with one exclusively borrowed Rust
database for the whole Ada receive loop. A terminal failure exits without retrying
an uncertain write or checkpointing that session.

The shared filesystem transport owns a 64 KiB grant buffer, and Rust queries
its capacity rather than duplicating the bound. Large requests take fewer
serialized IPC round trips; this is still a copying, single-outstanding-I/O
adapter. It does not expose Rust heap pages, bypass scopes or remove flushes.
WAL header/page vectors are packed directly into that owned loan, with bounded
borrowed descriptors and no intermediate concatenated payload. Oversized vectors
split into sequential batches; a failed/short batch still retires the backend.
Native File-path, Store-level attribution, and public Config commit measurements
are distinguished in `tests/config-turso/io-findings.md`, with their limits.

Public `Config.create(type)` now routes through the native asynchronous channel:
authorize, persist the declaration, provision its approved type, restore any
value, then issue a handle under the still-current caller authority. Hosted
real-Turso tests exercise this full service path with modeled IPC. Native
compilation and the KVM/TCG public client regression pass, including independent
SQLite/WAL and read-only e2fsck validation of the declaration and two revisions.
The catalog starts empty and now recovers persisted objects on demand through
Open. Read-only callers need no Write authority and supply no type metadata:
the worker recovers the declaration, Config validates it against the requested
schema key, provisions/loads the value, then issues a fresh handle after checking
the caller's authority lifetime. Missing and schema-error replies also recheck
that lifetime. Two independent TCG boots and a KVM reopen regression pass,
including denied writes and independent SQLite/WAL/e2fsck checks. See
`tests/config-object-client/native-app/README.md`.

**Not complete:** eager startup enumeration, automatic worker replacement,
application/CCL bindings and legacy byte-setting migration. Ordinary desktop
profiles do not start this worker, and their byte settings are still volatile.

The database is now experimental format **3**, adding immutable, namespace/
context-scoped `object_types` declarations. Creating a declaration does not
invent a default value or increment a revision. Recovery imports portable CBOR
through the same Ada type/layout validator as native schema provisioning.
Missing metadata alongside existing revisions is an error, not an unset object.
Format-2 experimental databases are rejected; no silent migration, reseeding or
deletion is performed. Use a new test database when testing the new format.

## Build and test

The worker now shares `userspace/rust/std/cargo-cubit.sh` and the
`x86_64-unknown-cubit` target with other native Rust applications. Its strong
allocator, randomness and wall-clock hooks remain in `std_hooks.rs`; no
probe-specific std build or target remains. Native allocator backing recognizes
both the existing no-std target and the new CuBit std target, avoiding recursion
through std's System allocator. Database access remains explicit native IPC,
not ambient `std::fs` access. The worker itself is still single-owner.

Validated 2026-09-25: unified-runtime KVM public Config writer and independent
read-only reboot pass, including exact scalar/nested/discovered values, access
denials, equivalent-schema recreation and independent SQLite/WAL/ext2 checks.
No extra revisions are introduced by reopening. Logs:
`/tmp/cubit-unified-std-threaded.log` and `/tmp/cubit-unified-std-reopen.log`.
The normal desktop ISO was restored; desktop persistence is still opt-in.

From the repository root, with the usual kernel/services/disk already built:

```sh
flock --exclusive coordination/build.lock nix develop -c bash -c '
  make -C kernel config procmgr ccl-manifest &&
  bash userspace/services/config-storage/build.sh &&
  QEMU_CPU_MODEL=host bash tests/headless/run.sh --test config-storage \
    --accel kvm --timeout 40 --keep-logs'
```

For TCG omit `QEMU_CPU_MODEL=host` and use `--accel tcg,thread=multi`. The test
copies binaries into a disposable ext2 disk and uses a separate system-config
fixture. It requires attachment/ready markers, then checks the guest-created
database **and WAL** with Linux SQLite and runs read-only `e2fsck`. It checks a
quiescent initialization, not arbitrary power-loss recovery or typed IPC commits.
The `config-inspection` regression also checks that a normal client cannot
nominate itself as the backend and that failed-launch policy cleanup succeeds.

This transitional Rust std port requires working RDRAND and fails closed if
its provider rejects the CPU. On this AMD host, KVM with `-cpu Broadwell`
reported `AuthenticAMD` with an Intel model, triggering getrandom's old-AMD
family exclusion. KVM with `-cpu host` and TCG with Broadwell both passed; no
entropy check was disabled. General OS entropy integration remains future work.

The tested native filesystem bridge is shared under
`userspace/lib/storage/native/`. Until the general Rust std port owns its hooks,
`std_hooks.rs` is shared with the native Turso probe. The pinned Turso backend
and generated std sources still come from `tests/config-turso/`; moving that
backend to a production library is a separate packaging step.

## Limits

The Rust foreign main calls `config_storage_hostinit` exactly once before any
Ada export. This is GNAT's generated initializer for the standalone static
library, not a hand-maintained list of package-body calls. It initializes both
worker and native-storage dependencies (including protocol constants requiring
elaboration). Zero-filled BSS does not replace Ada elaboration. The public IPC
regression caught this distinction where the earlier direct-storage probes did
not. Worker authentication also checks the kernel's default PID authority tag,
not zero; its source predicate is shared with the hosted startup tests.

- One owning thread and one outstanding worker operation; no thread-safety claim.
- File I/O submits native async IPC, but the worker's Rust File adapter waits
  synchronously and copies through its owned grant page; not zero-copy/pipelined.
- SPARK proves focused pure message/store/codec properties, **not** this syscall,
  FFI, Turso or startup shell. Native boot and failure behavior are regressions.
- Persisted type keys are identifiers, not verified signatures or trusted hashes.
- Service-side sender-incarnation migration awaits the kernel ABI handoff.
- Ext2 is not journaled; clean reopen and a SQLite commit are not proof of
  power-fail atomicity for the entire storage stack.
