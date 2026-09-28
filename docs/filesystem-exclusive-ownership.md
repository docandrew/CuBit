# Exclusive file ownership

`OPEN_DENY_SHARING` requests a lifetime-exclusive ordinary-file handle. It is
not authority, a POSIX advisory lock, or the existing `OPEN_EXCLUSIVE` flag
(which means create-if-absent). The service first checks the caller's existing
path authority, then admits the requested sharing mode by volume/inode identity.

## Implemented contract

- Deny-sharing requires a write-capable open; a read-only caller cannot reserve
  a file against its writers. Existing authority checks still apply.
- An exclusive open fails if any handle already references that inode.
- While the exclusive handle lives, every additional open fails, including
  read-only opens and requests by the same process. No in-place upgrade exists.
- A conflict returns `REPLY_SHARING_VIOLATION` without attaching a handle or
  truncating the file. Create-if-absent can independently return already-exists.
- Renaming the held inode is rejected, including by its owner. Authorization
  is checked before inspecting ownership. Other rename errors retain their
  existing classification; failed metadata reads never allow a mutation.
- Close, authenticated access-profile replacement/revocation, or existing
  recovery-required handle retirement detaches ownership. Generation checking
  prevents a stale close from releasing a replacement handle's hold.
- Ordinary shared opens remain the default. Exclusivity adds bounded open/
  rename admission work, not lock messages or checks on each read/write.

The implementation uses the existing `Shared_Objects` table. Its `Sharing_Mode`
enum is attached to an internal handle slot; it is not a client-supplied owner
identity. The filesystem remains a single serialized dispatcher. This table is
not a substitute for synchronization if dispatch or storage becomes concurrent.

## Proof and tests

The production generic has Ghost predicates for exclusive-owner isolation and
a default-initialization contract. In the hosted model instantiation, SPARK
proves successful exclusive attachment is isolated by object identity, conflicts
leave state unchanged, and attach/replace/detach preserve established isolation.
The proof includes initialization and runtime safety. It does not prove native
PID authentication, handle-table coupling, IPC, disk contents or lifecycle events.

Hosted tests cover 3,968 ordered handle-slot/mode scenarios, volume separation,
close/reuse and metadata preservation; protocol tests exhaust 8,192 option words.
The native storage test checks same-process reopen/truncate conflicts, rejected
rename, contents/flush, close/reacquire, stale-close rejection and ordinary
sharing after release. Its success marker is `FILE-EXCLUSIVE-CHECK: PASS`.

## Turso integration boundary

This is a prerequisite for the native adapter, not yet a database-wide lease.
The adapter must acquire the database handle before invoking the engine, keep
it alive for the connection's lifetime, and independently protect sidecar files.
It must report real errors rather than fake successful lock/unlock operations.
A request to downgrade cannot silently discard the backing-file hold.

Parent-directory names are not pinned by a file handle: renaming an ancestor
is outside this contract. Database and sidecar namespace authority must therefore
be restricted to the owning service before relying on path-based reopen. This
does not grant Config clients access to its backing files.

An app crash does not yet reliably notify the filesystem. Until authenticated
cleanup, its handles may continue blocking acquisition. That is intentionally
fail-closed; do not steal a hold based on a timeout, PID guess or client claim.
Procman resets filesystem policy/handles before resuming a reused PID. Prompt
crash cleanup still needs a trusted process-lifetime event or broker action.
Filesystem-service restart and mutable/removable volume generations need explicit
recovery too; these holds are not persistent and old handles must not be replayed.

The persistent Turso adapter remains pending. Its next pieces are checked native
grant lifetime, exact positioned/vectored completion, size/resize and durable
flush, exercised with the shared I/O workload. Ext2 journaling/power-loss atomicity
and Config adoption remain separate gates.
