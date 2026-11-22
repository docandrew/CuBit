# Asynchronous native storage channel

`userspace/lib/storage/Storage_Channel` is the reusable Ada request layer now
used by the native Turso probe. These **Linux-hosted** tests compile that real
implementation and the real filesystem message constructors, replacing only
kernel submission/grant operations with controllable fixtures.

```sh
nix develop -c bash tests/storage-channel/run.sh
```

The 43 channel scenarios cover every operation, owned input copies, high-index output
strings, busy/unconsumed-result exclusion, stale/duplicate/invalid completions,
token non-reuse/exhaustion, definite admission denials, malformed successes,
oversized reads, target death/cancellation/queue failure, failed grant creation,
submission rejection, cleanup-only operation after poisoning, and retirement
while an operation is pending. The delayed-retirement case reproduces a subtle
kernel contract: repeated revoke can return false after the grant has become
inactive; the owned-generation retirement query remains authoritative.

Fixtures assert that exactly sixteen aligned pages (64 KiB) are granted.
Boundary cases exercise 1/4095/4096/4097/65535/65536-byte transfers, exact
content and byte counts, too-small output retries, untouched trailing bytes,
and oversized submission rejection. They delay completion
until explicitly driven. They are not a kernel authentication proof or a
device-failure simulator. No SPARK proof is claimed for this syscall/grant adapter.

The generic `Submit_With_Payload` shares the normal submission/admission state
machine, but lets a trusted in-process producer fill the owned buffer directly.
Tests assert the callback's address IS the grant address (no intermediate
payload), and that invalid, busy, unconsumed-result and retired states cannot
invoke it. Queue rejection still consumes its token. The callback must fill all
bytes and must not reenter, retain the view or perform IPC; this is not a public
client callback or an additional security authority.

`native_bridge_tests` adds 26 checks of the real native FFI implementation with
modeled syscalls: malformed/empty/oversized descriptor arrays, validation before
payload dereference, native header/page layout, owned copies, scalar/read
regressions and poisoned completion retirement. Caller pointer validity is an
FFI obligation, not a proved property. The native Rust tests exercise the actual
C-layout boundary in CuBit, with independently checked database output.

## Ownership and dispatch

One channel has one outstanding operation and owns one 64 KiB transfer buffer.
Initialize once with an **already-held endpoint capability**. The channel does
not discover a PID or mint authority. `Submit` invokes `capSubmit`, never
`capCall`. Input is copied before returning; caller buffers can then go away.
The limited channel object itself must stay at its original address.

The owning dispatcher drains the kernel completion queue, routes its entries,
and calls `Complete`. Do not turn this into a handler for client-supplied IPC
messages: a token, `from` field, or serialized completion record is not evidence
of authority. Kernel request tracking and the consumed reply capability provide
authentication. The channel never polls and discards another component's work.
Tokens must be unique for the process lifetime, including channel replacement;
use one dispatcher-owned allocator, not independently restarting counters.

`Take_Result` copies only a completed read's bytes. Until that call succeeds,
another operation cannot reuse the page. Unknown data/open outcomes poison the
channel; only explicit close cleanup remains available. `Retire` is terminal,
not cancellation or rollback. Its page may only be reclaimed after the kernel
confirms retirement. Failed or pending revocation does not license reuse.
This is single-dispatcher ownership, **not** a thread-safe shared object.

## Native integration and limits

The [native Turso probe](../config-turso/native/README.md) uses this exact channel
over real CuBit IPC. Its FFI wrapper remains synchronous for Turso: it owns the
completion queue and parks in `waitCompletion` between submit and completion.
Do not embed that blocking wrapper in Config's request dispatcher. A dedicated
worker may use it; integrating Turso's own event loop can remove that wait later.

The native Config storage worker also uses this channel; its authenticated
binding and typed collection recovery are tested separately. Existing Config
snapshot publication proofs do not prove this adapter. It is still a copying,
queue-depth-one path, not zero-copy or concurrent device I/O. Rust queries the
capacity from its Ada owner, so increasing this bound does not require a matching
hard-coded Rust chunk size. Larger transfers use the existing FS messages and
authorization checks, not a new privilege or unsafe direct Rust heap grant.
