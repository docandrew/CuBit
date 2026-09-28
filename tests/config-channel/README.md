# Nonblocking typed Config worker channel

```sh
nix develop -c bash tests/config-channel/run.sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c \
  'cd kernel && alr exec -- gprbuild -p -P ../tests/config-channel/native.gpr'
```

`Config_Worker_Channel` uses existing endpoint submission/completion and
generation-checked grants. It has no polling/waiting loop. A limited,
stable-address channel owns an eight-page loan (a durable type frame, a
seven-page provisioning image, or a five-page value frame prefix) plus private request
and response snapshots. No CBOR/client serialization occurs here; the security
snapshots are copies, not a zero-copy claim.

The dispatcher authorizes namespace/context and supplies a trusted CCL binding
before Submit. Tokens must come from its process-wide nonreusing allocator;
the channel additionally enforces increasing tokens, including queue rejection.

Worker schema provisioning compares complete nominal roots with
`CCL.Objects.Same_Schema`, not whole process-local registry records. Equivalent
roots with unrelated declarations receive the existing idempotent acknowledgment
without another slot or replacement; conflicting keys/shapes still fail. The
hosted receiver suite includes this case (86 scenarios as of 2026-09-25).
Pending and unconsumed-result states prevent overlapping operations/early reuse.
It does not mint authority or manage Config handles. It transports durable
type operations to the same private worker, alongside existing value operations.

`Exchange_Type` (0x0617) carries a native `Config_Schema_Protocol.Frame` for
Create/Recover. Its distinct `Type_Ready` acknowledgment cannot be confused
with value or provisioning completion. The header binds the operation to a
namespace, context, session and nonreusing token; the payload is native schema
metadata, not CBOR. Only successful recovery returns metadata. Creating a type
does not invent a value or grant access to its namespace.

The metadata executor shares receiver failure state with value operations.
Snapshots are released before invoking the database; a failed response mapping
retires both paths. Durable metadata does not populate the live provisioning
catalog by itself: Config must validate/approve the recovered binding and then
provision it before value traffic. Public client Create and Config startup
catalog recovery are still the next integration layer.

Current evidence: 305 protocol/executor checks, 680 type-channel checks,
42 existing channel scenarios and 85 receiver scenarios (including metadata
authentication, malformed frames, lost mappings and mutation during I/O).
The focused metadata protocol/executor proof discharges 50 checks, including
the concrete generic instance. Raw mapping lifetime and kernel completion
authentication are assumptions of this hosted model, not SPARK-proved here.

The real Linux-hosted Turso test adds 108 checks: create, duplicate/conflict,
reopen, absence, lost response and subsequent recovery without retrying. SQLite
independently verifies exactly two canonical declarations and zero values or
revisions. Run `bash tests/ccl-objects/run-durable-turso.sh` inside Nix. Native
worker/channel compilation is checked separately; this hosted result is not a
live client Create request to Config.

`Provision_Schema` (0x0615) uses the same channel and token domain, with a distinct
canonical Schema_Ready acknowledgment. A frame acknowledgment cannot finish
provisioning, nor can a schema acknowledgment stand in for a durable receipt.
`Config_Object_Service.Restore` provisions its approved binding asynchronously
before submitting the retained load. Provisioning gets the earlier token;
completion triggers the load once. No main-loop wait or polling interval is added.

The private `Exchange_Frame` envelope (0x0610) carries grant slot, generation,
exact frame size and version. `Frame_Ready` acknowledges transport, not storage
success: the validated frame holds the typed outcome. `Config_Worker_Receiver`
authenticates kernel-stamped source/tag using a trusted launch-shell callback,
checks the envelope and acquires the grant via a held owner endpoint. The kernel
derives the expected grant owner from that endpoint, not a supplied PID. It
snapshots and releases the request mapping before calling the database, then
reacquires the same grant generation to deliver the response. No borrowed
mapping reaches Rust or remains acquired across filesystem I/O.

Revocation after accepting a snapshot prevents response delivery, not an
already accepted commit. Failed acquisition of the response or failed return
of a mapping permanently retires that receiver. A replacement must reopen and
recover; there is no blind retry or reset method. Caller authentication and
schema approval come from the trusted service shell, not from frame contents.
The receiver no longer accepts an out-of-band Contract parameter. Its private
bounded catalog is populated only through authenticated provisioning IPC, after
snapshot/release and metadata validation. Identical definitions are idempotent;
same-key/different-definition requests fail, as do unknown schemas on data
requests. A full catalog does not evict existing definitions or widen trust.

Complete accepts ONLY entries from the kernel completion queue. Kernel reply
authority identifies the peer/request; the token only correlates work. Never
feed ordinary incoming messages into it. Errors, malformed replies and uncertain
backend outcomes stop further submissions. They do not establish that a commit
did not happen. Config_Objects handles recovery and cache publication.

Returned bytes are snapshotted before checking against the retained request and
binding; later loan mutation cannot alter the result. Retire is terminal even
while work is pending, not cancellation/rollback. Revoke acceptance alone does
not permit destruction: wait for confirmed grant retirement.

42 hosted channel scenarios cover success/load/absence/conflict/rejection, late/wrong/
duplicate completions, malformed fields, target death/cancellation/queue overflow,
copy isolation, busy results, queue-rejection token reuse, grant creation failure
and delayed/failed revocation, provisioning validation and cross-operation
acknowledgment rejection. Existing storage-channel tests still pass all
36 scenarios after the shared grant fixture gained distinct loan/transfer sizes.

47 receiver/channel scenarios cover denied sender/tag, malformed
envelopes/native objects, acquisition and return failures, malformed/uncertain
backend output, owned-snapshot isolation during storage, and refusal to call
storage again after retirement, unknown types, schema conflicts, idempotent
provisioning, capacity and failed schema-grant return. The shared fixture models syscalls; resetting
it between scenarios is not a proof of kernel cleanup on failed return.

The real hosted Ada/Rust Turso publication test now runs through both channel
and receiver. It commits revision 2, denies response acquisition, closes and
reopens the database, and recovers without retry. Independent SQLite inspection
requires exactly two revisions. Run `tests/ccl-objects/run-durable-turso.sh`
inside Nix for that test; actual storage is real, IPC/grants are modeled.

Native library compilation, including the provisioning-aware receiver and the
native Config service/client instances against the actual runtime, passes.
Live Config and the worker receive loop are
not attached yet. These syscall-model tests do not prove kernel authentication,
concurrency/lifetime safety or end-to-end persistence. Protocol/value proofs are
separate from this unproved syscall adapter.
