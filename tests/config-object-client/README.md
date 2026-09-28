# Native-object Config client boundary

## Resource-bound collection client (2026-09-25)

`Config_Object_Client.Resources.Collection` owns the stable native client and
its exact CCL registry lease. It implements nonblocking Create/Get/Set/Close,
authenticated-completion dispatch, result consumption, retirement and cleanup.
Collection references are never integer handles, serialized objects, or inputs
to an API that can attach them to a different already-open client.

- Create checks the resource's value parameter against the collection contract
  in the registry's pinned type universe and reserves before submitting effects.
- Get/Set/Close require the exact live reference associated with that client.
  A second compatible collection's reference is insufficient.
- Stop or per-resource retirement invalidates script use immediately while
  keeping pending completions drainable. Late success cannot revive ownership.
- Cleanup closes known handles and waits for confirmed loan-grant retirement
  before reclaiming the registry lease and rearming the same stable storage.
  Its completion-token floor survives reuse; old references remain invalid.
- Unknown acquisition handles and uncertain Close results are quarantined,
  not treated as successful cleanup. Service-lifetime reconciliation remains
  future work. The backing object must remain alive until release; quarantine
  is deliberately not permission to destroy/reuse its memory.

The host still serializes calls, supplies distinct registry contexts and
process-wide unique completion tokens, and dispatches kernel-authenticated
receipts to the pending machine/run. The wrapper grants no authority: the
kernel endpoint and Config's existing subkey policy enforce access. It neither
polls nor owns an event loop, and no database codec runs in the client.

`resource_client_tests.adb` passes 1,170 hosted checks with modeled IPC/grants:
normal lifecycle, stop/retire during acquisition, stop during Get/Set, failed
submission/grant creation, denied/uncertain acquisition, uncertain write/close,
two concurrent clients, crossed references/receipts/registries, delayed grant
retirement, token reuse rejection and 96 acquisition/release cycles.
`CCL.Resources` has 72 SPARK checks discharged after adding `Retire_Lease` and
the host-only pinned-type accessor; none unproved or justified. This proves
the registry's selected contracts/safety, not the address-overlay IPC shell.
The native `receiver_fixture` now uses this shared wrapper instead of hand-
maintained client/registry pairing. Public source factories and Workbench host
pool/event integration remain separate unfinished work.

Native KVM writer, independent SQLite/WAL/ext2 inspection, and fresh-boot
recovery pass with this shared wrapper. Workbench and the normal desktop ISO
were rebuilt afterward. Logs: `/tmp/cubit-resource-client-native.log`,
`/tmp/cubit-resource-client-host.log`, `/tmp/cubit-resource-client-proof.log`.

## Opaque resource VM values (2026-09-25)

The shared VM can receive a factory's opaque registry reference, initialize a
must-handle local, borrow it for a read and consume it on close. Raw Config
handles stay inside `Config_Object_Client.Client`. The completion bridge checks
the full nominal resource definition and registry lifetime; the host still
authenticates and correlates the receipt. Sequential owned calls now start a
fresh lifecycle after the previous terminal completion.

`resource_vm_tests` passes 29 hosted checks with modeled IPC. The native app's
`resource_fixture` performs actual acquire/read/close IPC against the existing
Integer collection, with no new durable revision; the required
`config-objects-resource-vm` marker and independent SQLite/WAL/ext2 checks pass.
Both use a host-built program. Public source factories, portable resource
signatures and receiver-plus-data calls for general get/set remain unfinished.
See [resource-value tests](../ccl-resource-values/README.md) for proof scope and
the explicit proved exclusion of resource metadata from persistence conversion.

## Nonblocking native-machine dispatch (2026-09-25)

`Config_Object_Client.VM.Calls` now has native-machine Submit/Resume overloads.
They connect the existing asynchronous collection client to whole-object CCL
imports without a callback waiting for IPC. Submission inspects the suspended
machine, matches the trusted host's binding and full expected result schema,
and exports writes under the retained collection contract. The native VM can
unbox scalar native result images as well as retain aggregate snapshots; writes
therefore return the ordinary ConfigWrite variant without leaking raw handles.

The new `native_call_tests` passes 234 Linux-hosted checks: Create must finish
before use, wrong binding/schema causes no submission, duplicate submissions
stay Busy, unrelated completion tokens are ignored, every admitted read status
retains its native result, 8 KiB read-to-write forwarding, Committed/Denied/
Uncertain writes, no retry after uncertainty, and stop/drain/close/retire.
All tests use the existing IPC fixture and assert zero blocking waits. The
broader Config/discovery/core-VM suites pass. These are not concurrency proofs.

Focused VM/native-store/host-value SPARK validation passes 321 obligations with
none unproved or justified. Capacity preflight is an expression predicate shared
with admission; no duplicate bounds guard, Assume or new SPARK Off region.
The IPC client/dispatcher itself is regression-tested, not covered by that proof.
The native read fixture now drives the shared bridge from its event loop; only
that dedicated test event loop parks while waiting. Writer and independent
read-only reboot pass, including nested 8 KiB data and compiled field/match.
Independent SQLite/WAL/ext2 checks pass for both disks. Logs:
`/tmp/cubit-native-config-dispatch-{native,reopen}.log`. Writes through the new
bridge are hosted-tested; native writes still use the existing interpreter host.
The native VM suite now passes 240 checks, including scalar native-image
completion beyond the aggregate snapshot count without consuming those slots.

The host must correlate authenticated completion entries and keep its client,
program and machine associated with one run. A matching binding number is not
a run identity. A stopped machine or mismatched call/schema leaves the result
pending for explicit draining; handling Uncertain does not unpoison the client.
Public CCL resource-returning Create and Workbench event-loop integration remain
unfinished. Native values are copied at ownership boundaries, not serialized
by the client and not claimed to be zero-copy.

## Compiled native reads (2026-09-25)

The bytecode VM now consumes the same native Config read outcomes as the
interpreter. General `match` exposes Found/Stale's snapshot, and `field` can
inspect its revision or nested stored value without copying entire objects.
The shared host source tests pass 378 checks, including compiled paths for every
service status, malformed completion rejection, nested boolean fields, and
forwarding a stored subtree to a separately authorized write. Missing never
issues that write. Missing grants fail linking before any host call, including
when read authority is present but write authority is not.

The native writer test passes real Config IPC plus independent SQLite/WAL/ext2
checks with both whole-object bytecode returns and compiled read/field/match.
An independent read-only reboot passes the same checks on recovered objects
(`/tmp/cubit-object-projection-reopen.log`); native Workbench builds and the
normal desktop ISO is restored.
Its dedicated test callback still waits for IPC; this is not yet the production
Workbench's nonblocking lifecycle. The VM itself suspends normally. Focused
VM/native-wrapper/host-value SPARK checks pass 318 obligations; the interpreter,
codec and entire storage stack are not thereby fully proved.

## Owned typed evaluator results

`CCL.Language.Interpret_Object_With_Values` returns a complete native object
under an explicit expected binding. `Interpret_Object` is the pure form with no
visible service interfaces or grants. The separate Object_Interpretation_Result
contains status/diagnostics/fuel, Has_Value and an owned Image; the existing
scalar/UI result record does not grow another 16 KiB buffer.

Before execution, Matches_Type compares the expected binding's entire nominal
definition with the checked expression type. The same digest with a different
definition is insufficient. Normal grant admission still applies independently.
Export builds native cells/text while snapshots remain live, applies the
approved identity, validates the complete result and then clears temporary
storage. Failure returns an empty image and Has_Value=False. This copies owned
native data, not client serialization or zero-copy transport.

The structured-source suite now passes 230 checks, including pure/local records,
every Config read status, no effects on expected-type or grant mismatch, malformed
host reply, zero fuel and parse failure. The native read fixture additionally
returns and compares complete Missing/Found results after evaluation teardown,
including the 8 KiB nested text case. Writer/SQLite/WAL/ext2 validation passes:
`/tmp/cubit-native-result-native.log`. Broader native object/type/formatter
regressions pass (`/tmp/cubit-native-result-broad.log`). Focused Matches_Type
SPARK analysis passes two flow checks (dependencies/termination), with zero
runtime or functional proof obligations; this is not a full evaluator proof.
Independent read-only reboot and SQLite/WAL/ext2 validation also pass:
`/tmp/cubit-native-result-reopen.log`. Normal desktop ISO restored after testing.

Standalone native strings now retain their owned snapshot, up to the full
8 KiB text capacity. Length/indexing do not copy bytes; exact-size copying
supports arbitrary destination bounds. Concatenation can construct up to 8 KiB,
and native returns/arguments preserve it. Smaller Text_Value endpoints reject
oversize arguments before invocation; the scalar UI result API still rejects
results over 1 KiB rather than truncating. Targeted tests cover these boundaries,
pure string construction, empty concatenation and out-of-range indexing.
Aggregate construction bytecode, Workbench display and public collection lifecycle are still
pending. These APIs do not implement Config.create(type) or grant storage access.

## Source-level records, nested reads and arguments

Interpreted CCL can now inspect approved native host results using ordinary
`match` and `(field snapshot revision)` / `(field (field snapshot value) title)`.
BASIC uses `field(snapshot, revision)`. This is shared language functionality,
not a Config-specific parser or authority bypass. Unknown fields are rejected
before host execution; admitted metadata without a grant still cannot call.

A nested value can also be passed to an independently authorized host operation:
`(config-test.write (field snapshot value))`. The interpreter materializes only
that native subtree against the receiver's approved schema. Full nominal type
correspondence is required, not just the same layout. Text offsets are rebased;
unrelated sibling text and fields are not exported. This is a bounded native
copy, not zero-copy IPC and not client-side serialization. Possessing a value
never grants authority to call the receiver: a missing write grant fails
preflight before any read or write host effect.

Source can construct these values too: `(Settings "hello" (Reading.Text "world")
true)`. Local definitions use `(type Settings (record (title String) (reading
Reading) (enabled Boolean)))`; BASIC has `TYPE Settings = RECORD (...)` and
ordinary positional calls. Types may instead come from the visible catalog.
Strings and nested products/sums are admitted as variant payloads; nullary
alternatives remain atoms. Type checking rejects field count/type errors and
live-resource payloads before evaluation.

Capture_Local retains a private, schema-less nominal snapshot. Public Bind still
rejects No_Schema; Copy_Value needs a separate approved nonzero schema binding.
No synthetic digest is minted for local expressions. Append_Value builds native
subtrees; failure invalidates the incomplete builder, which is never published.

`CCL.Objects.Views` owns and validates one snapshot per constructed/returned aggregate,
indexes subtree boundaries, and uses owner-relative cursors for inspection.
Cursors are not capabilities and must remain paired with their owning snapshot.
The interpreter admits at most 16 such snapshots per evaluation, reserves space
before invoking the host or evaluating constructor arguments, and clears them
at teardown. It does not copy a full
native image into each local or stack slot. Extracted strings remain views into
the owning snapshot and do not consume the small interpreter text region.

177 view checks and 230 structured-source checks pass, including nested text,
variants, fields, malformed replies, denied calls, BASIC roundtrips and capacity
exhaustion before a seventeenth host call, exact subtree copies, shifted type
registries, lookalike nominal types and independently authorized writes. The
view layer's 81 focused SPARK checks pass: 45 runtime, 23 initialization,
12 termination and one validated-output contract for successful Copy_Value.
Exact projection preservation is regression-tested; interpreter integration
is not fully proved. Proof log: `/tmp/cubit-native-strings-proof.log`. Constructor
tests include empty/full sixteen-field records, function returns, BASIC
roundtrips, exact 8 KiB aggregate text, overflow rejection, snapshot exhaustion,
and rejection of unsupported compiled aggregates rather than scalarization.

The native read fixture executes actual CCL source over Config IPC, including
Missing before a write and Found after storage and independent read-only reboot.
Both boots pass independent SQLite/WAL/ext2 validation. Logs:
`/tmp/cubit-object-read-source-native.log` and
`/tmp/cubit-object-read-source-reopen.log`; the normal desktop ISO is restored.
This test host waits for IPC in its dedicated process, not a Workbench async loop.

The follow-on native writer also passes an owned nested object through an
interpreted host argument to actual Config Set and matches its typed Committed
receipt. This replaces the existing second nested write (same revision/data),
including the full 8 KiB text block. Writer and fresh-boot read-only recovery
pass with independent SQLite/WAL/ext2 checks:
`/tmp/cubit-object-arguments-native.log`, `/tmp/cubit-object-arguments-reopen.log`.
The hosted source suite additionally tests projecting a read result into a
write argument. The native supplier is a test host, not a source constructor.
The subsequent constructor test also builds the first nested Preferences record
and Mode.Active wrapper in actual CCL before its native Set, retaining a supplied
5 KiB typed subtree. Writer and independent reboot pass with the same disk
oracle: `/tmp/cubit-constructors-native.log`, `/tmp/cubit-constructors-reopen.log`.
The normal desktop ISO is restored after testing.

CCLB aggregate execution, public `Config.create(type)` and Workbench lifecycle
bindings remain unfinished. Unsupported compilation is rejected explicitly.

## Typed native read outcomes

`Config_Read_Outcomes` specializes ordinary CCL product/sum metadata to an
approved stored value type `T`. Found and Stale carry a snapshot with `revision`
and `value: T`; the other alternatives are Missing, Denied, Busy, Unavailable,
SchemaMismatch, InvalidRequest and InvalidCompletion. There is no default value
on a failed read. Stale data is never relabeled current.

The authorized host supplies the result/snapshot names and result schema key.
`Define` constructs metadata, not a signature, digest, grant or policy decision.
Retain this description per binding; do not reconstruct it for every read. The
whole result must fit normal object budgets: the envelope costs three cells,
and retains the complete text budget because it adds no text. Oversized schemas
fail during definition; actual values are never truncated to make them fit.

`Config_Object_Client.Host.Take_Read_Outcome` consumes a Get completion into
an ordinary owned Object_Value. It checks the entire value binding, not merely
its name/local number/key. A mismatched description leaves the result pending.
Invalid transport produces a typed InvalidCompletion but still poisons the
client. Resuming application code to handle that data cannot revive/retry it.
No waits, SQL, CBOR, new authority or client serialization are introduced.

768 hosted checks cover nested records/variants and text offsets, empty/full
8 KiB strings, shifted type registries, same-key incompatible value types,
all service statuses and revision boundaries, malformed values, name/key
collisions, exact 256-cell result capacity, rejected oversized schemas and the
shared client's completion lifecycle. The constructor's 32 focused SPARK checks
pass (14 runtime, 1 validated-output postcondition, 17 initialization/termination).
Exact value/status preservation is regression-tested; this is not a semantic
proof of the entire API or the IPC client.

Native writer and independent read-only reboot pass with required typed-read
markers and independent SQLite/WAL/ext2 checks. Logs:
`/tmp/cubit-config-read-native.log`, `/tmp/cubit-config-read-reopen.log`.
The normal desktop ISO is restored after these test boots.

Source inspection of nested result payloads is now supported as described above.
Typed native result export is available above; CCLB aggregate execution remains unfinished;
the existing write-result scalar variants remain executable in both modes.

```sh
nix develop -c bash tests/config-object-client/run.sh
```

## Typed CCL write outcomes

`Config_Object_Outcomes` publishes the ordinary nominal sum `ConfigWrite`:
`Committed(Integer revision)`, `InvalidRequest`, `Denied`, `Busy`, `Unavailable`,
`Conflict`, `Rejected`, and `Uncertain`. Its reviewed declaration is
`userspace/lib/config/config-write-outcome.schema`; the SHA-256 identity is
checked by `test-outcome-schema.py`. That identity is neither a signature nor
authority to write Config.

Both source interpretation and compiled CCLB use ordinary `match` arms. There
is no Config-specific compiler form or built-in Option/Result. The VM-call
adapter no longer returns Boolean for writes: all write receipts, including
uncertain transport, produce the corresponding typed alternative. The host
can resume a script to handle the outcome without asserting that the write
succeeded. Uncertain completion still poisons the native client; neither
matching the alternative nor continuing the VM retries the write.

For Set_Object, worker loss or an invalid deferred commit receipt now returns
explicit wire Uncertain, not pre-submission Unavailable. A valid Uncertain
receipt poisons the client just as an ambiguous transport failure does: consuming
it enters Failed; late success and another write cannot revive/reuse the handle.
Recovery requires fresh lifecycle handling, not automatic replay.

Create_Collection also admits Uncertain: the declaration may already be saved
even though the caller never received a handle. Retire that client, then Open
with a fresh client/token and the same approved binding. The new open still
requires current namespace authority; neither a stored schema nor an old reply
grants it. Identical create-or-open is idempotent, not implicit retry machinery.
Denied/Capacity_Exceeded after the declaration phase means no handle was granted,
not that persistent creation was rolled back. Open is read-only and does not
admit Uncertain. Create does not generate an initial value.

The focused protocol/dispatch SPARK run proves 43 obligations (16 runtime,
4 functional contracts, 23 initialization/termination). In particular, Lost's
ready completion is exactly Uncertain and clears pending state; a terminal
deferred write cannot return pre-submission Unavailable. Dispatcher fixtures
exercise lost acknowledgments, explicit worker uncertainty and invalid receipts.
The actual Turso MemoryIO test additionally recovers a committed revision after
receipt loss and rejects stale replay. These hosted failure tests do not replace
native worker-death injection or establish power-loss consistency.

Missing or conflicting result definitions leave the completion unconsumed;
full nominal definitions are checked, independent of local numbering. Stale
reads retain their existing explicit freshness policy. General read Result<T>
and lifecycle handle bindings remain future work.

`outcome_tests.adb` passes 450 checks, covering every alternative through both
execution modes, shifted/conflicting registries, malformed receipt combinations
and revision boundaries. The outcome module's 17 SPARK obligations discharge
(3 runtime, 14 initialization/termination); this is not an end-to-end Config
proof. The native read-only fixture also attempts a real write, matches Denied
in compiled CCL, and reads the unchanged value: marker
`config-objects-write-outcome-denied` in `/tmp/cubit-config-uncertain-reopen.serial`.

## Reusable nonblocking VM call bridge

`Config_Object_Client.VM.Calls` extracts submission and completion conversion
from the native asynchronous test host. An authorized host dispatch selects
Read/Write from its granted binding table; scripts do not choose raw collection
handles or operations. The host retains the client, program registry and VM
as one single-owner lifetime and supplies unique completion tokens.

`Submit` accepts a bound suspended import and enforces the canonical no-argument
read. Existing native admission validates typed writes and returns Busy while
a request is pending, without replay. `Take_Outcome` preserves service status,
revision, value presence and transport uncertainty. Denied/conflicting writes
are explicit ConfigWrite values, not ordinary False values. Stale reads require explicit `Accept_Stale` before
`Can_Resume` permits using their value. Unsupported representations and other
operations' replies remain available to the caller. No waits, retries, grants
or cancellation are hidden inside this adapter.

189 hosted checks cover submission, duplicate/foreign-token handling, legal
read/write status matrices, freshness policy, invalid write receipts, unsupported
native text with owned-result fallback, malformed read replies, rejected
submission tokens and noncanonical/authority-tagged read arguments. The standard
`run.sh` now includes the host-object and VM-call suites. Native compiled Config
writer plus independent SQLite/WAL/ext2 checks pass using the extracted bridge
(`/tmp/cubit-config-call-adapter-native.log`). Independent read-only KVM recovery
and SQLite/WAL/ext2 validation also pass (`/tmp/cubit-config-call-adapter-reopen.log`).
These are regression tests, not a SPARK proof of this IPC adapter.

The envelope record is a **host API**; its write Value now carries ConfigWrite.
General read Result<T>, lifecycle handles and Workbench dispatch remain required.

## Shared CCL VM adapter

`Config_Object_Client.VM` accepts the existing `CCL.VM.Value` plus its actual
program registry. `Set_Value` checks against the parent's retained approved
binding before submitting native IPC. It does not serialize to CBOR, expose a
collection handle to the script, install schemas or invent a Config-only value
representation. The host still owns collection creation, authority and tokens.

`Take_Get_Result` is nonblocking and uses the owned, validated completion
snapshot. It distinguishes no completion, another operation's pending reply,
invalid transport, service rejection/missing value, incompatible VM type, and
a decoded value. Success versus Stale and the revision remain explicit. A
type/representation mismatch leaves the response pending for a corrected
registry or ordinary `Take_Result`; other operations' replies are never stolen.
It shares the parent's result-consumption transition without copying another
16 KiB native image merely to discard it. No cancellation or automatic retry.

354 Linux-hosted adapter checks include real compiler/VM-produced integers,
Booleans and all scalar variant payload kinds; shifted registry IDs; ownership
rejection before IPC; busy clients; stale/missing/denied outcomes; owned snapshot
isolation; unsupported native strings; raw-result fallback; rejected submissions,
burned tokens, late completions and uncertain writes poisoning the client.
The fixture asserts zero blocking waits. These are regressions, **not a SPARK
proof of the client or adapter**. The existing 43 protocol/dispatch/startup
checks still prove. General aggregate source-language host imports and async
interpreter suspension remain separate work. Native VM suspension now uses the
shared call bridge above.

```sh
nix develop -c bash tests/config-object-client/run.sh --prove
```

926 Linux-hosted client checks, 132 dispatch checks and 384 receiver
fault-injection checks pass against the syscall/grant model.
All 43 focused SPARK checks of messages/dispatch/startup discharge,
including the rule that handling Set never emits an immediate success and
deferred work exposes no reply. This is NOT a live
Config connection or a proof of the syscall client, kernel authentication,
shared-memory synchronization, policy, or disk persistence. Native compilation
of client/service/schema and actual syscall instances now passes:

```sh
flock --exclusive --nonblock --conflict-exit-code 75 coordination/build.lock \
  nix develop -c bash -c \
  'cd kernel && alr exec -- gprbuild -p -P ../tests/config-object-client/native.gpr'
```

`Config_Object_Client` has nonblocking Create/Open/Get/Set/Close operations, a held
Config endpoint, a private approved binding and an owned grant. Applications
pass/receive `CCL.Objects.Image` directly. The native object starts at offset
4096 within the eight-page grant; the first page contains a bounded open
descriptor. Create uses the remaining seven pages for a native type declaration;
Get/Set use the first four of those pages for the native value. There is no
CBOR, JSON or string conversion here. Object snapshots
are deliberate copies, not a claim of zero-copy or immutable sender memory.

The descriptor names collection/context, requested read/write mode, and expected
schema identity. It conveys no authority. The service must resolve that against
its approved catalog and actual subject grants, and reject unsupported contexts
rather than substituting machine context. Later messages carry the issued
handle and packed generation-bearing grant reference, never another name or
profile selector. Descriptor scalars admit every raw bit pattern before validation.

One client binds one open collection. Keep it at a stable address, with one
owning dispatcher, until grant retirement is confirmed. Use process-wide
nonreusing completion tokens, including across multiple/replacement clients.
Queue rejection burns its token; pending/unconsumed results block new work.
Complete accepts ONLY kernel completions, not service events or ordinary IPC.
Schema validation uses the client's retained binding, not the server's claim.

Valid denied/missing/stale responses are distinct from invalid or failed
transport. Transport failure permanently poisons this client: a Set might
already have committed, and must not be blindly retried. No operation here
automatically retries. A valid read carrying stale data reports it explicitly.
Success for Set must acknowledge exactly expected revision + 1. Invalid
objects, operation-inappropriate statuses, malformed envelopes and unexpected
payloads are rejected; errors expose no native value or revision.

Incoming mapped reads use a volatile view before copying into private memory.
Volatile prevents treating peer-owned memory as an ordinary invariant Ada
object; it is NOT synchronization or an atomic multi-page snapshot. The kernel
handoff and service single-owner rules still matter. The same read pattern was
applied to the worker channel/receiver; their regressions and real hosted Turso
lost-response recovery still pass.

Retire is not Close, cancellation or rollback. Close the remote handle before
normal retirement; service-side process-lifetime cleanup must reclaim handles
of dead clients. Revoke acceptance alone does not permit buffer destruction.

The shared `CCL.Resources` registry now models reserve/publish/use/stop/drain/
reclaim independently of the IPC client. `resource_tests` adds 135 hosted
integration checks: stopped Create returns no script reference but its received
handle is closed; stopped Get/Set drains without reviving the reference; slots
are retained until grant retirement is confirmed. The fixture models IPC, not
the actual kernel. See `tests/ccl-resources/README.md` for proof scope and the
remaining uncertain-Create handle-cleanup requirement. This is groundwork for
the shared public factory, not a production Workbench reference pool yet.

### Acquisition reply delivery

The native kernel `replyCap` ABI returns **1 for delivered, 0 for failed**,
consuming the reply authority either way. `Config_Object_Receiver` now checks
that result for freshly minted Open/Create handles and closes just that handle
on non-delivery. This covers direct Open, cached Open, recovered Open, and
deferred Create. It never rolls back a collection or closes an existing handle
because a Get/Set reply failed. A delivered completion is still the host's
responsibility to drain, even after its script stops.

Hosted receiver tests now have 5,134 checks, including more failed acquisitions
than the handle table capacity and retention of previously delivered handles.
A dropped Set receipt with revision 1 deliberately collides numerically with
live handle 1 and must leave it usable. The real-Turso recovery fixture has
4,484 checks and independent SQLite verification of exactly two revisions after
128 failed acquisition deliveries. Send mocks now match the actual kernel's
1/0 ABI, rather than their previous unused 0/error convention.

The Config collections/authority/store SPARK slice discharges 158 checks,
including the new `Close` contract preserving every published value/revision.
This is not a proof of the syscall shell or kernel delivery semantics. Fault
injection is hosted; the native writer regression checks actual successful
Open/Create delivery, not forced caller death or arbitrary power failure.

`Config_Object_Dispatch` now implements the service's owned-message dispatch
over `Config_Typed_Store`. Open/Get/Set/Close enforce the same scope/handle model.
Set admission checks authority before reporting queue/reply-capacity status;
unauthorized callers cannot probe whether another write is pending. A trusted
`Reply_Reserved` input is required before staging: the thin shell must actually
move the current kernel reply capability into an empty saved slot. This Boolean
is NOT a caller-provided request field or proof of a real saved capability.

`Await_Storage` returns a null message, not a provisional success. Cached reads
continue using current reply authority. Finish/Lost produce at most one response
for the saved client request; stale receipts and background restores do not
consume its reply. Revocation after acceptance prevents later operations but
does not undo an already accepted commit. The dispatcher retains no client
mapping while waiting. Its tests cover these transitions and malformed framing.

The real hosted Turso fixture now drives the public operation messages through
the service owner, receiver and dispatcher: 472 checks pass, with delayed success, caller denial, loss
after commit, unavailable response and recovery without retry. Independent
SQLite inspection verifies exactly two revisions. It models grant ownership,
borrow/release and single-use replies; it does not exercise actual kernel
reply-capability moves.

Recovery fault cases include revoked/regranted authority during Missing,
schema-mismatch and successful value-load responses; loss of the metadata
reply; saved-reply cleanup; terminal admission failure; and explicit owner
replacement ignoring a late old completion. No automatic retry or extra
database revision is allowed. Multiple complete service instances live on the
hosted fixture's heap to avoid exceeding Linux's default test-stack size; this
does not change native Config allocation or its fixed catalogs.

`Config_Object_Receiver` is the single-owner syscall shell. Its native instance
uses `Acquire` with the authenticated receive sender as expected grant owner,
`Return_Acquisition`, `saveReplyCap` and `replyCap`. The dedicated saved slot
must initially be empty and remain exclusively owned by this instance. Incoming
requests must come from RECEIVE on the same thread that invokes Handle, not an
event or caller-supplied sender field. Source decoding never Enum_Val's an
unchecked request. Open borrows only the control page; Set borrows only the
native object, after handle authorization. Both snapshot and release before
dispatch. Get borrows a writable output only after successful authorization.

An accepted Set keeps its reply authority, not a caller mapping. A failed save
does not stage work; immediate Set failures after a save consume the saved slot.
Concurrent reads and denied/busy requests use the current reply slot without
overwriting the saved one. Stale receipts cannot reply twice. Kernel delivery
failure consumes the reply without retrying or undoing the write. A failed
grant return permanently stops new receiver requests; the owner must retire
that transport, but it may still complete an already accepted saved reply.

The native instance reserves slot 62 and is now wired into Config's owning
event loop. Neither this shell nor its kernel syscalls are SPARK-proved.
`Config_Object_Service` now owns the receiver, typed store and worker channel,
and automatically submits accepted writes/restores and handles completion or
enqueue failure. Tokens have package/process lifetime, surviving state
replacement; instantiate once and keep the same completion-token domain. The
hosted Turso test exercises this event-facing owner, not a hand-wired pipeline.
Its Restore now asynchronously provisions the approved type before loading;
the worker selects bindings from its authenticated catalog, not test arguments.
Live receive/completion dispatch and trusted worker startup are implemented;
the headless `config-storage` case tests attachment/database initialization,
and `config-inspection` denies client backend nomination. Public durable Create
is now routed as opcode 0x0618. It requires namespace Write_Config (plus Read
when requested), validates and releases the native type snapshot, preflights
catalog capacity/conflicts, then retains the saved reply across durable creation,
approved worker provisioning and value restoration. Only then does it return a
handle, after checking the original authority revision and current rights.
An identical Create on a ready object does not reload its cache. Conflicting
types are rejected, not replaced. A lost response is not automatically retried.

Creation/authority lifetime fault tests are Linux-hosted with modeled IPC.
The [native app regression](native-app/README.md) now passes the live public
client-to-worker path in KVM and TCG: Create/unset/Get/Set/Close, exact native values,
conflict preservation, scope denial, read-only handles and idempotent Create.
Independent SQLite/WAL and read-only e2fsck checks verify one type and exactly
two committed revisions. Read-only Open now recovers an uncached persisted
object without Write authority or client-supplied type metadata. The same
definition-acquisition state machine serves Create and Open; cached Open can
still run while another request owns the saved reply. Authority replacement
invalidates a delayed response, including Missing/schema errors, and recovery
never restores old grants. The unused out-of-band service Register entrypoint
was removed; durable Create or validated recovery populate the catalog.

Two independent TCG boots pass: a writer publishes, then a different read-only
app recovers native value 42/revision 2 from a fresh Config catalog. KVM reopening
the seed disk also passes. Both oracles verify exactly one type and two revisions.
Eager startup enumeration and application/CCL bindings remain pending; default
desktop profiles do not enable the worker, and byte settings remain volatile.

The hosted runner additionally checks 20 startup-envelope/worker-source cases.
With `--prove`, message/dispatch/startup units discharge 43 focused checks.
Envelope validation is not sender authentication: the native Config shell
separately requires the registered procmgr for attachment.

The live regression caught two integration mistakes: the worker expected zero
instead of the kernel-stamped holder PID authority tag; and its Rust main had
not called Ada library elaboration. The worker now uses the tested source
predicate and GNAT's generated standalone-library initializer before any Ada
entrypoint. No response validation, authorization or entropy checks were relaxed.

## Interpreter host-object adapter

`Config_Object_Client.Host` is the reusable, nonblocking counterpart of the VM
adapter. Set accepts a host value and validates it against the client's retained
approved schema before submitting IPC. Get returns an owned native object,
including its revision; stale, denied, missing and uncertain outcomes stay
distinct. Taking a Get result never consumes an unrelated operation's result.
The adapter does not wait, serialize, issue SQL or expose the private handle.

Run the Linux-hosted transport fixture with Nix:

```sh
nix develop -c bash -c 'cd kernel && \
  alr exec -- gprbuild -p -P ../tests/config-object-client/host_client.gpr && \
  ../tests/config-object-client/build/host/host_tests'
```

45 checks cover an 8 KiB string object, schema/padding/type rejection before
submission, busy/unavailable admission, revision/status preservation, unrelated
results, poisoned malformed completions and zero waits. The fixture uses mocked
IPC; see [native source integration](native-app/README.md) for real CuBit tests.

## Shared request lifetime and owned receivers

The production client uses `CuBit.Async_Requests` from the common runtime for
token reservation and completion consumption. This is the same component used
by the filesystem channel; Config keeps its schema and grant/handle logic.

`resource_calls.gpr` exercises the shared `Config_Object_Client.Resources.Calls`
adapter with source-compiled CCL: native typed reads/writes, mismatched receiver,
binding and result schema rejected before IPC, queue rejection, duplicate
submission/completion, Stop followed by draining, and uncertain write outcomes.
It uses modeled kernel IPC on Linux and does not establish Workbench integration.

## Shared Config descriptors

`interfaces.gpr` exercises `Config_Object_Interfaces` using the real catalog,
completion, source compiler and linker. The checks cover scalar and record
collections, reuse of shared schemas/resource specializations, transactional
rejection, exact completion contracts, wrong data/receiver types, and rejection
of visible-but-ungranted calls. These are Linux-hosted tests, not live service
discovery or Workbench IPC execution. The standard `run.sh` includes them.

## Pinned event-loop execution

`runs.gpr` exercises `Config_Object_Client.Resources.Runs`: one owned copy of
the verified program, native-object machine, registry, collection, and pending
invocation. Its 378 hosted checks cover Stop at each operation, repeat Run,
replacement rejection while draining, duplicates/foreign receipts, rejected
submission, observed revisions, conflicts without replay, delayed grant
retirement, quarantined acquisition, and token exhaustion. Both shipped native
samples also pass syntax-view conversion, compilation, linking and verification.
`run.sh` includes this suite. Native keyboard/IPC/storage coverage is separate:
see `tests/config-workbench/README.md`.
