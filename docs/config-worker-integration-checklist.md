# Persistent Config integration checklist

Updated 2026-09-25. This is not an additional authority model. Config remains
the policy-enforcing dispatcher; Turso is its owned storage worker, not an
alternative client access path.

## Current boundary

**Declarative authority (2026-09-26):** follow
[declarative configuration and mutable state](config-declarative-state.md).
CCL remains the source of desired settings; Turso stores active revision data and
separately classified application state. Earlier "durable defaults" items below
must not be implemented as unconditional persistence of scalar `set` calls.
The native Workbench counter is app-state, not system configuration activation.
The new hosted read-only plan comparator is a review building block only.

Typed collection registration now supports immutable application-state versus
declaration-managed classification; ordinary managed writes and reclassification
are denied in the common catalog/store/IPC path. Database format4 now preserves
and recovers that classification; cold-cache Create cannot downgrade a managed
registration. Appearance migration still needs trusted initial registration/name
reservation, source-bound revisions and authorized activation. No automatic
migration from old database formats or managed-value write API is provided.

The shared `Config_Activation` controller now owns one reviewed CCL setting and
rechecks explicit scoped activation authority, grant lifetime and expected base.
Storage selection and consumer application are distinct states. This is a
Linux-hosted-tested core, not native activation or a Turso adapter; exact-source
revision persistence, typed schema binding and recovery remain the next join.
Normal Read_Write/wire grants do not gain activation; ordinary Set stays denied.

**Ordinary desktop integration (2026-09-26):** Config's Turso worker is now a
stage-2 build target started by the normal desktop profile. All three desktop
launchers share verified scratch-disk staging, including current Workbench and
the two Config samples. Native Apps-menu launch/write/reboot/read and independent
SQLite/WAL/ext2 checks pass on a disposable 4 KiB disk retaining the browser
assets. Normal launchers still reset their scratch disk; use the playground's
`--reuse` for retained work. See the [current security audit](config-security-audit.md)
for proof/test evidence, broad bootstrap administration, raw-PID lifetime gaps,
and the distinction between durable typed objects and in-memory scalar settings.
This update supersedes earlier Workbench-integration-pending notes below; it
does not claim interpreter/REPL integration or durable defaults are complete.

**Nonblocking native-object dispatch (2026-09-25):** shared VM.Calls native
Submit/Resume overloads preflight the actual suspended call, binding and complete
read/write result schema, export only its current argument under the collection
contract, and resume with typed outcomes. Existing asynchronous client owns
Create/Open/Close and the private collection handle. Hosted native-call234 and
broader Config/discovery/core VM suites pass; focused VM/native-store/host-value
SPARK321 has none unproved/justified. IPC client/dispatch is tested, not included
in that proof. Native compiled read fixture now uses this event-driven bridge,
not the interpreter's blocking Invoke callback. Native writer and independent
read-only reboot pass along with both SQLite/WAL/ext2 oracles; the new bridge's
write path is hosted-tested, while native writes still use the interpreter host.
Scalar native completion remains outside the aggregate storage quota; native
VM regression240 passes. Logs /tmp/cubit-native-config-dispatch-*.log.
Public resource-returning Config.create(type),
nonblocking Workbench hosting, bytecode aggregate construction/string operations,
durable settings defaults and performance work remain outstanding.

**Compiled field/match validation (2026-09-25):** generic native-object VM
execution now resolves product fields and general sum payloads through owned
snapshot cursors; no whole-object copy or fresh snapshot per projection. The
same type-verifying loop drives the scalar and native instances. Hosted object
tests124/source378 pass, including every Config read status and read-to-write
subtree forwarding. Missing/malformed results do not trigger the write and
grant failure prevents execution. Focused VM/native-wrapper/host-values SPARK
discharges318 obligations with none unproved/justified (72 runtime,26 assertions,
21 contracts,159 initialization,4 non-aliasing,35 termination,1 dependency).
An explicit no-global-effects contract on the actual storage callback avoids
the GNATprove generic-inlining compiler assertion without disabling checks.
Native writer and independent read-only reboot pass real Config IPC and
independent SQLite/WAL/ext2 validation, including compiled field/match on
recovered objects. Native Workbench builds and normal desktop ISO restored.
Logs `/tmp/cubit-object-projection-{native,reopen,final-host,final-config}.log`.
Aggregate constructors/string VM operations, public Config.create(type) resource
lifecycle and nonblocking Workbench integration, durable defaults and I/O
performance remain unfinished. Full codec proof is still pending; the earlier
proof-generation run was stopped for excessive memory, not reported as proved.

**Compiled native object transport (2026-09-25):** CCLB v6 adds the Object_Value
kind for persistable shapes beyond scalar variants. Compiler/codec/verifier/
schema-pinned linker support receiving, retaining in locals, forwarding and
returning whole native objects across suspended host calls. The optional
CCL.VM.Native_Objects wrapper owns snapshots; ordinary scalar Machine_State
does not gain a large pool. Public scalar completions and external initial
locals cannot inject references. Storage exhaustion prevents the next host
effect. Hosted roundtrip/rejection/lifetime tests55 pass, along with broader
Config/object/type suites. Focused VM+native-wrapper SPARK195 pass (33 runtime,
14 assertions,17 contracts,114 initialization,3 non-aliasing,14 termination).
This proves the checked obligations, not whole-system semantic preservation or
authorization noninterference. Native Config writer and independent read-only
reboot pass, including compiled get/suspend/resume of complete typed read
outcomes and independent SQLite/WAL/ext2 checks. The dedicated native test
callback waits for IPC; the VM itself remains nonblocking. Logs:
`/tmp/cubit-object-vm-{native,reopen}.log`.
Construction bytecodes, async public collection
lifecycle/Workbench, durable defaults and performance remain pending.

**Native strings (2026-09-25):** standalone host strings and field projections
retain an owned snapshot rather than eagerly copying through the 1 KiB text
region. Native return/argument export and concatenation support the full 8 KiB
object text budget. Text-only endpoints and scalar UI results enforce their
separate limits without truncation. Hosted views177/source230 pass. Focused
Views SPARK81 pass (45 runtime,23 initialization,12 termination,1 existing
validated-output contract); no unproved checks or Assume. Interpreter behavior
is regression-tested, not fully proved. Native writer and independent read-only
reboot pass: the CCL source projects Preferences.name (including the full 8 KiB
case) and returns a standalone typed string equal to the recovered object field.
Independent SQLite/WAL/ext2 checks pass on both disks. Logs:
`/tmp/cubit-native-strings-{native,reopen}.log`.

**Owned native evaluator return (2026-09-25):** Interpret_Object_With_Values
and the pure Interpret_Object return an owned Image under an independently
approved expected schema. Full nominal result-type correspondence and normal
grant preflight precede effects; validated export precedes temporary teardown.
No serialization and no buffer inflation for ordinary scalar evaluations.
Hosted structured-source200 plus broader object/type/formatter suites pass.
Native writer returns whole Missing/Found objects and compares them after
teardown, including 8 KiB nested text; SQLite/WAL/ext2 oracle passes. Log
`/tmp/cubit-native-result-native.log`. Focused Matches_Type flow analysis passes
dependencies and termination only (no functional/runtime proof obligations);
interpreter correctness remains regression-tested.
Independent read-only reboot and disk oracle also pass:
`/tmp/cubit-native-result-reopen.log`. Normal desktop ISO restored.

Still pending: aggregate bytecode, public collection lifecycle/Config.create(type), Workbench
integration, durable defaults and performance validation. The scalar UI API
remains separate from the new typed embedding API.

**Interpreted constructors (2026-09-25):** record declarations/positional
constructors and persistable nested variant payloads work in Lisp and BASIC.
Local snapshots have nominal metadata but no advertised identity; public Bind
still rejects No_Schema. Materializing a host argument requires its approved
binding and independent authority. No Config-specific syntax is introduced.
Hosted views162/source155 pass, including full 16-field/8 KiB construction,
overflow, exhaustion, BASIC and nominal/resource rejections. Views SPARK65 pass
(34 runtime,19 initialization,11 termination,1 validated-output contract);
interpreter behavior is regression-tested, not fully proved.

The native writer constructs its first Preferences record/Mode.Active variant
in CCL, preserving the exact 5 KiB supplied typed payload, then gets an actual
Committed revision1. Its second source write retains the 8 KiB test. Independent
SQLite/WAL/ext2 oracle passes without altering expected declarations/revisions.
Writer log `/tmp/cubit-constructors-native.log`; proof log
`/tmp/cubit-constructors-proof.log`. Bytecode aggregates and
public Config lifecycle/Workbench/durable defaults/performance remain pending.
Independent read-only reboot also passes with SQLite/WAL/ext2 validation:
`/tmp/cubit-constructors-reopen.log`. Normal desktop ISO restored. Broader
type-discovery/portable bytecode and formatter tests pass; unsupported aggregate
bytecode remains explicitly rejected, while source construction is now admitted.

**Typed aggregate host arguments (2026-09-25):** source can pass a nested native
value to another independently authorized host operation, e.g.
`(config-test.write (field snapshot value))`. Copy_Value requires complete
nominal correspondence with the receiver's approved binding, rebases text
offsets and exports only the subtree. This is bounded native copying, not
serialization or a zero-copy claim. All-import preflight still rejects absent
write authority before any read or write effect.

Hosted view156/source96 and full client/type-discovery regressions pass. Focused
view SPARK63 checks pass:34 runtime,17 initialization,11 termination and one
postcondition ensuring successful output validates against the target binding.
Exact value preservation and interpreter behavior remain regression-tested.
Log `/tmp/cubit-object-arguments-proof.log`. This supersedes the host-argument
limitation in earlier notes; construction/final export/bytecode remain pending.
Native writer now routes the existing second nested Set through interpreted
CCL, from a test-host-supplied owned aggregate to the real Config Set adapter,
and matches Committed. Exact maximum-text image, revision2, independent reboot
and SQLite/WAL/ext2 checks pass without changing the database oracle. Logs
`/tmp/cubit-object-arguments-native.log`, `/tmp/cubit-object-arguments-reopen.log`.
The read-result projection-to-write scenario is hosted-tested; native supplied
values are not arbitrary source constructors or public Workbench API bindings.

**Source inspection of typed results (2026-09-25):** interpreted CCL now uses
ordinary `match` and `field` to inspect nested approved host objects. The shared
`CCL.Objects.Views` layer owns validated snapshots and indexes subtree bounds;
owner-relative cursors carry no authority. The interpreter reserves one of 16
snapshot slots before each aggregate-producing host call, then clears the pool
at teardown. No per-local full-image copies or Config-specific syntax are added.
BASIC formatting supports `field(snapshot, revision)`.

Hosted view120/source84 checks and broader type/catalog/formatter regressions
pass. Focused view SPARK53 checks pass (28 runtime,14 initialization,11 termination;
no functional contracts). This does not prove the whole interpreter. Native
writer and independent read-only reboot execute CCL matching Missing/Found and
reading the revision, with exact native value comparison and independent
SQLite/WAL/ext2 validation. Logs `/tmp/cubit-object-read-source-native.log` and
`/tmp/cubit-object-read-source-reopen.log`; normal desktop ISO restored.

Still unfinished: CCLB aggregate execution, public `Config.create(type)` and
Workbench lifecycle/async bindings, durable default settings and performance
validation. The native read fixture's blocking host is test-only. Unsupported
aggregate compilation is rejected, not flattened or silently interpreted.

**General native read outcomes (2026-09-25):** `Config_Read_Outcomes` constructs
an ordinary CCL result specialized to the approved stored value type, with
Found/Stale carrying (revision, value) and explicit failure alternatives. The
shared nonblocking Host adapter consumes these without serialization or implicit
retry. Wrong complete value binding leaves completion pending; invalid transport
becomes InvalidCompletion while preserving client poisoning. Caller/catalog
supplies approved schema identity and names; no hash/signature or authority is
invented by the metadata factory. Retain descriptions per binding.

Hosted768 checks pass, including nested strings/products/sums, exact size limits,
stale/error/invalid results and lifecycle state. Focused constructor SPARK32 checks
pass (14 runtime, 1 validated-output contract, 17 initialization/termination).
Logs `/tmp/cubit-config-read-proof.log`, `/tmp/cubit-config-read-native.log`.
Native writer and independent read-only reboot pass with required typed-read
markers and SQLite/WAL/ext2 validation; normal desktop ISO restored. The fixture
checks Missing before the first nested write, then exact Found data, and recovered
Found data after reboot with shifted local type IDs. Reboot evidence:
`/tmp/cubit-config-read-reopen.log`. No extra declarations/revisions or filesystem
authority were added to the native client.
The subsequent source-inspection milestone above makes these nested results
readable by the interpreter. General aggregate construction and VM execution
remain unfinished; this is not Config.create(type) or finished Workbench
integration. No Config-only bytecode tag or string coercion was introduced.

**Create recovery (2026-09-25):** a staged Create now returns Uncertain if its
worker/receipt is lost, or storage reports success but local handle opening
cannot establish a usable collection. Read-only Open recovery remains
Unavailable. Client consumption preserves the explicit status and enters Failed;
late success cannot revive the old client. Retire, then use a fresh authorized
Open with the same expected binding to inspect what actually exists. Missing
means no definition was recovered, not an instruction to silently retry Create.
Create-or-open of an identical definition remains idempotent; conflicting actual
definitions, including ones claiming the same digest, cannot replace it.

Creation has two effects to distinguish: persisting the declaration and granting
a live handle. Only Success promises the latter. Denied after an authority change
or Capacity_Exceeded during handle creation does not promise that the declaration
is absent. No default object/value revision is synthesized by Create. Recovering
stored metadata never restores a prior process's authority or handle.

The old Lost path failed the new receiver regression at check273 before the fix.
Linux-hosted client926/dispatch132/receiver384/startup20/VM354/host45/calls189/
outcomes450 and channel42/receiver-channel86/type-channel680 checks pass. Actual
Turso worker tests pass11, including losing a created declaration's receipt,
reopening the same MemoryIO store, exact schema recovery, identical-create
idempotence, conflict rejection and zero value revisions throughout. Focused
protocol/dispatch SPARK43 checks pass; the pointer/IPC receiver is regression
tested, not proved. Logs `/tmp/cubit-config-create-validation.log`,
`/tmp/cubit-config-create-turso.log`, `/tmp/cubit-config-create-native.log`.
The full hosted Turso suite also passes all48 tests, with no filtered tests
(`/tmp/cubit-config-create-turso-full.log`). This includes the existing storage
fault-injection tests; it is not a claim of ext2 power-loss atomicity.
Native writer and independent read-only reboot pass with SQLite/WAL/ext2
validation, including compiled CCL's real Denied-write branch. Normal desktop
ISO restored. Reboot log: `/tmp/cubit-config-create-reopen.log`. Receipt loss
is injected in hosted tests, not in these normal native regression boots.

Next read-outcome work must carry the actual approved value type and revision,
not special-case Integer or coerce objects to strings. The native image and
host/interpreter bridge already carry nested products/sums; the current VM
value representation only carries scalar variants. A general read-result sum
around a structured value therefore needs general aggregate execution, not a
second Config-only type system or a tag masquerading as an object. Keep stale
versus current data explicit and retain the client's authority/lifetime rules.

**Pending-write uncertainty fixed (2026-09-25):** `Set_Object` now has an explicit
wire `Uncertain` receipt. Worker loss, invalid commit receipts and unresolved
deferred writes preserve it through the native client and `ConfigWrite.Uncertain`.
The client enters Failed after consumption; a script can handle the outcome but
cannot thereby retry the old handle. Pre-submission Unavailable remains distinct.
This does not promise rollback. A red regression reproduced the former incorrect
Unavailable receipt before the dispatcher fix.

Focused protocol/dispatch SPARK analysis discharges 43 checks (16 runtime,
4 functional contracts, 23 initialization/termination), including exact Lost
completion and the permitted terminal deferred-write receipts. Hosted suites
pass: client906, dispatch132, receiver358, startup20, VM354, host45, calls189,
outcomes450. Six actual Turso worker tests also pass, including committing into
MemoryIO, losing the receipt, retiring/reopening the worker, recovering revision1
and rejecting stale replay without producing revision2. These are Linux-hosted
failure tests, not native worker-kill or power-loss tests.

Native KVM writer and independent read-only reboot pass, including a real denied
write handled in CCL; independent SQLite/WAL/ext2 validation passes for both.
Logs: `/tmp/cubit-config-uncertain-native.log`,
`/tmp/cubit-config-uncertain-reopen.log`,
`/tmp/cubit-config-uncertain-validation.log`,
`/tmp/cubit-config-uncertain-turso.log`, and
`/tmp/cubit-config-uncertain-terminal-proof.log`. Normal desktop ISO restored.
These checks do not prove the IPC client, filesystem crash consistency or all
Config behavior. Create's separate multi-stage definition/restore lifecycle is
covered by the newer entry above. Public CCL lifecycle,
general typed read outcomes, Workbench integration and durable default settings
remain unfinished. Config stays the priority before graphics/Mesa work.

Typed write results (2026-09-25): `Config_Object_Outcomes` supplies a reviewed,
schema-pinned normal CCL sum. Committed carries the revision; denied/conflicting
writes and uncertain transport are explicit alternatives, not Boolean False or
an untyped host failure. Native VM.Calls uses it; the interpreter host bridge
uses the same definition. Hosted418 schema/receipt/interpreter/VM checks and176
call-bridge checks pass;
17 outcome-module SPARK checks pass (3 runtime, 14 initialization/termination,
not full Config correctness). A read-only native client attempts a write,
matches ConfigWrite.Denied in CCL and performs unchanged reads. The service
retains handle/ACL enforcement; no script number becomes authority. Generic
read outcomes and lifecycle handles are not implemented by this slice.

Reusable VM-call bridge (2026-09-25): the native async fixture now uses
`Config_Object_Client.VM.Calls` instead of implementing get/set receipt handling
itself. The host API preserves status/revision, explicit stale-read policy,
unsupported-type fallback and uncertain writes. Hosted167 checks pass, as does
the complete hosted client suite, native writer and independent read-only KVM
recovery, each with SQLite/WAL/ext2 validation. Logs:
`/tmp/cubit-config-call-adapter-native.log`,
`/tmp/cubit-config-call-adapter-reopen.log`, `/tmp/cubit-config-vm-calls-final.log`.
This is not yet a generic CCL Result<T> or the public create/type syntax; it
removes a test-only dispatch path so those integrations can share production
code. No new authority model, SQL/CBOR client codec, polling loop or retries.

CCLB v5 portable typed imports (2026-09-25): implemented source lowering,
schema-bearing codec and atomic schema-aware linking. Hosted source/VM suites
pass, including portable roundtrips and forged/missing schema/grant rejection.
Host-value/catalog proof: 133 checks, none unproved. Broad codec proof interrupted
due to proof-expansion resource use, not a proof pass. Native compiled writer
and independent read-only KVM reader now pass, together with independent
SQLite/WAL/ext2 checks. The initial test snippet used unsupported branch-local
`let`; moving its locals outside branches fixed the fixture without weakening
compiler admission. Expanded portable regression: 1023 checks pass, including
every truncated prefix, late-import atomic failure and stripped schema keys.
Logs: `/tmp/cubit-cclb5-fixed.log`, `/tmp/cubit-cclb5-reopen.log` and
`/tmp/cubit-cclb5-extra.log`. Normal desktop ISO restored. This is still a
test-only Config host, not the finished public API or default durable settings.
The v4 limitations in the historical entries below are superseded by v5.

Proof follow-up: even `--no-inlining` with selected field lines still expands
the monolithic encoder substantially. The field proof was deliberately stopped,
not passed. The isolated admission helper completes only its initialization/
termination checks; it is not a functional codec proof. Before another broad
run, separate fixed-size import metadata encoding/decoding from whole-program
orchestration so its bounds and roundtrip properties can be proved in isolation.
Do not add assumptions or defensive guards merely to appease the prover.

Async VM prerequisite (2026-09-25): the internal import declaration now includes
nominal argument/result types. Verification propagates them through the stack;
resume rejects wrong nominal definitions/alternatives and tagged/noncopyable host
results. A regression reproduced the old kind-only acceptance of a tagged scalar.
159 hosted import/codec/linkage checks, existing full VM/ownership tests and both
Config adapter suites pass. The VM's implemented proof obligations discharge
(174 checks, none unproved); this is not a full semantic soundness theorem.

Native Config writer and independent read-only reader also pass two successive
reads into the SAME suspended VM: no re-evaluation or timer polling inside the
VM/client, no extra revisions, retained approved schema with shifted reader IDs.
Independent SQLite/WAL/ext2 checks pass. This fixture constructs an internal VM
program; CCLB v4 explicitly rejects these typed imports rather than erasing their
metadata. Next is schema-pinned portable import/compiler admission, including
full nominal-definition comparison against an authorized catalog, then reusable
typed Config outcomes/handles and Workbench dispatch. The earlier synchronous
source fixture remains a separate test, not the intended GUI implementation.

Portable next step: keep runtime-local nominal references in `VM.Program`, and
put stable schema identities in portable linkage alongside the pinned interface
digest. The linker must receive the authorized schema catalog and compare each
referenced program type's complete nominal definition with that binding before
installing *any* runtime binding. A matching numeric local ID or claimed schema
key is insufficient. This must also distinguish an object-wrapped Integer from
an unwrapped scalar import. Bump the format instead of silently interpreting old
reserved bytes; test encode/decode, changed definitions, missing schemas, absent
grants, atomic failed links, then compile and suspend the existing native Config
source fixture through that path.

Typed source-host slice (2026-09-25): import descriptors now carry approved
argument/result schema keys; the shared CCL catalog supplies the actual nominal
types. The host value ABI can own full native objects, with schema validation at
the boundary. The reusable `Config_Object_Client.Host` adapter is nonblocking
and preserves status/revision/uncertain completion semantics. Hosted source109,
host-adapter45 and VM-adapter354 regressions pass. Focused object conversion29,
schema catalog22 and owned callback envelope10 SPARK checks pass, none unproved.
The selected interpreter object-call region also passes through actual direct/
periodic host instantiations: 66 runtime plus 324 other checks, none unproved;
this is not a whole-interpreter proof.

Native KVM writer and independent read-only reader now execute actual CCL
`config-test.set/get` calls for a discovered Reading variant. Both independent
SQLite/WAL/ext2 oracles pass with exactly three declarations/six revisions.
See [native scope and snippets](../tests/config-object-client/native-app/README.md).
This is a real IPC integration fixture, not a finished Workbench API: its test
host waits on activity; the shared adapter does not. Remaining before calling
Config finished: public typed outcome/handle bindings, interpreter suspension
and GUI integration, supported aggregate source/CCLB representation, durable
default settings and end-to-end performance validation. Mesa/GPU work remains
deferred. Earlier entries below are chronological evidence, not all current
limitations.

Unified Rust std adoption (2026-09-25): the probe and worker now build with the
shared CuBit std target, retaining the bounded allocator and explicit random/
clock hooks. Fixed native allocator target selection so `os=cubit` cannot fall
back into hosted System allocation recursively. The obsolete probe-only std
port was removed. The native probe's four-thread grant-backed File regression
passes, including exact readback and callbacks re-entering `size`; its SQLite/
ext2 oracle also passes. This is serialized transport concurrency, not parallel
SQL connections, an async reactor, or a throughput claim. Public Config writer
and independent read-only KVM reboot also pass, with SQLite/WAL/ext2 validation
and no extra revisions. Normal desktop ISO restored. Logs:
`/tmp/cubit-unified-std-threaded.log`, `/tmp/cubit-unified-std-reopen.log`.
This completes runtime adoption, not general source imports or default durable
desktop settings.

Root admission follow-up (2026-09-25): handler-result rejection and its source
diagnostic now remain inside the type checker's validated-node path. The
broader run found an unresolved caller-side index; focused proof and hosted
regressions pass after the control-flow change. See
[scope and reproduction](../tests/ccl-type-discovery/README.md#root-result-admission-2026-09-25).
The broad run was stopped, not completed. General typed source host imports
remain the next integration work, not something proved by this diagnostic fix.

Owned host-result follow-up (2026-09-25): shared CCL callbacks now return a
non-discriminated `Call_Result` envelope containing the discriminated value and
existing call-success flag. The four kind-changing assignments described below
now prove through the actual direct and periodic instantiations. All host
consumers were migrated, including native Config bindings, Workbench and remote
control; no old callback-profile compatibility overload remains. This is not a
CCLB/wire-format change or a new source error model. See
[result API and proof scope](../tests/ccl-host-results/README.md).

Interpreter prerequisite (2026-09-25): private scalar payloads now have an
Integer/Boolean-only representation, with no VM ownership/variant fields. This
removes the reproduced host-conversion precondition failure. Scalar-copy host
results now reject tagged/noncopyable values instead of silently stripping their
ownership metadata; a hosted regression reproduced the old behavior. Hosted
frontend/remote/periodic/Config adapter tests and native Workbench startup pass.
The actual generic callback proof still reports four discriminant checks on
its kind-changing `out Value` result (default/conversion assignments in two
instantiations). Address the callback result API shape next; no blanket guards,
exception handling or claim that the full interpreter is proved. General source
Config host imports are still pending. See
[scalar boundary evidence](../tests/ccl-type-discovery/README.md#scalar-boundary-hardening-2026-09-25).

CPU-path measurement (2026-09-25): the isolated production-store benchmark
identified canonical zero-byte scans, not schema-registry copies, as a useful
small-object cost to remove. Ada array equality against static zero buffers
reduces hosted integer Set/request/reply/publication batch means from 21.204 to
11.437 µs; full 8 KiB strings from 14.216 to 10.932 µs. Cached Get is unchanged.
This is not a native IPC/disk/p99 result. The 29,499 object regressions include
every tail position and individual unused byte; SPARK proves exact equivalence
to all-zero text/padding predicates, with 137 focused object/bridge/publication
checks discharged. Snapshot ownership, scope checks and durable publication
ordering are unchanged. See [benchmark scope](../tests/config-collections/README.md#cpu-path-measurement).

Schema-equivalence fix (2026-09-25): Config registration and worker provisioning
now compare approved keys plus full nominal root graphs, not whole registry
records. The old comparison rejected idempotent Create from another registry
layout; reproduced by `/tmp/cubit-schema-equality-before.log`. Durable Create's
byte-conflict slow path validates the existing declaration and accepts only
equivalent schemas, without rewriting it or adding revisions. Failed/missing
recovery remains terminal/uncertain. Hosted real-Turso tests confirm idempotency
across three opens with independent SQLite validation; no client codec added.
Native KVM recreation of a shifted nested schema and a separate read-only boot
also pass, with exact SQLite/WAL/ext2 checks and no added revisions. Nominal
correspondence now has 10,589 hosted checks; collection/authority/store and
object/bridge/publication proof sets discharge 157 and 132 checks respectively.
The new durable read-and-compare orchestration is regression-tested, not proved.

Keep registry scope explicit: the service's collection catalog spans application
namespaces, which may legitimately use the same short type name under different
schema keys. The CCL schema catalog is one authorized compiler/host view and
rejects ambiguous names within that view. Sharing definitions globally must not
accidentally turn local CCL type names into a system-wide namespace restriction.

Approved schema catalog (2026-09-25): `CCL.Objects.Catalog` stores one shared
type registry and compact key/root entries for an authorized discovery view.
It rejects identity/name conflicts and capacity exhaustion atomically; repeat
publication is idempotent. 5,467 hosted checks and 16 focused SPARK obligations
pass, including publication count/rollback and lookup-key contracts. The native
discovered-type Config fixture now resolves its approved binding through this
catalog before publishing compiler-visible names. This does not grant Config
authority or implement general aggregate source host calls; see
[schema catalog scope](../tests/ccl-schema-catalog/README.md).

Discovered-type integration (2026-09-25): the shared catalog can now publish
approved named types into the compiler/interpreter's initial registry. Atomic
reachable imports reject conflicts and exhaustion without partial publication;
50,162 import checks, 58 frontend/CCLB checks and 50 focused registry proof checks
pass. A native third Config collection stores two values of a catalog-discovered
Reading variant; the snippets contain no declaration text. KVM writer and
independent KVM/TCG readers pass, including a shifted local type ID, denied
read-only writes and exact SQLite/WAL/ext2 validation of three declarations/six
revisions. This uncovered and fixed VM rejection of unrelated record metadata;
36 admission/rejection checks and 171 VM SPARK checks pass, retaining checks at
every executable use. This includes rejection of a discovered string-payload
variant that the scalar interpreter representation cannot preserve. The broad
interpreter/catalog proof reported unresolved function-bookkeeping and host
conversion obligations and was stopped before revising the interpreter. An
isolated HEAD comparison reproduced the function-count increment failure;
narrower function indices and reserved-slot publication address that invariant
without defensive guards. Do not call the whole interpreter proved or assume
every reported obligation predates this work. Source Config host imports are still
pending; the native test host drives the shared asynchronous client adapter.

Shared host adapter (2026-09-25): `Config_Object_Client.VM` now accepts actual
compiled VM values for asynchronous Set and reconstructs Get results under the
program's registry. It preserves revision/stale/denied/uncertain outcomes,
keeps handles private, rejects nonpersistable ownership before IPC and leaves
type-mismatched reads pending rather than discarding them. 354 hosted adapter
checks pass; the existing 43 protocol and 130 object/bridge proof checks still
discharge. The adapter/client itself is regression-tested, not SPARK-proved.
The native KVM writer persists compiler-produced 41 then 42, and an independent
read-only KVM and TCG boots recover 42 as a VM value; SQLite/WAL/ext2 verification passes
without new revisions. General source-language Config imports and suspension
remain pending, as does enabling persistent settings in the normal desktop.

Language bridge hardening (2026-09-25): VM/native-object conversion now receives
the actual program registry and uses shared nominal-definition correspondence,
not local type-number equality. Hosted compiled-program tests cover shifted
IDs and reject same-ID/different-definition values in both directions. General
nested correspondence has 8,270 checks; the bridge/object/state-machine proof
has 130 discharged checks. Native-runtime library compilation passes. This
does not yet expose Config.Create/Get/Set as general aggregate CCL host imports
and does not make schema matching an authorization mechanism.

Latest native-object extension: the public Config-only writer now persists
both a scalar and nested Preferences/Mode/ActiveData declarations, with two
revisions each. It exercises a 5,000-byte nested string, the full 8,192-byte
text budget, signed integer extremes, variant shape changes, local malformed
value rejection and a service-side revision conflict. Independent KVM writer
and read-only reboot tests pass; the reader also passes under TCG with its
process-local type IDs deliberately shifted. SQLite independently verifies
both declarations and all four exact payloads; e2fsck is clean. No public client
contains CBOR or SQL. Logs `/tmp/cubit-nested-{create,reopen,reopen-tcg}.*`.
The current client/dispatcher/receiver/startup suite remains 891/98/358/20 checks,
with 43 focused SPARK checks discharged (not a whole-service proof).

CCL source-level aggregate host imports are still separate work. The present
`CCL_Config_Bindings` text inspector is not the native typed object client;
see the updated [shared-language integration roadmap](ccl-unified-documents-roadmap.md).

Latest validation: native Config storage startup passes in TCG and KVM with
the host CPU model. The KVM run's independent Linux SQLite check reads the WAL,
verifies format 3 and the exact empty schema, and read-only e2fsck passes. Live
`config-inspection` verifies backend nomination denial and now rejects failed
cleanup diagnostics. The original native Turso probe still passes exact typed
CBOR/history and e2fsck checks after extracting its reusable storage bridge.
Hosted regressions: 39 Rust tests, 472 real-Turso publication/recovery checks,
42 channel/85 worker-receiver scenarios, 891 client/98 dispatch/358 receiver
checks, 20 startup-envelope/worker-source checks, 10 CCL configuration tests. The focused
message/dispatch/startup SPARK report has 43 checks, all discharged; not a
whole-service, FFI, database or boot-security proof.

Live `config.svc/main.adb` now routes native-object requests and worker
completions through `Config_Typed_Service`, alongside volatile byte values and
authorized inspection. Trusted startup may select a private `config-storage.svc`
using `(role config-storage)`. That real worker opens its scoped Turso database
over native filesystem IPC. KVM/TCG boot attachment and an independent Linux
SQLite/WAL/ext2 initialization check pass. A normal client is denied backend
nomination in the live Config regression.

**The typed catalog starts empty and recovers on demand:** public durable Create
or authorized Open populate it through the asynchronous worker. Read-only Open
loads the persisted declaration and value without Write authority or client
type metadata; authority revision is rechecked before returning a handle or
lookup error. Canonical CBOR stays inside the storage worker and is imported
by Ada's shared type validator. Eager startup enumeration is not implemented.
Live client-to-worker typed
creation/publication now passes in both KVM and TCG. Default
desktop profiles have not enabled the worker; existing byte settings are still
volatile. The public native regression uses a Config-only application manifest
and independently checks the resulting database and filesystem. See
`userspace/services/config-storage/README.md` for the exact boundary and commands.

Schema persistence adds a 112-check focused SPARK proof and 29,478 codec
regressions. Real Linux-hosted Ada/Turso/SQLite tests restore both a large type
catalog and native value, including an unset object surviving reopen. Rust has
39 tests, including declaration conflicts, malformed metadata, shared worker
retirement and injected Create write/flush failures. See
`tests/ccl-objects/SCHEMA-PERSISTENCE.md`. Database format 3 is intentionally
incompatible with earlier experimental stores; no automatic migration/reseed.

The native direct-worker probe also passes two independent TCG boots with an
exact persisted type declaration: create + value 41, then recover that type and
commit value 42. SQLite independently checks both histories and read-only
e2fsck passes. Artifacts: `/tmp/cubit-schema-reboot-final.S8vTZD/run`.
This is actual CuBit filesystem I/O and Ada/Rust worker execution, not yet a
client Create message to live Config. The final close path now also rejects a
retired worker rather than asking it to checkpoint a failed session.

The private worker IPC layer now transports durable Create/Recover in an
eight-page native frame (0x0617) using the existing single-owner channel,
completion authentication and generation-checked grant discipline. No CBOR
crosses IPC. Metadata and value operations share terminal receiver failure;
the worker validates names and schemas only after an owned snapshot and release.
The metadata protocol/executor proves 50 focused checks; hosted real-Turso
channel tests pass 108 checks, including lost-response recovery, with an
independent SQLite oracle. Public Config client Create is now connected.

Implemented public-layer requirements: authorize Write_Config for the requested
namespace before acquiring the client type image; validate the owned type;
check catalog capacity/conflicts before storage submission; retain a saved
reply capability while cached reads remain serviceable; register/publish only
after a matched durable result. Recheck the caller's current authority revision
before issuing any handle after completion. Recovery may restore definitions,
never historical grants. Treat malformed storage as failure, not absence.

Public Create/Open validation: Linux-hosted end-to-end checks with real Turso
and independent SQLite; 891 client, 98 dispatch, 358 receiver and 20 startup
checks. Includes revocation/regrant while awaiting a durable result (no returned
handle), unauthorized/read-only/wrong-scope denial before metadata acquisition,
save-reply failure, malformed metadata, snapshot isolation from later caller
edits, busy admission, duplicate completion, and idempotent creation without a
second value load. The catalogue/store focused proof has 157 checks and the
message/dispatch/startup proof has 43, all discharged. Neither proves the
new mapping/orchestration shell or the database. Native Config and the client
library compile. Live creation now has an application regression.

Native startup was rerun after public Create integration: KVM with
`QEMU_CPU_MODEL=host` passes attachment/readiness, independent SQLite/WAL and
read-only e2fsck (`/tmp/cubit-public-create-startup-host.*`). The initial
default-Broadwell KVM run hit the already documented getrandom CPU-identity
restriction and failed closed; no entropy fallback or check bypass was added.
This startup test does not yet send a public Create request in the VM.

The subsequent `config-objects` regression DOES send public Create/Get/Set
requests inside CuBit (KVM and TCG PASS). The app has no FS/worker authority;
it checks exact native values, conflict preservation, read-only handles,
wrong-namespace denial and idempotent Create. The independent SQLite/WAL oracle
checks exactly one type and two revisions, and read-only e2fsck passes. Logs:
`/tmp/cubit-config-objects-final.*`, `/tmp/cubit-config-objects-tcg.*`.
Four oracle unit tests (including 11 corruption subcases) also pass.

Public recovery now passes two independent TCG boots: a writer creates the
type and commits native integers 41 then 42; the next boot starts an empty
Config catalog and a different app with only namespace Read. Open recovers
42/revision 2; Create, writable Open and Set are denied. Missing/wrong-schema
outcomes and cached reopen also pass. A separate KVM reader boot passes against
the same seed. SQLite/WAL and read-only e2fsck validate both boots, including
exactly two revisions after read-only recovery. Artifacts:
`/tmp/cubit-read-open-reboot.sDhLcv/run`, `/tmp/cubit-read-open-kvm.*`.
This is quiescent restart recovery, not arbitrary power-cut atomicity.

The hosted recovery suite also revokes/regrants authority while negative or
successful responses are pending, loses the metadata response, verifies the
saved reply is unwound, and replaces the retired owner. Late old completions
do not finish the new request; independent SQLite still sees only the original
two revisions. The suite uses modeled kernel IPC; it complements rather than
replaces the native two-boot regression.

This native run exposed and fixed two integration bugs missed by modeled IPC:
the storage worker incorrectly expected tag zero on its policy-minted endpoint
(the kernel stamps the holder PID); and the Rust entrypoint omitted Ada library
elaboration, leaving a protocol constant zeroed. The real source predicate is
now tested; GNAT generates the standalone-library initialization closure, called
once before any Ada export. No validator or authority check was weakened.

Current devmgr starts/waits for filesystem.svc, then starts and seeds Config
from system.ccl, then starts procmgr later. Earlier notes saying Config starts
before filesystem were stale. Filesystem readiness alone does not establish
backing-volume availability or durability. Keep boot seeds independent of
persistent attachment and do not introduce a Config/storage dependency cycle.

## Audit findings

0. **Fixed during startup integration:** failed-launch cleanup reused the
   filesystem call's in/out message when contacting Config. That message had
   already become a reply, so Config rejected the cleanup. Each service now
   gets a freshly constructed revocation request before the suspended child's
   PID is released. The same helper handles failed worker attachment. Native
   regression now treats cleanup rejection as failure. This does not settle
   the broader sender-incarnation work or change TLS/network policy handling.

1. **Fixed in the adapter:** opening previously recreated missing tables with
   `CREATE TABLE IF NOT EXISTS` and reseeded an empty format marker. Admission
   now initializes only an empty application schema, creating all tables and
   the marker in one SQL transaction. Existing stores need exactly the expected
   tables and one supported format row; foreign application objects and partial
   stores are rejected. This is not a complete verification of every table
   constraint or of hostile SQLite internals.
2. **Fixed in the adapter:** a stored zero revision head looked like an absent
   collection. Nonpositive/malformed/duplicate heads and orphan revisions now
   cause errors. This does not authenticate history or prevent rollback of an
   entire externally replaced database.
3. **Fixed launcher gap:** `parseAndSendACL` reports FS/Config installation
   failure, including unavailable Config transfer authority and malformed
   acknowledgments. The suspended child is not resumed; partial policy is
   removed before killing it to avoid PID-reuse cleanup races. A native test
   makes Config reject scope rights and verifies launch failure before normal
   allowed/denied clients run. Existing TLS/network approval is untouched.
   General required/optional grant declarations remain separate work;
   missing/malformed policy still grants nothing.
4. **Lifetime/capacity:** Config_Authority has 32 profile slots and 16 rules per
   subject; dynamic PIDs do not remove these bounds. Capacity denial needs
   visibility, not a wildcard fallback. Cached authority still uses raw PID;
   service-side incarnation keying awaits the kernel agent's ABI. procmgr's
   reset-before-resume reduces reuse risk but is not that final binding.
5. **Contexts:** a syntactically valid namespace/context in a worker frame is
   not authority. Derive both from the client's issued collection handle and
   authorize before queueing. A schema digest identifies shape, not publisher
   identity. No persisted handle or approval becomes a live grant on reload.
6. **Value migration/bounds:** old Config bytes need an explicit opaque schema
   or per-collection migration, not automatic UTF-8 conversion. A 16 KiB CCL
   image/13,120-byte encoded bound does not implement the proposed 1 MiB general
   Config limit. Multi-collection memory and history budgets remain pending.
7. **Durability:** SQL transactions and NVMe flush do not make ext2 allocation
   metadata power-loss atomic. Distinguish clean reopen, process death,
   filesystem-service failure and power loss in tests and claims.

## Historical implementation checkpoints

The following records preserve the sequence and proof scope of earlier work.
Statements about pending integration below describe that checkpoint, not current
status; use **Current boundary** above for what is implemented today.

### Original pre-attachment checklist

| Boundary | Required behavior |
|---|---|
| Launch | Trusted boot plan selects backing location; narrow filesystem scopes and exclusive database/WAL handles |
| Attachment | Held endpoint/authenticated completions identify worker; session/token only correlate work |
| Transfer | Snapshot once into owned memory before validation; no grant/client pointer reaches Rust |
| Scheduling | Dispatcher continues cached reads; synchronous Turso adapter runs only in the worker |
| Publication | Matching successful durable receipt publishes candidate; ambiguous outcome requires recovery, never blind retry |
| Restore | Validate schema, context and generation before cache publication; damaged storage is not absence |
| Activation | Durable intent does not imply a live device accepted a setting or mint new authority |

Start with one explicitly eligible, non-boot-critical typed collection. Add
trusted worker launch/endpoint installation and bounded message routing, with
one pending operation per collection and process-wide nonreused request tokens.
Keep bootstrap settings readable and report attachment state. Reject writes
while initial restore/recovery is unresolved. Route kernel completions separately
from unsolicited client IPC; arbitrary messages must not acknowledge storage.

Required regressions: unauthorized/wrong-context access, wrong worker/session,
late completion, dropped acknowledgment, worker death, denied filesystem scope,
exclusive-owner conflict, malformed storage and failed flush. Add visible
outcomes before making Settings depend on durable Config.

`Config_Worker_Channel` now supplies nonblocking submission/completion, private
snapshots, response validation and terminal grant retirement. It passes 32
hosted fault scenarios and native library compilation. Connecting the actual
dispatcher and worker receive loop remains pending; ordinary incoming messages
must never be treated as authenticated kernel completions.

`Config_Worker_Receiver` now supplies the worker-side transfer adapter: trusted
source/tag admission, endpoint-derived grant ownership, owned request snapshot,
release before storage I/O, generation-checked response reacquisition and
terminal recovery on uncertain delivery. Its 19 hosted fault scenarios pass,
and a concrete instance compiles against the native runtime. The actual Turso
hosted test exercises both adapters and recovers revision 2 after deliberately
denying response acquisition, with exactly two revisions checked independently
by SQLite. This does not install live worker authority or implement its schema
registry/receive loop. No syscall, pointer-lifetime or database proof is claimed;
the existing protocol/executor's 51 focused SPARK checks still pass.

`Config_Collections` now provides approved schema registration and subject-bound
collection handles, reusing Config_Authority rather than inventing a second ACL
engine. Every resolution checks minted rights, current namespace scopes and
the grant installation revision; policy replacement/regrant cannot revive an
old handle. Machine context zero is the only supported context. 159 hosted
checks and 81 focused SPARK checks pass, including the ghost successful-access
property and nonreusing token issuance. The native library compiles. Registration
is not durable creation, and this catalog is not yet exposed by live Config.
The next dispatcher join must authorize before staging native objects and
acknowledge create/set only after durable publication. Sender incarnation,
schema distribution/persistence and actual worker attachment remain outstanding.

The dispatcher join is now implemented as `Config_Typed_Store`: authorized
native-object Get/Set, stable collection routing, one pending storage request,
cached reads and validated completion/publication. All 97 store checks and 159
handle checks pass; the combined focused proof discharges 147 checks, including
unchanged publication during Set and zero output for denied Get. The real
hosted Turso test exercises this join through modeled grants/completions and
recovers after response loss without duplicate writes (64 checks plus SQLite).
Native compilation passes. This is still not Config's live message loop or
durable schema creation; those and worker startup remain the next integration.

The public native object boundary now has `Config_Object_Messages` and a
nonblocking `Config_Object_Client`: schema-bound Open/Get/Set/Close, native
CCL image grants, canonical operation-specific replies and checked retirement.
473 hosted model checks and 22 focused message/descriptor proofs pass. The
client syscall adapter is not proved. Incoming mapped worker/client reads now
use volatile views before owned validation; this is not an atomic-snapshot or
memory-ordering proof. Native compilation is pending the other agent's build
window. The service must still dispatch these operations and preserve reply
authority while a Set waits for authenticated storage completion.

`Config_Object_Dispatch` now implements owned-message Open/Get/Set/Close and
deferred reply decisions over the typed store. Authority checks precede Set
capacity checks. Without trusted reply reservation, no Set is staged; accepted
work emits no immediate reply. Stale receipts/background restores cannot finish
the saved client operation. 98 dispatch checks and all 41 focused message/
dispatch proofs pass, including no immediate Set success. The real hosted
Turso fixture sends public messages through this dispatcher (76 checks plus
independent SQLite). Actual grant acquisition/return, saved kernel reply moves,
the live service loop and startup are still pending. Native lock attempt returned
75 while the other agent's suite holds it; no native build started this chunk.

`Config_Object_Receiver` now provides the thin grant/reply shell, with a native
instance selecting actual acquire/return/save/reply syscalls. It snapshots and
releases client input before staging, preserves a dedicated single-use reply
slot across cached reads, and stops admission after a failed grant return.
The real hosted Turso test now passes through this shell too (160 checks and
independent SQLite), asserting neither outer client nor inner worker mappings
remain borrowed during database calls. Transport is modeled; this is NOT a
native IPC run. The combined catalog/authority/store proof rerun has 151 checks,
none unproved, and the message/dispatcher proof has 41. Native compilation of
the new shell/client remains deferred (lock exit 75); live main was not edited
during the other agent's kernel/thread suite.

`Config_Object_Service` now owns this event-driven pipeline: typed store,
receiver, worker channel, monotonic tokens, automatic submission and completion
dispatch. It unwinds a saved reply on enqueue failure and retires uncertain
worker transport without retry. Cached reads remain available (explicitly stale
after loss). Tokens outlive replacement State objects within the single
process/package instance. Real hosted Turso drives this API (191 checks plus
SQLite), including delayed replies, invalid/late completions, attachment rules,
queue rejection, and unconfirmed retirement. Live main/worker startup and
durable Create are STILL not implemented; native compilation awaits the shared
build window. This orchestration is tested, not SPARK-proved.

Approved schema provisioning now has a native data representation in
`CCL.Objects.Schemas`: bounded names, explicit scalar IDs, ordered product/sum
definitions and a root, without exposing the private Ada Registry layout.
Import goes through Types.Define and Objects.Bind. The raw image contains no
invalid Ada enum/count representations, pointers, code or live handles.
4,223 hosted checks and 44 focused SPARK safety/termination checks pass. These
are not a proof of semantic round-trip equivalence or provenance. The candidate
Read success/binding postcondition was removed because Bind lacks a functional
postcondition; none was assumed or suppressed. Adding/proving that small
builder contract can be done when shared CCL edits are safe.

The real hosted Turso fixture uses an independently imported worker binding
(193 checks + SQLite). A claimed key remains just a label until an approved
source provisions it; existing binding conflicts must be rejected. The actual
authenticated provisioning IPC/ack, native worker startup, persisted schemas
and durable Create still need implementation. No hard-coded production schema
was added as a shortcut around those requirements.

Worker provisioning is now part of the actual receiver protocol (0x0615),
authenticated before grant acquisition, snapshotted/released before import,
with idempotent same-binding registration and rejection of conflicting keys.
The old out-of-band Contract parameter was removed. Data operations select an
already approved binding from the private worker catalog; unknown keys never
reach the database. Config's service owner drives this handshake asynchronously
before each restore, with separate increasing schema/load tokens on one loan.
42 channel and 47 receiver scenarios, 198 real hosted Turso checks plus SQLite,
and existing object-client/storage-channel regressions pass. Native compilation
of both client/service and worker instances against CuBit's runtime now passes.
Live main, production worker startup/boot attachment and durable Create remain
pending; no production boot behavior changed in this round.
