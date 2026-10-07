# One CCL language, typed documents, separately authorized actions

Status: roadmap, not a claim that all current `.ccl` consumers accept the same
programs. Current manifest, service-catalog, boot configuration, image and theme
loaders have specialized declaration grammars. Several delegate individual
fields to the real CCL evaluator; this does not make their outer forms ordinary
REPL programs.

## Intended model

Use the same CCL language and type system to construct configuration objects,
executable manifests, service catalogs, artifact catalogs and system/image plans.
Functions, bindings and composition should work consistently; Lisp and BASIC
remain two frontends for the same meaning. Domain types and constructors should
be library/schema definitions, not another collection of parser-only languages.

The evaluation environment determines available bindings, effects and resource
budgets. A declaration environment is deterministic, bounded and effect-free:
no ambient filesystem, network, current clock, random state or service discovery.
External inputs are explicitly supplied and recorded. Evaluation produces an
owned value of the required document type; it does not execute the plan.

Consumers validate the result and act under independently supplied authority:

* A manifest describes requested authority for one executable. It cannot mint
  grants; compilation emits checked ELF metadata tied to that executable.
* A service catalog describes interfaces/bindings. Names and schema digests do
  not authenticate publishers or grant discovery rights.
* An image plan describes artifacts, placement and startup/configuration inputs.
  A separately authorized realizer accesses files and runs declared build steps.
* A Config value describes desired settings. Storing, migrating and activating
  it are distinct operations; correct types never substitute for authorization.

## Persistence is a representation, not another language

Config's public load/store boundary is a **typed owned CCL value**, not a bag
of opaque settings bytes. Opaque bytes are one explicit data type alongside
other persistable values, not the representation all callers must manually
encode into. Establish this boundary before enabling the persistent Config
worker; the current byte store and scalar-profile experiment are bring-up tools,
not the target API.

Persistable CCL values should use the same schema identities as the corresponding
typed interfaces. Records/variants/collections may be persistable recursively;
live authority, handles, borrowed storage, callbacks and process state are not
silently serializable. An ordinary data record describing a desired resource
binding can be saved, but activating that recipe requires fresh binding and
authorization. This is not persistence of a value whose type is `Resource`,
nor persistence of a live resource's identity or granted rights.

The desired law is semantic `decode<T>(encode<T>(value)) = value` for the supported
persistable subset. It does not preserve memory addresses, live authority, source
formatting or running execution. Require explicit schema migration; no executable
constructors or automatic activation during decoding.

CBOR can encode the typed value; Turso can store that payload plus revision,
context, schema and provenance metadata transactionally. Neither the database nor
CBOR defines CCL's type system. Small experimental scalar codecs must not grow
into an independent Config-only type system.

### Typed-object boundary

* Reuse `CCL.Types` descriptions for primitives, products (records/tuples), and
  sums (enums/variants). Add bounded collection and explicit byte-value support
  to the shared CCL model where necessary, not only to Config. Support expands
  with the language; unsupported values must produce an explicit outcome.
* Persist nominal schema identity/version and validated value data. A local
  `Type_Reference`, VM tag, string-region allocation, or raw Ada/Rust record
  image is not a durable representation. Resolve portable schema identity to
  the receiving CCL registry and reconstruct owned values with fresh storage.
* Typed reads request an expected type and return either that type or a typed
  missing/mismatch/unavailable outcome. Typed writes validate the entire value
  against the collection's declared schema before acknowledging publication.
  No implicit byte-to-text, integer-to-text, or enum-to-integer conversion.
* Persistability is checked through the complete value/type graph. Reject live
  handles, authority, borrowed references, pending operations and callbacks,
  including when nested inside a record or variant. Schema validation does not
  itself prove unrestricted ownership or permission to persist a value.
* Decoding is data reconstruction, not evaluating saved CCL source or invoking
  constructors with effects. Named resource descriptions may be ordinary data;
  activating them and acquiring authority are separate authorized operations.
* The storage worker can transactionally store schema-tagged CBOR payloads in
  Turso. The shared CCL codec and Config's schema/authority checks define what
  they mean; neither SQL column types nor CBOR's generic value kinds replace
  CCL's type system. An inspector uses the same descriptors to display fields
  and alternatives rather than presenting a generic blob editor.

Initial implementation: `CCL.Objects` now provides a shared pointer-free native
image for primitive data and nested products/sums using `CCL.Types`. Local
validation uses a trusted schema binding without reconstructing objects. The
`CCL.Objects.Values` bridge handles the current narrower VM/host subsets;
`Config_Objects` supplies a single-owner typed slot with staged commits,
session/request-correlated publication, stale-cache visibility and explicit
recovery after uncertain outcomes. It cannot publish merely because a write
was submitted. A Linux-hosted real Turso worker fixture exercises dropped-ack
recovery through this state machine. See
[tests, layout and proof scope](../tests/ccl-objects/README.md).

**Current integration (2026-09-25):** this is now a live Config IPC path.
`Config_Object_Client` supplies native Create/Open/Get/Set/Close with scoped
authority, schema identity, revision conflicts and owned grant-backed objects.
The native `config-storage` worker uses Turso on ext2/NVMe; CBOR is confined to
the worker/database boundary. Public clients neither encode nor decode it.
Independent boots exercise read-only recovery from stored declarations with
freshly authorized handles. The slot takes an owned snapshot before validation,
so serialization-free does not mean zero-copy. See the
[native tests](../tests/config-object-client/native-app/README.md) and
[codec boundary](../userspace/ccl/persistence/README.md).
Schema keys currently come from the trusted caller,
not a new digest-generation or discovery implementation. Larger native aggregates
do not expand the current VM's executable value subset by themselves.

### Remaining application/language integration

Discovered type foundation (2026-09-25): a trusted host can publish a root data
type into `CCL.Catalog` with `Publish_Type`. Shared `CCL.Types.Import_Definition`
imports only reachable dependencies, translates IDs, reuses identical names and
rejects conflicting definitions/capacity exhaustion atomically. The compiler and
analyser take one catalog type snapshot before parsing; source need not repeat
the advertised declaration. This is description visibility, not call authority,
publisher authentication, schema-key generation or a new Config-only type model.
No per-node registry copy or live registry mutation while executing. See
[discovery tests](../tests/ccl-type-discovery/README.md).

Discovery may include types the current VM cannot instantiate. Valid metadata
alone must not reject an otherwise executable program; the verifier checks
supported value shapes at every executable use. Hosted roundtrip/rejection
tests and the native shifted-registry recovery fixture cover this distinction.

The shared `Config_Object_Client.VM` adapter now connects current VM values to
the asynchronous native client. It retains private handles/approved bindings,
validates before submission, decodes owned Get completions using nominal type
correspondence, preserves service outcomes and never waits internally. Type
mismatch preserves the pending native result; uncertain transport poisons the
client rather than retrying a possibly committed write. Hosted tests compile
real CCL programs, and the native writer/reopen fixtures now use this adapter.
This is host-side groundwork, not a claim that a CCL script can yet call typed
Config.Create/Get/Set directly. The wider host-call/suspension tasks below remain.

The native object protocol and CCL source host-call ABI are different layers.
`CCL.Host_Values` now carries owned native objects as well as integer, Boolean,
bounded text and handler arguments. The compiler/CCLB v6/optional native VM
store support schema-pinned object imports, locals, field projection and general
matches. Interpreter aggregate construction and native returns were implemented;
aggregate construction and string operations in bytecode were pending when
this was written. (The interpreter was removed 2026-10-05; every program now
runs on the VM.)
`CCL_Config_Bindings` still exposes the old
read-only text inspection operations; it is **not** the typed persistence API.
Desktop default byte settings are still volatile and do not enable the worker.

Before spelling the full `Config.create<T>/get/set` model in CCL source:

Implemented foundation (2026-09-25): `CCL.Types.Correspondence.Resolve` matches
approved nominal definitions into an existing local registry, including nested
products/sums and shared subgraphs. It requires matching names, shapes,
field/alternative order and recursively matching payloads, not numeric-ID
equality. The VM/native-object bridge now requires the program's actual registry
and translates accepted variant IDs in both directions. It does not install
types, mutate a schema, authenticate a publisher, or grant an import. This removes
the old same-registry assumption. General aggregate host-call metadata and owned
values have since been implemented, without making live resources persistable.
See [object bridge tests](../tests/ccl-objects/README.md).

1. Extend the shared import contract/value model with approved nominal schemas
   and owned persistable objects, including type-number translation between
   registries. Do not create Config-specific record types or flatten records
   into text. Host/VM capabilities, handlers and borrows remain nonpersistable.

   Foundation added 2026-09-25: `CCL.Objects.Catalog` retains approved bindings
   as compact key/root entries over one shared type registry. Rebinding a key,
   replacing a nominal definition or exceeding either capacity leaves the view
   unchanged; repeated publication is idempotent. This is host-owned metadata
   for one authorized discovery view, not a schema-key-based permission system.
   Hosted checks and focused contracts are documented in
   [schema catalog tests](../tests/ccl-schema-catalog/README.md). The native
   discovered Config fixture now obtains its contract from this catalog.
   General aggregate host-call declarations/values and compiled field/match are
   now implemented; see the Config integration checklist for current validation.
2. Resolve the expected schema during analysis/linking through an authorized
   interface binding. A received schema or digest is not proof of identity or
   authority. Preserve exact type/grant matching in the analyser and CCLB.
3. Map returned Config handles to private host-owned bindings, not ordinary
   integers a script can forge. Preserve read-only rights, revision conflicts,
   missing/mismatch/unavailable outcomes and handle retirement.
4. Suspend/resume for noncancellable persistence work through the shared async
   import lifecycle. A slow commit must not block a Workbench GUI thread or
   consume a busy-poll loop. Cancellation cannot imply rollback of a commit.
5. Use these shared bindings in a native CCL demo and the Workbench, then migrate
   existing settings to declared objects. Keep bootstrap Config sufficient to
   locate storage; selecting a backend must not require querying that backend.

The new native-machine overloads in `Config_Object_Client.VM.Calls` connect
these object imports to the existing asynchronous Create/Open/Get/Set/Close
client. They preflight the actual suspended call, host-selected binding and
complete result schema before issuing an operation. Writes export only the
current argument under the collection's retained contract. Typed read/write
outcomes resume once; stopped machines and mismatched bindings/types preserve
the completion for its owner to drain. This is shared dispatch machinery,
not a parser special case or a public resource-returning factory yet.

Remaining source/API boundary: a `Config.create(type)` application needs an
explicit namespace/context and approved schema binding, not a globally named
singleton selected only by its type. Several collections may share one type.
The type argument is static metadata, not authority; create can still be denied
at runtime. The result must carry a nonforgeable, host-owned collection binding
whose read/write/close operations retain that type and its granted rights.
CCL's current owned-import machinery consumes/borrows existing resources;
returning a newly acquired resource needs explicit verifier/host lifetime rules.
Do not substitute an unrestricted integer handle or a serialized schema string.
Specialization should extend ordinary typed interface metadata, not hard-code
Config record names or parsing behavior in the compiler.

The same specialization must be able to advertise typed stream operations. A
collection specialization for `Preferences` can describe one-shot
`create/get/set/close` plus separately authorized `watch` and bounded snapshot
operations returning `Stream<ConfigChange<Preferences>>` or
`Stream<Array<Preferences, N>>`. `T` is the same approved nominal type used by
Get/Set; streams do not introduce a second Config type system. A stream element
uses a generated stable wire schema backed by canonical CCL object CBOR, never
the local native object-image layout. `N`, capacity, byte budget and delivery
policy are compile-time contract data and must be visible in discovery/type
cards. Subscription remains a distinct authority from collection read/write.
See [typed object values and batches](typed-ipc.md#typed-object-values-and-batches).

Opaque resource metadata foundation (2026-09-25): `CCL.Types.Resource` describes
a nominal live reference with named type parameters, rather than pretending it
is a data record or integer. A collection parameter can identify its Settings
type without embedding a Settings value in the reference. Type import,
correspondence and CCLB type-description encoding preserve this distinction.
No resource instance is created by publishing its description. Current source
constructors and VM data imports/locals still reject these executable uses;
factory returns and their ownership rules remain to be implemented.

The data persistence boundary rejects resources transitively, including an
inactive sum alternative containing one. Native and CBOR schema export retain
only the root dependency closure, so seeing an unrelated resource type does
not prevent persisting ordinary data or leak that description into the saved
schema. The new hosted resource suite has 62 checks; its six-unit SPARK slice
discharges 281 checks. That is evidence for those checked properties, not a
proof of authority provenance, lifetime soundness or the complete CCL language.

The asynchronous host must also retain the same program/machine/run association
until the client completes or grant retirement is confirmed. A matching import
binding number is not a run generation; stop is not cancellation or rollback.
Admission of a new run cannot recycle storage still visible to an old operation.

Host lifetime foundation (2026-09-25): `CCL.Resources` now reserves typed slots
before acquisition, publishes opaque run-scoped references after validated
completion, and issues nonreusing operation tickets. Stop revokes script access
without discarding pending calls; host-only cleanup can close a resource that
arrived after Stop. Reclamation requires a drained retiring lease, and a new
run waits until the registry is empty. The host still supplies distinct context
identities, authenticates completions and establishes external cleanup/grant
retirement. The registry has 65 proved checks and 38,206 hosted checks; async
Config integration adds 135 modeled-IPC checks. See
[resource lifetime tests](../tests/ccl-resources/README.md).

The next layer must connect this to generic owned host values/factory signatures
and source/bytecode move/borrow checking, then Config's approved schema binding.
Do not treat resource-bearing outcomes as serializable data objects. The lost
Create receipt concern is now handled at the existing native reply boundary:
Config closes a newly minted handle when kernel reply delivery fails. A
successful kernel delivery puts the completion in the caller's reserved queue
(or completes its synchronous handoff); the host must still drain it and close
the resource after Stop. No new session protocol is required for that native
case. Local grant retirement is still not evidence that a remote handle closed,
and this does not establish future network-IPC acknowledgement semantics.
Public source factories and Workbench integration remain incomplete.

Dynamic ownership transfer (2026-09-25): the VM now allows a moved value to
initialize a dynamic local of the exact same ownership type, including
move-only and must-handle types. Operand metadata retains the ownership tag
through stack operations and branch joins; a literal cannot mint ownership.
The ownership control-flow pass tracks the new owner rather than restricting
all dynamic locals to unrestricted values. Halt rejects moved values abandoned
below the returned operand. Local-argument imports now account for their
completion operand in stack verification, matching execution. These changes
are generic VM machinery, not yet resource-valued imports, source factories,
or a Config-specific integer-handle shortcut. See
[owned-local tests](../tests/ccl-owned-locals/README.md).

Opaque VM resources (2026-09-25): the VM has a distinct Resource value carrying
a private host-registry reference, never a service handle or persisted object.
The trusted completion bridge checks liveness and full nominal correspondence
before moving a factory result onto the stack under its declared ownership type.
Resource arguments require local borrow/move semantics; ordinary scalar result
completion, initial-local injection, copying and persistence conversion reject
them. Sequential owned calls now recycle only completed import lifecycles.
Hosted Config-client integration exercises acquire/read/close with the service
handle confined to the client. Source factories and portable resource signatures
remain to be connected.
CCLB is now v7; resource-valued portable imports remain explicitly rejected
until the approved signature/linkage representation is implemented.

Receiver/data imports (2026-09-25): a VM import may now declare a nominal
resource receiver separately from its ordinary typed operand. The receiver
comes from a checked owned local; the data argument comes from the operand
stack. Both type paths are checked independently. The native-object machine
supports resource acquisition, submission acknowledgement, aggregate completion
and exporting only the data argument. An offered borrow cannot consume an
object completion before submission is acknowledged. No resource is embedded
in a persistable product. Fixed-client Config adapters reject these owned calls
until a resource-aware host can pin the live reference/client association;
binding numbers alone are not sufficient. The dedicated native receiver fixture
provides this association for one client and checks the entire acquire/read/
write/read-back/close flow. It is not a public source factory or Workbench pool.

Resource-bound Config client (2026-09-25): `Config_Object_Client.Resources`
now owns the stable client/reference association instead of relying on test-host
convention. Acquisition reserves before submission; the nominal resource's value
parameter must match the approved collection contract. Get/Set/Close require its
exact live reference, not merely a compatible type. Stop and per-lease retirement
leave pending calls drainable without publishing or reviving a resource. Cleanup
closes known handles, waits for grant retirement, and then reclaims the lease.
The same limited client storage is reusable only after that sequence, preserving
its completion-token floor. Uncertain acquisitions with unknown remote handles
and uncertain closes are quarantined: reclaiming those requires service-lifetime
reconciliation, not an invented success or grant revocation alone.

The native receiver fixture uses this shared client. Public source factories,
portable resource signatures and the reusable Workbench event dispatcher still
need integration. Hosts still supply distinct registry contexts, process-wide
unique completion tokens, authenticated kernel receipts, serialized calls and
stable storage lifetimes. This is not a proof of the IPC shell or a new source
of authority. Endpoint grants and Config subkey policy continue to enforce access.

Approved resource policies (2026-09-25): the interface catalog now accepts atomic
publication of nominal resource definitions and their ownership/disposition
rules. Transition targets are nominal names; a bounded layout pass assigns
local ownership tags for the selected dependency closure. Weaker replacement
policies and conflicting target definitions are rejected without partial
publication. The focused hosted suite has 1,568 checks; the two-unit SPARK slice
discharges its runtime checks and failure-atomicity contracts. See
[resource policy tests](../tests/ccl-resource-policies/README.md) for the precise
scope. Portable resource signatures and source lowering do not consume these
policies yet; this is metadata infrastructure, not a new authority grant.

Resource signature/source analysis (2026-09-25): host signatures now distinguish
resource arguments/results using canonical nominal names, separate from stored
object schema identities. Source analysis requires the visible resource type
and its approved ownership policy, and rejects incompatible resource types.
Host-value persistence conversion proves that resources cannot be accepted as
stored objects. Interpreter admission rejected resource-bearing calls before any
effects (the interpreter was removed 2026-10-05); source compilation still rejects them pending ownership-tag lowering
and portable linkage. This is not yet factory execution or generic type-argument
specialization. The [resource signature tests](../tests/ccl-resource-signatures/README.md)
exercise those boundaries without Config-specific compiler logic.

Source resource lowering (2026-09-25): analysis retains the approved policy
snapshot alongside its type registry. Compilation assigns bounded ownership
tags, moves resource-valued expressions, and borrows/moves owned locals for
resource imports. The VM verifier rejects invalid ownership flow before a
resource program is accepted. Linkage recomputes the expected ownership layout
from the independently supplied catalog, checks complete nominal correspondence
and the operation's local association, and installs bindings only after the
entire program passes. Calls to one service operation on distinct locals do
not accidentally share a receiver binding. The native Config resource fixture
now builds its open/get/close program from source through this path. General
type-argument specialization, portable resource
encoding and the Workbench's resource dispatch remain subsequent work.

The compiler/catalog/host-value/object-conversion slice discharges 340 SPARK checks with
none unproved or justified. Central checked AST access closes three formerly
unproved child-reference conversions; this is safety evidence, not a proof of
compiler semantic preservation. The resource-signature suite separately checks
352 positive/negative cases, including ownership and atomic linkage rejection.

Source receiver/data calls (2026-09-25): host descriptors can name a separate
owned receiver while retaining an unrestricted typed data operand. Source spells
the receiver first, e.g. `(collections.set collection value)`; the catalog's
parameter count describes data operands only. The analyzer checks both nominal
types, the compiler lowers the receiver to an owned local and data to the value
stack, and trusted linkage independently checks receiver correspondence. The
existing VM verifier rejects consumption of the receiver during data evaluation.
There is no Config-specific syntax or separate ownership checker. The native
Config receiver fixture now compiles its create/read/set/read/close sequence
from source instead of constructing instructions by hand. Generic type arguments
and a production Workbench resource dispatcher remain pending.

Config ergonomics: simple keys should still feel like typed key/value access.
A collection established for `Preferences` accepts `Preferences` on Set and
returns that type on successful Get, with explicit typed failure outcomes.
Returned data is an ordinary copyable snapshot, independent of the collection
handle's lifetime. Ownership/borrowing applies to live resource handles, not
every Config datum. Creation does not grant namespace authority; schema
validation does not replace authorization or domain-specific validation.

Implementation constraint: keep approved schema definitions in a shared bounded
catalog, with compact checked references in individual import declarations.
Do not embed an entire registry or 16 KiB native image into every AST host-call
node, import argument, or VM stack slot. The current compiler copies import
descriptors into multiple structures; blindly widening them would multiply
memory use before any script executes. An owned aggregate store must have an
explicit lifetime and checked references in the VM, not a
Config-only pointer escape. The client adapter above retains one owned native
completion and can reconstruct currently representable VM values without an
extra native-image copy on result consumption.

Nested native objects can be validated independently of that language work;
they do not justify claiming arbitrary aggregate host imports are implemented.

## Sequence and acceptance

1. Specify the persistable value subset and exact schema identity/descriptor
   binding using the existing CCL type/interface model. Keep ownership and
   authority-bearing types excluded until their lifetimes have an explicit model.
2. Define a bounded deterministic CBOR profile and shared golden vectors. Test
   malformed lengths, nesting, duplicate fields, unknown tags, schema mismatch
   and trailing data. Establish round trips across Ada/CCL and other adapters.
3. Give Config a typed owned-object storage boundary. Evaluate CBOR payloads in
   the hosted database without adding SQL or database representations to CCL.
4. Replace declaration-specific construction with ordinary CCL constructors and
   required result types, one consumer at a time. Retain the consumers' semantic
   validation. Remove obsolete parsers instead of permanent compatibility modes.
5. Require Lisp/BASIC semantic equivalence and ordinary expression reuse for
   each migrated document type. Verify that evaluation cannot perform effects
   and realization/activation cannot amplify supplied authority.

Native database integration separately requires Rust platform support and real
storage durability. This roadmap must not turn Config loading into arbitrary
code execution or make runtime configuration depend on evaluating source text.

First storage experiment: the [hosted Config database](../tests/config-turso/README.md)
stores one bounded scalar-profile CBOR payload per transactional revision and
cross-checks a shared byte fixture with the pinned Ada CBOR library. Its schema
digest identifies only that experimental codec. This is preliminary evidence
for steps 2–3, not completion of the shared CCL schema/persistence model.

Related: [Config](config-contexts-and-inspection.md),
[packages and images](ccl-packages.md),
[interface descriptors](ccl-interface-descriptors.md),
[CBOR evaluation](ccl-cbor-evaluation.md).
