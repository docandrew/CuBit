# Configuration contexts, profiles, and inspection

Architecture update (2026-09-26): [declarative configuration and mutable
state](config-declarative-state.md) defines CCL as desired intent, Config as the
active realization and Turso as revision/application-state storage. Do not add
independent durable scalar overrides of CCL-managed values. Historical backend
evaluation notes below are retained as evidence, not the current integration plan.

## Invariants

Configuration names identify resources; they never confer authority. A process
needs an endpoint capability **and** the Config operation/scope grant issued to
it by the trusted launch path. Signing, profile names, and human identities may
inform issuance decisions, but cannot replace a grant. There is no superuser
account or implicit authority obtained by selecting a profile named production.

Keep three independent concepts:

* **Namespace** identifies a settings collection owned by an authenticated
  application/service identity. Reserve `cubit.*` for platform collections;
  third-party collections use reverse-domain names. Namespace registration and
  ownership enforcement are planned, not implied by the spelling of a key.
* **Profile** is a versioned saved configuration, such as development, test,
  production, or a person's desktop preferences. Profiles contain data, not
  executable closures, kernel handles, or serialized authority.
* **Context** is the authorized instance where settings are applied. Two live
  service instances may use different profiles simultaneously. Machine,
  profile-backed, and temporary session contexts do not create separate
  authorization systems. An application receives a context-bound Config handle;
  it must not choose a foreign context merely by supplying its name.

Do not put people into paths like `users.jon.cubit.display.theme`. Configuration
contexts are useful without OS user accounts. Secrets stay in the secrets
service; any reference to a secret still requires separate resolution authority.

## Collections and ergonomics (design, not yet implemented)

Follow the [unified CCL document roadmap](ccl-unified-documents-roadmap.md): Config
objects should use the CCL/shared interface type model, not a separate database
type system. Typed CBOR is a possible payload encoding, not its schema or source
language. Pure construction, authorized storage and activation remain separate.

Declare setting types, descriptions, defaults, versions, and permitted scopes.
Unknown fields are rejected unless an explicit extensible collection permits
them. Reading, updating, creating entries, deleting, and managing contexts are
separate operations; the old Config write/upsert right is not yet that model.

An authorized collection handle supplies namespace and context once. Apps then
read local names such as `theme`. Settings may edit Desktop's collection through
delegated authority without becoming its owner. An inspector can have explicit
global read authority without any write or administration authority.

Examples: `cubit.display.theme`, `cubit.display.wallpaper`, and
`cubit.display.layouts.desk`. A layout is one typed object containing named
monitors, coordinates, scales, rotation, and primary selection; do not scatter
its coordinates across independently committed keys. Current Desktop still uses
`desktop.appearance.v1`; migration has not happened yet.

Major application versions may deliberately own separate collections such as
`com.adobe.photoshop.v8`; this is an application version, not its schema version.
Shared preferences under `com.adobe.photoshop` are separately declared and
authorized, never inherited from a common name prefix. Package installation
should register declared collections, defaults, schemas and lifecycle metadata;
each executable manifest requests its own scoped access. Registration validates
publisher ownership and consults policy rather than granting authority merely
because a manifest names a namespace. A universal application-identity namespace
is still a design question; do not impose one through this Config experiment.

Schema-defined inheritance only: show the effective value and its source.
Machine security policy is not overridable by a session preference. Profile
activation validates a whole revision, checks the target's existing authority,
and asks each consumer to apply live or restart as declared. It does not mint
privileges or promise atomic hardware changes just because storage commits.

## Initial native inspection slice

Implemented:

* `CuBit.Config_Inspection` defines typed operations/statuses and component-boundary
  namespace matching. Existing manifest scopes ending in a dot are accepted;
  `test.config` never authorizes `test.configuration`. Empty scope means an
  explicitly installed wildcard grant, not default access.
* New read-only IPC uses `(slot, generation, key length, context)` and validates
  message length, bounds, owner, generation, writable mapping, and mapped byte
  length before touching a grant. The service returns its acquisition before
  replying. The native reader owns each result and uses a separate grant per
  synchronous call, not the old singleton client's borrowed pointer.
* Machine context ID zero is the **only** supported context. Other IDs fail;
  no profile/context creation, selection, inheritance, or durable saving is
  implemented by these messages.
* Read responses distinguish missing, denied, oversized, invalid, and unavailable.
  Results are bounded to 1024 bytes; key enumeration returns newline-separated
  names and fails on overflow rather than reporting a truncated list as complete.
  Pagination and structured records are follow-ons. Stored values remain bytes,
  not schema-checked typed values; use this text adapter for textual settings.
* Workbench explicitly requests global non-secret Config read in its manifest.
  Its broad inspection power applies to scripts it runs; do not treat embedded
  scripts as isolated from the host's installed bindings. There is no write or
  Config-admin binding. The endpoint's `read-write` transport rights are not
  permission to update Config entries.
* `config.get` and `config.keys` are typed CCL source-host imports. Discovery is
  enabled only after the native global-read probe succeeds. They use the same
  adapter in Workbench and the remote CCL host. The plaintext development remote
  host is **not** granted Config access by default. Installing a binding never
  caches an exemption from subsequent server authorization checks.
* The Linux preview has no Config connection and exposes no fabricated values.
* **Apps → Config Inspector** opens a native read-only inspector. Its shared
  toolkit tree groups real keys by namespace beneath Machine, with folder and
  setting icons from the bundled Bluecurve artwork. Selection reads the value
  over the same checked IPC interface; Refresh preserves selection and expansion.
  Painting and pointer motion do not query Config. This is not a profile editor:
  values are still stored bytes, and only the machine context exists today.

Try in the **native CuBit Workbench**, using Interpret/REPL:

```lisp
(config.keys "")
(config.keys "desktop")
(config.get "desktop.appearance.v1")
(ui.output-append (config.keys ""))
```

The appearance key can be absent until Settings first writes it. The sample
`config-inspector.ccl` is picked up by the existing Live CD sample collection.
Richer typed error outcomes are not implemented here; evaluation reports
host-call failure on an unsuccessful query.

The ELF interface description is a schema fingerprint, not a signature or
proof of service identity. Dynamic authenticated schema discovery remains
separate work; hosts currently publish the known description after probing.

## Next implementation steps

1. Config's owned volatile store is extracted into `Config_Store` and used by
   the native IPC handlers. Next add context-bound handles and distinct
   creation/update rights. Keep the default machine context explicit while
   migrating existing consumers.
2. Declare/validate schemas and namespace ownership, then add revisioned profile
   objects and expected-revision updates. Never allow inspection grants to mint
   grants, create contexts, or activate policy.
3. Keep CCL authoritative for desired configuration. Classify managed collections
   separately from mutable application state; neither may silently overwrite the
   other. Typed Turso commits already serve the native app-state experiment.
4. Add revision-pinned proposal/review/activation with source correspondence and
   consumer status. Expose desired, active, overridden and runtime facts without
   pretending a database commit atomically reconfigures services.
5. Make appearance survive reboot through this managed-declaration flow on an
   explicitly selected writable store, then named monitor profiles once durable
   monitor identities are available.

The data and ACL handlers now acquire owner- and generation-checked grants;
bootstrap, procmgr, and the Ada client use the new wire format together. No
raw-slot compatibility path remains. The pure data-request decoder validates
wire lengths before narrowing, and ACL installation uses a private snapshot
before publishing an owned candidate. Get/list output is assembled before
publication; an overflowing list fails instead of returning a partial success.

The unsafe key=value load/save implementation, implicit `config.store` disk
overlay, and unused C Config client have been removed. `system.ccl` seeds the
store, with the current boot's RTC sample added separately. Changes are volatile
until reboot; a successful update does **not** imply durable saving. Config no
longer receives a bootstrap filesystem endpoint or wildcard filesystem scope.
The older Ada borrowed-buffer client remains serialized/non-reentrant; the
inspector uses owned results. None of these changes proves the full IPC service,
kernel grant machinery, or durable storage. No new broad remote grant is enabled.

## Config database evaluation (native backend still planned)

A [Linux-hosted Turso experiment](../tests/config-turso/README.md) now runs real
transactions, revision conflicts, process-exit recovery and SQLite snapshot
compatibility tests. A separate [native CuBit probe](../tests/config-turso/native/README.md)
now runs the same typed Config/CBOR adapter and real Turso against volatile
MemoryIO. It is not linked into `config.svc`. The native runtime/storage gaps
and initial measurements are recorded there; production adoption remains gated.
The hosted format now stores a complete, bounded scalar profile as deterministic
CBOR with a codec-schema identity in each transactional revision. It deliberately
does not implement a separate general-purpose Config type system; broader object
support follows the shared CCL persistable-value roadmap. A codec digest does
not establish publisher trust, grant authority, or validate an application's
full schema. Native Config remains volatile.

Evaluate [Turso Database](https://github.com/tursodatabase/turso), the Rust
SQLite rewrite, as an embedded transactional backend for Config. This is not
an adoption decision for the production service or a requirement for Turso's hosted
service. Its [MIT license](https://github.com/tursodatabase/turso/blob/main/LICENSE.md)
is suitable for evaluation; audit the licenses and implementation languages of
the actual pinned dependency/feature closure separately.

Keep the backend behind Config's typed, authority-checked IPC. Applications and
CCL scripts do not receive arbitrary SQL access or access to its backing file.
CCL remains the declarative system configuration and authoring/export language;
the database stores approved revisions and their provenance, not a competing
implicit startup overlay. Secrets remain separate, and storing a policy proposal
does not authorize its activation.

Evaluation milestones:

1. Build a minimal Linux-hosted prototype in Nix. Measure reads, atomic
   expected-revision updates, history/rollback, startup, and memory use under
   realistic configuration sizes and quotas. Keep it separate from native CuBit
   integration and require explicit schema/version migration behavior.
2. Audit native portability: Rust runtime, allocator, threading, time/entropy,
   and filesystem requirements. Evaluate static linking and an Ada-facing C ABI
   without introducing a C implementation or unnecessary C dependencies. Adapt
   storage to scoped CuBit filesystem authority and native asynchronous I/O.
3. Establish real flush, ordering, locking, and recovery requirements before
   claiming durability. Test interrupted commits, torn writes, restart recovery,
   exhausted storage, denied operations, and revision conflicts. Never simulate
   successful durable writes when the storage stack cannot provide them.
4. Expose seeded, staged, saved, and active revisions clearly. A database commit
   does not make updates across services or hardware atomic. Keep SPARK claims
   about Config's policy/protocol model distinct from backend testing and any
   unproved storage assumptions.

### Portable profiles: CuBit's equivalent of dotfiles

A supported, self-contained `.sqlite` snapshot could make a profile easy to
back up, inspect, and move between machines. Validate the chosen engine's file
compatibility and snapshot mechanism before promising that format. Provide an
authorized export operation producing a consistent snapshot; do not tell users
to copy a live backing file that may depend on an outstanding journal or WAL.
CCL export should remain an alternative for reviewable, diffable configuration.

Export only the selected, authorized collections/context, not the entire system
database by default. The portable bundle contains settings and schema versions,
not live handles, capability grants, secrets, or implicitly trusted identity.
Import is untrusted input: validate bounds and schemas, show a change preview,
and apply only within destination authority. Imported provenance is a claim
unless independently authenticated; importing settings never imports privileges.

Separate portable preferences from machine-specific bindings such as monitor
identities, device assignments, filesystem references, and secret references.
Offer explicit rebinding for unresolved references instead of silently applying
one machine's topology to another. Preserve the saved intent without forcing it
active. This supports personal, development, test, and production profiles
without introducing OS user accounts or copying an installation's security policy
as a side effect of restoring someone's preferences.

## Validation

### Owned authorization model

`userspace/services/config/config_authority` is now the service's private,
owned authorization table, independent of IPC and the key/value store. Rights
are typed read/write operations. A missing profile or empty rule set denies
access; wildcard scope is explicit. Installation replaces rather than merges
a profile, and capacity/invalid-subject failures preserve the previous state.
The IPC decoder constructs a candidate before publishing it and rejects
overlong scopes/unknown rights rather than truncating them. The legacy trusted
launch message's zero count still explicitly requests a wildcard; that wire
convention is not the model's empty-rule-set behavior.

SPARK proves runtime checks in this package, failed-update preservation,
successful installation creating a profile, revocation removing the profile,
and install/revoke preserving other subjects' profiles. The last property uses
a Ghost predicate, not additional production bookkeeping. These are **not**
proofs of sender authentication, the IPC decoder, kernel grants, service-role
exceptions, profile uniqueness over arbitrary corrupted memory, or durability.

Before launching a suspended child, procmgr now resets its Config profile as
well as its filesystem profile. Failure to reset Config while the service is
registered fails the launch. This prevents the normal launch path from keeping
a recycled PID's prior Config scopes, including when the new ELF has no scopes.
The launcher and PID-reuse lifecycle are not SPARK-proved by this package.

`nix develop -c make -C kernel config procmgr devmgr config-check` builds the
native fixture and both trusted producers of the changed wire format.
`nix develop -c tests/headless/run.sh --test config-inspection --accel kvm --cpus 4 --timeout 35`
boots actual CuBit and checks authorized inspection, namespace-boundary denial,
read-versus-write separation, unsupported contexts, stale/invalid grants, and a
CCL expression that reads the real service. This is regression evidence, not a
proof of Config or the kernel. Hosted namespace checks and a focused SPARK run
are separate from native IPC validation. The pure wire-length decoder also
proves all five runtime checks and two initialization checks. Native tests cover
full-size values, list overflow, read-only/stale grants and retirement, and the
absence of the retired disk overlay; they do not establish crash durability.

### Owned volatile store

`userspace/services/config/config_store` now owns entries behind private state.
Bounded key/value records distinguish an unused slot from a present empty value.
Put validates before mutation; replacement works even at capacity; deletion
releases a slot; returned values are owned, not pointers into the table. Both
inspection and data IPC use this implementation rather than separate raw arrays.

SPARK proves 35 runtime checks and two functional contracts: successful Put
contains the requested key/value, rejected Put preserves state, and a missing
Remove preserves state. Eight initialization/termination checks also pass.
The successful-write predicate is Ghost and absent from the native object.
No assumptions or SPARK-Off sections were introduced. This does not prove
authorization, durability, key uniqueness as a global invariant, or the entire
IPC path. Host tests cover maximum payloads/capacity, replacement, slot reuse,
empty values, non-1 bounds and keys at `Positive'Last`; native regression remains
separate evidence.
