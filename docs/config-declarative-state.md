# Declarative configuration and mutable state

Status: agreed architecture, incremental implementation. The review helper and
typed collection classification/enforcement, durable classification recovery and
shared reviewed-activation controller below exist. Durable/native activation,
scalar managed-key enforcement and GUI source editing do not. This supersedes
proposals to persist every scalar Config write independently
of the CCL declarations that seeded it.

## One source of desired configuration

**CCL declares intent. Config serves the active revision. Turso stores revisions
and separately owned application state.** A database is a storage mechanism, not
a second authority for what the system should become.

| Class | Source of truth | Mutation and recovery |
| --- | --- | --- |
| Desired configuration | Reviewed CCL declarations and their pinned inputs | Edit/propose, validate, authorize and activate a revision |
| Active configuration | Validated realization of an identified desired revision | Record activation status; restore the selected committed revision after restart |
| Application state | Application-owned, schema-checked records | Ordinary authorized typed updates, with expected-revision conflict checks |

The Workbench persistent counter is **application state**. Desktop theme and
monitor arrangement are candidate **desired configuration**. Last-open document
and window history are normally application state. Schema/collection registration
must specify which class a resource belongs to; callers cannot change that class
to bypass managed-setting enforcement. All classes use the common CCL type model,
Config protocol and existing authority checks, not different permission systems.

The distinction is independent of namespace, profile and context. A development
profile can have declarative settings and mutable application state without
implying broader authority than a production profile.

## Generations (direction, user 2026-10-05)

Config follows the Storehouse-and-views model of backlog FS-020 (Guix/Nix):
- **One generation, one switch.** A generation records both the program
  views (which Storehouse entries `Applications/`, `Services/` and `Drivers/`
  show) and the config revision. Activation switches both at once, and
  rollback restores both, so a program never runs against another
  generation's config.
- **Revisions are content-addressed.** A realized revision is identified by
  its hash, like a Storehouse entry: identical config is shared between generations, a generation
  pins its revision by hash, and garbage collection drops revisions that no
  kept generation references.
- **Removal is declarative.** Config an application declares leaves the next
  generation when the application leaves the declaration. Application-owned
  state goes when the last generation referencing the application is
  collected, so rollback still finds it.
- **Mutable application state** is not content-addressed. A schema change in
  a new application version migrates it into a new revision at activation,
  and the old revision is kept until its generation is collected.
  Copy-on-write snapshots may follow once the filesystem journal exists.

## Installed systems and boot

An installed system should keep its authoritative CCL on disk, alongside the
exact inputs needed to reproduce the selected configuration. The current boot
archive's `system.ccl` remains the implementation today. See
[bootstrap storage and Config](boot-storage-and-config.md) for the minimum
trusted information required before the filesystem and Config database exist.

Boot should activate a selected, validated revision, not execute whatever bytes
an application most recently wrote to a configuration path. Writing source,
selecting the next boot revision and activating policy are separately authorized
actions. A source hash establishes correspondence, not trust in its author.
Future signed/admitted revisions must bind source, dependencies, schemas, target
context and evaluated plan; a signature alone never grants authority.

Turso may retain compiled typed values, source correspondence and activation
history. That avoids reparsing on every get and permits recovery without turning
ordinary DB writes into an alternate configuration-management channel. Database
recovery must not accidentally choose an unapproved candidate over the selected
revision. Application state is backed up/restored independently; reverting system
configuration does not rewind a counter or undo a database migration.

## Editing and activation

The intended flow is:

1. Start from an identified base revision and authorized context.
2. Edit CCL or propose changes to a data-only managed CCL fragment. Do not attempt
   to reverse arbitrary computed expressions into source. Fragment ownership and
   inclusion must be explicit; there is no hidden last-writer-wins overlay.
3. Evaluate with pinned inputs, validate schemas and compute an owned candidate.
4. Review effective additions, replacements and removals, with authorized access
   to provenance and values. A review must not leak inaccessible keys or secrets.
5. Check activation authority and the expected base revision at commit time.
   A changed source, binding, policy or active base invalidates a stale approval;
   re-review/re-authorize instead of silently applying it to a different target.
6. Persist the selected candidate and track each affected consumer's application
   status. Expose pending, applied, failed and restart-required states distinctly.

An ACID commit does **not** make a collection of IPC calls, device changes and
process restarts atomic. Recovery needs an explicit activation record and
idempotent consumer operations or reconciliation. Never report global success
because only the database commit succeeded. A timed-out update with uncertain
effects must be inspected/reconciled, not blindly repeated.

A generic `Config.set` must reject writes to declaration-managed values; use the
proposal/activation flow instead. Temporary overrides, if supported, need their
own explicit authority, provenance, scope and lifetime, and must remain visibly
different from desired intent. They must not silently survive reboot or become
security-policy overrides. Default suggestions and mandatory managed values are
different schema semantics, not guessed from the key's spelling.

## Useful first slice: read-only configuration review

`CCL.Configurations.Changes.Compare` compares two successful evaluated
`system-config v1` results. It reports candidate additions/replacements and
baseline removals. It rejects failed compilations and startup profiles. Ordering
does not change setting identity; startup ordering must not use this comparator.
The result indexes the two owned plans, which callers must retain unchanged.

The **Linux-hosted** `ccl-config-review` boundary reads two files and emits only
actions and keys. It never writes files, invokes CuBit IPC, accesses Turso or
authorizes/activates changes. Both valid unchanged and changed reviews exit zero;
invalid input exits nonzero without a partial review. Build and use:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../userspace/ccl/ccl_config_review.gpr'
nix develop -c userspace/ccl/build/config-review/ccl-config-review before.ccl after.ccl
nix develop -c python3 tests/ccl-configurations/test-review.py
```

The current v1 frontend normalizes integers and booleans to strings. Consequently
`(+ 20 22)` and `"42"` compare equal here; this is **not** a nominally typed object
comparison or a source formatter. Typed schemas must be included when this is
extended to the typed collection path. The review lists candidate changes only:
removing a declaration does not yet define whether a consumer reverts to a schema
default, stops, or rejects activation. That needs explicit schema semantics.

There is no native activation endpoint behind this tool. A future native/remote
review must obtain both plans through authorized interfaces and pin their
revisions. Reading a local file for a hosted development comparison is not such
authorization. Key-only output reduces accidental value disclosure, but key
names can themselves be sensitive.

Validation (2026-09-26): Nix-hosted tests pass seven groups, including 150
randomized comparisons against an independent dictionary model, maximum entry
counts/key/value bounds, rejection without partial output, and unchanged input
files. Focused GNATprove discharges 11 obligations: eight runtime checks, one
initialization and two termination checks, with none unproved or justified.
There are no functional-contract obligations in this result; semantic agreement
is regression-tested, not a proved equivalence theorem. No Assume or SPARK-Off
escape is introduced. The Linux file/console boundary is outside that proof.

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../userspace/ccl/ccl_config_review.gpr -u ccl-configurations-changes.adb --mode=prove --level=2 -j2'
```

## Implementation sequence

### Typed collection enforcement implemented (2026-09-26)

The existing shared `Config_Collections` catalog now records an immutable
`Management_Kind`: `Application_State` or `Declaration_Managed`. Trusted
registration selects it; no client IPC descriptor accepts a classification.
Existing registration paths default to application state. Neither re-registration
nor public Create can change an existing collection's class, even with the same
schema and an explicitly granted wildcard Config scope.

Managed collections allow normal authorized read-only handles, but deny ordinary
write-only/read-write opens. Resolve checks classification again, alongside
subject, handle rights, current scoped authority and grant-installation revision.
Thus the typed store's access preflight and Set share the same restriction;
there is no cached authorization bypass or special administration exception.
Requests are denied rather than silently reducing requested rights. Public
re-Create returns Denied before queuing storage or saving a deferred reply.
A management conflict during completion does not retire a healthy storage worker.

These changes are in the native service's common code, with hosted execution of
the real catalog/store/IPC dispatch and receiver. The initial catalog-only slice
compiled native `config.svc`; later persistence/QEMU evidence is recorded below.
Tests include all four requested
read/write combinations, both attempted class conversions, wildcard authority,
revocation/regrant, trusted recovery/read and rejected Set with no worker request.
The focused authority/catalog/store proof has 162 discharged obligations, including
10 functional contracts. In particular, successful Resolve implies the ghost
authorization predicate, now including the management restriction. This does not
prove durable classification or whole-system authorization; the IPC boundary is
regression-tested, not included in that proof.

**Not yet enabled for actual appearance settings:** no native installation path
registers managed collections yet. A trusted installer must reserve the durable
registration before exposing the name; it must not silently adopt a name already
claimed as application state. Do not infer classification from a namespace prefix
or trust client-supplied schema metadata. Trusted Restore can load a stored managed
value; it is not a client activation API and does not yet bind source/provenance.
Legacy scalar settings remain volatile and are not covered by this restriction.

### Durable classification implemented (2026-09-26)

Private database format **4** stores immutable management classification beside
each object definition. Creation compares both schema and class; even equivalent
schemas cannot change management. The read path validates the stored class, and
malformed/missing class values do not become writable application state. Earlier
database formats are rejected explicitly, not upgraded by guessing classification.
Existing test/playground databases need a fresh store or a future explicit
export/migration; no user database was deleted or rewritten by this change.

The authenticated worker metadata reply distinguishes application-state and
managed recovery. Config registers the recovered class before issuing a handle.
A cold-cache Create of an already-persisted managed collection is rejected by
Turso; it cannot preempt recovery or downgrade the registration. Cold read-only
Open recovers the class and retains ordinary scope checks. Management conflicts
are definite denials, not grounds for retiring the worker.

Ordinary database commits check classification inside their transaction too,
including the lower-level object/scalar adapters. A managed write rolls back
without changing history; the worker reports Rejected and remains usable after
confirmed rollback. A rollback/storage error remains an uncertain failure and
retires the session. This adds one indexed metadata lookup per durable commit,
not per cached Get, filesystem operation or transferred page; its latency cost
has not yet been benchmarked. No performance improvement is claimed.

The backend's trusted `register_object` operation is used by installation test
fixtures, not exposed as an application IPC or unrestricted activation bypass.
No API yet writes managed values. Database classification is not a signature or
new authority source: it relies on the existing protected Config storage boundary.
This does not resist an attacker who can rewrite that database offline; trusted
installation identity, authenticated storage/boot and rollback policy remain work.

Evidence: 51 Rust tests pass, including real-file close/reopen with an independent
SQLite check and malformed-class rejection. Hosted native-service code, modeled
IPC and real Turso pass 4651 checks, including two cold Config/worker lifetimes
that recover managed metadata and deny Create/Open-write/Set; SQLite confirms
unchanged class and zero unauthorized revisions. The metadata channel adds306
checks across three database lifetimes. Focused schema protocol/executor SPARK
discharges50 obligations (four functional contracts), none unproved/justified;
the Rust/SQL durability behavior is tested, not SPARK-proved. Both native services
compile and link. Separate QEMU/KVM regression passes native Workbench editor →
compiler → Config IPC → Turso: two writes, independent reboot/read42 assertion,
and exact SQLite/WAL/ext2 checks after both boots. This exercises ordinary
application-state persistence with format4; managed cold-recovery enforcement is
covered by the hosted native-service/real-Turso tests, not that QEMU script.
Artifacts: `tests/config-workbench/build/artifacts/managed-v4.vyNZhz/run` and
`tests/ccl-objects/build/artifacts/config-publication.BUSdFt`. Normal development
ISO was rebuilt; no user's base disk or existing database was modified.

### Source-bound activation controller (2026-09-26)

`userspace/services/config/config_activation.ads/.adb` is shared SPARK service
code, exercised on **Linux**, not an exposed native activation endpoint. It owns
one `system-config v1` setting fragment in the machine context. The existing pure
CCL evaluator produces the value; exact source bytes, target key, expected base,
proposal ID, reviewer and grant-installation revision remain bound together.
There are no ambient imports or source-file rereads. This frontend still yields
normalized string values, **not** a new nominally typed persistence encoding.

`Activate_Config` is a distinct operation in the existing scoped rule engine,
not a second ACL system. Review requires both read and activation authority for
the target. `Read_Write`, including the bootstrap wildcard, does not grant it.
Existing grant-wire masks remain 0..3 and cannot express activation. Only trusted
in-process installation can exercise the pilot today; a separately authorized
native policy/issuer path must be defined before exposing it. Ordinary collection
handles reject activation rights altogether, for either management class.

The state transitions are explicit:

```text
candidate -> reviewed -> committing -> selected -> applied / apply-failed
                              |           |
                              |           +-- waiting for the bound consumer
                              +-- definitely rejected / commit-uncertain
```

A new proposal consumes the old review, even if the new source is invalid or
evaluates to the same value. Begin checks proposal ID, reviewer, current grant
revision, live scoped rights and base again. Wrong subjects cannot consume
another review. A replaced or revoked/regranted authority cannot resurrect an
old review. Admission returns a private, owned commit request, not a writable
pointer into UI state. Readback is separately authorized and clears output on
denial, so a revoked reviewer cannot continue inspecting the retained source.

**Authorization linearizes at Begin_Commit**, under the service's serialized
dispatcher. As with existing accepted Config writes, later revocation does not
retroactively cancel an already submitted durable transaction. Storage must still
compare the expected base inside its transaction. Policy that requires cancelling
accepted work would need an explicit cancellation/commit protocol; this controller
does not claim to provide that. Source editing alone never grants activation.

Storage success records `Selected`, never `Applied`. Application acknowledgement
must match the proposal, selected revision and assigned consumer instance. The
shell must authenticate worker/consumer replies and bind their service sessions;
correlation IDs are not capabilities. Wrong/delayed replies are ignored. An
uncertain commit, pending application or failed application blocks proposal
replacement. There is no blind retry or clear-error shortcut. Restart/reconciliation
must resolve the durable outcome before issuing another controller/session.

The controller is **not yet joined to Turso, typed schema binding, Settings or
native IPC**. No database write bypass was added; managed values remain unwritable
through ordinary Set. The next adapter must require an already-reserved managed
collection and its approved schema, encode/validate the candidate as that type,
atomically store exact source correspondence with its selected revision, perform
the transactional base comparison, and recover pending consumer application.
Source/schema/consumer binding changes must invalidate a reviewed proposal.

Each controller is trusted service-owned session state, not a globally writable
proposal slot. The shell must authorize session access and serialize it with grant
changes; the package is not independently thread-safe. Proposal IDs are nonreusing
only within that controller lifetime. Consumer IDs must denote service instances,
not bare reusable PIDs; the existing kernel identity gap is not solved here.
Proofs do not establish sender authenticity, compiler semantic equivalence, SQL
atomicity, hardware effects or whole-system security.

Validation: 126 hosted activation checks pass in Nix. GNATprove discharges all43
controller obligations (15 runtime, five functional,10 initialization and13
termination), none unproved or justified. The five contracts cover proposal/source
correspondence, empty denied inspection, live-authority/source-bound admission,
storage completion never claiming application, and exact application
acknowledgement/state preservation. Existing authority/catalog/store proof162 and
grant-wire proof18 still pass. Catalog168/publication97/managed68, exhaustive
grant-wire135222 and the full Config IPC/CCL client regressions pass. Native
`config.svc` compiles/links; the activation unit compiles with the CuBit runtime.
Native object inspection finds no code symbols for the four Ghost predicates;
no runtime assertion switch, Assume or SPARK-Off was added. No new ISO or native
activation/QEMU result is claimed in this slice. Database format remains4.

### Remaining integration sequence

1. Keep scalar writes explicitly volatile and the counter explicitly app-state;
   do not add independent scalar persistence as an intermediate shortcut.
2. Define typed collection ownership/classification, desired revision identity,
   source correspondence and expected-base activation requests. Retain the
   existing scoped checks and uncertain-commit handling.
3. Implement one small managed collection end-to-end, including durable source
   proposal, authorized activation, consumer acknowledgement and restart recovery.
   Desktop appearance is a candidate; security policy is not the first pilot.
4. Expose desired-versus-active-versus-overridden state in Inspector/Workbench,
   then reuse that interface in Settings and the remote shell.
5. Test stale approvals, unauthorized class changes, interrupted activation,
   source/database disagreement and rollback with preserved application state.

SPARK can prove bounded comparison and transition properties. It does not by
itself prove durable storage, authority authenticity or distributed activation;
those require explicit models and fault-injection/native regression evidence.
