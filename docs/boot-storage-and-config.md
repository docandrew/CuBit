# Bootstrap storage and Config

Status: agreed design direction, not an implemented installed-system boot path.
The internal filesystem volume list exists today; declarations are still fixed
in filesystem.svc bootstrap code. See [storage I/O](storage-io.md).

## One configuration, a small boot snapshot

CCL declares the desired volume list, including boot-critical volumes. Config
can persist and serve its selected realization, with correspondence to that CCL.
See [declarative configuration and mutable state](config-declarative-state.md).
It cannot be the only place holding the information needed to reach itself.
Generate a minimal bootstrap snapshot from the selected system configuration
and load it alongside the kernel and initial services. A future installed system
can keep those artifacts on its boot partition; GRUB can supply the snapshot as
a module or it can be carried inside the initrd. No filesystem service is needed
to read the already-loaded snapshot. Do not add a second configuration language
or encode the volume list as a collection of GRUB command-line options.

The full system-image CCL is the build/install recipe, not necessarily the source
evaluated at boot. Its small bootstrap output is a versioned CCL declaration plan
using the common pure, bounded frontend. It is not an unrestricted startup script.

Three distinct representations serve different purposes:

* **Desired CCL declarations, realized through Config:** stable volume names,
  provider/media selectors, filesystem expectations, options and the Config
  backing location. The database is not a competing editable source of intent.
* **Boot snapshot:** the minimum dependency closure needed to reach that location,
  plus a configuration-generation identity for reconciliation.
* **Live list in filesystem.svc:** admitted bindings, availability and current
  service-lifetime identities. Endpoint slots, grants and live handles are never
  persisted as reusable authority.

The service may publish a read-only observation of its live list through authorized
IPC for inspection. This must remain distinguishable from desired configuration;
an unavailable volume is not silently erased from the desired list.

## Startup phases

1. **Bootstrap:** validate the snapshot, start required providers, wait for explicit
   readiness, and open the minimum storage needed for Config.
2. **Configure:** load the selected Config state, validate requirements and authority
   decisions, and reconcile declarations with the already-open bootstrap bindings.
3. **Run:** start remaining services in dependency order, again using readiness
   messages rather than sleeps or process existence as a proxy for readiness.

Retain one declaration format and dependency model across phases. Report the
service, dependency and reason when startup cannot progress. Required failures
stop the affected startup path; optional absence must be declared, not inferred
from arbitrary I/O errors. Readiness waits need explicit bounds and failure policy.

## Reconciliation and updates

CCL is the source of desired state; Config stores the selected realization and
its revision identity. A boot snapshot identifies the selected generation used
to reach it. A mismatch does not authorize silently changing the
active backing volume or reinterpreting open handles. Report it and require an
explicit transition. Opening Config from a different location is a migration,
not an ordinary setting update.

An update affecting boot-critical declarations must prepare a complete, consistent
boot generation before activation. Power loss must leave a usable old generation
or a usable new one, not mixed artifacts. The boot artifact publication/recovery
mechanism is still to be designed and tested; current Ext2 work does not supply
this guarantee. Noncritical changes can apply live only where provider lifecycle
and handle retirement support them.

## Authority and trust

Declarations request bindings; they do not mint capabilities. Existing authorized
endpoints and policy decisions remain the enforcement mechanism. A volume name or
filesystem UUID is identifying evidence, not proof of ownership or authenticity.
Discovery, configuration edits and file access are separate authorized operations.

Authenticate bootstrap configuration together with the kernel and initial service
artifacts. Otherwise an attacker could redirect startup toward an unintended Config
database. Signature enforcement, authenticated media identity, rollback policy and
recovery authorization remain separate implementation work.

## Current code is not this whole design

Today devmgr reads `system.ccl` from CPIO and seeds Config using native IPC. That
seed is authoritative for the boot; no implicit durable overlay is loaded by that
path. Filesystem.svc registers RAM/ATA/NVMe bindings in code. CPIO and ISO readers
remain separate from the block-volume list. The Turso experiments do not yet make
the installed-system boot/update scheme above operational.

Checked admission outcomes now distinguish explicit absence from failures. Next:
a bounded typed volume declaration plan; authorized bootstrap binding installation;
explicit Config reconciliation; and
failure-injected boot-generation publication tests. Do not expose raw endpoint
slot numbers as durable user configuration.

## Next implementation boundary: attaching persistent Config

The isolated Turso probe now writes a standard SQLite database through native
filesystem IPC. This does not change `config.svc`: its current `Config_Store`
is explicitly volatile, and devmgr supplies the CCL boot seeds over IPC.

Keep attachment separate from ordinary setting updates:

* Start with validated boot seeds and report their source as bootstrap/volatile.
  Config must remain available while the filesystem starts; do not block its
  entire IPC loop waiting for the storage service that needs those seeds.
* The trusted boot plan selects the backing location and expected generation.
  The launch/authority path grants the required filesystem access. Neither a
  path read from the database nor an arbitrary Config client may redirect it.
* Restore into a bounded candidate snapshot. Validate format, schema, history,
  collection ownership/context and generation before publishing cached values.
  Reacquire live authority through existing policy; never deserialize handles,
  endpoint slots, signatures-as-permissions, or saved approval as live grants.
* Overlay only explicitly eligible collections. Boot-critical settings and
  security policy must not become runtime-overridable merely because a database
  contains a key with the same spelling. Mismatched generations require an
  explicit reconciliation decision, not a silent merge.
* A missing optional store, denied access, incompatible format, corrupt data,
  conflicting exclusive owner and uncertain I/O are distinct outcomes. An
  existing but unreadable database must not be silently replaced or reseeded.
* A successful durable update advances the cached collection revision only after
  storage acknowledges the transaction. Reads can keep serving the previous
  complete snapshot while a commit is pending. A failed/ambiguous acknowledgement
  requires recovery, not an automatic retry or a claim that nothing changed.
  Live application of settings remains separate from durable storage success.

Before wiring this into Config, choose the nonblocking backing execution boundary
(a dedicated worker versus an isolated storage service), define bounded pending
updates and typed attachment outcomes, and test recovery from service death and
metadata/device failures. The current synchronous File adapter establishes the
storage contract; it is not permission to introduce blocking disk I/O into every
Config read or a second policy authority in the database layer.

The value boundary also needs an explicit mapping: today's native store holds
bounded opaque bytes, while the Turso experiment stores typed text/integer/boolean
profiles. Do not silently interpret arbitrary existing bytes as UTF-8, flatten
typed values into text, or mistake a SQL namespace/profile string for an
authorized Config context. Define and validate that mapping at the owned-value
Ada/Rust interface before connecting existing clients to persistent collections.

### Implemented publication building block

`Config_Objects` provides schema-bound double-buffered publication. The
`Config_Typed_Store` dispatcher core joins it to approved collection definitions,
subject-bound handles, current Config scope grants and worker request routing.
Cached reads continue while a single storage request is pending. Staging does
not change any published value/revision; only a matching validated receipt can
publish. Ambiguous outcomes require restoration, never a blind write retry.

The [focused tests and proof](../tests/config-collections/README.md) cover this
owned-data/authorization boundary. The real hosted Turso fixture now exercises
the same core through modeled channel/receiver IPC, including response loss
after a commit and independent SQLite validation. Live Config remains volatile:
worker launch, schema provisioning and native IPC dispatch are still pending.
The unused text-only publication prototype and its test suite were removed;
`Config_Store`, which still serves the live byte-value API, remains intact.

### Implemented asynchronous filesystem boundary

`userspace/lib/storage/Storage_Channel` now separates capability-authorized
submission, kernel completion handling and owned-result consumption. The native
Turso probe uses it for real filesystem operations. No new syscall, policy
authority, service role or PID-based trust decision is introduced. The kernel
validates the endpoint at submission and consumes request-specific reply
authority before producing a completion. A request token only correlates work;
the dispatcher must never feed client-supplied messages into the completion path.

One stable, limited channel owns one aligned transfer page. Only one request is
outstanding; an unconsumed result also prevents page reuse. Submission returns
without waiting for the filesystem. Errors with uncertain effects stop data I/O,
while explicit close cleanup remains possible. Retirement is terminal and its
generation query, not merely accepted revocation, determines safe reclamation.
Tokens must not be reused across channel lifetimes within a process.

The Turso probe currently wraps this channel with a blocking wait, suitable for
an isolated worker. Config must not call that wrapper from its service loop.
The intended next integration is a dedicated, narrowly authorized storage worker:
Config submits approved collection operations through an existing held endpoint,
continues cached reads, then validates owned results before publication. The
worker's filesystem scopes come from the same launch policy as other services;
database rows cannot widen them. No arbitrary-path or SQL execution endpoint is
intended for Config clients.

Still required before enabling that boot path: worker endpoint installation via
the trusted launcher, explicit typed/opaque-value schema mapping, bounded whole
snapshot transfer, process-wide request correlation, client-visible attachment
status and write-conflict behavior while loading. This filesystem channel alone
is neither Config-to-worker authentication nor end-to-end asynchronous Turso.
Its fault tests and native two-boot persistence tests are regression evidence,
not an extension of the pure publication component's SPARK proof.
