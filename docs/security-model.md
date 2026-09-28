# CuBit Security Model

Status: implemented architecture plus design proposal

CuBit is a capability-oriented microkernel operating system designed for
security by default. Downloading and executing untrusted code should be safe
because a process receives no ambient authority and can perform only operations
explicitly made available to it.

> **Foundational invariant:** Every protected effect requires the exercise of
> valid authority, and every active authority must be traceable to an
> authenticated and authorized policy or delegation root.

Authority is enforced through protected references (capabilities), not an
ambient permission associated with an identity. In the short explanation:
**policy governs grants; handles exercise authority**. The
[security vocabulary](security-vocabulary.md) defines these four core terms
and maps endpoints, slots, tags, replies and memory grants to the code. It does
not introduce another security mechanism or rename the current ABI.

## Six rules to teach and enforce

1. **No authority, no action.** Every protected effect is mediated; no identity,
   local caller or administrative label bypasses enforcement.
2. **Names are not permission.** Names, signatures and profiles are evidence or
   context, not authority by themselves.
3. **Giving authority requires authority.** Grants stay within explicit issuance
   or delegation power, recipient request ceilings and applicable approvals.
4. **Content cannot rewrite the rules.** Writing data or executable bytes does
   not itself authorize policy changes or privileged activation.
5. **Authority has an explicit lifetime.** Restart, revocation and reuse cannot
   silently resurrect it; accepted work has defined completion semantics.
6. **Effective authority is explainable and inspectable.** Authorized tools see
   actual enforcement state, its provenance and known gaps, not just manifests.

These are target requirements. Current bootstrap exceptions and incomplete
inspection/delegation paths are not evidence that every rule is implemented.
The [model verification plan](security-model-verification.md) describes attack
cases, formal obligations, implementation correspondence and trust assumptions;
it is not an existing Lean proof or a claim of end-to-end SPARK verification.

> **Identity is evidence, not authority:** CuBit never authorizes an operation
> merely because of who a caller is. Identity can inform an authorized decision
> to issue a capability; the operation itself requires that capability.

Isolation alone is insufficient. A secure system must also make its authority,
exceptions, decisions, and unresolved obligations understandable. CuBit
therefore treats **explainable authority** as part of enforcement rather than a
debugging feature added afterward.

> If CuBit cannot explain why an operation was permitted, it must not permit it.

The implementation-hardening backlog is maintained separately in
[`security-hardening.md`](security-hardening.md). Typed IPC and ownership
transitions are described in [`typed-ipc.md`](typed-ipc.md). Kernel request
identity and one-use reply retirement are described in
[`ipc-request-lifetimes.md`](ipc-request-lifetimes.md). CCL's language
rules are described in [`control-language.md`](control-language.md). Package
realization, deployment, and authority requests are described in
[`ccl-packages.md`](ccl-packages.md). The first production-shaped server
application and its SPARKTLS trust boundaries are described in
[`web-hosting.md`](web-hosting.md). Mission-scoped authority, typed tools,
approval, and information-flow boundaries for AI agents are described in
[`agent-security.md`](agent-security.md). The migration to an IPC-only public
kernel ABI is tracked in [`syscall-abi-audit.md`](syscall-abi-audit.md).

Configuration namespaces, profiles, and contexts are described in
[`config-contexts-and-inspection.md`](config-contexts-and-inspection.md).
A namespace identifies a collection, a profile describes saved intent, and a
context identifies where it applies. None grants authority by its name or by a
human identity. Inspection may be explicitly global and read-only; it must not
imply update, creation, profile activation, or grant-administration rights.
Context selection is not an alternate path to acquiring capabilities. These
are model invariants; the current implementation slice supplies machine-context
inspection, not the full profile/context lifecycle or durable policy engine.

## Security objectives

CuBit should:

* deny ambient authority by default;
* confine compromised or malicious applications to explicitly granted effects;
* prevent authority from being forged, amplified, duplicated, or silently
  discarded;
* keep drivers and privileged services in separately isolated processes;
* make initial authority declarative and inspectable before execution;
* make dynamic delegation and resource ownership visible while executing;
* explain every security-relevant decision in terms understandable to both
  users and expert auditors;
* make exceptions conspicuous, scoped, attributable, reviewable, and
  resolvable;
* preserve useful evidence without leaking secrets or enabling audit-log denial
  of service; and
* prove critical enforcement, parsing, bounds, and state-transition properties
  in SPARK.

Security properties take priority over Unix compatibility. CuBit does not rely
on users, groups, file descriptors, path ownership, or a privileged shell as
its primary isolation model.

## Threat model

CuBit assumes that an application may be malicious, compromised, malformed, or
downloaded from an untrusted source. It may attempt to:

* invoke undeclared services or operations;
* forge handles, replies, identities, or protocol messages;
* confuse services about message schemas or ownership transfers;
* retain, duplicate, or use resources after moving or returning them;
* exhaust CPU, memory, IPC, rendering, storage, or audit resources;
* misuse authority legitimately granted for a different purpose;
* exploit a userspace driver or service; or
* obscure its behavior among normal system activity.

The kernel, boot chain, policy authority, and explicitly identified trusted
computing base are trusted according to their published proof and audit status.
Formal verification reduces but does not erase risks from specifications,
hardware, compilers, cryptography, side channels, physical access, or trusted
code outside a proved boundary.

## Authority model

CuBit's implemented unit of kernel authority is a **capability**. Present the
allowed effects as **authority**, the protected reference as a **handle**, the
authorized assignment as a **grant**, and the rules for assignment as **policy**.
These describe one architecture; they do not rename or replace kernel records.
Endpoints are destinations and slots are local table positions, not more tiers
of permission. See the [vocabulary](security-vocabulary.md) for precise mappings.

### One authority system: policy issues, capabilities authorize

This is the target model, not a claim that every current service path already
implements it. Capabilities are the only operational authority mechanism.
Policy defines and distributes authority through capability issuance,
attenuation, delegation, and revocation. It is not a competing permission
system or an alternative path around a missing capability.

Manifest requests, admitted application provenance, the authenticated launching
process, installation approval, and session context are evidence and constraints
used by an authorized policy decision. None independently authorizes an effect.
A valid signature establishes provenance, not permission. Starting a program
does not automatically give it the launcher's authority; any transfer must be
explicit and within the applicable issuance and delegation constraints.

The intended centralized policy service applies common decision rules, while
the kernel and resource services enforce the capabilities they protect. A
service-issued handle is a capability when it is a protected reference to an
object with explicit rights and safe lifetime semantics: forging or copying
its identifier cannot acquire authority. Recipient-bound handles enforce that
property through authenticated caller and ownership checks as well as current
object/generation validation. Kernel slots and service handles are different representations of
the same authority model, not independent ways to bypass it. Current
path-scoped service ACLs are transitional issuance/enforcement machinery, not
precedent for a second ambient permission hierarchy.

### No identity-based superuser

CuBit deliberately has no superuser identity or identity-based authorization
bypass. The OS does not need a Unix-style user/group account and ownership
hierarchy. Applications may model people, organizations, accounts, or remote
identities and restrict their own behavior accordingly. Those identities can
inform policy, but never implicitly supply extra OS authority.

Authenticated process-instance identities remain necessary to bind handles,
attribute actions, and prevent impersonation or stale reuse. Checking the
recipient of a capability is not granting authority merely because that
recipient has a particular identity.

No superuser does not mean no trusted or powerful components. The kernel,
resource services, bootstrap authority, and policy issuers remain explicit
trust boundaries. Administrative operations require scoped administrative
capabilities; a policy issuer cannot grant beyond its authorized issuance
power. An approval application needs explicit authority to approve the
particular request, not merely an administrator label or an authenticated
human behind its UI. Bootstrap powers and implementation exceptions must be
named and auditable rather than becoming a hidden universal bypass.

The goal is safe execution of known-untrusted code without granting trust in
the machine. That does not imply that granted authority cannot be abused or
that the current implementation is immune to exploits. Resource limits,
trusted-service hardening, and honest proof boundaries remain essential.

### Architecture implemented today

Each process has a fixed-size kernel capability table. A non-empty slot contains
a capability type, rights mask, authority tag, object reference and generation. Current
types cover endpoints, notifications, memory, I/O ports, IRQs, processes,
device memory, replies, resource quotas, and capability-space administration.
Rights are read, write, execute, grant, and revoke. Ordinary derivation can only
reduce rights. Installing a newly constructed capability is a policy-root
operation requiring an explicit `CAP_CSPACE`; ordinary process-control
authority cannot perform it.

An **authority tag** is metadata on a capability, stamped by the kernel into
`Message.authorityTag` when that capability authorizes IPC. A receiver interprets
it in the context of its endpoint and trusted grant machinery. It is neither
independent authority nor necessarily a unique capability identifier. The
separate 16-bit `MessageTag.reserved` field is not authenticated authority
metadata. Application-facing handles continue to identify service-managed
resources; authority tags do not introduce another permission tier.

Hardware ownership and publication authority are independent. Holding an IRQ
capability permits a driver to observe only its admitted interrupt; it does not
permit that driver to send unsolicited messages to arbitrary processes. Input
drivers publish only through a read-only notification grant for the declared
input role (or a current endpoint capability with write authority). The kernel
overwrites the message's source authority tag with the authority tag of the grant it actually
resolved. Receivers key source state from that authority tag, never from a
producer-supplied identifier. A bounded queue rejection is returned to the
producer, and the next accepted state-bearing report explicitly requests
resynchronization.

The intended public kernel ABI exposes only capability-directed IPC
invocation, waiting, and reply primitives. A capability may name a userspace
endpoint or a kernel object such as an address space, IRQ, I/O range, timer, or
process-control object. Kernel-object invocations execute directly without a
userspace context switch, but pass through the same type, generation, rights,
and operation checks. Existing operation-specific syscalls are migration debt,
not permanent exceptions to complete mediation.

At process creation, `procmgr` reads several ELF sections:

* `.cubit.id` supplies package identity metadata;
* `.cubit.caps` version 1 requests capabilities, their rights, parameters, and
  destination slots;
* `.cubit.streams` declares stream IDs; and
* `.cubit.access` supplies filesystem/configuration access policy and sandbox
  selection.

These sections do not all produce the same kind of object. `procmgr` translates
capability requests into kernel capability slots. Stream declarations and
path/configuration access are enforced by their userspace services. A manifest
entry is a request interpreted by `procmgr`, not authority by itself. Package
identity metadata must likewise not be described as authenticated provenance
until a package signature and provenance chain actually authenticates it.

The current mechanisms map to the design vocabulary as follows:

| Design concept | Current CuBit mechanism |
|---|---|
| Authority request | ELF manifest entry interpreted by `procmgr` |
| Effective kernel authority | Capability in a process capability-table slot |
| CCL/application handle | Proposed typed userspace reference, normally backed by an endpoint capability plus a service object/session ID |
| Reply authority | Kernel-minted `CAP_REPLY`, held by the receiving thread; the ABI names it with the reserved highest slot, and it may be moved to a process slot for a deferred reply |
| Bulk-memory access | Generation-tagged kernel grant reference; creation and acquisition may be authorized through endpoint capabilities, while legacy PID-based paths remain |
| Filesystem/configuration scope | Service-level ACL derived from `.cubit.access` |
| Stream access | `.cubit.streams` declaration and stream/service enforcement |
| Resource limits | `CAP_RESOURCE`, populated from the manifest or configured quota |

Kernel grants are deliberately distinct from capability-table entries: a grant
temporarily maps memory into a grantee, is explicitly acquired and returned,
and is later revoked. Preferred capability-directed creation obtains the
grantee from an endpoint capability; capability-directed acquisition derives
the expected owner from the endpoint rather than trusting a caller-supplied
PID. Both check current capability rights and generations. Remaining
PID-directed grant calls are compatibility paths to remove or constrain; they
must not silently become the model exposed to CCL.

The implementation is not yet fully policy-complete. `procmgr` no longer gives
every spawned child keyboard, mouse, filesystem, or wildcard process authority.
Desktop and standalone shell input registration is declared in their manifests.
Several service-registration capabilities are still assigned using hard-coded
`.cubit.id` comparisons, and manifest admission does not yet authenticate the
package or consult the final install/launch/approval policy. Initial kernel setup
clears the entire capability table and supplies only an attenuated self endpoint
and self-process capability. The remaining compatibility and policy gaps are
tracked in
[`security-hardening.md`](security-hardening.md) and must remain visible through
the future security-posture application until replaced by explicit,
authenticated policy.

Capabilities can be inserted, moved, removed, and checked for stale generations
at runtime. Consequently, the implementation does **not** currently justify a
blanket claim that all authority is immutable for a process lifetime. What may
be stable is the proposed manifest-derived authority request ceiling; dynamic
kernel capabilities, service sessions, grants, and reply capabilities remain
separate runtime state.

### Requests, handles and replies: one authority system

The [CCL exposure model](ccl-interactive-composition.md#one-interface-definition-explicit-exposure)
does not introduce another authority system. Embedded-only, published-only and
dual exposure describe where an operation may be offered, not who may use it.
Catalog discovery, invocation, stream opening and delegation remain scoped by
the existing grants and handles. There is no privileged global CCL interpreter.
For embedded scripts, the interpreter and host adapters enforce the narrower
script binding set inside the process; the kernel does not isolate scripts
from their native host. Published IPC adapters must preserve authenticated
caller scope rather than silently substituting the provider's broader authority.
Typed subscriptions and bulk streams use the same model; matching a schema
never authorizes access. See [typed streams](typed-ipc.md#calls-and-streams-share-one-interface-model).

Runtime stream wiring must independently authorize changing the binding,
releasing the source to the selected recipient, and accepting that source at the
destination. Approvals bind the controller, current connection/port generations
and selected contracts. Adapters and fan-out do not bypass those checks; every
edge needs approval before recipient-readable memory is exposed. Compatible
types are not authority, and static types do not validate hostile native payloads.
These are target invariants; the current pure admission model trusts its supplied
evidence and is not yet live enforcement. See [runtime wiring](stream-wiring.md).

The following are distinct stages and forms of authority, not three separate
permission systems. The previous "permissions / handles / reply capabilities"
presentation is superseded by this explanation.

### Authority requests and approved ceilings

An ELF manifest declares what the process may request. Installation, launch,
session policy and the issuer's scope constrain what may actually be granted.
A declaration is a request, not a grant. Approval is a decision, not proof that
the approved authority has been installed or a resource handle opened.

Examples include requests to observe network activity, create a desktop
surface, use a named secret for TLS, or ask the process manager to control a
specific application class.

The manifest-derived request ceiling is intended to be fixed for an admitted
executable instance. Policy approval and active grants can change independently;
the ceiling alone is not authorization. Only decisions based exclusively on
that stable ceiling may be cached without lifetime invalidation. Effective kernel
capabilities, service policy, sessions, grants, and external resources must be
revalidated according to their own lifetime and generation rules.

### Handles

A handle is the proposed application/CCL-level dynamic, typed reference to a
particular kernel or userspace resource: a service session, stream, surface,
subscription, secret-use session, device queue, or remote-node relationship.
It is backed by existing kernel capabilities and/or a service-managed object
identifier. Handle designs must be generation-tagged or otherwise protected
against stale reuse and scoped to their owning process or explicit delegation
domain. They are not native pointers.

An approved request ceiling does not imply possession of every corresponding
handle. A process may be eligible to request network observation without having
opened a telemetry session. Handle types constrain the available operations and
their ownership rules.

### Reply authority

A reply capability authorizes exactly one response to a particular pending
request. It cannot be used as a general endpoint, duplicated, or redirected.
Reply capabilities are consumed by sending, cancelling, or explicitly returning
the reply according to the protocol.

Inspection distinguishes these stages; not every protocol uses a separate
object or message for each stage:

```text
requested → approved → granted → opened/acquired → used → closed/retired
```

Security tools must not collapse these states into a single “has permission”
indicator.

### Content, security state and activation

Ordinary file/Config writes must not authorize modification of authority rules,
trust anchors, grants or executable activation records. Security metadata can
share physical storage with content only if the access paths preserve that
boundary. An application handle to content is never a raw-volume handle.

A dedicated **security-state volume** is a proposed defense-in-depth boundary,
not an implemented guarantee. Ordinary applications must have no discovery or
open grant for it; guessing its identifier must still fail. Trusted services
expose narrowly authorized typed management operations, not a general writable
policy file or database. Inspection authority remains separate from change
authority and must not disclose secret payloads.

A separate volume alone does not isolate a compromised filesystem or raw-block
driver that can access both volumes. Stronger containment requires scoped block
handles and potentially a separate filesystem instance. Encryption, authenticated
storage, rollback protection and a trusted boot/key path address different
offline threats; none follows merely from creating a partition. Existing ext2
on-disk compatibility is unchanged by this proposal.

Writing executable bytes does not authorize installation, startup registration
or activation, and replacing approved bytes must not silently inherit approval
of the old executable. Privileged CCL, plugins, startup configuration and stream
routing need the same consideration as native executables. Exact policy for
binding approvals to executable versions/provenance remains design work.

Prove separately that content operations preserve authority metadata and
unrelated storage, allocation cannot alias protected live blocks, and reused
storage does not reveal a previous holder's data. Memory-safety proofs alone
do not establish these properties. A compromised authorized editor can still
destroy its authorized documents; recovery copies require independent authority.

## Names and locators

CuBit names resources with locators such as `@net:tcp:example.org:443`,
`@system:fonts/a.ttf` or `@config:ui:theme`. A locator selects among
capabilities its holder already has; it never grants one. This is
deliberately not "everything is a file" or Redox-style URLs everywhere
(recorded 2026-09-27):

1. **A global string namespace is ambient authority.** If anything nameable
   is reachable, every service must check every name against every caller,
   and one missed check is a hole.
2. **Confused deputies.** A service handed a client's string tends to open it
   with its own rights.
3. **Names drift from objects.** What a string names can change between
   check and use, or between launches; a capability cannot be re-pointed.
4. **Uniform open/read/write flattens typed operations** into byte streams
   plus escape hatches (ioctl).
5. **Every boundary parses strings.** Hand-written parsers are where bugs
   live: netstack's own target parser accepted `10.0.2.4294967298` as
   `10.0.2.2` until it was replaced by a proved one.

Locators have three layers:

- **Syntax.** `@<authority>:<rest>`. The authority is a short name (lower-case
  letters, digits, hyphens). The rest is built from shared pieces:
  `:`-separated fields, `/`-separated paths (no `..`, no empty segments, no
  second authority inside), bounded decimals, and bracketed literals for
  values containing `:` (`@net:tcp:[2001:db8::1]:443`). One proved runtime
  unit splits and checks these; services compose their grammars from it
  rather than parsing by hand.
- **Resolution, per process.** A process's view maps each authority name to
  a capability slot it holds, optionally with a starting point inside it. The
  view is assembled at launch from the manifest and the approval, like the
  friendly storage roots above. A name absent from the view does not
  resolve, whatever any registry says. Resolution happens in the requester's
  view, and a service receives the resulting capability, never a string to
  open on the requester's behalf.
- **Interpretation, per service, with typed operations.** `@net` opens a
  channel, `@config` a configuration handle, `@system` a directory handle.
  There is no generic read/write on every resource.

**Aliases.** Well-known names such as `@system` (the OS volume) and `@config`
(the registered configuration service) are registered globally:

- **A registration defines what a name means; it grants nothing.** A process
  uses `@system` only if its view holds a capability for that object.
- **Only a trusted registrar registers** (procmgr's policy path, the
  installer), never an application. Registrations are typed: a filesystem
  alias cannot be bound to netstack. Every change is recorded in the audit
  trail.
- **Core names are reserved** (`@net`, `@system`, `@config`, ...); packages
  add names only inside their own namespace, so nothing shadows a core name.
- **Aliases bind at launch.** Re-pointing `@system` changes what new launches
  receive; running processes keep the object they were given. A registry
  change cannot redirect a running program.
- **Views may differ per application.** An application may receive `@system`
  bound to a narrowed subtree rather than the whole volume, as with
  `documents` today.
- **Device selectors stay internal.** `@nvme:0/` belongs to the registry and
  trusted configuration; applications name `@system` or `@documents`, never a
  device, so a backend is not part of their identity.

In CCL, each authority kind is a type and a locator literal is checked
against its kind's grammar at compile time ([control
language](control-language.md#locators-and-authority-kinds)).

Status: the syntax layer is implemented and proved: `CuBit.Locators` and
the `@net` grammar `CuBit.Net_Locator` (tests/locators), which netstack
now uses. Resolution, the registry and
aliases are not implemented.

## Network authority specialization

Network policy distributes the same endpoint capabilities used elsewhere.
Their kernel-stamped authority tags identify owner-bound scope records inside
netstack; the tag is neither a caller assertion nor a packed permission mask.
A general service endpoint does not imply connection, listening, configuration,
or driver authority. Outbound TCP destination-prefix/port scopes and exact
local-address/port listener scopes are independent. DNS resolution cannot widen
an outbound destination scope: the result is checked before connection creation.

A manifest is only a request. Initial implementation requires explicit trusted
boot approval (`network=declared`); ordinary app launches cannot self-approve.
This is not yet the persistent approved-installation ceiling or signed admission
design. Channels and listeners remain bound to their owning process and grant.
See [network authority](network-authority.md) for the implemented representation,
test/proof boundary, and outstanding lifecycle and policy work.

## Ownership and completion

Typed IPC declares whether an argument is copied, moved, borrowed-ro, or
borrowed-rw. Must-handle resources carry explicit completion verbs. Examples
include:

```text
Transaction: commit | rollback | return
Reply:       send | cancel | return
Frame:       commit | discard | return
Stream:      close | abort | return
```

Accepted asynchronous work creates an obligation that survives suspension.
Cancellation is an outcome, not disappearance; some operations are not
cancellable. The system must always be able to show who owns a resource, who
borrows it, which operation is pending, and how the obligation can be resolved.

## Storage authority specialization

Storage applies the existing permission, handle, reply-capability, ownership,
and shared-memory-grant model. It does not introduce a second capability system,
a storage-specific kernel capability table, or kernel `CAP_FILE` objects.
ISO9660, memory-backed storage, ext2, and future remote stores are backends
behind the same typed service interface and cannot alter its authority rules.

The storage concepts refine existing CuBit primitives as follows:

| Storage concept | Existing authority primitive |
|---|---|
| Permission to request storage operations | Manifest request intersected with installation, launch, and session policy |
| Permission to contact a storage service | Generation-checked endpoint capability |
| Particular volume, tree, directory, file, or change subscription | Typed, generation-tagged service handle |
| Asynchronous completion | Submission token and kernel-minted one-use reply capability |
| Bulk file data | Shared-memory grant created through an endpoint capability |
| File-change delivery | Typed stream or notification associated with a change-subscription handle |
| Resource limits | Kernel resource capability plus service-enforced handle, queue, memory, and I/O quotas |
| Handle revocation | Service-object generation invalidation, distinct from capability removal and memory-grant revocation |
| Explanation | Stable decision and delegation provenance attached to permissions and handles |

Kernel endpoint rights remain coarse transport authority. Storage verbs such as
`inspect`, `enumerate`, `read`, `append`, `replace`, `create-child`,
`remove-child`, `rename`, `watch`, and `delegate` are validated by the storage
service against a typed handle. Merely reaching the service endpoint does not
authorize an object operation.

For a process `P`, endpoint capability `E`, storage handle `H`, requested verb
`V`, and target object `T`, authorization requires the conjunction:

```text
Storage_Authorized(P, E, H, V, T) :=
    Endpoint_Current(E)
    and Endpoint_Allows_Invocation(E)
    and Handle_Current(H)
    and Handle_Owned_Or_Delegated_To(H, P)
    and Verb_In_Rights(V, H)
    and Target_Within_Scope(T, H)
    and Quota_Allows(P, H, V)
```

A rejected request changes no protected storage or authority state, although it
may append a bounded denial record. Successful child-handle derivation and
delegation are monotonic:

```text
Child.Rights subset-of Parent.Rights
Child.Scope  subset-of Parent.Scope
Child.Provenance extends Parent.Provenance
```

The manifest-derived storage permission is a stable maximum request profile;
it is not a file handle. Runtime policy may supply, attenuate, replace, expire,
or revoke concrete handles, but cannot exceed that maximum or the delegating
authority. An application cannot grant itself authority. Until general
capability copy/move exists, a trusted resource chooser or authenticated policy
service may mediate issuance of an attenuated handle to a target process; a raw
PID is not delegation authority.

### Current filesystem policy implementation and proof boundary

`CuBit.File_Access` implements the current service policy as bounded entries
containing a path-component scope and a named set of read/write/execute/create
rights. These remain the existing manifest wire bits; they are not Unix mode
bits. A matching entry must contain every requested right. Independent entries
are not combined to manufacture a stronger handle. Invalid paths and an empty
policy deny access, and a prefix such as `documents` never matches
`documents-private`. Execute is represented by the existing format but does not
replace executable-admission policy in procmgr.

The pure decoder rejects oversized prefixes, unknown rights bits, nonzero
reserved header bytes, NULs, and parent traversal. It builds a candidate before
publication; failed decoding is proved to produce an empty policy. The service
returns the input loan before replacing the active policy and invalidating that
owner's handles. Malformed updates leave both the active profile and its handles
unchanged. Only the current registered devmgr/procmgr can install or revoke a
profile. The old zero-entry *administrative* bootstrap request still explicitly
means wildcard; ordinary default/empty policies do not.

Procmgr rejects truncated/oversized scope declarations rather than shortening
them. Before each application launch it explicitly resets filesystem authority
for the still-suspended child PID, including missing/invalid-manifest launches;
failure to reset prevents resume. This closes inheritance of an old filesystem
profile through the normal PID-reuse launch path. It is not yet a general
generation-bound service-session or process-death notification protocol.

Directory navigation derives a read-only child from an owned, generation-checked
parent handle and a single component. It rechecks the existing application
profile, resolves within that parent's backend/inode, and rejects dot/dot-dot,
paths, backend selectors, NULs, and symlinks. Back retains prior handles; it does
not resolve `..`. Closing a handle is not subtree revocation; replacing/revoking
the application profile invalidates all its handles. Directory scans are live,
not snapshot-consistent transactions.

The two bounded helper packages are SPARK-proved; the entire filesystem service,
ext2 implementation, and trusted launch/IPC plumbing are not. QEMU tests cover
native scope enforcement and malformed directory handling. They do not establish
complete mediation under hostile raw-volume writers, malicious on-disk directory
hard links, concurrent rename/unlink, process-death races, or future delegation.
Those require the remaining object-lifetime and authority-model work below.

### Application roots and trusted resource selection

The filesystem/package implementation must preserve these additional model
obligations (the admission and persistence pieces are still planned):

1. Effective filesystem authority is bounded by both the application's request
   ceiling and the current installation approval. Approval eligibility alone
   does not issue a usable object handle.
2. Authenticating a package or publisher cannot itself grant storage,
   delegation, policy-editing, or deployment-activation authority.
3. Durable policy identifies an admitted application/installation and stable
   storage objects; live handles additionally identify a process instance and
   object generation. A reused PID, claimed identity string, renamed object, or
   replacement executable cannot manufacture identity continuity.
4. An update may retain previously approved data bindings only through an
   explicit continuity rule. New requested rights require a new approval;
   rollback of code does not imply rollback of data or external effects.
5. Rejected validation/authorization changes no protected object. A failure
   after I/O submission is a different outcome: report restoration, recovery
   required, or an outstanding noncancelable operation honestly. Do not hide
   uncertain writes behind an ordinary rejection or claim crash atomicity.

See [FS policy implementation](filesystem-maturity.md#fs-policy-implementation-and-package-lifecycle)
and [package installation policy](ccl-packages.md#filesystem-policy-and-installation-identity)
for the staged implementation and open continuity/persistence decisions.

Ordinary applications do not receive a current working directory, a global
root, or authority implied by knowing a pathname. Their storage namespace is a
small launch-time view assembled from typed handles. Stable names such as
`documents`, `pictures`, `project`, `application-data`, `cache`, and `temporary`
describe the purpose of a supplied handle; they are not ambient machine-wide
directories. Two applications may therefore receive different objects under
the same friendly name.

A manifest distinguishes private required storage from broader authority the
application may request. For example, a word processor can require its private
application-data area and permission to create its own projects, while declaring
that it may request a user-selected document or project tree. A photo editor can
similarly request selected images or a pictures collection. Installation and
launch policy bound the request, and an interactive grant cannot exceed either
that bound or the chooser's delegating handle.

The provisional `Files` application is both an explorer and a trusted resource
chooser. Its ordinary browsing view is constructed from explicit enumerable
root handles; removable media and newly mounted volumes do not silently appear
inside another application's authority. When invoked for Open, Save, Export,
or Select Folder, its desktop-owned selection surface shows the requesting
application's verified identity, requested verbs, selected scope, duration,
and stated purpose. A successful selection returns an attenuated file or tree
handle through an authenticated launch/session path. It does not return a
string that the requester can reinterpret as broader authority.

Explorer authority is separable by verb. Enumerating names does not imply
reading content, reading does not imply replacing, and create-child does not
imply removing or renaming existing children. Thumbnailing, indexing, preview,
and content-type detection must use explicitly delegated read handles and
bounded helper services rather than turning the explorer into an unrestricted
content parser. Administrative volume inspection and mounting are separate
roles from ordinary file selection.

Recent resources and bookmarks retain object identity plus delegation
provenance, not a promise that a remembered path is forever authorized. Reopen
may require current policy validation or renewed user approval. Save As should
return authority over the newly selected target; atomic replacement should use
a scoped transaction handle with an explicit committed, rolled-back, returned,
or uncertain outcome.

A shared-memory grant authorizes access to a memory range, not access to a file.
The storage handle determines which bytes may be read or written; the grant
determines where those bytes may travel. A read borrows its destination buffer
read-write until completion, while a write borrows its source buffer read-only.
Pending I/O remains a must-handle obligation until it completes, is validly
cancelled where supported, or is transferred. Accepted non-cancellable work is
not erased by subsequent revocation.

Storage delegation is a control plane, not a payload route. After
`storage.svc` delegates a volume-restricted block session to the filesystem,
steady-state I/O travels directly between the filesystem and block driver. For
eligible bulk I/O, the filesystem derives a narrower child loan from the
application's memory grant so the same physical pages reach the driver and its
IOMMU domain. `storage.svc` and filesystem-owned payload bounce buffers are not
on that path. The filesystem still validates the file handle, bounds, rights,
and extent translation before authorizing the derived loan. The complete data
path and explicit copy fallbacks are specified in
[Storage Control Plane and Zero-Copy Data Path](storage-io.md).

A file-change subscription is an ordinary typed service handle, provisionally
named `Change_Stream`. The authority to read content, enumerate names, observe
changes, and observe changes recursively remains distinct. Its event queue is
bounded and sequence-numbered; overflow is an explicit event that requires a
generation-tagged resnapshot. Subscription delivery cannot block the storage
commit path, and revocation follows the same ownership and generation rules as
other handles.

The storage specialization carries these candidate proof invariants:

1. A process receives no ambient storage authority.
2. Permission to invoke a storage service does not imply authority over any
   storage object.
3. A storage operation succeeds only with current endpoint and handle
   generations, valid ownership or delegation, sufficient rights, contained
   scope, and available quota.
4. Handle derivation and delegation cannot amplify rights or widen scope.
5. Runtime policy cannot exceed either the manifest-derived maximum permission
   profile or its own delegating authority.
6. A shared-memory grant never implies storage authority.
7. A stale, expired, closed, or revoked handle cannot authorize an operation.
8. Accepted asynchronous work resolves exactly once, borrowed memory is
   returned according to its ownership contract, and change-stream overflow is
   never silent.
9. Every storage backend enforces the same authority semantics; selecting or
   crossing a backend cannot widen authority.
10. A pathname, friendly application-root name, recent-resource entry, or
    bookmark is never sufficient evidence of storage authority.
11. The namespace visible to one process contains only roots explicitly
    supplied to that process; mounting or attaching storage does not widen it.
12. Resource selection can only attenuate a chooser's current delegation and
    the requester's manifest-derived maximum; it cannot amplify either.
13. Enumeration, content read, mutation, delegation, and volume administration
    remain independently authorizable operations.
14. A selected object is returned as a current typed handle through an
    authenticated channel, never reconstructed from an untrusted path string.
15. A derived memory loan cannot widen its parent's range or permissions, and
    a parent cannot resolve while an accepted child remains active.
16. Storage control-plane delegation does not require payload forwarding;
    eligible aligned bulk transfers use the same physical pages end to end.
17. DMA mappings contain only pages pinned for current accepted operations and
    are removed before the associated loan is returned.

The first executable enforcement steps are now in place: ordinary shared-memory
grant creation rejects ranges in the received-grant aperture. Consequently a
borrower cannot use the generic operation to forward borrowed pages, amplify a
read-only mapping to read-write, or detach a child mapping from its parent's
lifetime. The pure `Memory_Grants` SPARK policy defines and proves permission
and subrange attenuation plus the acquire/revoke/return lifecycle without
assumptions. Its private lifecycle type cannot represent an inactive grant with
borrowers or a pending revocation with no borrower. Explicit derived loans and
child liveness accounting remain future transitions and must not be inferred
from this single-hop state machine.

The same policy models a grant reference as an explicit global-slot and
generation pair. Slot construction provably recovers its owner and local slot;
generation zero is invalid; advancing a generation never wraps. Exhaustion
retires the slot. Process teardown now invalidates grants both created by and
targeting the dying process, preventing stale inbound records from following a
recycled PID. Kernel grant slots now store those persistent generations.
Owner-only lookup completes a newly created reference; generation-checked
revocation rejects a stale owner; and authoritative acquisition checks current
grantee, authenticated owner, generation, permission, and byte bounds before
returning a mapping. Its first acquisition pins every backing frame and its
final return unpins them. Revocation becomes pending and rejects new
acquisitions until the last borrower returns; owner teardown retains the grant,
backing storage, and process identity during that interval. Grantee teardown
forcibly returns all inbound acquisitions before deleting their mappings.

A typed userspace API and live negative tests exercise that path.
Application/filesystem requests and `Block.Device.V1` now carry both fields.
Filesystem.svc, ATA, and NVMe acquire each reference through the kernel with
direction-specific access requirements and complete byte bounds, use it, and
return it. HDA creates its PCM loan and the mixer acquires it through endpoint
capabilities, so neither operation trusts a supplied peer PID. Directory reads
carry their actual buffer capacity rather than inheriting the grant aperture's
maximum slot size. Other service protocols still transporting legacy
single-word grant IDs remain migration debt, so generation validation and
acquired lifetime are not yet end-to-end properties of all services.

These CPU-side pins prevent allocator reuse; they do not constrain bus-mastering
DMA. Until CuBit programs VT-d/AMD-Vi domains which contain only controller
state and currently acquired buffers, each DMA device and the driver programming
it remain in the memory-isolation trusted computing base. Parent/child loan
derivation, cancellation, and bounded pinned-memory quotas also remain open.

The current PID-indexed pathname-prefix ACL implementation and its backend
selector paths are transitional mechanisms, not evidence that these invariants
already hold. Filesystem and config services no longer make the first caller an
administrator: they accept policy operations only from the live devmgr or
procmgr identity in the kernel service registry, and an ordinary filesystem
client is tested to ensure it cannot grant itself access. This still identifies
an administrative role by a registry PID at request time rather than by a
dedicated operation capability. Migration must bind handles to process and
object generations, canonicalize names before authorization, define
already-open-handle revocation behavior, and replace PID-directed delegation
and registry-role checks with capability-authorized transitions.

## Explainable authority

Every security-relevant state and action must answer five questions:

### What

What typed operation, permission, handle, resource, policy rule, ownership
transition, denial, or exception is involved? Descriptions use stable interface,
operation, and schema identities with human-readable names. Raw numeric slots
are supporting detail, not the explanation.

### Who

Who requested, granted, delegated, approved, or exercised the authority? An
identity may include:

* process and isolate identity;
* package identity and content digest;
* signer and provenance chain;
* launching process or authenticated management principal;
* policy authority or approving administrator; and
* remote node identity authenticated by mutual TLS.

“Who” is an authenticated principal or software identity, not merely a mutable
display name. Identity is evidence for attribution and issuance decisions,
not an implicit grant. Explanations must identify the actual authorizing
capability and its issuer, rather than stopping at "administrator" or
"trusted publisher."

### When

When was authority requested, granted, acquired, first exercised, last
exercised, renewed, expired, returned, or revoked? Time records identify their
clock domain and certainty. Ordering must remain meaningful even when a trusted
wall clock is unavailable during boot or recovery.

### Where

Where does the authority apply? Scope may identify a local process, service
endpoint, operation, stream, device, desktop surface, configuration namespace,
secret, remote CuBit node, or other security boundary. “Network access” without
a destination or endpoint scope is not an adequate explanation when a narrower
scope exists.

### Why

Why was the action allowed or denied? The explanation identifies:

* the manifest request;
* the installation and launch policy decision;
* the delegation chain;
* the handle type and permitted operation;
* any active exception;
* the exact failed requirement for a denial; and
* a stable reason identifier suitable for tools and tests.

Two additional questions make an explanation operationally useful:

### How

How did authority reach this process or operation? CuBit records a delegation
path rather than presenting the final grant without context:

```text
desktop session 14
  → Network.Observe permission
  → telemetry session handle #7
  → network.telemetry.snapshot
```

### What next

What must happen to resolve the state? An explanation lists valid completion
verbs, expiration, review requirements, cancellation behavior, or the
consequences of removing authority.

## Uniform explanations

The following is a target rendering; signer and policy lines appear only when
the corresponding evidence is authenticated and available. Inspection surfaces
should present the same structured explanation regardless
of whether the subject is a kernel operation, IPC request, CCL import, stream,
UI handle, policy exception, or remote action.

```text
WHAT
  Read network telemetry

WHO
  network-dashboard.ccl
  signer: CuBit system examples

WHEN
  Granted at launch
  Last exercised 240 ms ago

WHERE
  netstack.service / telemetry.snapshot
  local node

WHY
  Requested by signed manifest
  Allowed by Local Diagnostics policy
  Delegated by desktop session 14

HOW
  desktop session 14
    → Network.Observe
    → observer handle #7
    → telemetry.snapshot

WHAT NEXT
  No unresolved ownership obligations
  Grant ends when the dashboard closes
```

Denials use the same model and identify a corrective requirement without
silently broadening authority:

```text
WHAT
  DENIED: network.control/reconfigure

WHO
  network-dashboard.ccl

WHERE
  netstack.service / control.reconfigure

WHY
  Observer handle #7 does not provide Network.Controller authority

WHAT NEXT
  Run through a policy profile that explicitly delegates Network.Controller
  Review the resulting control scope before launch
```

## Decision provenance

The target architecture requires authorization to produce structured provenance at the time a grant, handle,
delegation, exception, or denial is created. Security tooling must not
reconstruct the reason later from mutable configuration.

A decision record contains bounded identifiers for:

* subject identity;
* requested permission or typed operation;
* target scope;
* manifest declaration;
* policy version and matching rule;
* delegating authority and parent decision;
* decision result and stable reason code;
* exception identity, if any; and
* timestamps and lifecycle state.

The kernel fast path need not emit a verbose text log for every authorized
operation. Handles carry a stable reference to their grant provenance, and
services emit bounded structured transition events according to audit policy.
Human-readable explanations are rendered from versioned reason data. This keeps
explanation complete without turning routine IPC into unbounded logging.

## Exceptions

Real systems require exceptions. CuBit treats an exception as an open security
condition rather than an invisible configuration bit.

Every exception has:

* an owner responsible for review and resolution;
* the normal policy rule being overridden;
* an exact subject, operation, and resource scope;
* a human justification and stable machine-readable category;
* an activation time and explicit lifetime;
* whether it survives restart;
* an approving authority and delegation path;
* activity attributable to the exception;
* review and expiration state; and
* an explicit resolution outcome.

Defaults should favor narrow, temporary, non-persistent exceptions. A permanent
exception is possible only when policy explicitly supports it and must remain
visible as part of the effective security posture.

Removing an exception should support impact preview: which processes, handles,
pending operations, and future launches will be affected. Removal does not
pretend to cancel accepted non-cancellable work or recover already consumed
resources.

## Security observability protocol

The future security-posture application, CCL Workbench/debugger, and remote
management tools consume one versioned, typed inspection protocol. They must
not maintain separate guesses about kernel and service state.

The retired Security Center prototype demonstrated live kernel capability-slot
inspection, including type, rights, object reference, and effective endpoint
relationships, but its information architecture is not retained. The unified
protocol must expose those authoritative facts alongside grant-table state and
bounded reports from filesystem, configuration, stream, CCL, and other service
policy domains; it must not replace live kernel state with a reconstructed
manifest view.

The protocol provides bounded snapshots and event streams for:

* active, waiting, disabled, inactive, terminal, and retained CCL instances;
* all processes and authenticated package identities;
* requested and granted permissions;
* currently acquired handles and their types;
* reply capabilities and pending requests;
* ownership, borrow, move, and must-handle state;
* valid completion verbs and unresolved obligations;
* IPC and stream relationships;
* local and remote delegation paths;
* denials, quota violations, expirations, and revocations;
* active and historical exceptions; and
* proof, trust-boundary, and implementation-assurance status.

Inspection authority is itself explicit. A process may inspect its own state
without gaining system-wide visibility. System-wide observation, remote
inspection, policy modification, and authority control are distinct
permissions and handles. Explanation interfaces redact secret values while
retaining secret identity, use purpose, and decision provenance.

Snapshots are generation-tagged and internally consistent. Event streams carry
sequence numbers so tools can detect loss and request a new snapshot. The
protocol never exposes raw kernel pointers.

## CCL authority observatory

The CCL debugger includes an authority observatory for every loaded CCL,
including instances that are inactive or retained after termination. Each
instance exposes:

* source and bytecode digest, signer, and provenance;
* lifecycle state and the reason it entered that state;
* parent process, session, or launching principal;
* declared, granted, acquired, exercised, and retained authority;
* live service, stream, UI, timer, secret-use, and remote-node handles;
* pending imports, result types, cancellation modes, and deadlines;
* owned, moved, borrowed-ro, borrowed-rw, and must-handle values;
* unresolved completion verbs;
* fuel, memory, event-queue, IPC, and rendering budgets;
* recent structured events and denials; and
* active exceptions and their owners.

The debugger explains language execution and typed state. The future
security-posture application provides the system-wide authority, policy,
provenance, and exception view. Both use the same underlying records and stable
reason identifiers.

## User experience

Visibility is a primary security requirement, not optional diagnostic polish.
Operators must be able to understand and adjust policy without granting broad
bypass authority merely to make an application usable. The UI obtains its
answers from the same authoritative capability and decision records used by
enforcement, not a separate approximation of the security model.

In particular, distinguish **requested**, **eligible for approval**,
**currently granted**, and **actually exercised** authority. Eligibility is
not an issued handle, and an issued handle is not evidence that its rights
have been exercised. Show the resource and limits, holder and issuer, lifetime
and pending obligations, scope, and the manifest/policy/approval/delegation
chain: WHAT, WHO, WHEN, WHERE, and WHY.

A useful exception is "allow this editor to modify this project for this
session," not "run the editor as administrator." Such an exception must fit
the approved request ceiling or require an explicitly authorized change to
that ceiling. It remains a scoped, attributable capability issuance with
visible lifetime and revocation effects, never a bypass of enforcement.

Security interfaces lead with questions rather than implementation vocabulary:

* What can this program do?
* What is it doing now?
* Who gave it that authority?
* When did this change?
* Where does the authority apply?
* Why is this allowed or denied?
* How did authority reach it?
* What is unusual or unresolved?
* What happens if I remove it?

Expert views may expand endpoint identities, schemas, generation numbers,
ownership modes, policy versions, raw event records, and proof status. A graph
view shows delegation and resource relationships; a timeline shows decisions
and transitions; a diff view compares requested, granted, acquired, exercised,
and retained authority.

Risk presentation must distinguish a dangerous capability from evidence of
malicious behavior. It should show scope, exposure, actual exercise, surrounding
controls, and exceptions without claiming that a broad grant is itself an
exploit.

## Audit integrity and bounds

Audit and explanation mechanisms must resist both tampering and resource
exhaustion:

* records use bounded schemas and stable identities;
* privileged producers authenticate their event source;
* sequence gaps and dropped records are visible;
* retention and backpressure policies are explicit;
* high-rate events may aggregate counts while preserving grant provenance;
* secret material and sensitive payloads are never copied into explanations;
* access to audit data is separately authorized;
* remote export uses authenticated, typed channels; and
* durable logs should support integrity chaining or equivalent tamper evidence.

An audit overflow must create a visible security condition. It must not silently
discard precisely the evidence an operator believes is being retained.

## Enforcement and trust boundaries

Today, the kernel enforces process isolation, capability type and rights,
generation checks on relevant paths, mappings/grants, and reply-capability
semantics. `procmgr` parses ELF requests and performs privileged minting;
userspace services separately enforce application-level schemas, streams, and
path/configuration policy. The retired prototype could inspect the live
capability table, but no current application presents a system-wide posture
view. Not every capability path yet carries decision provenance, and not every
service policy is exposed through one protocol.

The target architecture retains those boundaries and adds a traceable policy
chain:

```text
ELF requests + authenticated package identity
        |
installation / launch / session policy
        |
authorized issuers arrange scoped kernel/service capabilities
        |
kernel capability slots + grants + service sessions/objects
        |
typed IPC and ownership state machines
        |
unified inspection protocol over authoritative kernel and service state
```

Generated typed interfaces should keep both sides aligned. Security-posture
tools observe and control through explicit authority; they are not privileged
bypasses around normal enforcement.

## Formal transition correspondence

The [model verification plan](security-model-verification.md) specifies the
initial invariants and hostile traces to formalize using this correspondence.
The [vocabulary](security-vocabulary.md) keeps user-facing terms and technical
transition names aligned; neither document constitutes a machine-checked proof.

The abstract security model must use stable semantic transitions rather than
syscall numbers. Syscalls and typed IPC messages are untrusted requests; an
abstract transition occurs only after the kernel or service has validated the
request and changed authoritative state. This distinction lets the public ABI
evolve without changing the theorem being proved.

The initial correspondence to the implementation is:

| Abstract transition | Current syscall, IPC operation, or internal path | Correspondence status |
|---|---|---|
| `Kernel_Bootstrap` | `Process.create` calls `Capabilities.Operations.grantInitialCaps`; `Modules.setup` installs the initial `CAP_CSPACE`, process, device, and registration capabilities with `insertCapAt` | Implemented internal origin. The model must distinguish self-scoped bootstrap authority from the global capability-space policy root. |
| `Process_Create` | `SYSCALL_SPAWN`, followed by `grantInitialCaps`; `procmgr` spawns suspended children before applying manifest policy | Implemented specialized syscall. It is a compound transition that creates process state and kernel-bootstrap capabilities. |
| `Policy_Mint` | `SYSCALL_POLICY_MINT_CAPABILITY` -> `Syscall.Admin.handleMintCap` -> `insertCapAt`, authorized by `CAP_CSPACE/RIGHT_GRANT` | Implemented policy-root construction. It is not ordinary derivation. The current operation replaces the selected slot, so replacement and any displaced ownership obligation must be explicit in the model. |
| `Capability_Derive` | Pure `Capabilities.derive` function | Defined and locally proved, but has no public invocation or live caller. A future capability-space operation must identify the parent slot and preserve object, generation, authority tag, and reduced rights. |
| `Capability_Mint_Authority_Tag` | Pure `Capabilities.mint` function | Defined and locally proved, but has no public invocation or live caller. This is authority tag-changing attenuation and is deliberately distinct from `Policy_Mint`. |
| `Capability_Copy` / `Capability_Move` | No general capability transfer exists in the current IPC message format | Not implemented. Ordinary IPC transfers message words, not capability-table entries. |
| `Capability_Drop` | Internal `Capabilities.Operations.removeCap`; `clearTable` during process teardown | Partially implemented internally; there is no public general drop operation. |
| `Capability_Invoke` | `SYSCALL_SEND_VIA_ENDPOINT_CAPABILITY`, `SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY`, and `SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY`; endpoint paths use `resolveCurrentEndpoint`. Transitional `SYSCALL_SEND_EVENT` accepts only a role-scoped notification publication grant or a current write endpoint and kernel-stamps its authority tag. | Implemented for endpoints and constrained event publication. The target ABI generalizes this into typed invocation of endpoint and kernel-object capabilities and eventually removes PID-spelled event delivery. |
| `Wait_Receive` | `SYSCALL_RECEIVE`, `SYSCALL_RECEIVE_UNTIL_MONOTONIC_MILLISECOND`, `SYSCALL_POLL_SERVICE_REQUEST`, event polling, and completion wait/poll paths | Implemented transport behavior, but receive still selects the process/default mailbox rather than an explicit endpoint or wait-set capability. The deadline form changes only blocking behavior; it grants no additional receive authority. |
| `Reply_Create` | Successful request receive paths install a kernel-created `CAP_REPLY` in `REPLY_CAP_SLOT` | Implemented. This transition is caused by accepting a request and must never be reachable through `Policy_Mint`, ordinary derivation, or transfer. |
| `Reply_Move` | `SYSCALL_MOVE_REPLY_CAPABILITY` -> `moveReplyCapFrom` | Implemented exact move from the calling thread's current reply capability into an empty deferred slot. |
| `Reply_Consume` | `SYSCALL_REPLY_AND_CONSUME_REPLY_CAPABILITY` -> `takeReplyCap`; PID-spelled `SYSCALL_REPLY` and `SYSCALL_REPLY_WAIT` consume matching reply authority through the compatibility path | Implemented. The explicit-slot path is the target; PID-spelled reply is migration debt. |
| `Memory_Grant_Create` | Preferred `SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY` -> `createGrant`; legacy `SYSCALL_CREATE_SHARED_MEMORY_GRANT_FOR_PROCESS_ID` names a PID and performs relationship scans | Implemented with a preferred capability-directed path and a legacy path to remove or constrain. A memory grant is a temporary mapping, not a capability-table entry. |
| `Memory_Grant_Acquire` | `SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT` and preferred `SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT_VIA_CAPABILITY` -> `acquireGrant` | Implemented. Validation and acquisition accounting are serialized with create, return, revoke, and teardown. Published mappings already own frame pins. The capability-directed path derives the expected owner from a current endpoint capability. |
| `Memory_Grant_Return` | `SYSCALL_RETURN_SHARED_MEMORY_GRANT_ACQUISITION` -> `returnGrant` | Implemented. Final return permits a pending revoke/deferred owner teardown to complete. Retirement removes mappings and acknowledges TLB invalidation before releasing mapping-owned frame pins. Without pending revocation the mapping remains live. |
| `Memory_Grant_Revoke` | `SYSCALL_REVOKE_SHARED_MEMORY_GRANT` -> `revokeGrant`; `revokeAllGrants` during process termination | Implemented and now named distinctly from capability removal or object invalidation. Revocation is pending rather than destructive while acquisitions remain. |
| `Object_Invalidate` | `Process.kill` advances the process generation; endpoint, process, and reply capabilities with the old generation become stale | Implemented for process-referencing capabilities. Exhausted generations prevent PID reuse rather than wrapping. |
| `Handle_Open` / `Handle_Close` | Service IPC such as filesystem `OP_OPEN`/`OP_CLOSE`, mixer `OP_AUDIO_OPEN`/`OP_AUDIO_CLOSE`, and netstack `OP_NET_OPEN`/`OP_NET_CLOSE` | Implemented independently by services using numeric session identifiers and caller checks. The future proof must refine each typed handle to its endpoint capability plus service-owned session state. |
| `Offer_Register` | `SYSCALL_REGISTER_DRIVER`, `SYSCALL_SET_WELL_KNOWN`, and the static `Sysinfo.DriverList` | Transitional bootstrap discovery, not the proposed authenticated interface catalog. Static driver IDs and hard-coded registration roles remain migration debt. |
| `Need_Resolve` | `procmgr.parseAndGrantManifest` handles `REQ_SERVICE`, queries `SYSINFO_REGISTERED_DRIVER`, and invokes `SYSCALL_POLICY_MINT_CAPABILITY`; hosted CCL currently links a separate `Granted_Bindings` view | Partially implemented with static service IDs and unauthenticated package identity. The target resolver matches `NEEDS`, authenticated offers, schema digests, and policy before performing `Policy_Mint` or delegation. |
| `Capability_Inspect` | `SYSCALL_INSPECT_CAPABILITY`, currently authorized by `CAP_PROCESS/RIGHT_READ` for the target | Implemented specialized syscall. The target is an explicit typed inspection authority and protocol with bounded disclosure. |
| `Process_Terminate` | `SYSCALL_EXIT` or capability-checked `SYSCALL_KILL` -> `Process.kill` | Implemented compound transition: replies and waiters are resolved, grants and registrations are invalidated, generations advance, and the process capability table is cleared. Backing frames and an owner's PID remain retained when an accepted acquisition still exists. |

The shared implementation and its current proof boundary are described in
[IPC buffer lifetimes](ipc-buffer-lifetimes.md), including the distinction
between lifetime protection and content immutability. Desktop attachment uses
these existing kernel verbs, not a separate authority mechanism.

The formal vocabulary intentionally disambiguates overloaded implementation
terms:

* `Policy_Mint` constructs authority under a capability-space policy root;
* `Capability_Derive` and `Capability_Mint_Authority_Tag` attenuate an identified
  parent capability;
* capability delegation is `Capability_Copy` or `Capability_Move`;
* `Memory_Grant_Create` creates a temporary shared mapping;
* `Memory_Grant_Acquire` / `Memory_Grant_Return` delimit its usable lifetime;
  and
* `Memory_Grant_Revoke`, `Capability_Drop`, and `Object_Invalidate` are three
  different state changes.

A successful kernel entry or service operation may refine to one abstract
transition, a sequence of transitions, or one atomic compound transition. A
rejected request refines to no authority or protected-resource state change,
although it may append a bounded denial record. Compatibility syscalls must be
mapped to the same semantic transitions as their replacements until they are
removed; omission from the model is not evidence that they are safe.

## Verification requirements

Security-critical components should prove, within published boundaries:

* manifest and policy parsing is free of runtime errors;
* once the compatibility/bootstrap grants are eliminated or explicitly
  modeled, undeclared authority cannot be minted;
* decision provenance always identifies a valid policy or delegation root;
* handle generation and ownership prevent forgery and stale reuse;
* reply capabilities are unique and use-once;
* IPC ownership transitions resolve exactly once;
* bounded snapshots and event queues cannot overflow silently;
* redaction never exposes secret payloads;
* exception scope cannot exceed the approving authority; and
* inspection and explanation do not themselves amplify authority;
* storage-handle derivation preserves rights and scope attenuation;
* storage backends cannot change authorization outcomes for equivalent typed
  objects; and
* shared-memory grants cannot be used as evidence of file authority.

Proof results must report which SMT provers ran, which obligations remain, and
whether assumptions or suppressed checks exist. Successful GNATprove process
exit without invoked provers is not proof.

## Initial implementation milestones

1. Define stable identity and reason-code schemas for grants and denials.
2. Attach provenance records to manifest decisions, kernel capability minting,
   grant creation, and service-managed handles without putting verbose logging
   on the IPC fast path.
3. Define the typed security snapshot and transition-event protocols.
4. Design and build a new security-posture application around What, Who, When,
   Where, Why, How, and What next; do not inherit the retired prototype's
   tab-oriented information architecture.
5. Add the CCL authority observatory for active and inactive instances.
6. Add delegation graph, timeline, authority-stage diff, and exception views.
7. Implement bounded audit loss detection and snapshot resynchronization.
8. Add exception creation, impact preview, expiration, review, and resolution.
9. Expose the protocol through explicitly authorized remote management.
10. Prove non-amplification, provenance completeness, bounds, and redaction for
    the initial inspection path.

## Target foundational rules

CuBit security work should converge on and preserve these rules. Any current
exception, including the bootstrap grants identified above, must be documented
as an implementation gap rather than treated as precedent:

1. No ambient authority.
2. A declaration requests authority; it does not grant it.
3. Authority is typed, scoped, attributable, and no broader than its source.
4. Dynamic resources have explicit ownership and completion semantics.
5. Every security decision answers What, Who, When, Where, and Why.
6. Every delegation can explain How authority arrived.
7. Every unresolved obligation or exception explains What next.
8. Exceptions are visible security conditions with owners and lifetimes.
9. Inspection uses authoritative structured state, not reconstructed guesses.
10. If an operation cannot be explained, it fails closed.
11. Identity is evidence, never ambient authority; there is no superuser bypass.
12. Policy governs capability distribution, not a parallel authorization path.
13. Launch and installation do not implicitly transfer authority. Requested,
    approval-eligible, granted, and exercised authority remain distinguishable.
14. Content access cannot change authority metadata or confer executable
    activation; raw storage and security-state administration are separate grants.
15. Incomplete, stale or unauthorized inspection is visibly unknown, not evidence
    of an empty authority set or a healthy system.
