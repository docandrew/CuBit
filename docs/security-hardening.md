# CuBit Security and Verification Ledger

Status: living document

This document records security, isolation, verification, and hardening issues
discovered during development. An entry remains open until its acceptance
criteria are met. It is not a vulnerability disclosure list; it includes
design debt and missing assurance as well as confirmed defects. The normative
security architecture and explainable-authority principles are defined in
[`security-model.md`](security-model.md).

Priority meanings:

* **P0**: known isolation break or practical authority escalation;
* **P1**: conflicts with CuBit's security model and should block a security
  milestone;
* **P2**: defense-in-depth, assurance, or maintainability work; and
* **P3**: longer-term hardening or research.

## Open issues

### SEC-021 — Formal authority model and complete security visibility

Priority: **P1**. Status: design recorded; model proof and end-to-end enforcement
correspondence not implemented.

Use the [shared vocabulary](security-vocabulary.md) and
[attack/proof plan](security-model-verification.md). Keep authority, handle,
grant and policy understandable without disguising endpoints, slots, memory
grants or issuer power as interchangeable concepts.

Acceptance criteria:

* Formalize explicit bootstrap roots and checked state transitions; prove bounded
  issuance first, then binding, lifetime/revocation and rejection preservation.
  Demonstrate useful allowed executions as well as hostile traces. Publish all
  assumptions; no proof by merely defining successful requests as authorized.
* Tie each abstract transition to current syscalls/service operations and their
  trusted adapters. Separate Lean model proofs, SPARK implementation contracts
  and native regression evidence; track remaining gaps under SEC-020/SEC-019.
* Make requested, approved and active authority distinguishable in authoritative
  inspection. Report scope, revision, missing providers, provenance gaps and
  broad bootstrap powers. Unknown/incomplete must never mean healthy or empty.
* Separate content writes, security-state management and executable activation.
  Design the protected security-state volume and scoped storage path without
  claiming partitioning protects against a shared compromised raw-block driver.
* Migrate ambiguous names in scoped, tested changes; preserve semantic types and
  avoid ABI-wide cosmetic renames or compatibility aliases.

### SEC-020 — Simplify and constrain bootstrap authority delegation

Priority: **P1**. Status: open; architecture audit and redesign required.

Audit the current concentration of bootstrap/issuance authority in devmgr and
the handoff through procmgr to services and applications. The present flow is
too difficult to explain and audit. Inventory the actual grants first; this
entry does not assert that devmgr literally owns every capability.

Acceptance criteria:

* Document who creates, owns, delegates, attenuates, and revokes each authority,
  from kernel bootstrap through devmgr/procmgr to service-owned resources.
  Distinguish permission to use a service from permission to grant its use.
* Separate temporary bootstrap powers from steady-state issuance. Give each
  broker only explicit, scoped delegation authority; define which bootstrap
  powers can be retired after handoff. Neither process ancestry nor a registered
  service identity should substitute for the required operation authority.
* Express policy through the existing capability/authority model and CCL
  manifests/configuration, not a parallel permissions system or superuser.
  Launch grants must stay within the executable's requests, approved policy,
  and the issuer's explicit issuance/delegation ceiling. Trace boot launches
  and Apps-menu launches separately, including Config and outbound networking.
* Make the grant chain and decision reasons inspectable by authorized tools.
  Add negative tests for self-granting, unauthorized delegation, scope widening,
  stale identities, and retained bootstrap powers; formalize and prove the
  non-amplification properties of the extracted decision logic where feasible.

Related: SEC-001, SEC-003, SEC-019 and the authority model in
[`security-model.md`](security-model.md#authority-model).

### SEC-019 — Legacy Config grant paths and persistence need replacement

Priority: **P1**. Status: partially addressed; raw-slot paths and unsafe
persistence removed. End-to-end assurance and durable storage remain open.

The [2026-09-26 Config audit](config-security-audit.md) records native typed
Turso persistence through ordinary Apps-launched Workbench and the focused
158-check SPARK run. Scalar defaults are still in-memory; broad registered-role
administration, subject-incarnation/restart binding and complete inspection
remain open. These narrower results do not close this issue.

Config's get/set/delete/list and ACL handlers now acquire owner- and
generation-checked references. Bootstrap, procmgr, and the Ada client migrated
together; old raw-slot frames are rejected, not supported through aliases.
Wire lengths are validated before narrowing, ACL parsing uses an owned snapshot,
and list overflow fails without publishing partial success. The pure
`Config_Protocol` decoder's runtime checks prove; this is not a proof of IPC
authentication, grant acquisition/lifetime, or the store implementation.

The authorization table has since been extracted into `Config_Authority`, a
private owned SPARK model. Candidate installs are all-or-nothing, invalid scope
lengths and unknown rights are rejected, and the ACL count is checked before
integer narrowing. Its rejection/revocation/isolation contracts prove; this
does not prove the IPC adapter. The normal procmgr launch path
now clears Config scopes before resuming a new child, matching its filesystem
policy reset and preventing stale PID-scoped Config inheritance. Kernel PID
lifecycle and the launcher remain outside this proof boundary.

The native entry table is now encapsulated by `Config_Store`. Its bounded owned
records, successful-write property and rejected-mutation preservation prove
without assumptions (35 runtime checks, two functional contracts). This removes
direct entry mutation from IPC handlers, but is not a proof of the whole service
or global key uniqueness. Turso is evaluated separately on Linux; it is not a
native dependency or a proved persistence layer.

The key=value persistence stub could truncate its backing file before committing,
silently omit entries after 4 KiB, and acknowledge success after failure. It and
the implicit `config.store` overlay have been removed, together with Config's
bootstrap filesystem authority and the unused C client. Updates are currently
volatile. Add versioned, complete-revision publication and recovery only after
establishing the storage flush/ordering contract; see the Turso evaluation in
the Config roadmap. The borrowed-buffer Ada client still requires serialized
use; it is not a thread-safe owned-value API.

Acceptance: hostile request and foreign/stale/revoked grant tests for every
operation; validated lifecycle/scoped authority model; no raw-slot Config path;
no partial load publication or successful truncated save; interrupted-commit
tests and explicit volatile-versus-durable results. See
[Config contexts and inspection](config-contexts-and-inspection.md).

### SEC-018 — Periodic CCL host needs isolated, deadline-aware dispatch

Priority: P2 (before claiming bounded wall-clock responsiveness)

The first periodic label runs in the native control application's serialized
event loop with a fixed Clock-only binding set. It wakes from network accept
at its timer deadline, coalesces missed periods, and gives each evaluation
fresh fuel and string storage. A failed evaluation stops further invocations.
Its retained state survives browser disconnection, not host exit or reboot.

The state machine is proved, but fuel does not bound a blocking IPC call.
An incomplete HTTP request can delay a tick until its five-second deadline;
other synchronous host calls can delay it further. A stop request cannot be
processed by this serialized adapter while it is blocked in a host call.
Do not describe this as hard real-time, asynchronous cancellation, or a
multi-tenant runtime. The browser's generation check prevents stale actions
within the one lab slot; it is not remote authentication or authority.

Acceptance: deadline-aware asynchronous host dispatch with explicit
non-cancellable completion semantics, per-instance admitted grant/budget
ownership, fair event scheduling, and lifecycle/resource reclamation tests.
Authenticated NEEDS resolution, a general typed UI surface protocol, and saved
widget deployment remain separate follow-ups. Keep SEC-016's remote-exposure
restriction until network owner-death reclamation is complete.

### SEC-016 — Network owner-death and failed-reply reclamation are incomplete

Priority: P1 (before exposing a remote management service beyond an isolated lab)

The native accept path now returns grants/reservations on timeout, listener
close, and failed accept delivery. General process-death teardown still needs
to release that owner's listeners, accepted/outbound channels, acquired buffers,
and pending operations. An application disappearing after successful accept
does not currently cause that complete cleanup.

Code inspection also found that `Process.IPC.replyCap` returns early for a closed
target mailbox or generation mismatch before consuming the saved reply cap and
clearing its deferred-slot bitmap. A service can release its own pending record
but retain an unusable reply slot; later `saveReplyCap` correctly refuses to
overwrite it. This needs a deliberate consume/discard-on-terminal-failure rule,
not permissive overwriting of arbitrary live reply capabilities.

Acceptance: regress caller death before/after deferred accept, PID reuse,
failed delivery and subsequent slot reuse; verify zero leaked reply slots,
grant acquisitions, listeners and TCP reservations; retain single-use and
wrong-generation rejection properties. No kernel reply semantics were changed
as part of the initial passive TCP integration. Internet exposure also remains
blocked on the TCP and TLS/admission requirements in
`network-inbound-implementation.md`.

### SEC-015 — CBOR dependency and remote codec assurance are provisional

Priority: P2 (must resolve the applicable obligations before adopting a remote ABI)

The [hosted CBOR evaluation](ccl-cbor-evaluation.md) pins an unmodified source
revision, passes the normal 697 upstream tests and a contract-enabled local
corpus, and proves 865 core/schema analysis items. It is not linked into a
native service. Three additional upstream float property assertions remain
unproved locally; all-contracts upstream testing also exposes negative-bound
anonymous array fixtures that violate the decoder's documented precondition.

Acceptance criteria before production adoption:

* reconcile the relevant upstream proof/test discrepancies with recorded
  toolchains and without assumptions or suppressed failures;
* define and verify the actual bounded message schema, accepted CBOR subset,
  versioning and deterministic-encoding rules where hashes/signatures need them;
* validate UTF-8 at encoding boundaries, and distinguish complete-message
  validation from single-container-header decoding;
* budget full call-chain and secondary-stack use, buffer lifetimes, copying,
  message admission and work limits—not merely explicit heap allocation;
* enforce session authority and reference lifetimes independently of successful
  decoding, with malformed traffic and disconnect regressions; and
* require scoped inspection and redaction for any eventual message tracer.

### SEC-014 — Kernel locking and process retirement are not end-to-end sound

Priority: P1

The [kernel locking audit](kernel-locking.md) adds a proved production ownership
policy and an atomic owner-word adapter, fixes sleep/wakeup serialization and a
missing process-queue tail update, and adds native concurrent/queue regressions.
This is not a proof of whole-kernel deadlock freedom or safe process retirement.

The follow-up [process retirement implementation](kernel-process-retirement.md)
addresses those two concrete paths: stop/CPU-departure/claim precede cleanup on
a separate worker stack, and mailbox cleanup takes mailbox-before-process locks.
It also serializes pending/completion accounting, reserves completion capacity,
removes stale queued sender identities before PID reuse, and generation-binds
final IPC/grant admission. The pure lifetime core proves all 30 checks; hosted
concurrency and actual QEMU exit/saturation regressions supplement that proof.

This item remains open for the full intrusive-queue/handoff proof, concurrent
capability-table publication, hardware/IRQ registry and DMA quiescence, explicit
remote-kill/PID-reuse integration coverage, and interrupt-blackout measurements.
See the retirement document for boundaries and the asynchronous kill semantics.

Acceptance criteria:

* no stack, page table, or PID reuse until every executing context has stopped
  using the retired process, with explicit CPU acknowledgment/handoff;
* mailbox teardown observes the same lifetime and locking protocol as producers;
* blocked/runnable state and intrusive queue membership transitions obey a
  documented, verified ownership protocol;
* lock-order and hardware atomic assumptions are stated separately from SPARK
  state proofs, with targeted concurrent regressions; and
* latency measurements cover wait/hold and full interrupt-disabled intervals
  before making bounded-latency claims.

### SEC-001 — Ambient capabilities granted to every spawned process

Priority: P0

`procmgr` historically minted keyboard-focus, mouse-focus, and wildcard process
management capabilities for every child, independently of the child's ELF
manifest. This violated the intended rule that downloaded code receives only
explicitly declared authority. The wildcard process capability was especially
broad; input registration permits a process to contend for global input
ownership.

Remediation direction: remove these unconditional grants. Represent each
authority in the manifest and apply policy at installation or launch. If a
short-lived compatibility policy is necessary, restrict it by authenticated
package identity and record it explicitly.

Acceptance criteria:

* a zero-authority process receives no keyboard, mouse, or process-management
  capability;
* each grant can be traced to a manifest declaration and policy decision; and
* negative tests prove that undeclared registration and process control fail.

Current progress: kernel process creation now clears the complete capability
table and installs only an attenuated self endpoint and self-process handle.
Filesystem authority is no longer ambient. Capability-table administration is
represented by a distinct `CAP_CSPACE`; ordinary `CAP_PROCESS/GRANT` can no
longer invoke `MINT_CAP`. The unconditional input and wildcard process grants
have been removed; desktop and standalone shell input registration is now an
explicit manifest request. A headless adversarial test covers the former
self-mint escalation and confirms that an empty manifest receives none of the
filesystem, input, or wildcard process slots. Authenticated package-policy
admission for declared input authority remains open, so SEC-001 is not closed.

### SEC-012 — PID-directed IPC bypasses endpoint authority

Priority: P0

The legacy `SEND`, `CALL`, and `SUBMIT` syscalls accept a destination PID rather
than a capability slot. The first two have no live source callers; raw submit is
still used by shell inspection, child-stream, process-manager, and log-store
flows. `SEND_EVENT` no longer treats an IRQ capability as ambient publication
authority: userspace must hold a read-only notification grant whose registered
role currently resolves to the destination, or a generation-current writable
endpoint capability. The kernel replaces any producer authority tag with the resolved
authority authority tag and returns bounded queue rejection to the caller.

The complete call-site inventory and staged removal plan are maintained in
[`legacy-ipc-audit.md`](legacy-ipc-audit.md).

Acceptance criteria:

* every destination-directed IPC operation resolves a typed, rights-checked,
  generation-valid endpoint, notification, session, or reply handle;
* no production syscall derives messaging authority from knowledge of a PID;
* administrative inspection uses an explicit broker authority rather than
  temporary arbitrary endpoints; and
* rebuilt first-party artifacts contain no calls to the removed syscall ABI.

### SEC-002 — Core userspace services are not yet SPARK-proved

Priority: P1

Core services are generally compiled with optimized Ada builds, but
`SPARK_Mode` annotations and successful compilation do not establish proof.
The process manager, filesystem, configuration, networking, storage, display,
and other privileged services still have outstanding proof and hardening work.

The [2026-09-06 desktop freeze audit](desktop-freeze-correctness-audit.md)
records a kernel double fault, the context-switch interrupt-restoration repair,
and the remaining display/GPU validation and service-progress boundaries.
The focused capability proofs must not be described as a whole-kernel or
whole-desktop proof of crash freedom.

Remediation direction: define per-service proof boundaries, isolate unavoidable
non-SPARK code behind small contracts, enable reproducible GNATprove targets,
and ratchet proof levels in CI. Start with authority parsing/minting and IPC
message validation in `procmgr`, since errors there can amplify authority.

Acceptance criteria:

* each core service publishes its trusted-code boundary and proof status;
* absence of runtime errors is proved for its SPARK core without suppressed
  verification conditions; and
* parsing, bounds, capability-policy, and IPC-state invariants have explicit
  contracts and regression tests.

### SEC-003 — Service-registration policy is hard-coded in procmgr

Priority: P2

Driver/service registration authority is currently minted through repeated
package-identity checks in `procmgr`. The CCL test host follows this existing
pattern. It is narrow, but policy encoded as ad hoc string comparisons is hard
to audit and scale.

Remediation direction: use a structured, versioned service-provider declaration
and a central policy table or installation record. Package identity must not by
itself imply authority unless authenticated by the package trust mechanism.

Acceptance criteria:

* registration grants are data-driven and validated once;
* duplicate provider identities and driver claims fail closed; and
* policy decisions are inspectable through the security UI or audit stream.

### SEC-004 — CCL proof is incomplete

Priority: P2

The CCL parser, type checker, verifier, and VM use bounded storage and
`SPARK_Mode => On`, but outstanding verification conditions remain. Typed host
imports have behavioral tests, not yet a proof that verified bytecode cannot
forge authority, violate stack typing, or exceed its accounting bounds.

Current progress: the standalone ownership-state engine has native behavioral
fixtures and passes its current GNATprove level-1 flow and runtime-safety checks.
The separate ownership bytecode verifier also passes native fixtures and its
current GNATprove level-1 checks, including control-flow ownership joins. The
primary VM verifier now consumes this ownership result and runtime execution
mirrors it. The result is not yet an end-to-end authority-preservation proof:
`.cclb` v3 now validates and serializes descriptor-pinned imports, including
borrow/move and cancellation metadata, without runtime bindings. The contracts
do not yet express the full end-to-end authority-preservation theorem.

The VM now exposes a machine-state well-formedness predicate and proves its
preservation across initialization, execution, host completion, and owned-import
submission acknowledgment. Adding those contracts found a real integration
defect: terminal host-completion paths cleared `Waiting` but could leave
`Waiting_Owned` set. Later proof work found the converse defect on internal
ownership-transition errors: the VM cleared both flags even though the import
lifecycle correctly remained offered or accepted. Error paths now retain the
pending state, and successful transitions publish the waiting flags atomically.
Regression tests exercise offer, accepted completion, rejected submission, and
wrong-type completion.

The focused `CCL.VM` GNATprove level-2 run now has no unproved runtime checks,
assertions, or functional contracts. The instruction position is an exact
modular slot index; execution fuel is a separately proved ADT; suspension state
is staged locally and published atomically; and explicit loop invariants capture
the rule that a waiting import ends the current dispatch pass. This is not yet
an end-to-end authority-preservation theorem for the complete CCL toolchain.
The same focused proof now includes the bounded debugger inspection path.
Inspection copies the private operand stack, locals, ownership/borrow state, and
pending-import metadata into a value snapshot; it exposes no operation that can
write the machine state. The native suite checks both stack and initialized-local
snapshots and then continues execution, guarding against accidental observation
side effects.

VM signed addition now computes in a proved 128-bit intermediate and narrows to
`Integer_64` only after an explicit range-membership test. Its isolated
GNATprove run discharges all checks. Native tests cover both
`Integer_64'Last + 1` and `Integer_64'First + (-1)`. The source interpreter's
separate arithmetic path checks overflow before performing addition.

The shared source frontend now exposes a bounded typed syntax tree consumed by
direct interpretation and future CCLB compilation. A focused GNATprove level-1
run discharges every check in `CCL.Language`, including parsing, type checking,
tree construction, evaluation, overflow handling, and the public fuel bound.
This boundary contains no `pragma Assume` statements or SPARK-disabled code.
The broader CCL project still has outstanding obligations in other packages;
this result must not be presented as an end-to-end proof of the toolchain.

The initial typed-AST compiler and bounded debug-map ADT also pass focused
GNATprove level-1 runs. Debug-map validation and innermost-range lookup are
proved free of runtime errors, but the metadata is deliberately outside the VM
admission theorem: it may explain verified bytecode and never authorize or
modify it.

The source compiler no longer contains a clock-specific form or static service
binding. An explicit bounded interface catalog controls what operations may be
discovered. Descriptors contain no runtime binding; compilation emits unresolved
imports plus descriptor-pinned linkage. A separate granted-binding view performs
transactional admission and rejects absent authority or a substituted import
contract without partially linking the program. The catalog and linker, generic
compiler lowering, and catalog-aware frontend discharge their level-1 GNATprove
checks without `pragma Assume` or SPARK-disabled code. This is not yet an
end-to-end authority-preservation proof: descriptor hash validation and binding
to kernel-enforced opaque handles remain open. Initial CCLB v3 linkage
serialization is implemented and behaviorally tested; its focused proof pass is
still pending.

The verifier and executable operand stacks now share a separate bounded-stack
ADT rather than each manipulating a raw array and depth independently. Its
private representation uses an exact modular slot index, a separately
capacity-checked unsigned count, and canonical clearing on pop. `Push`, `Pop`,
and `Peek_Top` return explicit full/empty/invalid results; neither VM path now
performs stack indexing or depth arithmetic. Both concrete instantiations have
no outstanding GNATprove level-1 runtime-safety checks, and the native suite
includes an explicit maximum-depth overflow fixture.

Migrating host completion to the ADT exposed a lifecycle-ordering defect. An
accepted owned import previously validated its response type and available
stack space before completing the ownership transition. A bad response could
therefore terminate the VM while leaving the binding suspended. Accepted owned
imports now complete their lifecycle transition first, then validate and push
the response; a regression test covers rejection of a wrong response type
without retaining suspended ownership.

The Nix development shell now includes cvc5 and Z3. Previously it provided
GNATprove and Why3 but no SMT executable: verification runs generated VCs and
performed flow analysis while the proof summary showed zero invoked provers.
Proof claims must check the summary's prover counts, not merely command success.

Acceptance criteria:

* absence of runtime errors is proved for the CCL core;
* verifier preservation and VM stack invariants are expressed as contracts;
* fuel accounting is proved monotonic and bounded; and
* host imports are proved to reference only declared bindings with matching
  argument and result types.

### SEC-005 — CCL host scheduling needs bounded outstanding-work policy

Priority: P2

The VM can suspend for a host import, but a production multi-isolate host will
need quotas for outstanding IPC, completion storage, elapsed deadlines, and
per-isolate memory. Fuel bounds CPU execution; they do not bound time or
resources held by an external operation.

Current mitigation: the bounded scheduler has four fixed isolate slots, one
outstanding import per isolate, round-robin runnable selection, and
generation-tagged tokens. Native and in-guest tests show that another isolate
runs while an import waits and that an unknown token cannot resume it. Memory
arenas, deadlines, cancellation, and host-wide fairness remain open.

Acceptance criteria:

* each isolate has explicit in-flight, memory, and deadline quotas;
* late, duplicated, unknown, and cancelled completion tokens fail safely; and
* one isolate cannot starve completion processing for another.

### SEC-006 — Completion APIs do not encode output-buffer size in their types

Priority: P1

The userspace `waitCompletion` wrapper accepts an untyped address plus a count,
while the kernel imports that address as a full `CompletionRing` and clears the
entire ring before draining entries. Passing the address of one
`CompletionEntry`, even with `max = 1`, therefore overwrites adjacent userspace
memory. The CCL adapter caught and corrected such a misuse during integration.

Immediate mitigation: the kernel now initializes and writes only the clamped
`maxEntries` extent, and CCL supplies a complete ring. The raw API still makes
buffer-size mismatches expressible, so this issue remains open.

Remediation direction: provide typed single-entry and ring APIs whose writable
extent matches their parameters. The kernel must validate and write only the
requested number of entries rather than assigning a full imported ring.

Acceptance criteria:

* callers cannot express an undersized completion buffer through the safe Ada
  API;
* the kernel writes at most `maxEntries` records;
* zero, one, maximum, and invalid counts have guard-page regression tests; and
* raw address-based wrappers are confined to a small reviewed binding layer.

### SEC-007 — CCL module provenance envelope is not implemented

Priority: P1 before installing or remotely accepting modules

The canonical `.cclb` payload and bounded loader are implemented, but publisher
identity, content digests, signatures, expiry, and deployment constraints are
not yet carried or verified. Unsigned modules are suitable for local developer
tests only. A signature must establish provenance without implicitly granting
the imports declared by a module.

Acceptance criteria:

* a versioned envelope binds the exact canonical payload, module identity,
  publisher identity, declared effects/authority, resources, and policy version;
* signature and digest algorithms are explicit and downgrade-resistant;
* malformed, unknown-key, expired, replayed, and altered envelopes fail closed;
* trust and authority policy remain separate decisions; and
* verified cryptographic code or a narrowly scoped verification service forms
  the trust boundary.

### SEC-008 — Ownership locals are not yet bound to opaque host authority

Priority: P1 before authority-bearing locals reach host imports

The merged VM models ownership locals and disposition transitions, but these
locals currently contain abstract state only. They cannot yet invoke a host
operation. When opaque descriptors and authority values are added, declarations
inside a module must be treated as requirements, never as constructors of
authority.

Current mitigation: `.cclb` v3 serializes declarations but never local values.
Modules declaring locals are rejected by the ordinary VM
initializer. The explicit host-injection initializer requires the exact local
count and matching value-kind and ownership-type tags. Native tests cover
missing and mismatched injection, and the in-guest ownership test uses the
explicit path. Opaque descriptor identity and session-policy lookup remain to
be implemented, so this issue remains open.

Acceptance criteria:

* isolate creation matches every required initial local to a value explicitly
  supplied by session or installation policy;
* type, scope, ownership mode, and disposition protocol are checked at injection;
* missing, extra, or mismatched values fail module instantiation;
* no bytecode instruction can construct or retag an opaque authority value; and
* negative tests prove that declaring an authority local grants nothing.

### SEC-009 — IPC and stream schemas are not yet broker-enforced

Priority: P1 before ownership-bearing CCL imports

IPC labels and legacy stream type tags do not prove that producers, consumers,
and brokers agree on a wire schema or ownership transition. A mismatch can be a
memory-safety, confused-deputy, or authority-lifecycle defect even when endpoint
authorization itself is correct.

Current mitigation: the pure `CuBit.Protocols` metadata model defines stable
interface/schema identities, versions, fixed sizes, transfer modes, completion
effects, and outstanding-operation bounds. Both CCL test IPC endpoints now use
one shared contract. Typed stream producers retain a schema contract and reject
mismatched writes. Typed subscribers negotiate identity, version, fixed versus
bounded sizing, and size before the producer creates a read-only grant. Focused
negative tests cover each mismatch. The syscall ABI remains unchanged;
manifest requirements, broker matching, broad stream migration, and CCL import
enforcement remain open.

Acceptance criteria:

* generated client and server stubs share one canonical interface definition;
* endpoint binding rejects incompatible interface and schema versions;
* stream subscription rejects incompatible entry schemas before sharing memory;
* every accepted move or borrow has exactly one verified terminal completion;
* cancellation explicitly consumes, returns, or transitions owned arguments;
* raw/untyped IPC and streams are isolated as an explicit compatibility class;
  and
* negative tests cover schema confusion, version mismatch, duplicate completion,
  cancellation, and failure before versus after accepted submission.

CCL progress: the standalone ownership bytecode verifier now admits abstract
async imports only when copy/move/borrow rules close on both completion paths.
Its native negative fixtures and isolated GNATprove level-1 pass are clean. The
executable VM and `.cclb` format do not expose this operation yet; admission
must remain unavailable there until submission acceptance and terminal
completion are distinct runtime events.

The standalone `CCL.Imports` lifecycle now provides that distinction and models
not-cancellable, best-effort, and guaranteed-request cancellation policies.
Tests cover ownership preservation on enqueue rejection, suspension after
acceptance, cancellation without premature release, terminal disposition,
non-cancellable requests, returned borrows, and duplicate completion. Its
isolated GNATprove level-1 pass is clean. Integration into VM machine state is
now implemented for non-cancellable owned imports, including explicit enqueue
acknowledgment and fail-closed rejection. Native tests exercise offer,
acceptance, terminal completion/resume, and enqueue rejection; the CuBit guest
regression remains clean. Cancellable VM imports remain inadmissible until their
third ownership outcome is statically joined. `.cclb` v3 preserves the complete
owned-import contract even when current VM admission rejects that policy.

### SEC-010 — Authority inspection authorization and retention are provisional

Priority: P1 before remote inspection or policy control

The first explainable-authority slice records bounded launch provenance in
`procmgr`. The retired Security Center prototype correlated those records with
live kernel capability slots, but the unauthenticated
`com.cubit.security-center` package-ID exception has been removed. No process
currently receives system-wide inspection merely by naming itself. A new
posture application remains blocked on a dedicated inspection authority and
authenticated launch policy.

The ledger is also a bounded 512-record ring and currently stores one current
launch explanation per PID/slot. Eviction, PID reuse, slot replacement, and
multiple historical decisions for one slot need explicit generation and loss
semantics before this becomes an audit record. Kernel capability inspection
remains the source of effective state; the ledger must never become an
enforcement oracle.

Acceptance criteria:

* system-wide inspection requires a dedicated kernel-enforced authority;
* authenticated package/launch policy, not self-declared identity text, grants
  that authority;
* records bind PID, process generation, capability generation, and stable
  authority identity;
* replacement and derivation preserve parent lineage rather than overwriting
  history;
* bounded eviction exposes a sequence gap or loss condition;
* self-inspection and system-wide inspection remain distinct; and
* malformed or unauthorized queries fail without disclosing provenance.

### SEC-011 — Kernel IPC process-state proof boundary is incomplete

Priority: P1

2026-09-06 follow-up: [interrupt/lock handoff](kernel-interrupt-handoff.md)
now has a production SPARK state ADT and proved handoff-independence and nested
exclusion properties. Invalid blanket SPARK annotations have been corrected so
the buildable kernel passes legality checking. This is not a scheduler/IPC
functional proof: hardware/lock ownership, process-queue transitions, and the
address-overlay assumptions documented there remain open.

Concurrent network traffic exposed a check-then-wake race in kernel IPC.
Notification producers could observe a waiting process, lose the race to a
producer on another CPU, and then panic the kernel when `Process.notify`
observed the process already ready or running.  The wake transition now treats
that narrowly defined already-awake outcome as idempotent while retaining an
invariant failure for unrelated states.

Before the boundary correction above, the optimized kernel built but neither
the full-kernel nor focused `Process` GNATprove run reached its proof obligations.
GNAT 16 rejected ACPI volatile imports, allocator-backed linked lists,
anonymous-access spinlock labels, and other package-level constructs in SPARK.
Removing those invalid annotations does not prove the excluded implementations.

Refactoring direction: split IPC policy from mechanism.  Introduce a pure
SPARK state-transition component whose inputs are the current process state and
a typed wake reason, and whose outputs are the next state and an explicit
accepted/idempotent/rejected result.  It should contain no scheduler queues,
access types, locks, volatile objects, or machine-code dependencies.  The
non-SPARK kernel boundary will hold the required lock, obtain raw state, invoke
the proved transition, and apply the resulting scheduler operation.  Context
switching, MMIO, imported memory overlays, page-table manipulation, and
allocator internals may remain explicitly non-SPARK behind similarly narrow
reviewed interfaces; capability policy, IPC lifecycle, bounds, and ownership
state should remain in the proved core.

Acceptance criteria:

* wake-state observation and transition are atomic under a documented lock;
* a pure SPARK transition function accepts a typed wake reason and proves that
  it can wake only the corresponding wait state;
* duplicate event and completion wakeups are proved idempotent;
* reply wakeups cannot overwrite or deliver a stale reply to a later wait;
* an SMP regression test races network/event delivery with IPC waits without a
  panic or lost work; and
* the `Process` IPC proof boundary is accepted by GNATprove with runtime-safety
  obligations discharged.

### SEC-013 — Bus-mastering devices are not isolated by an IOMMU

Priority: P1 before claiming containment of userspace device drivers

Shared-memory acquisitions now validate ownership, range, generation, and
access, then pin their physical frames until the final return. DMA allocations
are also ownership-tagged. These are CPU-side lifetime and provenance controls;
they do not restrict addresses a bus-mastering device can access. HDA and NVMe
currently receive physical DMA addresses, so the device and the userspace
driver programming it remain in CuBit's memory-isolation trusted computing
base.

Implement Intel VT-d first for the current hardware target, followed by AMD-Vi.
The kernel should own immutable IOMMU policy and expose a narrow typed mapping
mechanism: a device is assigned to one isolation domain, and that domain admits
only its bounded controller queues, descriptors, and pages pinned for accepted
operations. Drivers receive opaque I/O virtual addresses rather than arbitrary
physical addresses. Domain invalidation must complete before an acquisition is
returned and its frames can be reused.

Acceptance criteria:

* boot discovers and validates DMAR/IOMMU topology without trusting a driver;
* every bus-mastering device is blocked or assigned to an explicit domain
  before it can initiate DMA;
* mappings can only name kernel-validated DMA allocations or currently acquired
  loan pages with compatible direction and bounds;
* unmap plus required IOTLB invalidation completes before frame reuse;
* driver termination tears down or quarantines its domain without exposing
  other memory;
* QEMU tests demonstrate that DMA outside the domain faults and cannot corrupt
  a sentinel page; and
* systems running without an IOMMU visibly report degraded DMA containment and
  never claim full userspace-driver isolation.

## Resolved issues

### SEC-017 — Host-enabled interpreter proof coverage completed

Resolved 2026-09-08: the focused language/catalog/concrete test-host proof
discharges **1468/1468 obligations**, including all 11 functional contracts;
zero justified checks, assumptions, or disabled-SPARK sections.

The previous 20 unresolved checks were removed structurally: AST string spans
use bounded inclusive slice endpoints; the secondary region separates its
aggregate capacity from each value's maximum length; concatenation uses checked
bulk copies with explicit success flow; decimal formatting is an isolated
bounded reverse traversal with a decimal-digit subtype. The opening string
delimiter is consumed by the parser branch that established its presence.

Tests cover empty/max-length strings, rejected oversized concatenations,
signed integer extremes, exact/forged/missing grants, host failures and wrong
types, skipped branches, fuel, and native clock formatting. The separate
HTTP/CBOR boundary discharges 145/145 checks, and the periodic lifecycle core
21/21 (including its stop-state postcondition). These are scoped proofs, not
proof of the native adapter, kernel IPC, upstream floating-point CBOR routines,
or wall-clock deadlines; the latter are tracked by SEC-018.

### SEC-R003 — Deferred reply authority could be selected by PID

Resolution: NetStack deferred requests now retain a typed reply-capability slot
and complete through that exact one-use authority. Saving a reply capability is
required for admission to the pending table; failure no longer creates an
uncompletable pending operation. Immediate NetStack replies use the current
kernel-minted reply slot rather than the PID compatibility call.

The capability-table ADT now rejects moves onto occupied slots and proves that a
successful save moves the exact capability, clears the source, and changes no
other slot. Consumption returns the exact old capability and clears only its
slot. A ghost two-attempt harness proves that the same slot cannot be consumed
twice. Ordinary derive and mint operations reject `CAP_REPLY`, and the syscall
mint gate uses the same predicate. Focused GNATprove level-1 runs discharge all
11 functional contracts and all runtime checks with CVC5 invoked, with no
justifications, unproved checks, or `pragma Assume` statements.

The headless asynchronous IPC regression includes occupied-slot preservation:
attempting to save a second request over a held reply capability fails, while
both the held authority and the new current authority remain independently
usable exactly once. Remaining PID-spelled reply callers are tracked in
`legacy-ipc-audit.md`; therefore complete end-to-end IPC mediation is not yet
claimed.

### SEC-R002 — Lazy FP/SIMD switching exposed stale register state

Resolution: replaced lazy `CR0.TS`/`#NM` switching with eager, initialized
FP/SIMD state transitions and added a cross-process regression test.

The former first-use path allocated a save area only after a process executed
an FP/SIMD instruction. When a new process triggered `#NM`, the kernel saved
the previous owner's state but did not restore a clean state for the new
process. Because `FXSAVE` does not clear the hardware registers, the new
process could directly observe stale x87/SSE state. Independently, retaining
another process's registers behind `CR0.TS` is the design pattern vulnerable to
speculative LazyFP disclosure (CVE-2018-3665).

Every user process now receives a valid architectural reset-state `FXSAVE64`
image at creation. Scheduler and direct-IPC context switches save the departing
user state and restore the arriving user state before it executes; the kernel
does not use FP/SIMD. `#NM` is fail-closed because it indicates that the eager
transition invariant was violated. The headless IPC test places distinct
sentinels in XMM0 to verify clean first entry and preservation over scheduler
and direct-switch IPC paths.

AVX remains disabled. Enabling it later requires an explicit `XSAVE`/`XRSTOR`
design sized from CPUID, with the enabled XCR0 feature set treated as part of
the process-state ABI; it must not be enabled while only the 512-byte FXSAVE
area is preserved.

### SEC-R001 — Zero-count manifest sections underflowed in procmgr

Resolution: fixed and regression-tested during CCL guest integration.

Zero-count identity, stream, or capability sections could evaluate `count - 1`
and scan unrelated ELF bytes. Each loop is now guarded by a nonzero count. This
was a concrete parser-safety defect and reinforces SEC-002's priority.
