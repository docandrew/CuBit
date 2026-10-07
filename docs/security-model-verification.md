# Security model: attacks, formal obligations and implementation evidence

Status: design and verification plan. No Lean model or model-soundness theorem
is implemented by this document. Existing SPARK proofs establish only their
published component contracts; they do not establish this whole model.

Use the [four-term vocabulary](security-vocabulary.md), the normative
[security model](security-model.md), and its existing
[transition correspondence](security-model.md#formal-transition-correspondence).
Do not create a second policy architecture to make the proof easier.

## Threat boundaries

Assume an attacker fully controls an ordinary application process, including
its native code and all message contents. The attacker can use all authority
actually held by that process. Memory-safe CCL is not required of an attacker.
An AI agent following a hostile instruction is another instance of this case.

Distinguish application compromise from compromise of a filesystem, policy
issuer, driver or kernel. Each guarantee must identify the components it trusts.
A privileged component's ability to damage its own scope is not eliminated by
checking clients at its public endpoint. Limit that scope where practical.

| Attack / hostile trace | Required boundary or explicit limit |
|---|---|
| RCE in a browser/parser uses arbitrary native IPC | Complete mediation and containment to the compromised component's actual authority, not its application's intended behavior. |
| Forged handle, copied slot number, guessed authority tag, replayed approval | Authenticated recipient and object/context lifetime checks; identifiers and metadata are not independent credentials. |
| Broker reads a secret on behalf of an unauthorized caller | Authorize the beneficiary and exact effect; the broker's ability to read is not the caller's approval. |
| Rename/replacement, PID reuse, stale reply or service restart during approval | Bind current instances and object identities; define linearization and restart epochs without wraparound reuse. |
| Approval delayed until after revocation; duplicated grant message | No resurrection after the defined revocation-complete boundary; replay cannot add authority or duplicate use-once resources. |
| Fake prompt or authentic prompt manipulated by malware | Only authorized approvers can approve the bound request; trusted UI identifies recipient, scope and context. Human deception remains possible. |
| Malicious signed update or false service advertisement | Provenance/schema are evidence, not permission; validate provider binding and executable activation independently. |
| Authorized insider changes policy or deletes evidence | Separate proposal/approval/application/audit-management authority as policy requires; do not let the same compromised authority impersonate independent approvers. |
| Ransomware or exfiltration through legitimate file/network grants | Narrow scopes, deliberate stream wiring and independently protected recovery. Valid read plus valid egress can leak data without an authority violation. |
| Writable content becomes startup code, privileged CCL or policy | Content, security-state changes and executable activation are separate protected effects. |
| Flood of valid requests, retained memory loans, repeated service failure | Admission/resource bounds, backpressure and explicit cleanup/quarantine; no permissive policy fallback. Latency/liveness requires separate scheduling assumptions. |
| Raw disk write, malicious DMA, offline tampering | Explicit storage/driver trust boundary; volume separation alone does not defeat raw access. IOMMU, authenticated storage and boot protection are distinct follow-ons. |

Typed payloads rule out some mismatches, not malicious intent or information
flow. This first model is authority safety, not a confidentiality noninterference
proof, side-channel defense, malware classifier or universal availability proof.

## Model state and transitions

Use explicit state for:

* live process/service instances, objects and non-reusing lifetime identifiers;
* contexts and their lifetimes (profile names alone never select authority);
* admitted executable request ceilings and authenticated approval decisions;
* active authority assignments, including use versus issuance/delegation scope;
* handles/replies and outstanding operations or borrows;
* grant provenance and bounded authoritative inspection metadata.

An assignment describes holder, protected object/scope, allowed operations,
context, lifetime, and source grant/root. Model these as typed fields, not one
untyped bitmask shared across unrelated resource kinds. A process authorized to
issue a particular authority need not itself possess permission to use it.

Scope containment needs a resource-specific relation: exact object identity,
directory scope, Config namespace, network prefix/port range, etc. Do not treat
string prefix matching as a universal scope proof. Prove the relevant relation
and its transitivity for each admitted scope type.

Start from an explicit, finite bootstrap authority inventory. The assumptions
must name broad root powers; hiding them in an unrestricted `Grant` transition
would make non-amplification meaningless. Current procmgr receives wildcard
`CAP_CSPACE` issuance authority from devmgr; reducing that power is SEC-020,
not a property already provided by a scoped-policy model.

Define checked transitions for approve, issue, delegate/restrict, open, invoke,
reply, close, revoke, invalidate and restart. Refine them to existing kernel
verbs and service messages rather than requiring identical names or one syscall
per transition. Model denial, partial completion and uncertain outcomes too.

For cross-service issuance, approvals bind recipient instance, object/scope,
operations, context, decision identity and relevant generations. Define the
activation and revocation completion points before selecting the transport.
No global IPC transaction or distributed lock is assumed. Local protected
objects serialize local state only; never hold their locks across IPC.

Revocation specifies which new work is rejected and how already accepted work
drains. Acknowledging a request is distinct from confirming withdrawal. Repeated
issuance may return the same result, but must not create extra authority;
uncertain non-idempotent resource operations must not be blindly replayed.

## Initial proof obligations

| Property | Required statement |
|---|---|
| Complete mediation | Every modeled protected effect has a matching live authority for its authenticated actor/beneficiary at the defined acceptance point. Enumerate effects; do not model only successful authorized calls. |
| Bounded issuance | New grants satisfy the recipient ceiling, applicable approvals and issuer's explicit issuance/delegation scope. Ordinary derivation cannot widen scope, operations, context or lifetime. |
| Binding | Untrusted names, handles, tags, signatures and copied messages cannot substitute another recipient, object or context. |
| Lifetime safety | Closed/invalidated identities cannot become usable again through reuse/restart. Revocation-complete prevents the effects its contract excludes; it does not undo completed work. |
| One-use obligations | Replies and other must-handle resources cannot be duplicated or consumed twice; moving preserves their identity and obligation. |
| Rejection preservation | Denied requests do not add authority or perform the rejected protected effect. Bounded audit/accounting and explicitly authorized cleanup are allowed changes. |
| Content/policy separation | Ordinary content updates preserve authority and activation state. The storage refinement additionally preserves unrelated blocks and prevents stale-data exposure. |
| Explainable authority | Each active assignment has a valid root/decision/delegation explanation; permitted inspection reports enforcement state with explicit completeness and revision. |

Prove initialization and preservation across every transition, then obtain an
invariant over reachable executions. Also demonstrate ordinary useful reachable
executions: a model that denies everything can satisfy safety vacuously.

Do not define "valid grant" as "already satisfies the conclusion" and call that
an issuance proof. The decision implementation must establish the conclusion
from checked inputs. Trusted facts such as authenticated caller identity need
named assumptions and a separate implementation correspondence obligation.

### Proof sequence

1. Specify the bootstrap inventory and first exact-object grant transition;
   review it against `CAP_CSPACE` minting and a service-owned handle path.
2. Prove one non-amplification property in Lean (or another agreed model prover),
   with executable allowed/denied examples and an explicit assumption list.
3. Add lifetime/revocation, then delayed/duplicated messages and restart. Try
   hostile interleavings, not merely sequential happy paths.
4. Refine a small Config decision/handle path to these transitions in SPARK.
   Prove functional contracts as well as absence of runtime errors; prefer
   types and correct state structure over assertion/guard accumulation.
5. Test the native caller/authentication/IPC/storage adapters and failure paths
   that remain outside the proof. Expand to filesystem, streams and networking
   only with their actual scope/lifetime semantics.

No theorem may quietly rely on `sorry`, an admitted conclusion, or an unreported
axiom. Publish the theorem's assumptions and proof tool output. A Lean theorem
about the abstract model is not proof that kernel assembly, crypto, DMA or the
service implementations refine it. A successful regression test is not a proof.

## Visibility is an enforcement interface

The authorized inspector needs authoritative, bounded snapshots, not just logs
or manifests. For each visible process/service instance show requested, approved,
active and observed-used authority separately, including bootstrap exceptions.
Bind explanations to the enforcement records; unavailable provenance is a gap,
not a fabricated approval. Existing grants can carry compact provenance IDs so
routine I/O need not call policy or format log messages.

Inspection is itself authorized. Read access to authority metadata does not
grant policy editing or secret contents. A restricted view must state its scope;
missing providers, lost events and stale snapshots must appear as unknown or
incomplete, not "no permissions" or a green healthy indicator. Across services,
publish revisions/coverage or retry inconsistent snapshots; do not claim one
atomic global view without a protocol that establishes it.

Full-system visibility also requires an authorized live-instance inventory so
background services do not disappear merely because they have no windows.
Describing authority and admitted provenance cannot establish whether that
process's current behavior is benign. The UI must distinguish observation from
that stronger, generally unavailable conclusion.

## First Config evidence checklist

Before claiming the Config boundary is complete:

- [ ] Trace authenticated process instances into namespace/sub-key and handle
      checks for every supported read/write/create/delete/list/inspect path.
      Record absent operations rather than inventing coverage.
- [ ] Exercise cross-caller handles, guessed tags, boundary keys and contexts,
      scope replacement/revocation, stale handles and service restart.
- [ ] Check rejected operations preserve protected values and authority;
      revision conflicts cannot silently become successful overwrites.
- [ ] Check malformed typed objects and mutable shared request buffers cannot
      change the decision after validation; snapshot or enforce immutability.
- [ ] Test delayed replies, full queues, death during issuance/close and lost
      acknowledgments; preserve required quarantine instead of inventing success.
- [ ] Demonstrate native Workbench write/read/reboot persistence and independent
      storage validation (the REPL now uses the bytecode path; the interpreter
      was removed 2026-10-05).
- [ ] Publish proof boundaries, remaining privileged issuance paths and native
      test results. No whole-system soundness claim from a pure policy predicate.

This checklist defines the next audit, not results of a new run. Prior native
Config evidence remains in [the Workbench tests](../tests/config-workbench/README.md)
and [the integration checklist](config-worker-integration-checklist.md).
