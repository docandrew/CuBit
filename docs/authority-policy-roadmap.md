# Authority policy: first executable slice

Every operation must trace back to explicitly granted authority. Policy decides
how authority is distributed; it never supplies an enforcement bypass. See the
[security model](security-model.md) for the overall trust boundaries.

Use the [shared vocabulary](security-vocabulary.md): **policy governs grants;
handles exercise authority**. Requests and approvals are not active grants.
The [attack and verification plan](security-model-verification.md) specifies
the first proof obligations and Config audit, with implementation gaps explicit.
Runtime approval is a target: the policy manager decides, resource owners issue
and enforce scoped handles, and the kernel enforces its protected references.
Procmgr remains responsible for launch, not every file open or routine I/O.
Ordinary use of a live handle must not require a policy IPC round trip; dynamic
decisions need explicit invalidation/lifetime semantics, not unchecked caching.

## Implemented boundary

`CuBit.Authority_Policy` is a pure SPARK decision function over four already
scope-specific Boolean facts: requested, installation-approved, session-approved,
and issuer-allowed. It returns an explicit approval or a deterministic denial
reason. Its contract states approval if and only if all four facts hold.
It does not authenticate those facts, intersect filesystem paths/network ranges,
evaluate CCL, or install capabilities.

`procmgr` uses it for master-audio and log-observer startup gates. Bootstrap approval
still means the trusted startup path approves the declared request; ordinary
`OP_SPAWN` callers cannot set that flag. Session and issuer inputs are explicitly
true for this existing bootstrap path, not evidence of a completed session-policy
or constrained-issuer implementation. Other authority paths remain unchanged.

The live log broker matches caller, issued observer tag, and subscription
handle. Procmgr issues distinct tags without wrapping within its lifetime.
A trusted monotonic clock drives a 30-second idle lease for bounded
reclamation. This is not immediate revocation and does not by itself solve issuer
restart. Reader authority covers the whole diagnostic feed, not selected topics.

The legacy query/clear protocol and disabled proactive-subscription experiment
have been removed. The native service validates typed grant-backed publications;
shell uses the authorized reader and clock emits its startup diagnostic. Native
QEMU tests cover forged tags, invalid grants/records, overflow and ordinary-launch
observer denial. This does not yet migrate the kernel or all services, and it is
not a durable audit log. [Tests and limitations](../tests/log-fanout/README.md).

## Roadmap

1. Broaden native lifetime tests and design restart epochs/invalidation. A shared
   bootstrap publication budget now prevents copies multiplying admission credit;
   per-deployment assignments/fairness and transport abuse limits remain work.
   Then migrate more services to the shared logging client.
2. Introduce typed resource scopes and approved-installation ceilings; required
   requests fail launch when unavailable, optional requests can be withheld.
3. Bind decisions to admitted executable identity and process instances. Add
   signatures, durable policy, update/rollback and air-gapped provisioning rules.
4. Add controlled runtime approval with explicitly authorized approvers, expiry
   and revocation semantics. A graphical prompt is not authority by itself.
5. Surface actual grants, handles, denials and policy changes in System Inspector:
   what, who, when, where, and why. Observation itself remains authorized.
6. Apply the same machinery to remote CCL sessions and declarative deployments;
   no remote superuser or identity-based bypass.

CCL should describe typed policy data compiled into a bounded representation.
Authorization evaluation must not invoke arbitrary callbacks or network/service
discovery. Normal users interact with curated defaults, installation choices,
trusted resource pickers and understandable approval dialogs, not source code.

Lean or another model prover could formalize issuance/delegation/revocation state
transitions. SPARK proofs and hostile-client regression tests must separately
cover the implementation and its binding to kernel-enforced authority. A proved
Boolean intersection is only the first small property, not model soundness.
