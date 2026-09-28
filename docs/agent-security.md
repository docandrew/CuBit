# CuBit AI Agent Security

Status: design proposal

CuBit treats an AI agent as untrusted computation that may propose useful
actions. Intelligence, model quality, a natural-language instruction, and a
human-associated session are not authority. An agent can affect the system only
through the typed permissions, handles, and reply capabilities explicitly
provided for its current task.

> A model may decide what to ask for. It does not decide what it is allowed to
> do.

This document applies the authority architecture in
[`security-model.md`](security-model.md) to autonomous and semi-autonomous
agents. CCL's typed effects, bounded execution, and host imports are described
in [`control-language.md`](control-language.md). Signed packages and immutable
realizations are described in [`ccl-packages.md`](ccl-packages.md).

## Objectives

CuBit agent execution should:

* treat model output as untrusted regardless of model provenance or quality;
* prevent agents from inheriting ambient user, operator, host, or orchestrator
  authority;
* expose only typed tools backed by explicit CuBit handles;
* derive a fresh, bounded authority environment for each mission;
* require separately authorized applications to approve consequential actions;
* confine direct and indirect prompt injection to the effects already granted;
* prevent sub-agents, tools, and persistent memory from amplifying authority;
* control dangerous combinations of otherwise legitimate authority;
* bound CPU, memory, time, IPC, storage, model use, and external cost;
* preserve the causal provenance of inputs, proposals, approvals, and effects;
  and
* make agent policy and enforcement suitable for implementation and proof in
  SPARK.

CuBit does not claim to prove that a model is truthful, aligned, unbiased,
competent, or immune to adversarial inputs. It limits what incorrect or
malicious decisions can cause.

## Threat model

An agent may be incorrect, hallucinating, malicious, compromised, or influenced
by direct instructions, retrieved documents, webpages, email, tool results,
other agents, poisoned memory, or manipulated model artifacts. It may attempt
to:

* invoke tools outside the current task;
* enumerate services, devices, data, secrets, or authority;
* confuse untrusted data with operator instructions;
* forge approval or claim that a user authorized an action;
* disclose sensitive inputs through a legitimate output tool;
* retain handles or data beyond the mission that introduced them;
* delegate broader authority to a sub-agent;
* poison persistent memory for use by future missions;
* alter its tools, configuration, model, or policy;
* exploit a parser, tool adapter, inference runtime, or native model library;
* consume unbounded local or remote resources; or
* obscure the causal chain between an input and a resulting action.

Model safety mechanisms and prompt-level guardrails are defense in depth. They
are not enforcement boundaries.

## Identity is not authority

Agent explanations distinguish several identities:

* the signed agent-host package and exact executable digest;
* the model artifact, model version, or authenticated remote model provider;
* the orchestrator and CCL program constructing the mission;
* each tool implementation and typed interface descriptor;
* the principal or process that requested the mission;
* the approval application and policy that authorized an action; and
* each local or remote node involved in execution.

A valid signature authenticates the claimed artifact and signer according to
local trust policy. It does not make model output trusted and does not grant the
artifact runtime authority. The signed manifest remains a maximum request
profile that is intersected with installation, launch, mission, and action
policy.

## Authority layers

An agent is constrained by progressively narrower layers:

```text
signed package manifest           maximum operations the host may request
          |
installation and launch policy    whether the host may execute and its ceiling
          |
mission grant                     resources and tools for this task
          |
typed tool handle                 one service, object, scope, and lifetime
          |
action approval                   optional use-once authority for one effect
          |
reply capability                  exactly one completion of a pending request
```

No lower layer may exceed an upper layer. A tool name, model request, natural
language statement, human identity, or user-interface event cannot substitute
for a missing handle.

### Manifest eligibility

An agent package distinguishes authority required to launch from authority it
may request later:

```text
needs       required before the agent host can launch
may-request eligible for a later policy or user-mediated request
accepts     may receive a resource selected by another authorized component
provides    typed interfaces implemented by the package
```

`needs` never means automatically granted. If local policy denies required
authority, launch fails without leaving a partially authorized process.

### Mission grant

The unit of agent authority is a **mission**, not a user account or a persistent
agent identity. Each mission receives a new environment containing only the
handles, budgets, output sinks, and approval rules required for that task.

```text
Mission {
  identity,
  goal_digest,
  requesting_principal,
  agent_and_model_identity,
  input_handles,
  tool_handles,
  permitted_output_sinks,
  authority_composition_rules,
  approval_rules,
  resource_and_cost_budgets,
  deadline,
  retention_policy,
  provenance_policy
}
```

The goal text is evidence explaining why the mission exists; it is not an
executable policy and cannot widen authority. Mission identifiers and process
generations prevent stale grants from being reused after restart.

Long-lived assistants must not accumulate the union of every prior mission's
authority. Persistent presentation and conversation state may outlive an
individual mission, but task handles and sensitive working data expire or are
returned according to their declared lifecycle.

## Typed tools, not ambient integration

An agent tool is a typed CuBit interface supplied as a handle. Native agents do
not receive a shell, global filesystem, inherited environment, generic network
client, or universal plugin registry merely because those mechanisms are
convenient integration points elsewhere.

Tool declarations include:

* argument and result types with explicit bounds;
* read, write, control, delegation, and inspection effects;
* copied, moved, borrowed-ro, and borrowed-rw parameter modes;
* must-handle completion verbs;
* cancellability and behavior after cancellation is requested;
* rate, size, destination, and resource constraints;
* required permission and handle types;
* data-origin and information-flow behavior; and
* interface identity, version, and immutable descriptor digest.

Agent-visible tools are resolved from the handles in the mission environment.
The agent cannot enumerate the global service catalog or inspect other
processes unless it separately receives the relevant discovery or inspection
authority.

Open-ended adapters such as `execute-command`, `open-path`, `http-request-any`,
or `call-tool-by-name` defeat useful static review. Compatibility boundaries
must translate them into narrower interfaces or isolate them as explicitly
high-risk services.

## Prompt injection and input provenance

Text, images, audio, retrieved records, webpages, email, tool output, model
output, and messages from peer agents are data. Their contents do not create
authority even when a model interprets them as instructions.

CuBit should distinguish at least:

* operator or orchestrator instructions;
* authenticated policy and approval decisions;
* internal trusted records;
* external or untrusted content;
* model-generated proposals;
* tool-generated observations; and
* persisted memory with its original sources and age.

This provenance does not require the OS to understand natural-language
semantics. It lets tool and policy services apply structural rules such as:

```text
external content influenced this action       require review
confidential data may reach an external sink  require declassification
stale or untrusted memory influenced control  deny or require review
model output supplied an executable artifact  build in a confined service
```

An untrusted webpage may convince a model to request an upload. Without an
authorized output handle the request fails. If the mission legitimately has an
output handle, the system must also consider authority composition and
information flow.

## Dangerous authority composition

Capability confinement alone cannot prevent disclosure when one process
legitimately possesses both sensitive-read authority and an arbitrary external
write channel. Similar conflicts include approval plus execution, audit write
plus audit deletion, and production deployment plus policy modification.

CuBit controls these combinations using:

* launch and mission policy that rejects forbidden combinations;
* destination-scoped handles rather than generic network or messaging access;
* separate reader, planner, and executor processes;
* opaque secret-use handles that do not reveal secret values;
* provenance labels on values crossing typed interfaces;
* explicit declassification operations authorized by separate handles; and
* approval close to the point of irreversible effect.

A high-assurance agent can be decomposed as:

```text
public retriever       public network read; no confidential inputs or effects
        |
        | bounded, source-labelled observations
        v
planner                proposes typed actions; no effectful service handles
        |
        | checked action plan
        v
executor               narrow internal handles; no arbitrary retrieval
        |
        | proposal requiring policy or human approval
        v
target service         performs the exact approved effect
```

Compartment boundaries are ordinary CuBit process and IPC boundaries, not
promises encoded only in an agent framework.

## Propose, approve, and commit

Consequential operations should separate proposal from commitment:

```text
agent constructs typed proposal
          |
policy validates mission, scope, provenance, and current state
          |
authorized approval application obtains user or automated decision
          |
use-once approval token binds subject, operation, arguments, and expiration
          |
owning service consumes token and performs the exact action
```

Examples include sending a message, merging code, deploying to production,
quarantining a host, modifying a ledger, spending money, releasing confidential
data, or changing persistent agent memory.

The application presenting an approval need not possess the underlying
resource. It holds a narrow authority such as `Approve<ProductionDeployment>`
or `Approve<ExternalMessage>`. The target service owns the resource and creates
the resulting handle or effect only after validating the approval token.

The requesting agent cannot choose its approver, approve itself, forge a user
gesture, or nominate a malicious prompt window. A fake approval window may draw
pixels and receive input but lacks the pending reply capability and typed
approval authority required to complete the transaction.

Approval tokens are use-once, non-amplifying, and bound to:

* the exact requesting process and generation;
* mission and request identifiers;
* typed operation and canonical arguments or proposal digest;
* selected provider and resource scope;
* approving application and policy rule;
* permitted result and delegation behavior; and
* deadline or expiration condition.

Approval is not a substitute for validation. The target service remains
responsible for schema, bounds, generation, state, and authority checks.

## Sub-agents and delegation

Creating a sub-agent never copies the parent's environment. The parent must
construct a child mission and explicitly delegate selected handles. Delegation
must preserve type, scope, ownership, lifetime, provenance, and any prohibition
on further delegation.

```text
child authority ⊆ delegated parent authority ⊆ parent mission authority
```

Move-only and must-handle values follow their normal ownership rules. Borrowed
authority cannot be retained by a child after the borrow ends. A parent remains
responsible for every outstanding child mission and must join, cancel, return,
or otherwise resolve it according to the task type.

Peer-agent messages are untrusted input unless authenticated and explicitly
typed. Mutual TLS authenticates a remote CuBit node or service; it does not
transfer local authority or make remote model output trustworthy.

## Memory and retrieval

Agent memory is a service-managed resource, not an ambient mutable transcript.
Memory handles specify:

* readable and writable collections;
* data classification and originating mission;
* source identities and source digests where available;
* creation, last-validation, and expiration information;
* who may retrieve, append, replace, or delete entries;
* whether an entry may influence control decisions; and
* retention, redaction, and audit policy.

Reading memory does not imply authority to modify it. Writing temporary working
state does not imply authority to publish persistent knowledge. Content from
external retrieval retains its untrusted provenance when embedded or
summarized; a model-generated summary does not launder its source.

High-impact persistent-memory changes may use the same propose-and-commit model
as other consequential effects. Retrieval services are separately confined so
that poisoned documents cannot directly invoke agent tools.

## Local and remote inference

Inference is a typed service. A local model service receives only explicitly
provided input buffers and resource quotas. It does not inherit the calling
agent's tools, mission handles, secrets, storage, or network access.

A remote inference service additionally declares:

* the authenticated provider and remote node or endpoint;
* which data classifications may be transmitted;
* encryption and peer-authentication requirements;
* retention and training-use policy claims;
* maximum request and response sizes;
* rate, monetary, and time budgets; and
* failure, retry, cancellation, and duplicate-request behavior.

Sensitive data requires an explicit export or declassification path before it
is placed in a remote prompt. Secrets should normally remain behind purpose-
specific services and be used without revealing their values to the agent or
model.

Model responses return as bounded, untrusted values. Tool adapters parse and
validate them into typed proposals; no raw model response is executed as native
code or treated as an approval.

## Resource, availability, and cost control

Every mission has explicit budgets for resources including:

* CPU time and scheduling class;
* memory and shared-buffer capacity;
* wall or monotonic elapsed time;
* CCL or tool-call fuel;
* concurrent tasks and sub-agents;
* IPC messages and outstanding replies;
* storage and persistent-memory growth;
* inference requests, tokens, and retries;
* network bytes and destinations; and
* metered external cost.

Exhaustion produces a typed outcome and begins deterministic cleanup. It must
not silently increase a quota, switch providers, discard must-handle values, or
claim cancellation of an operation that cannot be cancelled. Rate limits also
protect target services from a compromised or looping agent.

Agent work must not receive latency or real-time scheduling merely because it
is interactive. Resource policy may distinguish user-visible planning from
background indexing without allowing either to starve audio, input, security,
or critical services.

## Explainable agent action

Every proposed, approved, denied, or completed agent action participates in
CuBit's WHAT–WHO–WHEN–WHERE–WHY model.

### What

The mission, typed tool operation, canonical arguments, affected resource,
result, ownership transition, information-flow decision, and resource cost.

### Who

The requesting principal, signed agent host, exact model or remote provider,
orchestrator, tool implementation, policy service, approval application, target
service, and remote peer identities.

### When

When relevant inputs were obtained, when the proposal was generated, when
policy evaluated it, when approval was requested and completed, when the effect
occurred, and when resulting authority or data expires.

### Where

The source of every influential input, the process and node performing each
stage, the target resource, and every permitted output destination.

### Why

The mission goal, originating instruction, influential retrieved data and tool
results, manifest request, delegation chain, policy rule, approval decision,
exception, and disposition that allowed or denied the effect.

The system preserves a bounded causal chain:

```text
request -> mission -> observations -> proposal -> policy -> approval -> effect
```

Natural-language rationale supplied by the model is untrusted supporting data,
not the authoritative explanation. The authoritative explanation is derived
from actual handles, messages, policy decisions, and service state.

Visibility into agent missions, tools, memory, and authority is itself gated.
The CCL Workbench and future security-posture application receive explicit
inspection handles;
ordinary agents do not gain system reconnaissance merely because the evidence
exists.

## CCL integration

CCL is the preferred language for mission construction, typed tool adapters,
bounded policy expressions, monitors, and small agent workflows. A model may
produce CCL source or values, but generated code passes through the normal
parser, type checker, effect checker, bytecode compiler, verifier, and explicit
session-authority construction before execution.

An illustrative workflow may eventually resemble:

```ccl
mission reconcile-invoice
  using agent accounting-assistant
  read invoice selected-invoice
  read purchase-order matching-order
  permit draft accounting-adjustment
  require approval ledger.modify from finance-approver
  deny external-network
  budget model-requests 12
  expires after 10 minutes
```

The equivalent Lisp form elaborates to the same typed mission value. Evaluating
the description produces a proposed mission; an authorized mission service
performs resolution and launch. CCL evaluation itself cannot mint handles.

CCL effect types should make agent workflows reviewable before launch. A script
that can only read telemetry and render a bounded dashboard is observably
different from one that can restart services. Dynamic approval is represented
as a typed host operation and use-once value, not as an unchecked callback.

## Enterprise policy examples

CuBit should be able to express policies such as:

* a coding agent may read one repository, create a branch, and run a confined
  build, but cannot read developer secrets, push, merge, or deploy;
* a support agent may read one customer's tickets and draft a response, but a
  separately authorized application must approve sending it;
* a security agent may read selected telemetry and propose quarantine, but
  cannot modify audit history or execute quarantine without SOC approval;
* a finance agent may reconcile records and prepare a transaction, but cannot
  commit it or communicate customer data externally;
* a local model may process confidential data while a remote model endpoint is
  limited to public material; and
* unsigned experimental agents may run only in an authorityless, bounded lab
  profile, while production agents require approved signatures and provenance.

These are process, handle, and service policies rather than conventions tied to
a Unix user, group, shell, container, or shared cloud credential.

## Verification targets

The initial SPARK verification boundary should cover:

* mission parsing, bounds, and deterministic validation;
* intersection of manifest, launch, mission, and action authority;
* non-amplifying handle derivation and sub-agent delegation;
* approval-token uniqueness, subject binding, expiration, and use-once
  consumption;
* ownership completion across asynchronous tool calls;
* forbidden authority-combination checks;
* provenance propagation through typed values and tool calls;
* resource accounting without overflow or silent quota expansion;
* bounded audit construction and redaction; and
* cleanup after completion, denial, timeout, crash, or cancellation.

Cryptographic, inference-runtime, model, hardware, compiler, and external-
service assumptions must be stated rather than hidden behind proof claims.

## Implementation path

The first slice of step 1 is now implemented in the CCL core. Interface
discovery is an explicit bounded catalog view, compilation produces unresolved
descriptor-pinned linkage, and a separate granted-binding view performs
all-or-nothing admission. The initial clock adapter uses this generic path.
Initial CCLB v3 linkage serialization is implemented. Descriptor hash
validation at publication and mapping to kernel-enforced opaque handles remain
before the step is complete.

1. Define immutable typed service-interface descriptors and replace CCL's
   semantic dependence on static driver identifiers.
2. Define bounded `Mission`, `Tool_Grant`, `Approval_Rule`, `Data_Origin`, and
   `Resource_Budget` ADTs in SPARK.
3. Add a mission service that intersects signed manifest eligibility with
   launch and mission policy and constructs a fresh process environment.
4. Expose one harmless typed tool, such as monotonic clock observation, through
   the generic CCL import mechanism.
5. Add a proposal/approval/commit path using an existing trusted desktop
   application and a use-once reply capability.
6. Add Workbench inspection of mission authority, tools, budgets, outstanding
   obligations, approvals, and provenance.
7. Split a demonstration agent into retriever, planner, and executor processes
   and show that prompt injection cannot cross the missing authority edges.
8. Add persistent-memory and remote-inference services with explicit data-
   export policy.
9. Prove the mission and approval state machines, then harden parsers and
   adapters at each remaining native boundary.

## Open questions

* Which provenance labels and composition rules provide useful protection
  without attempting an impractical universal information-flow system?
* Which operations always require interactive approval, and which may be
  approved by persistent or automated policy?
* How should an operator safely authorize an evolving set of arguments without
  creating a reusable broad grant?
* Which model and remote-provider claims are authenticated facts, and which are
  declarations that must remain visibly unverified?
* How are mission histories summarized without losing the causal inputs that
  matter for an investigation?
* Which persistent-memory transformations require declassification or review?
* How should non-cancellable external operations be represented in agent plans
  and recovery UX?
* What minimum protected UI is required for trustworthy agent approvals while
  keeping approval authority outside the general desktop compositor?
* Which signed execution profiles permit unsigned source or CCL bytecode inside
  authorityless sandboxes?

## Related external work

The design should track, but not depend upon, external agent terminology and
frameworks:

* [MITRE ATLAS](https://atlas.mitre.org/) catalogs prompt injection, tool and
  context poisoning, credential access, exfiltration, and agent-enabled impact.
* [OWASP Excessive Agency](https://genai.owasp.org/llmrisk/llm062025-excessive-agency/)
  identifies excessive functionality, permissions, and autonomy as primary
  causes of agent harm.
* The UK NCSC's
  [agentic AI guidance](https://www.ncsc.gov.uk/blogs/thinking-carefully-before-adopting-agentic-ai)
  emphasizes conventional access control, monitoring, and avoiding unrestricted
  access to sensitive systems.

CuBit's contribution is to make those controls native, typed, explainable, and
enforced below the probabilistic model and agent framework.
