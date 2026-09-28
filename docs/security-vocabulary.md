# CuBit security vocabulary

Status: preferred design and presentation vocabulary; implementation names are
mapped below, not renamed by this document. The
[security model](security-model.md) defines the required behavior.

## The short explanation

Every app gets keys that open specific doors. Apps cannot make their own keys.
Getting another key requires approval from something allowed to give it. An
authorized inspector can show which keys an app has, where they came from,
and what they open.

The metaphor is for teaching, not another API: do not introduce `Key` as a
synonym for handles or confuse it with a cryptographic key. Software holding
keys can abuse the doors they legitimately open; confinement is not proof of
good intent.

## Four core terms

| Term | Meaning | Example |
|---|---|---|
| **Authority** | What protected operations are allowed, on which objects, under which limits and lifetime conditions. | Read one selected document, but not modify it or grant access to another app. |
| **Handle** | An opaque reference used to exercise specific authority. Its validity depends on trusted state, ownership and lifetime checks, not knowing a number. | The editor's open document handle. |
| **Grant** | An authorized assignment of authority to a recipient; also the record explaining that assignment. | A trusted picker arranges read access to the selected document for this editor instance. |
| **Policy** | Rules constraining which grants may be made and retained. Policy is not an alternative way to perform an operation without authority. | This installation may request selected documents; broader directory access needs approval. |

In one sentence: **policy governs grants; handles exercise authority**.

An approved request is not necessarily an installed grant or an opened handle.
A grant record explains enforcement state; merely possessing, copying or editing
its serialized description never confers authority. A handle is a capability
only when the implementation actually enforces the protected-reference contract.
Do not describe an unchecked numeric identifier as a secure handle.

The four terms do not imply four separate runtime objects, services, or IPC
round trips. A kernel capability entry can represent an active assignment;
a service can combine its handle and grant records. Keep trusted provenance
linked to the enforcement state without synchronous verbose logging or a policy
lookup on every read/write.

## Explain authority using ordinary words

Use **allowed operations**, **scope**, **holder**, **granted by**, **reason**,
**profile/context**, and **lifetime** as fields, not additional security tiers.
Use "permissions" only as informal language for allowed operations, never a
second mechanism beside authority. In particular, replace the old phrase
"initial permissions" with **authority requests** or **approved ceiling**, as
appropriate; a manifest declaration is neither a grant nor an active handle.

The usual UI should lead with questions:

* What can this app do?
* What did it request, what was approved, and what is active now?
* Who granted that, why, in which context, and until when?
* What changes if I withdraw this grant?

"Active" means currently usable authority, not proof it has been exercised.
"Observed use" needs separate evidence. A lifetime may be process-bound rather
than time-limited; do not display an invented expiration date. Pending
withdrawal, incomplete inspection and unavailable providers must be explicit.

## Precise terms below the surface

These distinctions remain useful in code, protocols and expert inspection.
They are not synonyms to be mechanically collapsed.

| Existing term / representation | Meaning and preferred presentation |
|---|---|
| **Capability**; `Capabilities.Capability`, `CAP_*` | The established technical term for an unforgeable authority-bearing reference. In CuBit's kernel it is the protected table entry. Present its allowed effects as **authority**; retain capability in technical explanations and current code. A service-issued handle can implement the same concept without being a kernel table entry. |
| **Endpoint**; `CAP_ENDPOINT` | A message destination. An endpoint handle authorizes specified communication; it does not confer every resource operation offered there. |
| **Slot**; `CapabilitySlot` | A process-local table position used to select a kernel capability. The same number in another process is unrelated; a slot number is not transferable authority. Replies also have thread-local/current and saved-slot rules. |
| **Authority tag**; `Message.authorityTag` | Kernel-stamped metadata identifying scope to the receiving service in its trusted protocol context. Not an independent credential, globally unique grant ID, or caller-selected permission mask. |
| **Rights**; `CapabilityRights`, service operation sets | Allowed operations on the referenced object. `RIGHT_GRANT` is interpreted for its object type; it is not universal permission to manufacture authority. |
| **Capability space**; `CAP_CSPACE` | Authority-table administration. Current policy minting under this authority is not ordinary attenuation; bounded issuance remains an implementation gap. |
| **Shared-memory grant**; `Memory_Grants.Grant_Reference` | A separate kernel-managed sharing/lifetime object, not a capability-table slot or a policy approval. Always qualify this term as **memory grant**. Access to transport memory is not file/Config authority. |
| **Memory loan** | A borrow/use relationship with explicit return obligations. Do not rename every memory grant to "loan": the existing creation, acquisition, return and retirement states are distinct. |
| **Reply capability**; `CAP_REPLY` | One-use authority to respond to a particular request. Present as **reply authority**, not an independent permission tier. |
| **ACL / scope rules**; `Config_Authority.Rule_Set` | Service-side rules used to admit or enforce scoped access. Transitional implementations must remain visible; this is not permission from object names or a parallel superuser model. |
| **Profile / context** | Saved configuration intent / where it applies. Names are not authority. `Config_Authority.Profile` currently means a subject's installed rule set, not a selectable user/dev/prod profile; that code name needs a scoped follow-up. |
| **Identity / signature / schema digest** | Evidence about a participant, provenance, or interface compatibility. None independently grants access. |

Source anchors: [kernel capabilities](../kernel/src/capabilities.ads),
[IPC ABI](../userspace/runtime/gnat/cubit-messages.ads),
[memory grants](../userspace/runtime/gnat/cubit-memory_grants.ads),
[filesystem handles](../userspace/runtime/gnat/cubit-filesystems.ads), and
[Config authority state](../userspace/services/config/config_authority.ads).

## Action words must preserve different semantics

* **Request** asks; **approve** records an authorized decision; **grant** makes
  the permitted authority available through validated enforcement state.
* **Delegate** gives a recipient authority within explicitly held delegation
  power; **restrict** (attenuate) narrows scope/operations. Issuance power need
  not imply permission to use every resource it may issue.
* **Open / close** manage a resource handle. Closing one handle does not
  necessarily withdraw the ability to open another or invalidate other handles.
* **Revoke** withdraws authority with a defined scope and completion boundary.
  Acceptance is not necessarily completion, and completed effects are not undone.
* **Drop** removes a local reference; **invalidate** ends an object/lifetime;
  **return** completes a borrow. These are not interchangeable with revoke.

Public UI may say "Remove access", but the resulting operation must explain
whether it prevents new grants, invalidates existing handles, or drains accepted
work. No reassuring label should conceal weaker enforcement semantics.

## Adoption without an ABI-wide rename

1. Use these terms in new design docs, UI labels and explanatory diagnostics.
2. Update the main security model's former three-tier explanation; requests,
   handles and replies describe different stages/forms of one authority system.
3. Audit APIs as their subsystems change. Prefer `Endpoint_Slot` for a local
   slot argument, `Memory_Grant` for transport sharing, and `Authority_Rules`
   for per-subject rules where these clarify the actual type. Preserve distinct
   types; do not replace everything with an untyped `Handle` integer.
4. Migrate code and callers/tests together when a rename is approved. Remove
   obsolete names rather than adding compatibility aliases for the undeployed
   ABI. Do not rename `CAP_*`, syscalls or package trees just to improve prose.
5. Use one mapping into the formal model; do not invent another vocabulary in
   Lean. Keep low-level transition names from the existing
   [correspondence table](security-model.md#formal-transition-correspondence)
   when they identify genuinely different operations.

This document changes terminology guidance, not enforcement, binary layouts,
proof coverage, service discovery or runtime delegation support.
