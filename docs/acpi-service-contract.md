# Userspace ACPI service: first contract and decoder milestone

2026-10-01. Design contract, not a deployed service ABI. Implementation now
includes a bounded namespace and partial AML evaluator, table snapshot/request
core, authenticated native IPC runner and a linked service executable. See
[the service implementation status](../userspace/services/acpi/README.md) for
current tests, proved properties and limits. Kernel launch/table-provider wiring,
live register access, full AML and CCL event integration remain incomplete.
The early decoder audit below records the initial milestone, not current coverage.

## Audit and scope

The kernel's `acpi.adb` admits RSDP/root/child tables and consumes MADT, MCFG,
FADT's DSDT pointer, and HPET. DSDT parsing checks the signature and stops at an
AML TODO; SSDTs are logged as unsupported. HPET is now decoded through shared
pure code, contrary to the earlier roadmap text. Table overlays remain trusted
raw-memory adapters; checksum admission does not establish snapshot immutability.
Do not move topology/timer/interrupt dependencies during this first milestone.
No current userspace AML implementation was found in the userspace source tree.

The first implementation decodes seven integer encodings and validates AML
package extents, using bounded loops, discriminated failure results and no
allocation. It is interpreter groundwork, not an AML interpreter. It cannot
interpret OperationRegion, execute a method, or discover a real device. It
requires the caller to supply a stable slice beginning at the operand and ending
within its enclosing package/table. Never scan past an unsupported opcode in
search of a recognizable byte: bytes inside another operand are not opcodes.
`Read_Package` includes the length encoding itself, excludes the opcode, and
must not be used for Field bit lengths (which have different extent semantics).
It accepts nonminimal multibyte encodings with zero reserved bits. Zero length
and lengths shorter than their own encoding fail. Integer width is selected
once from the DSDT revision (<2:32, >=2:64), including integers in SSDTs.

## Service and authority contract

The future startup supervisor grants a read-only table snapshot endpoint and
scoped IPC endpoints. ACPI has no CSPACE minting, arbitrary physical mapping,
port access, PCI access or DMA authority. A firmware address is never a grant.
Do not introduce a competing capability system or bootstrap grant path.

The boot handoff remains unimplemented. Latest discussion (2026-10-01) rules
out blanket read/write mappings of original ACPI memory. The working proposal
is one uniform mediated architecture: copied immutable table snapshots in
dedicated zero-padded pages, delivered through read-only bulk grants, plus
on-demand live reads and explicit writes through scoped hardware backends.
No automatic direct-map fast path is required. Original-page exposure/cache
classifiers remain optional groundwork, not startup prerequisites.

Copies of live state are return values of real reads, not writable shadow pages
to synchronize later. Backends must preserve read side effects, write semantics,
widths, ordering and required transaction synchronization. A scoped region
handle and validated operation cannot authorize arbitrary physical addresses.
AML interpretation and ACPI policy stay in the userspace service; existing
bus/device services can own the corresponding backend, with narrow kernel
primitives for privileged accesses and firmware/sleep coordination.

For table snapshots, revalidate the copy and retain exact lengths and provider
identities in a bounded manifest. Bulk grants avoid small IPC byte transfers.
Copying does not itself authorize reclamation: kernel consumers, service restart
and snapshot ownership must be accounted for first.

Table copying now has a kernel-side primitive, `ACPI.Copy_Table`. A trusted
caller supplies disjoint writable storage; the adapter checks the catalog index,
destination capacity and readable firmware backing. `Firmware_Tables.Copies`
copies the exact table bytes and validates the destination against the recorded
signature, length and revision plus checksum. Success preserves source bytes and
zeroes padding; failure zeroes the entire destination. Those copy properties are
SPARK-proved and regression-tested, including all single-byte corruptions of a
36-byte SDT. The raw physical overlay remains a trusted adapter. Native kernel
build and stack checks pass; this is not yet an export syscall or retained
snapshot owner, and it does not authorize reclaiming the source.

The typed `ACPI_Native_Blocks` consumer adapter now acquires a table-sized range
through the existing capability-based grant API and calls the proved bulk
request/endpoint core. It retains failed return references for cleanup retry.
It has hosted mock lifecycle tests and a native library compile, not live IPC
integration. Startup must supply consistent provider tag/endpoint authority and
a provider that keeps source bytes stable; acquisition alone does not freeze
the owner. Kernel snapshot production and the wire/receive loop remain absent.

The hardware-access interface must not become an arbitrary-memory primitive.
Backend sources now live in `userspace/services/acpi-backend`, separately from
the AML/table service. Its explicit native library source list excludes AML and
table-service units. `ACPI_Region_Protocol` is an independent wire type, avoiding
an indirect interpreter dependency. `ACPI_Backend_Endpoint` converts the real
CuBit IPC envelope and takes authority only from `Message.authorityTag`, leaving
reply authority metadata zero and pending tickets internal. A native mock
instantiation compiles against the real runtime ABI with no AML references in
its dependency files. This is a build boundary, not runtime process isolation:
separate process creation, receive loop and startup authority still do not exist.
The privileged backend owns immutable admitted region records: physical backing,
length, address space, read/write rights, permitted transaction widths and any
register-specific operation restrictions. Admission requires trusted platform
resource policy and ownership checks, not merely a well-formed GAS, firmware
checksum or AML OperationRegion declaration. The ACPI service cannot create,
rebase, enlarge or widen rights on those records by naming an address. Dynamic
AML regions remain unavailable until independently admitted by the resource owner.

The public protocol now names an explicitly admitted register ID, operation and
value. It accepts no address, offset or access width. Trusted configuration binds
that ID to a fixed internal offset, width and permitted write bits, frozen with
the resource epoch. Knowing an ID is not authority: the authenticated endpoint
stamp and live epoch must also match. Internal region bounds still check every
byte of the complete transaction, including alignment and overflow. Register
bindings default disabled (ID zero). A write mask rejects forbidden bits; it
never truncates input or performs implicit read-modify-write. Admission must
establish that an exact write has safe device-specific semantics. More complex
controls need explicit operation handlers, not a generic register-write alias.

The intended live owner of address resolution and access enforcement is the
kernel. The current portable policy/executor and mock tests model that boundary;
they do NOT yet install a kernel register catalog or syscall. Do not launch the
userspace backend with unrestricted access as a substitute. AML declarations,
GAS addresses and callers cannot populate or enlarge the trusted catalog.

`kernel/src/hardware_catalog` now implements bounded inventory metadata and
membership selection. Trusted discovery can build up to 64 descriptors, then
seal the inventory. IDs are unique across categories; sealed membership cannot
be edited. Revocation blocks selection, and replacement increments a nonwrapping
epoch so old selections cannot name replacement resources. `Can_Select` checks
membership and the shared narrowing relation. Descriptor validity checks
transaction geometry; it does not establish physical ownership or safe register
semantics. No boot instance, discovery adapter, cspace install, hardware syscall
or actual I/O is wired yet. Catalog revocation does not free in-flight backing.
The catalog's `Resolve` now checks current epoch, the authenticated scope's
permission subset and both scope/catalog operation restrictions before returning
a kernel-internal descriptor. It does not require delegation permission merely
to use an existing grant. Syscall glue must retrieve that scope from kernel-owned
capability state, not user words, and reserve backing through actual completion.
A successful readonly lookup alone does not prevent revocation races.
`Begin_Access` now reserves one transaction in the catalog. Revoke prevents new
admissions but keeps the inventory and reservation; replacement is rejected
while busy. Only the matching monotonically increasing completion ticket clears
it. Unknown hardware completion must remain reserved, and tickets stay inside
the kernel. `Ready_To_Release` requires both revocation and completion. Actual
mapping retention, serialization and the hardware callback must obey this model;
none is established merely by the policy proof. Checked native contract snapshots
still require a whole-call-chain stack assessment before live integration.

Startup authority may group the discovered concrete resources under categories
such as ACPI or GPIO. A group is an explicit kernel-owned membership set, never
an address wildcard or permission to register new resources. Delegation selects
existing members and a subset of their allowed operations, with no widening of
value restrictions. A GPIO child could receive one pin's output operation;
a power service could receive admitted sleep controls. Hibernate is an
orchestrated power operation, not assumed to correspond to one register.
`shared/hardware/Hardware_Authority` now supplies the common per-resource
permission relation used by ACPI dispatch. Its SPARK theorem establishes that
every access allowed by a derivable child was allowed by the parent, including
write-bit restrictions. These records are trusted policy data, not forgeable
standalone capabilities. The current kernel's ordinary derivation keeps the
same object; selecting a group member still needs a membership-checking kernel
operation and revocation linkage, not tag-changing endpoint minting.

The kernel now defines CAP_HARDWARE_GROUP and CAP_HARDWARE_REGISTER and rejects
both from generic POLICY_MINT_CAPABILITY through isPolicyMintable. Existing
capability ordinals are preserved. The kernel build includes the shared hardware
permission definitions, and hosted tests compile its actual capability and
catalog sources. Hardware_Grants now allocates registry lifetime identities,
stores immutable grant permissions and tracks ancestor revocation. Its Cspace
child checks actual capability type, generation, registry identity and rights
before reserving access with the stored permission. Trusted Install_Group and
checked Install_Child bind grants into empty kernel capability slots; rejected
requests leave both registry and table unchanged. No boot caller or access
syscall is wired yet. Platform resource admission, serialization, actual caller
and destination-table authorization, mappings and hardware execution remain
required before live use.

Membership changes, removal, replacement and parent revocation must invalidate
or appropriately restrict derived authority; stale IDs cannot select replacement
hardware. Initial implementation should freeze membership for each grant epoch;
newly discovered resources require explicit trusted regrant. The bounded catalog
and grant helpers implement epoch checks, member selection and ancestor
revocation. Their live initialization and enforcement still need kernel wiring.

The ACPI service must not receive CAP_DEVICE_MEM/CAP_IOPORT as a shortcut for
mediated access: those capabilities also authorize other kernel access paths
and could bypass backend policy. Privileged resource authority stays with the
backend; the service receives only the scoped endpoint authority needed for
allowed requests, without minting or unrestricted delegation rights. Reuse
existing capability checks behind that boundary, not as broader service grants.

`ACPI_Region_Policy` now models a backend-owned region record behind existing
endpoint authority. Trusted Install stores the base, length, space, rights and
width set; untrusted Resolve inputs contain only stamp/token/offset/width/op.
Resolve checks the authenticated tag and record epoch, permissions, complete
transaction extent, natural alignment and nonwrapping address arithmetic.
Revoke disables the record; reinstall requires inactivity and increments its
epoch without wrapping. Preserve this record across reuse, never reset epochs.
`Begin_Access` now reserves one in-flight operation and issues a monotonically
increasing completion ticket. Revoke blocks new admissions without clearing that
reservation; Install cannot replace a busy record. Only the matching completion
can clear it, so delayed duplicate completions cannot release a later operation.
`Ready_To_Release` requires both revocation and completion. Ticket counters never
wrap and must survive record reuse. Native handlers must use this reservation
path, not execute a bare Resolve result. The model now has 86 SPARK checks and
20,029 hosted regression checks. It is not yet wired to kernel-received stamps
or actual I/O: independently trusted resource admission, serialized state
transitions and physical backing retention until release remain required. A public
Ada Install procedure is a trusted internal API, not an allowed service request.

`ACPI_Region_IO` now executes typed scalar requests through a reserved region
and an injected backend callback. Oversized write values and nonzero read input
are rejected without truncation; denied/malformed requests leave policy and
backend state unchanged. A callback's confirmed completion releases the ticket;
a read result wider than the selected width revokes the region as a backend
fault. Uncertain completion revokes new access and preserves the reservation,
returning an internal ticket for trusted cleanup. It never retries the operation.
The SPARK-verified instantiation is the hosted mock, not a native hardware
adapter. Its proof depends on the instantiated callback contract; native bindings
need their own review/proof boundary. Its draft `Dispatch` wire boundary now uses a separately scoped resource
endpoint: label 0 reads and label 1 writes; four words carry epoch, register ID,
value, and zero. Read value must be zero; flags and
reserved bits must be zero. The kernel-authenticated stamp is a separate input,
never a payload word. Unknown labels cannot invoke Install, Revoke or Finish.
Reply label 0xF002 carries outcome, low32 value, high32 value, zero. Outcomes are
0 Denied, 1 Malformed, 2 Done, 3 Indeterminate, 4 Backend_Fault. Cleanup tickets
remain backend-only. This protocol still lacks a native receive loop, resource
admission and real device-access endpoint, and does not implement atomic
bit-field read-modify-write transactions.

`ACPI_FADT.Transactions` is only geometry supplied with candidate bounds. Its
caller-supplied bounds are NOT evidence of authority. Before any live hardware
endpoint is enabled, it must be wired to backend-owned admitted records and
tested with forged bounds/handles, overflow, boundary crossings and revocation.
This enforcement path remains unimplemented; a passing geometry proof is not a
proof of capability confinement.

Live register and OperationRegion access is separate from snapshot delivery.
Copying a register description does not relocate the physical resource it names.
The immutable-table copy fallback must never be applied to live writable state.
If a live range shares a page with bytes outside service authority, withhold the
whole page mapping and mediate operations against the original backing. Use a
scoped region handle plus validated offset/width/operation, not arbitrary physical
addresses. Preserve field/update semantics and required transaction locking;
separate read and write RPCs are not automatically an atomic read-modify-write.
A shadow page with later copy-back is not a general solution for side-effectful
registers or firmware-shared state. Direct RW mapping is allowed only when the
entire page is authorized for that access, including the reads mapping entails.
A scoped hardware backend must access the actual memory or I/O range, through
an authorized mapping or mediated operations, preserving access widths, cache
attributes, ordering and register side effects. AML calls this backend; native
driver ownership must be coordinated rather than bypassed. FACS is live shared
state, excluded from the immutable SDT snapshot. Preserve its original backing
and ACPI NVS lifetime; prefer narrow kernel support for the firmware Global Lock
and wake-vector/suspend-resume coordination. These hardware paths remain design
requirements, not implemented service capabilities.

Current allocator evidence: `Multiboot.getMemoryAreas` maps ACPI reclaim and
NVS to ACPI/HIBERNATE rather than USABLE. `MemoryAreas.Allocation_Map` reserves
all touched pages for non-USABLE regions, and `BootAllocator.setup` admits only
classified usable frames. Thus a direct handoff need not race an existing boot
reclamation pass. `ACPI.setup` now records admitted root/child SDTs and the
FADT-selected DSDT in `Firmware_Tables.Catalog`, publishing only at the end of
successful discovery. `ACPI.Table_Count` returns zero until publication, or if
capacity/conflicting DSDTs invalidate the inventory. `Table_Source` returns the
DSDT first and then other tables in discovery order, with physical address,
extent, signature and revision. Repeated identical DSDT observations coalesce.
The bounded inventory holds at most 256 tables; it does not truncate a prefix
on overflow. That metadata capacity is independent of the service's current
32-table, 64-KiB-per-table import budgets, which the future provider must check
before starting an import. FACS is excluded from this immutable inventory.

Cache-attribute checks and the authority-bound grant lifecycle remain to be
implemented. Metadata validity does not prove raw memory backing or lifetime;
the boot adapter relies on admitted immutable firmware and retained backing.
Existing boot reservations do not themselves authorize a userspace mapping.
The catalog model is SPARK-verified; the raw kernel overlays are not. Inventory
failure leaves existing kernel ACPI consumers running and disables export.

`ACPI.Table_Page_Exposure` now uses the pure `Firmware_Tables.Exposure`
planner. It returns the rounded 4-KiB page range, byte offset and page count,
and classifies it as `Retained_Candidate` or `Copy_Required`. A candidate has
proved coverage of **every byte** in the rounded range by admitted immutable
SDTs, including adjacent/overlapping tables in arbitrary discovery order. A
one-byte gap is enough to require a copy; unknown neighboring bytes must never
be exposed or scrubbed in place. The bounded interval scan avoids per-byte
work in the native implementation. Ghost range checks are erased there.

This is content eligibility, not mapping authority. Firmware backing/cache
attributes and retained lifetime must still be established separately. In
particular, `Firmware_Readable` is a kernel-reading check and admits some NVS
and legacy regions; it is not sufficient approval for a userspace grant.
`ACPI.Table_Backing_Is_Reclaim_RAM` now classifies the rounded range separately
through `Multiboot.Firmware_Reclaim_Pages`. The latter requires a published boot
map, nonzero page-aligned base and length, and the complete range within the
kernel direct-map limit. The pure `Multiboot_Memory_Map.Reclaim` policy requires
complete coverage by ACPI-reclaim entries and rejects any overlapping different
kind, including NVS, plus malformed nonempty entries. Adjacent entries may cover
a range jointly, regardless of order. This identifies a firmware-map kind; it
does not establish cache attributes, ownership/pins, or authority to map it.

`Process.IPC.createGrant` currently resolves the calling process's mapping and
pins each frame with `BuddyAllocator.pinOwnedFrame`; rollback/retirement assumes
those pins. Firmware sources cannot simply be inserted into that path. A new
adapter must distinguish ownership and release semantics, or copy into normal
owned pages before using existing grants. Generic non-public Sysinfo queries
currently accept CAP_PROCESS+READ; this alone does not identify the trusted
snapshot provider and must not become an implicit firmware-export authority.

Proposed requests, with versioned typed replies and generation-bearing IDs:

- Snapshot import: kernel/broker supplies copied DSDT, ordered SSDTs and other
  immutable description tables, their
  admitted extents, signatures, OEM identity and DSDT integer width. Enforce
  per-table and total byte budgets before allocation/copy, validate checksums
  again, and keep immutable owned storage for the namespace lifetime. Reject
  missing/truncated tables, duplicate installation and budget exhaustion;
  don't publish a partial namespace as complete discovery. FACS, NVS and
  registers require live coordinated ownership, not snapshot treatment.
- Enumeration/resource query: return device IDs, parent IDs, object types,
  evaluation status and typed resource descriptions. Firmware resource claims
  are untrusted metadata. Declarations alone do not establish presence; `_STA`
  can require execution and hardware access. Report unavailable/unsupported
  explicitly. There is no public arbitrary EvaluateMethod endpoint.
- Broker-mediated operation region transaction: bind an installed region to
  an existing device authority and validate space, base, length, access width,
  alignment, direction, owner and generation at every transaction. Check ranges
  by subtraction to avoid wraparound. Deny all accesses by default, including
  reads (reads can acknowledge or change registers). No catch-all SystemMemory
  or SystemIO mapping. PCI segment/BDF/register bounds are explicit; EC/SMBus/
  GPIO/GenericSerialBus/PCC need distinct protocol brokers, not memory aliases.
- Diagnostics: bounded errors include table/AML offset, device/method, reason,
  and known completed effects. Failures carry no usable object/resource handle.
  Restart invalidates old generations; interrupted writes are not rolled back.

Before a native launch, configure finite table bytes/count, namespace objects,
value/buffer bytes, operand/call depth, instruction fuel, outstanding requests,
event queue length and wall-time deadlines. Charge work before execution and
bound callbacks as well as instructions. Numeric limits must be justified by
real captured firmware and adversarial tests, not silently raised on failure.
The present decoder has maximum eight integer payload iterations and three
package continuation iterations, regardless of input length, and no callbacks.

## Policy, sequencing and events

User power preferences belong to a separate policy client. ACPI implements
firmware mechanisms; device drivers own their hardware sequencing. In particular,
i915 retains GPU power/reset/display sequencing. ACPI power resources and
`_PSx`/`_DSM` requests require an explicit driver handshake, shared transaction
serialization and defined recovery. Enumeration does not evaluate `_INI`, switch
ACPI mode, select an `_OSI` personality, or take over native PCIe control `_OSC`.
These effects require a later coordinated activation milestone.

SCI/GPE plan: kernel remains responsible for interrupt delivery/enforcement;
ACPI receives a scoped event channel. A dedicated broker serializes status,
enable and acknowledgment register transactions. Distinguish level/edge GPE
semantics, mask storms, use bounded deferred work and preserve pending status
until the specified acknowledgment point. Never execute AML in an interrupt
handler. Fixed events, GPEs, Notify and wake events need separate typed routing.

EC plan: one owner arbitrates command/data ports and EC operation regions,
with bounded waits, query queueing, burst-mode handling and platform-required
global-lock semantics. AML mutexes, serialization and firmware global lock
have distinct semantics; define lock ordering with native drivers before use.
Do not silently poll forever or replay a partially completed EC transaction.

Battery/thermal plan: begin with captured fixtures for `_BIX`/`_BIF`, `_BST`,
`_TMP` and trip-point objects, preserving units and unknown/error states. These
are not intrinsically side-effect-free methods. Enable real reads only after
operation-region mediation and platform review. No fan control, thermal-policy
change, shutdown, sleep (`_PTS`/sleep-control writes), wake vector or suspend
transition in initial milestones. Suspend later needs a system coordinator for
quiescence, DMA, driver state, power resources and resume ordering; modern idle
support is a separate platform requirement from legacy S3.

## Interpreter choice

No third-party dependency is added in this milestone.

| Option | Coverage and benefit | Cost / licensing boundary |
| --- | --- | --- |
| ACPICA | Mature interpreter, namespace/resource/event machinery and iASL/acpiexec fixture tools; Linux integration is a useful cross-check | Substantial C trusted base and OS adaptation; upstream distribution/header license must be pinned and reviewed (dual BSD/GPLv2 alternatives exist; packaging differs). Linux glue must not be assumed permissively licensed. All OS callbacks still require capability mediation and budgets. |
| uACPI | Portable C implementation; upstream advertises interpreter, operation regions, events and resources | MIT license; less deployment history than ACPICA. Review the pinned version's callbacks, synchronization and cancellation, and independently test coverage. Upstream performance/compatibility claims are not CuBit measurements. |
| Ada/SPARK core | Fits existing runtime and permits bounded storage and local proofs; no foreign interpreter dependency | A complete AML implementation is a large sustained effort: coercions, references, namespace lookup, dynamic objects, synchronization and real firmware quirks. A small proved decoder establishes none of that coverage. |

Decision: keep this small Ada/SPARK foundation while assembling a read-only
firmware corpus; do not commit to a production interpreter on proof aesthetics
alone. Next compare a denied-I/O hosted ACPICA/uACPI harness against a narrow
SPARK Name/Scope/data-object loader. Gate production selection on namespace and
pure-method coverage, resource bounds, integration effort and license review.
Never silently treat unsupported AML as successful enumeration.

## References and next evidence

Primary semantics: [ACPI 6.6](https://uefi.org/sites/default/files/resources/ACPI_Spec_6.6.pdf),
sections 5 (namespace/events), 6 (devices), 12 (EC), 19 (integer rules), and
20.2.3–20.2.4 (data and package encodings). The HTML AML chapter returned HTTP
403 during this audit; the official PDF/search-indexed text was available.
Cross-check: Linux [ACPICA package decoder](https://github.com/torvalds/linux/blob/master/drivers/acpi/acpica/psargs.c)
uses the same low-six/low-four-bit and continuation layout; this is source
inspection, not a differential test or copied Linux implementation.
Options: [ACPICA upstream](https://github.com/open-acpica/acpica),
[uACPI upstream and license](https://github.com/uACPI/uACPI).

Next milestones: bounded NameString and constant-object namespace loading,
whole-definition-block failure publication, generated ASL differential fixtures,
then native read-only snapshot handoff in an isolated build workspace. Capture
N95/N100 model/firmware identity and DSDT/SSDT bytes before claiming platform
coverage. Hosted tests establish no native service behavior; QEMU integration
would establish no physical NUC firmware behavior. Physical laptop battery,
thermal and suspend results must be recorded separately.

## Active full-service goal (2026-10-01)

The user now explicitly requests a SPARK-verified AML interpreter and a userspace
ACPI service moving all feasible ongoing ACPI work out of the kernel. That is
the active objective; the decoder milestones do not redefine completion.
ACPICA now supplies pinned upstream ASLTS and generated differential validation
through `tests/aml-core/run-acpica.sh`; uACPI remains a comparison candidate.
Neither substitutes for the requested verified interpreter. The original option comparison records
engineering cost and licensing, not authorization to replace that objective.

`AML_Names` now adds bounded NameString parsing, including all encoded segment
counts and explicit parent-prefix resource exhaustion. Its contracts establish
published name grammar and extent safety. Full namespace loading/resolution,
AML values/execution/synchronization, service integration, capability mediation,
and device/event functionality remain outstanding. Full correctness is not
established by these local proofs; each later semantic claim requires explicit
contracts, proof evidence, regression and integration coverage.

## User-facing operations and service responsibilities

User clarification, 2026-10-01: ACPI participates in lid, backlight and hibernate
support but should not own all those features. Proposed interface layering:

| User operation | Policy / coordination owner | ACPI contribution | Other implementation |
| --- | --- | --- | --- |
| Lid closes | Power policy considers user preference, dock/external display, inhibitors and session state | Lid device adapter observes Notify(0x80), reevaluates _LID, publishes typed open/closed/unknown state | Session locks; display manager changes outputs; suspend coordinator runs only if policy requests it |
| Adjust brightness | Display/backlight service exposes per-output levels and selects one backend | Firmware backend provides _BCL/_BCM/_BQC where supported and authorized | GPU/panel driver may own native PWM or AUX control; model-specific backend may be needed |
| Hibernate | System power coordinator orchestrates quiescence and recovery | Firmware preparation, wake configuration, S4 entry hooks when applicable | Kernel freezes/snapshots/restores system state; storage persists image; early boot restores it; drivers quiesce DMA and reinitialize |

ACPI standardizes firmware-facing interfaces, not identical hardware. Expose
capabilities and explicit unsupported/unknown states. Select one owner per
physical backlight; do not run firmware and native controls concurrently. A
brightness hotkey is a request/event distinct from the panel control mechanism.

Suggested consumer APIs are SubscribeLidState, QueryBacklightCapabilities,
SetBrightness(output, level), and RequestSystemTransition(kind). These are
conceptual interfaces, not allocated wire opcodes. Only platform adapters get
typed firmware operations; desktop clients use domain services. Device adapters
can initially live beside the interpreter in the ACPI process, while keeping
policy outside and access narrow. Splitting every adapter into a separate
process is not required to preserve this boundary.

Lid events describe change, not necessarily the new value; requery _LID. Carry
sequence/generation and validity, report read failures as unknown, resynchronize
on subscribe/resume and handle duplicate notifications. System transitions need
explicit prepare/commit/resume/failure phases and ordered dependencies; a failed
prepare must prevent the final hardware transition. Hibernate is not a single
AML call and cannot be implemented solely in this userspace service.

References: [ACPI lid device](https://uefi.org/specs/ACPI/6.6/09_ACPI_Defined_Devices_and_Device_Specific_Objects.html#control-method-lid-device),
[ACPI sleep/wake](https://uefi.org/specs/ACPI/6.6/16_Waking_and_Sleeping.html),
[Linux backlight abstraction](https://docs.kernel.org/gpu/backlight.html),
[Linux hibernation lifecycle](https://docs.kernel.org/admin-guide/pm/sleep-states.html).

## CCL IPC and observability (user decision, 2026-10-01)

Use the existing CCL typed IPC/interface system for consumer notifications and
queries. Do not invent a parallel ACPI command language. Reuse bounded shared
stream transport and CuBit.Logging/logstore; CCL observes logs through the
existing log-observer authority. Catalog/manifest additions require coordination
with the CCL/startup owners. No interface IDs or wire ordinals allocated yet.
Current descriptor documentation notes scalar/stream format limitations; verify
actual compiler/runtime support before publishing structured stream descriptors.
A design schema is not evidence of a working CCL binding.

Implemented foundation: `ACPI_Requests` now handles bounded snapshot uploads
and seven revision-bearing metric pages without runtime dependencies. Its
service-local draft labels are not allocated catalog interfaces. Observer calls
preserve all state; unclassified callers receive denial and zero data. Replies
fit CCL's signed 64-bit integers. Native endpoint authentication, restart epochs,
CCL bindings and event/log streams remain unfinished. The
[service README](../userspace/services/acpi/README.md) specifies the draft packet
layout and explicitly separates these assumptions from the proved core.
`ACPI_Endpoint` now supplies exact tag classification and canonical success/error
encoding, with a compiled `CuBit.Messages` adapter. Invalid tag configurations
deny all access. The adapter consumes the separate kernel-stamped authority
field, never caller-controlled header bits. This does not allocate capabilities,
authenticate locally constructed messages, or implement a native receive loop.

Planned semantic surfaces, separately granted:

- Observation queries: paged device inventory and supported features, current
  lid/battery/thermal readings with validity and sample age, service status and
  metrics. Queries read cached state by default; any refresh that executes AML
  has a distinct authorized operation, deadline and effect contract.
- Event subscriptions: bounded typed device/state notifications with service
  generation, device ID, monotonic sequence/time, event kind and typed payload.
  Use snapshot-plus-sequence establishment so subscribing does not lose changes
  between query and stream binding. Explicit gaps require resynchronization.
  Slow observers cannot stall SCI/EC processing. State updates can coalesce
  with visible sequence gaps; button/transition events need explicit loss
  reporting and must not be silently treated as state snapshots.
- Metrics snapshot: admitted table bytes/count, namespace used/capacity, active
  evaluations, instructions charged, execution failures/budget exhaustion,
  denied region operations, pending events, queue high-water mark, coalesced or
  dropped events, subscriber gaps and log drops. Counters saturate with an
  overflow flag; snapshot includes generation/time. No raw physical addresses,
  arbitrary firmware buffers or per-device unbounded metric labels.
- Diagnostics: bounded severity/text records via existing logstore, with stable
  error categories and bounded device/method/AML-offset context. Rate-limit
  repeated failures and account for loss. Authenticated publisher identity comes
  from the existing transport, not a producer-provided field. Structured ACPI
  failure details belong in the typed observation interface until the common
  logging schema supports them; do not pretend today's text record has fields
  it does not. Logs are diagnostic and cannot be the control event channel.
- Control: domain services expose brightness/system-transition requests to
  policy clients. ACPI's firmware operation endpoints remain separately scoped;
  observation authority never grants arbitrary AML evaluation or register I/O.

Acceptance requires CCL-native query/subscription tests, unauthorized binding
rejection, version/type mismatch rejection, slow-consumer and overflow recovery,
restart/stale-generation rejection, bounded metrics/logging under event storms,
and proof of the pure codec/queue/accounting components. This is planned work;
the current transport-independent request core implements bounded metrics and
completed-snapshot SDT metadata/byte queries, but no live CCL endpoint or event
subscription. See [request layouts](../userspace/services/acpi/README.md).
Description-table admission checks common headers and checksums; semantic
MADT/MCFG/DMAR/SRAT/SLIT decoding remains separate work. A pure FADT
structural decoder now reads retained FACP tables; hardware descriptor
normalization, authorization and live CCL exposure remain unfinished. Mutable FACS is
excluded. Observation authority permits no AML execution or hardware access.
