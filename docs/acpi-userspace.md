# ACPI and AML outside the kernel

## Hosted Field I/O checkpoint (2026-10-21)

One named whole Byte/Word/Dword register can be bound to an explicit hosted
provider context using NoLock. An indeterminate result latches uncertainty,
stops AML continuation and blocks replay until the exact receipt is reconciled.
See [Core and unknown reads](../tests/aml-field-io/README.md),
[cached reference replay](../tests/aml-field-replay/README.md),
[default-wrapper regressions](../tests/aml-field-default-regressions/README.md)
and [Namespace Reset/reconciliation](../tests/aml-field-reset/README.md).

Both current-source profiles passed 702 Core/unknown-read checks and 522 cached
reference checks. Reference errors and original goldens remain preserved.
Current default-wrapper and bare Namespace suites passed 5,232 and 42 checks
per profile. Fifteen maintained mains passed 808,013 checks per profile using
25 accepted stages plus 41 fresh stages. Original failures remain recorded;
checked call coverage uses separate width processes of the same binary, and
the unsplit checked timeout remains unresolved. These receipts do not claim
that all 33 service selections ran or repair the direct Core.Values compiler
instantiation issue.

The existing Field_Layout.Admitted postcondition and Octets/Aligned dependencies
have 10 proof diagnostics, 6 selected flow diagnostics and two explicit
generated-empty effect summaries. This narrow proof is not full admission,
interpreter or native correctness. BankField, IndexField, partial/RMW,
multidatum, Qword, FACS locking and native hardware access remain unsupported
by this interface.

Source inventory, hosted behavior and proof evidence have separate scopes.
The 15-command metadata validation receipt 42307 belongs to the preceding
code/metadata tree; five unsupported validation families were metadata-only.
The [historical evidence index](../tests/aml-field-io/EVIDENCE.json) records exact
source and receipt scopes; it supplies no private-file execution fallback.

## Fatal hosted milestone (2026-10-21)

Typed Fatal propagation, cleanup and the hosted observer/formatter passed the
final six-main focused suites: 813 checks per profile (release 75451, checked
76862). The exact 14-table cached replay passed 713 checks per profile
(63818/33215), preserving its original ACPICA observations and hook goldens.
Fifteen maintained mains per profile passed 808,013 checks per profile in gate
85389. Checked call coverage used the same binary in separate width processes;
the original unsplit checked timeout remains unresolved. This does not claim
all 33 service selections were rerun.

Pure AML_Fatal framing/status, Kind, little-endian Code and host-low32 projection
contracts have 34 proved checks and four flow checks, with explicit compiler
Interfaces.Shift_Left intrinsic dependencies. Interpreter propagation, cleanup
and observer behavior are tested, not proved end to end. Native Fatal logging
and shutdown delivery remain unfinished. The standalone focused and cached
replay suites are opt-in. Portable registration is checked separately with
`--validate-only`; those checks do not compile or execute the interpreter.
Existing historical records below are preserved.

## Optimized scalar-ledger checkpoint (2026-10-21)

The maintained live-object count helper passed separate complete 76-entity
proof gates with 859 proved checks and zero unresolved checks in each instance:
small capacities 3 objects/6 roots/generation 50, and the current service
capacities 30,559 objects/30,559 roots with the production default generation
limit. This is capacity-specific helper proof, not generic-capacity or full
interpreter proof. The exact two ledger units are unchanged from those proofs.

Hosted composition gate 13797 passed 8,177 checks per profile across nine mains
on current Mutex and landed BreakPoint sources, including 144 ordered matrix
observations per profile. Checked call coverage used the same binary in two
processes with 2,509 checks each; the unsplit timeout remains unresolved. These
are scoped hosted results, not native or all-service-suite coverage. Portable registration runtime evidence is recorded separately from this hosted
composition evidence; see the
[focused ledger registration](../tests/aml-scalar-ledger-count/README.md). Historical
proof receipts and sizing provenance remain immutable, with no new proof implied
by manifest updates. Existing service-milestone selections remain unchanged.


## Non-debug BreakPoint checkpoint (2026-10-21)

BreakPoint (0xCC) now follows the non-debugger Noop policy and retains normal
statement fuel charging. The focused hosted fixture passed 209 checks per
profile (release 8964, checked 96822). The standalone
[BreakPoint suite](../tests/aml-breakpoint/README.md) is opt-in; the service
suite retains its 33 selections. Portable relocation passed eight actual
`--validate-only` commands; five unsupported runners remained metadata-only.
Central `--group breakpoint` release worker 18634 and checked worker 75622 each
passed 209 checks. This does not claim an all-33 service runtime run.
Cached normal ACPICA evidence preserves all eight compiled/disassembled cases,
including debugger errors and two module timeouts; it is not eight scalar
passes or a debugger equivalence claim. Fatal, other remaining extended
operators, native debugger delivery and full proof remain unfinished.

## Mutex invocation and ownership checkpoint (2026-10-21)

Ordinary uncontended Acquire/Release and serialized-method ownership now have
hosted coverage, including recursion, sync-level ordering, same-invocation
sharing, authenticated object/frame identity, implicit-lock restoration before
frame retirement, explicit-lock cleanup at invocation exit, and held-object GC
roots. Contention waits, inter-caller/concurrent serialization, FACS Global Lock,
native scheduler integration and full locking proof remain unfinished.

The procedural raw-State API is Invoke_Read_Only, Invoke_Mutable and
Invoke_With_Tables. Callers must provide exclusive synchronous access throughout
the invocation, including callbacks; callbacks must not reenter the same State.
Per-invocation lock state does not serialize separate callers. The public spec
states this assumption; it is not a proved concurrency precondition. The wire
error Mutex_Not_Acquired is appended as 40; existing codes 0 through 39 retain their
values. This is exact error reporting, not new external authority.

Evidence is separately scoped: the initial focused integration passed 2375
checks/profile, including the exact 24-table/144-observation ACPICA matrix.
The maintained-session continuation passed 17 release mains plus Requests
semantic compilation; original call_tests release 5018 passed separately.
Segmented checked call_tests covered 5018 checks in fresh processes; the original
unsplit 30-second checked timeout remains unresolved. Table sessions passed 415
checks/profile. The unchanged original 137-byte reference replay passed 40 checks
per profile in worker 18201. These are distinct runs, not an all 33 portable
rerun or completion of the original full 75-stage gate. Historical failures and
reference bytes remain immutable. The negative Package-Mutex IPKG remains
Unsupported_Value with recovery; no valid positive temporary-package
ReleaseIndex coverage is claimed. No full SPARK proof or native execution is
claimed. See the [service milestone registration](../tests/acpi-service-milestone/README.md).

Historical stage1 summary (original wording retained):


Runtime Mutex declaration and retained-object lifetime now have hosted tests:
469 checks per profile through the portable service-milestone runner. The normal
ACPICA table replay advances through module admission to the still-unsupported
Acquire operation. Lock ownership, Acquire/Release, concurrent integration and
current-source proof remain unfinished. See the
[coverage checkpoint](acpi-coverage.md#mutex-declaration-and-lifetime-checkpoint-2026-10-21).

The explicit hosted `table-fields` group passes release and assertion-enabled
checked profiles: 662,835 focused checks, 12 cached ACPICA comparisons, 1,862
protocol checks and 13 evidence guards per profile. Table-backed fields now
extract directly into owned arena Buffers beyond 1,024 bytes within arena quotas;
byte windows retain a 1,024-byte per-request bound. Legacy inspection remains
bounded. Literal-string DataTableRegion selectors and the supported read-only
module-level Field subset load through the service; general module-level TermArg
evaluation, side effects and forward-resolution semantics remain incomplete.
These are hosted results, not a full interpreter proof or native deployment.
See [table-field registration](../tests/aml-table-fields/README.md) for commands,
reference scope and the narrower proof/admission evidence.

Hosted progress (2026-10-14): the AML layer now has tested static Alias
lookup and an internal Core notification queue. Physical namespace bindings remain
separate from canonical object identity, including owner-bound root metadata.
The queue copies event descriptions and preserves the original target identity;
it does not grant hardware authority. CCL event transport, subscriptions and
native asynchronous completion ownership are still design/integration work.
Focused tests and selected ACPICA Notify comparisons pass in both strict build
modes; this is not a full interpreter proof or live service deployment. The
public `notify-alias` hosted group reproduces the focused and reference checks;
broader BufferField regressions also pass in both strict build modes.

The metadata milestone now distinguishes live namespace values from captured
Device/Thermal descriptors, with authenticated retention, root binding and
collection checks. Ten focused programs pass 2,914 checks per profile; the cached
metadata corpus matches 82 cases per profile, including 12 explicit coarse error
projections. The public `metadata` group passes in both strict profiles. These results do not prove the full interpreter or implement dynamic
Device declarations, complete descriptor copying, or CCL delivery.

The public `buffer-sizing` group passes in both strict profiles: five mains
exercise 9,077 checks, thirteen adapters compile with source-path verification,
and eighteen cached ACPICA cases match (four use explicit coarse error mappings).
The group also passes 2,448 protocol and 15 evidence-guard checks. Larger literal
buffers now use validated spans and arena-backed allocation instead of the
former 1,024-byte literal-decoder cap. Owned table-field extraction now exceeds the legacy 1,024-byte inspection
limit; individual byte-window requests remain bounded to 1,024 bytes. Hosted behavior is tested. Separate unit proofs discharge
183 decoder checks
(including Check_Buffer_Count_Span) and 91 layout checks on the applied sources.
Allocator and full-interpreter proofs remain incomplete; native deployment is
untested.
See [buffer sizing registration](../tests/aml-buffer-sizing/REGISTRATION.md).

Runtime Buffer count expressions now have a separate hosted milestone. The
`buffer-dynamic` group passes 473 owner checks and 20 ACPICA comparisons in each
strict profile, including count-method side effects and retained values across
actual collection. After combining separately proved span/layout updates, the
checked group passed again; release evidence is from the preceding snapshot.
Parsing remains bounded by the Buffer package. General
static-loader count execution and nested Package/VarPackage evaluation remain
unfinished; current-source interpreter proof and native integration are pending.
See [dynamic Buffer registration](../tests/aml-buffer-dynamic/REGISTRATION.md).

Track remaining work and completion evidence in the
[ACPI coverage checklist](acpi-coverage.md).

Status: shared table admission is integrated into native BIOS/UEFI boot. A
SPARK-analyzed userspace service core and partial AML interpreter have hosted
validation; no live userspace ACPI service or IOMMU enforcement is deployed.
See [current service core](../userspace/services/acpi/README.md) for implemented
interfaces and remaining integration work.

The hosted AML implementation includes runtime buffer fields and package capture
of their values, with authenticated backing references, bounds checks and
collection tests. Buffer fields operate on service-owned AML data; they do not
map firmware registers or confer hardware capabilities. See the
[buffer-field milestone](acpi-coverage.md#buffer-field-hosted-milestone-2026-10-14)
for test coverage and remaining proof/deployment work. Lid, backlight, sleep and
hibernate policy, SCI/GPE/EC handling and CCL event delivery still require their
separate service and hardware-authority integration.

## Invariant

Firmware describes hardware; it does not grant authority. Evaluating a firmware
method must not create access to memory, I/O ports, PCI configuration or another
service. Hardware effects require existing, explicitly provisioned authority.
Use CuBit's [authority model](security-model.md), not a parallel ACPI permission
system or a privileged escape hatch.

Move ongoing ACPI discovery, AML evaluation, device/power policy and event
handling into userspace. Keep the kernel's unavoidable early bootstrap and
hardware enforcement small and auditable. Do not introduce an in-kernel AML
interpreter as an intermediate milestone.

## Current implementation

`kernel/src/acpi.adb` discovers firmware tables, handles FADT/MADT/MCFG data and
feeds early platform setup. `parseDSDT` checks the signature but leaves AML
interpretation as a TODO. SSDT handling is reported as unsupported. HPET descriptors are now decoded
through `Firmware_Tables.HPET` for the early kernel timer path.
Existing address overlays/table walkers are not made safe merely by marking
their package SPARK On; table admission needs the same explicit raw-address
boundary and bounded pure parsing approach as the boot memory map.

The boot-entry regression exposed an immediate legacy-FADT overread: the old
QEMU chipset's short table has no X_DSDT field, but the kernel read it anyway.
The adapter now checks the legacy prefix length and conditionally reads the
complete extension, falling back to the legacy DSDT address when absent/zero.
This fixed that concrete failure without adding AML. Subsequent UEFI work now
checks table extents, retained physical backing and checksums at admission,
but complete immutable snapshots and all semantic walkers still need review;
the native ACPI adapter is not claimed SPARK-proved.
See the [early text-boot regression](../tests/multiboot-entry/README.md).

Initially retain the minimum early topology, interrupt-routing and timer data
needed to start the scheduler and services. Over time, move additional table
consumers out when the boot dependency graph permits it. Kernel memory
reservation/mapping and interrupt/device-access enforcement remain kernel
responsibilities. Reserve firmware ranges until their consumers release them;
do not free ACPI reclaimable memory before copying needed tables. ACPI NVS is
not ordinary reclaimable storage.

## Proposed service boundary

### Placement and next implementation steps

User decision (2026-09-26): AML interpretation runs in a userspace driver/service,
never an in-kernel interpreter. Move additional ACPI consumers out as bootstrap
dependencies permit. This does not move the enforcement of DMA isolation out of
the kernel.

- **Shared pure code:** `shared/firmware/firmware_tables.*` admits copied RSDP
  and standard description-table bytes. It has no kernel, GNAT runtime-specific,
  raw-pointer or device-access dependencies. Its result variants expose header
  metadata only on acceptance. Native boot now uses it for the RSDP and SDTs.
- **Early kernel adapter:** locate firmware through a defined bootloader
  handoff, validate physical backing and lengths before reading, reserve/copy
  tables, then use shared parsers. Retain early CPU/interrupt topology and the
  minimum data needed to establish isolation before drivers can perform DMA.
- **Kernel enforcement:** own IOMMU domains, I/O page tables, invalidation,
  interrupt remapping and pin/mapping lifetimes. A firmware address or device
  scope is not itself authority to access memory.
- **Userspace ACPI:** consume admitted snapshots, build the device namespace,
  interpret AML, handle events and apply device/power policy via scoped existing
  authorities. Mutable firmware control structures are not immutable snapshots.

UEFI now uses Multiboot2 ACPI tags, admitted by `Multiboot2_Info` into an owned
snapshot before allocator setup. The existing module-lifetime, framebuffer and
boot-map policies are shared with BIOS. Active EFI boot services are rejected;
GRUB must terminate them. The BIOS path scans only the standard EBDA/ROM windows,
checking the version-specific RSDP length and checksums. No RAM-wide signature
search is used. QEMU/OVMF reaches the desktop and runs native apps; NUC hardware
validation remains outstanding.

The native ACPI adapter checks backing against the retained boot map and admits
SDT lengths/checksums before use. Ordinary allocatable RAM is rejected as table
backing. RSDT entries are read at their actual 32-bit width, MADT subrecords are
bounded before access, and short/misaligned root/MCFG tables fail closed. These
raw adapters remain trusted Ada, not newly proved SPARK code. Firmware tables
are assumed immutable during boot; complete immutable child-table snapshots,
all semantic validators, and device-register resource admission remain work.

The userspace DMAR metadata candidate now provides bounded decoding of forms
0–6 and variable-length device scopes/paths, with authenticated read-only queries
19–23. Unknown records and scopes stay explicit. Full-table structural validation
precedes replies; addresses are signed-safe split words, not mapping permissions.
Unknown-record scope counts describe decoded scopes only. ANDD names are bounded
raw bytes accessible through existing table reads, not admitted namespace objects.
Hosted release and checked query/routing tests pass; current-source proof and
live startup remain pending. This does not implement VT-d resource admission.

For VT-d, admit topology and explicit hardware resources next; full AML is not
a prerequisite. Isolation must account for hardware topology, reserved
regions and devices that cannot be separated. Teardown must quiesce DMA and
complete invalidations before releasing pinned buffers. Fault reporting should
identify the device and affected domain without exposing unrelated memory.

The [focused tests](../tests/acpi-tables/README.md) distinguish pure parsing
evidence from the still-unimplemented raw-memory adapter and native integration.

### Snapshot and hardware access rules

Use a validated, kernel-owned snapshot of immutable description tables or an
explicitly lifetime-managed read-only grant. Shared mutable firmware structures
and hardware registers need separate, coordinated access; copying them does not
replace their live semantics. Table lengths, checksums, references and resource
budgets must be validated before namespace publication. A checksum detects
corruption, not malicious firmware or authenticated provenance.

The ACPI service owns a typed namespace and bounded AML execution state. Other
services request typed operations for device enumeration, resource descriptions,
battery/thermal information and power transitions. Expose only granted
interfaces to consumers; ordinary applications should not get arbitrary AML
method evaluation or raw OperationRegion access.

AML definition blocks create a namespace containing static objects and control
methods. Evaluation can include memory and I/O operations; AML is not simply
CCL bytecode with different opcodes. Implement its own value/coercion, namespace,
serialization and synchronization semantics. The [ACPI software programming
model](https://uefi.org/specs/ACPI/6.6/05_ACPI_Software_Programming_Model.html)
is the semantic reference, not a requirement to reproduce another OS's internal
architecture.

OperationRegion access is mediated through existing device authorities/handles
and narrowly scoped adapters for memory, ports, PCI and embedded-controller
transactions. Validate address range, width, permitted operation and ownership;
a range named by firmware is evidence to review, not an automatic grant.
Serialize conflicting hardware transactions with native drivers. Avoid
per-instruction IPC: execute pure AML locally and cross the boundary for actual
hardware operations. Any batching must preserve ordering and side effects.

Some platform operations inevitably have broad consequences. A broker or service
authorized to reset hardware, control power or program DMA remains part of the
trusted computing base for those effects. Userspace memory isolation does not
contain arbitrary DMA without appropriate hardware isolation. A crashed ACPI
service can still affect availability, and restarting it does not undo register
writes or prove that interrupted power transitions are recoverable.

## SPARK and failure handling

Build the decoder, namespace storage and interpreter core as pure/isolated SPARK
components wherever possible. Separate hardware access and OS integration from
the core, rather than hiding unverifiable operations in falsely annotated units.

Start by proving input/output bounds, checked namespace references, package and
buffer extents, stack push/pop invariants and explicit failure publication.
Then prove individual supported opcode semantics and ownership transitions.
Use specific types and sound state transitions before adding helper contracts;
keep proof-only bookkeeping Ghost and absent from release code.

Apply explicit budgets to table size, namespace allocations, recursion/call
frames and executed instructions. Account separately for elapsed waits, event
storms and synchronization. Yield/suspend through the scheduler rather than
busy-waiting. Bounded interpreter execution is not proof that arbitrary firmware
methods terminate successfully or meet hardware timing requirements.

Kernel capture now measures the sealed catalog and allocates an owned snapshot
with exactly that table count, total payload and largest-table capacity. Its
allocation, including metadata, must fit the configured maximum buddy block
(currently 32 MiB); catalog count remains bounded at 256. This is allocator
policy, not an ACPI format limit. Native service startup still selects prototype
defaults of 64 KiB per table, 32 tables and 1 MiB total pending startup/grant wiring. Exceeding a budget
fails the snapshot/import rather than truncating a table or admitting a prefix
as a complete table set. Essential kernel ACPI setup can succeed while this
userspace handoff is unavailable.

The shared snapshot now supports runtime capacities and packs tables into a
byte allocation, rather than reserving a fixed payload slot for each table.
Hosted fixtures cover 39 tables and a single table larger than 1 MiB. The
service core and bootstrap likewise accept construction-time capacities; their
hosted fixtures retain a table larger than 1 MiB and complete a 35-table
bootstrap. The request core and native grant adapter now enforce the selected instance
capacities, and capacity metrics report those same values. Native startup still
constructs a statically constrained default instance; discovered-size allocation
is not connected. A >1 MiB grant passed the hosted native-adapter fixture,
including failed-return retry and owned-copy readback; kernel acquisition was
mocked. All 108 request/endpoint proof checks passed (86013), with no unproved checks
or assumptions.
The service/bootstrap capacity contracts and request/endpoint callers passed
SPARK proof (40571); this does not prove the remaining allocation/grant wiring.

Before general deployment, use allocations and grants sized at runtime from validated firmware lengths, subject
to an explicit resource quota. The operator should not have to predict firmware
table sizes. Round owned allocations up to pages and zero padding before
read-only grant publication. Report required table count,
largest table and total bytes when admission exceeds that quota. This work is
not implemented end to end yet. Increasing a constant alone requires reviewing
kernel snapshot storage, service state copies, scratch/stack usage and proof bounds;
it must not imply wider register capabilities or physical-memory access.

Timeouts are not transactional rollback. Record the failed method, device,
operation, reason and any known completed effects; quarantine the affected
operation/device when necessary and use an explicit recovery policy. Thermal
and power failures need a conservative platform-specific safety response, not
blind retries or a claim that all methods can be cancelled.

## Sleep and resume orchestration roadmap

User direction (2026-10-20): support pluggable CCL handlers for policy-level
ACPI notifications. A handler may decide to request sleep; the power service
owns the coordinated transition. This is planned work, not implemented or
validated sleep support. Track completion under S04/S05 in
[the coverage checklist](acpi-coverage.md).

- [ ] Deliver typed, queued notifications to authorized CCL handlers, with
  device identity, state, timestamps, execution limits and explicit queue-loss
  behavior. Keep interrupt acknowledgment, EC servicing and mandatory AML event
  processing below the scripting boundary. Thermal emergency handling must not
  depend on an available or responsive script. Event delivery grants no new
  hardware authority.
- [ ] Define an authenticated sleep request and a power-coordinator state
  machine: prepare, commit, resume and abort. Serialize transitions, validate
  supported modes and wake sources, and track completed preparation steps.
  Use bounded acknowledgments; avoid synchronous peer callback cycles.
- [ ] Coordinate drivers and services before commit: drain work, flush storage,
  stop new submissions, quiesce DMA and save device state. Keep dependencies
  needed for AML, hardware access and coordination available until their final
  use. Specify dependency ordering and bounded failure handling.
- [ ] Have the ACPI service obtain supported sleep-state encodings, perform
  required preparation/wake methods (including applicable _PTS/_WAK handling),
  and configure authorized wake sources. Define the exact platform-dependent
  ordering with device preparation; evaluating _PTS alone does not enter sleep.
- [ ] Keep the final CPU/context, resume-entry and hardware-commit mechanics in
  a narrow kernel operation. Resolve explicitly authorized register IDs to
  addresses internally, restrict fields/values, and bind commit to a valid
  prepared transition. Register-write authority alone must not bypass the
  preparation protocol; never accept arbitrary physical addresses from CCL or
  firmware as authority. Coordinate any kernel/devmgr/procmgr changes before
  implementation.
- [ ] Implement abort before commit and recovery after wake: unwind completed
  preparation where possible, restore kernel/device/service state in dependency
  order, perform ACPI wake processing, and publish completion/failure/resumed
  notifications. Record known effects; a timeout does not undo firmware or
  hardware writes. Handle wake events racing with sleep entry.
- [ ] Select an explicitly supported backend: S3 suspend-to-RAM, low-power idle,
  or hibernation. Do not assume every machine supports S3. Hibernation requires
  separate durable image creation, validation and restore machinery; it is not
  implemented by a sleep-register write. Keep unsupported modes explicit.
- [ ] Validate the coordinator with hosted failure injection and policy proofs;
  test denied/duplicate/stale requests, preparation timeout, rollback, wake
  races and resume ordering. Then validate supported transitions in QEMU and
  repeated cycles on actual hardware. Record hosted, proved and hardware-tested
  evidence separately; an AML Event opcode test is not a system-sleep test.

Specification reference: [ACPI 6.6 Waking and Sleeping](https://uefi.org/specs/ACPI/6.6/16_Waking_and_Sleeping.html).

## Incremental roadmap

1. Audit current table overlays and dependencies; admit bounded table snapshots
   through a narrow bootstrap adapter and pure parsers.
2. Build a Linux-hosted SPARK AML decoder/namespace test target in Nix. Start
   with data objects and pure methods, with no hardware authority.
3. Add checked execution and differential tests using generated ASL fixtures and
   tools from [ACPICA](https://github.com/open-acpica/acpica), plus malformed and
   adversarial input tests. A reference implementation is useful evidence, not
   the specification or proof of correctness.
4. Integrate a native userspace service and scoped hardware-operation adapters;
   test denied accesses, concurrent drivers and interrupted transactions.
5. Add event, thermal and power support incrementally on QEMU and the laptop.
   Make firmware quirks narrowly matched, versioned, auditable and visible in
   diagnostics; unsupported methods must fail explicitly.

Open decisions: exact bootstrap handoff schema, broker ownership, AML support
milestones, shared firmware synchronization, safe failure/restart policy and
how to authorize firmware-described resources without granting broad access.

This work follows boot-map correctness; it is not part of the current allocator
proof. SPARK can establish implementation properties under explicit assumptions,
not that firmware describes real hardware truthfully or that every hardware
operation is harmless.

The first userspace decoder milestone and concrete authority/event plan are in
[the ACPI service contract](acpi-service-contract.md); it is hosted groundwork,
not a running service or complete interpreter.

Additional ACPICA probes confirm missing metadata-value behavior in DerefOf,
CopyObject and root/Device package members. Those functionality gaps remain
open; rejecting metadata data-cell writes does not establish complete metadata
value semantics.


The opt-in `capture` hosted selection passed 13,798 checks in each profile on the integrated operand-capture and region-selector source. Separate compatibility validation completed through preserved runs: all 26 adapter compilation stages and five runtime mains per profile, including the original checked typed test (26,138 checks in 74.477 seconds under an explicit one-run 120-second exception). Earlier 30/60-second typed timeouts remain evidence; portable limits stay unchanged. Release bootstrap counters are not assertion coverage; checked bootstrap assertions passed separately. See `tests/aml-capture/README.md` for predecessor and carried evidence scopes. Complete owner sharing/Store width behavior, whole-interpreter proofs and native integration remain unfinished. Logger behavior is unchanged.

### Startup device observations (hosted candidate)

Snapshot `Complete` still means ordered table execution and member binding.
Device initialization has a separate outcome. Root and system-bus `_INI` execute
once using the startup fuel remaining after module execution. The current
service has no region provider: the barrier reports unavailable and device
initialization remains incomplete. This does not claim device presence, region
connection, or hardware activation. Returned initializer objects are discarded.

`Read_Metrics` retains `[page, 0, 0, 0]` requests and revision in reply word zero.
Observer/provider authorization and all existing pages remain unchanged.
New bounded pages return these three payload words:

| Page | word 1 | word 2 | word 3 |
|---|---|---|---|
| 9 | device phase | region outcome | error count |
| 10 | total startup charge | device charge | module charge |
| 11 | status method calls | initializer calls | first error node |
| 12 | first device initialization execution error code | 0 | 0 |

Device phases are explicitly encoded 0 not started, 1 preparing, 2 awaiting
regions, 3 walking, 4 completed, 5 completed with errors, 6 incomplete, 7 failed.
Region outcomes are 0 connected, 1 confirmed no regions required, 2 unavailable,
3 initialization failed. Error codes use the explicit `Startup_Error_Code`
representation in `acpi_requests.ads`; they do not depend on internal enum order.
A first-error field with error count zero is a default, not an error observation.
Unknown pages and nonzero reserved request words are malformed. Queries never
advance revision or change initialization state. Hosted evidence and source
contracts do not establish native execution or SPARK proof of the coordinator.

Pages 9–12 describe device progress; retained completion pages 13–16 are specified below.
When those earlier phases fail, device phase remains not started and its error
fields remain defaults. The existing snapshot status remains authoritative.

### Retained completion metrics

The pages below extend the existing authenticated `Read_Metrics`
request, `[page, 0, 0, 0]`, with response `[revision, a, b, c]`. Reading does not
execute or retry startup, advance the revision, or change upload state. Pages
0–12 retain their meaning.

| Page | a | b | c |
| --- | --- | --- | --- |
| 13 | Finish stage | Freeze outcome | Execution phase |
| 14 | Firmware preparation | Execution access status | Member access status |
| 15 | Module outcome | Completed table count | Failed catalog table ID |
| 16 | Module failure code | 0 | 0 |

Wire values are explicitly assigned, independent of internal enumeration order:
- Finish: not attempted=0, freeze failed=1, execution failed=2, members failed=3, finished=4.
- Freeze: frozen=0, wrong phase=1, empty=2.
- Execution phase: not started=0, preparation failed=1, module report=2.
- Preparation: not attempted=0, rejected=1, prepared=2, namespace capacity=3, value capacity=4.
- Access: available=0, invalid value=1, uninitialized element=2, wrong kind=3, out of bounds=4, root limit=5, identity exhausted=6, busy=7, wrong phase=8.
- Module: catalog rejected=0, completed=1, rejected=2.
- Module failure uses the existing explicit startup error codes (including No_Return=4, Unsupported=6, Budget_Exceeded=9). Mutex_Not_Acquired appends code 40; existing codes 0 through 39 are unchanged.

Read page0's overall phase together with these pages. Premature finish or an
upload failure can leave completion at **not attempted** even though the session
has failed. Default Catalog_Rejected, Wrong_Phase and No_Return fields describe
an unattempted report; they are not independent evidence of a failed execution
or successful startup. Available means a report was obtained, not that its
module succeeded. Successful module completion also retains No_Return; phase
and module outcome qualify it. Module fuel remains page10 word3. Failed table
ID is a lifetime-local catalog identifier, never a PID, address or capability.
Snapshot completion remains separate from device/region readiness.

The private hosted gate passed all three release mains: completion metrics 732,
existing startup metrics 57 and endpoint 129 checks (918 total). The checked
completion main compiled but reached its unchanged 30-second runtime limit; the
other two checked mains subsequently passed in a separate bounded gate
(startup 57 and endpoint 129). This is functional release
evidence with incomplete supplementary checked coverage, not a proof or native
validation. The relocated portable completion test separately passed all 732 checks in
release mode. This single selection is not an all-24-suite rerun.
No transport, subscription, grants, native startup binding, region provider or
logger behavior changed.


## Retained Notify polling

Authenticated Snapshot_Provider requests own the bounded polling receipt; Observer
requests can only inspect it. Labels24–28 configure startup interest, claim one
FIFO event into a retained receipt, read copied metadata/path, acknowledge the
exact nonzero receipt, and inspect state. Retry of a lost claim response returns
the same receipt. Duplicate old ACK never clears a newer receipt. No payload PID
or path grants authority. Current_Target records dequeue-time authentication,
not fresh authority to invoke a node later.

Pre-INI interest is applied only after module completion/Ready. Module Notify
still evaluates its target and value, including effects and errors, while queue
admission remains disabled. The module entry guard requires a disabled empty
queue. Module event delivery and asynchronous SCI/event push remain future work.

Private hosted validation passed607checks in each release/checked profile across
six mains. Concrete receipt-policy instances0/2/default separately proved their
Issue/ACK contracts; this does not prove the full service. Native reply failure
is represented by direct endpoint retry tests, not native transport execution.
Portable registration and active-manifest repins are not reruns of predecessor
suites; historical validation records remain immutable.

Portable release polling379 and modulepolicy68 additionally passed via the new
relative-path runner; no full-six portable rerun is implied.
