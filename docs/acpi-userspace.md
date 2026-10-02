# ACPI and AML outside the kernel

Track remaining work and completion evidence in the
[ACPI coverage checklist](acpi-coverage.md).

Status: shared table admission is integrated into native BIOS/UEFI boot. A
SPARK-analyzed userspace service core and partial AML interpreter have hosted
validation; no live userspace ACPI service or IOMMU enforcement is deployed.
See [current service core](../userspace/services/acpi/README.md) for implemented
interfaces and remaining integration work.

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

For VT-d, admit DMAR and its variable-length device scopes next; full AML is
not a prerequisite. Isolation must account for hardware topology, reserved
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
