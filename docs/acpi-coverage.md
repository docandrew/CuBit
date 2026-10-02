# ACPI service coverage and completion checklist

Owner: the main ACPI implementation agent. This is its persistent completion
ledger, alongside [architecture](acpi-userspace.md), the
[service contract](acpi-service-contract.md), and
[working coordination note](../coordination/acpi.md).

The goal remains a userspace ACPI service that moves as much functionality as
possible out of the kernel, including a SPARK-verified, semantically correct AML
interpreter. This checklist does not redefine that goal around the current subset.
It was seeded from the current checkout and recorded evidence; it is not a new
proof run, a complete specification audit, or a claim of deployment.

## How the agent should maintain this file

- Before selecting work, check current source, outstanding process handles and
  ownership in the coordination note. Prioritize a real compatibility blocker.
- Update the relevant row after a completed work chunk. Record source/test paths,
  command, source revision or hashes, terminal outcome and precise proof scope.
  Keep live jobs and transient session IDs in the coordination note.
- Track implementation, SPARK proof and integration separately. A successful
  parser or direct helper call does not complete an AML opcode or platform feature.
- Do not mark a row complete until every listed semantic case and its completion
  evidence pass. Split a row when its parts acquire different statuses; retain IDs.
- Preserve negative results, unsupported cases and unrun tests. A capability
  model is not live enforcement; a native link is not a successful service boot.
- Keep all unclassified tables/features visible. Architecture-specific or obsolete
  features need an explicit applicability decision; absence on one test machine
  does not make them implemented or remove them from the full inventory.

Status columns: **I** = implementation, **P** = proof, **T** = integration/testing.
`partial` means only a subset is evidenced; `pending` means work remains;
`audit` means current support has not been established; `complete` requires the
row's stated evidence. No whole feature family is certified complete by this seed.
All unchecked boxes below are remaining work, including rows with useful partial code.

## Next milestones, in dependency order

- [ ] **N1 — Close the active namespace-field work.** Verify the final region/field
  binding source, atomic rejection, method ownership and cleanup, catalog binding,
  object types, bit reads and native build. Do not promote the active proof note
  to a passed result without inspecting its terminal outcome. See A08/A09, V01.
- [ ] **N2 — Execute DataTableRegion and Field through AML.** Connect declaration
  evaluation and field-value lookup to retained tables. Exercise the real AML path,
  not only direct service calls. Resolve the ASLTS startup blocker. The executor
  now accepts an explicit limited read-only context and distinguishes lookup
  inspection from evaluation (1811 new checks; 418 core proof checks, zero
  unproved). Service.Invoke now evaluates API-bound table fields, including wide
  AML buffers. Method-time Field declarations now execute with bounded staging
  and cleanup (1523 declaration +978 boundary checks; 54 ACPICA comparisons).
  DataTableRegion, module-level Field loading, other access forms and value
  reclamation remain pending. Literal object materialization is now integrated
  (160 hosted cases, 70 ACPICA comparisons; focused executor/service proof
  3046 proof +414 flow checks, zero unproved/justified). Dynamic BufferSize,
  string-coercion integration, decoder bounds and arena reclamation remain.
  Pure implicit string conversion is now registered (65542 hosted checks;
  94 proof +4 flow checks; native unit compile). Fifty ACPICA values agree,
  with a control-reproduced shutdown allocation diagnostic recorded separately.
  These primitives do not yet execute DataTableRegion. See A08–A11.
- [ ] **N3 — Remove prototype capacity as a compatibility blocker.** Make bounded
  storage provisioning suitable for larger firmware and report required capacity.
  Selected ASLTS control tables already exceed the 64 KiB per-table limit. See B03.
- [ ] **N4 — Re-run and expand upstream ASLTS.** After each blocker is resolved,
  identify the next unsupported behavior from the actual run and update this file.
  Preserve full-suite completion as a separate gate. See V03.
- [ ] **N5 — Finish authenticated startup and table delivery.** Coordinate shared
  kernel/process-manager ownership; boot and query the real service. See S01–S03.
- [ ] **N6 — Connect explicitly authorized hardware effects and events.** Complete
  platform admission/enforcement before enabling AML region access. See H01–H06.

N2/N3 and independent integration work may proceed concurrently only within the
repository's ownership/build rules. This list does not authorize new agents or
changes to another agent's claimed files.

## Table transport, admission and storage

| ID | Remaining work / definition of done | I | P | T | Evidence / dependencies |
| --- | --- | --- | --- | --- | --- |
| B01 | Complete RSDP/root/child discovery across supported boot paths; validate all lengths, checksums, entry widths, duplicates, address arithmetic and backing lifetimes. | partial | partial | partial | `shared/firmware/firmware_tables.*`, catalog/exposure/backing tests; kernel boot adapters still require separate trust review. |
| B02 | Complete immutable owned snapshots and service delivery; preserve original reservations until all kernel/userspace consumers are safe; no writable firmware-page exposure. | partial | partial | partial | `firmware_tables-snapshots.*`, `tests/aml-core/snapshots/`; kernel snapshot boot tests are not service-delivery tests. Depends on S02. |
| B03 | Replace fixed development capacities with runtime-sized owned buffers/grants computed from validated firmware lengths, explicit resource quotas and capacity diagnostics; test oversized real tables, aggregate exhaustion, count exhaustion and allocation failure without truncation. | partial | partial | pending | Shared snapshot now has runtime capacities and packed storage: hosted 2680 checks (39 tables, >1 MiB single table), 95 snapshot proof checks with zero unproved/Assume. Catalog length and page-count metadata bounds widened. Service/bootstrap now accept explicit runtime capacities (hosted service 1051688/bootstrap 1314 and native link pass; capacity proof session 40571 passed with no unproved/Assume). Request/grant adapters now enforce per-instance capacities and report them through metrics; hosted block fixture passes 150 checks including >1 MiB grant and 35-table readback, with kernel acquisition mocked. All 108 request/endpoint proof checks passed (86013), zero unproved/Assume; final request boundary fixtures passed 92680 checks and native link passed. Discovered-size planner `Firmware_Tables.Provisioning` now proves exact total/maximum and quota decisions (29 proof checks, zero unproved/Assume; 1795 hosted cases). It preserves wide requirements on rejection and excludes incomplete catalogs. Kernel capture now reserves a checked buddy allocation before typed construction, sizes count/payload/largest from the catalog, and removes the scratch buffer. Private rebuilt kernel passes both Multiboot protocols with normal inventory, 40 tables/1,329,227 bytes, count rejection, late-table rejection, and forced allocation failure (10 cases). Kernel allocation is bounded by the configured maximum block including metadata (currently 32 MiB); the raw adapter remains trusted native code. Native userspace startup still defaults to 64 KiB/table, 32 tables, 1 MiB aggregate; startup allocation/grant wiring remains pending. Rebuilt kernel with default budget passes capture and two rejection cases under both Multiboot protocols (six cases, session 43286). These are not ACPI limits. User requests sizing from the machine rather than advance manual capacity selection; a published T490s DSDT is 0x2396A bytes (142.35 KiB), already above the per-table cap. [Primary boot log](https://github.com/katakombi/LinuxMint-t490s). This example is not a population survey or evidence of typical totals over 1 MiB. Review method/value/namespace/result budgets together; simply raising static arrays can exhaust stacks. |
| B04 | Complete table identity, revision, DSDT-selected integer width, ordering, duplicate policy and cross-table reference behavior. | partial | partial | partial | `ACPI_Service`, `Firmware_Tables.Identifiers`; DSDT/SSDT subset loading and retained-table matching tests. |
| B05 | Define immutable snapshot versus dynamic table-load lifetimes; ensure fields/handles cannot outlive or silently refer to reused storage. | partial | partial | pending | Current lifetime-local table indices and namespace bindings; depends on A17 and S03. |
| B06 | Specify unknown/vendor-table retention, diagnostics and consumer routing without treating descriptions as hardware authority. | partial | partial | pending | Generic Description-table admission is not semantic support. |

## Table-specific interpretation inventory

For each table: inspect all revisions/subtable types; decide the consuming
service and unavoidable early kernel dependency; implement bounded decoding;
prove length/index/arithmetic/representation properties; compare independent
fixtures; test the actual consumer. Record these separately from generic SDT
retention. FACS and resource structures with different formats must not be
forced through the ordinary immutable SDT path.

| ID | Table / structure | I / P / T | Remaining work and starting evidence |
| --- | --- | --- | --- |
| T-RSDP | RSDP | partial / partial / partial | Root discovery and revision/checksum handling exist in shared/kernel code; close boot-path and raw-memory boundary audit. |
| T-RSDT | RSDT | partial / partial / partial | Root entry walking exists; close lifetime/consumer split and malformed-reference coverage. |
| T-XSDT | XSDT | partial / partial / partial | Same as RSDT, including 64-bit address and bounds cases. |
| T-DSDT | DSDT | partial / partial / partial | Admission and partial AML loading exist; completion requires the AML semantics and initialization gates below. |
| T-SSDT | SSDT | partial / partial / partial | Partial AML loading exists; finish ordering, cross-table namespace behavior and dynamic loading semantics. |
| T-FACS | FACS | pending / pending / pending | Deliberately excluded from immutable standard-SDT import. Design mediated waking-vector/global-lock access, ownership and resume lifecycle. |
| T-AEST | AEST | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-AGDI | AGDI | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-APMT | APMT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-ASF | ASF | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-ASPT | ASPT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-BDAT | BDAT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-BERT | BERT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-BGRT | BGRT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-BOOT | BOOT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-CCEL | CCEL | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-CDAT | CDAT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-CEDT | CEDT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-CPEP | CPEP | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-CSRT | CSRT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-DBG2 | DBG2 | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-DBGP | DBGP | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-DMAR | DMAR | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-DRTM | DRTM | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-DTPR | DTPR | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-ECDT | ECDT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-EINJ | EINJ | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-ERDT | ERDT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-ERST | ERST | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-FADT | FADT | partial / partial / partial | FADT (wire signature FACP): userspace decoder and register-description helpers exist in `acpi_fadt*`; finish revision/flag audit and live fixed-hardware consumers. Metadata is not authorization. |
| T-FPDT | FPDT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-GTDT | GTDT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-HEST | HEST | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-HMAT | HMAT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-HPET | HPET | partial / partial / partial | Shared `Firmware_Tables.HPET` decoder and early kernel timer consumer exist; audit remaining fields/revisions and division of service responsibilities. |
| T-IORT | IORT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-IOVT | IOVT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-IVRS | IVRS | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-LPIT | LPIT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-MADT | MADT | partial / audit / partial | MADT (wire signature APIC): early kernel parsing exists; inventory every interrupt-controller/subtable form, prove bounded decoding, and keep only unavoidable bootstrap consumers in kernel. |
| T-MCFG | MCFG | partial / audit / partial | Early kernel PCI configuration discovery exists; audit segment/bus/range semantics and connect enumerated PCI authority to the appropriate service. |
| T-MCHI | MCHI | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-MPAM | MPAM | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-MPST | MPST | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-MRRM | MRRM | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-MSCT | MSCT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-MSDM | MSDM | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-NFIT | NFIT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-NHLT | NHLT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-PCCT | PCCT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-PDTT | PDTT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-PHAT | PHAT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-PMTT | PMTT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-PPTT | PPTT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-PRMT | PRMT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-RAS2 | RAS2 | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-RASF | RASF | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-RGRT | RGRT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-RHCT | RHCT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-RIMT | RIMT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-S3PT | S3PT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-SBST | SBST | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-SDEI | SDEI | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-SDEV | SDEV | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-SLIC | SLIC | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-SLIT | SLIT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-SPCR | SPCR | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-SPMI | SPMI | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-SRAT | SRAT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-STAO | STAO | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-SVKL | SVKL | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-SWFT | SWFT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-TCPA | TCPA | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-TDEL | TDEL | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-TPM2 | TPM2 | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-UEFI | UEFI | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-VIOT | VIOT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-WAET | WAET | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-WDAT | WDAT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-WDDT | WDDT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-WDRT | WDRT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-WPBT | WPBT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-WSMT | WSMT | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-XENV | XENV | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |

The rows above include every `ACPI_SIG_*` entry in the pinned ACPICA
`source/common/dmtable.c::AcpiDmTableData` registry, plus explicit roots, AML
tables and FACS. Registry names are symbolic names, not always literal wire
signatures. This is a reproducible starting inventory, not a claim that every
ACPI-defined/vendor structure appears there or belongs on every architecture.
CDAT and similar separately delivered structures need their own transport review.

- [ ] **T-AUDIT:** Reconcile the inventory against the chosen normative ACPI
  revision, companion specifications, supported architectures and target firmware.
  Add omitted/deprecated/vendor tables with a documented applicability decision.
- [ ] **T-OWNERS:** For every required table, record its userspace consumer, any
  early-kernel subset that must remain, and the end-to-end acceptance fixture.

## AML semantic coverage

Each row must be expanded into opcode × operand type × target/reference kind ×
32/64-bit mode × normal/error/lifetime cases before claiming semantic coverage.
For every applicable case require functional contracts, runtime-safety/termination
proof, malformed-input tests and ACPICA differential or normative expected results.
The table below records families, not an audited opcode denominator.

| ID | Feature family / remaining semantics | I | P | T | Starting evidence / dependency |
| --- | --- | --- | --- | --- | --- |
| A01 | Integer/string encodings, names, package and field lengths: close full grammar, truncation, reserved bits, null names, name resolution and failure precedence audit. | partial | partial | partial | `AML_Decode`, `AML_Names`, `AML_Fields`; pure parsing does not execute declarations. |
| A02 | Name, Alias, External, Scope, Device, Processor, PowerResource, ThermalZone: complete declaration/lookup rules, initialization order, forward references and conflicts. | partial | partial | partial | `AML_Namespace.Load_Names` handles a subset; audit each missing declaration separately. |
| A03 | Integer arithmetic/bitwise/logic: complete operand conversion and target semantics, including Increment/Decrement and all comparison types. | partial | partial | partial | `AML_Integers`, `AML_Logic`, evaluator; proved integer kernels do not prove all AML operand forms. |
| A04 | If/Else, While, Break, Continue, Return, Noop: close evaluation order, nesting, side effects, errors and all supported datum kinds. | partial | partial | partial | Branch/loop and ACPICA comparisons; budgets remain explicit. |
| A05 | Method calls, argument/local semantics, recursion and serialized methods: complete reference arguments, implicit conversions, reentrancy, yielding and concurrency. | partial | partial | partial | Synchronous integer-oriented calls and serialized ordering exist; no general concurrent AML scheduler. |
| A06 | Store and CopyObject: all source/destination types, implicit conversion, aliasing, reference targets, buffer/package mutation and error side effects. | partial | partial | partial | Named integer Store subset exists; do not mark Store complete from integer-only comparisons. |
| A07 | RefOf, DerefOf, Index, CondRefOf and reference identity/lifetime: build general reference model and alias-safe mutation. | pending | pending | pending | Required by A05/A06/A10/A14; absence/partial code must be audited per operator. |
| A08 | DataTableRegion: execute actual declaration operands, lookup/wildcards, errors, namespace ownership and table lifetimes. | partial | partial | partial | Catalog lookup and internal service binding exist; direct helper tests are not opcode execution. Owned-backing lookup is now integrated: 33811 selection checks, 23 proof +4 flow checks with zero unproved/justified; registered integration also passes 847 field-read checks. It rejects malformed inventories and proves first matching index. Namespace binding/accessor proof: 68 checks; service declaration/read adapters: 29 checks, zero unproved/Assume, terminal session 23652. Hosted namespace 351, service 1051624, ACPICA bit reads 80, full hosted regression and private native link passed; active method-owner execution remains untested. |
| A09 | Field, BankField, IndexField: declaration execution, accumulated offsets, access state, lock/update rules, region/selector binding and lifetime. | partial | partial | partial | Field-list framing and immutable table descriptors/readers exist. Service.Invoke now executes AML reads of API-bound fields, using integers or wide buffers, preserving type 5 inspection; 847 hosted checks and 54 ACPICA comparisons pass. Method-time Field now executes AnyAcc/ByteAcc NoLock/Preserve declarations with bounded descriptor staging and atomic failure; 1523 hosted declaration checks, 978 boundary checks and 54 actual-Field ACPICA comparisons pass. Final service proof87954:2458 proof+322 flow, zero unproved/justified. Module-level loading, DataTableRegion, other access forms, hardware effects and reclamation remain. Depends on A08/A10/H01. |
| A10 | OperationRegion and handlers: address-space semantics, deferred expressions, region activation/_REG, bounds, errors and authority. | partial | partial | pending | Policy/transaction models exist; actual AML declarations and admitted hardware handlers remain. Never authorize directly from firmware addresses. |
| A11 | Field reads/writes: integer-versus-buffer results, unaligned/multi-access fields, access widths, Preserve/WriteAsOnes/WriteAsZeros and atomicity/locking. | partial | partial | partial | `AML_Field_Data` proves immutable bit extraction; no general writable field implementation. |
| A12 | CreateField/CreateBitField/CreateByteField/CreateWordField/CreateDWordField/CreateQWordField: buffer aliasing, bounds, writes and lifetime. | pending | pending | pending | Depends on general object/reference model. |
| A13 | Buffer, Package, VarPackage: full runtime creation/evaluation, variable lengths, uninitialized elements, nested references, count expressions and mutation. | partial | partial | partial | Bounded constant loading and some named-count behavior exist; not full runtime semantics. |
| A14 | Concatenate, ConcatenateResTemplate, Mid, Match, SizeOf, ObjectType and package/buffer/string operations: all operand kinds and edge cases. | partial | partial | partial | SizeOf/ObjectType subsets exist; inventory other operations individually. |
| A15 | ToInteger/ToBuffer/ToString/ToDecimalString/ToHexString, FromBCD/ToBCD and implicit conversion: full explicit/implicit conversion matrix and failure behavior. | partial | partial | partial | Integer coercion helpers exist; not a complete conversion system. |
| A16 | Dynamic object ownership: temporary names/data/regions/fields, recursion, error cleanup, references surviving name deletion and storage reclamation. | partial | partial | partial | Temporary Method cleanup exists; region/field binding ownership is active work, not yet a demonstrated complete lifecycle. |
| A17 | Load, LoadTable, Unload and table handles: transactional loading, namespace updates, permissions and handle/reference invalidation. | pending | pending | pending | Depends on B05, A02, A07; immutable bootstrap import is not dynamic AML loading. |
| A18 | Mutex, Event, Acquire, Release, Wait, Signal, Reset: timeout, ordering, recursion, wakeups, cancellation and inter-method concurrency. | pending | pending | pending | Serialized synchronous ordering is only a prerequisite. |
| A19 | Sleep, Stall, Timer: timing units, clock behavior, bounded scheduling and cancellation without busy-waiting in inappropriate contexts. | pending | pending | pending | Requires service scheduler/runtime integration. |
| A20 | Notify: queued event semantics, target identity, coalescing/loss policy, subscription authority and delivery. | pending | pending | pending | Depends on S04/H04; not arbitrary IPC from AML. |
| A21 | Revision, Debug, BreakPoint, Fatal and remaining extended operators: enumerate and implement normative observable/error behavior. | audit | audit | audit | Must reconcile against opcode inventory; do not silently skip unsupported instructions. |
| A22 | Module-level execution, deferred initialization, predefined namespace objects/methods and initialization ordering (_INI, _STA, _REG, _OSI/_OS): complete platform policy and semantics. | partial | partial | pending | Loading a namespace does not perform full ACPI initialization. |
| A23 | Resource templates/descriptors and _CRS/_PRS/_SRS/_PRT consumers: parse all applicable descriptors, preserve checksums, validate resources and route to device/IRQ services. | pending | pending | pending | Depends on A13/A14/H01; descriptions cannot grant access. |
| A24 | Error model and resource budgets: atomicity where required, completed effects on failure, stable diagnostics, fuel/recursion/memory limits and recovery. | partial | partial | partial | Existing bounded failures/transactional loaders cover subsets; audit across every new opcode. |

- [ ] **A-AUDIT:** Enumerate every real AML opcode from the chosen specification
  and pinned ACPICA `source/components/parser/psopcode.c`. Map it to a row/cases;
  distinguish encoding aliases, internal pseudo-opcodes and actual instructions.
  Ensure the list includes all extended operators, declarations and operand forms.
- [ ] **A-ORACLE:** For each case record whether the expected behavior comes from
  the specification, ACPICA, or both; resolve discrepancies explicitly. Differential
  agreement alone is not proof of normative correctness.

## Service, hardware and platform completion

| ID | Remaining deliverable | I | P | T | Dependencies / completion evidence |
| --- | --- | --- | --- | --- | --- |
| S01 | Launch acpi.svc with authenticated bootstrap authority, provider capability and restricted cspace; support failure/restart policy. | partial | partial | pending | Linked executable/startup decoder are groundwork. Actual launcher wiring and boot evidence required; coordinate ownership. |
| S02 | Kernel/provider-to-service immutable table delivery using owned copies/read-only grants; complete partial-failure and grant-return lifecycles. | partial | partial | pending | B02; mocks/native compilation are not actual transport. Verify teardown, retries and no replay. |
| S03 | Service-aware AML execution context: retain immutable tables without unsafe references or repeated large stack copies; serialize mutable state safely. | partial | partial | partial | Explicit aliased immutable table backing now reaches the actual namespace evaluator via Service.Invoke. Selected proof session20247 passed 2701 proof +379 flow checks with zero unproved/justified; native link and426 input hashes pass. Service catalog preservation is contracted. Hosted847 field and1811 input checks pass, ACPICA54 field/212 typed/26 dynamic comparisons pass. Declaration opcodes, reclamation, concurrent service integration and complete native stack bounds remain unfinished. A05/A08/A16. |
| S04 | CCL events, metrics and log streams: schemas, authority, subscriptions, loss/backpressure, querying and observability integration. | partial | partial | pending | Scalar query/request core exists; logs and event streams are separate missing deliverables. Coordinate metrics/logging owners. |
| S05 | Device/power policy integration: lid/buttons, battery/AC, thermal/fan, backlight, device power/hotplug and sleep/hibernate/resume orchestration. | pending | pending | pending | AML + H01–H06; assign policy to appropriate services. Hibernate is not necessarily one register write. |
| H01 | Build a kernel-owned inventory of explicitly permitted registers/resources from independently validated platform ownership. | partial | partial | pending | Pure catalog models exist. No arbitrary-address registration by the ACPI service. |
| H02 | Enumerated ACPI/GPIO capability groups and individual IDs: startup grants, attenuation/delegation, revocation and race-safe lifetime. | partial | partial | pending | Shared authority/catalog/grant/cspace models exist; demonstrate live install/use/revoke, including child services. |
| H03 | Kernel-mediated read/write enforcement: allowed operation, width, bit masks, exact extent, side effects and hardware ordering. | partial | partial | pending | No caller-supplied address/offset/width that broadens authority. Prove and adversarially test live syscall/backend path. |
| H04 | SCI/GPE/fixed events: interrupt routing, acknowledge/mask/re-enable semantics, event AML, storm control and subscriber delivery. | pending | pending | pending | Requires H03/A18/A20/S04; test real event lifecycle and failures. |
| H05 | Address-space backends: SystemMemory, SystemIO, PCI_Config, EC, SMBus/GenericSerialBus, GPIO and other applicable spaces. | partial | partial | pending | Review each handler separately; no blanket R/W mappings. Coordinate existing drivers. |
| H06 | Global lock, FACS waking vectors and platform transition sequencing; preserve kernel-owned safety invariants across suspend/resume. | pending | pending | pending | T-FACS, A18, S05; boot/resume/physical-hardware evidence required. |
| S06 | Minimize the kernel ACPI role and document retained early-boot dependencies; remove superseded walkers only after replacement consumers work. | partial | partial | pending | Per-table owner map T-OWNERS; no in-kernel AML fallback. |

## Verification and release gates

- [ ] **V01 — SPARK:** Prove the current source, not merely a prior cached aggregate.
  Preserve exact functional contracts as well as bounds/initialization/termination.
  Inventory every SPARK-off/native boundary and assumption. Each proof report must
  identify analyzed units and source hashes/revision; pending proofs are not passes.
- [ ] **V02 — Hosted/reference tests:** Keep `tests/aml-core/run.sh --prove --acpica`
  comprehensive as features land. Include malformed input, both AML integer widths,
  budget exhaustion, lifetime/alias cases, and independent expected-value oracles.
- [ ] **V03 — Upstream ASLTS:** Run and eventually pass the required full inventory,
  including currently unselected entrypoints. Pin the source, retain per-case reasons
  and fail the completion gate for unsupported or unrun required cases.
- [ ] **V04 — Table parser tests:** Expand ACPICA/iASL and FWTS fixtures per table,
  including versions, optional tails, subtables, malformed extents and cross-links.
  Checksum tests do not certify table-specific semantic decoding.
- [ ] **V05 — Native/QEMU:** Boot the real service through both relevant boot paths,
  exercise authenticated table transport, CCL queries/events, kernel-mediated I/O,
  capability denial/revocation, service failure and resource exhaustion.
- [ ] **V06 — Firmware corpus/physical hardware:** Collect representative real tables
  and test supported machines, including laptop lid/backlight/power/resume behavior.
  A synthetic DSDT or QEMU-only pass does not establish real-firmware compatibility.
- [ ] **V07 — Resource/security audit:** Bound stack usage across whole call chains,
  allocations, interpreter fuel and concurrent work; fuzz untrusted parsers/IPC;
  test that rejected AML cannot access unrelated memory or enlarge its authority.
- [ ] **V08 — Final goal audit:** Reconcile every row with current evidence, keep
  remaining platform/scope decisions explicit, and verify the full original goal.
  A green subset test command does not complete the service or interpreter.

## Evidence baseline and reporting percentages

The latest retained upstream report inspected when seeding this file is
[`tests/aml-core/build/aslts/report.json`](../tests/aml-core/build/aslts/report.json):
0 configurations passed, 12 unsupported, 0 unexpected failures, and 339 entrypoints
not run. The 12 configurations are not 12 unique opcodes. Eight are blocked at
DataTableRegion setup; four control configurations exceed the per-table capacity.
The build report is generated evidence and may not be tracked; reproduce it via
the pinned runner and preserve a durable result artifact when claiming progress.

Existing focused reference comparisons, proofs and hosted checks are described
in [the test ledger](../tests/aml-core/README.md). Those results establish their
stated subsets only. The active namespace binding work in the coordination note
must be reconciled with terminal proof results before updating its proof status.
No jobs were resumed or stopped to create this checklist.

Do not publish a single percentage until T-AUDIT and A-AUDIT establish denominators.
After that, report separately:

1. Table semantic coverage: fully interpreted required table revisions/subtypes
   divided by the audited required inventory; report retention coverage separately.
2. AML semantic coverage: completed operand/target/error/lifetime cases divided
   by the audited case inventory. Report partial cases without arbitrary half-credit.
3. Independent conformance: passed, unsupported, failed and unrun ASLTS cases,
   using consistent configuration/entrypoint units and a pinned suite version.
4. Deployment: explicit integration gates passed, with the tested platforms listed.

Always show the counts, denominator definition, source revision and exclusions.
Changing applicability or splitting rows must not manufacture an apparent gain.
