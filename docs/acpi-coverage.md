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

## Current ToString checkpoint (2026-10-09)

ToString is applied to the shared source. Authenticated Integer, String and
Buffer inputs produce a fresh String prefix ending at the requested length,
source extent or first zero byte. Source and length remain rooted through
allocating target evaluation; conversion failures preserve earlier operand
side effects. Explicit conversion now shares its result with a differently
typed named target. A matching String or Buffer target retains its own object
and receives the value. This shared attachment correction also covers ToBuffer.

The [portable ToString group](../tests/aml-tostring/README.md) passes 21,557
focused checks per strict release and assertion-enabled profile: 4,443 owner
and 148 collecting checks for ToString, plus 16,964 existing conversion checks.
Each profile matches all 72 selected executable ACPICA cases with identical
output. Four normal compiler rejections are separate. Ten affected callback
adapters compile in each profile. Root verified the canonical inputs, runtime
artifacts and all 650 installed file hashes.

The corrected reference corpus uses DSDT revisions 1 and 2 with runtime width
witnesses, complete byte observations, identity tests and post-failure markers.
Earlier SSDT revision labels did not establish integer width and are excluded
from those claims. Compiler-warning and reference-import failures are preserved
in the private evidence. No new proof, full ASLTS pass, native deployment or
hardware validation follows from this milestone. Remaining AML semantics and
formal verification, service startup, hardware authorities and events remain
part of the full goal.

## Earlier Mid and conversion checkpoint (2026-10-09)

Mid is applied to the shared source. It produces fresh String/Buffer slices,
accepts width-normalized Integer input, authenticates compound sources and
performs target attachment transactionally. Suspended source, index and count
have separate GC roots, including during allocating dynamic target evaluation.
The [portable Mid group](../tests/aml-mid/README.md) passes 19,593 focused checks
and 56 cached ACPICA comparisons in each strict release and assertion-enabled
profile. Six normal compiler rejections are recorded separately. Ten affected
generic callers compile in both profiles on the identical production parent.
These counts include boundary assertions, not 19,593 distinct scenarios.

The pure AML_Slices helper remains only partly proved: two bounded proof runs
at levels 1 and 2 each discharged five of eight obligations. Two range checks
and the full clamping postcondition remain unresolved. Both runs used unchanged
source and contracts, with no assumptions or skipped checks. Passing slice,
owner and collecting tests do not establish full interpreter correctness.

Earlier applied functionality includes ToBuffer and typed Sleep/Stall requests.
Their canonical hosted gates passed 14,281 focused checks, 88 ToBuffer cached
classifications and 36 embedded delay correspondences per strict profile;
two ToBuffer and ten delay compiler rejections are separate. Pure delay policy
normalization and Unavailable_Provider subsequently discharged all 17 scoped
level-1 obligations and passed 140 boundary checks per profile. This proves
neither elapsed time nor evaluator, provider or scheduler behavior. The native
delay provider remains explicitly unavailable.

Tail conditional frame elision also passed 282 checks and 22 cached comparisons
per profile. These milestones are focused runs on their recorded source, not
one rerun of every historical suite. Full ASLTS coverage, complete AML semantics
and proofs, runtime sizing, live startup, hardware authority and event delivery
remain unfinished. The older checkpoints below retain their original scope.

## Earlier hosted lifetime and capacity checkpoint (2026-10-09)

The guarded source checkpoint includes collecting service execution with opaque
retained results, allocation-boundary root tracing, BCD opcodes, authenticated
String/Buffer-left comparisons with Integer/String/Buffer right operands, and static named Integer/String/Buffer size
conversion. Explicit node, aggregate-method and retained-root budgets preserve
the default configuration and the individual-method limit. Runtime provisioning
from firmware requirements remains unfinished. Integer-left comparison routing remains
unchanged; mixed object comparisons validate live owner storage before conversion.

Portable registered tests pass 507 explicit checks per strict release and checked
profile, plus 33 cached ACPICA integer comparisons and three precise policy
rejections per profile. Two compiler-rejected reference cases are recorded
separately. All 52 original hosted mains compile; this is not a claim that all
were rerun. Earlier default-service validation passed 3392 checks per profile on
the capacity parent, before the narrow static count coercion change. Exact
provenance is in the private registration and parent checkpoint manifests.

Selected release ASLTS arithmetic and logic variants pass; the full upstream
suite does not. A separate admission-only census loads all four selected control
tables in both profiles: 580 nodes, 233 methods and 138768–144973 method bytes.
Those counts describe static admission on its recorded capacity baseline, not
execution headroom or a completed control test. No methods were invoked by that
census. Hardware callbacks, startup, CCL delivery and native stack bounds remain
unfinished.

No new complete interpreter proof is claimed. The scoped original BCD proof
left four of nineteen obligations unresolved; a later unproved lemma did not
close that gap and was not promoted. Current collection, ownership, evaluator
and capacity changes still require milestone proofs. Historical proof evidence
below belongs to its recorded source, not automatically to this checkpoint.

The separately registered [mixed comparison suite](../tests/aml-mixed/README.md)
includes 338 explicit checks per profile: the pure/owner registration passed 112,
and the canonical Buffer-fixture relocation passed 226 in a separate focused run.
The registered 60 cached ACPICA classifications per profile include 58 scalar
matches and two expected empty-Buffer errors. The mixed comparison helper’s private
view-bound annotation discharged all 49 scoped helper obligations, including
runtime bounds and the current failure contract; full ordering semantics is
still tested, not formally proved. This does not prove owner authentication,
collection, or the full interpreter.

## Historical named-store checkpoints (2026-10-09)

The frozen coherent checkpoint `aml-named-store-integrated-f6aojk8w`
passes 27 hosted test mains: **87,189 explicit checks in each strict release
and assertion-enabled profile**, plus **216/216 cached and focused ACPICA
comparisons**. Root verified 314 final input hashes and 392 artifact hashes.
Counts include pool-fill assertions, not that many distinct scenarios. The
checked target comparisons initially raised Storage_Error with the default stack;
the same binaries passed the scoped rerun with the required 64 MiB stack. Failed
reports are preserved. No pre-failure binary hash was recorded; the report states
that evidence limit rather than inventing one.

This integrates Increment/Decrement, compound Local/Arg capture, explicit
ToInteger, construction-specific Name-member identity and named Store conversions.
The original runtime Name fixture is unchanged and passes 494 checks per profile.
Ordinary existing package members retain captured objects; construction-specific
self members use authenticated readonly Name identity. Explicit ToInteger can
replace String/Buffer target types, while ordinary Store converts to their existing
types. Named Package targets and package-element targets remain distinct. These
are tested portions of the semantics, not a claim that Store or AML is complete.

The further frozen descendant `aml-revision-store-integrated-fpl7klin` adds
Revision and passes eight scoped mains: **847,547 checks per strict profile**.
Root verified 320 inputs and 29 artifacts. The 27-main gate above belongs to its
parent and was not repeated for this small merge. Revision reports CuBit's named
interpreter version 1, separately from `_REV`, table revision and ACPICA's version.
Runtime and module-level Name/Package/Buffer use, illegal targets, truncation,
fuel and integer origin were tested. The original decoder fixture now recognizes
a lone extended prefix as truncated; its previous failing expectation is retained.
The earlier decoder proof does not cover the new Revision implementation.

The frozen Concatenate descendant `aml-concat-provenance-4k83zy3p` passes
all 27 retained integration mains in both strict profiles. All seven affected
adapters compile in both original profiles, and four adapter mains pass each.
Its owner/transaction tests pass 4,255 checks in release and assertion-enabled
builds; root found that the latter owner/adapter projects omit explicit overflow
checking and retain `-gnatwF`. The scoped rebuild `aml-concat-strict-verification-ajga43va` now passes
the owner checks and four adapter mains, and compiles all seven adapter bodies
with the required checked flags; root verified identical 106 production inputs. The 27-main checked project correctly uses `-gnata -gnato` without
that suppression. Do not conflate these validation scopes.

The classified comparison gate has 120 matches among 148 comparable observations
per profile; 28 known namespace-load failures remain. A further 12 cached
expression-provenance observations match per profile. Method-returned and
Store/CopyObject-expression package references resolve their element for
Concatenate, while Local-held references remain descriptors. The pure helper
`aml-concatenation-dlt8q0aw` separately passed 3,324 checks per strict profile.
Empty-Buffer indexed-byte oracle output remains inconclusive; no parity claim.
Root verified 318 final inputs, 775 runtime artifacts and the 106-file production
freeze. The private promotion rehearsal `aml-promotion-rehearsal-7w81__k3` adds
eight existing-fixture passes (101,223 checks per strict profile). Two obsolete
fixture expectations were corrected after diagnostics: compound copies beyond
Strings, and the truncated extended target prefix. All 106 production inputs are
unchanged; the two guarded fixture updates and failures are preserved. Shared
production promotion remains pending.

The latest uninstrumented upstream smoke `aslts-concatenate-gate-0usk9yg2`
gets past the earlier unsupported Concatenate and Revision boundary. It now
returns `VALUE_LIMIT` after 6,795 charged operations with the same cached AML
and ACPICA reference. Root verified 319 inputs and 13 artifacts. This is a later
failure, not a complete upstream pass or a performance result; the exhausted
resource is under investigation in a separate diagnostic child.

These checkpoints remain private hosted evidence. Full ASLTS, native service
integration and current interpreter proofs are incomplete. Proofs remain deferred
to major milestones; the limited earlier proof results below do not cover these
new changes.

## Private startup allocation checkpoint (2026-10-09)

`acpi-owned-backend-dl0anm16` constructs the limited request server directly in
owned, precommitted storage through a one-use GNAT simple storage pool. It checks
actual constrained Size/Object_Size, alignment, quotas and address bounds; reserve
and commit failures return explicit status before Ada construction. Failed cleanup
retains the exact reservation for retry. Construction cannot repeat, callback
reentry cannot publish partial state, and published backing lives for the process
lifetime. Unexpected initialization/pool invariant failure remains fatal.

Hosted release and checked runs each pass 10,251 checks (backend 163, layout 8,209,
startup admission 1,879). Actual aligned writable mock backing is used, including
poisoned pages, partial commit/release failures and extreme capacities rejected
before Reserve. The same production source passes 163 handler-free checks with
the native exception/finalization/tasking restrictions applied under the host
runtime. This is not native linking or a test of kernel mappings. Root verified
161 inputs, 123 preserved parent inputs and 18 artifacts. No new proof was run.

The private typed delivery controller `acpi-delivery-6yxmwjwe` now passes 427
checks per strict profile with the actual native grant adapter and mocked kernel
transport. It binds a single constructed server and provider configuration,
checks epoch before acquisition, and compares actual retained count/bytes/largest
before completion. Pending returns cannot cause a repeated import. The composition
checkpoint `acpi-startup-vertical-zt9rea_y` passes 49 checks per strict profile
through admission, allocation, binding, grant copy and completion, including
reserve/binding failure and duplicate metadata. Root verified 184 inputs, all
171 parent inputs preserved, and 10 artifacts. These tests retain the earlier AML
source lineage; no latest-interpreter overlay or native wire protocol is implied.

Authenticated provider discovery, launch metadata, actual epoch-bound wire
mapping and native instance wiring remain incomplete.
The shared service still has its earlier startup defaults; B03/N5 remain open.

## Earlier private Buffer and upstream evidence (2026-10-09)

`runtime-buffer-9t1s8gin` extends the reference checkpoint below with root
runtime `Name(Buffer(TermArg))` evaluation through the existing evaluator,
reference/value coercion, authenticated reservation cleanup, and a shared
low-32-bit Buffer length policy for static and dynamic forms. Nested dynamic
Package/VarPackage counts remain unfinished. No hardware authority is added.

Its full release/checked run passed 24,742 explicit checks per mode, including
12 extracted ACPICA bytecode cases (bounded resource divergences are explicit).
The final affected Name fixture separately passed 494 checks per mode; original
checked parser/caller checks and three timer adapters compiled and passed their
reported gates. Root verified 313 inputs, 291 preserved files, 21 artifacts and
19 before/after guards. Audit SHA256:
`cafef4d8d8b59352b422a2d4a8ab9eefa850e458d8d854a11811878731a5140f`.
These are hosted results; native stack bounds remain pending. The frozen Buffer
milestone proof discharged 142 decoder and 138 coercion obligations. The actual
Datum registry instance discharged 26/31 obligations, including all 23 runtime
checks; five state-validity postconditions remain unproved. Identity specifications
yielded flow/termination results but no proof obligations. These results do not
prove the executor or the full interpreter; further proof work is deferred to
major milestones.

The current-source upstream smoke in `aslts-hosted-gate-70rtehq6` built one
unmodified arithmetic/n64 configuration. ACPICA passed its 17 arithmetic methods;
CuBit admitted the table, then returned `UNSUPPORTED` with fuel usage 77.
**CuBit has not passed this configuration.** The report has zero passed, one
unsupported, 341 entrypoints and three other compiler modes unrun. No timeout or
resource failure occurred. A separate bounded diagnostic reproduced the same
status/fuel usage and located Store-to-Debug (`5B31`) in `STRT` at method offset
171. Its 47 trace records and exact AML bytes identify the parser rejection;
the diagnostic-only instrumentation is not production code. The later private
Debug compatibility slice below addresses this specific rejection.
Root verified 272 inputs and 18 artifacts for the uninstrumented smoke, audit SHA256
`c234685a91e240b62ee4fae5118d970269fe27c9f4e072bd77295b05f7bc1f36`.

This harness supplies a real hosted monotonic clock and input-sized table
capacity capped at 1 MiB; it leaves namespace/value/method quotas intact. It does
not solve production startup provisioning. The smoke used a 27,161-byte table,
so it does not validate admission of the larger control tables. Full ASLTS
completion remains a failing gate.

`aml-debug-targets-whwc75i4` implements Debug targets with an explicit readonly
observer and disabled production output. It passes 2,295 hosted checks per strict
profile. Cached ACPICA comparisons match 44/52; Buffer-to-Local copying and missing
Increment remain explicit failures. Root verified 282 inputs and 130 artifacts.
The uninstrumented successor `aslts-debug-gate-_0p9bf7g` reuses the exact pinned
AML/reference evidence and advances to `UNSUPPORTED` at fuel usage 176. Its
272 source hashes and 17 artifacts are verified; no timeout/resource failure
occurred. It still does not pass the configuration. Separate bounded tracing
reproduced the same failure with 218 records and identifies explicit ToInteger
(`99 60 00`) in `SET2`, method offset 14 / table offset 13420. Root verified
272 diagnostic inputs and 13 artifacts. The missing opcode handler is the next
demonstrated upstream blocker; diagnostic instrumentation remains private.

The private metrics loop separately passed off/on hosted checks and a second
run using actual CCL-generated publisher bindings (20 checks per mode). The
host compiler verified identical off-manifest bytes and exactly one on-request,
with encoded and generated slots agreeing. The coherent latest-Buffer loop
checkpoint separately passes off39/on20 checks per profile with a 64 MiB hosted
stack. Its earlier checked run failed with the default 8 MiB stack; the same
binary passed with the explicit budget. No native linking, startup, native stack
proof or live delivery follows from these checks.

## Earlier private reference milestone (2026-10-09)

The tested reference candidate is
`reference-condref-provisional-integrated-zhqkesod` under the private
`.acpi-proof-work` directory. Its ten suites pass **37,333 explicit checks in
each release and assertion/overflow-enabled mode**, including CondRefOf,
provisional name writes, integer origin transport, runtime static Name,
reference identity and compound CopyObject quota rollback. All 305 recorded
inputs were reverified after terminal success. `integrated-audit.json` has SHA256
`23f8a1d308c0147ebc7e8808981c4166f7b896a62cad41374dbdd7ede4006b1a`.
CondRefOf includes 20 extracted ACPICA AML-body cases; the field/region fixtures
use explicitly admitted table bindings rather than ACPICA's simulated hardware.

This candidate remains private: it is not the shared service source or a deployed
service. Its parent combined checkpoint compiled all 52 original mains, passed
60,554 original checked assertions and 74,619 service checks in each mode, plus
two ACPICA table-runner comparisons and six diagnostic runs. Those parent results
are not fresh tests of the newer descendant. Tests use a 64 MiB hosted stack;
none establishes native stack sufficiency. New reference work has no milestone
proof yet. The GNAT 15.3 generic callback precondition crash and the retained
call-site assertion workaround are recorded with both compiler reproductions.

Implemented private subsets now include RefOf/DerefOf/Index identity and frame
lifetimes, atomic CopyObject attachment, static runtime Name, authenticated
provisional reserve/complete/abort, scalar Store and CopyObject into provisional
names, and CondRefOf identity observation without executing methods or fields.
Named identity does not widen data read/write authority. At that checkpoint, general conversions, runtime Buffer/package counts,
reclamation and full ASLTS completion remained unfinished. The later root Buffer
implementation and current upstream result are recorded above.

Separately, `acpi-observability-wrapper-28y4edv4` has tested metrics-on/off
wrappers: publisher 292 checks, enabled wrapper 27 and disabled wrapper four in
each mode. Root verified 119 inputs and 22 artifacts. It uses the real metrics
batching code with transport/grant mocks, bounded pumping, coalescing, clock
conversion checks and retained state during shutdown. Event-loop wiring,
manifest selection, actual CCL delivery, log streams and events remain pending.
No new proof or native build is claimed for either checkpoint.

The older checkpoint sections below are historical evidence; their unsupported
operator lists describe those snapshots, not the current private candidate.

## Previous coherent hosted checkpoint

The coherent comparison/metadata candidate compiles all 52 original hosted mains.
Its 11 focused suites pass 69,450 explicit checks in each of release and checked
modes; checked builds additionally execute contracts and Ghost model assertions.
Seven affected original checked suites pass 20,892 checks. The original table
runner now explicitly initializes members after its single-table load: two
forward-alias fixtures match ACPICA and six diagnostic runs preserve deterministic
missing/unsupported reports. These are scoped checks, not full ACPICA conformance.
See [integration evidence](../tests/acpi-integration/README.md) and the private
candidate's source/artifact hash audit for the exact inputs.

New functionality includes limited owned service state, completed-load static
package-member initialization with retained diagnostics, owned string comparison,
and authenticated retained MCFG/SLIT/MADT metadata queries. Native label 8 remains
reserved; generic metadata dispatch rejects it. This milestone does not integrate
the separate typed Store work, SRAT query routes or DMAR parser, and does not
establish live startup or hardware access. No new proof/native build was run.
Historical proof counts below apply to their recorded source snapshots, not
implicitly to these changed units. Their proof status needs milestone revalidation.

## Integrated Store/SRAT candidate

The next private descendant adds bounded named string-to-string Store,
local/argument value capture and authenticated descriptor refresh, plus retained
SRAT queries 16–18. Exact parent hashes were checked before overlay. Previous
checkpoint counts above remain evidence for that checkpoint; the descendant's
own validation report records combined results: all 52 original mains compile,
14 suites pass 72,308 checks per mode, and ten affected original checked suites
pass 42,238 checks. Owned typed migration preserves all original bytecode/fuel
scenarios with ownership-correct identity/storage assertions. No new proof/native build or
live hardware integration follows from this Store/SRAT checkpoint. DMAR was added
in a later descendant described below.

String replacement is append-only and quota bounded, without reclamation.
Invalidated Index references fail closed. Mixed named conversions and general
RefOf remain unsupported; CopyObject ARGR/LOCR/INXR remain unsupported. SRAT
fields remain firmware metadata, not authority or operating-system domain indices.

A subsequent three-unit follow-up preserves `Value_Limit` for failed string
capture and named writes instead of collapsing quota exhaustion to Unsupported.
It leaves supported execution semantics unchanged. Its focused checked/release
evidence is separate from the broader pre-quota integration counts above.

## DMAR metadata descendant

A subsequent private child integrates the unchanged bounded DMAR decoder and
read-only query labels 19–23. It decodes outer forms 0–6, validates nested scopes
and paths, preserves unknown wire metadata, and exposes ANDD name offsets for
existing table-byte reads. Unknown-record scope counts mean decoded scopes only.
Addresses are split into signed-safe 32-bit words; they confer no register or DMA
authority. Native label 8 remains separate. Current-source proof, IOMMU admission,
topology policy and live provisioning remain pending.

The combined quota/DMAR child passed 7,915 checks in each release and checked
mode (DMAR 2,055 plus SRAT/MADT/metadata/request/endpoint regressions). The checked
hosted harness needs the parent's 64 MiB stack; this is no native-stack sufficiency
claim. All 44 quota-parent AML files remain byte-identical. The unchanged parser's
prior ACPICA oracle evidence is inherited, not a new oracle run on service routes.

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
| T-DMAR | DMAR | partial / audit / partial | Bounded typed forms 0–6 and nested device-scope/path metadata are available through authenticated labels 19–23; unknown records/scopes retain bounded metadata. Hosted release/checked service tests pass; unchanged parser has prior ACPICA fixture evidence. Current-source proof, IOMMU/topology admission and live integration remain pending. |
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
| T-MADT | MADT | partial / audit / partial | MADT (wire signature APIC): bounded record metadata and typed forms 0–5, 9 and 10 are decoded and queried through authenticated labels 13–15. Unknown records retain bounded metadata. Other forms, topology policy, current-source proofs and live integration remain pending. |
| T-MCFG | MCFG | partial / audit / partial | Bounded allocation metadata and authenticated retained queries 9–10 are hosted-tested. Address fields confer no authority; segment/bus policy, current-source proof and PCI integration remain pending. |
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
| T-SLIT | SLIT | partial / audit / partial | Bounded matrix decoding and authenticated retained info/distance queries 11–12 are hosted-tested. Topology policy, current-source proof and live integration remain pending. |
| T-SPCR | SPCR | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-SPMI | SPMI | audit / audit / audit | Audit current consumers and applicability; implement/prove/test required semantic decoding and service routing. Generic retention alone is insufficient. |
| T-SRAT | SRAT | partial / audit / partial | Bounded decoder and authenticated retained info/record/typed queries 16–18 are integrated in the private descendant. Unknown records retain metadata; domain/address/handle fields grant no authority. Current-source proof, topology policy and live integration remain pending. |
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
| A02 | Name, Alias, External, Scope, Device, Processor, PowerResource, ThermalZone: complete declaration/lookup rules, initialization order, forward references and conflicts. | partial | partial | partial | Namespace declaration subset plus explicit completed-load static package-member binding handles supported forward references. Missing/unsupported references retain diagnostics. Full declaration and initialization semantics remain incomplete. |
| A03 | Integer arithmetic/bitwise/logic: complete operand conversion and target semantics, including Increment/Decrement and all comparison types. | partial | partial | partial | `AML_Integers`, `AML_Logic`, evaluator; proved integer kernels do not prove all AML operand forms. |
| A04 | If/Else, While, Break, Continue, Return, Noop: close evaluation order, nesting, side effects, errors and all supported datum kinds. | partial | partial | partial | Branch/loop and ACPICA comparisons; budgets remain explicit. |
| A05 | Method calls, argument/local semantics, recursion and serialized methods: complete reference arguments, implicit conversions, reentrancy, yielding and concurrency. | partial | partial | partial | Synchronous integer-oriented calls and serialized ordering exist; no general concurrent AML scheduler. |
| A06 | Store and CopyObject: all source/destination types, implicit conversion, aliasing, reference targets, buffer/package mutation and error side effects. | partial | partial | partial | Private reference milestone adds origin-preserving transport, direct versus Arg-indirect target behavior, atomic compound CopyObject attachment and provisional scalar Store/CopyObject (37,333 combined checks per mode). Mixed conversions, all target forms and full failure semantics remain incomplete; no new proof or deployment. |
| A07 | RefOf, DerefOf, Index, CondRefOf and reference identity/lifetime: build general reference model and alias-safe mutation. | partial | pending | partial | Private RefOf/DerefOf/Index and CondRefOf implementation has authenticated arena/frame/incarnation lifetimes and bounded reference traversal. CondRefOf observes Method/Device/Region/Field identity without value access; 20 extracted ACPICA body cases plus reference/lifetime/quota regressions. Stale identities reject intentionally; nondata value coercion remains incomplete. No current-source proof or live integration. |
| A08 | DataTableRegion: execute actual declaration operands, lookup/wildcards, errors, namespace ownership and table lifetimes. | partial | partial | partial | Catalog lookup and internal service binding exist; direct helper tests are not opcode execution. Owned-backing lookup is now integrated: 33811 selection checks, 23 proof +4 flow checks with zero unproved/justified; registered integration also passes 847 field-read checks. It rejects malformed inventories and proves first matching index. Namespace binding/accessor proof: 68 checks; service declaration/read adapters: 29 checks, zero unproved/Assume, terminal session 23652. Hosted namespace 351, service 1051624, ACPICA bit reads 80, full hosted regression and private native link passed; active method-owner execution remains untested. |
| A09 | Field, BankField, IndexField: declaration execution, accumulated offsets, access state, lock/update rules, region/selector binding and lifetime. | partial | partial | partial | Field-list framing and immutable table descriptors/readers exist. Service.Invoke now executes AML reads of API-bound fields, using integers or wide buffers, preserving type 5 inspection; 847 hosted checks and 54 ACPICA comparisons pass. Method-time Field now executes AnyAcc/ByteAcc NoLock/Preserve declarations with bounded descriptor staging and atomic failure; 1523 hosted declaration checks, 978 boundary checks and 54 actual-Field ACPICA comparisons pass. Final service proof87954:2458 proof+322 flow, zero unproved/justified. Module-level loading, DataTableRegion, other access forms, hardware effects and reclamation remain. Depends on A08/A10/H01. |
| A10 | OperationRegion and handlers: address-space semantics, deferred expressions, region activation/_REG, bounds, errors and authority. | partial | partial | pending | Policy/transaction models exist; actual AML declarations and admitted hardware handlers remain. Never authorize directly from firmware addresses. |
| A11 | Field reads/writes: integer-versus-buffer results, unaligned/multi-access fields, access widths, Preserve/WriteAsOnes/WriteAsZeros and atomicity/locking. | partial | partial | partial | `AML_Field_Data` proves immutable bit extraction; no general writable field implementation. |
| A12 | CreateField/CreateBitField/CreateByteField/CreateWordField/CreateDWordField/CreateQWordField: buffer aliasing, bounds, writes and lifetime. | pending | pending | pending | Depends on general object/reference model. |
| A13 | Buffer, Package, VarPackage: full runtime creation/evaluation, variable lengths, uninitialized elements, nested references, count expressions and mutation. | partial | partial | partial | Private root Name(Buffer(TermArg)) now evaluates active counts once with authenticated reservation cleanup and shared low32 length policy; hosted release/checked and extracted ACPICA body cases pass within explicit bounds. Nested dynamic Package/VarPackage traversal, deferred identities and discarded-member side effects remain unfinished. Current-source proof pending. |
| A14 | Concatenate, ConcatenateResTemplate, Mid, Match, SizeOf, ObjectType and package/buffer/string operations: all operand kinds and edge cases. | partial | partial | partial | SizeOf/ObjectType subsets exist; inventory other operations individually. |
| A15 | ToInteger/ToBuffer/ToString/ToDecimalString/ToHexString, FromBCD/ToBCD and implicit conversion: full explicit/implicit conversion matrix and failure behavior. | partial | partial | partial | ToInteger, ToBuffer, ToString, BCD and admitted implicit conversions have hosted tests. Explicit decimal/hex conversion remains unfinished; the scoped BCD proof is incomplete. Not a complete conversion system. |
| A16 | Dynamic object ownership: temporary names/data/regions/fields, recursion, error cleanup, references surviving name deletion and storage reclamation. | partial | partial | partial | Private reservation primitives authenticate method ownership/incarnation; complete/abort preserve later side effects and invalidate escaped references on deletion. Nested/recursive lifecycle and provisional Store/Copy rollback are hosted-tested. Collecting service allocation boundaries and retained-result lifetimes are hosted-tested; complete region/field ownership and current-source proofs remain incomplete. |
| A17 | Load, LoadTable, Unload and table handles: transactional loading, namespace updates, permissions and handle/reference invalidation. | pending | pending | pending | Depends on B05, A02, A07; immutable bootstrap import is not dynamic AML loading. |
| A18 | Mutex, Event, Acquire, Release, Wait, Signal, Reset: timeout, ordering, recursion, wakeups, cancellation and inter-method concurrency. | pending | pending | pending | Serialized synchronous ordering is only a prerequisite. |
| A19 | Sleep, Stall, Timer: timing units, clock behavior, bounded scheduling and cancellation without busy-waiting in inappropriate contexts. | partial | partial | partial | Sleep/Stall requests and pure delay normalization have hosted tests; 17 scoped policy obligations proved. Native delay provider, timing, scheduling and cancellation remain unfinished. |
| A20 | Notify: queued event semantics, target identity, coalescing/loss policy, subscription authority and delivery. | pending | pending | pending | Depends on S04/H04; not arbitrary IPC from AML. |
| A21 | Revision, Debug, BreakPoint, Fatal and remaining extended operators: enumerate and implement normative observable/error behavior. | audit | audit | audit | Must reconcile against opcode inventory; do not silently skip unsupported instructions. |
| A22 | Module-level execution, deferred initialization, predefined namespace objects/methods and initialization ordering (_INI, _STA, _REG, _OSI/_OS): complete platform policy and semantics. | partial | partial | partial | Completed-load static package-member initialization is explicit and reported; it is not general module execution or full _INI/_STA/_REG platform initialization. |
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
| S03 | Service-aware AML execution context: retain immutable tables without unsafe references or repeated large stack copies; serialize mutable state safely. | partial | partial | partial | Explicit aliased immutable table backing now reaches the actual namespace evaluator via Service.Invoke. Selected proof session20247 passed 2701 proof +379 flow checks with zero unproved/justified; native link and426 input hashes pass. Service catalog preservation is contracted. Hosted847 field and1811 input checks pass, ACPICA54 field/212 typed/26 dynamic comparisons pass. Collecting service reclamation and opaque result lifetimes now have hosted coverage; complete declaration semantics, concurrent integration, current-source proofs and native stack bounds remain unfinished. A05/A08/A16. |
| S04 | CCL events, metrics and log streams: schemas, authority, subscriptions, loss/backpressure, querying and observability integration. | partial | partial | pending | Authenticated metrics include member initialization diagnostics; retained MCFG/SLIT/MADT/SRAT/DMAR query pages are hosted-tested and preserve state. Private metrics projection/publisher/on-off wrapper is hosted-tested with real batching and mocked transport/grants (292/27/4 checks per mode). Event-loop/manifest integration, real CCL delivery, logs and event streams remain missing deliverables. |
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
