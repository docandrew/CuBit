# ACPI service coverage and completion checklist

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



## Mutex declaration and lifetime checkpoint (2026-10-21)

Historical stage1 evidence follows; its original results are retained.

Runtime Mutex declarations now create real, owner-authenticated leaf objects
with immutable canonical SyncFlags. Method-owned names retire on return or
failure; retained object identities survive actual collection until released.
Direct and nested copying reject transactionally. Static metadata inspection
remains descriptive and does not confer lock ownership.

The two portable hosted selections passed 469 checks per profile in release
and assertion-enabled builds (307 object and 162 lifetime checks). A separate
focused run passed those tests plus 460 Event/retention regression checks per
profile. The unchanged 137-byte ACPICA-generated reference table now passes
module admission and member initialization, then fails its original TEST
expectation with Unsupported at Acquire, charged 2. Later reference observations
were not run. See the [hosted test registration](../tests/acpi-service-milestone/README.md).

Acquire/Release, invocation ownership, serialized-method locking, contention,
cleanup under concurrency and the global lock remain unfinished. This milestone
adds no SPARK proof or native execution claim. Adjacent suite manifests and
explicit source lists include the new unit; refreshing them is not a runtime
rerun of those suites.

## Private scalar-ledger proof checkpoint (2026-10-21)

The cached-count candidate remains private. All 74 selected entities passed on
the small 3-object/6-root/generation-50 instance: 815 proof obligations, zero
unresolved. The actual 30,559-object/30,559-root instance with the production
default generation hit the 180-second first-group limit during solver work.
That attempt is incomplete and does not establish the production-capacity or
generic proof. Runtime, stack and generated-code comparisons passed separately.

## Integrated provider transport checkpoint (2026-10-21)

The provider-process bridge now enforces optional Required startup Notify
arming. It consumes one configuration attempt before querying; an unknown,
rejected or malformed outcome permanently blocks upload. Only the actual known,
exact successful Finish admits notification service. Private consumer state
correlates each finite call, returns copied observations, and acknowledges only
after explicit delivery confirmation. Lost receipt replies retain the existing
exact-receipt retry policy; they never reopen stopped uploads.

All seven portable hosted selections passed freshly on this implementation in
release and assertion-enabled builds: 664 runtime checks across 36 processes
per profile, including 360 new transport cases, plus both compile-only selections
against actual runtime/allocator specs. Root checked sources, tools, binaries,
artifacts and 9,419/11,042 ALI dependency hashes. Installed validation passes all
seven inventories. These fresh results supersede the earlier 246-check release
and carried checked checkpoint for this bridge. See
[provider transport tests](../tests/acpi-provider-native/README.md).

The bridge retains finite deadlines, immutable copied table storage and
read-only grants until confirmed retirement. Uncertain grant creation quarantines
its process-lifetime backing. Runtime tests model IPC/grant/clock/allocator
boundaries: native kernel grants and SPARK correctness are not established.
Firmware export, trusted launch, the provider/broker main and CCL binding remain
unconnected. The bridge is not part of the ACPI server build; no kernel,
runtime, launcher or logger changes were needed for this integration.

## Private performance/proof checkpoint (2026-10-21, later)

The cached scalar-ledger candidate now has complete reports for its selected
71-entity union: 736 obligations proved and six unresolved, in the bounded
three-object/six-root/50-generation instance. Forty-six entities were freshly
analyzed in three groups; 25 retain exact-source reports from the earlier run.
The unresolved obligations concern root metadata, malformed batch-selection
rejection, batch-drop invariants and Copy's public postcondition. No selected
body was skipped and no Assume pragma was used. The candidate remains private;
this does not establish generic-capacity correctness or interpreter proof.
Root independently checked the report, source and tool hashes and entity union
in `ledger71-split-reviewed.json` under the private root evidence directory.

A separate reserved-frame snapshot optimization passed its portable release
and assertion-enabled tests: 174 internal equivalence checks and 667 checks
against unmodified candidate production per profile. The scoped proof attempt
failed during compiler Global generation before producing target obligations,
so it contributes no target proof credit. Focused source analysis found no
demonstrated runtime fault behind that diagnostic. It remains private pending
resolution of the proof/compiler issue; no production speedup is claimed.
See `reserved-portable-reviewed.json` for the independently checked runtime
evidence. Earlier proof/test checkpoints below retain their historical scope.

## Private ledger proof checkpoint (2026-10-21)

The cached scalar-ledger candidate passed its four hosted functional/equivalence
mains in release and assertion-enabled builds. The latest complete proof run
covered a declared 60-entity union in the bounded instance of three objects,
six roots and 50 generations: 517 obligations proved and 10 remained unresolved.
All five groups finished without skipped proof obligations or Assume pragmas;
flow diagnostics were informational. Remaining obligations cover metadata,
allocation frames, batch selection/checker equivalence and zero-root summaries.
The candidate is private and the milestone remains incomplete. These counts
neither establish generic-capacity correctness nor whole-interpreter proof.

A separate false-refinement control confirmed that runtime-erased refinements
still generate proof obligations while the public postconditions remain checked.
The root audit is retained in the private evidence directory as
`ledger60-terminal-reviewed.json`; subsequent source repairs need fresh evidence.

## Integrated snapshot provisioning policy (2026-10-21)

The bounded snapshot-provider policy is integrated with portable hosted tests.
It preflights catalog capacities, imports immutable tables in order, and invokes
Finish once. Unknown mutation outcomes stop further mutations; provider grant
retirement remains separate. Lost replies never trigger automatic re-import or
re-initialization. The endpoint fixture passed 165 checks in each strict release
and assertion-enabled profile, both privately and through the portable runner.
See [provider tests](../tests/acpi-provider-bootstrap/README.md).

This establishes hosted orchestration behavior under trusted synchronous
callbacks and exclusive provider ownership. It does not implement firmware
catalog export, native grant ownership, service launch, or CCL binding. Startup
Notify interest must be configured before Finish when required. The new policy
has not yet received a functional SPARK proof.

## Original control-flow profiling checkpoint (2026-10-21)

A diagnostic release build ran the unchanged original 155,922-byte ACPICA
control-flow AML on the current Arg/Notify baseline. Module execution completed
with 1,245 charged operations; MN00 then reached its existing 1,000,000-operation
budget and failed. The run produced a normal profile before the 64-second wall
limit. This is not a full-suite pass or evidence of an unsupported opcode.

The largest sampled CPU share was scalar transport `Put` (25.71%, 467,667 calls),
followed by object validation (11.85%) and live-root publication (11.75%). This
supports prioritizing the existing cached ledger-count work. That optimization
remains private pending its proof milestone. Profiling instrumentation and other
host activity mean these numbers are diagnostic proportions, not a production
benchmark or a promised speedup. No execution limits were increased.

## Notify consumer and native provisioning checkpoint (2026-10-21)

The bounded Notify consumer is integrated. Its portable hosted suite passed
243 checks in each release and assertion-enabled profile on the integrated
Arg/Notify baseline. It correlates replies with trusted host request tokens,
assembles one copied observation, and acknowledges only after explicit delivery
confirmation. Lost acknowledgment replies retry the same receipt. This is
at-least-once delivery, not exactly-once handler effects or a native CCL binding.
See [consumer tests](../tests/acpi-notify-consumer/README.md).

The native entry point, configuration decoder, receive loop and grant adapters
exist. Firmware catalog export, trusted provider startup, service launch and
CCL binding are not connected. Discovery-driven capacity selection, hardware
regions and native delay/Event providers remain unfinished. The next hosted
integration work covers provider upload and unknown-outcome recovery without
introducing physical-memory authority or changing the logger.

The unchanged ACPICA ASLTS Return collection was rejected by the pinned normal
compiler with 29 errors (6163: cross-method access to temporary objects),
13 warnings and 709 remarks. No AML was generated; disassembly, reference
execution and hosted execution were not run. This adds no interpreter coverage
and is not a CuBit runtime failure. The original full control-flow collection
also remains incomplete under the recorded execution bounds.

## Current Notify integration and AML reference checkpoint (2026-10-21)

Retained Notify polling is integrated into the shared checkout. Authenticated
provider requests configure interest, claim a queued event, read its copied
metadata, and acknowledge an exact receipt. Observer requests remain read-only.
A repeated claim retains the event; an old acknowledgment cannot clear a newer
receipt. Startup interest takes effect before device initialization. Module
Notify evaluates its operands and reports errors while event delivery remains
disabled during module execution.

Six hosted programs passed 607 checks in each release and assertion-enabled
profile. The portable runner separately passed polling (379) and module-policy
(68) checks. All six installed source inventories validate. These results cover
hosted request behavior; native failed replies, CCL consumer binding, SCI/GPE/EC
handling and full service proof remain unfinished. The finite receipt policy has
separate proofs for its zero, two and default-capacity instances. See the
[Notify test registration](../tests/acpi-notify-poll/README.md).

The integrated argument-reference correction passed 1,006
focused checks per profile and the full original ACPICA ASLTS provoke program
in hosted release mode. Both invocations returned MN00/ERRS zero, recorded all
five expected PASS results in order, and restored retained handles. The pinned
ACPICA executable still fails two of those original cases; this is not a
CuBit/ACPICA differential pass. Combined validation with Notify passed six programs (2,183 checks per profile),
including three explicitly carried release results, and the full original provoke
release gate. The guarded integration also repaired active ASLTS manifests; this
does not claim reruns of the local or buffer-field suites. See the [root audit](../../.acpi-proof-work/root-capture-composition-review-d9frwbwi/provoke-original-pass-reviewed.json).

These milestones do not establish complete opcode or semantic coverage, a
whole-interpreter proof, or native deployment. Historical checkpoints below
retain their original evidence scope; active manifest updates are not reruns.

## Current integrated milestone (2026-10-20 late)

The Event, named field-evaluation, device-startup and startup-metrics milestone
is now applied to the shared checkout. The guarded promotion changed 124 files,
including 22 production paths, with before/after content verification under the
shared build lock. The historical private checkpoints below describe earlier
states; their pending status is superseded by this entry.

The broader hosted regression passed 76 selected jobs: 16 semantic adapter
compilations and 22 runtime mains per release/checked profile, totaling 7,710
explicit runtime checks per profile. Of these jobs, 47 ran in the final
continuation; 29 retain individually verified successful prior executions with
exact compiled dependencies. Separate earlier focused evidence remains 314
checks/profile. The public collection fixture does not reproduce the historical
injected fixture's exact internal root-bitset assertions.

The portable three-main smoke passed 463 checks/profile on the predecessor
runner. The final runner restores the documented 64 MiB stack bound; all 23
installed inventories and source/reference hashes passed validation in Nix.
That validation is not a fresh execution of all 23 mains. See the
[service milestone registration](../tests/acpi-service-milestone/README.md).
The four applied pure-policy files have 10 discharged proof obligations and six
flow checks; this proves their specified policy only, not the whole interpreter.

The separate private continuation-capacity candidate passed 528 focused checks
in each profile. A full-setup ASLTS diagnostic with explicit capacity 128 passed
the earlier nesting failure, then failed on runtime-sized Buffers inside a
Package. A minimal normal ACPICA oracle succeeds while the hosted interpreter
reproduces `Unsupported_Value`. Runtime package construction is being implemented
privately, including deferred names, side-effect ordering and collection-safe
partial objects. Neither full ASLTS coverage nor a native service pass is claimed.

Root evidence includes `service-landing-za32fc9k`, `broad76-carry-reconciled.json`,
`service-installed-validation.json`, `startup-policy-proof-reviewed.json`, and
`nested-buffer-oracle-red-reviewed.json` in the private evidence directory below.
Full interpreter proof, native startup, hardware providers, SCI/GPE/EC handling,
and CCL event delivery remain unfinished.

## Private Event and startup integration checkpoint (2026-10-20 evening)

The single combined service candidate `full-service-startup-event-source-lh2e0083`
includes Event operations, named field evaluation, device initialization, startup
fuel accounting and CCL metrics pages. It remains private; its broader hosted
regression is in progress. Source integrity was independently checked across
3,026 pinned inputs and 14 guarded imports. This is not a combined test pass.

Focused combined Event/field tests passed 397 checks/profile, and a separate
operand-category replay passed 59/profile. Eight earlier executions covering
named Event copy, Wait identity, operand timing and operand categories retain
exact compiled dependency equivalence on the combined service (314 checks/profile).
These are carried results, not fresh executions on the combined candidate.
Startup and metrics parent candidates passed 18 and 49 checks/profile; legacy
startup/request tests passed 266/profile on their recorded parent. The broader
run will test their composition with the Event and field changes.

The next ordered ASLTS control run passed the earlier unsupported Event point,
then returned `Block_Limit` at 215,036 charges. An isolated WHL0 diagnostic instead
returned `Unsupported_Value` at 22 charges, before reaching the block limit.
Deeply nested loops in the corpus are a possible explanation, but the exact
full-run failure site remains under investigation. Neither result is a control
suite pass. Limits and original failures remain preserved.

Root audit records in the private evidence directory include
`combined397-reviewed.json`, `event-category59-reviewed.json`,
`full-service-composition-reviewed.json`, `full-service-eight-carries-reviewed.json`,
`startup-metrics49-reviewed.json`, `aslts-control-block-failure-reviewed.json`,
and `whl0-diagnostic-failure-reviewed.json`. New Event, field and startup behavior
is tested, not formally proved. Full interpreter proof, native service startup,
hardware providers, SCI/GPE/EC and CCL event delivery remain unfinished.

## Private AML compatibility and initialization checkpoint (2026-10-20)

This checkpoint supplements the historical entries below; it does not claim
100% opcode, semantic, upstream-suite, or proof coverage. The integer ownership
and completion changes and their portable test registration were subsequently
applied under the shared build lock on 2026-10-20. Event and device-initialization
candidates remain private. See the integer ownership registration below for the
exact scope of the private regression gates and portable smoke validation.

- The combined integer/ownership candidate `integer-completion-wide-source-m8g7cj3j`
  passed all 98 stages: 17 runtime programs and 14 semantic adapter compilations
  per release/checked profile, plus tool checks. Focused ToInteger evidence
  (151 checks and 20 observations/profile) is carried only after exact dependency
  comparison. A separate seven-main conversion regression passed all 30 stages
  and 29,190 checks/profile, including 28 supporting parser checks.
- These changes cover canonical scalar ownership, selected collection/lifetime
  cases, optional NAME sources, named Package Store, and explicit DerefOf and
  ToInteger completion. They do not establish full ownership/cycle equivalence.
- The ordered hosted ASLTS runner passes eight of twelve initial configurations
  (arithmetic and logic). Control tables first exceeded the hosted namespace and
  method-storage capacities, then the 100,000-instruction method budget. Explicit
  hosted capacity sizing and a bounded one-million-instruction diagnostic reached
  an unsupported method-local Event declaration. Four control configurations
  remain without a pass; 339 other entrypoints remain unrun. These are release
  results, not a full checked ASLTS pass.
- Normal ACPICA Event probes establish count/reset/timeout behavior and method
  cleanup. Twenty further successful observations across both integer widths
  establish returned Event lifetime through direct arguments, independent Event
  instances, and CopyObject creating an unsignaled independent Event. Twenty-four
  separate Store-based probes reject the Event source before the later operations;
  those are retained negative cases, not successful escape tests. Event storage
  and operations are under implementation, not claimed complete.
- A private device-initialization coordinator passed its first 60 hosted checks
  per profile, covering ordinary data retyping, ignored compound return cleanup,
  region-barrier outcomes and replay refusal. Broader traversal and error tests
  are in progress. This does not establish native startup or working providers.

Root reviews and source/tool/artifact provenance are in
[the private evidence directory](../../.acpi-proof-work/root-capture-composition-review-d9frwbwi/):
`integer-completion-wide-reviewed.json`, `tointeger29190-reviewed.json`,
`device-initialization60-reviewed.json`, `event-escape-store-rejection-reviewed.json`
and `event-direct-escape-reviewed.json`. Raw ACPICA warnings and failures remain
preserved. These are hosted tests and oracle observations; the new implementation
has not received milestone SPARK proof. Hardware authority integration, SCI/GPE/EC,
CCL delivery, full AML semantics/proof and native validation remain unfinished.

## Private operand capture and region-selector checkpoint (2026-10-18)

The private combined candidate `capture-milestone-fixtures-14twa4dp` passed
52 build/run stages and 9,062 checks in each strict release/checked profile.
Its `aml_execute.adb` SHA-256 is
`fc6ab0321724a353ae92c803859737d7d1b0df252f7c286fdd4f6adf0544150c`.
This covers selected numeric, conversion, concatenation, Match, staged-field,
reference and collection behavior, not all AML semantics. Independent resource
replay passed 4,736 checks/profile; staged-field lifetime tests passed 300 and
source-buffer replay/collection tests passed 144+152/profile. Actual collection
counters are asserted; method-boundary cleanup does not prove every internal
transport slot is cleared immediately.

A subsequent private DataTableRegion fix retains original operand identities
until all selectors have been evaluated. Ten normal ACPICA cases, using two
compiled target tables, distinguish late Store mutation from CopyObject
rebinding and ordinary Name copies. The unchanged replay fails on the baseline's
owned-Store case, then passes 270 checks/profile with the fix in
`region-selector-reservation-source-k25_jyk6`. Its Execute SHA-256 is
`f05483d00e09003e701b0cd0f52f83dbc6e54d6e2bb8dd818c091a0c8827aa0a`.
Raw ACPICA cache teardown warnings and the baseline failure are preserved.
Nested-region lifetime tests subsequently passed 314 checks/profile, including
observed collection, original-identity retention, nested declarations and recovery
after division failure. The changed-helper regression passed all ten release
mains and eight checked mains initially. The unchanged checked region-service
binary subsequently passed all 1,176 checks in 34.26 seconds under an explicit
60-second cap, and the independent checked metadata test passed 244 checks.
Together these establish all ten selected mains and 35,627 checks/profile.
This combines preserved initial results with the two follow-ups; the original
30-second timeouts remain recorded. Timing is not an isolated benchmark.
The earlier combined production also passed fifteen selected legacy mains with
56,430 checks/profile: eight release results were carried with dependency
evidence, seven release and all fifteen checked results were freshly run.
A separate current-source hosted startup gate then passed all five selected
Bootstrap, Requests and module Core mains: 344 checks/profile across 20 stages.
Root reverified 1,185 source inputs, 819 artifacts, five tools and ten binaries
in `capture-startup-validation-yu6wo_pu`. This establishes the tested hosted
startup cases, not native startup or all service behavior.

Commands use the pinned Nix development environment, `TMPDIR=/home/doc/cubit-build-tmp`,
and `nice -n 19 python3` with the preserved `run.py` in
`capture-milestone-family-validation-ge4nhj50` or
`region-selector-fixed-validation-3erjongq` under `.acpi-proof-work`.
Root audits, exact source/tool/artifact hashes, commands and terminal results:
[combined and region evidence](../../.acpi-proof-work/root-capture-composition-review-d9frwbwi/),
[staged lifetime evidence](../../.acpi-proof-work/root-staged-slot-review-9tvwaqja/).
These candidates have not been promoted to shared production. New bounded
scalar-transport contracts compile and pass 130 checks/profile, but their
whole-state refresh assertion has not yet been proved. Historical proof counts
do not cover these changes. Full interpreter proof, authenticated live startup,
hardware authorities, events and native validation remain unfinished.

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

## Dynamic Buffer count expressions — hosted validation

Runtime operand evaluation now evaluates admitted Buffer count expressions once
on the existing bounded expression stack. Count parsing stays inside its package;
initializer bytes remain data. Count methods retain earlier live values during
collection. Existing arena, expression-depth, reference-hop and execution-budget
limits still apply.

The explicit `buffer-dynamic` group passes in both strict profiles: 473 real-owner
checks, 20 cached ACPICA comparisons, 2,774 synthetic protocol checks and 18
evidence-guard checks per profile. The oracle corpus contains 16 integer flows,
two divide-error mappings and two explicitly coarse target-only error mappings;
the latter do not establish exact ACPICA error equivalence. Local tests separately
cover malformed package bounds, near-depth success and depth rejection,
mid-count budget exhaustion, metadata/reference-cycle rejection, standalone
expression rejection, side effects and mixed retained roots across actual GC.

Before the separately proved span/layout update, this dynamic production also
passed `buffer-sizing`, `metadata` and `notify-alias` in both profiles (18, 82 and
66 cached comparisons respectively). After combining the proved span/layout
sources, the public dynamic group passed checked mode again; release evidence
refers to the preceding source snapshot. These are hosted
results, not a current-source SPARK proof, full ASLTS result or native deployment.
Dynamic counts in arbitrary module-level/static-loader declarations and nested
Package/VarPackage construction remain unfinished. Owned table-field extraction
now exceeds the legacy 1,024-byte inspection limit; windows retain a per-request bound. See [dynamic Buffer registration](../tests/aml-buffer-dynamic/REGISTRATION.md).

## Buffer sizing — hosted validation

The public `buffer-sizing` group passes in both strict profiles: five mains
exercise 9,077 checks, thirteen adapters compile with source-path verification,
and eighteen cached ACPICA cases match (four use explicit coarse error mappings).
The group also passes 2,448 protocol and 15 evidence-guard checks. Larger literal
buffers now use validated spans and arena-backed allocation instead of the
former 1,024-byte literal-decoder cap. Runtime operand count expressions have
the separate hosted milestone above; static-loader/nested-package count paths
remain unfinished. The former blanket FieldData limit now applies only to
legacy inspection and individual byte-window requests, not owned extraction. Hosted behavior
is tested. Separate unit proofs discharge 183 decoder checks
(including Check_Buffer_Count_Span) and 91 layout checks on the applied sources.
Allocator and full-interpreter proofs remain incomplete; native deployment is
untested.
See [buffer sizing registration](../tests/aml-buffer-sizing/REGISTRATION.md).

## Metadata values and captured descriptors — hosted validation

The final isolated production snapshot passes 2,914 focused checks and all 82
cached ACPICA comparisons per strict profile. The 84-attempt corpus retains 70
integer flows, 12 explicitly coarse error comparisons and 2 normal compiler
rejections; 10,490 synthetic comparator checks pass. TEST/MARK/POST preserve one
loaded namespace and actual retained-result status. The public `metadata` group
passes all 21 stages in both strict profiles; see
[registration](../tests/aml-metadata/REGISTRATION.md).

Live metadata follows current namespace bindings. Copied Device/Thermal metadata
uses an independently authenticated capture, preserving its type after source
replacement. Root semantic bindings, retained results, foreign/stale rejection,
rollback and actual collection have focused coverage. The physical root and
ordinary data-cell authority remain separate. The root-kind invariant and
captured-copy contract are checked, not a full formal proof.

Dynamic Device declarations, runtime Alias lifetime, all descriptor state and
live metadata escaping scope remain open. Independent ACPICA captured-lifetime
observations remain future conformance cases rather than being omitted silently.
At that metadata checkpoint the constant-buffer decoder rejected lengths above
1,024; the later sizing milestone removes that literal cap within arena quotas. CCL delivery,
hardware effects and native deployment remain separate unfinished work.

Evidence: `aml-metadata-root-invariant-focused-5uf_v9dc` (30 stages) and
`metadata-82-final-run-u0wxbjzn` (7 stages, final production replay). No full
interpreter proof is claimed. The bounded level-1 milestone sweep left 10 of 66
reachability checks and 17 of 52 reclamation checks unresolved; allocation and
copy units timed out without final reports. Metadata-kind analysis had no
executable proof obligations. These are explicit remaining proof gaps, without
assumptions or weakened contracts.

On this metadata production snapshot, the broader BufferField suite also passed
68,395 checks, 3,225 protocol checks, 102 cached comparisons and ten adapter
compilations per profile. Notify/Alias passed 323 focused checks, 8,568 protocol
checks and 66 cached comparisons per profile. Historical counts below describe
their earlier snapshots.

## Notify and static Alias milestone — hosted validation

The 2026-10-14 checkpoint adds static Alias bindings, explicit canonical
object lookup, generation-authenticated root metadata references, and bounded
Notify observation queues. Aliases follow source-node identity through value
replacement and preserve the original method scope. Physical binding lookup and
canonical object lookup are separate; the latter distinguishes the root from a
missing name. Root metadata grants no data-cell read/write authority.

Six focused programs pass 290 checks in each strict release and assertion-enabled
profile. The unchanged Notify reference corpus matches all 66 selected ACPICA
cases in each profile: 48 compare positive event multisets; 18 compare status and
side effects plus a separately specified pre-admission empty-queue policy. Twenty
normal compiler rejections and two inconclusive marker cases are retained and
excluded from the passing count. These comparisons establish neither upstream
callback ordering nor the absence of callbacks in the threaded ACPICA harness.

The queue is an internal trusted-owner Core API, disabled by default. Admission
validates the target before applying observer/capacity policy. Admitted events
survive later AML errors; dequeue reports whether the original target identity
remains current. This does not implement CCL event transport or subscriptions.
Runtime Alias declarations, forward-load compatibility, full metadata descriptor
semantics, milestone proofs and native/hardware validation remain unfinished.
The metadata milestone above replaces the former root-package/Deref/
Copy rejection policy for the implemented cases; ownerless pure-state rejection
remains a distinct authority boundary.

Broader BufferField regressions passed in both strict profiles: 68,391 checks,
3,225 protocol checks, 102 cached comparisons and ten adapter compilations per
profile. A reporting step overwrote a pinned inherited checkpoint note after the
runs; the preserved correction records 1,474 matching inputs out of 1,475, with
all production/harness inputs unchanged and 1,711 runtime artifacts verified.
The public `notify-alias` hosted group passes 290 functional checks, 8,568
protocol checks and 66 cached ACPICA comparisons in each strict profile.
Prior proof counts do not cover this checkpoint.

Evidence: `aml-alias-strict-o35d18mw` (16 stages, 290 checks/profile),
`notify-alias-replay-prepared-zq77iyc9` (66 comparisons/profile),
`notify-alias-portable-validated-a82p1pl0` (public group), and independent
`root-alias-focused-audit-7nq3g5u0` / `root-notify-alias-final-audit-o3ifvvak`,
under the private `.acpi-proof-work` evidence directory.

Owner: the main ACPI implementation agent. This is its persistent completion
ledger, alongside [architecture](acpi-userspace.md), the
[service contract](acpi-service-contract.md), and
[working coordination note](../coordination/acpi.md).

The goal remains a userspace ACPI service that moves as much functionality as
possible out of the kernel, including a SPARK-verified, semantically correct AML
interpreter. This checklist does not redefine that goal around the current subset.
It was seeded from the current checkout and recorded evidence; it is not a new
proof run, a complete specification audit, or a claim of deployment.

## Buffer-field hosted milestone (2026-10-14)

Runtime CreateField, CreateBitField, CreateByteField, CreateWordField,
CreateDWordField and CreateQWordField now create authenticated views of owned
AML Buffers. Reads and writes preserve backing identity, enforce bit bounds and
retain type-14 field metadata. Generic fields return Buffers; fixed fields return
an Integer only when their width fits the effective AML integer width. Named and
unnamed runtime packages capture field values at construction. Later field writes
therefore do not change a captured generic-field Buffer.

Hosted testing on the same production sources passes 68,391 checks per strict
release and assertion-enabled profile across eleven programs, plus 102 cached
ACPICA comparisons per profile. Counts include pool-filling assertions, not that
many distinct semantic cases. Six reference attempts remain excluded: four normal
compiler rejections and two inconclusive executions. Ten affected evaluator
adapters compile in each profile, with actual selected source paths checked;
this adapter gate is compile-only. Tests cover operand evaluation order, aliasing,
32/64-bit result forms, metadata, quota failure, transaction rollback and actual
collection boundaries. Explicit CopyObject preserves authenticated Index
references; assigning those reference operands to a BufferField fails with the
observed operand-type error. Implicit value-copy behavior stays separate.

These are bounded hosted regressions and selected ACPICA correspondences, not a
proof of the complete interpreter. A12 remains partial: static/deferred declaration
coverage, complete language semantics, milestone proof and native/hardware
validation remain open. BufferField access reaches owned AML bytes only; it adds
no hardware register, I/O or arbitrary-memory authority. The original ACPI goal
and every unfinished service/power/event requirement remain in scope.

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
  Literal-selector DataTableRegion and the supported read-only module-level Field
  subset now load in hosted tests; general selector TermArgs, other access forms
  and full lifetime/reclamation semantics remain pending. Literal object materialization is now integrated
  (160 hosted cases, 70 ACPICA comparisons; focused executor/service proof
  3046 proof +414 flow checks, zero unproved/justified). Dynamic BufferSize,
  string-coercion integration, decoder bounds and arena reclamation remain.
  Pure implicit string conversion is now registered (65542 hosted checks;
  94 proof +4 flow checks; native unit compile). Fifty ACPICA values agree,
  with a control-reproduced shutdown allocation diagnostic recorded separately.
  Literal static DataTableRegion now has hosted coverage; general declaration
  execution remains incomplete. See A08–A11.
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
| A08 | DataTableRegion: execute actual declaration operands, lookup/wildcards, errors, namespace ownership and table lifetimes. | partial | partial | partial | Catalog lookup and internal service binding exist; direct helper tests are not opcode execution. Owned-backing lookup is now integrated: 33811 selection checks, 23 proof +4 flow checks with zero unproved/justified; registered integration also passes 847 field-read checks. It rejects malformed inventories and proves first matching index. Namespace binding/accessor proof: 68 checks; service declaration/read adapters: 29 checks, zero unproved/Assume, terminal session 23652. Earlier hosted namespace/service and bit-read evidence is retained above. The current table-fields milestone additionally passes literal static DataTableRegion loading, stable aliases and owned extraction, including twelve unchanged ACPICA type/length/endpoint comparisons in both profiles. General selector TermArgs and ordered module execution remain unfinished. Private owned-selector timing now has an independently audited before/after replay (270 checks/profile); see the 2026-10-18 checkpoint. Current proof and full selector/lifetime coverage remain incomplete. |
| A09 | Field, BankField, IndexField: declaration execution, accumulated offsets, access state, lock/update rules, region/selector binding and lifetime. | partial | partial | partial | Field-list framing and immutable table descriptors/readers exist. Service.Invoke now executes AML reads of API-bound fields, using integers or wide buffers, preserving type 5 inspection; 847 hosted checks and 54 ACPICA comparisons pass. Method-time Field now executes AnyAcc/ByteAcc NoLock/Preserve declarations with bounded descriptor staging and atomic failure; 1523 hosted declaration checks, 978 boundary checks and 54 actual-Field ACPICA comparisons pass. Final service proof87954:2458 proof+322 flow, zero unproved/justified. Literal static DataTableRegion and the supported read-only module-level Field subset now load; general selector evaluation, other access forms, hardware effects and full reclamation remain. Depends on A08/A10/H01. |
| A10 | OperationRegion and handlers: address-space semantics, deferred expressions, region activation/_REG, bounds, errors and authority. | partial | partial | pending | Policy/transaction models exist; actual AML declarations and admitted hardware handlers remain. Never authorize directly from firmware addresses. |
| A11 | Field reads/writes: integer-versus-buffer results, unaligned/multi-access fields, access widths, Preserve/WriteAsOnes/WriteAsZeros and atomicity/locking. | partial | partial | partial | `AML_Field_Data` proves immutable bit extraction; no general writable field implementation. |
| A12 | CreateField/CreateBitField/CreateByteField/CreateWordField/CreateDWordField/CreateQWordField: buffer aliasing, bounds, writes and lifetime. | partial | pending | partial | Runtime declarations, authenticated backing views, reads/writes, package capture and collecting lifetimes are implemented and hosted-tested; 102 selected ACPICA cases match in both widths/profiles. Static/deferred declaration coverage, full semantic audit and current-source milestone proofs remain. No hardware-region authority follows. |
| A13 | Buffer, Package, VarPackage: full runtime creation/evaluation, variable lengths, uninitialized elements, nested references, count expressions and mutation. | partial | partial | partial | The landed live package builder evaluates nested Buffer counts in named, unnamed and variable packages, with rooted partial construction, deferred names, excess-initializer effects, replacement-name completion and failure cleanup. Five scoped hosted suites passed 2,182 checks/profile; malformed-boundary and failure-after-retype suites passed 584/profile. Normal ACPICA replay and constructed malformed cases are distinguished in the [runtime-package registration](../tests/aml-runtime-packages/README.md). These results do not close all reference/cycle, mutation or allocation-failure semantics; builder/current-interpreter proof and native validation remain open. |
| A14 | Concatenate, ConcatenateResTemplate, Mid, Match, SizeOf, ObjectType and package/buffer/string operations: all operand kinds and edge cases. | partial | partial | partial | SizeOf/ObjectType subsets exist; inventory other operations individually. |
| A15 | ToInteger/ToBuffer/ToString/ToDecimalString/ToHexString, FromBCD/ToBCD and implicit conversion: full explicit/implicit conversion matrix and failure behavior. | partial | partial | partial | Explicit decimal/hex formatting is implemented alongside ToInteger, ToBuffer, ToString, BCD and admitted implicit conversions. The [explicit-format registration](../tests/aml-explicit-format/README.md) records hosted owner/capture/collection tests and 70 cached comparisons/profile (64 returned values, six exact operand-type errors); later [capture](../tests/aml-capture/README.md) and [integer-completion](../tests/aml-integer-ownership/README.md) milestones cover selected operand timing and width completion. These are scoped predecessor receipts, not a fresh complete matrix on current source. Pure formatter and BCD proof, full conversion/failure semantics and native validation remain incomplete. |
| A16 | Dynamic object ownership: temporary names/data/regions/fields, recursion, error cleanup, references surviving name deletion and storage reclamation. | partial | partial | partial | Private reservation primitives authenticate method ownership/incarnation; complete/abort preserve later side effects and invalidate escaped references on deletion. Nested/recursive lifecycle and provisional Store/Copy rollback are hosted-tested. Collecting service allocation boundaries and retained-result lifetimes are hosted-tested; complete region/field ownership and current-source proofs remain incomplete. |
| A17 | Load, LoadTable, Unload and table handles: transactional loading, namespace updates, permissions and handle/reference invalidation. | pending | pending | pending | Depends on B05, A02, A07; immutable bootstrap import is not dynamic AML loading. |
| A18 | Mutex, Event, Acquire, Release, Wait, Signal, Reset: timeout, ordering, recursion, wakeups, cancellation and inter-method concurrency. | partial | partial | partial | Landed Event behavior retains its scoped hosted and reference evidence. Ordinary uncontended Acquire/Release, same-invocation ownership and serialized-method restoration have hosted stage2 evidence described in the current checkpoint; the original 137-byte replay passes 40 checks/profile. Raw-State invocation requires exclusive synchronous caller access and no same-State callback reentry. Mutex_Not_Acquired appends wire value 40 without renumbering 0–39. Contended waits, inter-caller concurrency, wakeups/cancellation, FACS Global Lock and native integration remain incomplete. Runtime tests do not establish helper or actual Owned authority/GC/unwind proof. |
| A19 | Sleep, Stall, Timer: timing units, clock behavior, bounded scheduling and cancellation without busy-waiting in inappropriate contexts. | partial | partial | partial | Sleep/Stall requests and pure delay normalization have hosted tests; 17 scoped policy obligations proved. Native delay provider, timing, scheduling and cancellation remain unfinished. |
| A20 | Notify: queued event semantics, target identity, coalescing/loss policy, subscription authority and delivery. | partial | pending | partial | Internal bounded queue, authenticated target identity and trusted-owner Core observations are hosted-tested; 66 selected ACPICA comparisons match per profile. Subscription authority, CCL delivery, full loss policy and H04/S04 integration remain unfinished. No arbitrary IPC from AML or current-source proof. |
| A21 | Revision/Debug retain their existing scoped behavior; non-debug BreakPoint now executes as a charged Noop. Fatal and remaining extended operators require an explicit inventory and host policy. | partial | partial | audit | Focused BreakPoint: 209 checks/profile; cached ACPICA debugger differences and module timeouts preserved. No full extended-operator, native debugger or proof claim. |
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
| S05 | Device/power policy integration: lid/buttons, battery/AC, thermal/fan, backlight, device power/hotplug and sleep/hibernate/resume orchestration. | pending | pending | pending | AML + H01–H06; implement the [sleep/resume orchestration roadmap](acpi-userspace.md#sleep-and-resume-orchestration-roadmap): queued CCL policy handlers, power-service coordination, driver quiescence, ACPI preparation/wake, guarded kernel commit, abort/resume and supported backends. Pending; no system-sleep validation claimed. |
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


The opt-in `capture` hosted selection passed 13,798 checks in each profile on the integrated operand-capture and region-selector source. Separate compatibility validation completed through preserved runs: all 26 adapter compilation stages and five runtime mains per profile, including the original checked typed test (26,138 checks in 74.477 seconds under an explicit one-run 120-second exception). Earlier 30/60-second typed timeouts remain evidence; portable limits stay unchanged. Release bootstrap counters are not assertion coverage; checked bootstrap assertions passed separately. See `tests/aml-capture/README.md` for predecessor and carried evidence scopes. Complete owner sharing/Store width behavior, whole-interpreter proofs and native integration remain unfinished. Logger behavior is unchanged.


## Integer ownership portable registration

The opt-in `integer-ownership` hosted group selects the reviewed wide integer/ownership regression. Its dedicated runner also exposes `conversion` (seven focused ToInteger mains) and `completion` (the exact ten-case ToInteger completion replay). See `tests/aml-integer-ownership/README.md`. Private functional evidence is the 98-stage wide gate, the 30-stage conversion gate (29,190 checks/profile, including 28 pure-parser checks), and the separately dependency-matched 151-check/20-observation completion replay. The portable completion selection passed both profiles (151 checks/20 observations each) with explicit checked overflow checks, and 14 bounded guard/cleanup checks passed. The complete portable wide/conversion selections were not rerun; their identical private source gates remain the stated evidence. Repinned older manifests are not new execution evidence.

Existing capture and package-store manifests are refreshed for the current source inventory while their historical receipts remain scoped to their recorded predecessor sources. No new SPARK proof, native result, full sharedness/cycle equivalence or complete AML-support claim is made. Event and device-initialization work remains separate.

## Runtime packages and continuation capacity

A bounded live package builder now evaluates nested Buffer counts for named, unnamed and variable packages; it preserves count effects, rooted partial construction, excess-initializer evaluation, successful replacement of an initializing Name, and failure cleanup. Five private suites passed 2,182 checks/profile; malformed-boundary and failure-after-retype suites passed 584/profile. The composed current-service smoke passed 106/profile. These are scoped hosted results, not a full ASLTS or proof milestone. Full control-n64 progressed beyond the prior nested-package rejection but exhausted its 1,000,000 method-fuel limit. Default continuation capacity stays 64; an explicit typed capacity permits configured hosted nesting independently of lexical depth. Native stack suitability and complete builder protocol proofs remain open.

Portable entrypoint validation covers a release 48-check package main, one compile-only reference adapter, and all four release capacity partitions (528 checks). Checked capacity default32/default64 passed 129 each; large32 timed out at the retained 30-second limit and large64 was unrun. Eighteen lightweight guard checks passed. No complete checked-capacity or all-25-portable-selection pass is claimed.

A later controlled 10,000,000-fuel full control run reached its 64-second host limit without an MN00 result. This remains incomplete ASLTS evidence and does not change the portable regression limits.

The smaller WHL0 diagnostic variants at depths 1, 2 and 5 now match normally
compiled ACPICA results: two invocations each return zero and report zero errors.
Hosted execution charges are 2,036, 4,261 and 43,395 respectively, repeated
exactly. These tests use unchanged production and the original execution limits.
They do not establish the original depth-50 workload or full control-suite pass.
Timing and cost attribution remain separate from these functional results.


## Full original bfield revision-2 hosted checkpoint

The original upstream `functional/bfield/MAIN.asl` revision-2/default-optimized configuration now passes
in a private release hosted run on current shared production: two invocations
in the same arena return `MN00 = 0`, and public `Core.Evaluate_Named` reads the
existing `ERRS` value as zero after each. No wrapper AML or matrix reduction was
used. The arbitrary-field test retains its original 8-by-80 matrix. Invocation
charges were 1,088,769 and 1,088,859; this gate explicitly used a test-only
2,000,000 method-fuel budget and 180-second runtime cap for the complete process.
Module fuel remains 100,000, continuation capacity is explicitly 128, build cap
180 seconds, and stack/log caps remain 64 MiB/1 MiB. Default service and portable
regression limits did not change. Whole runtime was 97.268 seconds on the shared
host; this is an observation, not an isolated performance benchmark.

Earlier unchanged-production runs exhausted the original 1,000,000 method-fuel
budget. A public stored-data snapshot identified `m21d` with five earlier subtests
recorded PASS and zero errors; all observation handles were released. Those
failures remain evidence, not retroactive passes. Normal ACPICA compilation and
disassembly succeeded without force or suppression (3 warnings, 103 remarks).
Two five-second reference attempts timed out; one approved 30-second runtime-only
exception then produced `MN00/ERRS = 0/0` twice, with all six subtests PASS on each
invocation. Outstanding allocation/cache diagnostics remain recorded.

Evidence: `aslts-bfield-hosted-final-9upbyzfd/receipt.json` (hosted),
`aslts-bfield-normal-final-zy7jmyud/receipt.json` (reference),
`aslts-bfield-hosted-budget-g6w77_t1/receipt.json` and
`aslts-bfield-progress-final-ix4cexea/receipt.json` (preserved failures/diagnostic),
all under the private `.acpi-proof-work` evidence directory. No checked, n32,
o32 or o64 bfield pass, production proof, native result, or full ASLTS pass is
claimed. The opt-in [portable gate](../tests/aml-aslts-bfield/README.md) is now
installed after a separate release replay passed both invocations with the same
charges. Its final documentation/provenance refresh changed no executable or
reference corpus bytes; no additional run is claimed for that metadata refresh.

The pinned upstream inventory remains 342 runtime entrypoints at commit
`232ff3f8ae1a4da11c709f61d9154482cfe8e6df`. This is one newly exercised original
entrypoint, alongside the previously selected arithmetic, logic and control:
four selected entrypoints and 338 unselected. That selection count is not four
passes: full control remains incomplete. The earlier twelve-configuration
reports remain historical; this adds only the recorded bfield revision-2/default-optimized configuration. WHL depth derivatives and
repeated invocations are not additional entrypoints or configurations. No corpus
percentage is inferred.


### Bfield compiler-configuration correction

The preceding bfield result uses the full original revision-2 source compiled by
plain normal iasl with default optimizations. Historical `bfield-n64` filenames
and CLI selectors are retained as evidence identifiers, **not** a claim that the
upstream ASLTS n64 configuration ran. Upstream `Makefile.def` defines n64 as
`-oa -r 2`; its common flags additionally include `-of -cr -vs`. Those flags were
not used in this recorded oracle. Exact upstream o64 equivalence is not claimed
either. The recorded full-original-entrypoint pass, two repeated zero results,
limits and failures remain valid for its pinned AML. No new execution is implied.
Bfield still adds one original entrypoint selection, not multiple configurations;
no exact upstream n64/n32/o64/o32 or checked bfield pass is established.

The attempted original module collection is normal-compiler blocked: two missing
Device `_HID`/`_ADR` errors (6141) and an illegal `OB02` forward-reference error
(6142), with no generated AML or runtime. These compiler checks are present in
the same pinned ACPICA source, so a newer-compiler-only regression has not been
established. The entrypoint and raw diagnostics remain recorded; no force or
suppression was used.


## Original local explicit-configuration checkpoint

The full inventoried `functional/local/MAIN.asl` passed normal reference and
private release hosted execution with the explicit upstream common n64 flags
(`-oa -r 2`, common `-of -cr -vs` and listing outputs). Local is omitted from the
upstream default functional build; no default-suite-target claim is made. Two
same-arena MN00/ERRS results were zero. Read-only RMRC observations were 1 then 2,
and every RP0P completion record was exactly `:STST:functional:local:CMO0:PASS:`;
all observation handles were released. Charges were 888/903, module charge 1,046.
Normal 1,000,000 method/100,000 module fuel, 64-second runtime, 180-second build,
64 MiB stack/1 MiB logs and explicit 128-continuation capacity were retained.
Reference warnings/cache diagnostics are preserved. Evidence is
`aslts-local-hosted-final-d9bl7tw6/receipt.json` and
`aslts-local-normal-final-xdr79opj/receipt.json` under `.acpi-proof-work`.

The opt-in `tests/aml-aslts-local` portable relocation also passed release under
the same normal limits, including both completion-record checks and two isolated
input/optimized-Python guard negatives. No checked/other local configurations or whole ASLTS pass is
claimed. Original module remains compiler-blocked under both plain and exact
upstream flags (6141 twice,6142 once), with no AML or runtime, and is not counted
as an additional executed entrypoint or an interpreter failure. Of the pinned
342 original entrypoints, five have now been exercised (arithmetic, logic,
control, bfield, local); 337 have no runtime execution, including the blocked
module. Control remains incomplete. This is not five passes or a corpus
percentage; derivatives and repetitions add no entrypoints/configurations.

## Original descriptor compiler-blocked checkpoint

The full original `functional/descriptor/MAIN.asl` was attempted with the exact
upstream common n64 compiler flags, including `-oa -r 2 -of -cr -vs`, without
force, source edits or diagnostic suppression. Normal iasl returned 255 with
five error 6141 diagnostics: Device declarations GPII, GPIO, I2C, SPI and UART
lack `_HID` or `_ADR` (gpioint/gpioio/i2cserialbus/spiserialbus/uartserialbus).
The summary was five errors, 64 warnings and 45 remarks. This was a compiler
rejection, not a timeout or interpreter failure. No AML was emitted; disassembly,
header validation and runtime were not run. Listing/output-source sizes do not
establish a compiled AML capacity requirement.

The exact 47-file original include closure and normal tool inputs are preserved
in `.acpi-proof-work/aslts-descriptor-normal-oracle-b7upplfl`; its raw compiler
log is `results/descriptor-n64-iasl.log`. The independently hashed failure receipt
is `.acpi-proof-work/aslts-descriptor-normal-rejected-bkdk1y_4/receipt.json`.
Descriptor joins module as a recorded compiler-blocked original entrypoint.
Neither adds a runtime execution or pass: the existing five exercised original
entrypoints and 342-entrypoint denominator are unchanged.

## Original provoke reference-failed checkpoint

The full original `complex/provoke/MAIN.asl` compiled and disassembled with
the explicit upstream common n64 flags, without source edits, force or diagnostic
suppression. The resulting DSDT is 18,714 bytes, revision 2, with a valid length
and checksum. Compiler, disassembler and acpiexec process statuses were zero,
but the original reference semantic gate failed on both invocations.

Cases m130, m132 and m136 passed. Cases m133 and m134 expected Decrement through
a passed `RefOf(Local0)` to change the original 10 to 9; ACPICA instead reported
`AE_AML_OPERAND_TYPE` and left it at 10. The original `_ERR` handler returned
AE_OK to continue reporting, while CH03 explicitly counted the exception as
unexpected. Each failing case therefore produced a value mismatch and an
unexpected-exception error. Ordered MN00/ERRS results were 1/4 then 1/8; the
error count accumulates. This is not an intentional expected-error pass, a
compiler rejection, or a demonstrated historical regression. Allocation/cache
diagnostics remain preserved separately.

Raw evidence is `.acpi-proof-work/aslts-provoke-normal-oracle-k8zhb1fq/results`,
with hashed receipt `aslts-provoke-normal-failed-1ju7oj_m/receipt.json` and source
trace `aslts-provoke-source-diagnosis-pmu0q3zo/FINDINGS.txt` under the same
private evidence directory. No CuBit hosted execution was performed. Provoke
adds a reference attempt only; the existing five CuBit-exercised original
entrypoints and 342-entrypoint inventory denominator are unchanged.
