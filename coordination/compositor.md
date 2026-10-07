2026-10-07 Dependency blocked audit satisfied after three consecutive checks.
No further actionable handoff: Graphics revision52 unchanged/idle; no exactNUC
pipeline diagnostic; Networking/menubar ownership acknowledgment absent. Shared
publication still fails nonblocking lock; host ps confirms benchmark393114/393115
live at12minutes. Earlier checks were verified waits, but all available next
integration steps still require external state change. No own live jobs.
Goal marked blocked, NOT complete. Resume when source window is acknowledged,
benchmark releases lock for test publication, or physical pipeline record arrives.
Private tested UI candidate+nativeFiles, reusable visualtest and frozen diagnostic
artifacts retained at paths above. No speculative fixes, duplicate tests or image
changes. Full objective and its remaining hardware/performance gates unchanged.

2026-10-07 Verified wait continued: host ps confirms benchmark393114/393115
live with build.lock; bounded publication waiter66647 exited124 after55seconds,
no files published and no own job remains. UI/menus ownership and physical
pipeline diagnostic still unchanged. Previous and current goal turns are
verified waits, not completion; exact validated artifacts retained.

2026-10-07 Verified external build-lock wait; no competing workload launched.
Read-only host lslocks/ps identifies flock393114 and childbash393115 executing
Networking scratchpad ab-bench.sh, still live on recheck45seconds later. This
explains failed nonblocking publication; not a stale file or permission error.
Leave benchmark undisturbed. Exact validated focus test ready for locked copy.
No UI owner acknowledgment, no new physical pipeline record. Prior turnPROGRESS
(private nativeFiles linked); current turn verified wait on these live PIDs.
All own validation jobs terminal; frozen artifacts unchanged.

2026-10-07 Private repaired Files app native build PASS.
Pinned Alire GNAT16 compile+bind19488TERM0, link38053TERM0. Uses current Files
source and private UI candidate, existing current runtime/fonts/manifest; shared
outputs untouched. /tmp/cubit-ui-repair-1/native16-obj/files.app
SHA 173ef6fb9a834cb5d8904457f89390a8dc80ea531015e9dcb7e63989c4406c19. Not booted/staged, no native regression-pass claim yet.
This supersedes compile-only evidence using shell-default GNAT15; native build
now uses correct pinned toolchain. All own jobs terminal. Graphics confirmed
Networking/App/Surfaces and Servo/native-menubar claims and posted seven-file
idle-window request; await acknowledgment before shared edits. Focus fixture
publication still deferred by shared build lock; validated private copy intact.
Goal turn PROGRESS (full candidate native compile/bind/link and owner handoff).

2026-10-07 Reusable native focus gate validated; publication lock deferred.
Build34390TERM0 and native82373TERM0. Exact tested portable sources remain at
/tmp/cubit-focus-review-root/tests/compositor/focus-visual; replay evidence
/tmp/cubit-focus-replay-native-1/result.json. Runner integrates six changes,
three initial full-redraw pixel restorations and inactive-title strip checks.
Explicit boot/build inputs recorded, frozen Desktop unchanged. Publication to
tests/compositor/focus-visual still absent: last lock acquisition exited1; do
not bypass another shared build. Copy exact validated directory under lock in
next idle window. All own jobs terminal. Prior goal turn PROGRESS (nativevisual),
this turn PROGRESS (reusable fixture build/replay). Full goal not complete.

2026-10-07 Reusable focus visual gate being preserved/tested privately.
/tmp/cubit-focus-review-root/tests/compositor/focus-visual has fixture, explicit
frozen-runtime builder, runner with integrated initial-full-redraw/title-strip
pixel assertions, and scope README. Build34390TERM0; native replay82373live.
Shared publication attempts could not acquire build.lock, no shared tests edited.
Earlier shallow temporary path builder attempt failed before compiling; corrected
private directory layout and explicit --toolchain-root now pass. Ownership
request for seven UI repair files still pending (separate from this test).

2026-10-07 Native titlebar visual gate PASS; corrected stale task status.
Earlier2026-10-03 source fix already present (damagePreviousFocus); recent
messages saying artifact unresolved referred too broadly. Missing gate was
native visual confirmation. Exact released diagnostic Desktop f6fc080d contains
helper+focus/restore/maximize calls. Built private matching-runtime two-window
fixture80734TERM0 after correcting compiler selection to pinned alr GNAT16.
Native89929TERM0 /tmp/cubit-focus-visual-native-1,6 Alt-Tab changes,3 exact returns
to initial creation/full-redraw reference (excluding HUD/taskbar). Exposed old
title strip fully inactive, screenshot inspected. Pixelcheck77474 terminal0.
Fixture/scripts /tmp/cubit-focus-visual-1; no production or frozen image edits.
Software-only QEMU at100% DPI, no NUC/high-DPI visual/nativeGPU claim. Shared
UI repaint candidate still separate and not integrated. All own jobs terminal.

2026-10-07 Expanded private UI candidate PASS; no shared source changes.
Now seven files in /tmp/cubit-ui-repair-1/candidate.patch and sources.json:
previous five plus cubit-ui-menus.adb and cubit-ui-combo_boxes.adb. Audit found
both also registered through paint clips. Menu layout/hits use Input_Rect;
combo preserves its existing stable unclipped layout but uses Input_Rect for
hits. Removing that layout workaround failed existing pixel equivalence test;
it is preserved, all existing tests then pass.
3172TERM0 new full hit-map equivalence at100/125/200% for open menus/combos,
57885TERM0 CuBit native App compile-only,56323TERM0 existing menu/combosuites
and new regression. Combo includes100 pointer cycles,609 tiny fields,10
palette/density clipping cases. No native boot or new proof claim. All own jobs
terminal. Need acknowledged shared UI source window for seven files, then
shared apply/native Files and focus/damage regression. Request remains posted;
private work avoids shared source races. Full goal remains active/incomplete.

2026-10-07 Private hit-map candidate hosted PASS (19860TERM0).
/tmp/cubit-ui-repair-1/{candidate.patch,sources.json,result.json,check.adb}.
Five candidate files: cubit-ui.ads/adb, cubit-ui-app.adb, cubit-ui-widgets.adb,
cubit-ui-surfaces.adb. No shared edits. Input_Rect and With_Repair_Clip retain
layout/input constraints while preserving old drawing clip; With_Clip preserves
both and translated View carries both. Actual widgets/control map + pixel
sentinels at100/125/200%, nonzero origins, nested/empty clips pass. Original
hosted regression failed; candidate now retains Refresh after scrollbar repair.
No native or formal-proof claim. App Canvas wiring still needs native compile.
REQUEST Networking/UI owners: please acknowledge an idle source window for
these five files so candidate can be reviewed/applied and native Files tested.
Need audit other retained controls (e.g combo-box workaround) and explicit Canvas
aggregates before promotion. Frozen NUC diagnostic remains independent.
All own jobs terminal; goal turn PROGRESS.

2026-10-07 Private UI hit-map candidate /tmp/cubit-ui-repair-1 active.
Separate repair clip from input/layout clip, preserve nested parent constraints
and view translations. Actual widgets regression plus paint sentinel checks next.
Shared UI ownership request remains pending; no shared UI production edits.
Prior goal turn PROGRESS (diagnostic gaps,50 hosted cases,native fallback,handoff).

2026-10-07 Corrected pipeline diagnostic READY for Graphics packaging.
Missing required proc coverage: stage110 affine16,210 checker11,310 sources6;
explicit one-based indices in tests/compositor/pipeline-diagnostic/lookups.json.
Hosted26866TERM0: 50 cases including all33 null-proc exits,14 creation failures,
success,precondition guard,positive non-success. Updated patch/tests/stage key
published under shared build lock in tests/compositor/pipeline-diagnostic/.
Private build16115TERM0: /tmp/cubit-desktop-pipeline-diag-build-2
Desktop SHA f6fc080d3ac0c61ede35dd04294ccf053a95e5a3231acf25db9de272504d6f85.
Native68425TERM0: /tmp/cubit-desktop-pipeline-diag-native-2/result.json;
software fallback,3 menu restorations,32 cursor moves/8 round trips PASS.
All8 changed build sources match reviewed snapshot. All own jobs terminal.
No hardware pipeline execution or logsvc-delivery claim from these tests.
Graphics owns separate matched-system-budget candidate packaging; old28d39a8e
artifact superseded. Existing images unchanged; no shared production/UI edit.

2026-10-07 Task priorities updated for Graphics handoff.
1. Close every missing-proc diagnostic exit in affine/checker/source setup;
   explicit stable stage/index, inject each absent proc, preserve first failure.
2. Rebuild private diagnostic Desktop, repeat native software fallback, hand
   Graphics the new hash/manifests for a separately named matched image.
   Prior 28d39a8e artifact is superseded and MUST NOT be packaged.
3. Interpret physical NUC pipeline result with Graphics and fix supported cause.
4. Retain titlebar focus repaint regression and independent Files hit-map fix;
   coordinate shared UI ownership. Preserve SPARK policy/proof boundaries.
No frozen image or shared production source modified. Private diagnostics active.

2026-10-07 Pipeline diagnostic implemented privately at Graphics request.
Source /tmp/cubit-desktop-pipeline-diag-1 from exact quiet indicator baseline;
8changed files listed /tmp/cubit-pipeline-diag-tests-1/changes.json, patch there.
Scalar firstfailure stage/index/int32VkResult, no IPC/allocation/Mesa callbacks;
Ada Main emits after Prepare_Pipeline returns. Diagnostic FFI SPARK off,
policy/return behavior unchanged. Stage ranges100affine/200checker/300descriptors.
Build60421TERM0 SHA28d39a8ee1293fddb8fc9fb33ac445ee441add7cbe0da9a3ee2c328997e258d4.
Hosted82911TERM0 all14creation failures +success pass. Additional guard/positive
status tests running; native fallback59390live. No shared production/UI edits.
FrozenNUC image unchanged; artifact not released until native checks complete.

2026-10-07 New NUC pipeline fallback: read-only exact source trace delivered.
Graphics reports f0d2a706 policy quota2069889024, backing40->42MiB, metadata32->64,
targets passed; PIPELINE pair then SELECTED without upload/readback, startupCPU.
Exact Prepare_Pipeline child/preconditions + C affine/checker/source-descriptor
sequence reviewed. Affine creates layout/sampler/modules/3pipelines; checker
layout/modules/1pipeline; source initialization pool+140sampler sets. Every
Vulkan failure collapsed to1; no substage/VkResult record. Cleanup occurs before
return, so total backing snapshot cannot localize stage. Existing allocator
and device-lost logs may help; optional transport fixture hook absent from
exact linked binary (nm), not a promised log source. Suggested scalar stage/
index/signedVkResult captured C-side and emitted Ada after return if existing
logs insufficient; no instrumentation yet. Shared UI ownership/fix separately
pending; all own jobs terminal. Frozen images unchanged.

2026-10-07 Files regression REPRODUCED and root cause isolated.
Native9311TERM1 reproduces exactly both missing markers with fresh dependencies;
/tmp/cubit-files-pointer-repro-1.log and -run.log. Input still reaches152,139;
title ptr hit-down/drag-up present. Hosted39768TERM1 actual UI widgets/control
map repro /tmp/cubit-files-hit-repro-1: full canvas Refresh Hit=1, then Clear+
Button on scrollbar-only paint clip gives Hit=0 and fails assertion. This
establishes hit-map loss independently of logging/driver/event transport.
Fix needed: separate layout/input clipping from repaint clipping while retaining
all pixel writes inside repair region. Do not just remove paint clipping or
force whole-window redraw. Widgets use damage-clipped Parent_Canvas for hit
registration; app Canvas(win,damage) supplies that repair clip. Nested viewport
clipping must still constrain hit testing. Shared UI ownership request above
remains pending; no shared UI/Files edits. Native move fixture must replace
legacy retained-base marker with verified movement (existing drag-up evidence
alone is not a pixel-position oracle). Build lock released; all own jobs terminal.
Graphics notified; frozen system-budget image remains unchanged.

Files diagnosis update/request to Networking: native source confirms
prepareMoveBase returns immediately for nativeScene; legacy retained-move
marker is invalid for current default. Original serial has actual title
hit-down/drag-up, so that failure is fixture drift.
Likely independent Refresh defect: Files.Render uses Canvas(win, damage) and
Controls.Clear; Widgets.Button registers Clamp_Rect(Parent_Canvas(c, toolbar),
bounds). A scrollbar-only repaint can therefore rebuild the map without the
Refresh control. Request ownership coordination for shared UI widgets/canvas
fix or your preferred owner; I will first establish hosted regression, not
blindly broaden drawing beyond owned repair bounds. Current native repro9311
running under shared build lock, /tmp/cubit-files-pointer-repro-1*.log; it is
rebuilding dependencies (not yet native test outcome). No shared UI edits.

2026-10-07 Files regression handoff accepted, read-only diagnosis/repro.
Networking20:40 reports missing Refresh/move markers. Original serial
 tests/net-tcp/build-tmp/nix-shell.AuhDKK/cubit-headless-files-serial.log shows
pointer reaches152,139, releases, keyboard navigation works, title hit-down and
drag-up; native output rendering active (no legacy dragBase allocation). Scope
Desktop/Files event tracing and tests/headless/run.sh fixture; not editing Files
or shared UI source yet. Request to Networking via folder: preserve input/log
sources during reproduction; I will coordinate any shared-script edit under
build lock. Frozen NUC image remains untouched. Fresh native repro next.

2026-10-07 Hardware dependency audit3: blocked audit satisfied.
Third consecutive goal turn with unchanged NUC/CCL dependency. Graphics
revision51 unchanged, physical candidate test pending, no Desktop request;
all validation jobs terminal. Shared optional-render absent and compiler owner
reservation remains. Completed current compositor artifact/tests/review; cannot
advance matched hardware integration or default activation without external
result/handoff. No duplicate tests, speculative changes or image overwrite.
Mark broad goal blocked/incomplete. Resume on NUC result, actionable Graphics
request or CCL ownership handoff; original full acceptance gates remain open.

2026-10-07 Hardware dependency audit2.
Previous goal turn no-progress audit1. Graphics confirms NUC test pending and
is independently reviewing driver client accounting; no Desktop handoff or
physical evidence. Shared CCL optional-render support remains absent/reserved.
No own jobs or concrete live validation handle, no changed compositor source.
No-progress audit2 for same hardware/CCL dependency; goal active/incomplete.

2026-10-07 Hardware dependency audit1 after candidate validation.
Last goal work established final exact-image tools PASS (progress); intervening
handoff acknowledgment added no work. Revalidated ready candidate and all jobs
terminal in Graphics note; no new NUC result/source request. New Graphics turn
alone is not a live process/test handle. CCL optional-render support still absent
under owner reservation. No safe additional integration change follows without
handoff/evidence; no duplicate runs or speculative renderer changes. Goal active,
no-progress audit1 for current hardware/CCL dependencies.

2026-10-07 Exact system-budget candidate validation COMPLETE (Graphics).
Read authoritative command exec-fdfd80ed terminal0: quiet-xHCI corrected
USB/UEFI4CPU graphics-tools PASS /tmp/cubit-usb-live.nvie6sla. Prior noPS2
PASS crayi8wt retained. Final rehash f0d2a706e460a5bc49e89d5a79654716eeceefbb7e496d45125b4eb0a9970b2c
unchanged. New result advances release gate; no blocked mark this turn.
Next discriminating NUC evidence: system backing policy accepted/quota, all
startup checkpoint pairs through readback, startup READY/SOFTWARE, liveHUD,
input/Apps responsiveness. Neither QEMU test establishes GPU frame success,
physical latency or fairness. No own source/image changes or live jobs.

2026-10-07 Dependency audit2 after quiet-mode retry.
Previous turn no-progress audit1. Graphics bounded snapshot revision47
unchanged; no new failure/result/source handoff. CCL optional-render absent
and reservation unchanged. No own jobs or accessible live test handle; not a
verified wait. Existing validated Desktop preserved, duplicate tests would not
advance current gate. Goal active/incomplete; same integration dependency,
second consecutive no-progress audit.

2026-10-07 Dependency audit1 after candidate boot evidence.
Previous turn advanced noPS2 evidence. Graphics reports tools launched but
final harness assertion expected suppressed xHCI diagnostics; owner retrying
with existing quiet-xHCI option, unchanged image. No compositor defect/request
identified, no own accessible live process handle. Shared CCL optional-render
still missing/reserved. No implementation progress this turn; remain at same
integration dependency until completed result or handoff. Goal active, own jobs
terminal, no duplicate tests/edits/images.

2026-10-07 Matched candidate validation advances.
Graphics reports native12099 compile/private80639 link PASS and noPS2 exact
image PASS /tmp/cubit-usb-live.crayi8wt; directory and capture confirmed.
Graphics-tools evidence /tmp/cubit-usb-live.o6_843br contains final tools capture;
no terminal result claimed here until owner confirms. Owner session8201 not
accessible via this thread write_stdin (Unknown process id); that cross-thread
lookup failure does not prove process terminal. No duplicate run/restart.
No Desktop source/image change. Prior turn no-progress audit1; new exact-image
boot evidence advances validation, but GPU hardware/full goal remain unverified.

2026-10-07 Dependency audit1 after harness-path diagnosis.
Previous turn progressed identifying AF_UNIX harness failure and corrective
handoff. Latest Graphics says corrected image checks running but retrieved
markers terminal; no exact live process handle confirmed here. Bounded thread
snapshot unchanged, so not classified verified wait or test success. Candidate
f0d2a706 remains awaiting result; no new compositor request or changed Desktop
source. No duplicate builds/VMs, no image edits, no own jobs. No-progress audit1;
full goal active/incomplete with native result and CCL handoff pending.

2026-10-07 Candidate launch failure diagnosed; sent corrective handoff.
Previous turn no-progress audit2. New authoritative evidence: Graphics packaged
system-budget image f0d2a706e460a5bc49e89d5a79654716eeceefbb7e496d45125b4eb0a9970b2c.
Exact-image test failed before guest validation: AF_UNIX path too long under
private-workspace TMPDIR at monitor connect. Suggested inner export TMPDIR=/tmp
for short unique test paths, preserve failed evidence and handle own VM cleanup
before retry. No duplicate run/source edit. This is harness failure, not GPU/
Desktop regression; image not yet validated. Concrete new diagnosis/handoff is
progress; blocked audit not met. Broad goal active/incomplete.

2026-10-07 Dependency audit2: no new compositor handoff.
Prior turn no-progress audit1. Graphics latest private matched devmgr/intel
build command terminal; preparing exact-image packaging/tests, no process
handle supplied here and no new Desktop request. CCL optional-render support
still absent in reserved shared compiler/schema. Existing Desktop regression
remains authoritative; no changed source to justify rerun. No new edits beyond
this audit, no own jobs, goal active/incomplete. Same dependency condition;
second no-progress audit, not yet blocked threshold.

2026-10-07 Dependency audit1 following production-policy review.
Previous turn completed independent review (progress). Revalidated Graphics
now preparing matched private packaging; latest visible commands terminal,
no live build/test handle supplied to this thread. No additional Desktop change
requested. Frozen1d2112d2 native result still PASS; root CCL optional-render
support still absent under unchanged owner reservation. Do not duplicate
Graphics packaging or regenerate unchanged Desktop. No implementation/evidence
advance this turn; no-progress audit1, goal active/incomplete. Await concrete
candidate result or coordinated source handoff, not a claimed verified wait.

2026-10-07 Read-only backing policy integration review sent to Graphics.
Reviewed actual shared Native_System_Heap expression, both endpoint callsites,
supervisor growth callback, Configure_Heap admission and Memory_Budget. Shared
query1601 and rounded min(RAM/4,DMA/2) match; configure occurs before first
attempt. Potential stale32MiB preallocation query ruled out: budget unknown
until directory committed bytes nonzero, then reports configured Object.Limit.
No actionable mismatch found in inspected change. No source/test/image edits;
review does not establish native runtime behavior, fairness or resource supply.
Next gate matched native candidate then all startup stages and workload onNUC.
Own jobs terminal; full goal incomplete. Prior audit no progress; this turn
completed independent review of new production policy, evidence sent to owner.

2026-10-07 Dependency audit1 after memory-budget review.
Previous goal turn progressed by identifying post-target startup/runtime budget
coverage and correcting its shared-owner scope. Current audit read Graphics'
active policy work; no own or supplied live process handle to classify as a
verified wait. Shared optional-render compiler/schema support still absent;
CCL typed-manifest ownership reservation persists. Frozen Desktop1d2112d2
already passed native fallback; no changed Desktop behavior to rebuild/retest.
Asked Graphics for next concrete Desktop integration/review handoff. No source
or image changes this turn; classify no progress toward implementation. Goal
remains active/incomplete; first dependency audit, not blocked threshold.

2026-10-07 Memory admission audit: post-target allocations sent to Graphics.
Previous turn progress repaired current logging regression. Current exact
indicator source audit confirms single128MiB owner Budget shared by targets,
upload/readback and runtime source backings. Targets-only16+3x8MiB=40MiB
fixture omits later pipeline,2MiB upload,full-output readback (~8MiB1080p),
and normal runtime sources. Approx50MiB before extra allocations/padding is
not a measured total or sufficient quota recommendation. Graphics notified to
validate complete readiness/workload under trusted shared/client policy.
Corrected docs/compositor-backends.md budget scope and physical-denial evidence.
No production/image changes or live jobs; whole goal still incomplete.

2026-10-07 Goal continuation: hosted logging regression repaired, PASS.
Previous goal turn made concrete progress: frozen stats artifact built/native
validated and handed off. Latest physical quota evidence changes next action;
Graphics owns trusted backing-budget integration, not awaiting more NUC data.
Independent gap fixed in tests/compositor/test-desktop-logs.py under shared
build lock: retired CQ fixture replaced by current immediate Emit fixture.
Real Desktop_Logs/Text_To_Log compiled unchanged. Tests check split CRLF lines,
serial echo,40 immediate publications, oversized/control-byte drop reports,
1000 rejected writes bounded to two attempts each, and accepted recovery with
cumulative loss accounting. No transport ring/proof/native claims from fixture.
Nix50503TERM0 PASS; evidence tests/compositor/build/desktop-logs-host-87l62e6z.
Diff whitespace clean. Production sources/artifacts/images unchanged; no own
live jobs. Broad goal incomplete; continue integration with Graphics budget fix.

2026-10-06 Frozen indicator quiet artifact READY; sent to Graphics.
/tmp/cubit-desktop-indicator-quiet-build-1/desktop-vulkan-compositor.svc
SHA256 1d2112d21bed78ab87395b90d4b7f9bd7501f4edec55ad52087aedfab033f5b0.
Build87422TERM0; native72410TERM0 PASS (software fallback,3menu restores,
32cursor moves/8roundtrips; oracle excludes deliberate HUD strip). Evidence
/tmp/cubit-desktop-indicator-quiet-native-1/result.json; screenshot inspected
selectedCPU loop143/frame41/key18/mouse34/button2/request0. Full manifest diff
only Main periodic-stats serial call/comment; remaining entries identical to
indicator3. Original binary hash eca2acbd unchanged. No native GPU/NUC claim;
logsvc bypass verified at callsite, no separate collector assertion. All jobs
terminal; Graphics authorized separately named candidate packaging.

2026-10-06 Graphics requests frozen indicator3 stats-suppression artifact.
Private source /tmp/cubit-desktop-indicator-quiet-1 verified matching original
source manifest before sole Main stats call edit; private build
/tmp/cubit-desktop-indicator-quiet-build-1 active session87422 under Nix.
Existing source/artifacts/images immutable. Native regression pending.

2026-10-06 Exact target-size trace delivered to Graphics (read-only).
Indicator3 creates three sequential BGRA8 optimal-tiled full-output images,
one mip/layer/sample, color attachment + transfer source. At 1080p nominal
8,294,400 pixel bytes each; allocation uses vkGetImageMemoryRequirements2.size
unchanged with dedicated-image pNext, not pixel-byte calculation. ANV may add
CCS/alignment; native gem_create rounds to 4096 and caps each request at16MiB.
Current logs do not expose exact requested bytes or failed target index. Record
bytes at native create-buffer/driver boundary with denial subreason. Final
charged scene bytes=0 follows clean rejection/release; cannot infer zero request
or first-image failure. Graphics owns devmgr denial instrumentation/fix.

Nix serial input parser regression PASS (2 tests). Stats suppression handoff
sent to Graphics; Main released for candidate packaging, no own live jobs.

Stats logsvc suppression validation: git diff --check clean. Nix hosted
Desktop logging harness failed compiling its pre-existing obsolete interface
(Collect/Pump/Matches absent from current write-only Desktop_Logs); unchanged
harness/API mismatch, not a passing logging test. No native build/image claimed.

2026-10-06 Graphics returns Main ownership for stats logging change.
Implemented narrow logsvc suppression: periodic desktop: stats now uses
CuBit.Messages.debugPrint directly (serial only), bypassing Desktop_Logs.Write.
Existing serial consumers retain their exact record; metrics publishing/reset,
HUD counters, startup and failure logging unchanged. This does not disable
serial emission or counter collection. No build/test script or image changes.
Graphics owns allocation backing denial diagnosis: reason7=Backing_Unavailable,
stage10=Denied after explicit F001 zero-word receipt, not allocation timeout.

2026-10-06 TASK UPDATE — aligned with Graphics at user request.
Current priority: diagnose exact indicator3 native render-target setup, read-only
first. NUC TARGETS_BEFORE 00:07.703 -> TARGETS_AFTER 01:29.420 (~81.7s),
explicit startup=SOFTWARE confirms initial fallback. First event_drop=83506 is
consistent with startup backlog, not proof of the allocation failure mechanism.
New Graphics photo reports allocation unavailable reason=7, backing stage=10,
scene allocation bytes=0; Graphics owns exact-image driver enum decoding.

Ordered compositor tasks:
1. Trace Configure_Targets -> three owned Vulkan images -> native allocation;
   identify blocking/failure/cleanup boundaries and useful existing diagnostics.
   Correlate with Graphics' exact driver decode before proposing a fix.
2. Coordinate the smallest fix and any missing allocation failure diagnostics
   with Graphics; preserve bounded ownership and functional CPU fallback.
3. Validate an agreed candidate in native CuBit, including input responsiveness,
   allocation failure and fallback. Physical NUC confirmation remains required;
   hosted results cannot establish hardware correctness or latency.
4. Return to compositor performance and rendering defects after this blocker;
   defer speculative per-frame async redesign until evidence warrants it.

Ownership handoff: shared userspace/services/desktop/main.adb RELEASED to
Graphics for narrow periodic desktop: stats removal/default-off and coordinated
fixture changes. Preserve metrics publishing/reset, HUD counters and startup/
failure diagnostics. Existing tests consume exact stats text; retain meaningful
assertions via explicit test opt-in or updated fixtures. Shared build/test script
edits require build lock. Release sent directly to Graphics. No own active builds
or source edits. Existing NUC images must remain unchanged; no commit/push.
This updates priorities and ownership, not completion status of the broad goal.

2026-10-06 physical indicator advanced; fallback log interpretation.
Graphics relays blue transient then Desktop, user reports keys responding.
Photo selectedCPU loop4289/frame733/key0/mouse741/button0/request3583; snapshot
key0 not used to contradict user. Live CPU label does not prove startupCPU:
READY can later recoverCPU; first inspect startup=READY/SOFTWARE plus renderer
retired/fullsoftware repaint marker and frameCOMPLETE/PUBLISHED. Existing paired
CHECKPOINT logs denote returns/branches, not success booleans or VkResults.
Configuration gate oneoutput/primary0/enabled/output<=16MiB is not logged.
Health/noTargets and upload/readback failure remain ambiguous; absent records
cannot establish failure under loss. SOFTWARE precedes GPU.Stop; SELECTED follows
Start_Renderer return. Raw Mesa startup result discarded locally in FFI adapter.
Sent exact field map to Graphics. No persistent-blue variant or source changes;
next ask existing Logs chain before choosing bounded summary/timing instrument.

2026-10-06 indicator photo bootstrap-only: read-only boundary audit.
Graphics relays photo of blue Desktop-reached screen, persistence not yet
confirmed. Exact indicator3 leaves identical pixels during bootstrap receipt
wait/unconfirmed hold, all renderer startup phases, CPU fallback GPU.Stop, and
initial event/request/capture/submit/readback until first complete normal frame.
No HUD cannot identify Initialize as stalled. Proposed if persistent: CPU status
checkpoints through authenticated pool before each startup stage while selection
unselected, final readiness before Configure, CPUcleanup checkpoint before Stop.
Each stage confirms its own release before nextoperation; missing release retains
ownership. Label checkpoint-before-X, not definitive call-entry/hang claim.
Need per-stage hold fixtures and final first-framehold. Existing images unchanged;
no own live processes or new implementation. Await coordinated persistence result.

2026-10-06 resumed dependency audit3: blocked pending evidence/handoff.
Third consecutive resumed audit; previous turn no progress. Graphics now idle
and explicitly blocked pending indicator NUC test. No new physical result or
source request, no own live jobs. CCL reservation/missing support unchanged;
prepared patch still applies. Existing ready indicator discriminates next branch;
additional speculative variants/repeated tests do not resolve this dependency.
Mark compositor goal blocked/incomplete. Tested images/evidence preserved; no
production or image changes. Resume on indicator result or CCL ownership release.

2026-10-06 resumed dependency audit2: unchanged.
Previous turn no progress (audit1). No indicator NUC evidence, CCL ownership
release or shared optional-render support found. Graphics new turn active but
no live process/test handle; all own tests terminal. Existing tested indicator
is still the discriminating next physical action. No repeated tests/new source
changes. Goal remains active/incomplete through second resumed audit.

2026-10-06 resumed dependency audit1 after healthy physical software A/B.
Goal tool now active. Previous physical evidence changed next action to existing
indicator trial; this turn no new implementation/test result. Revalidated
Graphics note: indicator69aae60b still untested NUC; all prior root build/test
jobs terminal. Graphics thread active but no specific live tool/process to await.
CCL reservation and missing optional-render support unchanged. No source/image
changes or repeated tests. Next safe discriminating action depends on indicator
backend/counter result or shared compiler ownership release; first audit after
resume, not yet blocked threshold. Original full goal remains incomplete.

2026-10-06 PHYSICAL software A/B healthy; next recommendation delivered.
Graphics reports verified2ab1bf3b NUC cursor/click/key/Logs/Console all work.
Same kernel/services/apps except Desktop bypass GPU startup. This isolates a
Vulkan startup/selected-path dependency, not a sole synchronous-submit cause.
Reviewed exact indicator3: next use existing69aae60b, record selectedGPU/CPU and
counter deltas around motion/key/Apps click/release/further motion. HUD updates
only on completed frames; frozen HUD is not proof of missing input, framecount
is not scanout/photons, requests include background clients. GPU+regression ->
per-stage submission/readback timing; CPU+regression -> initialization/cleanup
or side effects; healthyindicator -> reproduce original, account observer effect.
No new variant/image/source changes. Initialization-with-retained-resources but
CPU-before-first-GPU-scene is a possible later isolation, not implemented.
No own live jobs. Physical accelerated performance and sharedCCL still unproven.

2026-10-06 PROGRESS native cooperative vs blocking backend comparison.
Graphics relayed user overnight authorization; private job80552TERM0 both modes
PASS. Cooperative5s Pending:7input events/49Desktop GET_INFORMATION replies.
Synchronous5s inside-call delay:0input/0replies untilreturn, then progress resumes.
Actual Main/facade with test-only injection and headless typedrequestprobe;
not nativeGPUexecution or NUC timing/cause. No structural starvation found when
backend yields Pending. Admission budgets cannot preempt entered FFI; safe fix
requires async submission/completion or separate bounded render execution with
ownership proof, not timeout-as-retirement. Graphics independently matched exact
linked Mesa source synchronous capCall path; no speculative production change.
Evidence /tmp/cubit-dispatch-{pending,blocking}-native-1 and
/tmp/cubit-dispatch-comparison-result.json; review JSON saved under startup-
diagnostics when lock available. Indicator image69aae60b exactboots pass per
Graphics; software_ab2 remains first physical comparison. All own jobs terminal.
Next genuinely missing evidence: NUC software/indicator result; shared CCL release
still separate. Preserve all three images/candidates.

2026-10-06 PROGRESS visible indicator native21621TERM0 PASS.
Private source /tmp/cubit-desktop-indicator-3, artifact
/tmp/cubit-desktop-indicator-build-3 SHA
 eca2acbdd8d4f80494a1efd462e78d1fb95816517fb830ef0bcc212cb218d216.
CPU overlay writes only after Complete_Output and before BP.Present; guarded
pool rendering/transfer writable/nonemptywriter,900x64physicalclamped damage
piggybacks existingframes. No new timer/CQpoll/per-eventlog. Selected label reads
live facade Full_Output (CPUfalse/GPUtrue); not proof of completedGPUframe.
Cumulative saturating loop/key/mouse/button/request/frame counters. Native3menu/
32cursor checks PASS excluding intentionalHUDregion; separate click changes
buttonrow. Screenshot visually shows selectedCPU,key18,mouse34,button2,request0.
Evidence /tmp/cubit-desktop-indicator-native-3/indicator-result.json and
indicator-after-click.png. Earlier indicator1/2 also passed;3is released successor.
Root software A/B image2ab1bf3b exactboots pass, baselinepayloads verified.
Next authorized overnight audit: cooperative Pending vs synchronous backend-call
stall fixtures with native input/request activity. Graphics found synchronous
capCall under driver QueueSubmit; exact linked-source attribution pending.
No unsafe timeout/reuse or hardware inference. No own live jobs/images changed.

2026-10-06 PROGRESS forced-software A/B candidate native PASS.
Graphics authorized private A/B+visible counters following physical regression.
Software-first job95525TERM0 build/native gate PASS. Source
/tmp/cubit-desktop-force-software-1 copied a9a source; only Start_Renderer replaced
with false-readiness one-shot Configure_Renderer, no GPU.Initialize/Configure/
Stop in startup. Original artifact/image preserved. New artifact
/tmp/cubit-desktop-force-software-build-1 SHA256
69167fe615d115a819d3fb39d18e2859ed56088659b877c06aa63e07fd7bc31d.
Native /tmp/cubit-desktop-force-software-native-1/result.json: release before
forcedsoftware marker, no INIT_BEFORE,3menus/32cursor restoration pass. Released
Graphics for distinct same-services/config image packaging and exact-image tests.
No HUD in this first A/B. Next private indicator: CPU overlay after completed
render/readback and before presentation, within writer ownership; add bounded
HUD damage only to existing frames. Cumulative loop/input/request/frame and
selected backend; no timer/per-event log/hidden CQ polling. No own live jobs.

2026-10-06 new physical report; requested read-only loop audit delivered.
Graphics reports brief blue bootstrap then Desktop; no app/click/key response,
stuttering cursor. This establishes progress, not GPU selection. Exact a9a Main
input/request/input drains precede painting; synchronous handler/FFI costs remain
outside admission budget. GPU Full_Output forces full scene for cursor damage;
readback256KiB per advance means32chunks1080p/127chunks4K. Pending may wait1ms
unless fresh work suppresses wait. Bridge uses QueueSubmit/GetFenceStatus, no
explicit WaitForFences there; underlying driver call latency not established.
Suggested controlled software-only-from-start A/B with same services/config,
then bounded screen-visible backend/event/request/stage diagnostics through
normal ownership. No live unsafe switch or image overwrite; no cause claimed.
No edits to source/image. Await coordinated targeted change from Graphics;
read-only scope requested, original goal status remains blocked pending handoff.

2026-10-06 scoped EDID ownership release to Graphics.
Graphics requested bounded detailed timing sync offset/width/type/polarity
metadata for later Intel admission. Checked four files clean relative to Git;
no compositor edits/builds depend on them. Release Graphics ownership of
userspace/lib/display/cubit-monitor_edid.ads/.adb and
 tests/monitor-edid/main.adb, tests/monitor-edid/README.md for that scope.
Preimages ads3cc3294d, adb b621f6c9, test427bacf3, README0fb11baf.
Existing consumer virtio-gpu reads Parsed.Preferred; preserve nominal refresh,
physical-size and allocation semantics; compile consumer after record changes.
No Display Main, MMIO, default mode or compositor candidate changes authorized
by this handoff. Existing strict SPARK/runtime gates and spec cross-check apply.
This coordination release does not resolve CCL/nativeGPU goal blockers; goal
remains blocked and incomplete. No own test/build processes.

2026-10-06 dependency audit3: compositor goal blocked, incomplete.
Third consecutive audit of same post-diagnostic integration dependency. CCL
reservation/missing optional-render compiler support unchanged; patch still
applies. No ownership release/user answer/new NUC result. Graphics independently
adds driver allocation tests; those do not authorize shared compiler changes or
provide nativeGPU Desktop evidence. No own live process; all declared diagnostic
native gates complete and artifacts preserved. Meaningful next compositor
integration requires CCL handoff or hardware/backend evidence. Mark goal blocked
rather than re-run completed tests. No production/image/staging changes.

2026-10-06 dependency audit2: unchanged external integration gates.
Previous turn no progress (dependency audit1). Rechecked CCL reserved files,
missing optional-render syntax and prepared patch applicability; unchanged.
Graphics is assessing separate driver work, but no new NUC result or requested
compositor change. No own live jobs or outstanding test handle. No safe shared
activation without owner handoff; completed diagnostic checks are not repeated.
Goal active/incomplete through second audit; no source/image changes.

2026-10-06 dependency audit1 after diagnostic handoff.
Previous turn completed exact-image evidence; this turn yields no new
implementation/test result. Revalidated CCL reservation and missing optional-
render compiler/schema support; prepared patch still applies. No owner release
or user answer observed. Graphics note confirms all packaging/boot jobs terminal;
new thread turn active but no live build/test handle yet, so not a verified
process wait. No new physical NUC evidence. Independent diagnostic work complete
at declared scope; shared activation needs CCL handoff, nativeGPU recovery/
performance needs supported hardware. Goal active, incomplete; first repeated-
blocker audit after diagnostic progress. No source/image/staging changes.

2026-10-06 PROGRESS packaged diagnostic native gates terminal.
Graphics root session7706 terminal0 per inspected command output: exact e5b02672
image USB/UEFI noPS2 PASS /tmp/cubit-usb-live.c9tmhx4a; separate packaged Apps
Console/Logs PASS /tmp/cubit-usb-live.rm_g1_m4. Independently inspected serial:
bootstrap released, startup SOFTWARE, Console first frame, Logs window ready.
Image hash unchanged by tests. This is exact-image QEMU software evidence,
not NUC/Intel accelerated Desktop or physical latency. Old image preserved.
No own live jobs; no production changes. Next external evidence: physical NUC
trial of separately named diagnostic image and CCL ownership/integration release.

2026-10-06 PROGRESS durable diagnostic review and packaged-image handoff.
Previous turn progressed private review patch dry-run. Shared lock became
available; held throughout both edits publishing tests/compositor/startup-diagnostics/
diagnostic.patch, review.json and10hashed result records. Review overlay is
against frozen71ae source only, not current shared Main. No default activation
or shared production/compiler edits. Source artifact a9a unchanged.
Graphics reports packaging23161TERM0; separate image
cubit_intel_startup_diagnostics_20261006.img SHA256
 e5b026720eafd12f4eeaad09d222a9012b2392c58c7cfc09ab97d0c800aa45d7.
Extracted Desktop matches a9a; previous4bec image unchanged. Graphics exact-image
USB/UEFI noPS2 +Console/Logs job7706 live; inspect via Graphics thread, do not
restart/duplicate. No own live jobs. Native GPU/recovery/performance and shared
CCL handoff remain open. Physical diagnosis cannot follow from packaging alone.

2026-10-06 diagnostic review preservation; integration dependency check.
Previous turn progressed completed gates/frozen handoff. Graphics authoritatively
active revision33 packaging separate image, no own native process. Reviewed
remaining recovery scope: actual hosted Vulkan adapter/policy tests and native
software already pass; native GPU recovery needs native backend. Shared CCL
optional-render compiler support still absent and ownership handoff unresolved.
Prepared /tmp/cubit-startup-diagnostics-review-20261006/{diagnostic.patch,review.json}
with exact frozen71ae preimages/current a9a source hashes and evidence handoff.
Shared build lock unavailable, so did not publish into tests/compositor or edit
shared sources. Patch dry-run handle77614 polled terminal separately. Review
bundle is against frozen source only, not current Main. No goal completion claim.

2026-10-06 PROGRESS declared diagnostic gates complete; Graphics handoff.
Original matrix34162 terminal1 at backward screenshot comparison: capture raced
Display finishing already submitted frame, not unsafe renderer start. Original
evidence preserved. Corrected private observer waits matching withheld receipt;
11130TERM0 backward/native-confirmed PASS, then frozen PASS with explicit2000
poll exhaustion and no INIT_BEFORE. Unavailable also PASS. All six matrix cases
plus separate lost/capture/logsvc gates complete at native software fixture scope.
Final28725TERM0 artifact/source identity guard and evidence hashes recorded in
/tmp/cubit-bootstrap-diagnostic-handoff-20261006.json. Exact artifact released to
Graphics for separate packaging/USB-UEFI gates:
/tmp/cubit-desktop-visible-build-2 SHA256
 a9a9965e5f548e793c5dae0bf182ecb94a67675a28e4c89f497445efff860ab4.
Candidate unchanged throughout fault testing. Fault/screenshot/observer binaries
are explicitly different test variants; selection policy proof conditional on
readiness. No native GPU, physical visibility,240Hz/1ms or NUC fix claim. Static
screen only identifies reaching Desktop; no guarantee every log survives load.
All own jobs terminal. Graphics owns new-image packaging and exact-image tests;
old4bec image must remain unchanged. Full compositor goal remains incomplete;
shared CCL publication/nativeGPU/hardware measurement gates remain.

2026-10-06 PROGRESS receipt-fault cases pass; clock cases live.
Resumed same34162 job (previous turn progress). Matrix results.json now has
PASS delay, rejected, stale. Rejected fixture sets kernel-valid false; stale
fixture changes nonzero frame number with token/session retained. Both native
serials show asynchronous transfer quarantined, process stopped, owned regions
retired12, and no INIT_BEFORE. Stable screenshots are observations, not proof
that an exited process retains storage. No mutation to normal candidate.
Job34162 still LIVE, now unavailable-clock build; backward/frozen queued. Poll
same handle. Evidence /tmp/cubit-bootstrap-fault-matrix-1/{rejected,stale}/native.
Packaging remains gated on terminal clock results. No shared/image changes.

2026-10-06 PROGRESS remaining bootstrap fault matrix running.
Previous turn progressed actual logsvc delivery. Prepared six independent native
fixtures /tmp/run-bootstrap-fault-matrix.py; sequential Nix job34162 confirmed
LIVE. Poll same handle; do not restart on timeout. Log
/tmp/cubit-bootstrap-fault-matrix-1.log. Delay250ms receipt case has terminal
build/boot PASS (including3menu cycles); job now compiling rejected receipt.
Remaining queued modes: rejected kernel validity, mismatched nonzero frame,
unavailable bootstrap clock, backward clock, frozen clock + withheld receipt.
Frozen must execute2000poll attempts and emit exhaustion marker; no time-based
permission. Rejected/stale must quarantine before INIT_BEFORE. Each has separate
source/artifact/native evidence under /tmp/cubit-bootstrap-fault-matrix-1.
Candidate SHA a9a9965e reverified unchanged. No packaging/shared production/
image changes. Do not report whole matrix passed while job is live.

2026-10-06 PROGRESS real logsvc observer native gate.
Previous turn progressed screen capture and missing receipt. Built private
headless SDK Reader with matching runtime92328TERM0; no Desktop/UI dependency.
Native33972TERM0 PASS /tmp/cubit-desktop-log-observer-native-1/logsvc-result.json.
Observer receives DESKTOP-CHECKPOINT: ENTERED before INIT_BEFORE/mainloop while
capture fixture holds5s, then INIT_BEFORE/INIT_AFTER/SELECTED and first-frame
markers; no reader gaps.3menu restorations/32cursor moves pass. This verifies
actual logsvc delivery independently of Desktop_Logs.Pump, not merely serial
mirroring. Fixture is separate1b7bd3cb; candidate a9a unchanged. Does not prove
all stage markers survive collector saturation or locate the NUC stall.
Observer source/build /tmp/cubit-desktop-log-observer-1;
builder /tmp/build-desktop-log-observer.py; runner /tmp/test-desktop-log-observer.py.
Remaining declared diagnostic gates: delayed/rejected/stale receipt and invalid/
backward/frozen clock tests; final frozen artifact handoff. All own jobs terminal.
No shared production/default/image changes. Original compositor goal incomplete.

2026-10-06 PROGRESS bootstrap screen capture and missing-receipt native gate.
Previous turn was progress (candidate native transition). Capture fixture99959
terminal0 PASS: /tmp/cubit-desktop-bootstrap-capture-native-1/bootstrap-status.png
visually inspected; five-second TEST-ONLY hold after authenticated release,
then software startup/3menus/32cursor moves pass. Fixture source/build paths
/tmp/cubit-desktop-bootstrap-capture-{1,build-1}; candidate a9a unchanged.
Missing-completion fixture39370 terminal0 PASS: normal dispatcher deliberately
withholds matching bootstrap receipt before CP.Complete; wait reports unconfirmed,
never enters INIT_BEFORE, retains pixel-identical screen over2seconds. Evidence
/tmp/cubit-desktop-bootstrap-lost-native-1/result.json and serial.log. This is a
Desktop-side receipt fault, not a claim Display still owns the buffer physically.
Graphics independently verified actual Configure_Renderer Accepted => previously
Unselected, native release->INIT->SOFTWARE ordering, hosted128readiness/16384
reselection/64drain cases, all-true GPU selection; scoped GNATprove terminal0
/tmp/cubit-bootstrap-selection.eyOVvG/gnatprove/gnatprove.out. No native GPU claim.
Still open: delayed/rejected/stale receipt and clock fault gates, independent
pre-mainloop logsvc observer, final frozen diagnostic handoff. All own jobs
terminal; no shared production, image, commit or push changes.

2026-10-06 PROGRESS private CPU bootstrap compile and native transition.
Previous task-list turn was planning, not implementation evidence; continued
with builds3497 and95561, both terminal0. Graphics review corrected one-shot
Complete/Submit/Released marker consumption and frozen-clock polling: bootstrap
suppresses renderer markers; wait has2000attempt bound plus2s clock deadline.
Kernel calls must still return; no hard wall-clock guarantee. Exact candidate:
/tmp/cubit-desktop-visible-build-2/desktop-vulkan-compositor.svc SHA256
 a9a9965e5f548e793c5dae0bf182ecb94a67675a28e4c89f497445efff860ab4.
Initial native92633 stopped at old harness startup assertion immediately after
internal-shell activation, before bootstrap release. Private harness
/tmp/test-desktop-visible-boot.py explicitly waits and asserts release-before-
INIT_BEFORE plus no renderer Complete/Submit marker consumed by bootstrap.
Native18563 terminal0 PASS: software fallback,3menu restorations,32cursor moves.
Evidence /tmp/cubit-desktop-visible-native-2/result.json and serial.log.
Still open: visible bootstrap capture, receipt/clock faults, explicit unselected
backend invariant/GPU readiness test and independent actual logsvc observer.
Not hardware validation, physical visibility or NUC black-screen diagnosis.
All own jobs terminal. No delivered image or shared production changes.

## Current task priorities — Graphics startup handoff (user approved)

The compositor goal remains active and incomplete. Prioritize the Graphics
black-screen investigation before further optimization or visual work.

- [ ] Build and validate the private visible-startup candidate at
  `/tmp/cubit-desktop-visible-diag-1`. CPU status frame must use the existing
  authenticated Display output pool without permanently selecting the software
  renderer. Code is drafted; compilation and native validation remain open.
- [ ] Require the matching authenticated presentation release before entering
  synchronous Mesa startup. Exercise delayed, rejected, stale and missing
  receipts; uncertainty must retain ownership and must never count as success.
- [ ] Verify readable startup/first-frame breadcrumbs reach actual logsvc
  before the main presentation loop, using an independent observer. Existing
  breadcrumb compile, hosted boundedness tests and native software interaction
  pass; they do not yet establish this delivery gate. Keep producers bounded
  and nonblocking, with completions handled only by the normal dispatcher.
- [ ] Capture the bootstrap screen and verify successful transition into the
  desktop, software fallback, and cursor/menu interaction. Keep diagnostic hold
  fixtures separate from the candidate intended for hardware use.
- [ ] Hand Graphics the verified artifact, exact source/binary hashes, native
  evidence and remaining limits for a coordinated diagnostic image. Preserve
  the previously delivered NUC image. No hardware-fix claim without NUC evidence.
- [ ] Correlate with Graphics-owned Display/supervisor startup diagnostics:
  Desktop may never be reached after firmware-console retirement. Missing
  log records alone do not locate the stall; obtain exact image identity and
  positive startup/presentation evidence.

Follow-on integration: resolve the existing CCL optional-render ownership
handoff, publish the coherent pending Desktop API/Main/build changes, then
resume native recovery and hardware performance gates. This task update is not
approval to overwrite another owner's files or replace a delivered image.
Graphics retains Display/i915/Mesa service work; compositor owns the private
Desktop bootstrap, breadcrumbs, receipt validation and handoff evidence.

2026-10-06 PROGRESS breadcrumb native gate +visible startup assessment.
Previous private diagnostic build/hosttests was progress.96791TERM0 nativeboot
PASS exact080e4ebf:3menus,32cursor moves,software startup preserved. Serial
checkpoint enum Image renders numeric on target; separate successor
/tmp/cubit-desktop-visible-diag-1 adds explicit labels (not yet rebuilt).
Original breadcrumb source/artifact preserved unchanged. Actual logsvc observer
gate still required. Graphics requests visible CPU status before synchronous
startup because black desktop prevents opening Logs. Source-backed assessment:
normal Begin_Output invokes Selection.Begin_Output and locks CPU, so cannot
simply flush before startup. Proposed diagnostic-only bootstrap bypasses renderer
admission but uses existing BP writable/render/present ownership and normal CP
receipt/retirement; initialize GPU only after authenticated release, retain on
timeout/uncertainty. No boot framebuffer reuse. Not implemented/tested yet.
Photo last visible display.svc launch may mean Desktop never reached; independent
Display/supervisor diagnostics still needed. All own jobs terminal; delivered
image and shared production unchanged. Goal active; CCL blocker separate.

2026-10-06 PROGRESS private black-screen diagnostic candidate.
Graphics requested implementation independently of shared-publication blocker.
Private /tmp/cubit-desktop-breadcrumbs-1 copied frozen71ae userspace; added
Desktop_Breadcrumbs and22fixed Main checkpoints. Static process-lived SDK
publishers,88KiB pages+metadata, one Emit/stage, shared requestSequence tokens,
normal CQ dispatcher only, no logger wait/hidden polling/reuse. Source and image
remain private. Source Main is frozen, not newer reopen implementation.
Rate admission: primary logstore Burst64/refill100ms budget shared with other
traffic;22markers can still be rate-limited. Missing marker NOT proof of stall.
First67694 compile failed Info enum; corrected Information.31551TERM0 native
link PASS /tmp/cubit-desktop-breadcrumbs-build-2.67406TERM0 hosted stalled/missing/
tokenexhaustion +repeat/out-of-order/duplicate/foreign completions PASS.
Test script /tmp/test-desktop-breadcrumbs.py, log /tmp/cubit-breadcrumb-host.log.
Native software interaction boot96791 LIVE; poll exactsession, log
/tmp/cubit-breadcrumb-native.log, evidence /tmp/cubit-breadcrumb-native-1.
Still needed: actual logsvc delivery before mainloop, native stalled/absentlogger
and failedreceipt lifetime tests. No diagnosis of NUC blackscreen yet. No shared
production/build scripts edited, no delivered image replaced. Graphics notified
no shared lock held. Goal active; CCL publication dependency remains separate.

2026-10-05 dependency audit3: goal blocked pending external handoff.
Third consecutive audit of same ownership/integration blocker. CCL reservation
unchanged; compiler/schema still lack optional-render support; prepared patch
still applies. No user answer or owner release, no live own process, no new
hardware evidence. Independent source publication, build adapter, software native
checks, metrics overload, hosted Vulkan preview/dispatcher and scoped proofs are
complete at recorded scope. Repeating them would not unblock shared activation
or native GPU/hardware measurements. Goal marked blocked, NOT complete; original
objective unchanged. Resume with CCL handoff/user approval, then coherent8API/
Main+6fixture publication/build routing, native recovery and hardware gates.
No staging/image/commit/push.

2026-10-05 dependency audit2: same unresolved integration handoff.
No new implementation progress. Checked CCL reservation/networking ownership
note and source: no release or optional-render syntax landed; patch applicability
still passes. User approval question remains unanswered. No live own jobs to
poll, and no new hardware result. Do not restart completed tests or infer consent
from automatic continuation. Goal active pending third-audit threshold; source,
image and staging unchanged. This note is bookkeeping, not progress.

2026-10-05 dependency audit1: no new implementation progress this turn.
Previous exact native adapter boot was progress. Revalidated primary compiler/
schema lack optional-render support; prepared3file patch still applies cleanly.
CCL ownership note remains in force, no release found. Graphics authoritatively
idle, no live own build/test jobs, no new native Intel validation result.
User async approval requested for scoped compiler patch+Desktop manifest, with
links to CCL reservation and coordination rule explaining why. Await response;
do not treat elapsed time as consent. Required dependent API/Main/default
integration cannot proceed before this handoff; hardware gates remain separate.
Goal remains active (first dependency audit, not blocked threshold). No source,
staging, image, or test changes; do not count this status note as progress.

2026-10-05 PROGRESS exact normal-route artifact nativeboot58596 terminal0 PASS.
Previous normalbuild adapter work was progress. Booted exact7ac15d96 artifact
from /tmp/cubit-vulkan-desktop-c5l0qbjj/artifact using recorded prebuilt kernel/
services and matching metrics collector/observer. Native CuBit/QEMU: deniedGPU
freshsoftwarechild,3menu restorations,32cursor moves/eight roundtrips,125%Settings
apply plus16scaled moves/four roundtrips;authenticated release/input/draw/submit
metrics and zero false GPU markers. Screenshot visually inspected; evidence
 tests/compositor/build/normal-vulkan-route-boot-20261005. This closes adapter
build-to-boot gate for private complete source, NOT shareddefault activation or
hardwareGPU/latency. No image/default/staging change. All own jobs terminal.

2026-10-05 PROGRESS opt-in normal-build Vulkan adapter wired and verified.
Previous focused proofs completed was progress. New tools/build_vulkan_desktop.py
runs existing cleanbuilder, verifies variant/artifact, atomically replaces only
selected scenario desktop.svc and records retained artifact path/hash. Variant
resolver accepts Vulkan; kernel desktop and metrics helper branch to adapter
with explicit Mesa bundle/source. Shared default STILL legacy; fullroot Vulkan
still needs pending8API/Main + optional-render manifest/compiler publication.
Shared build/helper edits held build.lock. Tested against complete private
reopen source and optional compiler, not shared install.19930 firstlink PASS but
artifact path inherited Nix TMPDIR; corrected persistent /tmp retention.
81893TERM0 corrected link PASS, artifact /tmp/cubit-vulkan-desktop-c5l0qbjj/artifact;
45247TERM0 helper syntax/variant preservation/unsupported scenario rejection.
No default/image/staging/commit/push. Exact post-shell artifact verification is
74534TERM0 PASS after shell exit: retained source/binary identity and installed
private variant SHA7ac15d9636f2f07bca59df024c9bb4202c5d3387ac566a46b402532e1dbbdc34.
All own jobs terminal; native boot of this new adapter artifact remains.

2026-10-05 PROGRESS published policy proof49835 terminal0 PASS.
Previous turn runtime gates/proof wait was progress. Resumed exact49835, now
terminal0. Geometry/binding/submission/row-copy report totals all zero justified
and unproved; reports retained separately (overlapping dependencies, do not sum
as uniquechecks). All copied/external source hashes rechecked unchanged.
Evidence tests/compositor/build/published-preview-policies-20261005 includes
reports, full log, inputs and copied source manifests. Boundary remains selected
policy units conditional on trusted observations; no foreign/Main/hardware proof.
Pending publication remains owned8API/Main plus approved6fixture closure; no
need to release other34tests to publish that coherent group. CCL generated
optional-render binding/default routing remain outstanding. All own jobs
terminal, no staging/image/default changes. Goal active and incomplete.

2026-10-05 PROGRESS focused published-policy runtime gates PASS; proof49835 LIVE.
Previous hosted actual preview/dispatcher run41665 was progress. Private copied
supplemental preview_geometry/binding/submission and readback fixtures, preserving
shared tests. Runtime passes:36864preview geometry pixels;Ada/C rejection+empty
clip;mock submission stale/bounds/pending retirement/draw cap/quarantine;8385
pitched readback cases+invalid bounds. GNATprove sequential job49835 remains LIVE:
geometry completed41checks and binding report completed; submission in phase3,
row-copy follows. Poll exact49835; do NOT restart on observation timeout.
Log /tmp/cubit-published-preview-policies.log, private tree unchanged production.
Graphics now explicitly approved additional3C/header callers; coherent publication
set is frozen3GPR/bridge + integration3C/header. Remaining34supplemental entries
not released and not prerequisites. No shared test publication yet; API/Main/CCL
integration gate remains. No default/staging/image changes.

2026-10-05 PROGRESS actual preview/dispatcher coverage restored privately.
Previous glyph/affine gate+coverage audit was progress. Supplemental40test
inventory/patch prepared from Graphics integration; no shared tests applied.
Only3C/header caller files overlaid in private checktree; retain frozen reviewed
GPR/bridge and pending dispatcher.41665TERM0 PASS: real Backend_Frame capture/
poll/externalreader/teardown assertions,720previewframes552960exactpixels,
49152glyph and233472affine pixels,validation0; fresh SPIRV validation.
Every recorded input/copied hash rechecked. Evidence
 tests/compositor/build/published-preview-dispatch-20261005; coverage/production
comparison included in review directory. No wholeMain/nativeGPU/hardware claim.
All57 Graphics production hashes equal integration current;4owned dispatcher/
API/GPR/Main differ. Historical13file handoff does not prove full test identity;
fresh run now binds source/test hashes. Supplemental review sent to Graphics.
All jobs terminal; no default/staging/image changes. Goal active.

2026-10-05 PROGRESS published-source Vulkan glyph/affine gate PASS; coverage gap identified.
Previous57source publication/defaultcompile was progress. Private tree
/tmp/cubit-published-vulkan-check-1 copies shared production/tests plus exact
pending8source/reviewed3fixture overlay, recorded/rechecked every input hash.
First14126 failed beforecompile: snapshot excluded build-named shader script;
explicitly copied/recorded script.32604TERM0 regenerates/validates SPIRV and
runs actual llvmpipe:49152glyphpixels,233472affinepixels, validation0 PASS.
Evidence tests/compositor/build/published-vulkan-20261005. NOT full preview or
runtime-dispatch coverage: root/frozen C host does not invoke new bridge paths.
Graphics integration has additional modified C hosts plus preview geometry/
binding/submission tests not copied into frozen candidate test tree. Requested
matching supplemental test closure/review; do not silently count uncalled APIs
as tested. Existing earlier private preview evidence remains distinct. No further
shared source/test publication or image changes. All own jobs terminal.

2026-10-05 PROGRESS57 reviewed Graphics production files published.
Graphics explicitly released exact57 graphics-owned inventory entries and3test
entries. Applied57 with all current-root/source hashes checked before writes,
then verified every postimage, under shared build.lock. Receipt:
/tmp/cubit-compositor-publication-review-20261005/published-graphics.json.
Held eight compositor-overlap files AND3dependent test files together: new
facade changes Begin/Complete signatures, Main needs optional-render binding,
and legacy/mesa bodies lack Configure/Recover. Publishing those piecemeal would
break default Desktop. CCL manifest publication remains owner-held. No source
synchronization outside reviewed paths. Native default legacy compile85933
TERM0 PASS (GNAT16 alr, separate graphics-publication-check subdir, sharedlock).
This is compilation, not link/boot evidence. Matching actual-Vulkan overlay
regression and shader regeneration remain next gate. Shared default unchanged;
no staged binary/image/commit/push; frozen NUC candidate unchanged.
Also fixed metrics-session disk oracle to resolve selected build variant rather
than hardcode build-metrics; full packaging gate remains after integration.
All own jobs terminal; Graphics notified lock released.

2026-10-05 publication review progress / verified peer wait.
Added tests-inventory.json/tests.patch under
/tmp/cubit-compositor-publication-review-20261005 for real Vulkan fixture GPR
and bridge ads/adb. All3 root hashes equal saved baseline; production65file
and test3file patches both pass git apply --check (read-only, not applied).
Shader diff reviewed locally: adds preview uniforms/sampling/mode4 and expands
quotient fallback bit range15->23; existing backdrop/ordinary image paths remain
in diff context. Saved baseline absent, Graphics explicit review still required.
Graphics thread01a02f68 latest turn01a10e74 remains authoritatively active at
revision27 after bounded wait; no inferred completion or source release. No
build/process restarted, no pending source set applied. CCL owner gate unchanged.

2026-10-05 PROGRESS first scoped source publication + complete review inventory.
Published own pure compositor_backend_selection.ads/adb and portable hosted/proof
fixture tests/compositor/backend-selection. Edits under build.lock; no Desktop
build selection change.28588TERM0:128startup vectors/16384reselections/64drain
combinations plus stale/uncertain/duplicate gates PASS; GNATprove all reported
checks proved. Evidence tests/compositor/build/published-selection-20261005.
Graphics acknowledged publication window, asks review before release. Prepared
/tmp/cubit-compositor-publication-review-20261005/{inventory.json,proposed.patch}
with65production paths and root/baseline/frozen/proposed hashes. Root matches
saved baseline wherever both exist; vulkan_affine.frag lacks saved baseline so
history UNKNOWN, not permission to overwrite. Only Main/backend-vulkan facade
proposed hashes differ from frozen (owned reopen recovery). Sent review to
Graphics; complete set NOT applied. CCL owner acknowledgment still pending.
All jobs terminal; existing image/default/staging unchanged. Goal active.

2026-10-05 PROGRESS native collector-backpressure gate PASS.
Previous native stage-metrics verification was progress. Reused actual metrics-
stall collector, freshly rebuilt against85c08712 candidate runtime (58470TERM0).
Owned boot harness now --metrics-stall requires unchanged held grant for600checks,
menu restoration during hold, resumed batches with explicit loss, no quarantine.
Shared harness edits held build.lock. First91719 timed out waiting for resumed
loss report: workload ended before grant release. Added post-release keyboard
input;26894TERM0 PASS: all3menu restorations during hold,32cursor moves/eight
roundtrips,600page stability checks, resumed batch3 reporting288dropped samples.
No false GPU markers or memory faults. Evidence
 tests/compositor/build/native-metrics-stall-20261005. Drops are expected bounded
telemetry overflow, not dropped input or frames. This is native CuBit/QEMU CPU
fallback under telemetry backpressure, not GPU/hardware latency evidence.
No production code/image/staging changes. All own jobs terminal. Goal active;
normal build publication and hardware/recovery/performance gates remain.

2026-10-05 PROGRESS native stage-metrics gate PASS.
Previous task update/verified metrics retry was progress. Expanded owned native
metrics observer to require authenticated, nonempty input-dispatch, scene-draw
and submit-call Latency series alongside growing submit-release Span series.
Checks full zero-padded names, units and source/series rejection/loss counters.
Harness requires new stage marker, so an old release-only observer cannot pass.
Shared test edits held build.lock. Initial69798 compile found mixed logical
operators; corrected syntax, fresh matching services5993 terminal0 PASS.
Native8931 terminal0 PASS: exact85c08712 candidate, metrics marker threeframes/
sixbatches/allthree stages,3menu restorations,32cursor moves/eight roundtrips,
zero false GPU markers. Evidence tests/compositor/build/native-stage-metrics-20261005.
Docs describe --metrics on, generated manifest/bindings and explicit compatible
collector/observer seeds. Request-dispatch is outside this no-client workload;
stages overlap, and QEMU cannot establish hardware/physical latency. No image,
shared runtime, default build or CCL compiler publication change. All jobs terminal.

2026-10-05 TASK UPDATE — user requested alignment with Graphics needs.
This ordered checklist supersedes the earlier four-item integration list.
Compositor owns Desktop startup/admission, dispatcher/recovery, final build and
native integration. Graphics retains driver allocation/session lifecycle and
scene/shader/source/backdrop/readback implementation. Coordinate overlapping
Main/facade changes before editing; no blanket source synchronization.

1. [ ] Publish reviewed compositor deltas into the normal build: resolve the
   existing CCL owner handoff for optional-render metadata, integrate the clean
   builder and explicit runtime backend selection, then verify default builds.
   Private candidate works; shared default is still legacy. Preserve software
   startup when GPU authority/device/readiness is absent.
2. [ ] Finish native recovery/fault gates for the newer reopen candidate:
   allocation pressure, delayed completion, teardown and replacement output;
   no writer/source/target reuse before confirmed retirement, full repaint
   before CPU fallback. Hosted Vulkan and native software evidence exist;
   native GPU recovery is still open.
3. [ ] Support the exact frozen NUC image test with Graphics: require startup
   READY, frame COMPLETE and PUBLISHED plus correct wallpaper, windows, cursor,
   dragging and menus. SOFTWARE is valid fallback, not GPU success. Keep the
   delivered 71ae3084 Desktop image unchanged until results justify a separate
   tested artifact. Hardware results remain a user/hardware dependency.
4. [ ] Obtain the logging owner's producer-ring source/API and coordinate its
   integration with Graphics. Primary CuBit.Logging remains single In_Flight;
   do not remove Mesa receipt waits until buffer ownership and slow/absent
   collector behavior are tested. Logging implementation remains owner-held.
5. [ ] Use authenticated metrics for allocation/render/readback/presentation
   profiling and overload tests, then measured optimization. Integrate Graphics'
   bounded allocation work into a separately identified candidate after review.
   Do not interpret QEMU spans as Intel timing, 240 Hz or photon latency.
6. [ ] After Graphics' session-lifecycle interface is settled, test repeated
   launch/close and stale identities from the compositor side. Broker endpoint
   disposal authority remains a separate Graphics/user decision; this task-list
   update does not silently grant new kernel authority.

Completed prerequisites: real Vulkan backend final link; private optional-render
manifest generation and fresh software-child fallback; runtime dispatch;
proved selection policy and hosted retirement/recovery checks; reusable builder,
artifact identity guard and native boot harness. Shared publication still open.

Metrics follow-up now terminal: build7922 and matching collector/observer55473
PASS; native retry23476 terminal0 PASS in
/tmp/cubit-compositor-metrics-boot-2/result.json. Exact binary85c08712, three menu
restorations,32cursor moves/eight roundtrips, no false GPU markers; observer saw
three authenticated release frames/six batches, no schema/loss rejection.
Initial old-seed observer failed memory-grant creation; matching rebuild passed.
This is native CuBit/QEMU software publication evidence, not hardware timing.
All own jobs terminal; no staged image, commit or push.

2026-10-05 PROGRESS clean-builder nativeboot +dedicated reusable harness PASS.
Previous cleanbuilder/link was progress. Promoted Graphics' exact identityguard
as tools/verify_desktop_vulkan_compositor.py and retained negative tests in
 tests/compositor/test-vulkan-compositor-artifact.py (73465TERM0 NixPASS).
New tests/compositor/test-desktop-vulkan-boot.py consumes compositor-result.json
directly, checks copiedinputs/binary before+after, rejects falseGPUprogress;
removed dependence on old startup aliases/featurebooleans. Allsharedtool/test
edits under buildlock.48785TERM0 exactcleanbuilder c20c18ab CuBit/QEMU bootPASS:
GPUdenied ->freshsoftwarechild,3exactmenu restorations,32cursor moves/8roundtrips,
zero falseGPUmarkers. Explicit seedkernel/services; hardware_validatedfalse.
Evidence tests/compositor/build/clean-builder-boot-20261005; helper documented.
No installed/stagedimage/sourcecompiler change; frozen71ae3084image unchanged.
All ownjobs terminal. Shareddefault/CCLpublication and hardware/performance gates
remain. New build+boot workflow now reusable without path-specific aliasfiles.

2026-10-05 PROGRESS reusable clean compositor builder verified.
Previous optionalmanifest handoff was progress. Added owned
 tools/build_desktop_vulkan_compositor.py under shared edit lock; explicit input
source/toolchain/Mesa bundle/headers/manifestcompiler/schema/catalog/newoutput.
No startup code injection or capability bytepatch; generates real manifest,
compiles Vulkan Ada, validates freshly generated SPIRV, compiles12Cbridges,
links verified bundle. Records commands/inputhashes including Mesaheaders,
rehashes inputs/copies and checks unresolvedsymbols. Copied runtime/font/
wallpaper artifacts are explicitly prebuilt inputs, not world rebuild.
17647TERM0 clean build PASS /tmp/cubit-compositor-builder-test-4; independent
Graphics artifact verifier accepts exact dedicated manifest+source/binary.
First37864 C++driver used forC bridge; fixed explicit Cprefix.25441/81584 missing
checker in list (first scripted correction failed to match); verified corrected
list then cleanrun4 passes. All3failedoutputs INCOMPLETE; independent verifier
rejects them. No hidden reuse of previous bridgeobjects. Docs describe tool and
fixed metrics/timingoff variant. Evidence tests/compositor/build/compositor-builder-20261005.
Shared build default STILL legacy; CCLsource publication and defaultpipeline
integration pending. This new artifact is linked only, not booted/installed;
previous candidate boots remain distinct. Packaged71ae3084 image unchanged.
All ownjobs terminal. Next native boot of cleanbuilder artifact and normalbuild
integration, preserving proof/hardware/performance gates.

2026-10-05 REQUEST CCL owner / optional render publication handoff ready.
Please review/incorporate tests/compositor/optional-render-manifest.patch or
acknowledge compositor may apply these3narrowfiles under shared coordination:
 ccl-manifests-keywords.adb, ccl-manifests-typed.adb,
 userspace/ccl/interfaces/executable-manifest.ccl.
No compiler files modified here. Patch preserves existing required render wire;
adds keyword request-render-optional and typed Request.Optional_Render with
existing procmgr param0=1 behavior. Adjacent optional-render-manifest-inputs.json
records exact currentbefore/expectedafter hashes; git apply --check PASS against
currentshared sources. Changes remain pending CCL reservation/earlier userquery.

PROGRESS: portable tests/compositor/test-optional-render-manifest.py accepts
explicit compiler/schema/newoutput, never mutates source or appliespatch.25129
TERM0 Nix isolatedcandidate compiler:7wire/rejection casesPASS. Evidence under
 tests/compositor/build/optional-render-portable-20261005. This is real emitted
ELF capability bytes, not compiler-only textual assertion. Native procmgr fresh
softwarechild already covered by exactcandidate boots; no new scopeclaim.
Sharedroot still CUBIT_COMPOSITOR=legacy and Vulkanhelper startup-only; must not
claim routinebuilds contain private compositor. Next normalbuild/publication
integration once ownerclearance ready; new reopencandidate remains separate.
All ownjobs terminal; no staged binaries/images or compiler/shared runtime edits.

2026-10-05 PROGRESS actual Main reopen helper +Vulkan and new nativeboot PASS.
Previous separate reopencandidate compile/policy tests were progress.22718TERM0
actual pumpReopenRecovery extracted unchanged into actual hosted Vulkan fixture;
oldwriter-unconfirmed/no-replacement-output blocks CPU, real D.Stop retirement
and newoutput RP/damage invalidation admitCPU. Existing exactpixel/validation0
gates pass. Driver/Display old-retirement observations supplied by fixture,
not a claim of nativeGPU teardown/reopen. Extra Main observer tests51983 separate.
55761TERM0 finalnative link cfd84a946d167496b00948f7bbee0f8ca6dc49e611588030d16796ad73fe58e4,
52827424bytes; /tmp/cubit-compositor-reopen-1/compositor-result.json,
compositor-sources.json SHA3e1811736a25db09b56d0b4281e18628e853f6167cd96d7babd7d6e23f8845c7
(nativeobjectseeds recorded; verified Mesa bundle reused).21463TERM0 exactbinary
CuBit/QEMU software fallback/3exactmenu restorations/no falseGPUmarkers PASS.
Evidence/fixtures tests/compositor/build/reopen-vulkan-20261005. No hardware,
latency or native GPUreopen claim. All own jobs terminal. Delivered Graphics
NUC image stays frozen71ae3084 (graphics exactimage QEMU gates reportedPASS);
no replacement, sourcepublication/staging/image change. Goal remains active.

2026-10-05 PROGRESS fullteardown/reopen wiring in separate candidate.
Previous audit/regression was progress. New /tmp/cubit-compositor-reopen-1 copied
userspace independently; packaged71ae3084 tree sourcehashes still unchanged.
Main preserves old real writer key when drain requested (or transfers existing
capacity-recovery key), records writer retirement only after every output's
renderer/Display/grant/storage retirement. Calls existing Recover_Renderer with
full-repaint false until new backBufferReady +enabledoutputs actually receive
RP invalidation +damage. pumpPresentation blocks draw until recovery completes.
Facade Forget_Targets shortcircuits after confirmed child retirement, avoiding
repeated child operations on a retired device. Existing proved selector unchanged;
new Main observations remain trusted adapter boundary, not new wholeMain proof.
56013 TERM0 nativecompile/bind PASS.51983 TERM0 extracted actual reopenpump with
real selection/repaint/damage: oldowner gate, absentoutputs, noenabledoutput,
delayeddevice, twooutputfullrepaint and one-wayCPU admission PASS. Source check
confirms oldwriter fact follows all-output retirement and precedes oldpool reset.
Evidence/patch/hashes tests/compositor/build/reopen-integration-20261005.
Not final linked/booted/nativeGPU recovery yet. No staging/install change; own
commands terminal. Next test actual Vulkan retirement with output-reopen Main
adapter, then final private link/nativefault gate. Frozen candidate remains safe
for Graphics' separate image testing; do not replace it with this incomplete tree.

2026-10-05 PROGRESS output reconfiguration audit +actual boundary regression.
Previous nativeDPI/documentation turn was progress. Full GPU Forget_Targets closes
scene permanently; a fresh output pool epoch cannot reopen Scene.Closed. Main
has no GPU restart in setupDisplayBuffer. However internalShellSurface stays
used and anySurfaceUsed includes it: closing last externalapp does NOT establish
this path. Scale-only Apply preserves physical targets. Informed Graphics of
refined scope, not ordinarywindow bug. Future fullteardown/hotplug path needs
explicit drainedGPU->CPU recovery before newdrawing (or fresh admitted process).
99531 TERM0 actual hosted dispatcher rejects newepoch aftertargetclose, then
existing recovery retiresdevice/zerocharge and admitsCPU PASS; validation0 and
existing pixelgates pass. Fixture/log evidence dispatch-reopen-real-20261005.
Only tests/owned docs/ownnote edited; all frozen sourcehashes rechecked unchanged.
Graphics packaging48700 reportedPASS and exactUSBboot16615 reportedlive; that is
owner's handle, no duplicate/restart. All own commands terminal. Remaining next
implementation work includes safe fullteardown/reopen integration and hardware
acceptance; no completion claim.

2026-10-05 PROGRESS native125percent DPI frozen candidate +proof boundaries.
Previous actual capacity test was progress.58371 TERM0 exact binary71ae3084
seeded CuBit/QEMU native software boot: render-unavailable fresh child,3exact
menu restorations, real Settings Apply125percent,16scaledcursor moves/4exact
roundtrips PASS. settings-scale-5-4.png visually inspected. Evidence retained
 tests/compositor/build/compositor-dpi-20261005. Not nativeGPU or timing evidence.
Updated owned docs/compositor-backends.md private candidate checkpoint: exact
artifact, current oneoutput/readback limits, actual copy path, bounded markers,
proved policy versus supplied adapter facts, evidence table, uncompleted hardware/
performance acceptance. Goal scope retained; multioutputGPU/zero-copyscanout and
physical timing not claimed complete. No source/binary/staging mutation; frozen
candidate stays held for Graphics packaging. All own commands terminal.

2026-10-05 PROGRESS actual capacity-triggered GPU recovery PASS.
Previous goal turn boundedmarkers/finalnativeboot was progress. Added test-only
CUBIT_TEST_CAPACITY mode to combined hosted dispatcher fixture. Original client
plus7distinct retained immutable sources fill8slots; ninth client reaches actual
Slots_Full ->Software_Required (not endless Retry), destination sentinel wholly
unchanged. Actual Recover_Renderer then closes Vulkan owners to device Retired/
Charged_Bytes0, admits CPU and rejects re-enable/duplicate hazards.
49110 TERM0 Nix hosted llvmpipe PASS; exact pixel/oracle/lifetime tests retained,
validation0;18sourcecreates=18destroys, no remaining live source. No mocked
pressure classification; output-writer/repaint recovery facts still fixture
observations (native Main pool/damage logic tested separately). No native Intel
or physical latency claim. Evidence/fixtures saved under
 tests/compositor/build/dispatch-capacity-real-20261005.
Only test files changed. Rechecked every compositor-sources hash and frozen
binary71ae3084 hash; packaging candidate unchanged and held for Graphics.
All own commands terminal. Next native hardware integration and performance
remain, plus publication ownership resolution. Goal remains active/incomplete.

2026-10-05 PROGRESS bounded GPU completion/publication diagnostics +candidate.
Previous turn actual hosted dispatch recovery was progress. New private Main
logs first successful GPU Complete_Output and first authenticated Display
Published/Released acknowledgment, per output/process. Exact output/epoch/frame/
buffer, plus session/token at publication; max4messages/2outputs. CPU silent;
not scanout/photon evidence. Existing Desktop_Logs route, no runtime changes.
65697 TERM0 nativecompile.69689 TERM0 extracted actual marker blocks with real
pool: CPU silence, stale ticket rejection,1000duplicateframes silent PASS.
8354 TERM0 link; dedicated compositor-result.json +compositor-sources.json in
/tmp/cubit-desktop-vulkan-active-link-1. Source manifest890records verified.
Binary desktop-vulkan-compositor.svc SHA71ae3084dad673459024f90810220c053a2afed2167189788a90b73ccf488cde
bytes52824968. gpu_drawing_enabled:true means runtime-admitted capability,
NOT hardware validation; hardware_validated:false. Graphics verifier informed.
37679 TERM0 exact binary native CuBit seeded boot PASS approved-but-unavailable
render ->fresh software child;3exactmenu restorations,zero false GPU markers.
Evidence tests/compositor/build/gpu-markers-20261005. Earlier342fdfbd boot remains
separate. No staging/installed image changes. All own commands terminal; source
snapshot held stable for Graphics verifier. Native hardware/capacity-triggered
recovery and publication ownership still open. Next capacity fault integration.

2026-10-05 PROGRESS actual hosted Vulkan dispatcher +recovery PASS.
Previous turn merged preview/native fallback boot was progress. Adapted private
Graphics fixture to explicitly configure runtimeGPU after real initialization;
added CPU software-text/allocator dependencies to its explicit GPR. Initial
41654/50198/95423 terminal compile failures were missing fixture header/source
lists, fixed.73471 TERM0 actual combined dispatcher fixture PASS: fonts49152,
preview552960, affine233472 exact pixels, textCPU-fallback/readback-reservation
invariants and validation0. All via Linux hosted llvmpipe, NOT Intel/CuBitGPU.
Extended fixture closes independent oracle scene then calls ACTUAL Recover_Renderer
with real Vulkan cleanup.5831 TERM0 PASS actual device Retired/Charged_Bytes0,
CPU Begin/Complete works, duplicate recovery safe, GPU re-enable rejected.
Initial16036 missing fixture enum visibility corrected. Writer/repaint facts are
explicit fixture facts; Main real pool/damage evidence is separate3186 test.
Does not inject capacity pressure yet or prove native hardware recovery.
Production source hash set/binary342fdfbd unchanged. Adapted fixture sources/logs
saved tests/compositor/build/dispatch-recovery-real-20261005 with hashes.
All own commands terminal, no staged/installed image changes. Next add Graphics'
requested one-time actual GPU completion/publication identity logs through existing
Desktop_Logs, then capacity-triggered integration/native gates. CCL publication
ownership question remains pending; independent work continues.

2026-10-05 PROGRESS reconciled Graphics preview +native fallback boot PASS.
Previous turn recovery compile/adapter fault tests were progress. Current Main
extracted retry/pump branches tested with real BP/RP/damage: rotated front held,
failed frame unpublished, fresh input retained/full repaint, transfer gate and
changed epoch rejection PASS3186 (initial48644 fixture aggregate compile fixed).
Graphics froze handoff; consumed13 files with hashes/recheck then released hold.
Draw_Preview merged into nestedCPU/GPU +dispatch/spec; only settingsWallpaper
replaced in Main. Scene/backdrop/geometry and current text/readback guard tests
copied. Existing shader/binding/submission sources matched owner's versions.
preview-handoff.json records consumed versions; recovery-preview-sources.json
records890 reconciled sources. These replace earlier startup source manifest
for current binary; old startup-sources.json belongs to prior boot only.
38130 TERM0 final native link342fdfbd48f619c3edee909ab74776a2a529d6b157db7cd5a7b449f5a64e6d06.
68893 TERM0 private CuBit/QEMU boot with approved-but-unavailable render ->fresh
software child. Three exact menu restorations,32cursor moves/8roundtrips PASS;
menu screenshot visually inspected, no guest faults. Prebuilt kernel/service
seeds explicitly recorded; not full current-source world/Intel GPU evidence.
Durable result/logs/pngs/tests under tests/compositor/build/recovery-preview-20261005.
Native GPU recovery and hardware performance remain unverified; fixture changes
from Graphics need adapted hosted dispatch test (defaultCPU now requires explicit
GPU configure in fixture). No primary/staged image/source publication. Optional
manifest publication still awaiting ownership clearance. All own jobs terminal.
Next actual GPU-dispatch recovery fixture and native fault gate, then publication.

2026-10-05 TASK UPDATE / recovery adapter private validation.
User authorized matching Graphics needs. Responsibility split confirmed directly:
Graphics owns capture/shaders/preview +native child lifecycle; compositor owns
startup/admission, runtime dispatch, whole-frame fallback/Main and integration.
Next gates: native recovery fault injection, preview source reconciliation with
Graphics' new Draw_Text regression, source publication after ownership clearance,
then supported-hardware validation. Logger changes remain with logging owner.

Consumed registry pressure classification and readback guard into frozen private
/tmp/cubit-desktop-vulkan-active-link-1 only. Software_Required requests keyed
recovery; failed frame is never presented. Main explicitly queues full repaint;
changed output epoch fails closed. Adapter closes children once, then polls Stop
until device Retired; no repeated scene/source close on retiring device.
73754 TERM0 native compile/bind, /tmp/cubit-recovery-adapter-native.log.
6209 TERM0 actual extracted Recover_Renderer with real selection policy +mock
retirement: busy, stale key, delayed retirement, writer/repaint gates, duplicate,
uncertain child/device tests PASS. /tmp/test-desktop-recovery.py and snapshot
recovery-adapter-test/. Hosted adapter test is NOT native GPU recovery evidence.
Pure recovery policy proof unchanged; actual owner observations are audit boundary.
No new final link/boot after recovery edits. startup-result/source manifests refer
to prior startup candidate only. No staged/shared source/image changes; all own
commands terminal. CCL manifest ownership question still pending.

2026-10-05 PROGRESS exact-identity GPU recovery policy proved privately.
Previous turn exact native software boots passed. Selection now has separate
Request_Recovery/Observe_Recovery: fixed output/epoch/frame/buffer key, capture
closed while draining, one-way CPU only after all renderer/source/readback/
writer retirement +full repaint queued. Missing facts retain GPU mode; unknown
retirement quarantines; stale/duplicate receipt cannot switch/reopen admission.
7452 TERM0 hosted all64 drain vectors, four stale-key dimensions, uncertainty,
duplicate/re-enable rejection plus128startup/16384reselection regression PASS;
GNATprove postconditions all proved (report in selection-test/obj/gnatprove).
Wrapper Begin_Output consults Can_Capture; no renderer call while draining.
This is policy evidence ONLY; actual recovery request/Main handling and GPU
retirement callbacks not wired yet, no new native recovery claim.
Graphics exact Software_Required delta agreed: only after Output_Repaint clears
Opened/Held_Writer, if Last_Pressure/=None. Graphics keeps enum/Main unchanged;
I own enum+handling on frozen copy. Graphics adds independent failclosed
Forget_Targets check Copy.Idle/readback-pending-none, tests in progress. Need
consume that tested guard, then facade close AND D.Stop/Device.Retired evidence
before switching. Fixed-output software startup binary7341a583 unchanged;
private policy sources now newer than its startup-sources.json (do not confuse).
No shared sources/staging/image changes; all own jobs terminal. Next recovery
adapter/native fault gate, then preview snapshot reconciliation.

2026-10-05 PROGRESS actual runtime-dispatch Desktop startup/boot PASS.
Private Main Start_Renderer consumes generated Slot_render (24, not fixture62),
inspects launch endpoint, initializes existing owner; gates targets/pipeline/
upload/readback and fixed one-output config through proved selector. GPU target
budget128MiB, upload2MiB, readback<=16MiB; failures retain independent GPU owners
while selecting safe CPU before any scene submission. Shader preview not rebased.
16237 TERM0 final native link7341a583, startup-result.json +startup-sources.json
in /tmp/cubit-desktop-vulkan-active-link-1. Compiler generates optional manifest
normally from private request-render-optional; no objcopy metadata patch.
87747 TERM0 native no-approval software;94945 TERM0 approved render rejected,
fresh child4294967328->8589934624 software admitted. Both actual defaultCPU
facade show desktop and3menu cycles with exact restored pixels, no guestfault.
Seeded CuBit/QEMU not Intel GPU; startup readiness GPU branch remains untested.
Screenshots visually inspected. Durable logs/results/PNGs in
 tests/compositor/build/startup-dispatch-20261005/boot-{1,2}; only own terminated
VM test disk/ISO files removed after hashing. No root/staged/NUC image changes.
Private scripts /tmp/wire-desktop-startup.py, /tmp/link-startup-desktop.py,
/tmp/cubit-startup-boot.py. Existing startup-sources manifest is current; old
result.json remains PRE-wrapper binary, use startup-result for this candidate.
All own jobs terminal. Next explicit drained GPU->CPU recovery for capacity
exhaustion, coordinated with Graphics registry classification; ordinary startup
selection still immutable. CCL ownership question remains pending; do not promote
compiler/shared sources without resolution. Full goal active/incomplete.

2026-10-05 PROGRESS backend selection moved into private proved SPARK policy.
Previous turn dispatch adapter compile/runtime passed. New private
compositor_backend_selection.ads/adb in /tmp/cubit-desktop-vulkan-active-link-1/
userspace/lib/compositor: seven readiness observations, one-shot selection,
early Begin locks CPU, all later changes rejected. Dispatcher now calls this
policy instead of owning unproved booleans; all13 calls use its one Mode.
Readiness covers admission/device/targets/pipeline/upload/readback/configuration;
trusted adapter must establish facts. No new authentication or retirement claim.
15034 TERM0 hosted128vectors/16384reselects +SPARK5checks0unproved/justified.
39600 TERM0 native compile/bind after policy integration. Proof boundary and
test in selection-test/, /tmp/cubit-backend-selection-proof.log and
/tmp/cubit-desktop-selection-native.log. Body dispatcher still SPARK Off;
new policy proof is not whole-renderer proof. No final relink/boot after edits.
No source changes in graphics integration/root; no image or staging changes.
All own jobs terminal. Optional manifest candidate remains private while user
ownership-clearance question is pending. Next wire startup facts from generated
optional binding and existing owners, not constants; software boot first.
Graphics preview work remains separate; await agreed Draw_Preview signature.

2026-10-05 PROGRESS startup-only dispatch private adapter compiled/tested.
Graphics approved runtime-dispatch design and own copied-wrapper work explicitly.
/tmp/cubit-desktop-vulkan-active-link-1 now has nested CPU/GPU implementations
in copied backend-vulkan body; their behavior preserved, all13 facade operations
dispatch to same selection. Configure_Renderer may select once; first Begin also
locks default CPU. GPU unsafe never switches to software. Full_Output/Selected
facade Global aspects now read Engine. No graphics integration/root source edits.
Generator /tmp/create-desktop-dispatch.py; original GPU body retained .original.
1772 TERM0 native compile/bind PASS, /tmp/cubit-desktop-dispatch-native.log.
86709 TERM0 extracted actual Configure/Begin/Full_Output hosted test with mock
backends: defaultCPU, explicitCPU/GPU, lateactivation rejected, GPU Start_Unsafe
preserved and switchback refused. /tmp/test-desktop-dispatch.py, dispatch-test/.
Adapter currently SPARK Off; selection state must move into proved policy before
production acceptance. No new full-link artifact claimed after wrapper edit;
existing result.json binary refers to PRE-wrapper link. Next startup init and
readiness checks, proved selector, final relink and native software boot.
Compiler candidate remains private pending user response to ownership question
(required by CCL reservation); independent dispatcher work proceeds. All own
jobs terminal, no images/staged binaries/shared source changes.

2026-10-05 PROGRESS optional render production-metadata candidate PRIVATE.
Previous turn native backend final link completed; this turn compiler gate passed.
/tmp/cubit-optional-render-ph1sk602 contains isolated CCL compiler/source/schema.
Proposed narrow delta: keyword request-render-optional read-write binding;
typed Request.Optional_Render binding. Existing required request remains wire
param0=0; optional emits param0=1, type11/rights3/param1=0 exactly as existing
procmgr render admission requires. Requests still do not grant authority.
Uses existing internal Service field for render demand, no kernel/wire revision.
No new helper function in typed schema (existing function-count budget retained).
33993 TERM0 compiler build,18007 TERM0 actual compiler+gcc+objcopy wire tests:
required/optional both keyword+typed, mixed duplicates, invalid rights rejected.
Test /tmp/test-optional-render.py, test-result.json, publication-guards.json in
private directory. Shared compiler/schema/manifest unchanged. No live jobs.
CCL OWNER REQUEST: narrow review/ownership clearance for ccl-manifests-keywords.adb,
ccl-manifests-typed.adb and interfaces/executable-manifest.ccl before promotion;
no active CCL chat found in current list. Graphics coordination can relay owner
status. Independent startup/fallback work can proceed privately meanwhile.
Next expand regression coverage and compile Desktop optional manifest via real
generated binding, then integrate admitted startup with graphics owner APIs.

2026-10-05 PROGRESS native actual Vulkan-backend final link PASS50839 terminal0.
Private experiment /tmp/cubit-vulkan-active-backend-link.py selects vulkan,
uses build-vulkan binder objects and correct copied manifest/wallpaper assets.
Copied/hash-guarded graphics integration + verified matching-runtime Mesa bundle;
artifact /tmp/cubit-desktop-vulkan-active-link-1/desktop-vulkan-link.svc with
result.json/inputs.json and /tmp/cubit-desktop-vulkan-active-link-1.log. Native
compile/bind/final link succeeds, no undefined symbols; Vulkan1.0 shaders validate.
Important: helper result gpu_enabled/drawing flags still describe old fixture;
this selects actual backend-vulkan but does NOT initialize/admit it or boot it.
Thus native linkage only, not live GPU composition. No shared source/staging or
image changes; all own jobs terminal. Graphics acknowledged split, no concurrent
Main/startup/facade edits; retains pending preview scene wiring.
Next production optional-render compiler/admission integration, coordinated with
CCL owner before edits; parser currently only required request-render and test
helper patches optional wire metadata. Need native software/absent authority
startup before admitted hardware path. Goal remains active.

2026-10-05 USER explicitly approved tasks requested by Graphics. ACTIVE ownership:
native Desktop startup/render admission, final linking, and safe whole-frame
software fallback integration. Graphics retains scene capture, shader/preview
pixel validation, source/backdrop/readback lifetime policies. Coordinate shared
Main/output-pass and facade signatures before edits. Current task list:
1 Link the actual private backend-vulkan (not legacy startup fixture).
2 Production optional-render metadata + trusted launch approval + startup.
3 Wire approved lifecycle/fallback contract and test absent authority/faults.
4 Exact native boot/admitted hardware handoff; promote only reviewed deltas.
No source sync or image staging yet. Existing NUC v60 stays unchanged.

2026-10-05 goal RESUMED by active user continuation; previous turn was progress
(read-only ownership handoff). Starting independent native Vulkan-backend final
link against graphics integration snapshot, with copied/hash-guarded inputs.
No edits to graphics private sources/Main/backend/lifetime APIs. Own new native
link experiment script/output under /tmp, then reusable link tooling after gate.
Production optional render CCL currently lacks optional syntax; existing helper
patches test-only metadata. Will coordinate compiler/manifest interface before
production admission; no synthetic approval. Native link alone is not boot/GPU.

2026-10-05 proposed graphics/compositor work split ACKNOWLEDGED, REVIEW ONLY.
The user's broad compositor goal remains PAUSED. Direct coordination permits
this review and durable handoff, not implementation resumption. No active source
claim/build/staging/lock; proposed ownership below becomes active only after
user authorizes the bounded integration work. Read newest filesystem.md and
private /tmp/cubit-readback-1iQDrc/{integration,baseline}; no snapshot sync.

Agree with split, with an explicit overlap boundary:
COMPOSITOR proposed ownership: userspace/services/desktop/manifest.ccl and
native generated-binding integration; desktop.gpr Vulkan/native linker selection;
tools/build_desktop_vulkan_link.py production final-link work (existing startup
probe modes stay evidence, not production admission); new focused native startup/
software-fallback fixtures under tests/compositor. Desktop main.adb STARTUP and
output-pass fallback/lifecycle call sites only, reviewed against graphics' private
Main delta. Production manifest must use normal CCL generation and approved
startup launch policy; tests/render-startup/desktop_optional_manifest.py currently
patches test metadata/slot62 and is explicitly NOT the production solution.
Any kernel/Makefile, boot-plan, procmgr/devmgr approval or manifest-compiler change
requires their owner's coordination; no global claim on those files.

GRAPHICS retains backend-vulkan/desktop_compositor.adb, Desktop_Readback_Output,
Desktop_Vulkan_Startup and source/backdrop/readback owners, Vulkan_Scene/capture,
preview geometry/shader/binding/submission, pixel validation and associated tests.
Main drawing/capture and Settings preview call sites remain graphics-owned.
Desktop compositor facade signatures, startup owner API and Main output-pass
fallback are SHARED INTERFACES: agree a patch/handoff before either agent edits
those overlapping hunks. I will not independently replace graphics' backend or
lifetime policies. Safe fallback integration means consuming their explicit
Complete/Repaint/Unsafe and retirement results, not weakening them. If API
changes are needed, request a coordinated graphics patch first.

Current review observations: private desktop.gpr includes Vulkan choice and Mesa
source dirs; default is still legacy. backend-vulkan already consumes readback
owner/capture results. Link helper remains described as link/startup checkpoint,
not enabled production GPU rendering. Reported compile/bind and hosted Vulkan
results are not final native linkage/boot. Settings wallpaper preview needs real
new-mode pixel validation and scene wiring before the complete UI path can be
accepted; retain explicit safe CPU fallback until then. v60 NUC teapots are
hardware application-rendering evidence only, not this private compositor.

User resumption needed: authorize the bounded native Desktop startup/admission,
final linking, whole-frame CPU fallback integration and native verification task.
This may be resumed separately while leaving the broader performance/zero-copy
compositor goal paused. After authorization, recheck peer ownership and baseline
hashes, build in isolated outputs with required Nix/shared lock, then verify
no-authority/software and admitted-render paths before any promotion. No image
staging or snapshot-wide publication follows merely from this acknowledgment.

2026-10-05 focused LIVE integration handoff, READ-ONLY; goal remains PAUSED.
Graphics promotion and captured Display incarnations supersede the older
constant-identity gap recorded below. Captured identities still do not mint
cross-service image admission. No implementation, build, staging, or source
ownership claim by this review (only this note edited).

SMALLEST DEPENDENCY-COMPLETE LIVE STEP: opt-in, one-output/fixed-geometry Desktop
GPU scene composition with PRIVATE GPU images and EXPLICIT CPU readback/copy into
an existing Desktop output-pool writer. Keep normal Display presentation and
CPU fallback. This uses the render application's self-bound endpoint exactly
as designed: Desktop is the admitted render client. It does not require Display
as an Intel recipient, DRIVER_GPU rebinding, Image_Lease cross-service import,
external-memory extensions, or native scanout. It also does not eliminate copies
or establish a latency improvement. Unsupported output/resize configuration
must select software under safe lifetime rules, not reuse a mismatched target.

Dependency order and concrete source boundaries:
1. Desktop optional render manifest + trusted startup-plan approval, through
existing procmgr/devmgr admission. userspace/services/desktop/manifest.ccl currently
has no render request. Coordinate manifest/catalog generated bindings and the
trusted boot launch plan with their owners; manifest request alone is not
approval. Use userspace/runtime/gnat/cubit-render_startup.ads policy including
fresh-child software retry for failed/uncertain admission. Never choose a slot
by PID or repurpose Display's virtio endpoint. Startup must verify admitted slot
before Desktop_Vulkan_Startup.Initialize; zero is software-only.
2. Native link of existing musl Mesa service plus compositor C/SPARK bridge:
userspace/mesa/service-device.h; userspace/services/desktop/desktop_vulkan_startup.*;
userspace/lib/compositor/vulkan_device_{owner,storage}.*, Vulkan_Scene,
Vulkan_Submission and upload/source owners. tests/compositor/
build-desktop-gpu-scene-native.py and desktop_gpu_scene_native.gpr are native
archive evidence/templates, NOT a live linked Desktop. Desktop desktop.gpr has
only legacy/mesa choices; backend-mesa/desktop_compositor.adb is the existing
CPU Mesa cache path, not Vulkan. Desktop owner must explicitly integrate backend
selection/native linking and metrics variants; kernel build/staging rules and
runtime/Mesa artifact provenance need coordination under build lock.
3. Capture the COMPLETE selected output scene from Desktop main.adb renderOutput/
drawCurrentScene into immutable Vulkan_Scene (wallpaper, chrome, title glyphs,
client surfaces, clip, cursor). Upload authorized immutable CPU source snapshots
through Configure_Upload/owned sources; retain source readers until GPU/upload
completion. Do not just accelerate client rectangles and omit composition, or
claim passing fixture commands is live scene integration. Admission, allocation
and scene-capacity failure before submission selects a whole software frame.
4. Add a bounded owned READBACK/COPY adapter: transfer the completed GPU image
into a layout-validated linear staging buffer, wait/poll its exact transfer
completion, perform required host-memory visibility/invalidation, and copy into
an acquired writable presentations(Output).Pool slot. Charge readback storage
against the budget; exclude submitted Display slots. Publish that CPU slot using
existing Compositor_Presentation/Display session and retain it until exact source
release. Readback buffer and GPU source reuse require their own completed reads.
Native-gallery-present.h illustrates synchronous completed readback consumption,
not a ready asynchronous Desktop adapter or generic output extent implementation.

CRITICAL existing bridge mismatch: Desktop_Vulkan_Startup.Take_Presentation
exports a ticket only. Confirm_Presentation explicitly attests NEW LATCH AND
EXACT OLD-FRONT retirement. A successful CPU copy or Display source-release
reply cannot honestly satisfy that contract for a GPU target. Add an explicit
completed-target readback-consumer acquisition/retirement operation (SPARK policy
plus narrow FFI), allowing reuse after transfer/CPU consumer drain without
claiming GPU scanout latch. Keep CPU output-pool presentation as an independent
lifetime. Do not call Confirm_Presentation(True) to make a copied-output test pass.
After uncertain GPU execution, quarantine GPU resources; software recovery must
use independent safe CPU targets and explicitly invalidate/repaint scene state,
never overwrite uncertain GPU backing or fall back per draw within the same frame.

Verification gates: software startup with absent render endpoint; admitted live
Desktop rendering of actual apps/chrome at output DPI; exact pixel comparison
against CPU scene (focus/move/occlusion/cursor); stale/duplicate completions,
readback failure, shutdown/device loss, budget/scene overflow and resize fallback;
unchanged CPU source release and bounded input processing. Native QEMU can verify
protocol/software branches; Intel-render claim needs actual NUC execution of the
exact Desktop artifact. Record upload/readback/copy bytes and stage timings.

ZERO-COPY IS A LATER STEP: real cross-service image recipient binding/lease
admission, imported output compatibility, native display ownership takeover and
accepted/latch/retire evidence remain necessary. Image_Consumers/Images provider
is useful groundwork, not authorization to map app memory into Display or grant
Gen12 GPU-read-only access. Driver owner can progress that boundary independently;
Desktop/Display owners must agree usage, recipient capability/incarnation, output
identity, and retirement receipts before connecting it. Kernel physical mapping
handoff requires kernel owner. No broadened rights or constant authorization.

2026-10-05 READ-ONLY review of private Buffer_Requests.Images integration.
Goal remains PAUSED. No implementation/build/staging; only this note updated.
Reviewed private Images/Image_Lease/Image_Layout and current Desktop/Display.

AUTHORIZE: use Display's authenticated output/session owner boundary, not a
Desktop assertion. userspace/services/display/main.adb Output_State already
holds displayOwner, activeSession, currentOutput/leasedOutput/sessionOutput,
backend selection and captured per-output continuations. outputUsable checks
registry liveness/presentability. userspace/lib/display/cubit-output_registry.ads
provides Output_Reference (registry instance, slot, revision) plus Backend_Output
(driver incarnation, driver-local output number). These are the right conceptual
identities, BUT current Display instantiates outputRegistry(1) and registers
Driver=>1. They are local metadata, NOT a production authenticated adapter or
cross-service restart epoch. Do not copy these constants into Key.Adapter or
Key.Output_Epoch as authorization. A capability-bound driver incarnation and
Display-instance/output-generation binding still need a trusted coordinator.

Private Image_Lease.Identity has Adapter, Session, Allocation, Output_Epoch,
Serial but NO intended consumer or output number. Keep a trusted bounded entry
keyed by the COMPLETE Key, recording the authenticated driver session, Display
instance + exact Output_Reference/backend number, intended recipient capability
and process incarnation, validated layout/usage, and current admission/retirement
state. Alternatively expand an eventual identity contract explicitly; do not
assume Output_Epoch distinguishes two outputs with equal local generations.
Authorize(Key,Image) must resolve an already admitted entry and compare the
EXACT descriptor and binding, actual session/BO authority, current output,
recipient and approved usage. Fresh serial must be minted/nonreused by owner.
Caller-supplied PID, local output index, CPU grant, or VkImage is insufficient.
The two Authorize checks around Producer_Drained need serialization with binding
invalidation; the admitted record must remain pinned for the entire lease.
Output changes stop new admission but must not erase old retirement records.
Image_Layout currently supports linear BGRA8 only: arbitrary ANV tiled/compressed
images are out of contract, regardless of CPU span validity.

CONSUMERS_DRAINED: no existing single production predicate establishes this.
Desktop main.adb Output_Presentation.Targets/Pool/Transfer and outputRetirement
track current CPU buffers; compositor_presentation.ads validates exact session/
frame/kernel-token release, compositor_output_retirement.ads separates renderer,
Display lease, grants and storage. These are integration points, not GPU proofs.
Display Output_State.frameState/backendID/backendToken/backendFrame/backendTarget
and finish-frame logic preserve captured submissions. Current DSP.Released is
release of the submitted CPU source after the backend path, NOT native Intel
old-front retirement. CuBit.Backend_Targets describes existing backend target
ownership, not an imported Intel image's all-consumer drain.

Minimal next contract: establish a trusted compositor-to-Display-to-driver
binding before Images.Prepare; freeze one admitted Key/descriptor/recipient in
an entry, reserve every GPU/CPU/display consumer obligation BEFORE handing off
access (including uncertain replies), and close further consumer admission when
retiring. Record each exact authenticated completion against its obligation.
Consumers_Drained(Key) becomes true ONLY for a known matching retiring entry
whose GPU submission readers, CPU acquisitions/derived grant readers, and Display
consumer obligations all have confirmed retirement. An absent/stale entry,
device loss, or lost reply is false, never an empty-consumer success. For a
never-dispatched rollback, prove no exposure occurred before discharging that
obligation. Registry metadata retirement, acceptance, and new-front latch do
not discharge an old image's remaining readers. Retain entry/pins after session
closure so cleanup can complete without authorizing any new access.

Driver can implement coordinator bookkeeping and authenticated binding admission
behind closed production admission; no constant-true callbacks. Production IPC /
capability delivery and real GPU/display retirement receipts are still missing.
Keep CPU fallback/copy presentation. Existing grant writer exclusion is valuable
but cannot authorize Gen12 GPU read-only PPGTT or establish native scanout.
No active Desktop source claim or concurrent build by this session.

2026-10-05 READ-ONLY graphics/compositor handoff review; broad goal PAUSED.
Read current rendering-to-presentation checkpoint, Gen12 access restriction,
service-device.h, vulkan_device_storage.h, vulkan_targets.h, Vulkan_Scene,
Intel_GPU_Buffer_Views and Display GPU operations. No build/test/hardware claims.
NUC teapot rendering evidence belongs to graphics; its completed readback ->
Client_Frame_Pair copy is not evidence of hardware Desktop composition.

(1) Exact missing integration is an AUTHORIZED IMAGE-LEASE PROVIDER plus a
Desktop adapter joining its lifetimes to the existing Vulkan owners/scene and
Display output generation. service-device.h supplies borrowed in-process device/
queue handles after launch admission; device_storage supplies local requests;
Vulkan_Scene supplies immutable commands. None supplies cross-service image
ownership, output acquisition, or presentation retirement. Desktop manifest
currently has display authority but no render request. Future Desktop startup
must use existing render admission, retaining software startup on rejection.

Required provider contract (conceptual operations, NOT assigned wire labels):
- Acquire/import a target lease authenticated to adapter + session incarnation,
  backing/allocation generation and output generation; return validated extent,
  format/modifier/layout, pitch/plane offsets and usage compatibility. Bind that
  lease to the compositor's local VkImage/view/framebuffer; never send numeric
  Vulkan handles as IPC authority. Target writes require exclusive admitted
  ownership, including producer quiescence and all prior consumer retirement.
- Submit completed target with matching render dependency and unique frame /
  lease identity to Display. Explicit accepted, latched, retired events must
  distinguish queue admission, observed presentation of new front, and release
  of each old consumer. Acceptance does not establish latch; latch alone is
  insufficient to free backing with other outstanding readers. GPU fence alone
  says nothing about later Display consumption. Correlate all completions with
  incarnation/output epoch; duplicates/stale messages never advance ownership.
- Close/reset/resize stops admission, drains or quarantines leases; partial
  import or uncertain reply retains pins and grants until confirmed retirement.
  GPU device/child teardown waits for external consumers as service-device says.

Client-source import is a SEPARATE lease, not automatically granted by output
render authority. CPU grant acquisition and source ownership checks must remain.
Gen12 cannot be promised GPU-read-only mappings from a CPU read-only grant.
A trusted, explicitly authorized RW ownership handoff with all other writers
excluded is a different protocol. Until available, upload immutable CPU source
pixels into compositor-private GPU images and report the copy. Native offscreen
composition can likewise use completed readback into the existing Display path
as an explicit interim copied-output adapter; it need not await native scanout,
but cannot be advertised as zero-copy. New-output/consumer leases are required
for eliminating that final copy, not for claiming the offscreen arithmetic ran
on Intel. Desktop must capture complete scenes and retire all source reads
before releasing application frames; per-draw CPU fallback after uncertain GPU
execution is unsafe. Whole-frame software fallback requires quiescent ownership.

(2) No active source edit/build/staging ownership or lock in this session.
Only coordination/compositor.md edited for this requested handoff. Historical
compositor policies/FFI and Desktop focus fix remain review context, not an
exclusive active claim blocking driver development. Latest local titlebar fix
was source+native-link tested, not a NUC visual validation or new image staging.
No authorization to resume broad Desktop/Vulkan integration is inferred.

(3) Driver-owned work can proceed independently: authenticated allocation/export
lease metadata and pins; retained CPU-reader lifecycle; producer completion and
writer exclusion; bounded dependency/retirement bookkeeping; same-adapter layout
validation; stale incarnation/reset/resize/partial-failure tests. Preserve
Share_Retained's independent pin and uncertain-creation/revocation retention;
it remains CPU-only, not an implicit GPU importer. Prepare and test provider
operations behind closed admission without editing Desktop's attachment ABI.
Native scanout needs exclusive firmware-writer handoff FIRST: close every
physical mapping admission path, drain kernel/userspace writers and aliases,
confirm CPU TLB retirement, validate current plane inventory/physical aliasing,
then observe actual hardware latch and old-front retirement. Existing linear
flip planners are not that handoff. Kernel changes require coordination with
kernel owner; this review grants no wider ownership. Do not enable plane writes
based solely on GPU_IS_PRIMARY, metadata planning, or a successful CPU grant.

2026-10-03 titlebar focus artifact SOURCE FIX COMPLETE; broad goal still paused.
New damagePreviousFocus in Desktop Main adds previous title/frame and taskbutton
before focusAndRaiseSurface, restoreSurface and toggleMaximizeSurface mutate focus.
Existing same-focus/top early exit retained. Explains photo's partial gray title
under stale teal strip: old title absent from damage while union crosses it.
New tests/compositor/test-focus-title-damage.py extracts actual helper/raise/focus/
restore and tests partial overlap, separate windows, reorder, no-op, missing and
minimized focus. Nix58964 TERM0: fixed passes, removed-call negativecontrol fails;
existing test-desktop-focus.py passes two negativecontrols too. Evidence directories
focus-title-damage-uqc69py1 and desktop-focus-routing-quymjjx8 under compositor/build.
Native metrics-enabled legacy compile/link30294 TERM0 under shared lock:
/tmp/cubit-focus-title-desktop.svc, /tmp/cubit-focus-title-native.log. Native runtime
visual regression/NUC confirmation NOT run. No boot binary/ISO/index changes; native
object cache updated only. All own jobs terminal. New helper is adapter regression
coverage, not an additional SPARK proof. Source ready for next Desktop image build.

2026-10-03 USER-REQUESTED titlebar focus artifact fix ACTIVE, broader goal paused.
Own Desktop main focus-damage helper/call sites and new extracted routing test.
Photo shows partly repainted inactive title. focusAndRaiseSurface damages only
new focus, omitting old title; restore/maximize have same omission. Will include
old title/task damage before focus mutation, preserve other agents' input fixes.
No native build/staging yet. Browser latest note reports own jobs terminal.

2026-10-03 refreshed browser publication coordination: broad goal remains PAUSED.
No concurrent Desktop Main ownership, source edits, builds, or staging by this
compositor session. Earlier conditional boundary review still applies to the
unchanged three-hunk exposure watermark fix with queue32; no new concern from
the reported logging-runtime rebase. This is not fresh candidate validation.
Browser reports rebasing onto current runtime and verifying both exact default
and metrics candidates through logstore and Penny. Retain source/linked-input
and per-variant staging guards under shared lock; preserve current kernel/ISO/
user disk and active VM. Prior artifact hashes above are historical, not current
publication expectations. No compositor goal resumption or outgoing message.

2026-10-03 publication provenance query: READ-ONLY, goal PAUSED.
This compositor session has NOT rebuilt or staged Desktop since dd73845a;
recent actions read sources/evidence and update this note only. No own livejobs.
Confirmed root build/desktop.svc acd382ecb3f0d1c63fde55ea20c51ddfd87a10d7e8479626c0569af14bc8d0d5
mtime10:49:23; isodir/boot/desktop.svc34f139336743184563f6f615ac9b2fba16a89368d64575f9906a6faca8165722
mtime10:49:40. IMPORTANT: staged isodir hash EXACTLY MATCHES current Desktop
build-metrics/desktop.svc. This identifies the matching variant, NOT the actor,
authorization, active job, or source provenance. No exact hashes/timestamp found
in coordination notes. Sandbox ps exposes only this invocation, so it CANNOT
establish host build inactivity. Preserve both artifacts and keep publication
stopped pending builder provenance; do not replace the old hash guard merely
because compiled source hashes agree. No outgoing thread message sent.

2026-10-03 private exposure-watermark boundary review: no implementation or staging; goal remains PAUSED.
Reviewed private penny-input-buffer-lmoby8wv Main against main-before-exposure.adb.
No boundary objection to the narrow three-hunk change: freeze nonempty validated
batch Through before Publish; require newest serial > exposedThrough AND existing
close barrier for coalescing; initialize watermark with the channel. This covers
partial writes, complete writes with uncertain loan return, and failed replies.
Acquisition-failure freezing is conservative headroom cost, not acknowledgment.
The nonempty guard is essential: empty Through equals caller-supplied After and
must not freeze future events. Monotonic max preserves earlier exposure. Recovery
retains serial progression and watermark; channel destruction resets both.
Close latch remains independent and prior-After acknowledgment remains unchanged.
Conditional code-review acceptance only: retain queue32 for this fix, run actual
handler/enqueue interleavings with success/acquire/write/return failure, same-After
retry, empty high-After request, close ordering, overflow/recovery and fresh channel;
no-freeze negative control must fail. Native and exact-candidate logstore gates,
source-drift checks and shared publication lock still apply. Main adapter behavior
is regression evidence, not a new end-to-end SPARK proof. No permission to resume
the broader goal or stage binaries is inferred from this review. No outgoing
cross-thread message sent; review recorded here for shared coordination.

2026-10-03 expanded input architecture review, READ-ONLY; goal remains PAUSED.
Concrete correctness finding (not the Penny keyboard-overflow cause):
Published batch motion can be silently lost through subsequent coalescing.
Reproduced actual copied SPARK policies underNix53641 TERM0: Push motionA
(serial1,payload100), SnapshotAfter0 publishesA butretainsit, Push motionB
(payload200) replaces same serial1, SnapshotAfter1 returnsnothing, AckAfter1
clearsB. Clientneverreceives200; noRESYNC. Main batchPublish applies onlyoldAfter
and IQ.Push has no published-through watermark, so this is a feasible service
interleaving. Existing localqueue contracts allowit: proof of those contracts
is not proof of end-to-end delivery semantics. Persistent reproduction/log/
sourcehashes: tests/compositor/build/input-architecture-review/. Fixpriority1:
freeze any client-visible serial against mutation; coalesce only unexposed tail
(or issue a newserial with correctlyspecified supersession). Track monotonic
published-through perstablechannel on confirmedpublication; account for failed/
uncertain publication conservatively. Test sameAfter retry, motionAFTERsnapshot
beforeack, publication failure, motion/buttonbarriers,close,reset andoverflow.
Largerqueue does NOT fix this bug. Do not publish128 as "inputcorrectnessfixed".

Confirmed architecture limitations, not newly demonstrated corruption:
- Device ingress: xHCI sends orderedkeyboardreports withsequence; failedsend
sets keyboardResync, snapshotscurrently0. Desktop tracks authority/device/
generation/sequence, rejects exactduplicates andresynchronizes gaps. Modifier
state resets onkeyboardgap ratherthan completeheld-key reconstruction. Pointer
has boundedpending/coalescing andsnapshot. This review did NOT validate all
kernel source-authentication paths or PS/2/virtio equivalence; those are open
cross-device conformance gates, not allegations of a capability flaw.
- Desktop per-stablesurface32-event history/batch8 andpriorAfter semantics are
bounded/loss-aware. Deliverypool retains uncertain grants afterchanneldeath.
Per-turn 500usinput/1000usrequest admission does NOT preempt one handler or
software scene draw. Main drains input before/after requests but synchronous
painting/IPC can delay nextdrain. Device-sourcegap andappqueueRESYNC are
separate counters and must not be conflated.
- UI.App lastEvent advances when copied into event/localpendingEvent, not after
Handle_Event. This is a receipt/local-retention ack, not a bug by itself.
- UI.App.Run renders after dirty nonmotion input torefresh immediate-mode hit
maps; orderingbarriers are required withcurrentwidgetdesign. This couples
keystroke handling to synchronouspaint, and configure/resync can also block on
theme/bufferIPC. Confirmed blockingarchitecture; no WCET provided bypollcaps.
- Loss recovery is pointer-centric: UI.App andSurfaces clear/resync capture,
but no common text-edit completeness/loss latch. Netsurf Edit_Location Enter
callsGo; no INPUT_RESYNC branch found. Penny has explicitAddress_Input_Lost.
This confirms missingcommon loss policy, NOT a tested wrongURL submission.
Require injectedgap+droppedcharacters+Enter regression inordinaryeditor/browser
before attributing concrete user-visible mis-submission.

Recommended fix order / defensible general design:
1 Freeze published input records as above; add end-to-end snapshot/ack tests.
2 Define shared input-loss generation/notification contract for apps/widgets:
cancel transientcaptures; mark affected incompleteedits and require explicit
recovery for consequential submit. Resync cannot reconstruct missingtext.
Test ordinary UI.App clients as well as Penny. Do not suppress gap safety.
3 Keep128 private as measured headroom proposal pendingpriorproof/logstore/
variant gates. It is orthogonal boundedburstcapacity (+36KiB), not latencyfix.
4 Separate cheap ordered model/interaction updates from expensive rasterwork.
Only coalesce motion under documentedbarriers; freeze or refresh hitmap/layout
on cheapmodelgeneration boundary. Never dispatch usingstalecoordinates. Render
lateststates on bounded opportunities, withimmutableframe snapshots, bounded
inflight buffers andoneevent-thread modelowner. Break up longCPUwork or use a
separate renderer with explicit ownership; do not move UI.App.Window polling
onto an unsynchronizedworker. Configure/themebufferIPC needs explicit defer/
completion phases before any genuinely nonblockingeventdispatch claim.
5 If independent ingress is required, design transport-only boundedspool with
separateauthority/CQ routing, immutable orderedserials, admission-before-ack,
explicitoverflow andprocess-exit retirement. Existingonewait asyncAPI rejects
batchedmode and cannot continuouslydrain a blockedowner; notdrop-in rescue.
6 Acceptance: device->Desktop->localreceipt->handler->paint->present timestamps,
occupancy/highwater+source/appgapcounts; exactkey/buttonorder under slowpaints,
all8clients/flood, focus/resize/close, collectorabsence and grantfailures.
Report CPUtime separately fromguestwall; no240Hz/1msclaim from queueheadroom.

No productioncode/staging changes made. This review supersedes any interpretation
of earlier128 ownershipacceptance as whole-input architecture approval.

2026-10-03 Penny128 integration review, read-only; broad goal remains paused.
Inspected strict audits7604javo/1zfrahql: baseline recovery[0]/resync1 vs128
no recovery/resync andexact8URLs; bothsurvive. Candidate maximum1593ms paint
stillshows latencyproblem, notperformancefix. Reviewedslowconsumer fixture:
8fullqueues,sameAfter immutable retries,ack8/replenish/drain serial+payload+
target+kind ordering,explicitoverflow. Hostedmatrix resultreports32/128PASS;
permutation multiplier13 is coprime for both capacities. RootstagedDesktopstill
dd73845a; rootCapacity32,batch8,poll32. Snapshot manifest.ccl,desktop_logs.adb,
GPR equalroot. All compiled-root-inputs entries matchroot NOW, but thatmanifest
records BASELINE queue32 while candidatequeue differs. Treat intendedcapacity
change explicitly, not as proof all candidatecompiledinputs equalmanifest.

No compositorownership objection to narrowlypublishing Capacity128 plus
capacity-generalized existingtests/newslowconsumer fixture. Conditions before
staging from compositor side:
1. Run applicable SPARK queue/ack/batch contracts at128 with zero unproved
(or report exactremaining proof gaps); capacity changes quantifier/loopbounds.
Keep existing closepriority/coalescing/serialexhaustion tests, not only keyflow.
2. Capture actualcandidate dependency/sourcehashes including queue128, runtime,
UI, fonts/nativeobjects, GPR+scenario variables, generatedmanifest/bindings and
linkedinputs. Recheck currentroot underheldlock, allowing only reviewedpatch.
Do not blanketcopy snapshot sources. If changed inputs affectcandidate, rebuild
and rerun appropriategate; no automatic need foranother36cycles if exacttested
binary+dependencies remainvalid. Preserve currentkernel/ISO/Penny artifacts.
3. Explicit defaultvariant legacy,metrics-off,timing-off,productionstorage,
productiondisplay mode; no fontfault/testhooks/tracing-only Main. Preserve
Desktop manifest logstore request and async Desktop_Logs hooks. Verify actual
candidate through native logobserver startup+retainedsoftwaretext record
retrieval (existing test-desktop-logs-native observer can be reused); metrics
must NOT accidentally be enabled without its manifest variant.
4. Immediatelybeforepublication verify stagedpreviousDesktop hash, savebackup,
then publish exacttestedcandidate to defaultbuild/desktop.svc and
kernel/isodir/boot/desktop.svc underlock; verifyhashes andretain manifest+audits.
No kernel/ISO rebuild or otheragent binary staging required by this change.

+36KiB static across8queues is acceptable measured boundedheadroom; do not
claim it fixes longpaint latency or guarantees arbitraryinputbursts. No wire,
batchsize, grants, capslots, acksemantics or per-turnpollcap changes approved
by this review. This is ownership/integration guidance, not an action here;
Penny remains responsible for its userauthorization and publication workflow.
No code or binaries changed by compositor.

2026-10-03 Penny buffering review, READ-ONLY; no implementation/staging; goal paused.
Current source: IQ.Capacity32 per stable surface, MAX_SURFACES8; batch snapshot
capacity8 is separate. Successful batch Publish applies ONLY previous After;
newly published events remain server-retained until later acknowledgment.
Thus local cached events do not buy eight EXTRA server slots. Ordinary input
Pop has different removal behavior; preserve both transport semantics.
Overflow emits explicit RESYNC from current authoritative state; adjacent motion
coalescing only. Never coalesce/drop/reorder key/button transitions or silently
turn a truncated address into a valid submission. Keep close priority/barriers.

Recommendation: first prototype a larger FIXED server queue in isolation,
size from measured peak event rate * observed consumer blackout + burst margin
+ retained unacked prefix, counting key-down AND key-up. 128 is a candidate to
measure, NOT a guarantee. Eight surfaces bound worst-case static cost; compare
8*(newcapacity-32)*Event'Object_Size/8 using actual compiler layout. Keep batch8,
wire format, grant extent and per-turn drain cap32 unchanged. Largerqueue's
linear scans/proof cost need overload measurement; it protects history but does
not improve one-second key-to-response latency. No finitebuffer coversunbounded
paint stalls. Prefer measured fixedheadroom before newnegotiation/spill protocol.

If negotiation/spill is needed: use a Desktop-owned bounded record pool with
per-owner and global reservation caps, charge only on admission, authenticate
surface owner+incarnation, fail extension atomically without dropping existing
entries. Preserve one globally ordered serial stream across base/spill; prior
After acknowledgment alone frees retained entries. No allocation per event.
Surface destruction releases metadata; quarantined delivery grants stay in the
existing service-lifetime Input_Transfers pool until confirmed return. Raising
queuecapacity does not authorize more reply capabilities, loans or transferpages.
Requiredproof/tests: no serialreuse/reordering, sameAfter retries, wrongowner/
generation, pool exhaustion/resync, closebarriers, teardown withpendingloan,
slowconsumer floods acrossall8surfaces and validkeysequencefollowingrecovery.

Existing async bridge: UI.App Submit_Input_Wait/Complete_Input_Wait is ONE
outstanding wait, explicitly rejects batchedInput, requires completion pumping.
It can remove one event into a reply but cannot continuously drain while the
same applicationthread spends1097ms painting; not a drop-in solution. Current
Client_Input_Channel.Fetch is synchronous, serialized process-lifetime page.
A worker cannot share UI.App.Window or poll/steal its completionqueue safely.
Independent ingress would need a transport-only owner, explicit capabilities/
completionrouting and bounded client spool, acknowledgment only after durable
local admission, then event-thread application of configure/resync/theme/resize.
That is a new ownership protocol, not "run Poll_Input on another thread".

Longer-term fix remains shorter/preemptible or separately owned rendering with
input service opportunities. UI.App cache-only processing can still perform
configure/resync theme/buffer IPC, so separate those durations in traces. No
ownership objection to a future narrowly scoped queue experiment after review,
but this note publishes no code or approval of a blanketcapacityincrease.

2026-10-03 Penny Poll_Cached_Input review: IMPORTANT blocking-contract caveat.
Reviewed private UI.App Take_Cached_Input/Poll_Cached_Input/Receive_Input.
No ownership objection to this narrow factoring as a NO-INPUT-FETCH API,
subject to invalid-cache/ordinary-fallback regression. Cached DP.Success
still returns through Valid even when found=False; rejection clears cache and
leaves lastEvent unchanged, preserving ordinary fallback's After value.
HOWEVER it is NOT no-IPC/nonblocking as described: Take_Cached_Input calls
Apply_Input_Result; INPUT_CONFIGURE calls Refresh_Theme (capCall perchunk,
privateApp line390) and Ensure_Buffer (capCall line170); INPUT_RESYNC also
calls Refresh_Theme. This path exists even with a healthy validated cache.
The new spec's "Never fetch or wait" must say no input fetch/wait, with explicit
configure/resync processing can still block, OR broader processing must be
separated under its own reviewed policy. Do not silently defer theme/resize
side effects: event payload/geometry and stale-controls behavior depend on them.
For Penny's deadline rule, claim only no ADDITIONAL INPUT fetch beyond deadline;
retain handler/paint wall-time caveat. Validate cached configure and resync,
empty/pendingwait/batchdisabled guards, wrongsurface/After and ordinarypoll
fallback; measure input-fetch IPC separately from theme/buffer IPC. A/B36cycle
claim came frompeer, not rerun here. No sharedcode/staging or broadgoalresume.

2026-10-03 Penny cached-drain review (read-only; broad goal remains paused).
Reviewed private penny-demand-nq9qtuvx Servo_Input_Admission and UI.App getter.
No compositor ownership objection to eventual source publication of ONLY
Cached_Input_Count declaration/expression body with event-thread-only comment,
under shared lock after hash-guarded review; no shared binary staging implied.
Getter is exact Remaining(inputCache), allocation/IPC/ack-free. Its observation
supports the proposed sole-consumer usage. Predicate preserves Controls_Stale
and strict Used<32; uncached work still calls existing Can_Poll. Count is polls,
not a guarantee of 32 delivered events or bounded handler wall-time.
CAVEAT: Cached>0 does not by itself guarantee App.Poll_Input performs no IPC.
Receive_Input falls through to synchronous capCall if Cache.Take rejects, e.g.
identity/ack mismatch. Current zero rejections supports healthy-path A/B only.
Keep monitoring Cache_Rejections; do not claim an absolute no-fetch-after-budget
contract without a cache-only consumption API or an explicit matching-invariant
proof. Such API/policy changes need a separate narrow review, not hidden in getter.
Confirmed trace lines973-982 bracket input_resync in1097ms guest-wall paint;
controls-stop immediatelyafter hascached7. Supports localdrain opportunity,
not resolution of long-paint/serveroverflow. Preserve stale-control and address
input-loss guards; publication of getter should not silently change either.
No code/tests/staging performed and no cross-thread message sent.

2026-10-03 Penny input coordination, READ-ONLY boundary review; goal stays paused.
No shared UI/input edits or staging in progress by compositor. UI.App owns
Window.inputCache, delivery/ack bookkeeping and completion routing; Penny owns
its custom Input_Batch/Controls_Stale/paint scheduling. Cache.Remaining already
exists as a pure query, while public Input_May_Remain combines cache occupancy
with the server More_Pending hint. Its current spec comment understates this.
A narrow diagnostic accessor (Cached_Input_Count(win) returning Natural via
Client_Input_Batch_Cache.Remaining) is acceptable from compositor ownership
perspective; no ownership objection to Penny proposing it under the shared lock.
It must be event-thread-only, no Fetch/Take/ack/grant/poll side effects. This is
not authorization to alter budgets, bypass Controls_Stale, or stage shared UI.
Existing fetched-minus-delivered counters are not an exact occupancy API after
cache clears/rejections/reset; do not substitute that as an invariant.
Source review: Receive_Input fetches only on empty cache then takes one event.
Servo Poll checks Controls_Stale and 1ms budget BEFORE App.Poll_Input; budget
counts32 polls, first always admitted. A slow fetch can therefore consume the
wall-time budget before subsequent cached events are delivered. Hypothesis,
not proof of reported resync. xwruw4zw sample nearline949 has fetched=89 and
 delivered=89, so that snapshot alone does not show stranded cache entries.
Please correlate before/after fetch duration, exact cache count, budget stop,
Controls_Stale stop and repaint begin/end. Preserve address-input-loss safety.
No message sent to peer thread: request received, but no new user authorization
for outgoing cross-thread messaging in this turn. Guidance available here.

2026-10-03 Desktoplogstore complete for explicituserrequest. 79410 TERM0
normalnative125%cursor/menu gate+stagingPASS mim13l_p, exactbinarydd73845a.
62927 TERM0 actualstagedbinary logstoreobserverPASS logstore-evidence:
boot-logs retrievedDesktopinternal-shell andretainedsoftwaretext records.
Defaultbuild/stagedDesktop updated; rollback54075378 retainedincheckpoint.
Published host/native tests/docs. Mainlogging adapterbounded32records+oneSDK
page,shareduniquetokens,no collectorwait; manifest grantslogstore. Serialcopy
retained. Native andmocktested; notnewwhole-serviceSPARKproof. Broadgoalstill
PAUSED, no update_goal/resume. No ownlivejobs; no kernel/ISO/procmgr/runtime
orGitindex changes. Next hardwareimagebuild includesmanifest/source changes.

2026-10-03 explicituserrequest Desktoplogs tologstore (broadgoal remainspaused).
Added Desktop_Logs bounded32recordFIFO+existingSPARKtextframer+asyncSDK;
Main localdebugPrint preservesstdout andqueuesrecords, completionroutinguses
shared requestSequence, Pumpafterinput/render. Manifest requestslogstore.
20390 TERM0 nativeobserverPASS v_7jm3yn: boot-logs readsactualDesktoprecords
fromlogstore includingretainedsoftwaretext/startup. 78865 TERM0 hostedactual
adapterPASS fragmentedCRLF,busy/foreignCQE,32capacity+8drops,oversizedline,
unavailablecollector1000attemptsno resubmit. No stageyet; next standardnative
regression/exactbinarystage. No kernel/runtime/procmgr changes. Scripts published.

2026-10-03 postcheckpoint dependency audit1. Previous turnPROGRESS staged
softwarecheckpoint. Rechecked service-device.h/native_gpu_presenter.c and
provider note: still deviceborrow plus CPUgrantforwarding, no authorized
compositorGPUtarget/Displayimport+latch/oldfrontretirement API. StagedDesktop
still54075378. No ownlivejobs orverifiedwait. Perusercheckpointsteering no
furthermarginalsoftwareoptimization. Next majorintegration requiresprovider
contract; existing request/docs retained. Goalactive, NOTcomplete; this is
first consecutive dependency-only turn aftercheckpoint, notblockedthreshold.

2026-10-03 softwarecheckpoint26306 TERM0 STAGED540753787e4c369cce68a7bcc9076ec7b2b9513030d56ebe99e6e9856e030c14.
Exactnormal legacy metrics-off/timing-off/production binary passed125%native
16cursor moves4rounds3menu restoration, cullactivation,no textfault. Source
andpreviousdestinationhashes checked underlockbefore atomicfilepublication.
Both userspace/services/desktop/build/desktop.svc andkernel/isodir/boot/
desktop.svc match. Backups+inputs+nativeevidence+staging.json retained in
build/software-checkpoint-hf87kw6f. nm confirms softwaretext/rowcopy andno
private testarm/fontwrap symbols. run-desktop-fast nowusesnewsoftwareDesktop.
No kernel/ISO/driver/browser/index/commit changes. No ownlivejobs. Goalactive;
softwarecheckpointconsolidated, remainingmajorworkGPUsharedtargets/presentation
andhardwaremeasurements. Avoid marginalsoftwarechanges absentnewfindings.

2026-10-03 softwarecheckpoint26306 LIVE /tmp/cubit-stage-software-checkpoint.log.
Script holds sharedlock across explicit legacy/metrics-off/timing-off/production
compile+privatelink,125%nativecursor/menu gate,hashguardedpublication. Backsup
both previousbuilddesktop andstageddesktop in unique software-checkpoint dir.
Publishes only afterPASS+activation+no textfault+source/destinationhashmatch.
Usercheckpointdiscussion: stop marginaloptimization; consolidate defaultsoftware
for run-desktop-fast. Metrics-on remains separatelytested variant. No Gitindex,
kernel/image/browser/driver staging by thisscript. Poll26306 exacthandle.

2026-10-03 overload30585 TERM0 PASS4workers/25refreshes/6pausecycles/close.
Retained jxqtxsey inputs/result/serial in cull-overload-comparison/candidate.
No timingcomparisonclaim yet. User asks remainingwork/shippingstatus: inspected
stagedDesktop bf951bca (Oct3 07:05MDT), no rowcopy/softwaretext symbols,
no cull/workmetrics markers. Currenttestedmetricsbinary82fe52d5 differs.
run-desktop-fast reusesstagedbinaries; latestownwork notstagedbyourtests.
Softwarecheckpointreadyfor consolidation; avoid furthermarginaloptimization
beforeGPUprovider. No ownlivejobs; goalactive, notcomplete/pause/block.

2026-10-03 cullingoverload30585 LIVE sharedlockheld,4worker nativeviewer
/tmp/cubit-cull-overload.log, persistentTMPDIR/cubit-overload-evidence.
Exactcurrentmetricsbinary/Main hashmatches priornativecullingregression.
Savedcandidatebinary+inputhashes build/cull-overload-comparison before run.
Next poll30585,verifyworkers/interactions/cullingmarker,matchedredrawsummary.
Comparison remains exploratorysharedhostTCG, nothardware/causalisolation.
No staging/indexchanges. Previous turnPROGRESS targetedfaultclosure.

2026-10-03 targetedculledfault67604 TERM0 PASS b2noula7. PrivateextendedGPR
Main overlay resetsCtrigger everypass,arms onCull; wrapperpermitsonefontcall
thenpersistentfailure. Serial645Cull,652replay,653CPUfallback;one startup,
125%16moves4rounds3menu restoresPASS. Closes specific culledpassfailuregap.
31431 TERM1 initialGPR relativeRuntime, fixedabsoluteRuntime/linkerpaths.
Published test-culled-software-fault.py; postrunmetadata-only provenanceguard
andprivateMainhash added; behavior testedunchanged. RootMain/stagingunchanged.
No ownlivejobs. CurrentPROGRESS faultcoverage. Goalactive hardwareunfinished.

2026-10-03 fault84432 TERM0 PASS publishednativewrapper oncurrentMain.
software-raster-fault-943z89nr:one replay/CPUfallback/startup,125%16moves4rounds,
3menu restores. Cullmarker once AFTERfallback (serial607/609vs648), NOTfault
insideculledredraw; recordedexactscope inresult/docs. Sourcehashesrecheckedby
wrapper. No ownlivejobs; sharedlockfree. Previous turnPROGRESS nativecull;
currentPROGRESS faultinteractionevidence. Goalactive, GPUproviderstillpending.

2026-10-03 native fault84432 LIVE publishedtest-desktop-software-fault.py,
/tmp/cubit-cull-raster-fault.log. Wrapper locks compile/bind/link and uses
private seededbootafterlink. Tests persistent fontrasterfailure afterfirstcall,
whole-scene replay,125%cursor/menu restoration,singlestartup. CurrentMain
includes separablesampling andwallpapercull. Need assertcullingactivation too
if wanting intersectionclaim. No rooteditsduringbuild; othernotesread.
Previous turnPROGRESS nativecullclosure; currentfaultvalidationinprogress.

2026-10-03 wallpaper native32335 TERM0 PASS180s fullmixedoutput gate.
Observed cullingactivation plus125/150%,primary,arrangements,splitdrag,
maximize,wallpaper/cursor restorationPASS. Inputs allhashmatched before/after.
Evidence retained build/wallpaper-cull-native-evidence (inputs/result/logs/
screenshots). Docs scope updated. No ownlivejobs; sharedlockreleased.
Previous turnPROGRESS implementation; currentPROGRESS nativeverification.
No staging/index/commit changes. Goalactive,hardware targethandoff unresolved.

2026-10-03 wallpaper cull62787 TERM0 native metricsbuildPASS. Native32335
LIVE sharedlockheld full180s mixedoutput/scaling/arrangement gate; logs
/tmp/cubit-wallpaper-cull-{run,serial}.log. Inputhashes captured in
build/wallpaper-cull-native-evidence/inputs.json; recheckaftertest. Mustrequire
activationmarker plus nativePASS, not assume branch exercised. Policyuses
existingprovedClip; shelleligibility/matchingopaquefill is nativeMain trusted
integration,notwholeDesktopproof. No ownotherjobs. No staging/indexchanges.

2026-10-03 own Main wallpaper culling: shell/nativeonly, skipfull damage
when one eligible visible window's clamped opaque body physicalClip equals
outputDamage. Uses existing SPARK clip, no regionqueue/allocation. Guards
match shellrendering, no clientopacity assumption. One-shot activationlog.
Need nativecompile/mixedoutput regression and observedactivation. No changes
to otheragents sources. Sharedlock heldduringedit.

2026-10-03 separable native69430 TERM0 PASS full180s mixedoutput gate.
Scaling125/150%,primary preview/revert/apply,arrangements,seams,splitdrag,
maximize,cursor/wallpaper restorationPASS. Retained logs/screenshots and
postrun source/binaryhashes build/separable-native-evidence. Docs updated.
No ownlivejobs,sharedlockreleased. Previous turnPROGRESS implementation+
proof; currentPROGRESS nativeintegration closure. Goalactive,hardware still
requires sharedtarget/presentationprovider. No measuredspeedupclaim.

2026-10-03 separable-sampling70126 TERM0 PASS5356800 pixel comparisons
all numerator/denominator1..16,negativeorigins,density29x23vslogical17x13.
Sampler SPARK48checks10flow38prover0unproved. Native metrics75397 TERM0
compile/linkPASS. Native mixed-output69430 LIVE under sharedlock,180s:
/tmp/cubit-separable-mixed-run.log and -serial.log. ExplicitmetricsDesktop,
private base from retainedviewer1iige2uh; CUBIT_TEST scaling/arrangement/primary.
Poll exacthandle, no restart for timeout. No sourceedits while running.
No measuredspeedupclaim; rawcopyvolumeunchanged. No staging/indexchanges.

2026-10-03 own Main scaled-client optimization: unrotated fallback hoists
proved Axis Y computation per row; X remains per pixel, no newmemory. Unit
rowcopy still first; rotated Map path unchanged. Need equivalence regression,
SPARK sampler proof, native compile/mixed-output gate before completionclaim.
Edits held sharedlock; peer driver/browser notes read, no ownership overlap.

2026-10-03 published tests/compositor/test-work-metrics.py; exactrunner
57076 TERM0 PASS40checks11flow29prover0unproved/justified. Snapshot
work-metrics-vxdpfw6d records/rechecks sourcehashes. Nix initialsandboxfailed
cacheaccess; escalatedrunpassed. Docs include reproduction. No ownlivejobs.
Current-source recheck: Main Begin_Output still through existingfacade;
Desktop_Vulkan_Startup Configure_Targets documented firstoutput/privateimages,
no Displayauthority; GPUsharedtarget interface remains outstanding. Do not
activate via CPUgrant or localtargetticket assumption. Previous turnPROGRESS
nativeevidence; currentPROGRESS reproducibleproof. No index/staging changes.

2026-10-03 workmetrics native68654 completed: authoritative log/result PASS,
39 eight-row refreshes,20 pause cycles,graphs/table/close. Visually inspected
table: scene_pixels andrepair_pixels each35 samples,p99 bounds2621440/0count.
Preserved result/inputs/serial/table in build/work-metrics-native-evidence.
Viewer counter column is histogram bound,NOTtotal/rate; docs now explicit,
including drop semantics. No newhardware performance claim. Previous status
turn yielded terminal native evidence; thisturn closes visibility/documentation.
No source/build/index/staging changes; goalactive.

2026-10-03 workmetricsproof82577 TERM0 PASS40checks11flow29prover,
0unproved/justified, snapshotwork-metrics-7m09iixm exactinputhasheschecked.
Nativeexpandedstream68654 LIVE; artifactpersistent/cubit-observatory-viewer-
1iige2uh, log /tmp/cubit-work-metrics-native.log. Sharedlockheldbytest.
No otherownlivejobs. Next confirm8keys andvisibleworkcountervalues andrecord
nativeevidence. Goalactive.

2026-10-03 typedworkmetrics integrated: newCompositor_Work_Metrics keys7/8
Counter/Count desktop.scene_pixels/desktop.repair_pixels deltas, notcumulative.
Main emitStats reusesexistingnowMs timestamp (guardmultiplyoverflow), append
onceperreportbeforecountersreset. PublishernewRecord_Work; on/offfacades;
batchpolicy8descriptions/55samples/page, still2pages, no newqueue/waits.
18820 TERM0 actualSDKpublisher18faultmodes inclmixedwork+invalidtimestamp,
2pagestream890drops/immutability/redeclaration and1000batchpolicytestsPASS.
18157 TERM0 native metricsbuildPASS.23436 TERM1 proofharnessmissinggnat2022;
82577LIVE correctedprivateproof /tmp/cubit-work-metrics-proof-r2.log.
NativeObservatoryexpandedstream justdispatched undersharedlock,
/tmp/cubit-work-metrics-native.log persistentTMPDIR. Poll exactnewhandle.
No collector/Observatory/schema changes. Neednativeworkrecordvisibilityand
proofclosure; sourceonly, stagedservices/indexunchanged.

2026-10-03 splitcounter25083 TERM0 build;22574 TERM0 nativeviewerPASS39updates.
Persistentzrd8hdbq result+inputs+serial copiedsoftware-row-comparison/counter-
evidence; assertedpositivescene_px andzero repair_px in allsceneintervals.
TheseareDIAGNOSTICLOGFIELDS, notyettypedworkmetrics. Currenttypedpublisher
onlyrelease+4stagedurations,6metadata descriptors. Next exposeworkrecords
keys7/8 viaCompositor_Work_Metrics pureSPARK andbatchdescription8slots; keep
SDKtwo pages, boundedappend, no synchronousIPC. Countercollector sums values,
so workrecords must bedeltas, NOTcumulativecounts. Reuseexistingclock sample
ratherthanextraper-pixelclock. Allownjobs terminal,lockfree. Goalactive.

2026-10-03 optimizedrowcopy mixedoutput82279 TERM0 PASS180s.
Savedcurrenthashverifiedresult/logs d7hfkojk/mixed-evidence; splitdrag,
primary/maximize,arrangement,125/150%scaling,seams/restorationallPASS.
Then telemetryonly Main change: native renderOutput increments statsScenePixels
andlogs scene_px; repair_px now remainsdirectpathrepaironly. Existingcounter
usedto combine unlikequantities. Historicalcomparisonparser accepts oldfield
onlyifnew scene_px absent. PreservedprechangeMain withmixed evidence.
No drawinggeometry/pixelcodechanged aftertest. Pendingnativecompile/counter
observation. No staging/index/commit/push. Goalactivehardwarepathpending.

2026-10-03 normalrowbuild1672 TERM0 PASS d7hfkojk privateexecutable.
82279 LIVE sharedlocked mixed-output gate,180s4CPU1GiB,
/tmp/cubit-row-copy-mixed-{run,serial}.log. UsesprivateDesktopoverride+private
baseimage; normalharnesscurrentlypreparingupdatedsharedbootprerequisites.
Poll82279, no restart. Rowcopyproof/overload docs publishedwithTCGlimitations
and repair_px semanticwarning. No new productionedits whiletest runs.

2026-10-03 rowcopyoverload79998 TERM0 PASS26updates/4workersfinish.
Persistenttprfck13 artifact, copiedbaseline/candidate result+inputs+serial to
software-row-comparison. New summarize-client-redraw.py selectsoneframe,
ev0/full0/client425600px/scene426132px intervals, rejectsfailed/missingtests.
11baseline samples mean244.91ms,14candidate110.43ms (~54.91%lowerobserved).
SHARED-HOST TCG,notcausalisolation/WCET/240Hz/photon. Samepixels, notcopyvolume
reductionclaim; planner removesper-pixeldivisions ineligible1:1draws.
CurrentnormalDesktopcompile+privatelinkLIVE /tmp/cubit-row-copy-standard-build.log
undersharedlock; poll exacthandle fromthreadhistory. Include rowplannerhashes
inprivate linkmanifest. Nextmixed-output pixelregression onnewrowpath.
No stagedbinary/index/commit/push. Fullhardwaregoalnotcomplete.

2026-10-03 row nativebuild52417 TERM0 PASS.29232 TERM0 baselineextraction
readonlydebugfs, SHA8920683d160148cf14c89e27bfefd674e40c8cb7d711d8381c5426f41c81953f
matchesprioroverloadinputs. Savedtests/compositor/build/software-row-comparison/
baseline-desktop.svc. Optimized4worker rerunjustdispatchedundersharedlock,
/tmp/cubit-row-copy-overload.log, persistentTMPDIR. Pollnewexacthandlefrom
threadhistory; no resultclaimyet. No otherownlivejobs.

2026-10-03 retainedoverload19744 TERM0 PASS25updates,4workersfinish.
Persistent /tmp/cubit-overload-evidence/cubit-observatory-viewer-fpi_71g5 and
result/inputs/serial copiedtoovgfbqzp/overload-evidence. BaselineDesktopbeing
extractedreadonlyfromprivatebase.img becausemetricsbuildnowbeingupdated.
Rowcopy newcompositor_row_copy.ads/adb SPARK24checks0unproved and952952
pixelcomparisons withsamplerPASS(row-copy-9atq1pg2),96851TERM0. Unitratio
unrotatedopaqueonly, arbitraryclipped/negativeorigin, rejectothertransforms.
Publishedtest-row-copy.py equivalentprivatefixture; plannerwiredintoonly
Main drawClientBuffer opaque fallback underlock; preserveclienttraceandmapped
leaseboundary.52417LIVE native metricsbuild /tmp/cubit-row-copy-native-build.log
sharedlockheld. Neednativepixel/overload rerun andsourcehashcomparisongates.
No stagedbinary/index/commit. BroadGPUgoalstillactive.

2026-10-03 overload7458 TERM0 PASS25refreshes inclworkerfinalgate, butNix
removedtemporaryartifactdir at exit. FinalPASSlog persists, detailedresult/
serial/inputmanifest lost. Attemptedcopy failed; emptyoverload-evidence dir
created, noresult copied. Do notpretend retaineddetailedevidence exists.
Reproduction nowLIVE with TMPDIR=/tmp/cubit-overload-evidence (persistent,
shortsocketpaths), samefreshbinaries/workload, log /tmp/cubit-software-dpi-
overload-retained.log. Poll exactnewhandle fromthreadhistory. Thisrepeat is
justifiedbylost evidence, not processobservationtimeout. Sharedlockheld.
Allfuturetest-viewer runs must setpersistentTMPDIR outsideNix transientroot.

2026-10-03 fresh overload7458 confirmedLIVE, functionalinteractioncomplete.
Freshviewer/metrics rebuild removedearliergrantfailures:0invalidslots,25updates,
7pausedmarkers,viewerclosed while4workersactive; workersdone0 atlastpoll.
Wait exact7458 forworkers/finalinputhash gate; do notdeclarePASSyet.
Artifact /tmp/nix-shell-2328612-2299838910/cubit-observatory-viewer-lhm96pi7.
Counteraudit: native renderOutput3850 chargesallphysicalscene repaint pixels
asstatsRepairPixels; old directpath3901 chargesonlypre-drawrepair. Current
426132repair_px with425600clientarea isNOT evidence oldrepaircopyoptimization
regressed. Do not compare toold532perframe withoutseparatingrequiredscene
renderfromredundantrepair. Main3030 blitSurfaceNative fallback invokes
Compositor_Sampling.Map forEVERYPIXEL evenunit-scale,unrotated,opaqueclient.
Concrete nextoptimization: proved contiguous-row blit admission for matching
source/outputdensity, retain general samplerforothertransforms, preserve
noteClientDraw andsame mappedbuffer/lease boundaries. ExistingcopyRegion2160
hasprecomputedcolumns butclients do not. No productionedits thisturn; next
changes wait untilownoverloadlockreleased. Goalprogress: freshmetricsworks
andactualhotpath/countersemantic finding changesnextaction; hardwarepathpending.

2026-10-03 refresh31209 TERM0 PASS viewer/workers/metricsvc nativebuilds.
Freshretry7458 LIVE undersharedlock, /tmp/cubit-software-dpi-overload-fresh.log.
Private /tmp/cubit-fresh-metrics-viewer.py identicalinteractiondriver butuses
fresh userspace/services/metricsvc/build/metrics.svc instead ofstagedcollector.
Roottestscriptunchanged. --desktop currentbuild-metrics. No stagedbinaries
replaced. Poll7458 anddiagnoseoutcome; old4389 terminalfailure preserved.

2026-10-03 metrics overload4389 TERM1 beforeviewerinteraction.
/tmp/nix-shell-2326321-8366282/cubit-observatory-viewer-9a_0udrv: Observatory
nativewindowready thencollectionunavailable; repeatedmemorygrantcreationinvalid
globalslot. Fourworkersbegan, no successfulmetricsupdates; NOToverloadPASS.
PrebuiltOct1viewer/workers andstagedcollector usedwithcurrentkernel; staleABI
is hypothesisonly, notestablishedcause. Fresh viewer/load/metricsvc rebuild
nowLIVE /tmp/cubit-software-metrics-refresh.log undersharedlock; polltoolhandle.
No productionmetrics/kernel edits; will retrycurrentbinariesbeforediagnosis.

2026-10-03 software DPI metrics build17010 TERM0 PASS, notstaged.
Updateduserspace/services/desktop/build-metrics/desktop.svc via helperwithout
--stage. Existingprerequisites fromsharedtree. Observatory4worker testjust
startedunderbuildlock /tmp/cubit-software-dpi-overload.log; exacttoolhandle
inthreadhistory. ExistingObservatory/load binaries recordedasseeds, nofresh
wholeworldclaim. No sharedsourceedits. Functionaloverload, notquiet-host
benchmark; peernativebrowserwork mayoverlap. Next inspectcopy/damagecounters
andinputresync underthiscurrent per-output default. Goalactive.

2026-10-03 mixed-output native gate54329 TERM0 PASS180s.
Existingheadless mixedoutputs/arrangement/primary/scaling test,2outputs1024x768
and1280x720,TCG4CPU1GiB. Passed native primarytaskbarmigration/newclient/
maximizeworkarea;125/150%scale,workspacefloor,cursorrepair,primaryreflow,
mixedscaleseam;above/left/below/offset layouts andverticalpointerseam;
splitdrag/peroutputmaximize/wallpaperrestoration. FinalheadlessPASS, nofault.
Evidence copied software-dpi-native-ovgfbqzp/mixed-evidence inclmanifest,
logs andprimary125%/secondary150% screenshots. Primary125% inspected.
PrivateDesktop hash/sourcehash verified; harnessbuiltsharedkernel/setup and
usedexplicitprivatebaseDisk copy, no manualsource orstagedDesktopchanges.
No performanceclaims fromTCG or sharedhost. Allownjobs terminal, lockfree.
Next meaningfulwork: refresh metrics/timing and overload evidence for new
per-output default; physicalGPU targethandoff andlatency remain incomplete.

2026-10-03 mixed-output native gate LIVE54329 shared buildlock held.
Existing tests/headless/run.sh desktop-dual-output, mixedoutputs/arrangement/
primary/scaling=1,TCG4CPU1GiB180s. CUBIT_DESKTOP_IMAGE points toprivatecurrent
software-dpi-native-ovgfbqzp; --disk is itsprivateunscaled desktop.img.
/tmp/cubit-software-mixed-dpi-{run,serial}.log. Harness currentlybuildingkernel
as partofnormalsetup (not --build world). No manualsharedsource/stagingedits.
Poll54329; no restart on observationtimeout. Goalprogressvia broader native
validation. Othernotesread, no activegraphicsjobatdispatch. Do not call this
performance benchmark: functional timings only and peerbrowserruns mayoverlap.

2026-10-03 NATIVE persistent raster failure recovery PASS.
42660 TERM0 privatefaultlink software-raster-fault-3ku3q71a: test-only linker
wrap cubit_font_raster_mask permitsonecall thenfailsforever; productionfont
library untouched.21526 TERM0 native-evidence125%Settings+16cursor moves/
4roundtrips+3menus exactrestoration PASS. Serialexactlyone internal shell
startup, one partialbatchreplay, one CPUtextfallback; no restarting. Validated
currentchangedsourcehashes andbinaryhash, saved fault-result.json. This closes
actualnative scene replay gate; not only a hosted facade result anymore.
Published tests/compositor/test-desktop-software-fault.py underlock addsfresh
compile/bind before private link, then existingnative runner and same recovery
assertions. Initial executed /tmp driver usedalreadyfresh currentobjects;
publishedcompletewrapper syntaxchecked, not yet independently rerun.
Allownjobs terminal, no lock/index/stagedbinary/commit/push. New fault fixture
only. Next workload: mixedoutput and timing/metrics comparison underload for
per-output software default. GPUsharedtargethandoff still outstanding.

2026-10-03 software DPI recovery CHECKPOINT PASS, all own jobs terminal.
78638 TERM0 software-text-43gc3w_m:164checks45flow119prover0unproved,
hosted cache/rotation/fault/retirement tests PASS.95470 previousfacade40checks
and two first/later raster failure processes PASS, software-facade-mxxlls6r.
Owner/facade result.json all input hashes match current root source.
79999 TERM0 native compile/bind/link software-dpi-native-ovgfbqzp.15451 TERM0
unscaled-evidence:32moves8roundtrips3menus exactrestoration.40028 TERM0
scaled-evidence:realSettings125%,16moves4roundtrips3menus exactrestoration.
Both use same recoverybinary, changed_sources all byteverified currentroot.
Kernel/initrd/display/clock/logstore recordedprebuiltseeds, not worldrebuild.
Tests ran concurrently: correctness only, absolutely not performance numbers.
Logicalsceneallocation0 and softwaredensitytext enabled; no physicalGPU/NUC
latency/tearing evidence. No stagedbinary/index/commit/push. Sharedlockfree.
Next important gates: native injected rasterfailure (actual scene replay),
mixedoutputs/layout and refreshed metrics/overload/performance comparison for
new per-output default. Existing hardwaretarget integration request outstanding.
Goalactive; this turn completes native verification, not overall objective.

2026-10-03 software DPI gates closed; recovery bug found and fixed.
47102 TERM0 wmehaus3 owner158checks0unproved +1000reuse/300eviction/rotation/
fault tests PASS exacthashes.58314 TERM0 du3hn8oq native125% PASS16moves4rounds
3menus, binaryfd624806...; currentsource later changed for failure recovery.
29671 TERM0 initial facade39checks/tests PASS, but Main replay audit found
newfacade failed to disable retainedtext before replay! Main3819 resets
textSceneRetry eachpass, so persistent late raster failure would restart.
Fixed via new owner Enabled/Disable transition (no waits/readers) and facade
Disable on glyph failure. Target-validation failures decline before raster;
emptybatch succeeds. Do not claim initial hostedtestproved actual recovery.
Published tests/compositor/test-software-facade.py now separate process first
and laterglyphfault cases, nextcall declines text without Repaint; targetguards
run beforedisable.95470 TERM0 software-facade-mxxlls6r PASS40proofchecks0unproved
andbothfaultcases.65834/40350 TERM1 intermediate facadeglobalrefinement rejected;
final latch lives inside owner, same abstraction as preexisting Mesa renderer.
78638 LIVE finalowner proof/test /tmp/cubit-software-text-disable.log.
Native recovery build just dispatched /tmp/cubit-software-dpi-recovery-build.log
under shared lock; poll exact handle from thread history. No Main/GPR/staged
binary/index changes. Need final recoverynative binary boot, unscaled and
mixed-output/load checks. Proofboundary still targetmapping/fontFFI trusted.

2026-10-03 latest handle update:93723 TERM0 native final compile/bind/link,
privatebinary software-dpi-native-du3hn8oq. No shared lock now held.
52712 TERM1 as expected: hosted tests PASS but source hash guard rejected
older snapshot after Quiescent API addition.47102 confirmedLIVE final source
owner tests; proof summary written in software-text-wmehaus3. Finalbinary
native boot just dispatched /tmp/cubit-software-dpi-boot-r3.log; poll toolhandle
from thread history. Earliernative125% screenshot qnbp1euu remains valid for
that recorded binary. No claim finalcurrentnative test finished yet.

2026-10-03 software DPI native integration PASS; final proof/build live.
Legacy now selects per-output scene and uses Compositor_Software_Text_Target.
Native75885 build and59074 link passed; qnbp1euu/scaled-evidence84194 PASS:
real Settings125%,16cursor moves4roundtrips,3menu restorations, zero private
scene allocation; retainedsoftwaretext marker. Screenshot inspected, crisp
fractional text. Secondbinarytz46x1qg78708 PASS same gates after invariant
addition; these tests precede final constant-time quiescence query correction.
New owner public Type_Invariant now Consistent, internal helpers use Consistent
to avoid recursive invariant obligations. Facade uses one-reader Quiescent
at Begin/Complete/Software_Text. Final facade proof20366 TERM0 ktb60mgr39checks
0unproved. Prior unused/proofonly global failures corrected, no waivers.
Ownera7vl6xnc157checks0unproved,52712 still test running at last poll; source
hash guard expected to reject because Quiescent was added after snapshot.
Finalcurrent published owner test47102 LIVE /tmp/cubit-software-text-final.log.
Finalnative compile/bind/link93723 LIVE /tmp/cubit-software-dpi-build-r3.log,
shared build lock held by job. Poll these exact handles; do not restart.
No Main/GPR/staged binary/index/commit changes. New target bridge SPARKoff
maps validated writable image; physical mapping authority/aliasing and font
FFI remain trusted, owner policy SPARK. Need finalnative rerun/current hashes
and facade fault tests before declaring this integration fully verified.

2026-10-03 live software DPI candidate: own backend-legacy/desktop_compositor.adb
and new compositor_software_text_target.ads/adb. Legacy Selected=True selects
existing per-output scene path; Draw_Text uses software-only glyph owner.
No Main/GPR edits. Narrow target mapping reuses existing bridge guards; partial
batch failure requests scene replay. Pending native build and boot validation;
not yet a native pass or production DPI quality claim. No staged binaries.

2026-10-03 software-only density glyph owner PASS (not yet wired to Desktop).
New compositor_software_text.ads/adb reuses existing glyph cache/storage/painter
with one synchronous read lease, 512KiB raster bound, no Mesa dependency.
41812 TERM0 private software-text-srt5jxu_: SPARK146 checks(42flow104prover),
0unproved/justified; hosted tests PASS1000 rational-equivalent reuse,300
multi-density eviction draws,4rotations clip guards,raster fault/retry,shutdown.
8375 duplicate binary execution TERM0 PASS; no active own jobs. Inputs hashes
verified against root, result.json saved. First66346 TERM1 only test visibility
clauses fixed. Published equivalent snapshot runner test-software-text.py under
shared lock with sourcehash/proofgate/result persistence. Font raster is a
synthetic fixture here; real font/storage mapping boundaries remain trusted.
Main/default backend untouched. Next: narrow software target bridge, wire
legacy text and per-output rendering, then native DPI and cursor regression.
No index/commit/push; goalactive. Previous status-only turn revalidated through
source inspection and new tested implementation; no whole-goal blocker.

2026-10-03 software DPI work: own new compositor_software_text.ads/adb.
CPU-only synchronous glyph owner reuses existing cache, raster storage and
software painter; no Mesa dependency. Draft pending build/proof, not live
Desktop activation. No shared Main/GPR/backend changes yet; no lock held.
Previous checklist turn was status only; this turn resumes implementation.

2026-10-03 native fractional DPI cursor regression PASS; publishedrerun live.
31309 TERM1 firstprivatefixture: threeUp selectedSameBoy, notSettings. Also
QEMUscanouthint1600x1000 didnotsetactiveprimarymode; nativehandoffreported
1024x768. No productionbugclaim from this failednavigation fixture.
4144 TERM0 corrected2Up andconfirmed1024output: Settings applied5/4 viareal
mouseclicks;16moves4roundtrips atlogical16/40positions. Exactcrop restoration,
no changesoutside outward-roundedold/newcursorfootprints. 3menucyclesPASS.
Evidencecursor-native-boot-6mtf7ow0/scaled-cursor-evidence; image
settings-scale-5-4.png inspected. This is softwarelegacy/QEMU, notGPUorNUC.
Root test-desktop-vulkan-boot.py --scaled-cursor-motion added underlock;
26467 TERM1 beforeQEMU: argparsehelp literal125% rejected; fixedhelptext.
51900 TERM0 /tmp/cubit-cursor-scaled-published-r2.log: publishedrunnerPASS
outputscaled-published/result.json, native5/4scale,16moves4roundtrips3menus.
Allownjobs terminal.
Readonlyfinding for nextcodework: Main nativeScene returnsDesktop_Compositor.
Selected (line317); legacySelected=False so125% screenshot followslogical
backbuffer scaling. Mesa-selected fallback alreadyhasphysical-output glyph
raster path. Investigate enabling density-aware software scene fallback without
requiringMesa selection; do not claim screenshotdemonstratesfinalcrispfonts.
No productionedits thisturn,noindex/staging/commit/push,nolockheld. Goalactive.

2026-10-03 native cursor-motion damage regression PUBLISHED and PASS.
14358 TERM0 privatecursor-motion-runner.py evidencecursor-native-boot-6mtf7ow0/
motion-evidence:32nativePS2moves/8roundtrips through(80,80),(144,80),(144,144),
(80,144),back; checked desktopcrop excludesliveclock/taskbar. Allcapturedsettled
frames havezerochangesoutsideold/new19x28arrowfootprints; exactrestoration
atstartaftereachround. Also3menuopenclosecyclesPASS. No Desktopstateinjection.
Sharedlocked root tests/compositor/test-desktop-vulkan-boot.py adds optional
--cursor-motion and truthfullegacy scope frommanifest.backend; defaulttest
behaviorunchanged.41794 TERM0 publishedrunner motion-published/result.json
PASS same32moves8rounds3menucycles,115274menu-changedpixels each.
Binary remainsf146cf4... frompreviousfreshMainlink;kernel/initrd/servicesrecorded
seeds. NativeCuBit/QEMU4CPU TCG1GiB,1xscale only, notNUC,GPU,tearing,latency
oroverloadmeasurement. Screenshotssettledwithin8s pollingdeadline, not every
scanoutframe; no claimtransientartifactabsence. Allownjobs terminal,no lock,
noindex/staging/commit/push. Goalactive; targetproviderhandoff stillpending.
Next independent native gate: scaled cursor/Settings interaction usingexisting
check-idle-dpi.py coordinates and recorded currentseed/privateDesktop.

2026-10-03 cursor atlas proofs CLOSED; modular lookup PUBLISHED; native boot PASS.
21062 TERM0 original staging4efppuu8 proof60checks0unproved +8064000singleicon
and17354892atlaspixel/guardsPASS. Ownerftdd93nq proof637checks0unproved;
96/81chunks,1000reuse/noqueue,retirement/quarantinePASS.
11666 TERM0 modular8p4q6cc1 childproof41checks(8flow33prover)0unproved,
17354892pixelguardsPASS. Reviewed/applied ONLY childatlasads/adb: private
Cursor_Value boundedlookup avoids carrying rawpixelaggregate intoouterloop.
All15candidateAda source/testinputs byteverifiedagainstroot; evidence
cursor-modular-8p4q6cc1/published-inputs.json. No assumptions/proofwaivers.
80580 TERM0 realVulkanafterpublish icon-atlas-real-4n6b0p6n PASS384frames2359296pixels,
2uploads,heldtargets/cleanup; affine233472pixels0errors. Native archive
1z3g7d93 PASS92objects SHA 05ff4cf8944e6730a4f67f925fb485775985f7c3bf90c37a9003e589241899fa.
46155 TERM0 linkedlegacyMain toprivatecursor-native-boot-6mtf7ow0 executable
with gprbuild -l -o underlock. Staged Desktop untouched.34560 TERM0 QEMU4CPU
TCG1GiB boot-evidence/result.jsonPASS3menuopenclosecycles,115274changedpixels
peropen,exactrestoredregioneachclose. FreshMain binary f146cf4ce29b828cb11ce0700eb005bd5034db121f7693ff368394e0633e6e92.
Kernel/initrd/display/clock/logstore are recorded prebuiltseeds; notwholeworld
rebuild. Runnerprivateadaptation labelslegacytruthfully. Cursor remainsat80,80;
this tests boot/menu/regression, NOTpointermotion,scalechange orGPUactivation.
Screenshotboot-evidence/menu-0.png inspected. Allownjobs terminal,no lockheld,
noindex/staging/commit/push. Previousverifiedwait+candidateprogress; current
publishedrefactorandnativeexecutionprogress. Targetproviderrequestpending.

2026-10-03 atlas proof cost investigation; private modular candidate running.
21062 confirmedLIVE original staging4efppuu8; escalated process snapshot shows
GNATwhy3 Pixel entity CPU~100%,7-9minelapsed; RSS varied~2.1GiB to~300MiB.
Active CVC5/Z3 children. Not stalled or missing; neverrestartonobservationtimeout.
Candidate cursor-modular-8p4q6cc1 adds private Cursor_Value function withbounded
coordinates, reads originaltable there; outer Pixel loops call helper instead
of referencing2161pixelaggregate. Rootproduction unchanged.11666 LIVE
/tmp/cubit-cursor-proof-modular-r1.log; helperproof activeCPU~100%,~138MiB
at observed checkpoint. No successclaim until finalGNATprove report/testexit.
Intermediate Why3files show143/143 old and99/99candidate nodes proved, NOT
finalcheckcounts or completion evidence. Bothjobs retained; no kill/restart.
Next: pollboth; inspectcandidate finalreport; ifclean, applyONLY childatlas
ads/adb diff and revalidate sourcehashes/owner/real/native as needed.
Candidate copied staging4efppuu8 and hasnootherrootsourceedits. No index/
staging/commit/push. No lockheld. Currentprogress is diagnosedproofcost and
privateimplementationtrial; fullgoalactive and targetproviderrequestpending.

2026-10-03 proved cursor geometry integrated into actual Desktop Main.
New pure Compositor_Cursor.Build validates hotspot/dimensions and computes
complete signed logical bounds in wide arithmetic; rejects unrepresentable
edges rather than shifting/clipping the hotspot. Valid postcondition proves
allfour edges. Main cursorShape now feeds cursorOriginX/Y and nativeoutput
cursorSurface; existing pixel sampler/damage path unchanged. No GPUactivation.
50782 TERM0 cursor-geometry-lfvkndz8 proof8checks(1flow7prover)0unproved,
605independentwide-reference cases all5cursors/negativeorigins/extremes,
invalidhotspots +maxdimensionsPASS. Published test-cursor-geometry.py adds
hash/result recording; executed /tmp driver had same test/proof commands.
21316 TERM0 nativelegacy Desktop fullcompile+bind under sharedlock PASS
/tmp/cubit-cursor-main-native-r1.log. No final link/boot/staging orbinarypublish.
61176 TERM0 realcursorfixture GPUusesCompositor_Cursor butCPUreference keeps
independenthotspotarithmetic: icon-atlas-real-6v336edv PASS384frames2359296
pixels,2uploads,heldtargets/cleanup; affine233472pixelszeroerrors.
21062 remainsLIVE /tmp/cubit-cursor-atlas-r1.log stagingproof4efppuu8 (then
ownerproof); exacthandlepollconfirmed. Do notrestartorclaimproofpassyet.
No lockheld, index/staging/commit/push. Prior/currentturn concreteprogress.
Targetimport/scanout provider handoff remainspending; goalactive.

2026-10-03 retained cursor atlas integration, validation running.
Read current peer notes; no overlap. Extend second control atlas to25x161:
controls top45rows, cursors atY45/73/97/115/140, actualdimensions +transparent
padding. Applicationatlas24x192 unchanged. Total34532rawBGRAbytes, no third
source slot or perframeupload. Straightalpha controls and premultiplied cursor
regions share image; mode remains per draw. Cursor_Region preserves originals.
/tmp/cubit-cursor-atlas-r1.py PARTIAL applied only production atlas ads/adb,
then assertion failed beforetestwrite. NEVERrerun. /tmp/cubit-cursor-atlas-
finish-r1.py SUCCESS test/inventory updates; NEVERrerun. Native GPR lists
Desktop_Cursors. Owner tiny256byte staging now81controlchunks (was7),96app.
21062 LIVE /tmp/cubit-cursor-atlas-r1.log: stagingproof thenownerproof/scenarios.
38413 TERM0 realVulkan --cursors icon-atlas-real-gpjbzod_ +ordinaryicons
icon-atlas-real-093v3wpj bothPASS384frames2359296exactpixels,twoinitialuploads,
heldfront/pendingexclusion,cleanup; affine233472pixels0validationerrors each.
New --cursors tests5shapes/premultalpha/hotspots/partialoffoutputfourDPIrotations.
1611 TERM0 /tmp/cubit-cursor-native-r1.log nativearchive desktop-gpu-native-7ktmdnlx
PASS92objects/817inputs SHA b104e75258582101eb986b00389541b3333922a0c465936ceb134428184a5cd1.
21062 remainslive; pollhandlebeforererun.
No Mainactivation ordriverchanges. No index/staging/commit/push; no lockheld.
Previous audit changednextaction; current concreteatlasproduction +realpixel
validation progress. Goalactive; targetimport/scanoutinterface request remains.

2026-10-03 current-source integration audit and GPU/Display handoff request.
Previous turn progress (production completion gate +proof/realexecution).
Current audit changes next integration action: native Mesa_Service only lends
VkDevice/queue; Vulkan_Device_Storage allocates private render targets, no
exported image identity. native_gpu_presenter attaches completed linear BOs
through read-only forwarded CPU grants, not Vulkan import or scanout ownership.
No existing API can connect these private targets to Display without a copy
or an agreed driver-owned GPU image transport. Do not claim Main GPU activation
is only a function-call wiring task, or implement fake scanout from CPU grants.

REQUEST for GPU/Display owner acknowledgment in coordination notes:
Please identify/propose the supported provider interface for three renderable
presentation targets per output. Need authenticated backing identity/generation,
per-plane layout/format/extent, authorized renderer import and scanout rights,
optional CPU mapping/coherency, exact latch/prior-front retirement events,
and known-quiescent cancellation/close. Prefer driver-owned targets imported
into Mesa; renderer-owned exports are also possible if these lifetimes hold.
No IPC operation numbers allocated by compositor; no driver/Mesa changes here.
For client images, same-session handle integers and forwarded CPU grants are
not GPU import authority; need explicit cross-session image import contract.
Reference updated docs/compositor-shared-targets.md source audit section.

Independent compositor work can continue on complete typed primitive capture
and whole-frame fallback. Existing software/Mesa path remains functional.
Full goal active; this is an interface dependency, not a whole-goal blocked
claim. No jobs live, no lock held, no index/staging/commit/push.

2026-10-03 whole-output completion adapter added, validation in progress.
New Desktop_GPU_Scene.Complete_Output + Software_Ready: Output_Complete /
Output_Repaint postcondition requires sceneIdle + D.Can_Retire_Readers.
Unsupported capture can discard complete scene; cold uploads remain pending;
repeated finish/cancel while submitted/uploading does not submit or cancel.
Closed/quarantined and premature capture polling return Output_Unsafe.
This prevents treating any low-level Rejected outcome as permission for CPU
fallback while previous work is still live. Main integration not activated.
New tests/compositor/test-gpu-output.py +desktop_gpu_output_tests.adb:
coldglyphs,100repeatedfinish/cancel,100pendingpolls,glyphreuse,overflow,
uncertaincompletionretain.80393 TERM0 /tmp/cubit-gpu-output-r1.log
snapshot gpu-output-pkz2jjqs:669checks234flow435prover0unproved/justified;
bothnormal+uncertaincompletion scenariosPASS. Allownjobs terminal.
3412 TERM0 real icon fixture now uses Complete_Output:384frames2359296
exactpixels,2uploads,heldfront/pending exclusions,cleanup; affine233472pixels
zeroVulkanerrors. Native desktop-gpu-native-2e9ii7vu PASS816inputs91objects
SHA a6d3bc37e6b0025a923d1c06cd52a6c3aa1c867b43590b6281a1238e0e82f206.
Compilation/elaboration archive only. New entrypoint not yet wired to Desktop
Compositor facade/Main, which still needs GPU target/source integration.
No shared build outputs, index/staging/commit/push; no lock held. Goalactive.
Previous turn progress; current production safety gate +realtest progress.

2026-10-03 capacity proof chain completed; real icon scene validation active.
33450 TERM0: source-partition-ma0mi1ab startup/context/image proof959checks
0unproved/justified; full140sources, strided, submission1000lifecycles/10000polls
PASS. Atlas owner xbmtr5zo proof637checks0unproved +96/7chunks,1000cachehits,
retirement/quarantinePASS. Documentation updated; published wrapper not executed.
New tests/compositor/test-icon-atlas-real.py adapts retained real scene fixture:
two icon atlas owners in130..131, direct uploads, all13 icons selected over384
frames, fourDPIscales/rotations/threeclips, straightalpha overopaque black.
Independent CPU reference samples original per-icon pixels, not packed windows.
58260 TERM1 fixture Physical_Extent conversion missing;33576 TERM1 importgate
expected reader on emptyclip;74800 TERM1 fixture Pixel_Edge useclause missing.
56527 TERM0 /tmp/cubit-icon-atlas-real-r4.log; icon-atlas-real-206ge3is
PASS384frames2359296exactpixels,twoinitialuploads,twoimageallocations,
heldfront/pendingexclusion,fullcleanup; affine233472pixels0validationerrors.
Allownjobs terminal. This closes hosted atlas upload-to-scene pixel validation,
not native execution or hardware performance.
No production source changes in this turn; root test/doc/note only. Main audit:
renderOutput -> drawCurrentScene -> drawNativeIcon still calls CPU-pointer
Desktop_Compositor.Draw_Output; new GPU scene capture remains separate.
No index/staging/commit/push; no lock held. Previous/currentturn progress.

2026-10-03 source partition integration in progress.
Read peer notes; no overlap. /tmp/cubit-source-partition-r1.py SUCCESS under
build lock; NEVER rerun. Slots now 0..127 glyphs,128..129 backdrops,
130..131 icon atlases,132..139 clients. Owners restrict their asset slots.
140 source descriptors/backings,144 allocation ledger,143 context children;
configured pixel-byte budget unchanged. Reader arrays derive source capacity.
33450 LIVE /tmp/cubit-source-partition-r1.log: full140-source capacity passes;
SPARK startup/context/image proof running, then strided/submission and atlas owner.
79930 TERM0 C metadata all140slots + checker lifecycle PASS; native archive
 desktop-gpu-native-dldsa6k5,816inputs91objects SHA
 6cf01796a1de8d7ce6ab00358cac6848922d4f5d2f8bae83af4a55ee62f5a8ca.
Compile/elaboration/archive only; no live GPU Desktop activation.
Published tests/compositor/test-source-partition.py for reproducibility,
same commands as current /tmp driver plus input hash validation/result manifest.
Current run uses /tmp driver, so do not attribute its execution to published wrapper.
Prior atlas jobs completed: icon-staging-ojbkoc04 PASS8064000 original +
11651868 packed pixels/guards, atlas mapping; icon-atlas-owner-10pr_b8x
637 proofchecks0unproved,96+7chunks,1000 cachehits,uncertainretentionPASS.
Native atlas closure desktop-gpu-native-0lb5g0t4 PASS91objects.
No staging/index/commit/push. Full goal active; previous turn status-only,
current turn concrete production changes and validation. No lock held.

2026-10-03 retained region SCENE and Desktop drawing VALIDATED.
/tmp/cubit-scene-regions-integrate.py SUCCESS underlock NEVERrerun. All6direct
Scene GPR inventories updated (native already hadregionunits); hosted mocks
linkregion_binding_mock C nowplaincompositorheader/noVulkanheaderdependency.
Scene Region_Textured/Straight_Region kinds, private512*24byteRegion_Table
(12KiBboundedmetadata), Append_Region only; rawAppend rejectsregionkinds.
Allappend/seal contracts preservepreviousRegion_At; gradientloop preserved.
Sources_Ready includesregiontickets; Replay callsbounded Submission.Regions.
Drawing.Image_Region validatesbeforeemptyelision, clips/pinswholeimage then
Append_Region. <=2layers/call proved. No perregionimage lease/newsource slot.
80521 TERM0 scene/binding/replay proof778checks0unproved/justified; fixture
region-binding-cxuqj9x5 tests metadata retention/stale-sourcepreflight/directkind
rejection/512layeroverflowPASS.31378 TERM0 drawingtests+proof46checks0unproved;
100regiondraws share1reader, pendingdiscard/closeholds, overflowcleanupPASS.
92001 TERM1 realfixture referencecomputedbeforeActive_Output initialized;
62771/90528 TERM1 privateCtrace: GPUregion137,29 correctrawtexel ff536497,
CPUreference wrong. Fixedrunnerreference derivesgeometryindependently.
87064 TERM0 region-scene-real-cu0kvdg_: realDesktopcapture/SceneReplay/Vulkan
384frames2359296exactfullscenepixels inclRGBAregionsfrom2retainedimages,
heldfront/pendingtargetgates,216uploadchunks,finalcleanupPASS; affine233472
pixels0Vulkanerrors. Native desktop-gpu-native-p7llwtm6 812inputs89objectsSHA
 ecaf6932e437553cec28d69131c0e9c6614a02f4435dfab76d2ad397fd8b4b3d.
No nativeGPUboot/DesktopMainactivation. Allownjobs terminal, no lock held,
no index/staging/commit/push. Previous/currentprogress goalactive.
Next atlaspacking/upload/residency for8x24px and5x9pxembeddedicons (2images,
20052rawpixelbytes). Plan source slot partition explicitly: current8general
slots also consumedbywallpaper, do not silentlystarveclientimages. ThenMain
capturehookswholeframefallback/nativeexecution. StrengthenAppend_Region
postcondition exactSurface/Over/Kind asneeded (currentlysource+region+kindset).

2026-10-03 SPARK source-region binding and submission VALIDATED.
New Compositor_Source_Region 24byteCgeometry w/explicit6wordlayout and bounds;
Vulkan_Region_FFI narrowoffbody callsproductionrecordregion; Affine_Binding.
Regions validateswindow/blendbeforeemptyelision, usesprovedA.Plan/Transform.
Submission.Regions usesexistingAdmit_Draw,Source_Valid,targetextent,Matches;
no newsourceowner/lease. KeepsSameSources,wholeframerejection,boundedDraws.
6111 TERM0 binding567checks0unproved +92416geometry/exactABIfaultcasesPASS.
82440 TERM1 submission test wronglyexpected End_Scene false onincompleteframe;
fixedtest asserts cleanpassclose +frameSTILLincomplete.94348 TERM0 final
region-binding-g3167mrd proof576checks182flow394prover0unproved/justified;
10000pendingpolls retainwholeatlas,4096budget,source/context/targetfailures,
failedcancelquarantinePASS. Log /tmp/cubit-region-submission-r2.log.
87308 TERM0 region-binding-real-lgr4g354 productionSPARK->realVulkan:233472pixel
oracle inclRGBAwindow andunchangedmaskpath,6scales4rot3modes3origins0errors.
Nativeinventory added7Adaunits; native desktop-gpu-native-wunwljig812inputs89obj
SHA1513b27d06faf6b780c08009beedc691781a6af4f344316d44f20bd699ddfa7e.
No finalservice link/boot orsceneintegration. Allownjobs terminal; no lock held;
no staging/index/commit/push. Previous/current concreteprogress goalactive.
Next Scene regioncapture+replay and Desktop CPUsourcepins, boundedatlas packing/
residency and Main drawingintegration. Avoidbreakingall positionalLayeraggregates:
considerprivateparallelregiontable +Append_Region guardedkind instead ofadding
publicLayerfield; account512*24byte boundedstorage. DirectAppendmustreject
uninitializedregionkind. NeedSceneSources_Ready andDesktopPin_Image recognize
newtexturedkind. No GPUactivation until completeframe/fallback path ready.

2026-10-03 icon staging VERIFIED; Vulkan source regions implemented/tested.
37266 TERM0 final writer22checks3flow19prover,0unproved/justified;
icon-staging-g4lq95gr result.json +8064000pixels+mappingguardsPASS.
80948 TERM0 initialicon native archive0bf647q5 (superseded afterregion edits).
Root C/header/fragment/backdrop coherent96bytepush+region at80; existingABI
entry useswholetexture, new cubit_vulkan_record_affine_region validates24byte
rectangle request then same draw path. /tmp/cubit-affine-region-edit.py SUCCESS
NEVERrerun. Caller must match descriptor dimensions/lifetime; no ownership or
SPARK policy added yet. Shader clamps local region beforeatlasoffset.
91761 TERM0 affine-regions-pmxfevi9: whole+fixed7x9window at2,3 each233472pixels,
6scales4rot3modes3origins,0Vulkanerrors. New test-affine-regions.py includes
new affine_region_boundary_test.c:50634 TERM0,11invalidregions0commands,
exact96bytepush/fullimagepositive controls. No new pipeline/descriptors.
6043 TERM0 checker-device-pixels-lvz_s33v alltarget/upload/affinechecksPASS;
finalnative desktop-gpu-native-8xadj8bj805inputs85objects SHA
69034e1c63f2d0483c401b6cd222aacf44b33dfd7285b7183023e65485779a51.
92558 TERM134 backdropboundaryold68bytefixture caughtlayoutchange; updated
payload24words+assertallreserved/regionzero.44995 TERM0 normal+forcedivide
backdrop each161280pixels andaffine233472pixels0errors. Sharedlockreleased.
All own jobs terminal. Native compile/archive only, notDesktopGPUboot.
Next SPARK region request validation/FFI/submission/scene source windows,
region edge/dimension oracles, bounded13icon atlas residency andMaincapture.
Eight general slots remainunchanged; no perframeiconupload. No index/staging/
commit/push. Previous/current concretePROGRESS; fullgoalactive.

2026-10-03 icon proof correction PROGRESS; full goal active.
71984 TERM1 authoritative first writer proof:23checks5unproved (32bit index
arithmetic/array indexing and unconstrained Length conversion). Pixels passed.
Root fixed Desktop_Icon_Pixels: precondition bounds Last instead of converting
potentially oversized Length; complete destination span checked in64bit BEFORE
any store; individual destination index uses64bit arithmetic. Pixel now regular
function body to reduce implicit expression expansion. No weakened contract.
59726 and64561 superseded proofs intentionally terminated (both TERM1 verified),
old snapshots preserved. Only current proof37266 remains LIVE, snapshot
icon-staging-g4lq95gr, log /tmp/cubit-icon-staging-r4.log. Poll exacthandle.
Upload adapter13316 previousPASS remains policy evidence; writer changed since.
Next finish writer proof and native compile, then atlas subregion sampling so
13icons share bounded source slots. Shader currently clamps whole texture;
merely shifting whole-atlas logical geometry would leak adjacent cells at
fractional DPI, so needs explicit source rectangle clamping. No implementation
yet, no claim of resident icons/GPU Main. No shared binaries/index changes.

2026-10-03 icon upload work IN PROGRESS, goal remains active.
New Desktop_Icon_Pixels typed SPARK writer: existing straight-alpha assets,
row slices/offsets/padding, rejects wrong dimensions/format/capacity. New
Desktop_Icon_Mapping narrow SPARK-Off writable address alias; synchronous,
no retained pointer/temporary pixel array. New Desktop_Icon_Upload uses
Begin_Write -> mapped copy -> Submit_Write, cancels invalid plan safely.
No Main/native project activation, no source slot expansion, no asset edits.
31440 TERM0 hosted pixel+mapping executables PASS 8064000 independent pixel/
guard comparisons for all13icons plus invalid/null/misaligned/short requests.
68056 TERM1 initial test missing enum use clause; corrected.
71984 LIVE test-icon-staging.py proof snapshot icon-staging-rpl2_rj5 (typed
writer only).59726 LIVE final writer+mapping snapshot icon-staging-x2tr71ss.
13316 TERM0 upload fixture icon-upload-f75sj2g_ PASS: proof595checks0unproved/
justified (upload adapter/dependencies, not typed pixel writer); provider mocks
24chunks/240pending polls, wrongshape cancellation, uncertain retention PASS.
71984 and59726 exact handles remain live;13316 terminal confirmed. Logs /tmp/cubit-icon-staging-
r2.log, r3.log and /tmp/cubit-icon-upload-r1.log. Do not restart on timeout.
SPARK result not yet known: constant atlas expansions yield25/37MiB proof
intermediates, no diagnostics so far. If expensive, make Pixel a conventional
function body to avoid expression-body expansion into caller; no weakened
indexing contracts. Final proof/native archive still needed before completion.
Source table has slots0..135, general128..135 (8) vs13embeddedicons. Residency
must use packing/reuse or explicit bounded slot policy, not blind per-icon
allocation. Staging here reserves no source slots. Next residency then Main.
No lock held; preserve all mixed index/staging. Current turn concrete progress.

2026-10-03 icon staging claim: new Desktop_Icon_Pixels SPARK typed staging
writer over existing immutable application/window icon tables; source assets
unchanged. New isolated pixel/proof fixture next. No Main activation or shared
native build/staging changes. Previous turn verified progress.

2026-10-03 bounded checker SCENE integration VALIDATED.
Drawing.Shadow emits <=3 layers, zero new source readers; Checker_Grid replay
uses existing bounded checker submission and device pipeline. Strengthened
Set_Clip count/reader contract and Shadow loop invariants (no weakened bounds).
4257 TERM0 final owner/drawing/scene proof:248checks68flow180prover,0unproved/
justified; Shadow14proved, Set_Clip2. Report checker-scene-proof-r3/gnatprove.
Earlier39717 TERM0 had reader-preservation unproved; superseded by final report.
28908 TERM0 hosted drawing tests PASS, including partial-capture failure and
pending lifetime; /tmp/cubit-checker-scene-tests-r3.log.
95514 TERM0 checker-submission-46teujhz proof587checks0unproved + ABI/fault/
20000heldpollsPASS; shadow-scene-real-iiuinz1_ realfonts+shadow49152exactpixels,
affine233472pixels,0Vulkanerrors. Prior79359 confirmedTERM0 too.
90454 TERM1 native invoked outside alr: exception mechanism mismatch. Correct
92414 TERM0 kernel/alr native archive desktop-gpu-native-nn3tfo3r 796inputs80obj,
SHAacafa5fba956eabcc3b843faf5f081098dbe63564498388bfdf8bab5ac3e2ed9.
Native compilation/elaboration/symbol closure only, no finalservice link/boot.
All own jobs terminal; no lock held, index/staging/commits/push untouched.
Checkpoint checker-scene-validated-r3/validation.json. Previous status-only
turn no progress; current turn concrete fixes and final verified evidence.
Goal remains active. Next Desktop Main scene capture integration: remaining
per-pixel icons/masks need bounded image/mask commands; wire complete draw
capture with whole-frame fallback, then native scene execution. Shared client/
scanout buffers and hardware latency remain outstanding. Do not claimGPU Main.

2026-10-03 checker scene integration claim: Vulkan_Scene Checker_Grid kind,
Replay via Vulkan_Submission.Checkers; Drawing.Shadow atmost3layers (oneclip,
twologicalcheckerstrips), no source readers; retain whole-frame failure. Update
all direct scene project inventories and mock checker provider coherently under
sharedlock. Existing base real Vulkan scene tests now link device-storage C
provider (no initialization implied); unsupported checker records reject.
No Main activation, protocol changes or staged binaries.

2026-10-03 checker pipeline group INTEGRATED/VALIDATED8ownedfiles.
/tmp/cubit-integrate-checker-group-r1.py SUCCESS underlock NEVERrerun; manifest
 tests/compositor/build/checker-group-edit-r1.json all8afterhashes reverified.
Actual device_storage nowchecker+affine+provider oneexistingSPARKchild; create
failurecleanups, providerclosefailure retainsboth, recordauth exactsubmission/
device/livegroup/targetextent. Shadergen emitscheckerheader; nativebuilder copies
shader/C; target_bundle/nativeGPR includechild/newC; contextsnapshot copiesfrag;
metadatafixture updatedstubs. ExistingaffineABI unchanged/noMainactivation.
95801 TERM0 actualtargetbundle VulkanallregressionsPASS zeroerrors.
97802 TERM0 nativearchive desktop-gpu-native-_xj_tb9x 793inputs79objects SHA
10f195997574e716095af96a25f717629220b0f8a89191a5a886cb933fa96222.
No nativefinalservice link/boot. nm confirmscheckerFFI resolvedbydeviceStorage.
48969 TERM1 fixtureflattenedheader paths compilefail;46126 TERM0 corrected
newdevicegroupfaulttests6freshprocessscenarios+existingmetadataPASS. Source
lifetime failurecases retainedbeforecleanup; fixtureclose retryNOTproduction.
87802 TERM0 checker-device-pixels-pjnfht9r: realCdeviceadapter+ownedtargets2304
exactpixels/wrongcontext+extent rejects, normalVulkanregressionszeroerrors.
Summary tests/compositor/build/checker-group-validated-r1; noownlivejobs,
no index/stagedbinary/commit/push. Previous/currentPROGRESS goalactive.
Next Vulkan_Scene Checker kind and SPARK Shadow capture usingtwo strips;
wire Checkers.Replay damagecommands (max8perlayer), rejectpartialcapture,
then actualscene-driven GPU pixels/heldreader/nativearchive tests. Need update
mockGPR inventory/stub fornewScene dependency and publicnative/headerinventory
cohesively underlock before Mainwork. Desktop fullGPU+sharedscanout+HWlatency
stilloutstanding, don'tclaimcomplete.

2026-10-03 checker submission child COMPLETE; resourcegroup wiring PENDING.
New vulkan_submission-checkers.ads/.adb Draw_Output/Replay admitsboundedattempts,
checksoutputdims, physicalclips, no sourceownershipmutation, rejectswholeframe.
Vulkan_Checker_FFI 64byteCrequest newimport cubit_vulkan_device_checker_record;
realCprovider notimplementedyet, currentlymockonly; unreferencedbyproduction.
40294 TERM1 testmissingnegativecoordinateuseclause fixed.47636/96023/90863 TERM0
proof587checks incldependencies,0unproved/justified; newchild25proved7draw18replay.
Final hosted tests exactABI signed16fields,4096exhaustion,statusfaults,cancel
quarantine,20000heldpolls with/withoutimagepins,8damage replay/mid-prefixfail.
Finalevidence tests/compositor/build/checker-submission-_do9pm1r/result.json;
fullsharedsourceinventoryhashes rechecked. 7417 TERM0 nativeAdacompile in
checker-submission-9cawjl_p/native.gpr, production4hashes unchanged, no link/boot.
No ownlivejobs/no staging/index/commit/push. Next cohesivepipelineintegration:
- vulkan_device_storage.c ownschecker alongsideaffine; checkercreatefailure
  destroysaffine; sourcesinitfailure destroysboth; sourcesdestroyfailure retains
  both; closeonlyafterexistingSPARKPipelinechildCanDestroy gate.
- C checkerrecord authenticatesexact&context.submission/livepipeline/targetdims,
  callschecker_record withownedcommand. No authority inferredfromCPUbuffer.
- shadergenerator build-vulkan-affine-shaders.py needscheckerheader; native
  builder c_names/shadercopy +nativeGPR sourceinventory; hosted target_bundle.gpr
  (onlybaseGPR withdevice_storage.c), metadata Cmock test needsnewstubs.
Need sharedlock whileeditingbuildscripts/GPR; don't breakcurrentbuilds with
partialCpublication. Then scenechild capture/replay/newboundedshadowcommands,
realGPU pixel+lifetime tests. Previous/currentPROGRESS fullgoalactive.

2026-10-03 checker SPARK submission claim: new vulkan_checker_ffi.ads/.adb
and Vulkan_Submission.Checkers child, bounded Draw_Output/Replay matching
existing submission admission/cancellation policy. New private-output test
runner with FFI status injection. Device-storage grouped lifecycle candidate
must wait for build/source inventory updates before publishing; existing shared
native build paths must stay linkable. No source-table ownership changes.

2026-10-03 descriptor-free checker Vulkan primitive implemented/validated.
New userspace/lib/compositor/vulkan_checker.c/.h/.frag: fixed separate pipeline,
existing fullscreenvertexshader, 64byte push/request ABI, one rectangle draw,
no descriptors/images/pixel allocations/upload/waits. Existing affineABI intact.
81551 TERM0 initialGPUoracle;56211 TERM0 finalfault+GPUoracle,6144frames/
4,718,592 exactpixels (256496painted/4462096preserved),256scales*4rot*3origins*
2clips,zeroVulkanvalidationerrors. 40badrequests noCmdcalls,11missingdispatch,
5creationfailures incl partialpipelinehandle allresourcesreleased. Snapshot
 tests/compositor/build/vulkan-checker-35w7wx9n; script rechecks source hashes.
73194 TERM0 native muslC compile, objSHA3d56707dfc68a57e5d6e9839f393016e806e2b3f7b12e4961cf889b1b8226c11,
record/nativeobjinpassingwork. No native link/boot/hardware execution claim.
Next SPARK binding/resourceowner and submission accounting/replay must own
pipeline until all referring commands/readers retire. Context activepass/target
dimensions/handleprovenance remaintrusted caller obligations. Do not wire Main
beforewhole-frame sources/cursor/interfaces ready. No liveownjobs, no staged
binaries/index/commit/push. Previous/currentPROGRESS; fullgoalactive.

2026-10-03 Vulkan checker claim: new vulkan_checker.c/.h/.frag, isolated
shader generator and hosted Vulkan oracle. Descriptor-free compatible-pass
pipeline with one rectangle draw, fixed resources, no pixel texture/allocation.
Preserve existing affine3pipeline ABI. Runtime/layout/truth boundary audited C;
SPARK Compositor_Shadow remains geometry authority. No Main or driver changes.

2026-10-03 bounded shadow geometry COMPLETE: new compositor_shadow.ads/.adb
Pure SPARK two strips, constant-work coverage preserves UNION of outward
logical cells (fractional DPI); center-sampled checker is NOT equivalent.
62446 TERM0 initial proof44 runtime checks; strengthened exact Build postcond,
3591 TERM0 proof50checks8flow42prover,0unproved/justified. Both hosted pixel
oracles PASS18,874,368 comparisons extractedactualMainloop; final evidence
 tests/compositor/build/shadow-geometry-4po4zo1k (source snapshot/proof/tests).
New test-shadow-geometry.py rejects unproved/justified and changing inputs;
final guard additions checked against final report, no production logic change.
Coverage equivalence regression-tested; exact Build contract formally proved;
no shader/native/hardware claim. Next descriptor-free checker Vulkan primitive
and scene replay; existing affine pipeline has3 textured pipelines and static
sampler usage, so new checker needs its own shader/pipeline or explicit source
binding (do not issue old textured shader without descriptor). New shader can
use inverseintegercoverage32bit (pixel<=65535, scale<=16, origin<=2^24), bounded
rectangles and scissor. Need audit narrow C binding/prove Ada replay, actual
hosted Vulkan pixels and nativecompile before Mainactivation. No live jobs,
no staging/index/commit/push edits. Previous/currentPROGRESS; goal active.

2026-10-03 bounded shadow implementation claim: new Compositor_Shadow pure
SPARK geometry (two checker rectangles, constant-time pixel coverage), dedicated
hosted oracle/proof in tests/compositor. Must preserve outward per-cell coverage
at fractional DPI rather than substitute pixel-center checker sampling. New
files only initially; Vulkan shader and scene binding follow verified geometry.
No shared native outputs, staged binaries or foreign ABI edits in this step.

2026-10-03 full-traversal shadow capacity gate VERIFIED. 31351 TERM0
new test-desktop-shadow-capture.py extracts actual Main shadow/depth and routes
putPixel through real Logical_Fill/scene owner with hosted device mocks.
1920x1080 screen, single640x480shadow1675calls; limit512layers, rejectpixel257.
320x200775calls,800x6002095calls also reject;64x64=187calls374layers accepted.
Whole-frame rejection/zero readers/teardown0charge PASS. No GPUexecution or
native/performance claim. First58998 TERM1 fixture dimensions32x24assert;
fixed only private copied metadata mock to1920x1080, retained failed evidence.
PASS tests/compositor/build/shadow-capture-dtvvblcq inputs/probe.log, rootdocs
updated. No live own jobs, no source staging/index/commit/push changes.
Next implement bounded procedural shadow capture/replay with exact-pixel
validation, then wire Main complete drawing scopes incl icons/cursor/client
images. Raising scene capacity or per-pixel lowering is not an integration
solution. Previous/current PROGRESS; full goal active, no new blocker audit.

2026-10-03 next integration audit: actual Main drawCurrentScene still includes
per-pixel dappled shadows (window/menu) before GPU capture. Claim new
 tests/compositor/test-desktop-shadow-capture.py: extract production shadow
body into real retained GPU drawing adapter with hosted device boundary mocks;
measure exact layer exhaustion across representative window dimensions. No
Main/driver ABI changes, no shared native outputs. This tests capture feasibility,
not GPU execution; required before wiring whole-scene facade blindly.

2026-10-03 logical fill adapter COMPLETE: owned drawing.ads/.adb adds
Logical_Fill using shared DPI/rotation/origin geometry and explicit physical
damage. Existing mixed_tests expanded independent inverse-pixel oracle:
3072 configurations/2359296 pixels PASS, empty/reversed clips, no readers,
prior-clip independence and outside-capture rejection. 78181 build TERM0,
31537 SPARK TERM0 25checks/11flow/14prover, zero unproved/justified;
1012 hosted regressions TERM0 includes cold/pending/uncertain/overflow.
Evidence frozen tests/compositor/build/logical-fill-evidence-r1 with source
hashes/logs/proof. No Main activation or native/hardware claim. No live jobs
from this slice, no staging/index/commit/push edits. Next connect whole-frame
Desktop drawing scopes; retain GPU shared-target/client import/multi-output
and physical measurement gates. Previous/current PROGRESS, goal active.

2026-10-03 checkpoint verified: published native async lease shutdown PASS
(boot-result.json in tests/compositor/build/desktop-completion-wvq3dkys),
staging unchanged. Published hosted caller/routing/policy tests PASS. Prior
72216/69001/75936 are completed; no restart of these evidence directories.
Claim next owned slice: desktop_gpu_scene-drawing.ads/.adb and existing mixed
drawing tests: logical fill placement/physical damage clipping in SPARK.
Private-output hosted build/proof only; no Main activation, shared ABI or driver
changes. Previous goal evidence PROGRESS; full hardware goal remains active.

2026-10-03 ASYNC OUTPUT LEASE PUBLISHED13ownedfiles+backenddocs underlock.
/tmp/cubit-publish-async-output-lease-r1.py SUCCESS; NEVERrerun. Manifest
 tests/compositor/build/async-output-lease-published.json; all13hashes verified.
Rootmain nowcapSubmit lease requests; Compositor_Lease_Request policyandtests,
20modecaller +exactcompletionrouting tests, asyncnativeprobe/builderflag/runner,
adaptedbaseprobeanchors, proofboundary docs. No kernel/Displaychanges.
89699 TERM0 privatepackagedshutdownrunner nativePASS/stagingunchanged;
41355 TERM0 fulllegacyDesktop57b76c212f8c56dbbc75f9b179cba0c16f21d35b3570609ca99cbf32b18a8e94.
Finalleaseoracle strengthenedrejectactualuncertaincompletion/idexhaustionlogs;
3positive31negativePASS +retainedall3native traces accepted. Baselineoracle29
negative remains. Current72216 LIVE publishedrootcaller/routing/policytests;
log/tmp/cubit-async-output-lease-published-tests-r1.log.69001 LIVE nativebuilder
--async-lease shutdown underbuildlock; /tmp/cubit-async-output-lease-published-build-r1.log.
Afterterminalinspectuniqueprintedfixture/result.json then runpublishednative
runner underlock; noVMstartedbybuilder. No stagedbinary/index/commit/pushchanges.
Priorretirementpublication manifest ishistorical: newasyncpublication intentionally
supersedes somefilehashes. Goalactive; previous/currentPROGRESS.

2026-10-03 asynclease nativevalidation COMPLETE:43102 TERM0 partialr1PASS
normalDesktopobserver/stagingunchanged.10890 TERM0 correctedshutdownr2PASS
heldactuallease replies, delayedsources/outputs tozeroPScharge,loop+kernelstop;
stagingunchanged. Shutdownr1FAIL retained; correctedfixture excludesUIinput
requirement aftershutdown, notacompositorbehaviorchange.
55376 TERM0 expandedexactcaller20modes coversprior16partiallease/grantlayouts+
shutdownsuppression plusheld/rejected/invalid/duplicate async completions.
47371 TERM0 privateportablecaller20modes+exactrouting +oracles29+25negativePASS.
Packageinstrumenter reproduces scaled43a5ffde..., shutdown4b0cef44...,partial
bf43230e... mainSHA BYTEEXACT. Private package adds async_lease_fixture and
--async-lease builder option; updatesbaseretirementanchors forasyncmain,
runnerusesleasechecker+bothheldinputtrigger onlyforscaledasyncmode. No rootedits.
89699 LIVE privatepackagedshutdownrunner validation underbuildlock,
/tmp/cubit-async-lease-packaged-shutdown-r1/run.py; outerlog samefoldername.log.
41355 LIVE privatelegacyDesktop link; /tmp/cubit-async-output-lease-legacy-build-r1.log.
Needterminalchecks, finalproof-boundary/docreview, exactallowlistpublication and
rootregressions/nativebuildercheck. Validation-only source_retirement_fixture
and outputretirementpolicies copiedinpackage MUSTNOTpublish asnewchanges.
Previous/currentPROGRESS; fullgoalactive; no index/stagedpromotion.

2026-10-03 86380 TERM0 correctedshutdownr2 link5e6a91c0eebdd36c1e5af696ebd1a745c196068079d2981f695b24d816f83911.
Readytonativelyrunafter43102releases sharedlock; no shutdownr2 boot yet.
43102 re-polled LIVE; partialserial shows bothreallease completionsheld/replayed,
leaseconfirmationPASS andinternal-sessionrecoveryPASS; normalobserverstill
running. Waitsame43102, inspectterminalboot-result beforeclaimfullPASS.
No productioncandidate changes; allnewworkprivate. Goalactive/PROGRESS.

2026-10-03 shutdown68005 TERM1 retainedFAIL/stagingunchanged. Bothreallease
repliesheld; fixtureincorrectlyrequired inputdispatchafterrunningFalse. Actual
Drain_Events/Drain_Requests deliberately guardedbyrunning. Shutdown shoulddrain
completions, notrestartUI dispatch. Nativeobserverfailed andDesktopstopped;
exactVMcleanQMPquit, priorlockreleased. No productionchange justifiedbythis
fixtureerror; normaldrain inputprogress already proven byscaled4083PASS.
86380 LIVE corrected private shutdownr2 build underNix, sourcebasedonr1;
remove input-afterhold gating andcommitinput>0 onlyforthisshutdownscenario,
retain3/7leaseholdpolls/realsubmit/reply/boundedretry/revokeonce/zerocharge and
actualloop/processstop. No QMPinput needed. /tmp/cubit-async-output-lease-shutdown-r2;
log/tmp/cubit-async-output-lease-shutdown-build-r2.log. Runafterpartialterminal.
70030 TERM0 partialfixturecompiled (see build logSHA), current43102 LIVE under
sharedlock /tmp/cubit-async-output-lease-partial-r1/run.py, outerlog
/tmp/cubit-async-output-lease-partial-run-r1.log. Real output1slot2 allocatedbut
ungrantedabort, asyncleasereplies held3/7polls forboth includingdisabledoutput,
3failednonpublishedsubmissions/output, realcompletionbeforerevoke, zerostorage,
fixture-driven activateInternalSession recovery plus normalDesktopchecks.
Partial/shutdown explicitlydonotclaim inputaftershutdown/beforeinputregistration.
Normal scaledr1passes fullinputduringheldleases +DPI/arrangement preservation.
Private productioncandidateunchanged, no root/index/stagingedits. Goalactive;
previous/currentPROGRESS. Keepfailedfixtureevidence; neverreruninplace.

2026-10-03 asynclease native4083 TERM0 PASS all4interactiongroups/scaledrestore/
reopen. /tmp/cubit-async-output-lease-native-r1/boot-result.json PASS,
stagingunchanged. Actualrelease replies bothheld, externalmodifierpress/release
processed(2events) before replay; bothleasesretiredafterinput; noearlyrevoke or
duplicates. Threeinjected nonpublished submissions/output thenrealsubmission.
Originalrealframeheld/replayed with2latecompletions. Nativeobservercleanquit.
30765 TERM0 shutdownfixture2e260f0a4fcb84b4c2eef9f565bed33c47acc1de4f943afeef2b39cd52af796f.
Private /tmp/cubit-async-output-lease-shutdown-r1 variant requestsactualshutdown
withqueuedframe, holdsbothlease replies andinjectsinput viaexistingobserverQMP,
requireslatelease/inputevidence+zeroPScharge+loop/kernelprocessstop. Native run
juststarted underbuildlock; outerlog/tmp/cubit-async-output-lease-shutdown-run-r1.log.
No rootsource/index/stagedchanges. Needasyncpartialsetup/nativefailure plus
portablefixtures/publication afterthisshutdown. Policy/routing/callerchecksPASS;
fullgoalactive, previous/currentPROGRESS.

2026-10-03 PRIVATE asynclease nextvalidation:92774 TERM0 exactcollectorbranch
routingtests PASS twooutputs/all10invalidkernel-or-envelopefields,101unrelated
tokens,duplicate completion;otheroutputstayspending. ExtractionSHA in
/tmp/cubit-async-output-lease-r1/routing/inputs.json. Fullmainqueue/retiredwatermark
routing is native scope, notclaimed by extractedbranch test.
18237 TERM0 nativeleasefixturelink23b511bed475221a4cc3b77372be702d68a1292e5a714ec5855dd596d08693c2.
4083 LIVE native /tmp/cubit-async-output-lease-native-r1 underbuildlock;
outerlog/tmp/cubit-async-output-lease-native-run-r1.log. Fixturegenerated from
privateasyncmain usingdeliberatelyadapted publishedoutputprobe: oldsynclease
Observe_Lease diagnosticanchor removed (exactone), lateframecounter anchored
onlytoframebranch toavoidnewleasematchedbranchcollision. Adds Probe_Lease_Submit
first3false attempts/output withoutpublishing;thenrealcapSubmit successes
mustonce/output. Probe_Lease_Poll holdsactualmatching LR completion peroutput;
replaysafter3/7polls AND inputaftereachhold. SeparateQMPinjector waits bothreplies
held beforemodifierpress/release. Asserts norevokebeforecorrectlease replay,
allleasesretiredafterinput, thenoriginalscaledreopen+4interactiongroups.
Nativehelper/runners/sources private; no rootproduction/index/staging edits.
Pollsame4083; portableobserver cleanquitsitself onallpasses. Existing baseline
policy/callerproofs remain; shutdown/partialsetup need asyncspecific validation
before publication. Previous/currentPROGRESS; goalactive.

2026-10-03 PRIVATE asynchronous output lease candidate, no rootpublication.
/tmp/cubit-async-output-lease-r1: newpure Compositor_Lease_Request stages Ready/
Submitting/Pending/Released/Quarantined. Prepare consumes strictlynew shared
Desktop token; failedcapSubmit returnsReady retaininglease; confirmedmatching
reply alone releases. Kernel Process.IPC.submitResolvedEndpoint audit confirms
False meansnoenqueue; success reserves completion andpublishes atomically.
transport-audit.json pinskernel/runtime/Displayinputs. No kernel/Displayedits.
9163 TERM0 policyhosted10krejectedsubmissions+10kpendingadmission attempts and
completionfaultcases PASS; SPARK9checks4flow5prover, zero unproved/justified.
Private main now2lease requeststates; closeOutput capSubmits encodedrelease
onlyafterrenderer+presentationretirement. collectPresentations routeslease
completionsincluding disabledpartialoutputs usingexacttoken/kernelvalid/status/
label/length/flags/reserved/allzero words. Invalid/duplicate quarantines;
acceptedpending neverresubmits. No revoke/storage beforeLR.Released. Tokens
remainuntilglobalretired watermark; newdrainresetsperoutputrequeststate.
47564 TERM0 nativeMesaDesktop link9e03af0c6874a6fbb0ef609792544002f1a6b55b3b7b53318b0b9b97f31acb92.
11162 TERM0 exactprivate cleanup4modes:2000failedcapSubmits,10kheldcompletion
polls,noduplicates/nosyncleasecall,staggeredfree andretainedIDs; failed/wrongtoken/
duplicate completion retainstorage. ControlledFFI policycompletion injection,
NOT fullnativecollector validation. Caller extraction inputs in p/caller/inputs.
Next: actualcollectPresentations envelope/routingtests +nativeheldreallease
reply/inflightshutdown/partialsetup/reopen fixtures. Existing root outputprobe
instrumenter expects Observe_Lease(R,Confirmed), nowchangedinprivate main; adapt
privateprobe deliberately, don'tsilentlyskip assertions. ReopenGET_INFO/acquire
still synchronous, separatefollowup. Allownjobs terminal/noVM/sharedlock;
no rootproduction/index/stagingchanges. Previous/currentPROGRESS; goalactive.

2026-10-03 FINAL integrated output retirement:44502 TERM0 root24modecaller,
oracle3positive29negative, retirement16layouts/16kbusy/24kconfirmation andlayout
10cases PASS.78949 TERM0 publishednativebuilder produced
 tests/compositor/build/desktop-completion-jsfcgeuo/desktop.svc SHA
23d5fc9672210d7ef64ad8f1a57dc3bb67e47299fdda92e3005e83263b8d6ef0;
generatedmain1212c389... matches retainednativefixture exactly.73610 TERM0
publishedportable runner native shutdownPASS, headless/faultscanPASS,
stagingunchanged; loop exitedafteractualdelayedpresentation+grantretirement,
zeroPScharge andkernelprocessstop. Allownjobs terminal/noVM/sharedlock.
21publishedhashes reverified/scopeddiffcheckclean. NEVERrerun outputpublisher.
No stagedbinary/index/commit/push changes. Source integration complete forthis
retirementcheckpoint; fullGPU/latencygoal remains active.
Next audit finding: closeOutput still invokes synchronous callDisplay forlease
release aftermatchingpresentationretired. Newrenderer/grantBusy polling returns
toeventloop, butleasecontrolIPC latency isnot bounded bythiswork. Considerasync
lease request/completion admission beforeclaiming wholecleanupnonblocking;
reopen GET_INFO/ACQUIRE also existing synccontrol. LiveGPUfacade, authenticated
clientimports/sharedscanouttargets andhardwaremeasurements remain outstanding.
Previous/currentPROGRESS.

2026-10-03 OUTPUT RETIREMENT PUBLISHED21ownedfiles+backenddocs underlock.
/tmp/cubit-publish-output-retirement-r1.py SUCCESS; NEVERrerun. Manifest
 tests/compositor/build/output-retirement-published.json; all21hashes verified.
Rootmain/interface/backends nowtypedTargetRelease andpersistentdrain state;
newOutputRetirement/LayoutRestorepolicies; tests/GPRs/exactcaller/nativehelper/
portablebuild+runner/docs included. Validation-only copieddependencies excluded.
No stagedbinary/index/commit/push changes. Initialpackagingindentstrings issue
fixedASTserialization; instrumenter reproduces all3testedMainfiles BYTEEXACT.
Oracle3positive29negative +retainedscaled/shutdown/partialevidencePASS.
9695 TERM0 packagednative shutdownobserver/run PASS;13036 TERM0 finalprivate
24modecaller+oraclePASS. Sourcewrapperonly changedROOTfortest; rootintegration
nowenables trueportable run. Current44502 LIVE rootregressions,log
/tmp/cubit-output-retirement-published-regressions-r1.log.78949 LIVE truepublished
nativebuilder --output-retirement shutdown underbuildlock, log
/tmp/cubit-output-retirement-published-native-build-r1.log. NoVMstartedbybuilder;
afterterminalbuild inspect printed unique fixture/result.json then run published
run-output-retirement-fixture.py underlock. Existingmanualnativeevidencepassdoes
notmean thisnewbuildervalidated yet. Previous/currentPROGRESS; fullgoalactive.

2026-10-03 FINAL59578 TERM0 partial native r2 PASS; recovered Desktop group
passes taskbar/splitdrag/maximize/wallpaper/cursor/Settings. boot-result PASS,
stagingunchanged; observer cleanQMPquit andsharedlockreleased. Same cd88d39e...
binary, correctedscenario recoverymarker. Earlier r1FAIL retained. Actualfive
allocatedtargets (oneungranted) drainbeforebackBufferReady tozeroPScharge;
fixture invokesexistingactivation then exercises normalnativeinteraction.
15361 TERM0 packaged retirement/layout hostedprojects PASS. Privatepackage
includes portablelayout_restore.gpr/tests andoutput_retirement.gpr/tests/check,
24modeexactcaller runner/template, interface/testcaller updatesandpolicy/main.
Copiedruntime cubit.ads andDisplay geometry/layout files are VALIDATION-ONLY
package dependencies, MUSTNOTpublish these aschanges. candidate-files.json is
STALE afterthis packaging step; regenerate exactexplicitowned allowlist before
publication. Stillneedportable nativeinstrumentation/runner packaging, proof
boundary/evidencefinaldocs andguardedpublication/rootregressions. Existing
baselines.json root4filespreviouslymatched; recheckbeforepublication.
Allownjobs terminal/noVM/sharedlock. No rootproduction/index/stagedpromotion.
Native gatesnowinclude unitdrainr2, scaleddrainr6, shutdownr1, partialsetupr2.
Goalactive; previous/currentPROGRESS.

2026-10-03 partial native19934 TERM1: realpartialrollback zerocharge and
fresh activateInternalSession recovery PASS; normalnative Desktop groupPASS.
Runner thenrejects missing initial "desktop: internal shell active" marker,
which cannot occur because fixture intentionallyfails initialactivation.
Retained /tmp/cubit-output-partial-native-r1/boot-result.json FAIL, staging
unchanged; observer cleanQMPquit, noVM remains. No codefailure found inthisrun.
59578 LIVE fresh r2 underbuildlock, samecd88d39e... binary/sourcefixture; private
headless expectedmarker now OUTPUT-PARTIAL: PASS recovered internal session.
Allotherstartup/asyncrelease/cclready markers, nativeobserver andfaultscanremain.
/tmp/cubit-output-partial-native-r2/run.py; outerlog
/tmp/cubit-output-partial-native-run-r2.log. Pollsame59578; observerquitsVM itself.
Recovery is fixture-driven call toexisting activateInternalSession afterdrain,
NOT claimthatproduction automaticallyretries failedinitialshellactivation.
Privatepackage prepared policies/interfaces/callers/controller/tests/docs and
candidate-files.json, nativefixturepackaging/layouttestGPRstillneeded. Source
maincandidateunchanged; no rootpublication/index/stagingpromotion. Goalactive.

2026-10-03 native shutdown COMPLETE:86487 TERM0 fixturelink8a5a25a5...;
39905 TERM0 nativePASS, boot-result stagingunchanged. Fixture requestsrunning
False withrealqueuedframe, exercises actualshutdownbranch/loop; delaysactual
completion +renderer/grant evidence, verifieszeroPScharge/noreopen/nopending
source/output, loop exit andkernel reported Desktop processstop. Two late
completions. /tmp/cubit-output-shutdown-native-r1; nohardwareGPUfenceclaim.
67151 TERM0 partialnativefixturelinkcd88d39e805fee6318e35c60440501e4ad7a0b0f5d1da13f50507ae188325204.
Partialfixture /tmp/cubit-output-partial-native-r1 abortsoutput1slot2 afterreal
allocation/address butbeforegrantcreation; output0alreadyopen,output1slot1
attached. Actualcleanup startsbeforebackBufferReady with7rendererBusypolls and
3/7grantconfirmationholds, checksnoearlyfree, retainedCPtokens, zerochargecommit.
Thenfixture invokesactual activateInternalSession once tocheckfreshrecovery.
Native run juststarted underbuildlock, outerlog
/tmp/cubit-output-partial-native-run-r1.log; observerwraps normaldualdesktop
functionalchecks then cleanQMPquits. Allassertions preserve actualMG/Displaycalls.
Both fixtures testprivatecandidate, no rootproduction/index/stagedpromotion.
Previous/currentPROGRESS. Fullgoal active.

2026-10-03 FINAL40585 TERM0 native scaled output-retirement PASS.
/tmp/cubit-output-retirement-native-fixture-r6/boot-result.json: all4observer
groups pass, actualinput2/latecompletion2, stagingunchanged. Same r3Desktop
binary withindependent QMPinjector sentmodifierpress/release; restoredscaledlayout
and reopened checks PASS. ExactVM cleanQMPquit onlyafter allmarkers; noownVM or
sharedlock. Earlier r3/r4 failed evidence retained; r5 neverbooted. Rootcause of
r4 harness failure confirmed second QMPchannel fixesinputinjection, keepingall
assertions andbinaryunchanged. Doesnot establish hardwareGPUfences/timing.
51167 TERM0 fullnativelegacyDesktop link2953b213e2247d2cd252abee5ab5af56554867f33040075b42aeb997bdb9fecf.
30256 TERM0 portable24mode exactcaller runner PASS inPRIVATE package:
/tmp/cubit-output-retirement-r1/package/tests/compositor/test-output-retirement.py
and output_controller_fixture.inc; output-retirement.md documentsproofboundaries.
Allownjobs terminal. Root4baselinehashesmatch. No rootproduction/index/staged
promotion. Native shutdown/partialsetup/fault gates and finalpackaging remain
before publication. Current main /tmp/cubit-output-retirement-r1/main.adb stays
authoritative; neverrerun oldprepare-integration.py. Goalactive; PROGRESS.

2026-10-03 controller33122 TERM0 all24modes PASS exact extracted closeOutput/
pump/release routines. New /tmp/cubit-output-retirement-caller-r2 tests all16
partial lease/grant combinations (three allocations, possibly ungranted),
busy-before-backBufferReady, alternating shutdown/reopen, idempotence; unsafe
renderer, five malformed lease reply fields, rejected first revoke retain all
storage and identities. Controlled mocks, NOT native shutdown proof.
73516 r4 TERM1 retained FAIL/stagingunchanged: separate injector timed out on
QMP greeting because existing observer holds sole channel; no key sent. ExactVM
QMPclosed only after Desktop exit and observerfailure.65348 r5 TERM1 beforeboot:
private runner preparation assertion found4QMPassignment sites; noVM. Corrected
single desktop-dual-output block, bash-n PASS, fresh r6 directory.
40585 LIVE r6 native /tmp/cubit-output-retirement-native-fixture-r6/run.py under
sharedlock. Private headless.sh root anchored to repo, second QMPchannel solely
for external shift_r input; existing observer and all guestassertions unchanged.
Same r3binary da1b1918...; retained runner-inputs.json. Outerlog
/tmp/cubit-output-retirement-native-run-r6.log. Inspect injection.json plusfour
observergroups and drain/scaledreopenmarkers before exactVM cleanquit.
51167 LIVE private full legacyDesktop link /tmp/cubit-output-retirement-r1/
link-legacy.py; log/tmp/cubit-output-retirement-legacy-native-r1.log.
No production source/index/stagedpromotion. Previous/current PROGRESS.

2026-10-03 facade6932 TERM0: Mesa86checks47flow39prover, legacy25flow,
zero unproved/justified; migrated native_output_test compiles. Private caller
changes remain /tmp/cubit-output-retirement-facade-r1, no root publication.
Scaled native47853 TERM1: r3 failed instrumentation watchdog requiring input;
held/replayed real completion, but no input observed before2000poll cap. Observer
also failed second Apply after Desktop exited. Exact failedVM QMPclosed;
boot-result FAIL/stagingunchanged. Retain all r3 evidence. Possible fixture
sequencing issue: capture settles2seconds, injectedBusy cannot finish untilnew
input; do not label production fault or dismiss without rerun evidence.
73516 LIVE current r4 native run under build lock:
/tmp/cubit-output-retirement-native-fixture-r4/run.py, outer
/tmp/cubit-output-retirement-native-run-r4.log. SAME r3 compiled binary SHA
da1b1918f9fcd0d7257c4500a2f4026fd621e24b71e5131669dfd90869f242cb,
all existing assertions retained. Runner watcher waits for drain marker then
injects QMP shift_r press/release into exactVM. injection.json records success;
requires sent plus actual Desktop input counter>0. No pointer motion or text.
Wait same73516 handle; inspect four observer PASS groups plus drain/reopen/scaled
markers before cleanQMPquit exactVM. Do not rerun fixtures in place. No root
source/index/staged promotion. Goal active; this turn PROGRESS, prior status
turn no implementation progress, now revalidated and advanced tests/evidence.

2026-10-03 output retirement progress: r2 native63802 TERM0 PASS all four
interaction groups; boot-result input5/late2/staging unchanged. Corrected
presentation gate also passes exact-controller15450 TERM0 with1000 pending
presentation observations and staggered releases. Layout restoration wired in
PRIVATE main, normal native45775 TERM0 and scaled fixture32641 TERM0 linked.
Current47853 LIVE under shared build lock: r3 fractional-DPI drain/reopen test,
/tmp/cubit-output-retirement-native-fixture-r3/run.py; outer log
/tmp/cubit-output-retirement-native-run-r3.log. Initial sandbox launches failed
Nix cache access before tests; retried with escalation, not a lock conflict.
Private facade caller migration /tmp/cubit-output-retirement-facade-r1:
41641 TERM0 all11 text/fill success and unsafe cases passed typed target release.
Native output test migrated privately, compilation still pending. Root sources,
index and staged binaries not promoted. Shutdown/partial-setup native gates,
facade proofs and packaging remain before publication. Goal active.

2026-10-03 nativeoutput r1 FAILED retained as evidence:94346 TERM1;
Desktop exited outputretirementuncertain and observerfailed. ExactfailedVM
cleanQMPquit; stagingunchangedtrue. Sourceaudit found Display rejects release
whilePS.frameState nonIdle. FixedPRIVATE main: keepnormalCP.Complete validation
whileclosing; gatelease release on notP.Enabled orCP.Writable(P.Transfer), via
newpure Can_Release_Lease. Priorignore-known-closingreply approach waswrong.
Actual r2 native build2708 TERM0 SHA
e8613b8050c6740fd4f818615611ee3e2f08ee39427080b843cad7dc87346b2e.
Freshfixture /tmp/cubit-output-retirement-native-fixture-r2 includes held-real
completion replay, inputwhilebusy, grantdelay3/7, rawphasefailurediagnostics.
r2bootstartingunderbuildlock; log/tmp/cubit-output-retirement-native-run-r2.log.
83779 TERM0 updatedretirementtests/proof andlayoutrestoretests/proofPASS.
Private /tmp/cubit-output-layout-r1 Compositor_Layout_Restore keepsaccepted
DPI/originexactlyonlyifphysicalmodes/count/namesmatch;10layoutcases,15checks
2flow13prover0unproved/justified. Initial8739TERM4 typingerrorsfixed.
Layoutrestore NOTyetwiredintoDesktop; Applyduringdrain also unresolved.
No rootproduction/indexedits; goalactive/currentPROGRESS.

2026-10-03 output nativefixture93279 TERM0 link
3ae5392a24a507d5cb4ef423ae881c91782983fd6c1b5d8f7dc3f0a894d0b68f.
Private /tmp/cubit-output-retirement-native-fixture-r1 startsdrain afterrealinput
andqueuedframe, withholdsrealcompletion thenreplaysduringdrain; injectsrenderer
Busyuntilinput+latecompletion, grantconfirmation3/7polls, checksrevokeonce,
noearlyfree, Ptokensretained, charged0commit/reopensamecharge. Nativebootnext
underbuildlock via run.py/testimageoverride; no root/staging/indexchanges.
Audit identifiedprepublicationgap: current setup resetsDPI/arrangement onreopen;
need preserveacceptedlogical layout andhandle Apply duringdrain withoutdropping
useraction. r1fixturetestsunit-layoutfirstinteraction, notthisunfixedcase.
Previous/currentPROGRESS; goalactive.

2026-10-03 PRIVATE output-drain MAIN integration nowlinks; no rootpublication.
/tmp/cubit-output-retirement-r1/main.adb authoritative candidate; initial
prepare-integration.py predates postgenerationfixes DO NOTrerunovermain.
52124 TERM1 localAda defaultconformance fixed.72138 TERM0 nativeMesaDesktop
linkfaeb653baea94f0c5e8b754626d52d45efd8293f03b34d61d2cdaa4c2f1036ce;
inputhashes desktop-link-inputs.json, candidate-hashes.json. TypedTarget_Release
(Retired/Busy/Unsafe) privatebothbackends. Persistent2output retirementstates.
closeOutput pollsrenderer, releasesleaseonlysuccess, revokesonce, pollsaccepted
grantconfirmation; storagefreeonlyReady. Globaldrain retainsP/tokens untilall
released thenwatermarkcommit/clear; collectPresentations consumesknownclosing
IDs without grantingwritepermission. Paint/cursorguards +1msretry; shutdownloop
waitsforoutput+source retirement, noindefinitewait/postcleanupdraw. Newwindows
keepmetadata/input duringdrain, reopenpending retriesoneowner-acquireattempt
without2mssleep (startup keepsoldboundedhandoff). Partialsetupcleanup preserved.
No falseBusyinterpretation ofDisplayerrors. NEED nativefault/normal/partialsetup/
shutdown tests andfacadeproof beforepublication; callerfactsnotwholemainproof.
91289 TERM4 hostedtestanonymousarrayaggregate syntax fixed;8675 TERM0 exact
closeOutput/pumpOutputRetirements/releaseDisplayBuffer routines PASS rendererhold,
revokeonce, deferredconfirm, staggeredoutputstoragefree, retainedcompletionIDs,
newwindowreopenrequest. /tmp/cubit-output-retirement-caller-r1 hasmockFFIharness/
source-extractionhashes;log/tmp/cubit-output-retirement-caller-r2.log.
Next nativefixture mustexplicitlyrequestglobaldrain whileinternalshellactive:
normaldualoutput testsleaveinternalShellsurfaceused, so lastwindowclosealone
DOESNOTexerciseoutputcleanup. Inject3+Busy anddelayedgrantconfirmations inactual
caller, validatePidentityretention,noearlyfree/revoke, safe-reopen; exercise
input/requestdispatch andlatecompletion. Existingprivate sourcefixture pattern
canadapt butnewtargettypedinterface also needs native_output_test/othercallers
updated beforepublication. Allownjobs terminal/noVM/sharedlock/indexchanges.
Previous/currentPROGRESS; goalactive.

2026-10-03 PRIVATE output retirement /tmp/cubit-output-retirement-r1.
61595 TERM0 concrete3target SPARK20checks9flow11prover,0unproved/justified;
hosted16lease/grantlayouts,16krendererBusy+24kpendingconfirmation observations,
orderedcleanup/partialtargetsets/uncertainty PASS. No rootpolicy/main/facade edits.
NewCompositor_Output_Retirement: renderer->lease->revoke acceptance->grant
confirmation->storage, persistentperoutputmetadata; Busy/falseconfirmation exact
statepreservation. Failedlease/revoke/rendereruncertain ->stickyquarantine.
CurrentDisplaywire hasNOexplicitBusyrelease; do NOTtreatBadStateasretryable.
Integrationaudit found latequeuedcompletionhazard: collectPresentations rejects
unknown token>retiredThrough, so mustretain closingoutput identities throughdrain
or explicitlyclassify knownclosingtokens. Can'tclearPafteroneoutputfreewhile
otherspending. Moreanchors/partialsetup/shutdown/writeguards inprivate
integration-audit.txt. Next actualMaincontroller+typedtargets integration, not
justenumrename. Allownjobs terminal/noVM/sharedlock/indexchanges. Prior/current
PROGRESS. Goalactive; no GPU/hardwarelatencycompletionclaim.

2026-10-03 FINAL41564 TERM0 root retirement regressions PASS: exactcaller
24kbusy, actualsurface/receipt routines, nativeoracle plus negativecontrols;
retained nativefixture accepted.18publishedhashes+scopeddiff previouslyverified.
Publisher source-retirement-r1 SUCCESS NEVERrerun. Allownjobs terminal/noVM/
sharedlock. No stagedbinary/index/commit/pushchanges. Source retirement is now
rootintegrated, notprivateonly. Next audit: Forget_Targets remainsBoolean and
releaseDisplayBuffer called onlastsurfaceDestroy/CloseAll andshutdown; ordinary
GPUbusy there mustdefer teardown without losingtargetleases or blockinginput.
Need completeoutput-drainstate integration, not an enum-only APIchange.
Previous/currentPROGRESS; goalactive.

2026-10-03 SOURCE RETIREMENT PUBLISHED18files+backenddoc.
/tmp/cubit-publish-source-retirement-r1.py SUCCESS NEVERrerun; manifest
 tests/compositor/build/source-retirement-published.json. Rootmain now reserves
bounded source loans before acquisition; typedSourceRelease, asyncglobalmetadata
retirement, receipt gating and boundedloopretry integrated. Bothbackends updated;
newpolicy/check/tests+portable callerfixture/nativeinjection/oracle+documentation.
62356 TERM0 nativelegacyDesktop link6d9c026955a6189ad8d1a51afd94470c0a0aaff01b7fb54189d159c45a9e6ab0
plus typednative_output_test compile. Packaged91058TERM1 ambiguous extraction
anchor fixedforwarddeclaration;53974TERM0 caller+surface+oraclechecksPASS.
Nativehelper packaged instrumentation matches bootedcandidate moduloindentation;
retained nativeevidence accepted.18publishedhashes verified/scopeddiffcheckclean.
Rootpostpublicationregressions running /tmp/cubit-source-retirement-published-r1.log.
NoVM/sharedlock/staging/indexchanges. Prior/currentPROGRESS. Next asyncoutput
teardown +protocol immediate-retirement assumptions; actualGPUfacade/targets
and hardware measurements remain open. Fullgoal active.

2026-10-03 FINAL native source-retirement95711 TERM0 PASS, all4native groups
(primary/scaling/arrangement/Desktop) plus fixture exact-source oracle.
Private /tmp/cubit-source-retirement-native-fixture-r1/boot-result.json: real
MG.Acquire/Return with3injectedBusyobservations each; pending detach followedby
confirmedreturns. SourceBusy injection withholds synchronous renderer evidence,
not GPU fence execution. CleanQMPquit exactVM afterallgroups; noVM/heldlock;
testimageoverride verified stagingunchanged. Runnerlog source-retirement-native-run-r1.log.
Facade49552 TERM0: Mesa85checks46flow39prover;legacy25flowchecks;0unproved/
justified, fresh typed-retire-r1 reports. Nativefixture build97581TERM0; source
surface25793TERM0 (publication receipt+close/reset+legacyreplacement tested).
All ownjobs terminal. Candidate remains PRIVATE, no rootproduction/index edits.
Next portable test/evidence packaging+guarded publication after legacy native
compile and typed native_output_test compilation. Private native_output_test.adb
updated for Source_Release; rootstillBoolean untilatomicpublication. Do notrerun
completednativefixtureinplace. Async targetteardown and immediate-retirement
assumptions in older desktop-check remain separate follow-up scopes. Current
PROGRESS; previousPROGRESS. Fullgoal notcomplete/hardware timingunmeasured.

2026-10-03 PRIVATE source surface25793 TERM0: exact main releaseSurfaceBuffer/
retirePublicationBuffer routines pass hosted publication replacement,1000pending
receipt polls, close+reset whileheld, legacyreplacement, idempotence. Controlled
FFI only; see /tmp/cubit-source-retirement-surface-r1.log/extraction manifest.
Nativefixture97581 TERM0 linkd3de85ade500a5d424f22955688b1cac007a00431fe4f5be3225accfd5dea0e5.
95711 LIVE native dualoutput fixture under sharedbuildlock, testimageoverride
(no stagedDesktopreplace), threeBusyobservations peractualsource acquisition
beforegrantreturn. Exact /tmp/cubit-source-retirement-native-fixture-r1/run.py;
log/tmp/cubit-source-retirement-native-run-r1.log,fixtureboot.log+serial.log.
Currentheadlessprerequisitebuild; do not edit sharedinputs/restart ontimeout.
Older native desktop-check publication tests assume immediate retirement; need
bounded exact-receipt polling before using asynchronousfixture withthose tests.
No rootpublication/indexchanges. Previous/current PROGRESS.

2026-10-03 PRIVATE Desktop source-loan integration now links and caller tests pass.
61020 TERM0 stronger concrete24slot proof:25checks7flow18prover,0unproved/
justified. source-retirement-checks-r3.log and checked-obj/proof-r3 report.
Private /tmp/cubit-source-retirement-r1 main.adb reserves24global metadata slots
before both authenticated MG.Acquire paths; actual grant/address survive surface
reset. Typed facade Source_Retired/Busy/Unsafe, both software backend bodies.
Publication receipts wait for Released; event loop pumps bounded retirement table
and includes pending loans in1ms retry condition. No production publication yet.
Native21210 TERM1 missingbufferLoan in2aggregateinitializers fixed privately.
21675 TERM0 nativeMesaDesktop linkSHA
7f217a6db84dfa3fce6f4107d8a331a4b68069ba233011aeb9562c2f9ecb7c5a;
input hashes verified desktop-link-inputs.json. FirstsandboxNix attempt failed
cacheRO, escalated normalNix run succeeded. No boot/hardware claim.
68006 TERM0 hosted exact-extracted actualDesktop helpers+controlledFFI:24kbusy
polls preserve mappings; fulltable failsbeforeacquiresyscall; failedacquirecancels;
staleticket cannotreturnnewloan; orderedreturn; renderer/returnuncertainty holds
actualrecords andrequestsrestart. Extractionhashes in caller-extraction.json.
Policytest24kbusy separate. Callerfixture notnativeIPC/GPU evidence.
Next: actual releaseSurfaceBuffer/retirePublicationBuffer receipt+closed-record
checks and native close/replacement/delayedsource fixture, legacy facade compile/
proof and updated native_output_test typedcaller. Last-surface displayteardown
still needs audit for asyncGPU target lifetime. No rootmain/backend/testGPR/index
changes; allownjobs terminal/noVM/sharedlock. Previous userchecklist turn was
NO PROGRESS; current turn PROGRESS. Goal remains active.

2026-10-03 source loan78693 TERM0:24kbusy testsPASS and concrete24slot
proof passed. Added stronger private frameconditions (everyother slot unchanged,
identity/serial unchanged exceptReserve); new tests/proof starting inproof-r3,
log/tmp/cubit-source-retirement-checks-r3.log. No mainintegration or publication
yet; generictemplate72438no-check run explicitly excluded. CurrentPROGRESS.

2026-10-03 PRIVATE source retirement policy /tmp/cubit-source-retirement-r1.
New generic Compositor_Source_Loans fixedslots reserve before acquisition;
Reserved->Attached->Renderer_Pending->Grant_Pending->Released, Busy unchanged,
uncertainty stickyQuarantined. Opaque generational tickets prevent slot reuse
until retirement and stale mutation. Concrete24slots=8surfaces*(2publication+
1replacement allowance), metadata only. Main integration not implemented yet.
72438 TERM1 generic-alone proof no checks, NOT evidence. Concrete checks78693
LIVE:24kbusyobservations, exhaustion/order/staletickets/cancellation/uncertainty
and source_loans_check instantiation proof; log/tmp/cubit-source-retirement-checks-r2.log.
Actualcaller plan: global owned loan table retains mapping after surface close,
reserve at both MG.Acquire sites, typed renderer retirement, bounded event-loop
polling. Publication receipts await real grant return; destroyed metadata can
clear only after global table owns it. No sharedsource/build/staging/index edits.
CurrentPROGRESS; priorPROGRESS.

2026-10-03 FINAL31616 TERM0 published2scene+10presentationregressionsPASS.
Fivepublicationhashes reverified. Cleanproof332checks198flow134prover,2units,
noneunproved/justified. Native76objects andrealMesa pixelchecksPASS. Allownjobs
terminal/noVM/sharedlock. Publishercapture-admission-r1 SUCCESS NEVERrerun.
Next integration audit: main retirePublicationBuffer/releaseSurfaceBuffer use
Boolean Forget_Source and local Compositor_Readers state; any non-clear result
restartsDesktop. Ordinary asynchronous GPU reader busy must eventually defer
retirement and retain source metadata/loan, while uncertain release remains
sticky quarantine. Surface destruction/replacement cannot discard queued
retirement or reuse occupied slots. Needs actual caller/lifetime integration,
not merely a bool-to-enum rename. No edits yet. Goalactive/currentPROGRESS.

2026-10-03 CAPTURE ADMISSION PUBLISHED5files+backenddoc.
/tmp/cubit-publish-capture-admission-r1.py SUCCESS; NEVER rerun. Clean53155
TERM0:332checks198flow134prover, exactly2units,noneunproved/justified.
Do NOT use old916 aggregate. Fivepublication hashesverified manifest
tests/compositor/build/capture-admission-published.json. Native76objectarchive,
10presentation+2scene cases/realMesa49152font+233472affinepixels allPASS.
Rootregressions running /tmp/cubit-capture-admission-published-r1.log.
NoVM/sharedlock/indexchanges. PreviousVERIFIEDWAIT/currentPROGRESS.

2026-10-03 cleanproof53155 stillLIVE by directpoll; logs show scene
contracts proved, owner analysis pending, no fresh summary yet. Prepared
/tmp/cubit-publish-capture-admission-r1.py NOT RUN, gates clean report/current
dependency locations, exact nativearchive/private sourcehashes, realpixeloracle
and5rootbaselines. Do not execute until53155terminalPASS. Allotherownjobs
terminal. NoVM/sharedlock/indexchanges. CurrentVERIFIEDWAIT; previousPROGRESS.

2026-10-03 admission17784 TERM0 no unproved; aggregate916includes14units
with stale dependency report locations (Damage.Add line30 vs current50). Do NOT
claim916as clean currenttotal. Actual Admit_Capture and Begin_Frame proved.
Fresh --subdirs=proof-clean-r3 proof started log/tmp/cubit-capture-admission-proof-r3.log.
Native62512 TERM0 PASS76objectarchive snapshotdesktop-gpu-native-d0pg__zl,786inputs
and symbolclosure verified.10presentation+2scene cases/realMesa pixeloraclePASS.
No publication yet pending cleanproofscope. All candidate sources unchanged;
noVM/sharedlock/indexchanges. CurrentPROGRESS.

2026-10-03 capture coverage98832 TERM0:10presentation scenarios with32
read-only preflights eachtransition pass, front/pending exclusion and latest
ready replacement accepted, no submission-FFI/byte/front/token mutations.
Real Mesa/font oracle49152exactpixels+233472affinepixels,validation0.
Initial2scene lifecyclecasesPASS. Selectedproof17784 stillLIVE by directpoll;
no error/high/medium messages yet. Native overlay helper prepared privately,
actual sourcepaths/hashes retained; native gate starting next. No publication.
CurrentPROGRESS; noVM/sharedlock/indexchanges.

2026-10-03 capture admission8935 TERM4 hosted GPR accidentally included
all runtime overrides (missing explicit inherited Source_Files). Corrected
private GPR with Desktop_GPU_Scene Source_Files and fresh obj-r2.17784 LIVE
tests/proof log/tmp/cubit-capture-admission-r2.log. No production changes;
source candidate unchanged. Prior/currentPROGRESS.

2026-10-03 PRIVATE capture admission /tmp/cubit-capture-admission-r1.
8935 LIVE tests +selected owner/scene proof; log/tmp/cubit-capture-admission-r1.log.
Read-only typed preflight: allowed/busy/unavailable/uncertain. Checks device,
pipeline, target readiness/dimensions, submission/upload busy and writer room
before Begin_Frame accepts readers/uploads. Render still revalidates; no
reservation, import authority or new allocation/FFI. Four guarded sourcefiles.
CurrentPROGRESS; no sharedproduction/build/staging/index changes.

GPU/Display owner handoff request (no operation numbers reserved): current
Display protocol still has only CPU-source attachment/copy semantics. Need
agreement on three authenticated GPU targets per output (format/modifier/pitch,
optional CPU mapping, renderer import capability), plus separate latch and
previous-front/final-front retirement evidence. Existing application BO
Map_Presentation forwarding is CPU-linear source access, not this scanout
contract. Please review docs/compositor-shared-targets.md before any driver ABI
implementation; compositor-side retained front/pending and cancellation policies
are already proved. No driver-owned source changes proposed in this turn.

2026-10-03 FINAL23089 TERM0 published retry regression PASS1000/32000;
oracle3positive12negative and native retry evidence PASS; scoped diffcheck clean.
Allownjobs terminal/noVM/sharedlock. Both output-begin and frame-retry publishers
successful: NEVER rerun. Eleven latestpublication sourcehashes verified earlier.
Goalactive, PROGRESS. Actual GPU facade/targetauthority and hardware latency
remain open; no completion claim.

2026-10-03 FRAME RETRY PUBLISHED11files+backenddoc.
/tmp/cubit-publish-frame-retry-r1.py SUCCESS; NEVER rerun. Native52898 TERM0
all retry checks and primary/scaling/arrangement/Desktop groups PASS. Exact
stagedDesktop restored f20c2fd5f700550a99a68a51115018d67967ff02c33f1b1eaa26fd6f527e985d.
Clean QMP quit after requiredgroups; noVM/sharedlock. Eleven sourcehashes
verified tests/compositor/build/frame-retry-published.json. Root tests/oracle
nowrunning /tmp/cubit-frame-retry-published-r1.log. Damageproof34checks
11flow23prover,noneunproved/justified; hosted1000retries/32000freshupdates.
No indexchanges. Next actual GPU facade scope+whole-scene capture, authenticated
client GPU imports/Displaytargets remain; no hardware presentation claim.
Previous/currentPROGRESS.

2026-10-03 native retry52898 LIVE verified by direct poll; currently
headless build prerequisites before boot. Oracle self-test PASS3positive/12negative.
Exact log/tmp/cubit-frame-retry-native-r1.log; fixture boot/serial/evidence under
/tmp/cubit-frame-retry-fixture-r1. Do not restart on observationtimeout.
Private retry-notes.md records proof/testscope; nativeacceptance stillpending.
No new productionpublication/index edits; currentturnPROGRESS.

2026-10-03 retry fixture56534 TERM0 native link
a323f532b90b98ded2c4cf25ee1f2028476a7a40aaafb6f0b1e2decfc4ab56ea.
Native retry run started under shared build.lock; /tmp/cubit-run-retry-fixture-r1.py
and log/tmp/cubit-frame-retry-native-r1.log. Runner restores stagedDesktop.
Do not edit shared build sources while active. Reusable --retry builder/runner/
oracle prepared privately; generated main exactly matches compiled fixture.
No production retry publication. Root output-begin publication is complete.

2026-10-03 private retry native3489 TERM0 link
042c7aae31e9094e1463acae9c7403d601bb0ede864570a806a1a49231e7c874.
Private /tmp/cubit-frame-retry-fixture-r1 now building; inject Retry on third
frame/output after confirmed software completion and corruptfirstpixel.
Asserts no displaytoken/front change, restored old+fresh damage, full repair
on failedslot, freshwriterticket, and subsequent recaptured publication.
Runner /tmp/cubit-run-retry-fixture-r1.py prepared; not booted yet.
Logs /tmp/cubit-frame-retry-fixture-build-r1.log. No shared production/staging/
index mutation. Goalactive; previous/currentPROGRESS.

2026-10-03 PRIVATE frame retry /tmp/cubit-frame-retry-r1. New proven
Compositor_Damage.Restore preserves captured+freshdamage, empties snapshot.
Proof22596 TERM0:34checks11flow23prover,noneunproved/justified. Hosted27755
TERM0:1000retryframes/32000freshupdates, failed-writer repair, heldfront safety,
no failed publication. Private main Retry outcome calls existing proved
Failed_Quiescent, RP.Failed_Render, Restore, Acquire; no Display submit.
No publication yet; native build next. Rootbeginoracle16278 TERM0 PASS.
NoVM/sharedlock/indexchanges. Previous/currentPROGRESS.

2026-10-03 output begin PUBLISHED eight files +backend doc.
/tmp/cubit-publish-output-begin-r1.py SUCCESS; NEVER rerun. Native65255 TERM0,
all four interaction groups and newfixture PASS, exact stagedDesktop restored.
Clean QMP quit after allrequiredgroups; no timeout/stoppedjob ambiguity.
Manifest tests/compositor/build/output-begin-published.json eighthashes verified.
Mesa84(45flow39prover)/legacy25flowchecks allproved;11softwarefaultmodesPASS.
NoVM/sharedlock/indexchanges. Next: proven full-frame retry recovery +Desktop
completion outcome for GPUcold upload recapture. Previous/currentPROGRESS.

2026-10-03 native65255 LIVE: begin checks both outputs PASS; completion
held3/6polls and next-frame damage bothPASS. Full interaction harness pending.
Prepared guarded /tmp/cubit-publish-output-begin-r1.py (NOT RUN): validates
terminal native PASS/restoration, exact generated fixture, eight root baselines,
Mesa84/legacy25proofchecks before publish. Do not execute until65255 terminal.
Next integration audit: GPUScene.Poll returns Retry after cold glyph upload;
facade currently only Complete/Pending/Unsafe. Need whole-frame recapture path
that restores captured damage plus fresh input, invalidates potentially partial
writer pixels, and retires only quiescent render ownership. Existing BP
Finish_Render(Failed_Quiescent) clearswriter withoutmakingReady; use/prove this
transition rather than publish incomplete frame. No implementation yet.

2026-10-03 begin fixture83597 TERM0 native link
b3bb18743b1704aa8e080eb76e1e5a06746abd25026de8b9017fadda790ce6ce.
Native r2 started under shared build.lock /tmp/cubit-run-begin-fixture-r2.py;
logs /tmp/cubit-output-begin-native-r2.log and fixture-r2 boot/serial logs.
Do not edit shared build sources while harness active. Runner restores staged
Desktop. Current fixture adds indefinite-idle assertion and main retry fix.
No root publication/index changes. Previous/current goalturns PROGRESS.

2026-10-03 PROGRESS: corrected Mesa facade proof9871 TERM0:84checks45flow/
39prover,0unproved/justified. Final46076 TERM1 only test invoked without mode;
native link and legacy proof passed. Re-run66112 TERM0 all11 text/fill modes
PASS and native final link9499107e0674612b3852887c5e280673dba08f1b529e65dc9efc2f8dc814a349.
Audit caught deferred begin could idle indefinitely: private main now includes
writable selected-renderer output damage in renderingPending (bounded1msretry).
No new allocation/metadata or speculative wait. Requires new native validation.
Prepared /tmp/cubit-output-begin-fixture-r2 with Probe_Idle assertion, build
started /tmp/cubit-output-begin-fixture-build-r2.log. Allrootbaselines unchanged,
no publication/index changes. Prior native r1 PASS does not cover new idle fix.

2026-10-03 native36583 TERM0 completePASS/restored. Proof58452 TERM1: two
flow errors because proposed In_Out Engine hooks only read current software
state. Corrected PRIVATE spec Begin/Complete to truthful Input contracts; no
fake state writes added. Future GPU implementation must extend its real state
contract. Proof9871 LIVE /tmp/cubit-output-begin-proof-r2.log. Native fixture
passed with identical executable logic but earlier Global contract; final native
relink/source inventory still required. Regression builder/inc/oracle/docs
prepared privately; generated fixture matches executed main exactly. All eight
root source baselines unchanged; no publication/index changes. CurrentPROGRESS.

2026-10-03 native begin36583 TERM0 PASS: all primary/scaling/arrangement/
Desktop groups pass, two-output deferral and pending checks pass. Staged
Desktop restored hashf20c2fd5f700550a99a68a51115018d67967ff02c33f1b1eaa26fd6f527e985d.
No VM/shared lock held. Private facade SPARK proof now started, log
/tmp/cubit-output-begin-proof-r1.log; no production publication yet.
Previous/current goal turns PROGRESS.

2026-10-03 native36583 still LIVE by direct poll. Both outputs passed
three deferred-start assertions; held completion retired after3/7polls and
fresh damage rendered on next frame for both. Full native interaction harness
still running; do not claim full PASS/restoration until runner terminal.
Private proof project prepared /tmp/cubit-output-begin-r1/glyph_renderer.gpr,
not yet run to avoid competing with VM. No production publication.

2026-10-03 LIVE36583 native begin/completion test under shared build.lock.
Command /tmp/cubit-run-begin-fixture.py via Nix; native harness may rebuild
kernel/image prerequisites. Do not edit shared build sources during this run.
Runner restores exact saved staged Desktop in finally. Fixture47845 TERM0,
SHAad6649237ea595b2f82eacfb370c5529c312f4ce72f75e6b8bbf3835bee41a51.
Logs /tmp/cubit-output-begin-native-r1.log and privatefixture boot.log/serial.log.
No production facade publication; no index changes. Poll36583 to terminal.

2026-10-03 Desktop begin integration PRIVATE: native link 22260 TERM0,
SHA79484b26e141cc645e363812e9dea1172615d0c4d569028ec98f699479e91349.
Candidate four facade/main/backend files in /tmp/cubit-output-begin-r1,
baseline inputs.json guards. No production publication yet. Building separate
/tmp/cubit-output-begin-fixture-r1: inject three Deferred begins, assert damage,
repair and ownership unchanged; completion fixture asserts exactly one begin
scope across pending polls. Private objects only; no shared staging/index.
Previous status-only turn NO PROGRESS; current native link evidence PROGRESS.
Goal active; main GPU renderer/imports/Display handoff remain incomplete.

2026-10-03 FINAL66621/67378 TERMINAL0: rootdrawingtests3PASS;
nativeAda+muslC archive PASS,76objects, no unresolvedcubit_vulkan_* symbols.
Snapshot tests/compositor/build/desktop-gpu-native-s7rimu_m hashes/result/build.log
retained. All8gpu-drawing publicationhashes reverified unchanged. Proof21checks,
realfonts49152pixels+affine233472pixels; validation0. Allownjobs terminal/noVM/lock.
No index/staging/commit/push. Goalactive. Actualmain drawing facade/frame scopes,
clientGPU imports and Display authenticatedimagehandoff remain integrationwork.
Previous/currentturnPROGRESS. Do not rerun successful GPUdrawingpublishscript.

2026-10-03 GPU drawing adapter PUBLISHED8files+docappend.
/tmp/cubit-publish-gpu-drawing-r1.py SUCCESS; NEVER rerun. Manifest
tests/compositor/build/gpu-drawing-published.json supersedes changed prior inventories.
70104 TERM0 lifecycle2PASS +selectedDrawing21checks9flow12prover, no unproved/justified.
34854 TERM4 testonlymissing coordinate use type fixed.78501 TERM0 mixedtests +realfont
49152exactpixels/8glyphallocations/2faces/4DPIs;233472affinepixels/validation0.
Published root hostedregression and nativecomponentbuild live, logs
/tmp/cubit-gpu-drawing-published-r1.log and /tmp/cubit-gpu-drawing-native-r1.log.
No sourceeditsduringbuilds; no index/staging/nativeVM changes. Main stillsoftware;
next facade/frame capture scopes +clientimageimports/Displayhandoff.
Previous/currentgoalturnPROGRESS.

2026-10-03 PRIVATE Desktop drawing adapter /tmp/cubit-gpu-drawing-r1.
New Desktop_GPU_Scene.Drawing child converts physicalfills, logicalimages with
physicaldamage and32glyph textbatches into retainedscene commands. Empty/offscreen
requests skip readers/uploads; each call suppliesits clip; cold/invalid captures
finish/discard wholeframe, neverperdrawCPUfallback. No newFFI/pixelcopy.
70104 LIVE text lifecycle scenarios (alreadyPASS2) then selected Drawing SPARKproof;
log/tmp/cubit-gpu-drawing-r1.log.34854 LIVE mixeddrawingtests +realMesa/fontoracle,
log/tmp/cubit-gpu-drawing-real-r1.log. Privateonly; sourceinputsfrozen.
No sharedproduction/build/index/staging. Prior/currentturnPROGRESS.
Main facade/scope/clientimports/displaytransport stillincomplete.

2026-10-03 FINAL24612 TERMINAL0 native component archive+symbolclosure PASS.
Snapshot tests/compositor/build/desktop-gpu-native-0r86gc_7; result/inputshashes/
external-symbols/build.log retained. All6physical-clip publicationhashes unchanged.
Allownjobs terminal/noVM/lock; no index/staging/commit/push. Proof132scene+
72owner checks, no unproved/justified; pixel/DPI/fault evidence documented.
Main drawing stillsoftware: next GPU drawing adapter for physicalfill/text/image
clips, then main frame capture scope and authenticated Display image sharing.
Goalactive; previous/currentturnPROGRESS, no blocker streak.

2026-10-03 physical clipping PUBLISHED6files+docappend.
/tmp/cubit-publish-physical-clip-r1.py SUCCESS; NEVER rerun.
63940 TERM0 selectedVulkan_Scene132checks20flow112prover, noneunproved/justified;
88650TERM0 realMesa384frames2359296pixels/validation0 then selectedDesktop_GPU_Scene
72checks35flow37prover, noneunproved/justified. Physical full/partial/emptyclips.
Source manifest tests/compositor/build/physical-clip-published.json authoritative.
24612 LIVE nativeAda+muslC archive gate, privateoutputs; log/tmp/cubit-physical-clip-native-r1.log.
Pollsamehandle; no sourceeditsduringbuild. No index/staging/nativeVM/sharedlock.
Next Desktop drawing adapter and main scope wiring; actual Display transport
stillabsent. Goalactive; currentturnPROGRESS.

2026-10-03 physical clip verification update (private):
63940 TERMINAL0:60 physicalclip/DPI/rotationcases +5 malformed clips PASS;
selectedVulkan_Scene proof completed, report clips-obj/proof-r1/gnatprove/gnatprove.out.
12001 TERM1 reference expected alpha0 outsideclip; rendererexistingopaqueRGB fill
alpha1 verified in vulkan_submission_native.c. Only test reference corrected.
88650 LIVE: realMesa384frames2359296exactpixels +233472affinepixels PASS,
validation0; now selectedDesktop_GPU_Scene proof running. Pollsamehandle.
Private /tmp/cubit-physical-clip-r1; inputsfrozen. No sourcepublicationyet.
Prior/currentturnPROGRESS; no sharedproduction/build/index/staging changes.

2026-10-03 PRIVATE physical clipping /tmp/cubit-physical-clip-r1.
Own private Vulkan_Scene spec/body +Desktop_GPU_Scene spec/body, backdrop scene
fixture and real bridge. New typed physical clip command avoids DPI roundtrip
and applies exact pixel edges to subsequent layers; no newFFI/pixelsallocation.
17840TERM4 testonlymissing use type corrected.63940 LIVE hosted60clip cases,
5malformedcases then selectedVulkan_Scene proof; log/tmp/cubit-physical-clip-proof-r2.log.
12001 LIVE realMesa clip/full/empty pixeloracle then selected sceneowner proof;
log/tmp/cubit-physical-clip-real-r1.log. Pollsamehandles, sourceinputsfrozen.
No sharedproduction/build/index/staging. Previous/currentturnPROGRESS.
Actualmain drawing bypasses/renderer capture/displaytransport stillpending.

2026-10-03 native GPU scene component gate PUBLISHED and VERIFIED.
44419 TERMINAL0 repository entrypoint: tests/compositor/build-desktop-gpu-scene-native.py
Nix/kernel-alr native Ada runtime +pinned CuBit musl C interfaces+existing Mesaheaders.
Snapshot tests/compositor/build/desktop-gpu-native-3rrrt_23; result.json/inputshashes/
external-symbols.json/build.log retained.122Ada sourcefiles/63Ada+binderobjects,
12Cobjects; all cubit_vulkan_* symbols resolved internally.779inputfileschecked.
New script/GPR/docs publishedmanifestdesktop-gpu-native-published.json reverified.
Archive component ONLY, not finalapplicationlink/boot/deviceadmission/hardwareGPU.
39667 TERM0 earlierAdaonly outputremovedbyNixTMPDIR;15028 TERM0 preservedAdaonly;
10335 TERM0 privateAda+C;44419TERM0 publishednormalentrypoint+symbolclosure.
All ownjobs terminal/noVM/sharedlock. No index/staging/commit/push modifications.
Previous/currentgoalturnPROGRESS; goalactive. Next actual Desktop GPU scene
capture/backend integration, then authenticated image sharing/Display handoff.

2026-10-03 private native GPU scene archive build started.
39667 LIVE Nix/kernel-alr build script /tmp/cubit-native-gpu-scene-r1/
build-desktop-gpu-scene-native.py; log/tmp/cubit-native-gpu-scene-build-r1.log.
122 actual production Ada source files, native runtime copied/hashchecked;
private /tmp/cubit-desktop-gpu-native-* outputs. Compile+bind+archive only,
not final link/boot/hardware rendering. No shared source/build/index/staging edits.
Candidate new test GPR/script private until verified. PreviousturnPROGRESS;
currentprogress closes native compiler/runtime gate before mainloop GPU capture.

2026-10-03 FINAL93197 TERMINAL0: shared checkout image-reader4, glyphscene2,
presentation10 scenarios PASS (16total). All9publishedhashes unchanged.
Allownjobs terminal/noVM/sharedlock released; no index/staging/commit/push.
scene-source-pins-r2 publisher SUCCESS (do not rerun); old r1 superseded.
Proofs: first owner+scene325checks; revisedscene+backdrop75checks, overlapping
notadditive; no unproved/justified. Realprovider capture-release check PASS.
Goal remainsactive. Next fullDesktopGPUcapture/native integration and
Display authenticated image sharing/retirement. Prior/currentgoalturnPROGRESS.

2026-10-03 scene source readers PUBLISHED9files+docappend.
/tmp/cubit-publish-scene-source-pins-r2.py SUCCESS; NEVER rerun.
Old r1 publisher superseded; DO NOT RUN. Manifest scene-source-pins-published.json
in tests/compositor/build is authoritative (supersedes changed older manifests).
37239 TERM0 first owner+scene325checks194flow131prover; D unchanged inr2.
5083 TERM0 revisedscene+backdrop75checks35flow40prover, noneunproved/justified.
25903 TERM0 all4image-reader scenarios/backdrop288/realMesa384frames2359296pixels
plus233472affinepixels, validation0. Actual provider Release_Source refused during
CPU capture, readercountzero aftercompletion. All fixed metadata/no newpixelcopy.
Root final build/test running; log /tmp/cubit-scene-source-pins-published-r2.log.
No index/staging/commit/push. FullDesktopGPUcapture and drivertransport pending.

2026-10-03 source-reader r2 private active; r1 proof FINISHED.
37239 TERMINAL0: selected owner+scene325checks194flow131prover, noneunproved/justified.
D spec/body unchanged byte-for-byte in /tmp/cubit-scene-source-pins-r2.
r2 strengthens Pin_Image Current/Layer_Count preservation contract; no weakening.
5083 LIVE selected revisedscene+backdrop proof, log/tmp/cubit-scene-source-pins-scene-proof-r2.log.
25903 LIVE image scenarios+backdrop+realMesa with actual release attempts during
CPU capture, log/tmp/cubit-scene-source-pins-tests-real-r2.log.
Frozen r2inputs; all outputprivate. r1publication script NOTRUN and needsr2update.
Prior turn PROGRESS; currentPROGRESS (proof evidence+contract repair+real regression).

2026-10-03 PRIVATE image-reader verification update:
59082 TERMINAL0: backdrop288cases +realMesa384frames2359296pixels, validation0.
46409 TERMINAL0 but PROOF INCOMPLETE: backdrop6checks, one unproved postcondition
because Pin_Image's contract lacks preserved Current/Layer_Count. Do NOT publish.
37239 confirmed LIVE by handle and OSprocess1383847; selected owner+scene proof.
After terminal, strengthen private Pin_Image post to preserve Current and
Layer_Count, then reprove affected scene/backdrop. Inputs remain frozen meanwhile.
/tmp/cubit-publish-scene-source-pins-r1.py PREPARED, NOT EXECUTED; updateproofpaths
if repaired runs use newsubdir; script blocks unproved reports. No sharedchanges.

2026-10-03 source-reader PRIVATE verification now active.
64768 TERMINAL0: all4new image-reader scenarios PASS plus2existing glyphscene
scenarios. Tests cover200draws/onepin,100pendingpolls, capture+GPU release guards,
unknowncompletionretention,136-tokenexhaustion, stale/reused/None tokens,
CPUnotretired, capturedshutdown, and stale source generation rejection.
56417 TERMINAL4 was missing with Vulkan_Submission; corrected before64768.
37239 LIVE selected SPARK proof owner+scene, log/tmp/cubit-scene-source-pins-proof-r1.log.
59082 LIVE backdropcapture+realMesaoracle, log/tmp/cubit-scene-source-pins-real-r1.log.
Sources frozen whilethesejobs run. No sharedoutputs/production/index changes.
FullnativeGPUcapture/transport stillpending; currentturnPROGRESS.

2026-10-03 PRIVATE source-reader integration /tmp/cubit-scene-source-pins-r1.
Own private Desktop_Vulkan_Startup +Desktop_GPU_Scene/backdrop spec/bodies;
explicit136 CPU-reader tokens gate source release/Stop, deduplicated scene image
pins retained through capture/upload/submission until confirmed retirement.
No production or sharedbuild/index edits.56417 private hosted compile;
log /tmp/cubit-scene-source-pins-build-r1.log. New focused tests/proofs next.
Previous turn PROGRESS (presentation cancellation published and verified).

2026-10-03 FINAL70083 TERMINAL0: integrated checkout compiled; all10
presentation scenarios PASS. Four published hashes rechecked unchanged.
Clean selected owner proof245checks152flow93prover, none unproved/justified.
All ownjobs terminal, no VM/lock; no index/staging/commit/push changes.
Cancellation integration complete; goal still active, next fullDesktopGPU
capture/source-lifetime integration and authenticated display handoff.

2026-10-03 presentation cancellation PUBLISHED after clean proof and real Mesa.
41857 TERMINAL0: all10 mock scenarios PASS; selected Desktop_Vulkan_Startup
proof245checks (152flow,93prover), zero unproved/justified. 65721 TERMINAL0:
realMesa384frames2359296exactpixels/382held-target exclusions/validation0.
63666 TERMINAL1 was only inherited relative GPR paths; corrected in65721.
/tmp/cubit-publish-gpu-presentation-cancel-r1.py SUCCESS; NEVER rerun.
Four source hashes: tests/compositor/build/gpu-presentation-cancel-published.json.
70083 LIVE final root presentation build and scenarios0..9 under sharedlock;
log /tmp/cubit-gpu-presentation-cancel-published-r1.log. Poll same handle.
No native VM/index/staging changes. Full Desktop GPU capture and actual
Display image export/authenticated fence handoff remain incomplete.
Previous status-summary turn NO PROGRESS; this turn PROGRESS: proof completed
and guarded integration published. No blocker audit streak.

2026-10-03 known-rejected presentation cancellation PRIVATE
/tmp/cubit-gpu-presentation-cancel-r1. New D.Cancel_Presentation calls existing
Pool.Retire_Display after explicit no-display-reader evidence; preservesfront
and latestready. Bad/stale/unconfirmed ticket quarantines, no implicitretry.
41857 LIVE10mockscenarios then cleanfullD proof, log
/tmp/cubit-gpu-presentation-cancel-r1.log.63666 LIVE realMesaoracle finalhandoff
cancels heldpending, presents latestready against originalfront, thenretires.
Log/tmp/cubit-gpu-presentation-cancel-real-r1.log. Pollbothsamehandles; sources
frozen. Rootexistingpublishedpresentation unchanged. Inputs.json baselines.
No sharedproduction/index/nativeVM/lock. Previous/currentgoalPROGRESS.
ActualDisplay authentication/export and fullDesktopGPUcapture stillrequired.

LATEST29485 TERMINAL0 finalrootpresentation project compiles and allfive
lifecycle/fault scenarios PASS. Sevenpublishedhashes unchanged. Allownjobs
terminal/noVM/lock. Proof241checks and realMesaheldtargetchecks verified.
Goal remainsactive; noindex/staging. Next known-rejected presentation rollback,
realimageexport/Displayadapter, fullDesktop capture and hardwareverification.

2026-10-03 GPU presentation integration PUBLISHED7files+docappend via
/tmp/cubit-publish-gpu-presentation-r1.py SUCCESS; NEVERrerun.
32482TERM0 cleanselected updatedD proof241checks150flow91prover,0unproved/justified.
68276TERM0 all5mockscenarios;21552TERM0 actualMesa384frames2359296pixels,
382framesexcludeactualheldfront/pendingframebuffers,cleanup/validation0 PASS.
Root tests/compositor/build/gpu-presentation-published.json authoritative for
changed D and strengthened realbackdropfixture (supersedes those earlierhashes).
29485 LIVE finalrootproject compile/run5scenarios under sharedlock, log
/tmp/cubit-gpu-presentation-published-r1.log. Pollsamehandle; no sourceedits.
No index/staging/nativeVM. Presentationinterfaceexportsidentityonly; Display
adapter/authorizedimageexport stillabsent. DriverConfirmed evidence simulated
inhosttests, notphysicalscanout. NormalDesktopsoftware.
Next consider safe pending-presentation cancellation for known-quiescent
submission rejection (existingPool.Retire_Display, notyetexposed), plus actual
mainloop/client/decorations and driverinterop. Fullgoalactive/progress.

2026-10-03 GPU presentation integration PRIVATE /tmp/cubit-gpu-presentation-r1.
Desktop_Vulkan_Startup copy now exposes existing Compositor_Pool display lease
operations: Take_Presentation (ticketonly), Confirm_Presentation exactcurrent+
previous latch/retirement evidence, Retire_Presentation finalfront, read-only
pending/front/fault queries. No newFFI/rawimageexport/nativeprotocol.
Pool displayheldtargets now stay in actualD render/stop lifecycle; latestready
can recycle thirdtarget while front+pending held. Renderer completion notlatch.
32482 LIVE cleanselected D SPARK proof, log /tmp/cubit-gpu-presentation-proof-r1.log.
68276 LIVE mock GPUowner presentationtests, log /tmp/cubit-gpu-presentation-tests-r1.log:
64readyreplacements, noqueuegrowth/heldtargetreuse, exacthandoff+stopretirement,
wrong epoch/previous/unconfirmed/finalretire faults retainallstorage.
Poll exacthandles; freezeprivateD/tests/GPR. No sharedsource/index/nativeVM.
Current goalPROGRESS, previous wallpaper30filesalreadyPUBLISHED. Physicaldriver
handoff/mainloop and remainingdrawingstillincomplete. No copy/performanceclaim.

LATEST94394 TERMINAL0 published root GPR source-inventory checks and real
Mesaoracle PASS384frames2359296pixels216chunks/twoimages/validation0, same
companion233472affinepixels. Inventory warnings missing ALI in fresh testdirs
are not compile evidence; actual real-oracle build/link/run succeeded. All30
published filehashes rechecked match backdrop-published.json. All ownjobs
terminal, no VM/lock. Native125%Settings screenshot inspected and available.
No index/staging changes; fullgoalactive. Next GPUmainloop/decorations/client
buffer and displayintegration; existingnativeDesktop stillsoftware.

2026-10-03 wallpaper sources PUBLISHED30files via
/tmp/cubit-publish-backdrop-r1.py SUCCESS; NEVER rerun. Root
 tests/compositor/build/backdrop-published.json verifies exactset.
16629TERM0 expanded nativeDPI fourgroups+headlessPASS; stoppedonlycompleted
ownVM1303882. Native125%screenshot /tmp/cubit-backdrop-capture-r1/native-desktop-125.png
visually inspected Settings+taskbar.78017TERM0 renamedrealoracle same384frames/
2359296pixels/zeroValidation.42482TERM0 cleanCAPTUREproof5checks2flow3prover;
previous589includedcachedunits; docs explicitlycorrectcounts5/11/31.
78452TERM7 gprls missingfresh outputdirs; no sourcefailure.94394 LIVE root
published-project inventories then realMesaoracle under sharedlock, log
/tmp/cubit-backdrop-published-check-r2.log. Pollsamehandle, sharedsourcesfrozen.
No index/staging/commit; normalDesktop remainssoftware, GPUmainloopunfinished.
Published testdocs tests/compositor/desktop-backdrop.md and runscript.
Current goalPROGRESS. Next afterrootcheck begin mainloop GPU capture boundaries
and remainingdecorations/client/displayhandoff; private softpipe-only facadedraw
cannotbe mislabeled asGPUoutput. No physical240Hz/latencyclaim.

LATEST61210 TERMINAL0 full nativeDesktop compile/bind/link+hashguard,
SHA8de871ea9ded52d8bf630ba6a47d1ed40ec675ffc28b9c5c6f241df15449d523.
16629 LIVE expanded nativeDPI/Desktop run under sharedlock:
/tmp/cubit-backdrop-dpi-r1.log and .serial.log, privateDesktopoverride;
SCALING/PRIMARY/ARRANGEMENT/MIXED_OUTPUTS=1. Poll16629; no restart/inputs edits.
Realhost49908TERM0 384frames2359296exactpixels/216chunks/2images/validation0
recorded real-verified.json. Sourcepublication stillpending nativeQA.
Need rename realhost bridge/test fixtures before rootpublication to avoid
replacing existing glyph scene realfixture; preserve root tests/gpr ownership.
Commonstyle changes need add desktop_backdrop_style.ads to explicit
wallpaper_output/settings_renderer/desktop-wallpaper GPR sources. New source
helpers not wiredintoMain yet; current native candidate onlysoftwarestyle.
All newbackdrop sources private, noindex/staging changes.

2026-10-03 REAL MESA WALLPAPER VERIFIED49908TERM0.
Actual embeddedassets -> ownerchunkupload -> GPUscene -> physicaltarget readback
384styles/DPI/rotations,2359296 EXACT pixels vs actualsoftwarePaint,216initial
chunks/tworetainedimages/accountedcleanup. Companion233472affinepixels and zero
Vulkanvalidationerrors. /tmp/cubit-backdrop-real-r3.log. This is hostedllvmpipe,
notnativeGPU/performance.94670TERM1 testoperatorvisibility;19374TERM1 privateGPR
missingcompositorsampling; correctedtestbuildonly beforeverifiedrun.
61210 LIVE nativeDesktop fullcompile/bind/link+hashguard under sharedlock via
/tmp/cubit-backdrop-link-desktop-r1.py, log/tmp/cubit-backdrop-native-link-r1.log,
privateoutputdesktop-mesa-wallpaper.svc. Freeze inputs/pollsamehandle. This
candidate changes sharedstyle in softwareWallpaper; doesnotactivateGPUMain.
AfterterminalPASS nativefullDPIregression then guardedsourcepublication/docs.
All sourcechanges PRIVATE; no index/staging. GoalPROGRESS, mainGPUroutingopen.

2026-10-03 real wallpaper oracle LIVE94670, privatebackdropcaptureworkspace.
/tmp/cubit-backdrop-real-r1.log. Nix vulkan-affine-shell; actual embedded native
wallpaper.o/wallpaper_cubie.o copied/hashrecorded assets/asset-hashes.json.
Private real bridge uses retainedowner for twoimages, G.Backdrop capture;
C host borrows real llvmpipe device, reads selectedactualtarget, compares with
software Desktop_Wallpaper.Paint.384style/DPI/rotation frames intended,216
initialchunks/twoallocations, no perframeuploads, cleanup asserts. Not PASSyet.
Sources/test/GPR frozen; poll94670, never restart livejob. No sharedoutputs,
production/index edits/nativeVM. Existingfontarchivefrozen reused for linkonly.
CurrentgoalPROGRESS implementation; physicalGPU/mainloop integration unfinished.

2026-10-02 retained wallpaper owner VERIFIED PRIVATE.
61523TERM0 bothassets72/144chunks,1000cachehits each/no upload, duplicate slot,
changed slot rejection, CPUcapture and pendingGPUframe close gates, fullrefund;
uncertainupload quarantine passes.39278TERM0 cleanselected owner SPARK proof
report obj/owner-proof-r3/gnatprove/gnatprove.out, no unproved/justified.
53597TERM1 after testsPASS: internal Advance lacked Available=>Resident summary;
strengthened contract, reran proof and executablecontracts tests (above).
All own jobs terminal. owner-verified.json hashes. No shared production/index
changes. Next realMesa sceneoracle using retainedowner+GPUBackdrop.Capture and
actual/synthetic immutableasset upload; then nativeadapter/fullDesktop routing.
Capture_Retired remains explicit caller attestation for CPUreferences; device
idle gate protects GPU readers. General slot reservations supplied by caller.
No automaticeviction/newheap; storage charged by existingD ledger.
Goal PROGRESS; actualhardware presentation/fullDesktop integration unfinished.

2026-10-02 retained wallpaper owner PRIVATE implementation.
Desktop_Backdrop_Owner limitedState explicit caller-reserved general slot128..135,
immutable asset identity; Fresh/ReadyToUpload/Uploading/Resident/Quarantine/Closed.
Acquire one allocation/chunk max, cachehit no pixelcopy; Poll onefence/nextchunk
max, Close requires CPUcapture retirement +healthyidle; pending/uncertain kept.
53597 LIVE private owner mock lifecycle tests then clean selectedSPARK proof,
/tmp/cubit-backdrop-owner-r2.log. Freeze owner/test/project inputs.
83441TERM1 Ada mixedlogicaloperators corrected; no proofchecksfirstattempt.
Tests normalwallpaper72/cubie144chunks+1000residenthits, duplicate slot, changed
identity rejection, CPUcapture and GPUframe Close gates, uncertainquarantine.
No new shared production/index edits or nativeVM. ActualMesa asset/render
oracle, Desktop routing and native integration still required. GoalPROGRESS.

LATEST30406 TERMINAL0 upload lifecycle all3scenarios PASS:72chunks,
720pending polls, busy backpressure, fullcoverage-only import/refund;
wrongshape cancelsbeforequeue; uncertaintransfer retainsbacking/refusesimport.
All own jobs terminal. upload-verified.json hashes source/testboundary.
Next bounded retained wallpaper asset owner, reuse D accounting/import,
then realMesa asset/pixeloracle and integrateDesktop; do not publishcandidate
as completed mainloop/GPUfeature. No shared production/index changes.

2026-10-02 wallpaper upload implementation PRIVATE /tmp/cubit-backdrop-capture-r1.
New Desktop_Backdrop_Pixels synchronous audited raw-memory boundary validates
plan/format/asset dimensions/mapping bytes before copying immutable atlas rows
into existing staging; padding untouched, no retained pointer/newpixelallocation.
Desktop_Backdrop_Upload SPARKStart borrows writer, copies one chunk, cancels
on rejection, submits once with producer-retired after synchronouscopy.
48003TERM0 fullatlas oracle464chunks bothimages/paddedrows+6no-writeguards PASS.
80274TERM0 proof mixed cache total600; superseded by clean96206TERM0
--subdirs=upload-proof-r2 report EXACT11checks7flow4prover,0unproved/justified,
1unit analyzed. Do not attribute cached600checks to newupload unit.
78034TERM4 test enum typo D.Failed corrected D.GPU_Failed.
30406 LIVE private lifecycle tests (72chunks/720pendingpolls/fullcoverage import,
wrongshape cancellation and uncertaintransfer retention), log
/tmp/cubit-backdrop-upload-tests-r2.log. Pollsamehandle,inputs frozen.
No own nativeVM/sharedlock/index changes. Retained asset owner and actualMesa
pixel rendering/mainloop integration remain; copy onlyinitialupload intended.
Current goal PROGRESS; no completion/hardwareperformance claim.

2026-10-02 shared wallpaper style PRIVATE verified.
70647TERM0 shared style: software384scaled/rotated/style cases and capture288
cases PASS, selected SPARK589checks197flow392prover none unproved/justified.
6404TERM0 independent scalar wallpaper1728clip/padding cases+8invalid guards
PASS, scratch1024 unchanged.28743TERM5 GPR inherited Source_Dirs omitted,
fixed privateGPR, no productionfailure. Logs /tmp/cubit-backdrop-shared-style-r1.log
and /tmp/cubit-backdrop-shared-strips-r2.log. All own jobs terminal.
New Desktop_Backdrop_Style centralizes dims/colors/placement; private existing
Desktop_Wallpaper spec/body and GPUchild use it. Candidate hash manifest
shared-style-verified.json. No shared production/index edits.
Next chunked immutable wallpaper upload/retained backing owner using existing
D.Begin_Write/Submit_Write/Poll_Upload; copy staticasset into mapped staging
only at initialupload, never perframe. Must gate Import_Backing on fullconfirmed
coverage, retain uncertain leases, then actualMesa pixeloracle and native
integration. Source dimensions currently2048x576 and2048x1152, general8slot
pool separatefromglyph128. No actual asset upload/GPUdesktop claim yet.
Goal progress; active. Never rerun successful straighticons publication.

2026-10-02 wallpaper capture70169 TERMINAL0: hosted288style/scale/rotation
cases PASS; missing image and fill-then-image overflow reject without submission.
Selected child SPARK proof closure588checks197flow391prover,0unproved/justified.
Report /tmp/cubit-backdrop-capture-r1/obj/gnatprove/gnatprove.out. Dependency
hash audit unchanged. All own jobs terminal; no shared production/index changes.
Child implementation/test/GPR remain PRIVATE pending real asset upload and
render integration. Next share style constants with software renderer, provide
immutable asset upload ownership, wire child into actual capture, then pixel
oracle/native tests. No new hardware/DPI pixel-quality/performance claim.
Current goal PROGRESS; fullgoal active. Straighticon publication alreadydone.

2026-10-02 wallpaper capture private candidate /tmp/cubit-backdrop-capture-r1.
New Desktop_GPU_Scene.Backdrop child captures physical background fill plus
existing Fill/Fit/Center wallpaper command,2layersmax, no pixels/copies/FFI.
Uses existing style colors and immutable2048x576/2048x1152 atlas dimensions.
Missing source/overflow invalidates whole capture; no partial background submit.
70169 LIVE private hosted regression then selected SPARK proof, log
/tmp/cubit-backdrop-capture-r3.log. Freeze privatechild/test/GPR inputs.
8396/52743 TERM1 contract syntax (limitedstate Old then unevaluated Old),
corrected scalar count snapshot with permitted unevaluated Old pragma.
Tests288style/DPI/rotation cases plus no-partial missing-source/overflow.
No shared production/index edits or nativeVM/buildlock. Existing wallpaper
CPU path unchanged; actual asset upload and mainloop routing still required.
Previous turn PROGRESS (straighticons published), current implementationPROGRESS.

2026-10-02 straight-alpha icons VERIFIED and PUBLISHED23files.
76017TERM0 mixed-output native Desktop: primary/scaling125-150%/arrangement/
standard interaction groups PASS; headlessPASS desktop-dual-output. OwnVM
1201590 stopped only after all observer groups passed; harness cleanupTERM0.
Source/binary hashes unchanged; guarded /tmp/cubit-publish-straight-icons-r1.py
SUCCESS (NEVER rerun). Root tests/compositor/build/straight-icons-published.json.
No index/staging/commit changes. No own livejobs/VMs. PhysicalGPU still not
integrated. Previous/current goal turns PROGRESS; goal remains active.
Next wallpaper/decorations/fallbackglyph routing plus full GPU capture and
client/display lifetime integration. Native150% capture is wallpaper-only:
functional cursor/geometry evidence, not a text-quality screenshot.

2026-10-02 DPI regression now LIVE76017 under shared build lock.
Exact log /tmp/cubit-straight-icons-dpi-r2.log, serial matching .serial.log;
CUBIT_TEST_{SCALING,PRIMARY,ARRANGEMENT,MIXED_OUTPUTS}=1, private Desktop
SHAecbda7f2. Poll76017, do not restart/edit its inputs. Latest prelaunch retry
had acquired lock but Nix cache was sandbox read-only (not lock contention);
escalated native invocation authorized and running. Previous turn PROGRESS;
current real test launched. Publication script remains NOT EXECUTED.

2026-10-02 continued icon integration: expanded DPI retry r2 did not start
(sharedlock busy; Servo release build noted). No own running handles/VMs.
Prepared private candidate.patch (23files), git apply --check PASS against
current shared sources, integration-notes.txt with ABI/proof boundaries.
Prepared /tmp/cubit-publish-straight-icons-r1.py, NOT EXECUTED: acquires lock,
requires all five expanded native PASS markers and unchanged source/binary
hashes before copying. Run only after successful dpi-r2; NEVER rerun once
manifest exists. No shared production/index edits. Next retry exact DPI command
from prior turn; no existing live test to poll. Goal active; turn PROGRESS
(reviewed patch/evidence and guarded integration artifact), no true blocker.

2026-10-02 straight-alpha candidate:39135TERM0 fullDesktop link;35102TERM0
native desktop-dual-output PASS (taskbar/split drag/per-monitor maximize/
wallpaper-cursor restoration/Settings).71991TERM0 submission SPARK74checks
(46flow28prover),none unproved/justified. Prior suite56724TERM0 unchanged.
Expanded DPI/primary/arrangement run attempted but sharedlock busy: NOJOB.
All own handles terminal, no own VM. Private integration-review.json records
23 candidate hashes and baseline conflict audit. Not published; no index edits.
Next acquire sharedlock for mixed-output125/150% regression with private
CUBIT_DESKTOP_IMAGE, then guarded publication. Current goal turn PROGRESS
(new terminal native/proof evidence); goal incomplete and active.

2026-10-02 native straight-alpha suite VERIFIED:56724TERM0 headlessPASS
softpipe, all384output/128affine/mask/batch/glyph-owner/Mesa-baseline tests PASS.
First17147TERM1 oldguardOver2nowvalid; fixtureusesinvalid3,checksretained.
OwnVMs stopped onlyafterappfinish (orfailedapp); no VM now.39135 LIVE actual
Desktop compile/bind/link+sourcehashguard underown sharedlock, privatecandidate
/tmp/cubit-straight-icons-r1/desktop-mesa-straight.svc. Need terminal thenDesktop
nativeinteraction/DPI beforepublication. No sharedsource/index/staging changes.
Previous/currentgoalPROGRESS. Privatehandoff updated.

2026-10-02 native probe RUNNING, ownsharedlock:17147 QEMU softpipe300s
/privateapp c436b20c hashchecked link31927TERM0. Logs /tmp/cubit-straight-icons-native*.
53408TERM0 facade82checks(43flow39prover),11faultcasesPASS;56819TERM0 final
fullsceneVulkan712queues/546816pixels/115straightlayers/validation0. All private
straight-icon sources frozen while native app runs; no shared source/indexedits.
Desktop link script prepared /tmp/cubit-straight-icons-link-desktop.py, notrun.
Prior/currentPROGRESS; nextnativeprobe outcome thenDesktop link/boot.

2026-10-02 UPDATE:1240 TERM0 final guardedsoftpipe nativeCcompile and
expanded384requestnativeprobe Ada compile/bind PASS. No link/execution yet.
All ownjobs terminal, noVM. Private /tmp/cubit-straight-icons-r1/handoff.txt
has lateststate. Next nativeprobe link/run +Desktop integration beforepublishing.
Current PROGRESS; no blocker. No shared straightalpha/index changes.

2026-10-02 private straight-alpha icon routing /tmp/cubit-straight-icons-r1.
GPUscene owner WAS published (10files), no pendingpublication there.
Own private main iconrouting/backend facade/affine+scene policy/nativeblend
adapters andoracles.24769 TERM0 selectedSPARK280checks allproved;81900 TERM0
realVulkan233472pixels/72newstraightcases,validation0;2597 TERM0 actualnative
main+backendcompileonly.61919 TERM0 nativeCcompile before finalindexguard;
finalguardcompile postponedlockbusy(nojob). New384requestnativeoracle prepared
butNOTcompiled/run. Need finalnativeC,probe+Desktop nativeintegration before
publishing. No shared straight-alpha changes or index/staging. Allownjobs
terminal, noVM; current/previousPROGRESS. Private handoff.txt detailscommands.

2026-10-02 Desktop_GPU_Scene VERIFIED/PUBLISHED (10newfiles).
Private /tmp/cubit-gpu-scene-r1.63894 TERM0 initialtests/proof;40533 TERM0
finaltests +selectedSPARK55checks (29flow26prover),0unproved/justified.
99124 TERM0 realhosted sceneowner,49152glyph/178176affinepixels,validation0,
8imagescleanup. Own snapshot+deduplicated glyph readers, one cold upload then
fresh capture, no partial submission;200glyphoccurrences/1reader test PASS.
No copies exported, pending/unknown retention, overflow/rejected shutdown tests.
All own jobs terminal. Root hashes match private; noindex/staging/nativeVM.
Next fullDesktop draw routing: wallpaper/icons/shadow/fallbackglyph still direct
CPU, existing facade lacks begin-scope; client producer/display handoff remains.
NormalDesktop software. Current/previous goal turns PROGRESS, no blocker.

2026-10-02 private Desktop_GPU_Scene /tmp/cubit-gpu-scene-r1.
Own new desktop_gpu_scene.ads/adb +tests only; complete snapshot and deduplicated
glyph readers, bounded cold-cache retry, no partial submission.63894 TERM0
initial mock lifecycle tests + selected SPARK proof. Final Close failure
quarantines stopped cache;40533 LIVE final tests/proof,99124 LIVE real Mesa
oracle. Private inputs frozen, shared sources unchanged; no native build/VM,
index/staging/commit changes. Fullscene mainloop/client imports still required.
Previous/current goal turns PROGRESS, no blocker. See private handoff.txt.

2026-10-02 GPU glyph residency VERIFIED/PUBLISHED. Private
/tmp/cubit-glyph-residency-r1.14004 TERM0 selected concrete proof387checks
(196flow191prover), none unproved/justified;72440 TERM0 defaultcache101checks.
24441 TERM0 final hostedreal fonts/cache8residentimages,49152glyph pixels,
178176affine,validation0,all8destroyed/refunded.32214 TERM0 trimmed uncertain
frame fixture; all assertions retained.128resident/pinned lifecycle +5faults
PASS; healthy-idle gate retains readers on uncertain frame.16files hashchecked
underlock; docs/evidence in tests/compositor/build/glyph-residency-*.
All own jobs terminal; no index/staging/commit changes. Current PROGRESS;
previous status turn yielded completed proof evidence. Next actual scene capture
and source/reader lifetime integration. Mainloopsoftware; no nativeGPU/HW claim.

2026-10-02 private glyph residency /tmp/cubit-glyph-residency-r1. Own new
Desktop_Glyph_Residency, reader-capacity/cache contracts and positive renderer
retirement query. Full128resident/pinned lifecycle/fault tests PASS. Uncertain
frame test97363 found logicallease release when Frame_Pendingfalse; fixed with
healthy-idle Can_Retire_Readers (nativeimageguard already retained resources).
14004 LIVE selectedResidency/Desktop proof after fixes;72440 LIVE defaultcache
proof;24441 LIVE finalrealVulkan cache rerun. Inputs FROZEN. Priorrealcache68246
TERM0 49152glyphpixels/178176affine/validation0,8images retired. No sharedsource
changes for residency yet; root typedglyph342check publication complete.
Current/priorPROGRESS; no index/staging. Mainloopsoftware,no nativeGPUclaim.

2026-10-02 real-font Vulkan +typed glyph binding VERIFIED/PUBLISHED.
Private /tmp/cubit-glyph-real-r1.53757 TERM0 frozen host Rust fonts build.
63567 TERM0 rawglyph realoracle;59541/39969 TERMfailure missingPixel_Format
operator visibility, fixed.43826 TERM0 finaltyped realfonts->exactownedstaging->
scene:2faces4DPI,8glyphs4allocs4rewrites,49152exactpixels,affine178176,
validation0.78827 TERM1 missing bind-resolve guarantee; GS.Bind contract strengthened with
proved uniqueness assertions,76630 revised proof completed; report build/glyph-bindings-proof.out.
Eight mockglyph cases check premature/wrongslot/density/duplicate/stale binding.
11files hashpublished. Nativepolicy singleton nowowns glyph associations and
Capture_Glyph; face/code are producer-attested raster metadata. No actual
nativeGPU/display/perfclaim. Captures /tmp/cubit-glyph-real-r1/captures,
glyph-dpi-sheet.png inspected. No index/staging changes. CurrentPROGRESS.
Next bounded glyph residency/eviction and fullDesktop scene traversal integration.

2026-10-02 padded upload/direct glyph producer VERIFIED/PUBLISHED.16files
hashchecked underlock, build/strided-upload-* and direct-glyph-upload-* evidence.
Private /tmp/cubit-strided-upload-r1.33980 initialfixture TERM1 wrongtargetsize;
80058 TERM0 full136strided+2160chunk/21600pending+SPARK243checks(146flow97prover),
0unproved.2608 TERM0 realhostVulkan padded rows poisoned:60chunks6144pixels,
affine178176,validation0.52899 TERM0 initial8glyphscenarios+proof13checks;
10601 TERM0 stronger8glyph scenarios +14defaultwriterfaults.30981 final glyph
checks/proof completed; explicit pitch/offset/fullspan checks, audited unchanged
fontFFI body/spec nowcontracted. No realfontGPU/nativeGPU/performanceclaim.
Caller still must poll completion, import, bindglyph, capturefullscene. Sources
now own136 under140entrybudget and139children from priorcapacitypublication.
Allownjobs terminal atpublication. No index/staging changes. CurrentPROGRESS.

2026-10-02 full source capacity VERIFIED/PUBLISHED underlock. Private
/tmp/cubit-source-capacity-r1; combined publication supersedes unrun standalone
storage publication script (DO NOT RUN /tmp/cubit-publish-storage-capacity-r1.py).
47225 TERM0 Desktop/context/image-owner SPARK326checks(169flow157prover),0unproved.
60510 TERM0 owners+14writer+9backing+6external regressions.61298 TERM0 sanitized
C136metadata bounds and actual hosted Desktop llvmpipe6144pixels/affine178176,
validation0. Fullmock136owned:128masks+8BGRA, exact140allocationbudget, slot
role exclusion/reuse. Sourceprovider stays136; backing136, ledger140, child139.
Ledger generic Slot_Count CPUdefault8, exact sum/refund proved146checks prior.
No nativeGPU/timing claim; mainloopsoftware. Current/priorPROGRESS. Allownjobs
terminal atpublication; no index/staging changes. Next glyph staging stride,
source residency and full scene integration, displayhandoff/hardwaretiming.

2026-10-02 actual Desktop singleton REAL VULKAN PASS/PUBLISHED. Private
/tmp/cubit-desktop-real-upload-r1.65502 TERM1 test C adapter prototypes wrong;
83078/51895 TERM1 pixeloracle version2 staleversion0: missing caller output
damage, diagnosed explicitly. Bridge now marks changed quad;30033 TERM0 BGRA.
35075/19866 TERM0 final BGRA/R8:40chunks,384B staging,8contents,4native source
allocs+4same-image rewrites,6144exact pixels, final source/target/context release
and budgetrefund. Actual Desktop/Device_FFI/Mesa_Service and real Vulkan; ONLY
service admission mocked with borrowed host device. Intercepted dispatch forwards
all operations, counts source lifetime and identifies recorded framebuffer.
Affine178176pixels passes,validation0errors.7testfiles hashpublished underlock;
build/desktop-real-upload-*. No production change from pixeltest. Prior/current
PROGRESS. Allownjobs terminal/noVM. Root writer fullnative link still UNVERIFIED:
49920 staleMesa adapter;42015 postlink runtime logging mutation; later retry
flock failed(nojob). See /tmp/cubit-desktop-writer-r1/link-followup.txt. Next
stable fullnative link, complete scene/client integration +displayhandoff;
mainloop remainssoftware,no nativeGPU/HWperformanceclaim. No index/staging edits.

2026-10-02 Desktop writer policy VERIFIED/PUBLISHED.95847 TERM0 actual
SPARK210checks(129flow81prover),0unproved/justified. Strong invariant single
active source, writing=>idle, transferring=>matchingpending; completed phase
contract strengthened.26708 TERM0 14lifecycle/fault scenarios +nativecompile;
93950 TERM0 final existing lifecycle regressions. CPU writer blocks descriptors,
frames and retirement; chunk completions yield to frames. Ownedslots0..4 now
completion-gated Import_Backing; trusted externalimports>=5.10files published
underlock/hashchecked; build/desktop-writer-* evidence. Native helper adds
upload_record C and required scheduler symbols. No index/staging changes.
Previous/current PROGRESS. Allownjobs terminal. Next fullnative link, then
realVulkan singleton test (existing hosted bridge tests components only), full
scene/client integration and presentation. Mainloop software, noHW/timingclaim.

2026-10-02 private writer integration PROGRESS, not published. Snapshot
/tmp/cubit-desktop-writer-r1 adds per-backing coverage, single CPU writer token,
transfer-vs-frame polling, protected retirement, owned-slot Import_Backing gate.
Raw external imports move to slots>=5; 0..4 reserved. Runtime lifetime tests
24792 initial13PASS; proof r2 failed four strengthened-invariant checks incl
real descriptor-release overlap while CPU writer active. Fixed release gate,
Fresh=>Available invariant, exact completed-phase contract and capacity guard.
95847 LIVE revised proof-r3 (same handle, do not restart/edit inputs).26708 TERM0
14writer scenarios incl interleaved frames/poll separation and uncertain
release exclusion +native Ada compile.68033 TERM0 separate private regression
snapshot: startup/backing/targets/frames/sources/pipeline/upload cases pass;
updated source/backing tests copied back to writer snapshot. Source guard fixes
in writer snapshot newer than that regression snapshot. No native full link or
real-Vulkan Desktop-scheduler execution yet. Native builder must add
vulkan_upload_record C object before publishing/linking. All other own jobs
terminal. Current/prior PROGRESS. Shared source/index/staging unchanged.

2026-10-02 private writer integration /tmp/cubit-desktop-writer-r1. Own Desktop singleton/API, upload policy integration and tests. No shared source changes until verification. Previous turn PROGRESS.

2026-10-02 native upload-linked Desktop +fallback VERIFIED.42917 TERM1
before link: saved bundle runtime hash stale.83020 TERM0 rebuilt NEW
 desktop-upload-service-bundle-r1 against current runtime (allguardsenabled),
linked desktop-upload-link-r2; required configure/release/uploadmetadata/bind
symbols verified.ELF60a396fae31efaacbadfda2c7cf60bfd213818de181e6319a5821429f837d3b9.
34174 TERM0 nativeQEMU --approve-render fallback: fresh software incarnation,
three keyboard/menu exactrestoration cycles PASS. Frozenkernel/service seeds
recorded; NO nativeGPUupload execution. Allownjobs terminal/noVM/lock. Logs
build/desktop-upload-{link-r1,link-r2,fallback-r1}. Next global writer/pending
purpose/completion-gated importer; mainloop software. Current/priorPROGRESS.

2026-10-02 Desktop upload admission VERIFIED/PUBLISHED. Own singleton now
owns U.State and Configure_Upload/Release_Upload with same aggregate Budget;
metadata Device_Upload_FFI constructs one private buffer on admitted device.
No mapping exposed yet; future writer API MUST extend retirement gate. Stop
retains pending/uncertain/health-lost dependencies.3090 TERM0 eleven scenarios
+actual selectedSPARK152checks(93flow59prover),0unproved/justified.4680 TERM0
native Desktop/FFI compile.78300 TERM0 existing startup/backing/target/frame/
source/pipeline regressions.29619 TERM1 AFTER sanitizer metadata PASS due C
harness pointer typo; corrected57748 TERM0 Vulkan32chunked sources,24576texture/
82944scene pixels,0validationerrors.12files hashpublished underlock incl native
builder C dependency; build/desktop-upload-*. Prior/current PROGRESS. No staging/
index changes. Native full link next; no live Desktop upload/draw or HW claim.

2026-10-02 own private Desktop upload-admission integration at /tmp/cubit-desktop-upload-r1. Scope device_storage C/header, new Device_Upload_FFI, Desktop singleton upload ownership, constructor/lifetime tests and native builder dependencies. No shared source writes until verified publication under lock. Previous turn PROGRESS.

2026-10-02 upload coverage VERIFIED/PUBLISHED. New Compositor_Upload_Progress
tracks confirmed rows with backing identity/nonwrapping chunk tickets; stale or
duplicate observations ignored, uncertainty quarantined, publication iff complete.
Same-backing content rewrite preserves sequence. Hosted bridge gates mapping,
retirement and descriptor publication; 384B staging forces chunked BGRA/R8.
Earlier34279 tests passed but proof ZERO checks (missing SPARK_Mode on instance),
NOT proof evidence. Fixed instance;71271 TERM0 actual23checks(16flow7prover),
all17entities analyzed, zero unproved/justified.42570 TERM0 native runtime compile.
Existing17107 TERM0 Vulkan32contents(16resizes+16same-backing),24576texture/
82944scene pixels,0validation errors;2160chunks/21600pending policy tests pass.
Ninefiles hash-checked underlock; build/upload-progress-published.json, proof-r2
and native-r1 logs copied. No index/staging/commit changes. Prior status-only
turn NO PROGRESS; current proof/native/publish PROGRESS. No own livejobs/VM.
Next Desktop admitted staging, global writer, upload-vs-frame purpose and importer
coverage gate. Hosted bridge only; mainloop software, no HW/latency claim.

2026-10-02 checked transfer recorder VERIFIED/PUBLISHED. New Compositor_Upload
geometry (rect/stride/alignment/span +fullwidthrowchunks), Upload_Record_FFI +
native C recorder and SPARK Upload_Recording; existingV.Seal_Transfer supports
nonrenderpass batch after>=1admitted operation. Guardmatchingheldparents/live
resources/freeassigneddescriptor/planfits; chargeboundedattempt, badrecord
invalidatesbatch forcancel. Discard/layout, producercompletion, matchingprivate
slot/noaliases remain trusted; partialcoldcontent NOTpublishable bythisAPI.
68336 TERM0 380160addresscases/4Ktiles/extremes +recording faulttests; selected
SPARK111checks0unproved/justified.85797 TERM0 ASanUBSan3positivebarrier/copy
sequences+40precommandrejections and Vulkan.80456 TERM0 real llvmpipe BGRA/R8
32resized CPUuploads via actualgeometry/recorder/SealTransfer/Submit/Poll;
24576texture/82944scene pixels, stagingretention/refund,0validationerrors.
16898 TERM0 nativeAda+Ccompile +existing1000submissionlifecycles/10000polls/
frame/source/damage regressions.20files published underlock andhashchecked:
build/upload-record-published.json. Allownjobs terminal/noVM/lock/index/staging
changes. Prior/currentPROGRESS. Next Desktop stagingconstructor/admission,
separate upload-vs-frame pendingpurpose, protectedmappedwrite and confirmedrow
coverage before descriptorpublication. Main drawingloop stillsoftware; no
nativeGPUupload/display/physicaltiming claim. Do not rerun publication script.

2026-10-02 accounted upload buffer VERIFIED/PUBLISHED. New Upload_Owner/FFI
and native upload_buffer C/header: contextchild before preparation, real buffer
requirement charged before dedicated coherent host-visible allocation/map;
capacity<=16MiB, noqueue/wait/pixelcopy insideboundary. Mapping presence NOT
writepermission duringGPUuse. Unknown retainschild/charge, cleanreleaseunmaps
andrefunds. GPUAccounting Extra_Slot=>True gives9 entries under SAME byte limit;
defaultCPU8 unchanged. Initial85540 TERM1 caughtTotalstill8, fixed privately.
27328 TERM0 13uploadcases+32reuses with8heldallocs +image/4096CPUstorage cycles;
selectedSPARK93checks0unproved/justified.97531 TERM0 ASanUBSan19Cpaths.
43071 TERM0 realllvmpipe32 CPU-written mapped sources +buffertoimage sampled
render24576texture/82944scenepixels, pendingstagingretention, finalrefund,
0validationerrors.15178 TERM0 nativeAda+Ccompile and CPUledger37proofchecks;
97213 TERM0 nineDesktopbackingcases on9slots. All18files published underlock
with build/upload-published.json hashes; headercomments clarifiedafterward.
Logs build/upload-*. Current/priorPROGRESS; allownjobs terminal/noVM/lock/index/
stagingchanges. Next device stagingmetadata +Desktopownership/admission and
production bounded transfer recorder/completion-controlled writes. Hostmanual
transfer/waits are fixture only; no nativeupload/drawing/display/perfclaim.

2026-10-02 Desktop backing allocation INTEGRATED/VERIFIED. DeviceStorage
five private BGRA/R8 source metadata records + new Device_Source_FFI; requires
SPARK Fresh/Closed serialized slot, native nohandle/nooccupiedview, matching
admitted device memory mask. Constructor no allocation/upload. Desktop
Backings[0..4] context-aware owners share existing 8-entry budget with3targets;
Allocate_Backing guardedhealthy/idle/notstopping/readytargets/pipeline/freeview,
generation lease; Release_Backing stale/pending/view/health checks, Stopattempts
unimported source retirement first. No source pointer exported by singleton.
97054 TERM0 ASanUBSanmetadata matrix;17010 TERM4 Loop_Entry syntax fixed;
26570 TERM0 ninebacking+sixsourcecases and selectedSPARK105checks0unproved/
justified.37153 TERM0 actualmetadata/AdaFFI Vulkan32resizelifetimes24576texture/
82944scenepixels0validationerrors.20600 TERM0 nativeLINK desktop-backing-r1 SHA
fe083b96e252d716f6d323e4a049de8518c61fa51c1a2b7c63afeb8b992a64c7;
nm confirms newallocation/release/constructor/sourceowner symbols. Mainloop
still not invoking allocation or GPU draw. Docs desktop-optional-render.md.
Allownjobs terminal/noVM/lock/index/stagingchanges. Prior/currentPROGRESS.
Next bounded upload +layout/import integration: full5sources+3targets exhaust
8ledger slots, so staging admission must be explicitly reserved/revised, never
unaccounted. Complete native scenes/display handoff/hardwarelatency stillopen.

2026-10-02 source metadata integration work: own device_storage C/header,
new Vulkan_Device_Source_FFI ads/adb, source metadata test and hostbridge.
Five private backing records share the existing 8-slot allocation budget with
three targets; no raw client pointer import. Callers must be Fresh/Closed and
serialized before metadata reset. No GPU allocation occurs in constructor.
Previous turn PROGRESS; continuing bounded Desktop backing integration.

2026-10-02 context-aware sampled backing VERIFIED/PUBLISHED. New
Vulkan_Owned_Source ads/adb reserves context child before image prepare/allocate;
clean rollback retires, unknown prepare/bind/release keeps child/charge; closed
reuse via Rearm, no ledger/context-generation reset. Hosted nine failure cases,
foreign context/held-reader rejection, unimported context retention, 32 reuse
cycles and other-child preservation PASS.63268 TERM0 initial proof;87187 TERM0
stronger selected proof13 (7flow/6prover), zero unproved/justified.77432 TERM0
native Alire/runtime object compile.77616 TERM0 actual llvmpipe +15233 TERM0
final strengthened-contract regression:24576texture/82944scene pixels,
32resizedlifetimes, pending descriptor release blocked, finalbudgetrefund,
0validationerrors. Logs build/source-parent-*. Allseven files published under
lock with hashes build/source-parent-published.json; main/private rechecked.
Earlier pending image-rearm fixture now superseded/published with sourceowner;
do NOT replay old /tmp/cubit-rearm-publish.py or source-parent-publication.py.
Allownjobs terminal/noVM/lock/index/stagingchanges. Prior/currentPROGRESS.
Next singleton sampledmetadata/allocation/boundedupload integration; new owner
still not called by DesktopStartup. Devicehealth, private matchingrequest and
full upload/draw/descriptor retirement are caller obligations, nohardwareclaim.

2026-10-02 image owner reuse VERIFIED:73769 terminal0 owner regression +
selected SPARK61checks0unproved/justified, build/image-rearm-r1.log. Main
Vulkan_Image_Owner.Rearm onlyClosed+noncurrentlease, no ledger/identity reset;
32cycles alongsideheldallocation, staleidentity/live/prepared/quarantine gates.
85980 terminal0 real llvmpipe private /tmp/cubit-image-rearm-vulkan-r1:
32resizedsampled lifetimes24576texturedpixels/82944scenepixels, deferred release
anddescriptor/backing/finalledgerrefundPASS, 0validationerrors. Production
owner hashes identical private/main. Sharedlock prevented hostfixture
publication (60649 boundedwait terminal1; subsequent nonblocking attempt1).
Pending tested two-file delta build/image-rearm-host.patch plus source hashes
build/image-rearm-sources.json. Apply under lock after hash check; do not rerun
non-idempotent /tmp/cubit-rearm-host.py blindly. Docs record exact boundary.
Allownjobs terminal/noVM/lock/index/stagingchanges. CurrentPROGRESS. Next publish
fixture then production sampled-image metadata/backing/upload integration;
Rearm alone does not reset native requests or provide upload/display authority.

2026-10-02 owner reuse work: own Vulkan_Image_Owner ads/adb and owner tests.
Adding guarded Rearm only after Closed and non-current ledger lease; ledger
never reset, native request reset explicitly NOT supplied by this API. Tests
exercise 32 reuses alongside a held allocation and reject live/prepared/unknown
states. No native/driver/UI edits. Previous status-only user checklist was no
progress; current implementation and verification are next safe action.

# Compositor backend effort

2026-10-02 actualnewprovider sampled source VERIFIED:80836 TERMINAL0
build/owned-source-rendering-r1.log. Hostbridge allocatesowned sampledimage via
SPARKImageOwner/sharedtargetbudget, GPUclear+shader-readablebarrier, actual
DevicePipelineFFI.Source_Request +managedImport, capturedtexturedscene.768exact
sampledoutputpixels, pendingreleaseblocked/nativebackingretained, completed
viewthenbackingrelease, finalsharedbudgetrefundPASS. Full59136scenepixels+
2304RGB+178176affine/0validationerrors. Ownhostbridge ads/adb/GPR+affineoracle
updated; productioncodeunchangedthisturn. NoCuBitadmission/CPUupload/display/
latencyclaim. Nextproduction sourcebacking+boundedupload lifecycle (including
resize/reuse, stale-source generation andsharedbudget), thenUIcapture/handoff.
Allownjobs terminal/noVM/lock/index/stagingchanges. Prior/currentPROGRESS.

2026-10-02 owned-image source constructor VERIFIED: Cdevice_storage constructs
fixedprovider request onlyforliveprivate ownedBGRA/R8 sampledimage, matching
instance/physical/device/dispatch, no targetimage/memoryalias, validextent/slot,
livepipeline/targets; occupiedslotrejectionpreservesrequest. SPARKDesktop
Import_Owned_Source guardsidle/healthy/livepipeline/openadmission/freeSlot before
metadata thenexistingmanagedImport.69366 TERM0 ASanUBSanmockmetadataPASS;
86994 TERM0 sixsource-lifetimecasesnowthroughconstructorwrapperPASS;90451 TERM0
SPARK83checks0unproved/justified;60633 TERM0 nativeLINK desktop-owned-sources-r1.
NoCPUaddressimport, noexternalcapabilityauthority, sourcebacking/layoutretained
callerobligations. Nextsourceallocation+boundedupload andrealnewprovider sampled
pixeloracle; no actualnative source/texture/presentation claim. Allownjobs
terminal/noVM/lock/index/stagingchanges. Previous/currentPROGRESS; goalACTIVE.

2026-10-02 Desktop managedsource wrappers VERIFIED: Import_Source requires
readydevice/livepipeline/openadmission/idle, pendingbusy noFFI; Release_Source
allowedafterStop butonlyhealthydevice/idle, nullkey NEVERbacks-releasepermission.
Source_Held tracksretainedregistration nothealth; uncertainty preservespipeline/
context viaquarantinedsubmission.65312 TERM0 sixsourcecasesPASS inclpending,
shutdownrelease, clean/unknownimport, unknownrelease, healthloss, stalegeneration
reuse+capturedoldscenerejectedwithoutsubmission.42189 TERM0 SPARK77checks0unproved/
justified.51451 TERM0 nativeLINK desktop-sources-r1. NewAPIsnotyetcalledfromnative
Desktoploop; sourceproviderrequestmetadata/backing construction stillneeded,
noCPU-pointer-as-texture orcrossprocessauthorityclaim. Next connectowned sampled
image/upload/import provider andcompletecapturedscene. Allownjobs terminal,
noVM/lock/index/stagingchanges. Prior/currentPROGRESS; goalACTIVE.

Graphics/Display follow-up contract remains docs/compositor-shared-targets.md:
currentprivateVkImages have NOscanoutauthority. Need negotiatedrender-write+
scanout identity/epoch/fence/latch/retirement tobindGPUtargets withoutreadbackcopy.
No newdriverABI orcapabilitynumbersinventedhere; compositor cancontinueowned
sourcework whilethatindependentintegrationadvances.

2026-10-02 existingtexturepipeline ownership INTEGRATED/VERIFIED: Desktop
Prepare_Pipeline reservescontextchild beforeexistingaffineengine+136descriptor
provider creation; cleanfailure retireschild, uncertainty retains; Stoprequires
GPU/sourcequiescence, failedhealth skipsrelease. NoMesa/Vulkanrewrite orpixel
upload/importauthority.71105 TERM0 fivepipeline +sixframecasesPASS inclpending/
health retention;92065 TERM0 selectedSPARK68checks0unproved/justified.47494 TERM0
realVulkan Cpipeline create/close +repeatrejection +targetpixeloraclePASS0validation
errors (newprovider NOTsampled yet).7619 TERM0 nativeLINK desktop-pipeline-r1,
--prepare-pipeline requirespreparetargets;6675 TERM0 nativefallback/menuPASS,
no pipelinecreation withoutreadytargets. Audit: facadeCPUimageinputs needGPU
sources and Complete_Output mutationcontract; privateGPUimages stillNOTDisplay
scanouttargets. Nextsource/backing/captureconnection, nofakeCPUhandleimports.
Descriptor/pipelineallocations outsideimageledger, fixedobjectcountsnotRAMclaim.
Allownjobs terminal/noVM/lock/index/stagingchanges. Prior/currentPROGRESS.

2026-10-02 boundedDesktopframe owner VERIFIED: Render/Poll_Frame/Damage_Output
useprivatepool/damage/context, onependingGPU, deferredbusyrequests, onepoll/call,
lateinputdamage preserved. Stopsetsstickyadmissionstop, dropsonlyunpublished
readycandidate, pending/uncertainretained; pollingcanfinishbeforesubsequentStop.
12310/92250 TERMINAL0 sixframecases +oldstartup/eighttargetcasesPASS;100busy
resubmissionsnoextrastart/submit +100pendingpollsexactonequeryeach.21756/1122
initialsyntaxTERM;81166 TERM1 two missingdamagecontractfacts, strengthened
Frame.Begin_Record and Scene_Record withoutweakeningchecks.83646 TERMINAL0
freshselectedproof168checks0unproved/justified.18445 TERM0 realnativeLINK
build/desktop-frame-owner-r1;99023 TERM0 nativefallback boot/targets-skipped/
3menus exactrestore PASS build/desktop-frame-owner-fallback-r1. Frame API not
yetcalledbyDesktopdrawingloop; nohardwareframes/displayhandoff/latencyclaim.
Nextactualscene-capture Render/Pollhook, display-readyleasehandoff remains.
Allownjobs terminal/noVM/lock/index/stagingchanges. Prior/currentPROGRESS.

2026-10-02 first-use layout IMPLEMENTED/VERIFIED: damageInitialized bits only
warmonCompleted; cancelled/unknownretain. Owned Prepare_Frame guardscontext/
epoch/writer/recording +coldfullpaint; sceneRecordcallsbeforepass, rejectedprep
cancelscommand. CbarrierrecordsUNDEFINEDorretainedCOLOR, noqueue/wait/pixelcopy.
53316 TERMINAL0 selectedSPARK116checks0unproved/justified.26372 TERM1 genuine
partialassertcaughtfullpendingdamage;3settlingframesfixfixture.48619 TERM0 real
Vulkan cancelledcoldretry +2304RGB +58368fillpixels/partial +178176affinePASS
0validationerrors.53882 TERM0 owner/recordfaultmatrix inclpreparefailurePASS.
61603 TERM0 ASanUBSan6barriers+66no-recordrejectionsPASS.14937 TERM0 native
DesktopLINK build/desktop-initial-layout-r1. NoDesktopGPUsubmitloop/hardware/
latencyclaim. Document target-context-lifetime.md; nextintegrateboundedframe
submit/poll intoDesktopowner, then scene capture/presentation. Allownjobs
terminal/noVM/lock/index/stagingchanges. Previous/currentPROGRESS, goalACTIVE.

2026-10-02 real hosted device-target path VERIFIED:12756 TERMINAL0
build/device-target-rendering-r1.log; oracle nowusesactualdevice_storage context
request +actualAda Vulkan_Device_Targets_FFI +existingSPARKtargetowner, nohandmade
imagerequests/mask.3allocations2304RGBpixels +72fillscenes55296pixels +surrounding
178176affinepixels PASS/0validationerrors. Contextcloseretention+heldfront+final
refundPASS. Test-onlymetadataintrospection in targetbundlebridge, GPRincludes
actualstorageC/FFI. NoMesaServiceIPC/nativeadmission/Intel/latencyclaim.
Nextproductionframegap confirmed: firstimageUNDEFINED->COLORtransition still
suppliedbyhostoracle; Desktop needsproved first-use/discard-vs-retained handling
including cancelledrecordings before GPUsubmissions. Existing scene spec already
requirescaller imageprep, so allocationhook doesnotimplyrenderready. Allownjobs
terminal/noVM/lock/index/stagingchanges. Previous/currentPROGRESS; goalACTIVE.

2026-10-02 native target hook VERIFIED fallback: new Vulkan_Device_Targets_FFI
realAda/C recordbridge discardsdirtyfailureoutputs; SPARK Configure_Targets
checksready beforemetadata, callsboundedowner, consumesfailedattempt.20759
TERMINAL0 startup+8targetscenariosPASS;15993 TERMINAL0 configure-proof37checks
0unproved/justified.45524 TERMINAL0 realnativeLINK desktop-configure-targets-r1
SHA d0ec7c169c54357d8bfde44fa6c9a831566d270515ab1c47d66ee9cd74e55b1a.
Builder --prepare-targets requiresadmittedstartup, usesvalidatedoutputdims,
64MiBimagebudget, logsready/charged/limit; softwaredrawingretained.16484
TERMINAL0 native approvedunavailablefallback desktop-configure-targets-fallback-r1
PASS emptyauthority, targetsskipped, freshsoftwarechild,3menu restorationcycles.
Hardwaretargetallocation UNEXECUTED; 64MiBfixturelimit cannotfit3full4Ktargets,
notproductionadmissionpolicy. Existingr2startupcandidateunchangedforgraphics.
Nextactualdevice-metadata+targetrenderhostoracle; realframeinitialization,
scene/framepipeline/presentation remainneeded. Allownjobs terminal/noVM/lock/
index/stagingchanges. Prior/currentPROGRESS, goalACTIVE.

2026-10-02 budget invariant +device metadata VERIFIED:99168 TERMINAL0 fresh
budget-proof-r2 selected image/targets/Desktop144checks0unproved/justified;
76522 priorTERM1 AdaOldconditional syntax corrected.31146 TERMINAL0 sixfault
caseswithcontractsPASS. Configured_Limit preservescallerbudget throughalltarget
operations; chargedactualimages only, NOTtotalMesa/processRAM. Addednarrow
vulkan_device_storage.h/.c metadata preparer (sameadmittedview/livecontext,
3distinctstaticrequests, excludesprotected/lazy, <=16384axes, onepublication).
63767 TERMINAL0 ASan+UBSan private device-target-metadata-dj69nmww testsPASS.
Sharedlockbusy initially; finalpublication exacttestedbytes/hashchecked against
unchangedoldroot underlock. Native Ada metadata wrapper/outputdimensionhook
stillpending; noactualnativeallocation/render/scanoutclaim. NoVM/ownjob/lock/
index/stagingchanges. Previous/currentPROGRESS; goalACTIVE.

2026-10-02 Desktop target owner INTEGRATED/VERIFIED: singleton now owns first
output target set +pool +device ledger. Prepare_Targets refuses beforeReady,
oneattempt/noledgerreset, threeactualallocations; Stop retireschildren before
device, skipschildFFIafterfailedhealth.77905 TERMINAL0 oldstartup +6freshprocess
faultcasesPASS: clean, lowbudget, uncertainbind, uncertainrelease, software,
healthloss.55314 TERMINAL0 selectedsingleton SPARK26checks0unproved/justified.
47231 TERMINAL0 native fullDesktopLINK build/desktop-target-owner-r1; addedreal
target/image/binding Cobjects toprivatebuilder. DefaultDesktopunchanged; native
caller stilldoesNOTpreparetargets orsubmitGPUframes. Next: admitteddevice
metadataadapter +dimensionhook; strengthen aggregate contracts to explicitly
preserve budgetLimit (ledgerValid alreadyproved; callerlimit guarantee needs
propagating leaf contracts). Multioutput/resize/presentation remainpending.
Allownjobs terminal/noVM/lock/index/staging changes. Previous/currentPROGRESS.

2026-10-02 admitted Desktop candidate VERIFIED fallback:61463 TERMINAL0
fresh startup-final SPARK456checks0unproved/justified; hosted singleton PASS.
90563 firstnativeLINKPASS;22766 bootTERM1 diagnostic enumImage numeric1 (guest
reachedsoftwareDesktop). Explicitphase labels fix;67558 TERMINAL0 finalLINK
build/desktop-admitted-startup-r2 SHA f56a53344cd9144208b01b0cf7d77a58766f37255c00d9b887c3c7c822e202e6.
25020 TERMINAL0 approved-unavailable nativeboot
build/desktop-admitted-startup-fallback-r2: freshsoftware incarnation, empty
render slot, actualsingleton startupSOFTWARE +3keyboardmenu restorationcycles.
RealadmittedMesa branch linked but NOT executed; softwaredrawing stillactive.
Candidate available forgraphics hardwarestartup testing; fixtureauthorityslot62
and rawoptionalmetadata are testonly, no newCCLsyntax. Allownhandles terminal,
noVM/lock/index/defaultstaging changes. GoalACTIVE; currentPROGRESS.

2026-10-02 admitted Desktop startup integration IN PROGRESS: strengthened
Vulkan_Device_Owner.Close contract to preserve nonfresh lifetime (no runtime
change), Desktop singleton hosted startup/health/retirement PASS. Fresh proof
61463 LIVE startup-final; initial71517 TERM1 was malformed prover option only.
Private real Mesa startup link90563 LIVE desktop-admitted-startup-r1 under
shared build lock. New opt-in path inspects admitted endpoint, starts Mesa and
context once, retains software drawing, checks health once before event loop,
and attempts retirement on normal exit. Boot runner now explicitly requires
software branch and rejects hardware-start markers in unavailable fixtures.
Typed production manifest migration preserved; no new syntax, no index/default
staging changes. Previous status-only turn NO PROGRESS, current source+test
work PROGRESS. Hardware admission/render/presentation remain unverified.

2026-10-02 devicehealth gate IMPLEMENTED/VERIFIED: Vulkan_Device_Owner.Check_Health
calls existingMesaService.Health throughtrustedFFI onlywhenReady; failedstatus
stickyquarantines withoutrelease/retry. No healthIPC insoftware/fresh/retiring/
retired/quarantine.79427 TERM0 freshhealth-proof11unitchecks0unproved/justified;
49854 TERM0 retainedchildren +pendingGPU +laterhealthyrefusal +oldstartupmatrix;
18872 TERM0 actualnativeadapter/controller compile vulkan-device-native-5el56onj
rootdrift[]. DefaultDesktopnotwired; nohardwareloss/latencyclaim. NativehealthIPC
canblock, scheduleoutsideinputdispatch, neverperprimitive. Allownjobs terminal,
noVM/lock/index/stagingchanges. Prior/currentgoalturn PROGRESS; goalACTIVE.
Graphics endpointdisposal coordinationrequest received; noauthority inferred
toeditsharedABI/bootstrap, no newmanifestkeywords. Typedoptionalrender and
successfulhardware-admitted Desktop remainnext integration dependencies.

2026-10-02 fullDesktopoptional recovery VERIFIED88916 TERMINAL0:
desktop-optional-render-fallback-r1 result/serial hashesPASS; failedsuspended
GPUattempt4294967328 neverresumed, freshsoftware8589934624 resumedemptySlot62,
newDeviceOwnersoftware/retirementmarker +3menuopen/exactrestorecyclesPASS.
17645unapprovedbranchPASS remains. Explicitoldbootseeds, no currentCCLbuild or
successfulGPUadmission/hardwareclaim. Docs desktop-optional-render.md records
wirefixture vsfuturetypedproductionmanifest. Allownjobs terminal/noVM/lock/
staging/indexchanges. Prior/currentgoalturn PROGRESS; goalACTIVE.

2026-10-02 optionalDesktop92347 TERMINAL0 native linkd93708ba, rebuilt Mesa
desktop-optional-service-bundle-r1 afterlibcchange; original/modified/finalELF
capsverified.17645 TERMINAL0 unapprovednativeboot desktop-optional-render-boot-r1
procmgrsoftware-only +emptyrender-slot +newownerstartup +3menucyclesPASS.
88916 currentlyLIVE approved-but-unavailable fixture; requires2distinctchild
incarnations FALSE/TRUE, denial+freshretry, oneDesktopprobe andinteraction.
No sharedlock/normalDesktopstaging/indexchanges. CCLcoordinationupdate viaServo:
typedmanifestrevamp; supersedeearlierliteral request-optional-render suggestion.
Need typed optionalrender requirement encoding, NOT newmagic-keywordsyntax.
Ourrawwirefixture keepsABIpolicycoverage separatefromproductionCCLschema.

2026-10-02 fullDesktop optional-render fixtureINPROGRESS: --optional-render-probe
requiresstartup-probe, appendscanonicaltype11rights3slot62param0=1param1=0 to
private manifestonly, retainsalloldrequests, verifieslinkedcaps, injectsnative
selfinspectempty-slot beforeprovedslot0startup. NO CCLcompiler/defaultmanifest
edits, no ad hoc admission orGPUactivation. tests/render-startup helper tests
preservation/invalidmetadata/duplicate-render/collision. First48399 TERM1 stale
bundlelibc.a detectedbeforebuild;92347 LIVE sharedlock refreshesbundle+native
Desktoplink r2. Bootwillrequireprocmgrsoftware-onlyadmitted +empty-slotmarker+
menuinteraction. CCLoptionalspelling stillneeded forproduction; rawwirefixture
isexplicitnativeintegrationcoverage. Prior/currentgoalturn PROGRESS.

2026-10-02 finalnative43533 TERMINAL0 native-scene-8fpgc8hh archive build and
allroot/snapshotinputhash checksPASS. Exactcurrenttarget/contextpolicy and C
contextboundary included. Allownjobs terminal/noVM/lock/index/stagingchanges.
See target-context-lifetime.md for51proofchecks +realVulkanpixel/retirementgate.
NormalDesktopGPUstartup/frame/presentation wiring stillpending; goalACTIVE.

2026-10-02 target/context lifetime INTEGRATED: Vulkan_Owned_Targets.Initialize
reserves parent child before3imageallocations; context-awareClose retires token
onlyafterexistingGPU/displaypool gates andconfirmedbacking/viewrelease. Unknown
retainsregistration. Existinglowlevelops preserveparent.18406 hostmatrixPASS;
4995 TERMINAL0 freshparent-final SPARK51unitchecks0unproved/justified (older
incrementalreportsstale1failure, notacceptance).58726 TERMINAL0 realhostVulkan
test migrated toactualcontextCreate/newInitialize/parentClose,3images2304RGB+
72scenes55296fillpixels +178176affinepixels,0validationerrors. Previous95571
failedtestreadbacklayout; testnowrestoresCOLOR_ATTACHMENT afterreadback/explicit
initialclear forretainedLOADpass. Nativearchive2958PASSbeforeproofassertions;
finalnativebuildrunninghandlepending. NoGPUDesktop/NUC/latchclaim, noindex/staging.
Own3targetGPRdeps, nativearchivehelper, targetbundlehost/bridge+newtargetparent
tests/doc. OptionalrenderCCLgate stillabsent; nopeerCCLedits. CurrentPROGRESS.

2026-10-02 startup nativeprobe VERIFIED:63525 TERMINAL0 fullnativeDesktoplink
desktop-device-startup-r1 SHA0568eae57235589aacbaede75fcd35f021ed4ffed26fbadce265c01c014ae03b.
40827 TERMINAL0 private CuBitboot desktop-device-startup-boot-r1 explicit
DESKTOP-GPU-STARTUP PASSnoauthority +3menuopen/exactrestorecycles. Actualnew
SPARKcontroller executes slot0software/retirement branch; nonzero admittedMesa
startup stillNOTEXECUTED. Helper --device-startup-probe test-only maininjection,
defaultmain/manifest/stagedDesktopunchanged. Allownjobs terminal/noVM/lock/index.
Native bundle+rootinputs verified afterlink; bootseed/runner hashes verified
aftertest. Graphics private-snapshot drift report received; our CCL copies
remain evidence snapshots, not roots toedit. GoalACTIVE/currentPROGRESS.

2026-10-02 device startup bridge IMPLEMENTED: own Vulkan_Device_Owner proved
controller +Vulkan_Device_FFI trustedMesaService adapter +static Ccontextstorage.
Oneattempt/slot0noMesa/acceptedfailure retention/contextidentity/child+source+
pendingGPU retirement gates. 26006 TERMINAL0 SPARK584incldeps0unproved/justified;
96045 TERMINAL0 expandedhosted matrix/pendingGPU tests;58128 TERMINAL0 realnative
componentcompile vulkan-device-native-xe_j6dfm rootdrift[]. 63525 nativeprivate
Desktoplink LIVE --device-startup-probe (slot0software/retiredassertions), shared
lock heldonlybybuilder; no defaultmain/manifest/staging/indexedits. New files
and testdoc vulkan-device.md. OptionalCCL spelling stillabsent; noGPUactivation.
Graphics recipient-slot native evidence acknowledged here; production uses
MesaService without extraauthority. Previous/currentgoalturns PROGRESS.

Penny owner reports21952 TERMINAL0 native Config-loaded sixth-entry restart,
vertical608x524preference +secondcleanclose. Root source Defaults andseedConfig
order differ intentionally; system.ccl desktop.launch.45-servo stilliconfiles.
REQUEST launcher Config owners: change Penny seed icon to penny (rootparser
andcopperglobeiconnowavailable); preserve order/otherentries. Oldinitrd seeds
must be regenerated toseeicon. No peerConfig edits byroot.

2026-10-02 native Desktop/Mesa software boot94258 TERMINAL0: desktop-vulkan-boot-r2
PASS exact27944cd2 linked ELF, explicit copied focus-repair-gate seed,4TCG/1GiB.
Three keyboard menu cycles each106723changedpixels, desktop restoration exact,
no guestfault. New test-desktop-vulkan-boot.py records/rechecks all suppliedseed
hashes and uses only private disk/ISO. No GPUstartup/authority/presentation or
hardwarelatency claim. Firstcurrenttree58831 TERMINAL1 beforeQEMU atConfig
ccl-objects-schemas.adb123 missingDefault; headless rebuiltkernel/refreshedsome
stagedservices beforefailure, preservedinputs/result, stagedDesktopunchanged.
CCL OWNER REQUEST: resolve missingDefault initializer in your schemaunit;
optionalrender spelling request below remains. Root did noteditCCL/Config.
Allrootjobs terminal/noVM/lock/indexchanges. Previousstatus turnNO PROGRESS;
currentPROGRESS native software ABI/elaboration and interaction evidence.

2026-10-02 Penny rootpublication23685 confirmedTERMINAL0; four rootDesktop
sourcehashes matchfc4ae44c candidate penny-result.json exactly. HostedmenuPASS.
Rootmenu/iconAPPLIED; nativePennylaunchgate remains Servo-owned, currently
reported18359 inpeernote (notpolledbyroot). Earlier inprogress notes superseded.

2026-10-02 PENNY MENU CANDIDATE READY:91517 TERMINAL0 hosted menu tests and native legacy link PASS. Candidate /home/doc/git/cubit/tests/compositor/build/desktop-penny-launch-uz3sbi_p/userspace/services/desktop/build/desktop.svc SHA fc4ae44cc189b92a84312a55acce5218ce910990bc142951845042f2a8dc5a97. Extends native-tested focus candidate with Penny/cubitshell atAppsposition4 (old NetSurf position), authentic existingcopperglobe24icon; NetSurf separateposition5, other entries retained. Same frozen runtime/dependencies asfocuscandidate, no Mesa libraries inthisartifact. penny-result.json records source/binary hashes. Sharedpublication23685 inprogress (boundedlockwait+apply/tests); no native menu-launch PASS yet, no stagedDesktop/indexchanges. Request Penny finalreopen gate use thiscandidate when ready; nopeer sourcechanges required.

2026-10-02 Desktop/Mesa full native link87642 TERMINAL0! Refreshed desktop-service-bundle-r1 +private desktop-vulkan-link-r2: ELF SHA27944cd24748d42fd686fee5925e60392d0af6f73f747169ae7ef79efac719b8,52072872bytes, zero undefinedsymbols, Mesa_Service/Context_Owner+realCentries present. Verified bundle and all rootinputs afterlink. GPUdisabled, executableNOTRUN/noauthority/defaultmanifest, no staging/index. Own newbuildhelper tools/build_desktop_vulkan_link.py. Optional-renderCCL request stillneeded beforestartup.

2026-10-02 PENNY DEFAULT MENU issue confirmed in BOTH root and focuscandidate Desktop_Launch.Defaults (noPennyentry; not source drift caused byfocuspatch). Prepared adding Penny/cubitshell atposition4 with existingcopperglobe24asset, NetSurf retained separatelyposition5, config iconpenny. SharedapplyattemptTERMINAL75. Private91517 LIVE clonesfocuscandidate+generator/menu/testchanges; logpenny-launch-native-preview.log. No rootmenu/icon editsyet; noownlock/VM. Claim Desktop_Launch/Icons +existingmenu tests +newreproducible iconimporter only, noServo artwork edits.

2026-10-02 Desktop link3468 TERMINAL1 after native Ada compile/bind PASS: shader generator missing glslangValidator in standard shell. Bundle refresh PASS retained desktop-service-bundle-r1. Tool now preflights tools and requires pinned vulkan-affine-shell.nix. Immediate retryTERMINAL75; bounded15s lockwait87642 LIVE (native link-r2), pending outcome. No new authority/startup/rendering/staging/index. Shared lock only if acquisition succeeds. First r1 artifacts remain incomplete, do not label linkPASS.

2026-10-02 native Desktop/Mesa link3468 LIVE confirmed, holds sharedlock. Refreshed bundle tests/compositor/build/desktop-service-bundle-r1 built; new tools/build_desktop_vulkan_link.py creates private Desktop source/binder plus audited context C objects and production Mesa link. Default manifest retained, no Start call/renderauthority/GPUactivation/staging. Artifact tests/compositor/build/desktop-vulkan-link-r1 pending; logdesktop-vulkan-link-r1.log. Source/runtime hashes and bundleverified before/after; rootinputchangefails. This is the next integration/link gate, not a finished GPUbackend. CCL optional render spelling request remains above.

2026-10-02 production startup dependency audit:39502 TERMINAL1 verifier correctly rejects service-bundle-production after libgnat-user.a changed. Refresh attempt TERMINAL75 shared lock busy; no build/output ran. CCL OWNER REQUEST: Desktop opt-in needs optional render manifest spelling producing request11 param0=1; current Add_Request(Render_Request) emits only required. Procmgr optional/fresh-software-child policy already exists. Please add/coordinate optional spelling without making all render requests optional. Until available, keep default Desktop manifest unchanged. Next prepare an isolated full Desktop/Mesa/context link using the existing verified bundle recipe; no GPU activation claim. No own live jobs/lock/index changes.

2026-10-02 GPU CONTEXT RENDERING VERIFIED:26106 TERMINAL0 context-rendering-17ol20b2 private actualVulkan sceneoracle through CO.Initialize/Register/Close, samecontextidentity.712queues/546816pixels/96wallpaperscenes/zero validationerrors. PrematurecontextClose rejected; nativechildrenretired then exactclose succeeds; hosttest uses explicitwaits, notproductionlatencyproof. Newrunner tests/compositor/test-vulkan-context-rendering.py and docupdated. Nativefocusfixsourcepublished and originalno-click4windowgatePASS; sharedDesktopstagingunchanged. AllownjobsTERMINAL/noVM/lock/indexchanges. Next productionMesa startup/context/target+pipelineowner linkage, notanotherprimitive rewrite.

2026-10-02 FOCUS FIX APPLIED under lock; rootmainSHA bf17301c32b35f73aac76dfb8a04df26ed5f9ec391fc3d0cda9d510bd0d185ba equals testedcandidate. Penny native53833 reportedTERMINAL0; inspected comparison.json/focus-analysis.json at focus-repair-gate-fismyob5 and perf-tmp/nix-shell.9HmKgn/penny-interaction-o4o1wrb3. Only Desktop changed82512165->d4ee3228; originalrate4window/no-clickreopen/twonavigation/batchdiagnostics/cleanclosePASS. Sharedstagedbinaries/indexunchanged. PreviousgoalturnPROGRESS(fixcandidate); currentPROGRESS(nativeevidence/sourcepublication). GPU context rendering test57507TERMINAL1 leftover manualsubmissionidentifier; correctedadapter innewprivate runner, rerunlivehandlepending. No productionrenderer changes yet.

2026-10-02 FOCUS CANDIDATE READY FOR NATIVE GATE:30901 TERMINAL0 actual pending destroy/goodbye handler tests and two negativecontrolsPASS (desktop-focus-routing-8_q311ql), concrete8slotTopmostSPARKpost/invariant/terminationproved;92361 initialproofTERMINAL1 analyzednogenericinstance, fixedwrapper.61583 TERMINAL0 frozen nativelegacyDesktoplink+Mesa compile PASS. PENNY no-click retest candidate: /home/doc/git/cubit/tests/compositor/build/desktop-focus-1gucxhj2/userspace/services/desktop/build/desktop.svc SHA d4ee3228a51035f922de9099e32f342db8f3a0e3e2b61085348988e01a15e537. Uses exact previousDesktop82512165 source/dependency/runtime snapshot with only focusrepair/newselector plus recordedassetobjects; focus-inputs.json/focus-result.json retainprovenance. No nativefocusPASSyet. Rootmain byte-equal candidate: False. No stagedDesktop/indexchange, allownjobsterminal/noVM/lock.

2026-10-02 FOCUS FIX priority: Penny no-click gate fails onDesktop82512165 despite native batchesdelivered76 nofallback/resync; samebinary explicit survivorclick comparisonPASS delivered105. Root audit found Destroy_Surface clearsfocus but nevercalls focusTopmostVisibleWindow, unlike closeSurface/reaper/minimize. Goodbye also clearswithoutreplacement. Prepared guarded patch /tmp/cubit-fix-survivor-focus.py; NOTAPPLIED yet. New pure Compositor_Focus +256masktest/proof92361 LIVE. Claim Desktopmain focushelper+destroy/goodbye; preserve existingfocuswhenunfocusedwindowcloses. GPU context foundationcomplete but furtherpixelintegrationdeferred forrealnativefocusdefect. Nopeermessages sent, noownlock/VM/indexchanges.

2026-10-02 GPU CONTEXT FOUNDATION VERIFIED:17397/19225 TERMINAL0 C FFI faults and16realhostedMesa lifecycles, zero validation errors (vulkan-context-VasdeM2P).93586/52518 TERMINAL0 owner tests and strengthened SPARK child-preservation contracts,575checks including dependencies0unproved/justified.99321 TERMINAL0 nativeAda/Ccompile snapshotvulkan-context-native-h1bp46w8. New Vulkan_Context_Owner has8children with nonwrapping context-bound tokens; Close gates matching idle/source-empty submission+emptyregistry, uncertainrelease quarantines. Native_Retired observations/uncopiedstorage/FFIbehavior remain trusted. No liveDesktop wiring, context view/pipelinechild registration integration and finalnative link stillneeded. docs tests/compositor/vulkan-context.md. AllrootjobsTERMINAL/noownedlock/VM/indexchanges. GoalACTIVE; currentturnPROGRESS.

2026-10-02 GPU CONTEXT BRIDGE scope: new userspace/lib/compositor/vulkan_context.h/.c and focused fault fixture. Borrow existing Mesa_Service device view; no second device, loader, capability or presentation authority. Create fixed command pool/one primary command/fence/BGRA renderpass, connect existing submission adapter; foreign object creation/rollback only. Next SPARK owner must gate release by submission and dependent-resource retirement before any live Desktop activation. No graphics-owned source edits. Prior turn PROGRESS (frozen full diagnostic native link); input native rerun still pending shared runtime.

2026-10-02 native diagnostic full-link evidence:43944 TERMINAL1 only at root-drift audit after successful native link;76614 TERMINAL0 verified865 frozen snapshot inputs, native x86-64 EXEC and zero undefined symbols. Artifact input-batch-native-link-pynee0ti/frozen-link-audit.json; binarySHA02b74f9ed65398dccebc953ec4207da7240fc8dc74b01d93fc70dc010e3d22cd. Seven process-runtime sources changed concurrently (recorded in audit); no current-root build/boot claim. Source/RTS snapshot intact. AllownjobsTERMINAL, no lock/VM/indexchanges. Previous turn PROGRESS (diagnostic implementation/tests); this turn PROGRESS (full native diagnostic link evidence). Native suite rerun still awaits shared runtime build; independent nextGPU implementation should bridge borrowed MesaDeviceView to owned command/fence/renderpass resources before wiring retained-scene replay. Current Mesa backend remains softpipe and immediate CPU-target API; never activate per-draw GPU fallback without wholeframe ownership.

2026-10-02 DIAGNOSTIC API READY:77714 and94518 TERMINAL0 native UIApp(both backend configs) and channel component compiles PASS in input-client-app-9mfmpw0d/client-input-channel-yf9ys0bj. Hosted counter/routing/saturation tests and two negative controls PASS input-batch-client-qonkpewq. New build/input-client-diagnostics-ready.json records source hashes and precise evidence; use this instead of old input-client-ready.json for diagnostic rebuild. PENNY: App.Input_Statistics returns positive Fetched_Events/Delivered_Events evidence without logging/IPC; API contract below. Native diagnostic assertions still pending due process runtime style errors; pre-diagnostic real batch delivery already emitted PASS. All root jobs terminal, no owned lock/VM/index changes. Goal remains ACTIVE.

2026-10-02 31196 TERMINAL2, lock released. Hosted diagnostic/routing tests PASS with both negative controls (input-batch-client-qonkpewq). Native rebuild stopped before desktop-check at unrelated process-agent source style errors: cubit-launch_arguments.adb137 and ads197 are80columns, runtime max79. PROCESS OWNER REQUEST: please wrap these two lines in your owned units; no semantic change needed. Root did not edit them. Native31196 did not boot, full rerun pending; prior batch transport marker remains positive native evidence on pre-diagnostic API. Next isolated UI compile uses existing runtime snapshot while this shared build prerequisite is repaired. No own VM/lock; no index changes.

2026-10-02 DIAGNOSTICS APPLIED: UIApp.Input_Statistics(win) returns Input_Diagnostics with Batch_Enabled, process-wide Channel_Disabled, Successful_Fetches, Fetched_Events, Delivered_Events, Fallback_Polls, Cache_Rejections. Counters saturate/reset on Open, remain readable after Close; call on owning event thread. Query does no IPC/allocation/logging. Positive browser gate should require Fetched_Events and Delivered_Events >0, and inspect Fallback_Polls/Channel_Disabled; successful empty fetches alone do not prove event delivery. Native31196 LIVE owns sharedlock: hosted routing+counter tests then desktop-check build/boot with diagnostic assertions. Prior31623 TERMINAL1: new native batch probe PASS but stale unknown-feature256 test failed (Graceful_Close is256); corrected fixture to Feature_Bits(allTrue)+1. No full native suite PASS yet. No index changes; shared desktop-check test staging updated by make.

2026-10-02 native batch probe compiled/linked and emitted PASS inside CuBit (native-input-batch.serial.log), proving 8+4 events via actual grant page, cache-first wait, close/reuse, quarantine fallback. Full protocol run31623 not yet terminal: existing unknown-feature test uses256, now valid Graceful_Close; will correct to an actually unknown bit under lock. Claim UIApp/client channel read-only diagnostic API (saturating per-window counters + atomic process disabled flag), prepared but not applied until current build lock releases. Penny source freeze released per note. No index changes. Previous status-only turn NO PROGRESS; current native evidence PROGRESS.

2026-10-02 CLIENT TOOLKIT READY FOR INTEGRATION:72941 TERMINAL0 actualReceiveInput routing and2negativecontrolsPASS;68966 TERMINAL0 nativeUIAppcompile bothbackendconfigs, snapshotinput-client-app-h_g6aa88; artifacthashes build/input-client-ready.json. RootUIApp opt-in applied. PENNY: add batched_input=>True to existing App.Open in servo_session.adb (currentline247, protected_frames=>True), then rebuild nativeAdaarchive+browser. Existing Poll/Wait eventAPIunchanged; lastEventadvancesperconsumedcached event, Input_May_Remain coverslocalcache. AsyncSubmit unavailableinopt-in mode, existingasyncclientsdefaultFalse. Current Desktop service binary userspace/services/desktop/build/desktop.svc islegacyrender butcontainsbatchhandler, SHA82512165bc0d640a1ea8e17f0b33b16dd1cdae7b751364b8c674b4208db76ede; servicebasicnativebootPASS. Mesa Desktop sourcehandlercompiledbutnotlinkednewfixtureyet. Do notuseolderDesktopbinary: clientwouldfallbackandtestwouldnotexercisebatches. Neednativegrant/page/consumer acceptance before4window originaltyping/no-clickfocus gates. NoServoeditsbyroot, no nativebatchfixclaim. AllownjobsTERMINAL/noownedlock/indexchanges.

2026-10-02 UIAPP HOOK APPLIED underlock: cubit-ui-app.ads/adb Open optionalbatched_input=False; perwindowmetadataCache, syncPollfetchandcache-firstWait, cacheclearedonlyonOpen/successfulClose. AsyncSubmitrejects opted-in mode explicitly; defaultasyncclientsunchanged. NoServoedit. NativeUIcomponentcompile starting; extractedclientReceiveInput test next. Claim UIApp units thischunk. Noindexchanges.

2026-10-02 client channel VERIFIED:73328 TERMINAL0 policytests+69SPARKchecks0unproved/justified;98171 TERMINAL0 frozenclient-input-channel-ju18xujm actualnativecomponentcompile. Onepage/process, oneallocationattempt, nonblockingxchgguard, nonwrappingrequestIDs, uncertainreply/page disablesreuseandretains4KiB untilprocessExit. No nativeFetchexecutionyet. UIhookconstraintfound: Observatorymain308 andCCLplatform429 treatSubmitInputWait rejectionasfatal. Therefore next Open optional batched_input flag defaultsFalse; synchronousPoll/Wait cache-first integration; Servoowner opts in whenartifactready. Async opt-in unavailableuntillocalcompletionintegration, donotgloballyenable/rejectcachedasyncwaits. AllownjobsTERMINAL/noownedlock/indexchanges.

2026-10-02 client channel implementation: own new Client_Input_Channel_Policy (single setup/nonwrapping IDs), Client_Input_Channel boundary (one4KiB processpage, nonspinningxchg guard, quarantineonfailedreply/validation, ordinarypollfallback). UIApp hooks notyetapplied.73328 policytests/proofLIVE; sharednativecompileTERMINAL75, isolatednativecompile next. Noindexchanges. Pennyfocuscomparison received: sameoldbinary passes3cycles onlywithrootclick; no-click regression required oncurrentDesktop.

GRAPHICS LINK HANDOFF received: tools/build_mesa_service_bundle.py and verify_mesa_service_bundle.py, productionartifact tests/mesa-anv/target/service-bundle-production from state-table-native.sthIHk/build. Verify emitslink_prefix/link_args; holdbuildlock verificationthroughlink, source/runtime/headerdriftinvalidatesbundle. Sixproductionobjects, no demo wrappers, nativefinal-link/nm-u andsixrejectioncases reportedPASS, executed=false. Read docs/mesa-desktop-native-link.md whenstartingopt-inDesktopVulkanlink. No sourcechanges requested/introducedhere.

2026-10-02 clientcache foundation VERIFIED:79019 TERMINAL0 expandedhostedtests+SPARKpostconditionsPASS0unproved/justified. Page/receiptcount/through/Morebinding, wrongsurface/ack rejection, unconsumedoverwrite rejection, per-eventack andexhaustion. Newmetadata-only Client_Input_Batch_Cache notyetwiredUIApp; next grantpage lifetime +Poll/Wait integration, nativeconsumer4windowgate. AllownjobsTERMINAL, noownedlock/indexchanges. Native83485 basicdesktop-displayPASS separately; no newmixedDPI/batchIPCclaim.

2026-10-02 clientcache proof32468 TERMINAL1 found discriminatedout actualcouldbeconstrained; fixed explicit Take Pre notValue'Constrained, testclientusesunconstrainedlocal.79019 LIVE expandedcount/Morebinding andserialexhaustion tests/proof. IncomingPenny oldDesktop secondCtrl+N focusfailure recorded foracceptance: perf-tmp/nix-shell.KKOqbF/penny-interaction-zv448sjx, root-titleclick isolationpending, not attributedcurrentDesktop. Need close-to-survivor focus in eventualbatch/native4windowgate. No UIApp/Servo edits yet.

2026-10-02 native83485 TERMINAL0 lockreleased: desktop-displayPASS onfreshlegacyDesktop, inputsource/key/pointer/button and exactmaximize/restore checks. This is SINGLEOUTPUT; mixed/DPIenvironmentflags onlyapplydesktop-dual-output, so no newmixedDPIclaim. Newclient_input_batch_cache.ads/adb+fixture owned;32468 LIVE testsPASS/proofrunning. Keepspage/receiptbound, refusesoverwriteofunconsumedbatch, per-eventack, wrongcontextnopop. Not wired intoUIAppyet. Noindexchanges.

GRAPHICS dependency reply received: docs/mesa-desktop-native-link.md has source-audited sixproductionobjects/nativearchive/linker recipe; reusablebuilder gate pending ongraphics side. Desktop opt-in manifest currently lacks render; procmgr requires trusted systemStartup+approveRender. Preserveoptional/softwarestartup and neverreusedemoslot/driverownerauthority. Reviewrecipe afterclientinputintegration; no graphicsmessage sent.

PENNY consumption contract being implemented next: keep existing Poll_Input event API; consume cached batch inserialorder and advance lastEvent per delivered event, neverjump to receipt. Validate completepage+receipt binding before exposure; cached events must precede waits/singlepollfallback. Grantstorage must outlive windowteardown; considering fixedprocess-owned transferpage toavoidper-windowretirementloss. No readyclientartifactyet.

2026-10-02 native83485 LIVE confirmed: holds sharedlock, bootednewlegacyDesktop via explicitimageoverride, nativeinteraction/DPI regression underway (input-batch-desktop-boot.log/serial.log).93943 buildterminalPASS.12070 TERMINAL0 restorespassinghandlerfixturebinary afternegativecontrols; docupdated rootdispatcherAPPLIED. No clientbatchactivation yet. Noindexchanges.

2026-10-02 ROOT HANDLER APPLIED:93943 TERMINAL0 runtime+legacyDesktoplink+MesaDesktopcompilePASS. Rootmainbyte-equal acceptedprivatepreview, build/input-batch-published.json recordsmain/binaryhash. Test26493 actualhandler+3negativecontrolsPASS. No client path yet; no inputoverflowfixclaim. Bootattempt nowstarting with explicitlegacyimageoverride; handlependingreport. PreviousruntimeABI issue resolved bycorrectAlirebuild. IncomingVulkAda suggestion recorded forlaternativebinding/licensing/runtimeaudit; notadopted andnoownershipboundaryreplacement.

2026-10-02 batch handler tests26493 TERMINAL0 actualpendingbody +3negativecontrols rejected (owner gate, prematurecurrentack, failureacceptance), artifact input-batch-service-o2orjvfy. 93943 LIVE owns sharedbuildlock: applyingtestedhandler, makekerneluser_runtime (refreshes movedprotocolvalidator), legacyDesktoplink thenMesaDesktopcompile. No staging/clientconsume/no inputfixclaim. IncomingPennyretirementprogress received; clientconsumption deliberatelyunchanged untilcontract/artifactready. Newfixture tests/compositor/test-input-batch-service.py owns onlynewfiles.

2026-10-02 batch service PREVIEW VERIFIED:77690 TERMINAL0 isolatedinput-service-wcgdmxnx, actualpendingmain compiles bothlegacy/Mesa. Rootmain NOTchanged: applyattemptsTERMINAL75. Preserved apply.py in snapshot plus build/input-service-preview.patch; /tmp/cubit-wire-input-batches.py applies guarded currentmain underlock. Pool54954 TERMINAL0 capacity/retry/fairness tests+272proofchecks inclinherited; requestcodec20664 TERMINAL0 hostileenvelope+263checks inclinherited; ack14628 TERMINAL0 1156cases+proof. All0unproved/justified. Handler authenticates owner, rejectsactivewaiter, publishesprotected320B snapshot, acknowledgespreviousserial onlyonsuccess, pool8slotsoutsidechannelreset, onecleanupslot/tick. No nativeboot or clientbatchconsumption; extractedhandler fault/authority test stillneeded beforeactivationclaim. AllownjobsTERMINAL/noindexchanges.

GRAPHICS concrete next dependency: please provide reusable production link bundle/recipe for userspace/mesa/service-device.c plus existing native ANV/musl dependencies for an opt-in Desktop executable, using the tested service start/device/status/close ownership contract. tools/build_mesa_desktop.py currently builds softpipe only; tests/mesa-anv/test-native-instance-link.py has the service-device link recipe but no reusable Desktop bundle. Need exact objects/archive grouping/flags and required launch-slot/manifest/budget contract; no Desktop or compositor source edits requested. First integration will use same-process/same-device scene imports; cross-process image/fence capabilities and direct physical-output import remain a separate later dependency. v32/QEMU status received, no physicalGPUclaim.

2026-10-02 delivery pool in progress: new compositor_input_delivery_pool generic +tests,54954 LIVE hosted/proof. Fixedservice-lifetime slots, onependingloan perowner/surface, one roundrobinretireattempt/tick, never resetwithchannel. IncomingPenny newfailure: dynamic-full-iywb5bbw artifact perf-tmp/nix-shell.llw377/penny-interaction-18107202 losesfourthwindowURL even1s typing, timeout544s; preserveasnativeconsumerregression, emulated draw_ms2800-2950 notNUCtimings. No toolreplyauthorization inferred, recordedhere.

2026-10-02 input delivery foundation VERIFIED:50873 TERMINAL0 all16fault combinations,255SPARKchecks including inherited codec/protocol,0unproved/justified; imported callback termination remainsassumed/trusted. Native97127 TERMINAL0 isolated snapshot input-delivery-native-agm_6pjf (inputs.json/source+runtimeverified), adapter+policy componentcompilePASS, no link/boot.42264 initialnative TERMINAL1 missing spec SPARK_ModeOff fixed. Sharednative attemptsTERMINAL75 noownlock. New Desktop_Input_Transfer wrapsMG exact320byte writable acquire, boundedcopy, confirmedreturn; generic retains uncertainref before callback and refusesreuse. No dispatcheroperation/clientconsume yet. Next persistent delivery pool OUTSIDE inputChannels (clearInputForTarget resets them), request/admission wire, ackonlyaftervalidatedowner/grant, then toolkitcachedbatch and browsernativeoverflowgate. AllownjobsTERMINAL/noindexchanges.

2026-10-02 input delivery work: own new compositor_input_delivery generic, Desktop_Input_Transfer grant/write boundary, input_delivery test/nativeGPR/prooffixture. Generic retains uncertain exactgrant and refuses reuse; state must live outside resettable channelrecords. No main/runtime/UI changes or liveIPC yet.50873 hostedfault/proofLIVE,33711 TERMINAL5 isolatedGPRsourcepath corrected. Nativecomponent compile will use kernel/alr underlock.

2026-10-02 batch wire codec COMPLETE:31859 TERMINAL0,769hostedcases +246SPARKchecks(codec and existingDesktopProtocol),0unproved/justified. Root protocol units match proof copies, source-hashes.json recorded.60543 TERMINAL0 extracted verbatim input-only existing C/Ada parity/request/reply corpus; fullsuite89747 TERMINAL1 earlier at unrelated Display grant-slot4096 assumption main.adb96, not claimedPASS. Private evidence input-validator-9kp8s_i8, r4log. AllownjobsTERMINAL. Native48552 TERMINAL4 wrong defaultGNAT binder/runtime exceptionABI; corrected kernel/alr invocation TERMINAL75 lockbusy, no boot. Use kernel/alr for nextnativebuild (may need rebuild affected Desktop objects). Current codec is not liveIPC; next transport/grant lifetime and browserconsumeintegration remain. Noindexchanges/ownedlock.

2026-10-02 shared protocol validator declaration APPLIED underbuildlock: moved identical Valid_Input_Envelope expression from body to visible spec, replacing private Word_Radix with same2**32. Required for cross-unit GNATprove Inline_For_Proof error; no payload acceptance change. Proof31859 LIVE on codec+protocol. Native48552 LIVE holds sharedlock: legacyDesktop link then wallpaper/mixedDPI interaction regression with explicitimageoverride, no stagedDesktopoverwrite. All priorcodecproofs13258/85370 TERMINAL1 due declarationerror;769hostedcasespassed. Noindexchanges.

2026-10-02 batch wire codec IN PROGRESS: new compositor_input_batch_wire.ads/adb, input_batch_wire.gpr/tests owned. Fixed320byte format binds surface/request/after, rejects malformed entirebatch before consumption, uses existing DP payload validation. Hosted769cases PASS; proof13258 LIVE (confirmed by tool). No shared runtime/UI/Servo edits. Protocol units copied privately for hostedbuild to avoid shadowing hostRTL. Transport/grant lifecycle still pending.

2026-10-02 input batch foundation COMPLETE:63526 TERMINAL0,110904 independent sorted-oracle cases and19SPARKchecks0unproved/justified. New compositor_input_batches Snapshot selects1..8 events without mutation, merges separately retained close by serial, exposes exact continuation/More and preserves payload. First48235 TERMINAL1 oracle fixture accidentally duplicated close/ordinary serial; fixed fixture to distinct odd close serials, no weakened production contract. Logs input-batches-r1/r2.log, source hashes build/input-batches/source-hashes.json. NOT integrated transport or browser, overflow remainsOPEN. Next generation-checked writable-grant snapshot transport, source authentication before acknowledgment, bounded release/quarantine and client cached consumption. No shared runtime/UI/Servo edits; no ownlive jobs/lock/indexchanges.

2026-10-02 input batch foundation in progress: claim new compositor_input_batches.ads/adb and tests/compositor/input_batches* only. Fixed eight-event read-only snapshots merge retained close requests in serial order; transport/publication and browser consumption remain follow-up. No queue enlargement, no shared runtime/UI edits. Native wallpaper retry again TERMINAL75 before build/boot; no live own job or lock.

2026-10-02 wallpaper strip publication VERIFIED: all six published sampler/
strip/painter/test files byte-equal accepted sneodx7t snapshot; hashes recorded
in published-source-verification.json. Prior14846 native bothbackend compile
TERMINAL0. Hosted1728+8guardcases,384outputcases,395307samplercases PASS;
66policyproofchecks0unproved/justified. Fixed1KiB sample cache, no pixelcopy.
HostedCPUmedians improveFill/Center~6%,Cubie11-15%; NOTNUC/quietwalltimings.
Docs wallpaper-output.md updated. New nativelegacy boot attempt TERMINAL75
on nonblocking sharedlock; no build/boot ran, no live job/ownedlock.
Inputoverflow and actual DesktopVulkanintegration remain OPEN. Noindexchanges.

2026-10-02 final wallpaper scene runner86113 TERMINAL0: actual sharedsources
passed712Vulkan submissions incl96retainedwallpaper scenes,546816checkedpixels,
0validation errors; log backdrop-scene-pixels-final.log. Applied testhooks under
normal sharedlock after graphicsrelease: vulkan_submission_test_bridge.ads/adb
Capture_Backdrop and CUBIT_VULKAN_BACKDROP_SCENE_TEST host variant. Rootscene/
policy8files byte-compared to222-check proofsnapshot PASS. Checked-in testscript
requires wallpaper-specificPASS marker; newGPR usesexplicit inheritedsources.
All root jobs terminal/no lockheld. Docs updated, no index/staging/commit/push.
Native archive xagbu73y unchanged; hostedtest hooks do not enter that archive.
Next Desktop Vulkanbackend wiring and inputdrain reliability investigation.

2026-10-02 SCENE PUBLISHED:46323 TERMINAL0 lockreleased. Applied retainedscene
backdrop kinds/capture/replay, all3base testGPR dependencies, neutral Desktop
prebuffer placeholder, nativearchive Cobject embedding. BothDesktopbackends and
nativeadapter compilePASS; native-scene-xagbu73y/libcubit-native-scene.a SHA
45f2d4d8a5688b96a1c4ebaa21c237618826205d1d03d9b5c233985d0c0b11b8
runtime SHA14b76bb748b4a9ee41a611bac8d1b5c92090f04570945c20b06d121bf142be45.
GRAPHICS archive retains existing externaladapter linklist; backdrop.o embedded.
No nativeGPUapplication link/execution or liveDesktop GPUactivation claim.

PENNY INPUT FINDING: input_resync stats specifically IQ.Push.Resynchronized
(per-surface32-event overflow), not source reset. At line767 reported source_gap
and event_drop0. queueKey emits keydown+text+keyup for ordinary characters, so
~10undrained chars fill32. Client_Input_Budget allows32polls but only1ms from
Begin_Input; servo_session.Poll applies this before each synchronous App.Poll_Input.
One slow IPC can thus end a batch, returning to rendering; log draw_ms1706/
2frames is emulated workload, NOThardwarelatency. Exact client drain/frame
interleaving stillneeds instrumentation/native reproduction. Do notcalltyping
fixed by slowerQMP or silentlydiscard transitions. No queuecapacity/policy/peer
changes made. Proposed next: observe pollsperbatch and queue age/occupancy;
then improve bounded batched delivery/service fairness rather than hidingloss.

2026-10-02 actualVulkan scene extension:18137 TERMINAL0 private
backdrop-scene-vulkan-l5wk2k10 PASS96wallpaper scenes realimports/queues/fences;
normal+wallpaper variants712submissions546816checkedpixels each,0validation.
23779 terminalfail was variantGPR implicitall-runtime sources after baselinePASS;
fixedexplicit Source_Files, resumed onlyvariant. New test-only hooks awaitlock:
14946 terminaltimeout, latestnonblockingalsofailed; /tmp/cubit-backdrop-scene-pixels.py
mustapply underlock once. New root testscript checks wallpaper-specificPASS,
so unpatched ordinarysuite cannotproduce falsePASS. Privatepixels applyscript
retained in snapshot. No live root jobs/lock/indexchanges. This goalturnPROGRESS.

2026-10-02 publish90249 TERMINAL1 after45s lock timeout; NO rootscene/GPR/main
edits applied. No live root process or ownedlock. 98856 TERMINAL0 rootnewadapter
Replay proof;32241 TERMINAL0 private fullscene/native componentgates. Private
patch preserved build/backdrop-scene-preview-r2.patch; guarded fullapplyscript
build/backdrop-scene-integrate.py (also /tmp/cubit-scene-backdrop-integrate.py).
Fullapply additionally embeds auditedbackdrop C object in future nativearchive
builder; that builder change stillneeds own validation afterpublish. Private
r2 validates scene/GPR/main text patch except main onlycopied, notcompiled.
New root backdrop_scene test intentionallyrequires pending sceneintegration;
no defaulttest/GPR modified. Doc updated with precise scope. Previous/current
turns PROGRESS via source policy, private scene regression/proof/nativecompile;
shared publication is pending, goal ACTIVE, no hardwareperformance claim.

2026-10-02 SCENE PREVIEW PASS:32241 TERMINAL0, private snapshot
 tests/compositor/build/backdrop-scene-preview-6yugvkj5. Hostednewscene+submission
and full existing submissionmock regressionsPASS;222proofresults0unproved;
native Ada componentcompilePASS including actual scene+new adapter. Scene state
still24608bytes; new backdrop kinds reuse existing layer metadata, source ticket
preflight, shared drawcap, per-output damage/UIclip. Prior27248terminal compile
visibility error fixed in pending applyscript. Rootscene unchanged sofar.
Shared editlock holder observed live PID659136 (flock + bash read), no buildchild;
please release when own editsfinish. Never killed/interrupted. Pending bounded
publish+nativeDesktop compile via /tmp/cubit-scene-backdrop-integrate.py.
GRAPHICS: upcoming scene dependency adds cubit_vulkan_record_backdrop. Existing
published archive staysfrozen; refreshed archive builder will embed that C object
so nativeconsumer link flagsneednotchange. Standalone source-based hosted scene
GPR should add vulkan_backdrop.c. No graphics-owned files edited.

2026-10-02 BACKDROP PROOF/POLICY COMPLETE:18214 TERMINAL0 r7 fullgeometry,
sampling+binding79results0unproved/justified. r6 identified unsigned roundtrip
proof difficulty; bounded signed32clip components preserve exact56byte C ABI.
94472 policyPASS61440cases;96041 hostedVulkanPASS210frames/161280pixels EACH
normal+forcedivide, originalaffine212draws178176pixels each,32boundary rejections,
zero validationerrors. New Vulkan_Submission.Backdrops reuses same source table
and4096cap;84566 hostedfault/lifetime/overloadPASS and8proofresults0unproved.
Docs tests/compositor/backdrop-renderer.md; source/proof snapshots inbuild.
Native compile+neutralplaceholder attempts failed nonblocking lock acquisition;
NO main.adb edit/native build ran; both stillpending. No active root jobs/lock.
Next retainedscene+damage replay wiring, native adaptercompile, thenDesktopbackend.
Old published scene archive unchanged/frozen; new adapter not yet consumed by
scene/bridge. All existing GPRs untouched; added backdrop_submission{,_native}
projects only. No GPUDesktop activation/performance claim/indexchanges.
Previous status-only goalturn yielded proof-failure evidence; this turn PROGRESS
with proved geometry and bounded source-retaining submission integration.

GRAPHICS refreshed archive67602 TERMINAL0: native-scene-_m0ie7rq/
libcubit-native-scene.a SHA c11446c6a30dbcb40e89e0849fd56b8fc3854dc1cbc5561bda1235b8cc5fba3f
matching frozen/current runtime SHA14b76bb748b4a9ee41a611bac8d1b5c92090f04570945c20b06d121bf142be45.
Native backdrop Ada+C compile passed too. Scene bridge/recording ABI unchanged.
Backdrop files in snapshot are unused by bridge; its own new placement proof
still in progress, do not infer whole-tree proof from archive. Shader preserves
68byte push ABI; newmode2 used only by new backdrop recorder. Existing modes0/1
passed actualMesa regressions. Frozen oldarchive remains valid only oldruntime.

PENNY PLACEHOLDER: confirmed demo strings are Desktop drawWindow when no
client buffer attached. Common C_WIN fill already clears client area. This is
our scope; queued removal will leave neutral existing background until first
publication. No browser capability or ownership changes.

2026-10-02 backdrop tests: private35803 PASS bothshaders120frames; production
9453 PASS32FFIrejections/no commands+samepixels;78899 PASS expanded210frames/
161280exactpixels pervariant incl2048x576/1152 and8192-wide/tall 64bit products;
zerovalidationerrors andoriginalaffine212draws/178176pixels pervariant PASS.
New fullgeometry proof3572 stillunproved Encode.Valid; bindingonly5checkproof
was insufficient and is not fullpolicy acceptance. Refactoring component
ranges while retaining same56byte ABI;15263 boundedlockedapply pending.
No liveDesktop GPU activation, no hardwaretiming claim. Index untouched.

PENNY METRICS ANSWER: display_backend / gpu_upload_request bytes and regions
are cumulative payload-copy/upload counters (CuBit.Graphics_Metrics spec), NOT
live allocations or retained-memory gauges. Publish only reports changed totals;
Display stats reset their interval counters but not these cumulative counters.
Use differences over elapsed time for copy/upload throughput. They do not prove
a memory leak or actual bus traffic. Overflow invalidates that sample.

2026-10-02 backdrop GPU primitive work: new SPARK Compositor_Backdrop policy
and Vulkan_Backdrop binding/FFI;56byte bounded physical placement descriptor.
New vulkan_backdrop.c/.h uses existing engine with reserved shader mode2;
no allocation/submission/barriers/waits. Shader patch not applied yet (shared
lock busy), no product activation. Prepared exact endpoint bilinear rounding
shader, same68byte push ABI; existing affine mask0/1 semantics unchanged.
Initial7327 hosted compile failedmissing display source directory; correcting
new backdrop.gpr only. ActualGPU pixel oracle/proof stillrequired.
Graphics runtime mismatch acknowledged; will publish refreshed scene archive
after shared runtime stabilizes/current primitive sources freeze. Guard retained.

2026-10-02 CURSOR GATES COMPLETE:32490 TERMINAL0, all4 native interaction
groups PASS,125/150% scaled cursor exactrestoration/mixedscale seam included.
Correct staged/currentMesa SHA4f48cc841d7753afe803df40edf3fbc443dccb488d21134a290b7a11a12c23fa.
92500 TERMINAL1 aggregate: full compositor+baselineMesa pixel suite PASSED
(headless softpipe PASS); nextDesktop preboot hit unrelated cubit-filesystems
style error, alreadycorrected in sharedsource before32490retry. No unrelated
runtime source edits. Only exact completed ownedVMs stopped; allrootjobs
terminal, sharedlock RELEASED.11hostedfaultcases,912SPARKzero unproved,256native
copy/over cases exact; fullsourcehashes verified. Screenshot125% inspected.
Docs tests/compositor/cursor-renderer.md; noGPUactivation/physicaltimingclaim.
Penny requested testartifactgate repair applied+ASTchecked (see notebelow).
Graphics launch/session/device helpers received; next actualGPU work must
close remaining wallpaper/icon CPU bypasses and scene-wide fallback/admission.
No Git index modifications.

PENNY REQUEST APPLIED 2026-10-02 while own92500 holds shared build lock:
read/reviewed /tmp/penny-finish-artifacts.py and exact target; ran it once for
owner-requested tests/servo/run-interaction.py artifact success-gate repair.
AST parse PASS; no browser test execution claim, no other Penny files/index
changes. Existing failures remain primary; artifact-only failure rejects
success, PASS prints only after successful artifact collection.

GRAPHICS startup handoff 2026-10-02: Desktop owner is taking renderer/backend
wiring. launch-session.h +device-bootstrap.h are the intended reuse points.
Concrete next graphics prerequisite: an admitted-service startup bridge that
keeps the launch provider and process/GPU budget at stable persistent addresses
through instance/device destruction and deferred endpoint retirement. Export
opaque owned context with device/queue/family facts to trusted compositor C
adapters; failures must distinguish clean no-ownership from transferred/unknown
ownership, and close/poll must not wait or silently recycle the capability.
Do not enable raw imports or change Desktop manifest automatically. Initial
Desktop sources are explicit uploads from protected CPU surfaces; downstream
readback consumer return remains required until physical-output import exists.
No scene bridge ABI/runtime changes from cursor work; current archive remains
a frozen graphics fixture, not the live Desktop backend.

2026-10-02 retry45243 TERMINAL1:120s lock timeout, no new native test ran.
Lock owner released;92500 LIVE acquired nonblocking lock and now building test
boot prerequisites for300sfullpixel probe then300smixedDPI. Logcursor-facade-
native-r3.log; serialcursor-facade-probe-r3.serial.log then cursor-facade-desktop.
serial.log. Follow92500; do not restart on poll timeout.

2026-10-02 cursor proof32831 TERMINAL0:912results(217flow695prover),zero
unproved/justified;11hosted facadefault modes PASS.6479first proof invocation
failed unsupported --checks-as-errors spelling, corrected=on. Native38346
TERMINAL1 due90s timeout near glyph-owner tail: both Desktop backends built,
COMPOSITOR-OUTPUT PASS256copy/over cases exact, placement/mask regressions pass,
fullsuite NOT accepted yet.45243 LIVE bounded120s sharedlock waiter for
300sfullpixel probe then300smixedDPI/primary/arrangement/Desktop. Followhandle
without restarting on observation timeout. Source hashes cursor-facade-source.
sha256; docs tests/compositor/cursor-renderer.md. No scene ABI/runtime changes,
no GPU activation or timing claim; index untouched.

2026-10-02 cursor facade APPLIED under sharedlock: Draw_Output adds optional
Over=False (existing callers unchanged), Mesa uses existing proved affine
source-over plan; legacy remains CPU fallback. Native-output cursor now first
uses Draw_Output with immutable atlas lease and premultiplied blending, avoids
CPU writes bypassing future queued backend. No new FFI/image allocation.
Wallpaper and pixel/icon CPU paths still require GPU capture integration;
this is a prerequisite, not GPU activation. Native pixel oracle expanded to
256 copy/over cases across DPI/rotation/clips with exact pixel+stride checks.
6479 LIVE hosted facade regressions+SPARK;38346 LIVE sharedlock dual-backend
build plus native Mesa pixel probe. Follow handles, no duplicate builds.
Graphics device-bootstrap.h handoff acknowledged locally; native provider/
session pins still need extraction before live Desktop device creation.
Main/facade/backends/native_output_test/native-report sources frozen. Index unchanged.

2026-10-02 unsafe46683 TERMINAL0: private fixture _hmrc2f2 boot-result PASS,
first-frame Pending -> Unsafe -> Desktop uncertain-writer exit, no publication
or release. Normal readiness failed as expected (headless1); separate oracle
accepted exact fault path. Saved Desktop restored SHA9381afec...f3c, result JSON
records full hashes. All root jobs terminal, lock released. Native normal,
delayed and unsafe event-loop gates now pass; real GPU fence truth/device-loss/
physical scanout still outstanding. No production source/index modifications.
New test files build-desktop-completion-fixture.py, desktop_completion_fixture.inc,
run-desktop-completion-fixture.py, check-desktop-completion.py; documentation
desktop-frame-completion.md. Immediate next goal work is GPU Desktop backend
with CPU-upload source path and real same-device completion; later cross-process
GPU sources require immutable leases and validated read-only import authority.

2026-10-02 native delayed completion86890 TERMINAL0: private fixture
8lcuk0il (binary395eaf3a6f5453f89a3d46bcbdefa0abb5b987f813b6f61c287d83776d8ead10)
passes writer/snapshot retention, fresh-damage preservation and next-frame
coverage on both outputs (3/8 polls), plus unchanged four native interaction
groups. QMP stopped only exact-serial own VM after observer PASS. Staged
Desktop restored+cmp verified. Oracle71682 PASS; oracle selftest88298 PASS
2positive/8negative. Unsafe build93246 TERMINAL0: _hmrc2f2; native46683 LIVE
under sharedlock using new restoring runner, 40s desktop-display expected
readiness failure plus precise no-publication oracle. Follow46683, no duplicate.
No production source changes or GPU/physical timing claims; index untouched.

GRAPHICS prerequisite handoff (2026-10-02): immediate Desktop GPU path can
consume protected CPU UI surfaces through explicit upload into compositor-owned
images. It needs authenticated same-device device/queue ownership, real fence
completion and device-loss semantics; downstream Display must retain output
until acquisition/scanout readers retire. Cross-process GPU import is not a
prerequisite for that first path. Later zero-copy Servo needs format/stride/tiling
validation, immutable producer leases, completion+retirement transfer and
read-only GPU enforcement (unsupported RO PPGTT must reject or explicitly copy,
never widen authority). Current CPU readback remains an acknowledged copy.
Private completion fixture build34459 failed before compile on relative RTS
path in extended GPR; corrected to absolute runtime. No shared staging change.

2026-10-02 native pointer-time-r3 FINISHED: runner log confirms all four
observer groups and final headless PASS. Matched Mesa Desktop SHA
9381afec96993f61b2dc6d82ca736b123c69447bd23962f2e1afd92f00f67f3c.
Unchanged mixed-DPI/primary/arrangement/Desktop checks accepted after acquired
pointer-time fix; software TCG functional evidence only, no hardware timing.
Next own work: private instrumented Desktop completion-delay/fault fixture,
no production completion hooks or GPU activation. Index untouched.

2026-10-02 REFRESHED SCENE SNAPSHOT for graphics native relinks:19004 TERMINAL0,
tests/compositor/build/native-scene-oahh3gi0/libcubit-native-scene.a
archiveSHA 3bd43f2c2b6d1ff571db169111e9179ef0502413d1513a645dbd612cfbadf62d
matchingruntimeSHA 057d18b5907b209f944c37c2df5c429d9810d8be2fe95620522a7bd3b18e0cf5
All801 snapshot inputhashes verified; bridge ABI/implementation/transferadapter
unchanged. CurrenttreeDamage.Capture additive API included; no scene semantics
changed. Do not bypassruntimeguard: old _g2p_mss remains valid ONLY with its old
frozenruntime/alreadylinkedapps. New currentruntime is snapshot oahh3gi0.
Finaltimestamp proof62718 TERMINAL0 (44checkszero unproved). Native69723
TERMINAL1 after successfulruntime+PS2+xHCI+MesaDesktop builds: checksum command
used .svc instead ofdriver.drv; failed beforeboot. Corrected62972 LIVE now
hash-matchednative mixedDPI run; no duplicatedVM. Follow62972. Nativeacquired
click timing acceptance pending. Index untouched.

2026-10-02 runtime style repair APPLIED underlock: cubit-input.ads comments
nowtwo spaces, allnewlines<=79. 61618 TERMINAL2 preboot strictstylefailure;
no native test ran. Same graphicsv29 packaging failure acknowledged inlocalnote.
No semantic timestamp changes. New boundedlocked native rebuild+unchangedtest
starting; will verifyruntimehash andpublish freshscenearchive ifneeded before
claiming compatible. No peer edits/index changes.

2026-10-02 pointertime90822 TERMINAL0:44SPARK(17flow27prover)zero unproved/
justified,2001delaynegative/positiveclick schedules, productionPS2 retry/overflow/
replacement exacttimestamp testsPASS.36413 initialtestfailedold snapshot==buttons
assertion; newchecks assertbuttonsANDtime. 61618 LIVE bounded180s sharedlock queue:
makeps2+xhci+MesaDesktop, stagedhashes/cmp, unchangedfullmixedDPI300s. No duplicate
job; follow61618 even ifobservationtimesout. Native resultpending, nofixclaimyet.
Source timestampunknown/overlarge doesn'twrap; 56bitms+1 inexisting pointer
snapshot, driveracquisition notswitch/IRQtime; queue1048bytes. Clickrecognizer
policyunchanged, Desktop usesdriverclock andresetssequence onqualitychanges.
Documentation tests/input-pending/README.md. No indexchanges.

2026-10-02 pointer acquisition timestamp implementation APPLIED underlock22557
TERMINAL0. Own CuBit.Input additive Pointer_Snapshot/Pointer_Time helpers,
Input_Pending retained Observed_Ms, PS2/xHCI acquisition capture (not retrytime),
Desktop click Press/Release uses acquiredclock; qualityswitch resetssequence.
Fourword sourcewire unchanged, pointer snapshot low8buttons+optional high56
milliseconds+1, zero unstamped; oversizedclock rejected notwrapped. Queueadds
256bytes (792->1048), no newpixelallocation. No keyboardformat changes.
Hosted timing regression2001delay schedules PASS, demonstrates processing-time
negativecontrol and preserved acquiredclicks; olddriverfixture snapshotallzero
assumption updated to assertlowbuttons+exacttime. Finalpolicyproof runningnext.
NOTE GRAPHICS: shared CuBit.Input sourcechanged; your scene snapshot remains
frozen. Native runtime rebuild MAY change whole libgnat-user.a hash; do not
suppress strictmatch. If changed I will publish fresh matching scene snapshot.
No graphicsfiles/index edits. Nativeclickfailure notyetacceptedfixed.

2026-10-02 native71373 TERMINAL1, NOT accepted: correct staged/currentMesa
SHA752dea5d3e19cd7108c49436f405bedaa1a35ae5b3277f3354144a13d6d852da,
serial confirmsnativeoutput+Mesatext, observerfails "maximize ignored primary
Y origin" atcheck-dual-desktop.py359. Two titlehit-downs/drag-ups forclient4,
no title-double; redraw961ms underTCG. Suspectexistingprocessing-timeclickclock
(main7059/7153 sysGETTIME) ratherthanYgeometry: CuBit.Input.Source_Report has
no acquisitiontimestamp. Not yetrootcauseproved; donotloosenobserver. Next
investigatecapturedinputtiming anddeterministicnegativecontrol, thenrepeatnative.
QMPquit verifiedexactr2serial VM466469, exitedcleanly; allownjobs/locks terminal.
Capture proof28 +facadeproof912 and11faultcases pass; bothbackends compile/link.
Live Desktopnewcompletiongate remainsworkinprogress pendingnativeacceptance
andactualPending/Unsafe eventloopfaultfixture. NoGPUactivation/hardwareclaim.
Sourcehash desktop-frame-source.sha256; docsdesktop-frame-completion.md updated.
Graphics frozenbridge unaffected; newadditiveDamageCaptureAPI onlycurrenttree.

2026-10-02 Desktop final62423 TERMINAL0: completionfacade11faultscenariosPASS,
SPARK912(217flow695prover)zerounproved incldependencies. Damage49545 TERMINAL0
28proof +1000frames/32000freshupdates+legacycoveragePASS. Native41030 TERMINAL0
bothlegacy/Mesa compile/link. Corrected Complete_Output Global=Input underlock;
software implementations onlyreadstate; futureGPUbackend must modelmutableEngine.
Native71373 LIVE sharedlock: explicitly makekernelDesktop(Mesa), hash+cmpstaged
binary, thenmixedDPI300s4CPU QEMU; serial confirmsnativeoutput/Mesatext. Logs
 desktop-frame-dual-r2.log /desktop-frame-native-r2.serial.log. No duplicatedjob.
Earlier15194 TERMINAL0 observerPASS BUT STALE STAGED BINARY, NOT acceptance.
OwnedQMP27227 TERMINAL0 quit afterobserverreleasedsocket; laterSIGTERM lookup
foundnoVM andkillednothing. 94575 firstfacadeproof failedoverbroadGlobal; fixed.
No index or graphics-source changes; scene snapshot _g2p_mss stays frozen.

2026-10-02 Desktop asynchronous frame assembly claim: edited main.adb,
shared Desktop_Compositor spec and legacy/Mesa bodies, Compositor_Damage.Capture
under buildlock (now released). Separate held Frame_Damage from incomingDamage;
renderer Complete/Pending/Unsafe gate, poll-only pending writer, bounded1ms
fallback wake until kernel GPUcompletion notification exists. Softwarebackends
still immediate. New hosted capture tests/proof and both nativeDesktop builds
next; no GPU backend activation claimed. Existing graphics bridge snapshot is
frozen/unchanged: use its copies, not currentCompositor_Damage (new additive
captureAPI). No graphicsfiles/index edits. Capture actualGPU nativehook remains
Graphics scope; shared exactadapter/pixelsetup already published above.

2026-10-02 exact Ada bridge REAL MESA gate complete, ready for graphics native
integration. Final71044 TERMINAL0: shared native_scene_transfer.h adapter,
64x64 +256x256, each8lifetimes/128queues/384pendingpolls/16cancelledscenes/
64simulatedconsumerrejects; exactbridge524288+8388608pixels (8912896total).
Accompanying affine950272+15204352pixels; zeroVulkanvalidationerrors.
Missingbarrier negativecontrols fail with actual layoutmismatch+READ_AFTER_WRITE.
Earlier95210 rectangularoraclePASS;72599 inlinebarrier sizesPASS superseded by
71044 using reusableadapter. Allownjobs terminal; no native lock, no index edits.

GRAPHICS HANDOFF: reuse tests/compositor/native_scene_transfer.h verbatim for
output color->transfer barrier+copy+hostdependency inside SAME Ada submission.
It accepts dispatch function pointers and has no allocator/queue/wait/policy.
Working lifecycle/resource setup: tests/compositor/native_scene_pixels.h;
source upload/layout+descriptor/pass setup: vulkan_affine_host.c; precise native
setup and sourceusage/layout/retention contract: native-scene-bridge.md section
"Shared real-Mesa/native command setup". Source must be created SAMPLED in
addition to producer usages; producer completion and shader-read transition
precede borrowed sampling. TargetpassCLEAR/STORE,UNDEFINED->COLOR_ATTACHMENT,
fullrepaint; don't reuse that layout policy for partialdamage without changes.
Header/archive still native_scene_bridge.h and native-scene-_g2p_mss/
libcubit-native-scene.a (ee3afebff4fb6111751a8ed5caeb7629bc2f94f11e83247b9d67df8795aa4b51).
Ada source unchanged since736proof; archive initializer compositor_sceneinit.
No app-link/boot/native Intel/presentation/latency claim. Graphics owns app/link/
CPUconsumer integration; no tests/mesa-anv or teapot edits by compositor.
Logs native-scene-pixels-shared.log and per-size positive/negative; sourcehashes
native-scene-pixels-source.sha256. Hostedsource is immutable uploaded BGRA
pattern, not a substitute renderer; native Mesa triangle/teapot source ownedby
Graphics remains next fullintegration gate. v28 untouched.

2026-10-02 native Ada bridge component gate complete. Own new tests/compositor/
native_scene_bridge.{ads,adb,h}, native_scene_bridge_{tests.adb,c_test.c},
bridge GPRs, build-native-scene-bridge.py and native-scene-bridge.md.
Proof55740 TERMINAL0:736 results(230flow506prover), zero unproved/justified,
includes dependencies. Final67402 TERMINAL0: nine lifecycle/fault scenarios,
real C->Ada ABI sequence,1000pendingpolls/no resubmit,100clean reopen/cancel/close
cycles, native archive+elaboration built from801hash-verified private inputs.
Header: tests/compositor/native_scene_bridge.h.
FINAL ARCHIVE: tests/compositor/build/native-scene-_g2p_mss/libcubit-native-scene.a
Matching runtime+header+sources are in that independent snapshot. Call generated
compositor_sceneinit exactly once. Archive is Ada only: required C adapters and
link integration details in tests/compositor/native-scene-bridge.md. Graphics
still owns tests/mesa-anv linker/app/barriers/presentation. No graphics edits.

Open borrows immutable same-device affine source through successfulClose;
Begin returns selected1..3 slot for preparation; Record runs production scene;
caller appends readback+barriers before Submit; Poll checks actualfence once;
Release requires CPUconsumerreturn; Close rejects heldready/pending. Code3sticky
quarantine retains ALL objects; timeout never authorizescleanup. SourceBGRA8
SAMPLED +shader-read-only descriptor; sameextent/device, nonaliasing, readybefore
sampling; targetBGRA8 colorattachment+transfersrc.16MiB actualbackingcharge cap.
Firstgate64x64triangle; dimensions12..65535 allowlater256teapot subjectbudget.
No GPUimport/scanoutauthority. No app link/boot/native GPU/pixel/latency claim yet.
NEXT: hosted realMesa exercise of this exact C/Ada bridge, then graphics native
app integration. Allownjobs terminal; index unchanged.

Earlier32651 sourcepath,87701 coordinateconversion,71613 damagebound proof and
48449 compiler/runtime mismatch FAILED then corrected. Use Nix+kernel alrexec
GNAT16 for nativebinding, not plainNix GNAT15; consistencychecks notsuppressed.
64676 intermediatearchivePASS superseded by final67402 exactheader/CABIartifact.

2026-10-02 graphics/Penny handoff inspected. Verified graphics hosted8cycles+
negativecontrol log and native compile/link-only artifact olc3vo5x SHA256
070f2e6ebc3d1451563352af07e7930538aed98058a7e714ba0da91db2cba633. Current smoke
executes production C submission functions, NOT Ada scene policy. No native
execution/physical presentation claim. I own the next compositor-local Ada
scene-recording/native bridge and its archive/header/build fixture; graphics
retains tests/mesa-anv app/image/driver/ANV transport. No peer source edits.

NEXT NATIVE CONTRACT (proposed integration, not an existing public GPU ABI):
Reuse graphics' authorized app VkDevice/queue and immutable app-owned offscreen
triangle source. Compose that texture plus a physical green fill into owned
targets using production Vulkan_Scene/Scene_Recording/Frame/Owned_Targets and
the existing affine Mesa pipeline. C bridge constructs Vulkan requests and
records audited layout/readback barriers; SPARK selects/adopts the writer,
preflights source/context/output epochs, records, queues and polls. Source stays
immutable and retained through the compositor fence. Keep explicit existing
readback presentation for this first gate; readback buffers stay alive until
the existing CPU consumer returns them. This is a same-device app-owned test,
NOT cross-process GPU import or zero-copy scanout. No driver change required
for that bounded first gate; graphics will need a linker/archive hook after
the compositor bridge is available. Preserve v26 and all existing probe modes.
Teapots follow as actual Mesa-rendered source content; no replacement rasterizer.

PENNY CONTRACT: existing protected UI.App CPU frame publication/readback remains
the available path. There is NO stable client GPU-import wire ABI yet. Planned
semantics: explicit recipient/device-scoped import authority and independent
allocation identity; surface/output epoch and non-reused frame identity;
validated extent/format/stride/plane layout/DPI; authenticated producer-GPU-ready
evidence; immutable accepted source until compositor-reader retirement. Frame
acceptance, compositor GPU completion, display latch and final buffer release
are different events. Resize creates a new epoch; old frames drain independently
and stale completions cannot release new resources. Use bounded newest-ready
admission: drop only unconsumed candidates, never overwrite GPU/display-held
buffers. Browser/video producer must receive explicit accepted/deferred/rejected
and terminal release outcomes; no busy-wait or unbounded FIFO is intended.
These are semantic requirements, not promises of current syscalls/endpoints.

Required GPU-import evidence: recipient authentication, rights attenuation,
forged/stale identities, wrong device/owner, resize/death with held readers,
revocation, producer/consumer device loss, overload and independent GPU/display
retirement. CPU RO grants do NOT confer GPU authority. Gen11/12 lack of enforced
RO PPGTT must reject unsupported RO imports or use an explicit permitted copy;
never silently map RW. Graphics retains this transport/VM work; Penny retains
browser/libc/netstack. I will not change browser code. Integration evidence must
separate C boundary smoke, Ada-policy execution, CPU readback, GPU import and
physical scanout. Coordination is recorded here; no direct thread message sent.

2026-10-02 scene-recording complete at component gate:91898 TERMINAL0 nine
recorded-not-ready/preflight/cancel/unknown cases;78889 TERMINAL0 proof702
(211flow491prover)zero unproved incl dependencies;14250 TERMINAL0 realMesa72
physical-fill scenes55296pixels through productionRecord_Scene +repaint-aware
pool/acquire/submit/fencepoll, plus2304RGB/178176affine0validation.95149 TERMINAL0
private nativecompile private-targets-29k_75b_ hashesPASS. Initial91255 visibility
and12429 missingmodular guarantees corrected. No Desktop/sharedruntime/driver
or index edits; allownjobs terminal. scene-recording.md states remaining native
frame/presentation and image/source barrier integration requirements.

2026-10-02 claim production scene-recording assembly: owner context/output epoch
and sealed-source preflight, begin/replay/end, cancel-unsubmitted on known failure,
retain writer/damage on uncertainty. Recorded is distinct from queued/completed.
Own Vulkan_Scene_Recording plus epoch getters/test bridges; no Desktop service
changes or driver/display authority. Hardware driver import transport still absent.

2026-10-02 physicalfill final59768/9233/94712 TERMINAL0: proof558(176flow382
prover)zero unproved incl dependencies;72actualMesa scenes55296exactpixels +
ownedtargets2304RGB/178176affine/18bindingfaults0validation; privateCuBit compile
snapshot private-targets-018sruyz hashesPASS.22688 earlier full712queue546816pixel
regressionPASS before defensive rejection flag fix. First92665 typevisibility,
73422 scaleconversions,90950 unsupportedlocalrecord,82076 staleAccepted defensive
return failures corrected; no failed proof counted. Allownjobs terminal.
No Desktop source/shared build/index edits. physical-fills.md documents remaining
native capture/frame begin/end/source admission/presentation work.

2026-10-02 claim output-pixel scene capture: Desktop Draw_Fill already receives
physical rectangles; Vulkan scene currently assumes logical coordinates. Add
typed physical fill capture and reuse bounded damage replay without a second
DPI/rotation transform. Own Vulkan_Frame/Scene and hosted tests only. Desktop
sources remain frozen while graphics95041 rebuilds consumers; no native rollout.

2026-10-02 final context19069 TERMINAL0: null-context clean backing rollback
and all policy regressionsPASS; isolated native compilationPASS complete target
bundle +both Cboundaries. build/private-targets-pfh2mali contains regular source/
runtime/musl/Mesa header copies, input hashes verified before/after, result.json.
80604 proof578(183flow395prover)zero unproved including unchangeddeps;4072 hosted
actual3targets2304RGB +178176affinepixels,18badbindings,0validationPASS. No jobs
or waiters remain. Shared grant-ABI outputs untouched; no index mutations.

2026-10-02 context binding: Attach captures Vulkan_Submission.Owner_Context;
Close rejects unrelated idle context even with matching outputepoch. Foreign
binding checks native submission device/queue/command/fence before publication.
80604 policy/proof TERMINAL0;4072 actualMesa3target+178176affine+18foreignfaults
TERMINAL0 zero validation. Firstprivate native58239 TERMINAL0 at
build/private-targets-japdqimn with hashes; next rerun includes context binding.
Own Vulkan_Submission getter and target files only; no sharedruntime/driver edits.

2026-10-02 private native compile scope: shared lock remains unavailable during
grant namespace build. New compile-private-targets.py copies compositor/display
sources, CuBit runtime, existing musl/Mesa Vulkan headers into independent regular
files, verifies hashes before/after compilation, writes only private artifacts.
No native linking/staging/boot or grant ABI edits; follows permitted snapshot
workflow for isolated component compilation. Driver GPU import/output authority
handoff remains acknowledged but absent, not inferred from CPU grants.

2026-10-02 final repeatablebundle7099 TERMINAL0: binding13negative+positive,
3ownedtargets2304RGBpixels+178176affinepixels, helddisplayretirement,0validation.
Source hashes vulkan-target-bundle-source.sha256. Allownjobs terminal; no waiter.

2026-10-02 owned-targets51921 TERMINAL0 proof576(182flow394prover) including
unchanged deps, zero unproved/justified.11832 TERMINAL0 expanded pending-GPU/
display-role/epoch/rollback/null/alias/cancel tests.74582 Cbinding13negative+
positivePASS.51247 TERMINAL0 actualhostMesa3ownedtargets2304exactRGBpixels,
heldfront retainsallimages/views, finalclose refunds; affine178176pixels0errors.
First14969 test used nonexistentpoolmethod;23938 missingloop invariant;
75695 objectcollision and16255 missingtestbridge builds failed, allcorrected.
NewC binding is vulkan_owned_target_binding.c (avoids Ada basename collision).
Native36465 TERMINAL1 locktimeout; laternonblocking unavailable. Individualimage
native70682 alreadyPASS Ada+muslC, aggregate nativeGPR remainspending. No jobs
active at this note; repeatablebundle script nowincludes Cbindingfaulttest.
NativeDesktop activation/import/displayauthority stillpending; indexuntouched.

2026-10-02 native70682 TERMINAL0: new image owner compiles against CuBit Ada
runtime and owned-image C compiles with existing musl Mesa ABI command. Native
compile gate nowPASS, not a GPU execution claim. Next own scope: SPARK owned
three-target bundle, tying backing retirement to existing GPU+display-role gate,
foreign image-to-view-request binding and focused failure/lifetime tests.

2026-10-02 owned-image implementation complete for hosted gate.26606 TERMINAL0:
SPARK59(25flow34prover)zero unproved;32typepositions/9failurecases/heldreader/
8slot/staleidentity policy testsPASS.95613 TERMINAL0: production C boundary
faultsPASS; actual Mesa llvmpipe3images/212draws/178176pixels, allchargesrefunded,
0validation.34502 TERMINAL0 full existing712queue546816pixel regressionPASS;
that larger fixture still uses old testallocator. First3520 compile visibility
error and83445 GPR source-dir error corrected before successful runs.
Native80992 TERMINAL1 after60s lock timeout, no native compile ran; no own jobs
remain. New nativeGPR ready for next sharedslot. No Mesa/kernel/runtime edits;
index untouched. Native Desktop wiring/import/outputretirement still pending.

2026-10-02 claim owned GPU images: new compositor-local Vulkan image FFI and
SPARK memory admission policy, independent hosted fault/real Vulkan tests.
Prepare/query before charged allocation; explicit quiescence before release.
No Mesa/kernel/driver/shared runtime edits, no external import or scanout
authority inferred. Existing staged checkpoint remains untouched.

2026-10-02 gradient5459 TERMINAL0 nativefullPASS all4suites: primary/DPI125-150/arrangements/exactcursor-window restoration,4CPU TCG1GiB2sfunctionalsettle. Quit only identifiedownVM afterall4PASS,300sdeadline notsoak.37559 TERMINAL0 Settingsgradient940pixels/20rows exact. NativeMesaactive; source+image hashes unchanged. primary-125.png visuallyinspected genuine125%primary; evidence gradient-color-evidence. Old86977wrongprofile earlymaximize retained49700pixel latechange proof. All ownjobs terminal, sharedlockreleased;384file stagedindexuntouched. GPUimport/outputretirement+completeDesktopGPUcapture remainunfinished.

2026-10-02 color gradient88373/58280 TERMINAL0: combined539proof results0unproved, capacity1/17slots casesPASS; hostedVulkan712queues546816pixels/22832gradientpixels0validation.86977 firstnative observerfailed earlymaximize(default0.8s, omittedfullDPIflags); SAMEVM latecapture changesall49700targetpixels, no productionregressionestablished. Saved late-maximized.json/PPMs and quit only identifiedfailedVM. Rerun with established2s Mesa settling +mixed/primary/arrangement/scaling flags, unchangedbinary, gradient-color-full.log/serial; newhandle next. Evidence copiedgradient-color-evidence; staged384unchanged.

2026-10-02 gradient21935 TERMINAL0 nativeMesaDesktop linked desktop-mesa-none.svc; Desktop+VulkanScene use Color_Run_Last, no pixels/FFI changes.88373 LIVE scene mock+proof;58280 LIVE actualhostedVulkan pixel suite;86977 LIVE bounded sharedlock queue native300s dual-output/DPI/restoration with thisMesaimage. Logs gradient-color-{scene,vulkan,dual}.log and dual.serial; resumehandles, no duplicates. Source frozen, prior384file index intact. Purehelper proof60 alreadyPASS.

2026-10-02 color gradient53871 TERMINAL0:270clipped cases117570exact pixels; flat2160rows1fill/subtle17/fullrange256; existing16777216channel+rowweights/maximalweightbands suitePASS. SPARK60(8flow52prover)0unproved/justified. First41637 wrong executable path terminal127,3856 terminatedonlyownhostedtest due test's quantified postcondition over2Bflatrows; bounded near-NaturalLast case replaces it.21935 LIVE bounded600s sharedlock queue to apply Desktop+Vulkan_Scene substitutions and nativeMesa build; gradient-color-native-build.log. No duplicate job/indexchanges. Caller/mocks and actualVulkan/nativeDPI gates follow application/build.

2026-10-02 claim gradient RGB coalescing: own Compositor_Gradient At_Weight/Color_Run_Last, hosted gradient pixel oracle; subsequent Desktop settingsGradient and Vulkan_Scene Append_Gradient caller substitutions under sharedlock. Merge only exactly equal finalRGB across existing bounded weight bands; no shader/driver/import/protocol change. Aims reduce command pressure before completeDesktopGPUcapture; GPU import/outputretirement still pending acknowledgedownercontract. No index changes.

2026-10-02 optional32510 TERMINAL0 nativePASS90s4CPU1GiB:4denials/5stops/2submissions/1retry/2runningsoftwarechildren with empty slots+Deviceswindow. FailedGPU17179869216 neverexecuted; retry4294967329, directsoftware12884901920. Core procmgr/policy/fixture sourcehashes unchanged from firstboot; packaging noexecstack +oracle shell-status fixes only.40824 policyPASS320+49cases,11SPARK(4flow7prover)0unproved;65463 observerPASS11negativecontrols plus actualrun.sh guard success/rejection. Allownjobs terminal; docs/evidence optional-corrected and optional-final.sha256. CCL optional keyword request stillpendingowner; no Desktop manifest activation/GPUimports/physicaltimingclaim.384staged index preserved. New optional wire support and boundedfresh-child retry are productionprocmgr, tests use rawmanifestbytes.

2026-10-02 optional32510 LIVE bounded sharedlock queue/build/rerun with fixture noexecstack + explicit oracle failure propagation, optional-corrected.log/serial. Prior repair35330 TERMINAL143: intentionally stopped only own queuedflock56491 beforeexecution because /tmp per-user quota left repairscript empty. Recreated script in /home/doc/git/.cubit-fill-tmp; no unrelatedVM/job touched. Production policy/procmgr alreadybuilt; nativeonlyfixture/runner fixes pendingthisjob. First65149 wrapper0 is FAILED evidence, not nativeacceptance. Allpolicyproofs terminalPASS; preserve384fileindex.

2026-10-02 optional65149 TERMINAL0 BUT INVALID TEST PASS: mandatorycases passed; all optionalapps rejected by ELF loader (manifest.o implied executable GNU stack). Oracle correctly failed, new run.sh branch failed to propagate Python status. Corrective run queued next: fixture linker explicit noexecstack + run.sh `if ! python ...; then exit1; fi`, then same90s gate; no productionpolicychanges. Applied production policy40824 TERMINAL0:320+49cases,11SPARK results(4flow7prover)zero unproved;11oracle negative controlsPASS. First native result explicitly NOT acceptance evidence. Logs optional-native.log+serial retained; preserve index.

2026-10-02 optional65149 LIVE bounded600s sharedlock waiter: /tmp/cubit-optional-startup-edit.py applies reviewed procmgr/runtime/run.sh edits only while holding lock, then build+90s4CPU1GiB native optional fixture. Evidence optional-native.log/serial. Prepared preview42018 TERMINAL0:320decision+49incarnation cases,11oracle negative controls, SPARK checks-as-errors PASS (private source preview, not yet production). tests/render-startup/native/{build.sh,main,fixture,init,check,check_tests} new. Shared run.sh opt-in CUBIT_RENDER_OPTIONAL_TEST extends existing required-admission fixture; default fixture preserved. No duplicate waiter or index changes; resume65149.

2026-10-02 claim optional render startup integration: procmgr main and own tests/render-startup/native fixture, under sharedlock. Explicit optional wire request type11/rights3/param0=1/param1=0, existing required param0=0 unchanged. Application-role only; no GPU approval inferred from manifest. Optional unapproved starts software with inspected empty slot; approved failed admission stops captured child then at most one fresh-incarnation software attempt, retaining broker resources. No Desktop manifest activation/native imports yet. REQUEST CCL owner: add `(request-optional-render read-write render)` encoding the same render request with param0=1, reject duplicates across both spellings and non-read-write rights; own ccl-manifests implementation/tests remain yours. Until acknowledgment, native fixtures encode the wire bytes directly and no CCL compiler changes here. Preserve index.

2026-10-02 startup69095 TERMINAL1 after native gate PASS: 70s4CPU1GiB render-launch-policy exactly2denials/1submission/2stops/Deviceswindow, no sentinel. Wrapper post-test hashes: procmgr+Render_Startup unchanged, CCL.Language changed concurrently => exit1, NOT frozen-source provenance claim. Saved before/after hashes incl staged kernel/procmgr/devmgr; native-verified.log+serial. All ownjobs terminal/sharedlockreleased. README records5SPARK/320hostcases, native gate scope and provenance limitation; optional startup/retry stillnot enabled. Prior48690 image-compiler Type_Source failure was owner-corrected without our CCL edits.384file index unchanged.

2026-10-02 startup69095 LIVE same native gate, native-verified.log/serial and before.sha256.48690 TERMINAL1 beforeVM: CCL image compiler Type_Source undefined; current CCL owner source now defines helper before use. No CCL edits here. Rerun records source hashes and checks them after test; sharedlock held by native gate. README +backends proof-boundary documentation added. No optional startup activation/index changes.

2026-10-02 startup native48690 LIVE: shared-lock make procmgr/devmgr/devices/render-launch-policy followed by native70s4CPU1GiB required-admission regression; tests/render-startup/build/evidence/native-retry.log and native-retry.serial.log. Previous59388 confirmed TERMINAL2: procmgr compiled/linked, devmgr missing ccl-streams.ads. Other owner's current GPR already includes dependency; no devmgr edit here. Hosted320cases and5SPARKresults PASS. Added tests/render-startup/README.md distinguishing proved decisions from authenticated observations, fresh-child retry and retained GPU-resource obligations. Optional manifest/retry not enabled. Preserve384-file staged checkpoint; no index changes.

2026-10-02 claim render startup decision policy: runtime CuBit.Render_Startup +tests/render-startup; procmgr mandatory acceptance gate uses proven decision without changing manifest/approval/launcher semantics. Optional policy permits software only with no admission attempt and kernel-confirmed empty slot; failed GPU child muststop before fresh retry. Optional manifest/retry plumbing not enabled yet. Sharedlock coversprocmgr edit/build. No driver/kernel/index edits.

2026-10-02 glyph association75297/native33550/realVulkan34999 TERMINAL0.128slots4096B metadata; typed Append_Glyph requires face/code/equivalent density+livegeneration, forbids live reassociation andduplicate keys/tickets. Combined SPARK report479(164flow315prover), zero unproved/justified; 128capacity, wronglayout/stale/duplicate/recording/sealed/pending/quarantine tests PASS. Real712queues546816pixels/19336glyphpixels pass through typed capture with repeatedsource release/rebind, zeroVulkanvalidation. Earlier fixture duplicateContext+Ada reserved Entry fixed. Ownjobs terminal; staged384 unchanged. NativeGPU images/lease authority andDesktopactivation stillmissing; image metadata truth assumedatforeignboundary. Evidence glyph-source-evidence.

2026-10-02 claim glyph key/source association: new Vulkan_Glyph_Sources reuses existing Glyph_Cache keys/rational density equality; typed Scene.Append_Glyph, mock and realhost bridge integration. Idle-only bindings, generation validation, no live reassociation, bounded128metadata slots. No actual VkImage import authority added; source metadata truthful foreign-owner assumption remains. No index changes.

2026-10-02 glyph68439 TERMINAL0: realhost712queues/546816pixels/96glyphscenes6densities4rotations/19336coveredglyph pixels/zeroVulkanvalidation. Native16907 TERMINAL0. First97977 fixture viewport dimensions wrong;19977 oracle outward-cell vs actualpixel-centre wrong. Corrected fixture only; independentpixeloracle nowPASS. Proof351/mock240 unchanged;24608Bscene. Allownjobs terminal; staged384untouched. Next: nativeglyphcache/import/density association +Desktopcapture. No nativeGPUactivation/physicaltimingclaim. Evidence glyph-scene-evidence/vulkan-glyph-corrected.log +positive +sourcehashes.

2026-10-02 glyphpolicy50651 TERMINAL0:240glyph-scene cases +existing mocks PASS;351SPARK(129flow222prover), zero unproved/justified. Native16907 and hostedVulkan80587 LIVE; source frozen, poll same handles. Glyph_Mask selects existing snapped density raster +cell/scene/damage clipping through affine binding, unchanged FFI and24608B scene. Real glyph pixel oracle stillpending; host suite currently covers616existing texture/fill/gradient/clip cases. No DesktopGPUactivation orindexchanges.

2026-10-02 claim Vulkan glyph scene commands: affine_binding, submission, frame, scene +five explicit GPR lists and submission mock coverage. Reuse Glyph_Placement and Text.Clip; retain source tickets, scene bounds/draw budget and existing FFI. No native image importer/provider changes; no index edits.

2026-10-02 native35475 TERMINAL0: all4 dual-final suites PASS productionMesaDesktop,4CPU TCG1GiB; primary/DPI125-150/arrangements/splitdrag/maximize/exactcursor-background. Quit ownVM afterallPASS, sharedlockreleased. Opaque text backgrounds32->1calls fullbatch;38SPARK zero unproved;1536geometrycases/8847360pixels;11backendfaultcases PASS. Evidence tests/compositor/build/text-background-evidence. Docsupdated, ownjobs terminal, staged384 unchanged. Next GPUglyph capture must use Glyph_Placement.Plan at output density and Text.Clip cell bounds; ordinary Surface scaling would resample raster. NativeGPUimport/targetlease handoff stillrequired.

2026-10-02 native35475 LIVE bounded lock queue then dual-final regression, same text-background Desktop binary; follow handle, no duplicate. Observer exact Settings-return restoration wait applied underlock. Investigating glyph GPU path: use existing Glyph_Placement at output density plus Text.Clip cell bounds; ordinary logical Surface scaling would resample density-rounded glyph raster. Staged index unchanged.

2026-10-02 text backgrounds: hosted1536cases/8847360pixels PASS;38SPARK zero unproved; nativebuild46110 TERMINAL0. Native51967 TERMINAL1 Settings keyboard restoration capture early:58267pixels differ, SAME VM later exact231000-byte region matches. Captured late-head-0.ppm then quit ownVM. Under sharedlock replacing single capture with <=15s exact-region readiness; same assertion/geometry, no production change. Rerun pending; staged384 index untouched.

2026-10-02 text backgrounds: hosted1536cases/8847360pixels PASS;38SPARK zero unproved; nativebuild46110 TERMINAL0. Native51967 TERMINAL1 Settings keyboard restoration capture early:58267pixels differ, SAME VM later exact231000-byte region matches. Captured late-head-0.ppm then quit ownVM. Need sharedlock for bounded15s exact-region readiness replacement and rerun; nonblocklockbusy, no observer edit yet. No own jobs live; staged384 index untouched.

2026-10-02 claim opaque text background coalescing: compositor_text.ads, Desktop drawUIText, new tests/compositor/text_background tests/GPR. Preserve staged384-file checkpoint. Prove contiguity eligibility and rectangle bounds, check scaled/rotated pixel union, then native integration. No GPU provider or other-agent source edits.

2026-10-02 user requested a fresh safekeeping checkpoint before bed. Refreshing the existing index with current tracked changes and new input-retention/native-test sources. Excluding generated build trees and untracked wallpaper assets. No commit or push; no new compositor implementation in this staging turn. Preserve this refreshed index for the user.

2026-10-02 nativeactivation75776 TERMINAL0: rebuiltprocmgr/devmgr/devices andrender-launchpolicy, CuBitQEMU4CPU1GiB70s PASS;2deniedchildren/1admissionsubmission/stopchecks andsurvivingDesktop+Deviceswindow. Evidence native-admission-audit log+serial. This verifies existing mandatory-render/separate-child denial semantics, not optionalDesktopauthority orsuccessfulGPUcomposition. Allrootjobs terminal/indexuntouched. Nextnativeproviderhandoff +Desktopprimitivecapture; graphicsownercontractrequest remains in note.

2026-10-02 nativeactivation75776 LIVE current-source build+render-launch-policy QEMU70s; resumehandle, unique compositor/build/native-admission-audit logs. Further concretegap: anv_cubit_memory.h external-memory/userptr/placed-map callbacks unset; anv_cubit_memory.c rejects imported/scanout BO classes. Read-only presentation forwarding isCPUlinear, not Vulkanimageimport. Graphics handoff request above now backed by exact source; detailed activationgates docs/compositor-backends.md. No production/index edits.

2026-10-02 native activation audit: Desktop manifest has no render request; procmgr rejects/discards every render-request child without Admitted, including timeout/Uncertain. Do not add a mandatory render request to critical Desktop or reinterpret Pending as software-safe. Optional acceleration needs separately admitted worker or a proved optional-admission/late-grant contract. Current GPU presentation export is CPU-linear RO grant forwarding, not a VkImage import/output lease. REQUEST graphics owner: agree native compositor GPU image import + display-target borrow/retirement contract before wiring borrowed Vk handles to Desktop; do not infer GPU authority from CPU grants. Existing native render-launch-policy gate queued to verify software Desktop/Devices survives both denied and unavailable requests on current tree. No production/index edits in this audit.

2026-10-02 clip native98080/14480 and realVulkan1192/68831 TERMINAL0. Final616realqueues/473088outputpixels/943872nonwriter;144newclipscenes6modes6scales4rotations,84512exactclippedbackground/1370exactsolid/22832exactgradient; zero Vulkanvalidation. Firstrealrun lackedexplicitclippedsolid, finaladds5thsceneentry andstrictsolidpixelcheck. Policy342SPARK unchanged; scene24608B unchanged. Evidence scene-clip-evidence finalsourcehashes/native-final/vulkan-final/vulkan-positive. Allrootjobs terminal/indexuntouched. NativeGPUauthorizedadapter/Desktopcompletecapture remainpending; no physical timingclaim.

2026-10-02 clip31416 TERMINAL0: tennewclip cases andallsubmissionmocktests PASS, state24608B unchanged. Scene/frame342SPARK126flow216prover zero unproved/justified; evidence scene-clip-evidence. Clipcmds reuseSurface, physicalscissorintersection retains texturegeometry/gradientorigin and sourceleases. Initial compile visibility fixes andmockwrongdescriptorarg corrected; no production bypass. Nativecompile98080 LIVE queued on shared lock; resume same handle; realVulkan scaled/rotated pixel oracle pending. Index untouched.

2026-10-02 claim Vulkan ordered clip capture: Scene Set_Clip/Reset_Clip using existingLayer geometry, no largerrecords; Frame replay intersects physical damage withclip while retaining fulloriginal texturegeometry. Own frame/scene/testmock/realbridgehost follow-up; source mutations underbuildlock. No nativeGPU authority invention. Prior rootjobs terminal/index untouched.

2026-10-02 native input52447 TERMINAL0: realCuBit loopback event mailbox32slots, sixrefusals/sixexactreports, fortyrefusals/eightnewest+recovery PASS; authorityPID/sequence/payload/buttons, exactkernel rejectioncounter and finaldeadlinewake/no newsource allchecked. Evidence tests/input-pending/build/native.06pF1E/{serial.log,input.sha256}; privateISO, reusedhashedkernel, no productionstaging. First46622 terminal1 fixtureexpectedlatermintedtag whileearlierselfendpointselected; fixedfixtureusesinheritedselfcap, no production/kernelchange. NO HID/crossprocessisolation/hardwarelatency claim. All rootjobs terminal. Index untouched.

2026-10-02 claim native input retention oracle: tests/input-pending/native new private ISO fixture, real kernel loopback mailbox saturation + typed authority stamp + timed wake + production queue, transient refusal/overflow. Privileged disposable devmgr bootstrap only, no production staging/GPU/cross-process isolation claim. All prior ownjobs terminal. Index unchanged.

2026-10-02 native26981 TERMINAL0: full dual-output MesaDesktop gate PASS all4observer suites (primary, scaling125/150, arrangement, exactdrag/cursor/window restoration). QMP quit only after4PASS;300s deadline notsoak. No mailbox refusals thisrun; initial sourcegap1 is explicit registration recovery. Evidence input-pending/build/evidence/dual-final.log/serial/screenshots. HostedactualPS2 refusal/overflow/replacement tests PASS; nativeUSB onlycompile. All ownjobs terminal/sharedlockreleased; index untouched. Next deterministic native source-publication fault gate and continued GPUprimitive/import integration; physical240Hz/1ms not measured.

2026-10-02 hosted actualPS2 driver94878 TERMINAL0: production main/protocol/queue with fault-injected ports+IPC; retry7refusals/finaltimer6exactreports, overflow41refusals/8newestreports+recovery, consumerreplacement dropold6/sendnewstate PASS. Shared deadline policy28results(7flow21prover)zero unproved, PS2+xHCI native19068PASS. Driver owner fixed mixed initializer/native97619PASS; new fullDPI26981 LIVE queued/running, logs input-pending/build/evidence/dual-final.log and serial. Resume26981; no duplicate. Tests/input-pending README/evidence updated. No index changes.

2026-10-02 REQUEST driver owner: native88865 TERMINAL1 before boot: intel_gpu_extent_directory.ads:68:66 components in others choice must have same type (Extent_Entry U64/U32 generic initializer). Please fix in your owned edit; no driver source changes here. USB19068 TERMINAL0: common deadline policy tests/proof and native PS2+xHCI compile/link PASS. No own jobs remain. Full native gate pending directory compile fix; existing360file index untouched.

2026-10-02 continuation: native88865 confirmed LIVE queued (other agent holds build.lock for extent-directory edits). Claim xHCI main/GPR pointer retention integration and common proved retry-deadline selector in Input_Pending, preserving storage/boot-log deadlines. PS2 timed wait to use same selector. All mutations/builds queued under shared lock after current native gate. No kernel/GPU/index edits.

2026-10-02 pointer policy69563/70426 and native3517 TERMINAL0. Hosted FIFO/retry/overflow/reset/wrap tests PASS; strengthened recovery contracts proved (see input-pending/build/evidence/policy-final.log). Native45206 TERMINAL1 observer early secondary scanout; SAME-VM late exact300000-byte region matches, cursor1180,90 correct, no publication loss during drag. VM quit after observer failure. New bounded-secondary predicate edit+native rerun88865 LIVE queued on shared lock; resume this handle. Final policy26checks(6flow20prover) zero unproved/justified, queue792bytes. PS2 retains32 reports and timer retries finalpacket; USB pending. User360file index untouched.

2026-10-02 user safekeeping checkpoint refreshed to360 staged files; preserve index from here. Claim bounded pointer publication retention: new userspace/lib/input/input_pending policy, tests/input-pending, PS2 main/GPR first. Four mailbox rejections in pixel-routing dual-workarea log explain280px drift. Retain exact packets across transport refusal; bounded FIFO, explicit full-queue recovery, timed retry for final packet, consumer-change reset. USB integration remains follow-up. No kernel/driver-GPU changes; no own live jobs.

2026-10-02 gradient native49437 and realVulkan92783/66883 TERMINAL0.472realqueues/9032exactgradientpixels/362496alloutput/722688nonwriter,24addedcases6scales4rotationsheights1..4096,zero Vulkanvalidation. First4781 failed fixture passed negative logical bounds to physicalDamage_Region; transformedcasesnowfullphysicaldamage, corrected. Dynamicreportcasecount fixed. Evidence scene-gradient-evidence/final-source-hashes.json/vulkan-positive.log/native.log; proof78alreadyPASS.
Nativepixelrouting65899 TERMINAL1 attimeout: primarysuitePASS then scalingclickmiss. Actualcursorx360 vsfixture80 (later928vs648) persistent280offset; no rendererfault established, investigate relativePS2/injection/modeldrift rather than timing-only retries. StopattemptfoundVMalreadyterminal, touchednothingelse. All ownjobs terminal; indexuntouched.

2026-10-02 native gradient compile49437 LIVE bounded600s sharedlockwait; scene-gradient-evidence/native.log. Desktop regression65899 alsoLIVE; resume both, no duplicate. GPR15312terminal0.

2026-10-02 scenegradient50566 TERMINAL0: exact per-row/color/band cases+allsubmissionmocktests PASS;78SPARK(13flow/65prover) zero unproved/justified. Evidence scene-gradient-evidence/proof.log/gnatprove.out/sourcehashes. GPR15312 TERMINAL0 dependency edits for3existingprojects applied. Nativecompile+realVulkanpixeloracle pending.
Pixelrouting50243 TERMINAL1: workarea snapshotearly, sameVM latecapture passesdifference; VMquit afterobserverfail. Atomic workarea wait+fullDPI rerun65899 LIVE queued; logs pixel-routing-evidence/dual-workarea. No other ownjobs, no index changes.

2026-10-02 claim scene gradient capture: own Vulkan_Scene ads/adb Append_Gradient lowers proven bands into <=256orderedSolid metadata entries, no pixels/imports/allocations. Fullsnapshotreject on boundedcapacity failure, preserve priorentries. Not yet compiled/proved; GPR dependency edits and tests pending sharedlock. Native pixel-routing gate50243 remains livewaiter. No driver/index changes.

2026-10-02 pixelgate33857 TERMINAL1: Settings reference captured Apps menu, laterreturned Settingscorrect. No pixelrouting defect established; old changed-vs-wallpaper check falsepositive. VMquit afterobserverfailure. Replacement atomic observerpatch/run50243 LIVE queued: actual940pixel defaultSettingsgradient requiredbefore referencecapture. Evidence pixel-routing-evidence/dual-settings.log/serial. Resume newhandle; preserve checkpoint.

2026-10-02 pixel-routing gate33857 LIVE: mixedoutputs+arrangement+primary+scaling,4CPU1GiB300s deadline, existing exact observer. Evidence pixel-routing-evidence/dual.log and dual.serial.log. Build91302terminal0; no other ownjobs. Resume33857.

2026-10-02 pixel-routing91302 TERMINAL0: main now routes putPixel through physical CPU fill with Use_Compositor=false; rectangle fills retain Mesa. NativeMesaDesktop build/link PASS. Previousbinary saved pixel-routing-evidence/before.svc. Need fullmixedDPI regression; no index edits.

2026-10-02 claim pixel routing correction: recent fillRect backend hook unintentionally sends every nativeOutputPass putPixel through Mesa clear+flush. Own Desktop main only: explicit Use_Compositor=false for pixel primitive; existing physical clipping/CPU writer unchanged; normal rectanglefills stillbackend. Native edit/build lockqueued next. No driver/index changes, no performance claim before measurement.

2026-10-02 fullDPI67085 TERMINAL0: all4native observer suites PASS and headless desktop-dual-output gate PASS. Exactdrag/restoration, Settings/arrangement/primary/workarea,125/150%,logicalminimum,cursorrepair,reflow,seams. VM quit AFTER allobserverPASS;300s wasdeadline notsoak. Evidence dual-primary.log/serial/screenshots/settings-125.png, native-gate-source-hashes.json. Native defaultlight Settings gradient940pixels alsoPASS. Strongerquantifiedgradientproof42checksPASS; contracts changed afterbinarybuild, algorithm unchanged. All rootjobs terminal/lockreleased. FullnativeGPUactivation/hardwarelatency stillpending. User348-file checkpoint preserved; allpostcheckpointworkunstaged.

2026-10-02 gradient stronger contract78369/24065 TERMINAL0: isolated then production Run_Last quantified all-rows weight equality, not merely endpoints.42SPARK(5flow/37prover) zero unproved/justified; exhaustive assertion-enabled regressions PASS. Algorithm unchanged; contract/invariant edits after67085build finished and VM booted. Evidence all-rows proof/summary/hashes in gradient-band-evidence. Native67085 remains LIVE, reached primary-selection sequence; no rerun. Checkpoint untouched.

2026-10-02 verified wait:67085 remains LIVE awaiting shared lock; dual-primary.log empty and second-primary patch not yet present, so no new VM has started. Do not duplicate or treat pending as a test failure. Driver supervisor migration owns lock; resume same67085. No new production or index edits.

2026-10-02 dual73102 TERMINAL1: initial/drag/Settings/above/left/below/offset and primary migration progressed; second new-primary maximize snapshot early. SAME-VM new-primary-late.png confirms correct1280x684workarea. Observer edit1617 alreadyterminal0. Own failedVM quit viaQMP. Atomic second-maximize predicate wait +fullDPI rerun67085 LIVE, dual-primary logs/serial. No production change or relaxed predicate; waitbounded15s, wholeVM300s. Resume67085 only.

2026-10-02 observer-edit1617 TERMINAL0 verified actual source. Full native gate73102 LIVE with dual-visible.serial.log/dual-visible.log,300sQEMU4CPU1GiB, mixedoutputs+arrangement+primary+scaling. Updated observer waits actual Workbench pane and both exact dragged fragments; no production changes. Resume73102, no duplicate. All earlier rootVMs terminal.

2026-10-02 dual36717 TERMINAL1: repeated initial capture still placeholder despite first-frame marker; later screenshot actual UI. Confirms publication !=scanout. Exact native Settings gradient oracle37544 TERMINAL0:940pixels/all20rows match independent signed-channel Alloy palette interpolation (check-settings-gradient.py). FailedVM explicitly quit via unique serial-identified QMP. Shared observer edit1617 LIVE bounded60s lock: wait for actual white Workbench source pane before drag, then bounded15s BOTH exact fragment predicates; no assertion removed. Poll1617 before any rerun. All other rootjobs terminal. FullDPI gate still pending; stagedcheckpoint untouched.

2026-10-02 dual63650 TERMINAL1: first-frame readiness fix applied under lock; drag+Settings appearance/display navigation+above arrangement passed. Later maximize screenshot preceded actual scanout; maximized-late.png from SAME liveVM confirms correct full work area. Own VM quit after observer failed. New36717 LIVE atomic observer edit+gate: first-frame counts for subsequent launches, exact maximize pixel predicate now bounded15s wait instead of fixed2s assumption. No production changes; softpipe/TCG correctness only. Evidence dual-pixels logs. Resume36717; no duplicate run.

2026-10-02 correction:24607 terminal1 (observer failure; own VM quit via QMP),58190 terminal1 patch-script syntax before edit/build.10942 terminal143 intentionally canceled while waiting because its nonblocking prerequisite edit did not acquire lock; no stale rerun started. Replacement atomic edit+test waiter will acquire shared lock once driver edit24464 releases. Patch script syntax now validated. No observer source edit yet; inspect replacement handle rather than relaunch.

2026-10-02 dual24607 observer failed before Settings: opened screenshot is initial placeholder, spanning is actual Workbench UI. Serial first-frame follows native-window-ready. QEMU handle24607 still LIVE bounded300s; do not restart. Queued shared-lock waiter58190 will change check-dual-desktop.py readiness from window-ready to first-frame-presented, preserving all pixel assertions, then run dual-ready300s gate. Own this shared observer edit; lock held through mutation and rerun. Evidence gradient-band-evidence/dual*.log and opened.png/spanning.png. Resume both exact handles; do not duplicate.

2026-10-02 native Settings/DPI gate24607 LIVE: dual-output QEMU4CPU1GiB300s, mixed1280x720secondary, arrangement+primary+125/150% scaling, MesaDesktop selected,2s functional settle. Resume same handle. Evidence gradient-band-evidence/dual.log and dual.serial.log. No source edits while build runs; checkpoint untouched.

2026-10-02 gradient97980/70460 TERMINAL0: exhaustive channel and band regressions PASS,40SPARK(5flow/35prover) zero unproved/justified, nativeMesaDesktop compile/link PASS. Run_Last logarithmic search groups equal-weight rows in actual settingsGradient,<=256fills/fullgradient, original unclipped color origin retained. Evidence gradient-band-evidence logs/proof/hashes. Initial1169 compile syntax corrected before finalchecks. No actual frame-time claim; Settings-specific native visual check outstanding. All own jobs terminal, staged checkpoint untouched.

2026-10-02 claim gradient band coalescing: owned compositor_gradient ads/adb, gradient_tests and Desktop settingsGradient. Preserve identical row-weight semantics with bounded search for last equal-weight row; no intermediate pixels. Edits under shared lock after prior packaging lock released. Proof/regression/native compile pending; index untouched.

2026-10-02 interaction16572 TERMINAL0: final Desktop Mesa fill build passes native CuBit QEMU4CPU1GiB180s gate.194673 geometric pixels; resume/full animated retired-buffer cycle/pause/Escape all PASS. Fresh fixture11244 resolved stale executable markers. Evidence desktop-fill-evidence includes screenshot, native oracle, passing policy-v2 log and476-result proof summary (zero unproved), source hashes with provenance caveat. All own jobs terminal; shared lock released. User staged348-file checkpoint preserved; new documentation/coordination changes remain unstaged. No native GPU/hardware latency claim. Next complete primitive capture and authorized GPU import/presentation integration.

2026-10-02 fixture11244 TERMINAL0: current cube/scene/winsys compiled against existing native Mesa libraries into private desktop-fill-evidence. Interaction16572 LIVE (shared lock, QEMU4CPU1GiB180s); resume same handle. Logs/evidence tests/compositor/build/desktop-fill-evidence/window.log and window.serial.log. No production edits or index changes after safekeeping.

2026-10-02 Desktop interaction18668 TERMINAL1: cube pixel oracle PASS194673, but selected Sep28 fixture lacks animation controls required by harness. Rebuild current cube fixture only against existing Mesa libraries, then rerun interaction; no compositor runtime failure established. User safekeeping staged348files; subsequent changes remain unstaged.

2026-10-02 filloracle24594 TERMINAL0 full CuBit QEMU4CPU1GiB240s gate PASS including32newbackendfills/padding, allpriorglyph/color/pool tests andsoftpipebaseline. FinalDesktop build81244 completed but harness TERMINAL1 beforeboot: Nix TMPDIR produced >108byte Unixsocket path. Rerun with short /home/doc/git/.cubit-fill-tmp and explicit native-mesa-cube.app +MESA_WINDOW_SCENE=cube, animation+physicalclient required, livehandle to record after launch. Source finalcallbackconstants rebuiltby81244; no restart due observationtimeout. Proof61210 terminal0 zero unproved.

2026-10-02 liveDesktopfill progress: build93933 TERMINAL0 native oracle+actualMesaDesktop. Hosted61210 TERMINAL0 all7text+4fillfault modes and backendproof476results(123flow/353prover), zero unproved/justified. Initial68336 proof2callback subtraction checks fixed by validated Width/Height constants captured before callback (no suppression). Native oracle24594 LIVE harness, QEMU app already emitted COMPOSITOR-OUTPUT plus COMPOSITOR-NATIVE/SOFTPIPE success; waitfullgate.32newpackedBGRA fill/padding cases run through liveDesktopbackend. Final callback-constant source requires actualDesktop rebuild before interaction test. No sourcebuild currently reading compositor; no otherjobs except24594. Defaultlegacy preserved, Mesa nativeOutputPass hookedfill has knownquiescentCPUfallback/unknownrestart. No performanceclaim, must runactualDesktop interaction before finalvalidation. Stagedcheckpoint untouched.

2026-10-02 claim live Desktop fill boundary: Desktop_Compositor.Draw_Fill +Mesa retained-target implementation, narrow softpipe clear_render_target adapter, nativeOutputPass fillRect hook with quiescent CPU fallback/unknown restart. Own desktop main, backend mesa/legacy bodies, common compositor spec/Mesa binding/FFI/softpipe/header, glyphmock +native oracle. No driver/kernel/ABI/index changes; no GPR edits yet. Previous jobs terminal. Native compile/oracle and Desktop integration verification required before claiming completion.

2026-10-02 solids40278/48615/92368/74106 TERMINAL0: ordered Scene.Solid layers source-free opaqueRGB; invalid source/blend/mask combinations reject. Frame.Replay_Fill uses proved DG.Damage outward transform +target-damage intersection, <=8attempts/sharedbudget/sourcepreservation.317combinedSPARK(124flow/193prover), zero unproved/justified (frame+scene rechecked plus priorresults).7solidcases+existing fill/scene/history/fault tests PASS. Initialtest23111 compile-only negative-literal visibility error fixed. RealMesa448queues/344064outputpixels/685824nonwriterpixels/41514pendingobservations,120solid/BGRA/R8 scenes with24cases6scales/4rotations/nonzero origins PASS; zero validationerrors. NativeAda/musl compile74106 PASS, notGPUexecution. Scene metadata24608B (512entries). Evidence tests/compositor/build/vulkan-solid-evidence/ hashes/logs/proof. All rootjobs terminal/sharedlockreleased. No index/commit/push changes.
Remaining complete Desktop primitive capture includes gradient lowering/native glyph+icon+wallpaper imports and client/cursor command hooks, plus authorized native GPU targets/scanout retirement. Existing Desktop software/Mesa selection unchanged. Driver latest canonical extent-directory migration owns its own sources; no native ABI invented here.

2026-10-02 claim ordered solid layers: Scene Solid kind with no source/descriptor, Frame.Replay_Fill using existing proven output damage transform +bounded intersection with target dirty plan. Real Vulkan mixedscene to include ordered solids and6scales/4rotations. Own frame/scene/test bridge/mock/host/docs; no sharedGPR/driver/index edits. Prior rootjobs terminal.

2026-10-02 fills16044/93098/52097/9727 TERMINAL0: production Vulkan_Submission.Fill_Output now uses narrow CmdClearAttachments dispatch, checked active-pass bounds, shared4096attempt budget, no sourceimage/copy/allocation. Vulkan_Scene captures opaque background and restores dirtyrectangles after complete source preflight then replayslayers; fixture-only layered clears removed. 292combinedSPARK(118flow/174prover), zero unproved/justified (submission+scene rechecked, priorresults retained).7fillcases+12scenecases incl fillfailure/budget stoppinglayers PASS. NativeC13malformedfillguards+9missingdispatch PASS. RealMesa424queues/325632outputpixels/648960nonwriterpixels/33684pending polls, zero validationerrors. NativeAda/musl compilePASS, notnativeGPU. Evidence tests/compositor/build/vulkan-fill-evidence/ sourcehashes/logs/proof; final scene header comment-only clarification afterproof. All rootjobs terminal/sharedlockreleased. Post-checkpoint work unstaged; no commit/push.
Desktop primitive audit: fillRect directCPU, settingsGradient perlogicalrow, icons per-pixel, wallpaper separateCPU painter. Need complete solid/gradient/text/icon/wallpaper/client/cursor command capture and native output/source lease integration; current VulkanScene primitive supports background but not yet arbitrary opaque layers. No Desktop Vulkan selection/driver ABI changes.

2026-10-02 claim production opaque fills: Vulkan_Submission.Fill_Output bounded admission+geometry checking, narrow vkCmdClearAttachments FFI. Vulkan_Scene now restores captured dirty background itself before all layers, replacing fixture-only clear. Own submission/FFI/header/native C, scene, mock/bridge/host/docs. No GPUdriver/Desktop backend selection/index edits. Prior jobs terminal.

2026-10-02 mixed58202/62907/37658 TERMINAL0: sealed scene now captures distinct BGRA+R8 imports; mask coverage/tint, moving/hiding/reorder and full CPU composition pass. Both managed source leases verified retained through recording/pending and released independently afterGPU completion.424queues/325632outputpixels/648960nonwriterpixels/34932pendingobservations, zero validationerrors. Vulkan_Scene.Sources_Ready now part of proved no-added-draws preflight contract;271combinedSPARK(115flow/156prover), zero unproved/justified. Existing scene/layer/history/fault tests PASS. NativeAda/musl compile PASS (notGPUexecution); initial lockbusy followed boundedwaiter37658 completed. All rootjobs terminal/lockreleased. Evidence tests/compositor/build/vulkan-mixed-scene-evidence/ sourcehashes/logs/proof. New edits remain unstaged, safekeeping checkpoint untouched.
Next complete Desktop command capture (solid fills/gradients/icons/text/client/cursor), native GPU-authorized sources/targets and scanout retirement. R8 fixture synthetic; actual font atlas not yet imported. Driver latest extent-directory work remains owned separately; no driver/UI/ABI edits here.

2026-10-02 claim heterogeneous scene imports: actual BGRA+R8 mask contexts captured into one sealed snapshot, both leases held through recording/pending and individually released after completion; CPU multilayer oracle. Strengthen Vulkan_Scene source-preflight contract: invalid ticket/unsealed scene leaves draw-attempt count unchanged. Own scene ads/adb, test bridge/host/docs; no driver/UI/GPR/index changes. Prior rootjobs terminal.

2026-10-02 scene40107/92965/20148 TERMINAL0: bounded Vulkan_Scene snapshot now used by real layered oracle.512layers/20512B metadata, sealed immutable geometry/order/tickets, capacity/lateedit rejection retainsprefix, preflight stale secondsource rejects before any firstlayer draw.10scene cases+collectingoverflow PASS; existing frame/layer/history/source tests PASS.269combinedSPARK(114flow/155prover), zero unproved/justified; new sceneunit checked plus unchanged previous results. RealMesa424queues/325632outputpixels/648960nonwriterpixels/24135pendingobservations, zero validationerrors. Native Ada/musl compilation PASS, not nativeGPUexecution. GPR lock28549 completed; native initiallockbusy then20148 completed. All ownjobs terminal/sharedlockreleased. Evidence tests/compositor/build/vulkan-scene-evidence/ sourcehashes/logs/proof. Post-checkpoint changes remain unstaged; no commit/push.
Desktop audit: drawCurrentScene/live surfaces and backend require synchronous completion. Need capture ALL wallpaper/chrome/text/client/cursor commands, authorized native GPU imports/output leases, asynchronous completion and separate scanout retirement.512-layer overflow must whole-scene fallback/batch, not truncate. Scene tickets context-local, not independent allocation authority; native owner continues import backing work. No fake Vulkan Desktop selection or wire ABI added.

2026-10-02 claim bounded Vulkan_Scene snapshot: source tickets+geometry,512layer cap, seal/no late edits, preflight all generation-tagged sources before first replay. Own new scene ads/adb, submission bridge/mock/host fixture and3GPR source lists under lock. Desktop inspection confirms synchronous live-state renderer; no fake async backend selection or driver ABI edits. Native import/retirement agreement still pending. Prior rootjobs terminal, staged checkpoint untouched.

2026-10-02 layers72248/36957/78034 TERMINAL0: Vulkan_Frame.Replay_Layer now proven bounded8attempts/layer, source preservation, wholeframe reject on bad output/source, foreign failure or frame-budget exhaustion. 234combinedSPARK(105flow/129prover), zero unproved/justified; frame rechecked plus unchanged prior unit results. New eightcase mock failure/empty/saturation/replay tests PASS. ActualMesa424queues/325632comparedpixels/648960nonwriterpixels/31183pendingobservations,96moving/hiding/reordered translucent scenes PASS;26631restoredbackgroundpixels vs73728fullclear (not layered fragment work/latency). Both standalone shader variants PASS. NativeAda/musl compile PASS, not nativeGPUexecution. Evidence tests/compositor/build/vulkan-layer-evidence/ hashes/logs/proof. Initial59480 proof rejected local-record scope before invariant, fixed supported declaration placement; final proof succeeds without suppressions. Native lock initially busy, bounded waiter78034 completed; all ownjobs terminal/lockreleased. Post-safekeeping changes remain unstaged; no commit/push.
Next: complete scene snapshot/enumeration ownership, native target import/retirement adapter with driver owner, Desktop backend selection and hardware latency. Layer replay is production policy exercised by hosted oracle, not yet selected by Desktop. Driver notes show export-retention work in progress; no new wire namespace invented.

2026-10-02 claim layered repaint: owned Vulkan_Frame.Replay_Layer bounded SPARK traversal of captured damage with submission cap/whole-frame rejection. Actual hosted oracle to restore damaged background and replay moving/hiding/reordered translucent layers, independent full-scene pixel reference. Own frame ads/adb, submission bridge ads/adb, vulkan_affine_host.c, mock tests/docs; no GPR/driver/index edits. Prior rootjobs all terminal.

2026-10-02 repaint42549/30887/72737 TERMINAL0: final proof, native Ada/musl compile and actualMesa partial repaint PASS. Proof needed explicit D.Valid conjuncts rather than nested Within predicates (same stronger envelope invariant, no suppressions); Change also proves preservation of all previous Later damage. Combined report213results(102flow/111prover), zero unproved/justified, includes unchanged prior pool/submission/owner results.600CPU pixel-history cycles PASS;328actual queues/96partial opaque scenes/15240vs73728paintedpixels/501504nonwriterpixels/28409pending polls/zero validationerrors. Both shader variants previously4165 PASS. Evidence tests/compositor/build/vulkan-repaint-evidence/ with final source hashes. All rootjobs terminal, sharedlockreleased. All post-checkpoint edits unstaged. Native GPU integration/transparent-layer replay/hardware latency still pending; no 240Hz/1ms claim.

2026-10-02 repaint validation: resumed old handles12216/28505 terminal1 (Box equality visibility compile error), GPR38260 terminal0. Fixed visibility. ActualMesa56613 and final4165 TERMINAL0:328queues,96partial opaque scenes,15240/73728paintedpixels,501504nonwriterpixels, zero validationerrors; both standalone shader variants pass. Native88110 TERMINAL0 Ada/musl compile, not nativeGPU. Independent600cycle pixel-history test PASS. Proof77874 then23897 terminal1; strengthened output-envelope invariant fixed bounds failures, two remaining validity obligations now split by explicit assertions. Proof41493 LIVE, sources frozen. Full goal not complete; transparent layer replay/native output import/Desktop selection/hardware measurements remain. User checkpoint staged311files; subsequent work remains unstaged, no commit/push.
ACK driver-owner target/import audit: docs/compositor-shared-targets.md now makes CPU mapping optional and independent from GPU import/render/scanout, with existing software presentation retained for unmappable GPU targets. No wire ABI or driver source edits by compositor.

2026-10-02 claim owned per-target repaint history: bounded8rect sets per3targets, separate changes arriving whilepainting, completion/cancel/unknown retention; repaint-aware Frame admission and completion, actual Mesa partial opaque-scene replay. No driver/index changes. GPR38260 waiting sharedlock, hosted outputs disjoint.

2026-10-02 target7512/40002/64114 TERMINAL0:135SPARK(78flow/57prover) across pool/frame/submission/target-owner, zero unproved/justified. Production target provider creates3views+3framebuffers around authorized images, no pixel allocations/copies;304B C metadata. SPARK owner requires matchingepoch, source/GPU quiescence and empty writer/ready/pending/front before close; unknown holds/no replay. Pool Discard_Ready preserves other roles and faults stale tickets.100target lifecycles/5initfaults/3closefaults/epoch/source gates PASS. RealMesa6partialrollback points/unknown retention/4missingdispatch/15malformed requests +232queues/178176draw pixels/354048unchanged nonwriter pixels/31446pending polls PASS; zero validationerrors. Native Ada+musl compile (target object no undefined refs), both standalone shader variants and full pool regressions PASS. Metadata target views/framebuffers now production-owned, underlying VkImages still fixture-owned; latch/retirement still SIMULATED. Evidence tests/compositor/build/vulkan-target-evidence/. Build-definition lock waited75171 then completed; independent private test/proof79791/89700 also terminal0, public final checks supersede. All rootjobs terminal/sharedlock released; user staged checkpoint untouched, newwork unstaged.
Next native image lease/import and Desktop backend integration; renderer primitives and resource teardown are ready for authoritative output targets. Driver latest bounded allocation compiled but no writable target ABI agreement yet. Do not reinterpret existing readonly app presenter or source-copy Released responses as scanout retirement. No hardware performance/240Hz/1ms claim.

2026-10-02 claim owned Vulkan target/framebuffer metadata provider +SPARK epoch/quiescent teardown gate; reuse three GPU images, no pixel allocations. Add pool Discard_Ready for output disable, preserve front/pending/writer. Actual fixture will use managed target views/framebuffers. No driver ABI/Desktop selection/index edits. GPR edit75171 under sharedlock; prior rootjobs terminal.

2026-10-02 frame32201/57369/40815/58495/66319 TERMINAL0: combined122SPARK(68flow/54prover) pool/frame/submission, zero unproved/justified. New production Vulkan_Frame couples actual command/fence operations to pool writer, target-indexed scenes, ready publication and uncertain retention. ActualMesa3distincttargets:232submissions/178176draw pixels/354048unchanged nonwriter pixels/61767pending observations; >50SIMULATEDlatches/>100ready replacements, zero validationerrors. Native Ada+musl compile PASS (not GPUexecution). Both standalone affine shader variants pass; hosted existing pool8000ready/15001front/3000copied cycles +frame1000frames/998deferrals/7commandfaults/2overlappingDisplayfaults PASS. Initial56747 proof failed because pool contract did not expose unchanged deferred state; strengthened actual Acquire postcondition and proved it, no weakened guarantees/suppressions. Evidence tests/compositor/build/vulkan-frame-evidence/. All rootjobs terminal, sharedlock released. Newwork remains unstaged; index unchanged.
Next native writable-target/framebuffer provider +actual output image leases and Desktop backend integration. Three actual GPU images/rendering are exercised on hosted Mesa, but latch/retirement signals are model inputs. No physical GPU/240Hz/1ms claim. Target scene bindings require authorized mutually nonaliasing images for matching output/epoch; native Display/GPU contract still pending.

2026-10-02 claim owned Vulkan_Frame adapter: bind acquired pool writer to target-indexed scene, connect actual command begin/end/submit/poll/cancel to pool GPU retention; reuse existing direct-front/latch policy. Integrate actual Mesa bridge and three-target/fault/saturation policy tests. No driver/Desktop selection/index edits. GPRs edited under sharedlock61731; no source changes owned by others.

2026-10-02 provider final50478/9021/28406 TERMINAL0:63SPARK(41flow/22prover), zero unproved/justified;100managed lifecycles/4import/3release faults PASS alongside existing136slot and submission tests. RealMesa provider136slots/6missingdispatch/4allocation-or-lying-result faults/11malformed requests +232queues/178176pixels/298222pending observations PASS, zero validation errors. Native Ada+musl C compilation PASS, provider/control objects have no undefined externals; not native GPU execution. Fixed provider metadata7672B, Ada state value3300B, excludes Mesa metadata/pixels. New vulkan_sources.c/h creates136descriptors once and BGRA8/R8 views per import; actual fixture draws through it. Import_Source/Release_Source are SPARK idle gates; unknown release preserves registrations, no replay; Remove_Source cannot detach managed entries. Final release contract preserves all other registrations. Evidence tests/compositor/build/vulkan-provider-evidence/. Initial58012 failed invalid inline aggregate iterator in test only; replaced named arrays, subsequent checks passed. All rootjobs terminal, sharedlock released; source/driver/index untouched outside owned scope. New work unstaged.
Next writable-target/framebuffer owner and native image-lease/import adapter feeding this provider plus Desktop integration. Provider accepts ALREADY authorized local VkImages; it does not import CPU grants or foreign application handles. Native GPU/physical presentation and240Hz/1ms measurements remain. REQUEST GPUowner: target/import/latch/retirement contract in docs/compositor-shared-targets.md still needs implementation agreement; readonly app presenter is insufficient.

2026-10-02 claim owned Vulkan source provider: fixed136 descriptors, per-import image views, SPARK-gated import/release, actual Mesa fixture and fault tests. No pixel allocation/import authority/driver edits. GPR edit under sharedlock. Prior rootjobs terminal; snapshot index untouched.

2026-10-02 source retention final71784/44982/95073 TERMINAL0:56SPARK(36flow/20prover), zero unproved/justified;136slot capacity/duplicate/null/stale handles +busy/quarantine retention PASS. RealMesa232queues/178176pixels/56760pending observations, source removal deferred through actual recording/sealed/pending; exact context returned after fence/cancel; zero validation errors. Native Ada+musl compile PASS (not GPU execution). Full State value storage2212bytes, no new pixel allocation. Sources now drawn by generation-tagged ticket; Quiescent differs from Can_Destroy(empty). Evidence tests/compositor/build/vulkan-source-evidence/. All rootjobs terminal, no lock. Initial host62474 compile failed shadowed package name V; fixed. Final checks initially stopped by /tmp quota before output; reran with task-owned TMPDIR in tests/compositor/build/source-check-tmp, no unrelated files removed. New work remains unstaged; user snapshot untouched.
Next native descriptor/image provider behind retained contexts +Desktop target/import integration. Existing source table is a proved lifetime gate for borrowed immutable contexts, not VkImage import or descriptor allocation. Provider must retain all referenced resources/aliases until successful removal. Native GPU/hardware tests still required; no acceleration/latency claim.

2026-10-02 claim owned submission source-context table:136slots, generation-tagged handles, idle-only install/remove, retained through recording/pending/quarantine, real Mesa fixture integration. Borrowed source pixels/views/descriptors stay provider-owned; this table establishes controller retirement gating, not native import authority. Source/tests/docs only, no driver/UI/index changes. All previous rootjobs terminal.

2026-10-02 render-pass policy79972 TERMINAL0:43SPARK(26flow/17prover), zero unproved/justified;1000cycles/10000pending/4096cap/21statusfaults/7forbiddenorders. Host11333 TERMINAL0:232actual Mesa queues/178176pixels/203953pending observations, three active-pass cancellations, 8missingdispatch/22invalidpass/3invalidend guards and3accepted controls; zero validationerrors. Saved /tmp/cubit-vulkan-pass-evidence/. Native57234 TERMINAL0: CuBit Ada +musl C compile PASS, no undefined C references; compilation only, not native GPU execution. Prior96101 failed Nix cache permission before compilation. All rootjobs terminal; sharedlock released; source hashes rechecked. Current docs/test guard changes unstaged; safekeeping index unchanged. No driver/Desktop activation; image authority/descriptor retention/native GPU execution still required.

2026-10-02 user renewed safekeeping request: staging current shared source, tests and docs, preserving existing index entries; excluding untracked build outputs, .gpu-* captures, wallpapers and old AML checkpoint trees. No commit/push. One bounded build-lock waiter for staging; no new build/test launched.

2026-10-02 claiming owned submission render-pass ordering: one pass/frame, only in-pass drawing, target-dimension checks, seal only after pass end; cancellation/unknown retention maintained. Actual Mesa fixture will use these transitions. No driver/UI/Desktop activation/index changes. All prior jobs terminal.

2026-10-02 affine40606 TERMINAL0 final summary inspected:155results(15flow/140prover), zero unproved/justified for binding+affine+transform. Final proof saved /tmp/cubit-vulkan-affine-evidence/gnatprove.out and proof-final.log; proof source hashes recorded. Submission final33results/realMesa232queues/178176pixels/nativecompile also PASS. All rootjobs/waiters terminal; no sharedlock. GPU Desktop activation/authority/descriptor ownership/physical presentation/hardware metrics remain. Root staged checkpoint untouched.

2026-10-02 submission64788 TERMINAL0: final33SPARK(19flow/14prover) zero unproved/justified;1000cycles/10000pending/4096cap/15statusfaults PASS. Hosted34492 TERMINAL0: realMesa232queues/178176pixels/204257pending observations, pool writer retained, capcancel+2context mismatches, zero validationerrors. Native53571 TERMINAL0 Ada+musl compile PASS (no GPUexecution). Initial95859 failed C/Ada basename collision; fixed native.c. No pending native work. Affine40606 still LIVE: OS confirms active CVC5 transform Build solver, not stalled/restarted. Evidence /tmp/cubit-vulkan-submission-evidence/. Root index unchanged. REQUEST GPUowner: review docs/compositor-shared-targets.md for writable/importable output leases and separate latch/prior-front retirement; current readonly app-image path cannot activate these renderers.

2026-10-02 claiming Vulkan submission ownership wrapper +narrow native command/fence FFI. One pending submission,4096 admission cap, cancellation only before submission, sticky unknown completion. Connect actual hosted affine path and Compositor_Pool writer retention in test bridge. No production Desktop/driver/UI/ABI edits; staged index unchanged. Previous affine proof40606 still LIVE same handle.

2026-10-02 affine26289 TERMINAL0: both normal/forced-integer-fallback Mesa shader variants PASS232frames/178176pixels each,6scales/4rotations/color/over/mask,16extreme transforms,16missingdispatch/2partialcreate/10badrecording faults; zero Vulkanvalidation. Floating-UV initial62277 failed real125%texeledge; exact rational sampling fixed15449 and full26289. Native93015 TERMINAL0 final normalSPIRV+CuBit Ada/musl compile, not GPUexecution. Proof40606 LIVE samehandle last rechecked after evidence capture (binding/affine progressed; transform pending), sources frozen for proof; no nativejob/waiter/lock. Evidence /tmp/cubit-vulkan-affine-evidence/. Index not modified by root.

2026-10-02 claiming owned Vulkan affine renderer (new vulkan_affine* files/tests): SPARK affine/clip/transform reuse; Vulkan fixed pipelines, sampled color/mask +premultiplied over; borrowed pass/descriptor/target. No driver ABI, shared UI or production Desktop selection. Hosted actual-shader pixel oracle/proof/nativecompile next. Safekeeping index unchanged by root.

2026-10-02 Vulkan-copy hosted94934 TERMINAL0: actual Ada planner/FFI + Mesa lavapipe160submissions,122880pixels,74draws/80empty/6rejected; zero validationerrors. Missing-barrier negative control PASS. Initial64860 TERMINAL1 only diagnostic-spelling expectation, corrected. Proof56328 TERMINAL0:41results(7flow/34prover), zero unproved/justified. Native44855 TERMINAL0: CuBit Ada +existing musl/Mesa C compile, no C undefined references; compile only, not native GPU execution. Evidence /tmp/cubit-vulkan-copy-evidence/. No jobs/waiters/lock. Not selected in Desktop; image authority/lifetimes/affine/blending/native GPU integration remain. All continuation work unstaged; root did not change index after user checkpoint.

2026-10-02 claiming owned Vulkan opaque client-copy adapter + tests/compositor/vulkan_copy* only. Reuse Desktop_Composition planner; narrow borrowed-command FFI, no allocation/import/presentation authority or new driver ABI. Hosted Mesa validation/pixel oracle next; production direct transport remains blocked on target interface. Safekeeping checkpoint staged; all continuation edits remain unstaged. No shared UI/driver edits.

2026-10-02 latest31141 TERMINAL0: complete240s nativeMesa/softpipe gate incl252ready replacements/64simulatedlatches/pixelchecks PASS; bothDesktopvariants compile. Follow-up4CPUworkers120s viewerupdates/6pauses/graph-table/page/refresh/close duringoverlap PASS; allworkersdone/faultscan/inputhash/privatebase PASS. Evidence /tmp/cubit-ready-replacement-evidence/. Sharedlockreleased, no rootjobs/waiters. CurrentDesktop eager acquisition remains defaultFalse; no directscanout/backendtransport activation. Newwork unstaged, safekeeping index not modified by root.

2026-10-02 latest88364 TERMINAL0:8000replacements +existing15001front/3000copy cycles, SPARK28results18flow10prover zero unproved/justified. Native31141 LIVE single sharedlock buildMesa+bothDesktop then240s softpipe and4worker copiedpath /tmp/cubit-ready-native.log. Newnativeoracle252ready replacements; latch/retirement events simulated, renderingrealMesa. Source frozen; previously staged snapshot left unchanged. No production opt-in without new scene work/driver lease transport.

2026-10-02 claiming owned pool opt-in Replace_Ready acquisition for newer scene work: when full, reclaim only quiescent ready allocation, never front/pending. Default eager Desktop acquisition unchanged. Hosted8000 supersession/pixel checks and proof next, then native Mesa producer integration. Do not enable opportunistically in Desktop pump: eager acquisition without new damage could incorrectly discard the last ready frame. No driver/ABI edits or direct-mode activation.

2026-10-02 front28667 TERMINAL0: unchanged binaries passed complete240s native softpipe/compositor suite incl FRONT64/POOL96/NATIVE192 and baseline pixels/triangle/finalfaultscan. Then4workers120s nativeviewer interactions/6pauses/graph-table/page/refresh/close duringoverlap PASS, workersdone/inputhash/privatebase PASS. Evidence /tmp/cubit-front-retention-evidence/. No rootjobs/waiters;sharedlockreleased. This continuation completed pending validation; safekeeping index not changed by root. New front-retention docs/ownnote evidence updates are unstaged. No direct GPUtransport/hardwarelatency claim; fullgoal active.

2026-10-02 front28667 LIVE single sharedlock recheck, unchanged native/compositor binary hashes verified. Prior3732 terminal timeout aftercompositorpass, no liveoldhandles. New240s headless softpipe then copiedpath4worker overload /tmp/cubit-front-recheck.log and .serial. Harness rebuilt boot prerequisites beforeVM; no root compositor source edits. Safekeeping staged index left unchanged by this continuation; only new evidence/own notes to update. Shared UI/runtime/font freeze already released toServo.

2026-10-02 safekeeping checkpoint requested by user: staging shared CuBit source/tests/docs/assets, excluding temporary .gpu-* captures; no commit/push. Native3732 TERMINAL1: new COMPOSITOR-FRONT64 +POOL96 +COMPOSITOR-NATIVE192 all PASS; full100s harness timed out after SOFTPIPE-NATIVE starting, before baseline pixel/triangle markers. Thus fullgate NOTPASS and subsequent overload was not reached. BothDesktopvariants nativecompile PASS. No rootjobs/waiters, sharedlockreleased. Follow-up: run fullnative oracle with adequate time, then copiedpath overload; do not claim complete integration. Policyproof11932/host22346 PASS.

2026-10-02 front11932 TERMINAL0 poolSPARK27results(18flow9prover), zero unproved/justified. Host22346 TERMINAL0:15001front latches/visible-pixel model/3slot exhaustion/9faults+duplicate; legacy3000cycles, actualDisplay2000 andcursor1000 frames+mutants PASS. Native3732 LIVE sharedlock: nativeMesa oracle extends64simulated latch transitions with real importedtarget writes, bothDesktopvariants rebuilding, then softpipe+4worker copiedpath. /tmp/cubit-front-native.log. Sources frozen. Driver latch signals remain simulated in oracle; no directscanout activation.

2026-10-02 claiming owned Compositor_Pool front role extension: distinct visible front and pending Display ticket, explicit paired latch/prior-front-retirement and final-front retirement, defer Acquire when allthree buffers held. Preserve current copied-source Retire_Display behavior and front invariance. Pure policy proof/independent ownership model first; no Desktop wire/driver/ABI change or claim direct mode activated.

2026-10-02 completion22919 TERMINAL0: bothDesktopvariants compile,4CPUworkers120s,27updates6pauses/graph-table/page/refresh/close during overlap, allworkersdone/faultscan/inputhash/privatebase PASS. Sharedlock released, no rootjobs/waiters, Servo notified. Evidence /tmp/cubit-completion-budget-evidence/. Proven completion count/time admission integrated into Desktop; source routing/fence semantics unchanged. GPUtarget contract proposal remains pendingowner agreement. Testedbinaries built butnotpromoted; no commit/push.

2026-10-02 completion19445 TERMINAL0: forcebuilt existing8000dispatch scenarios +3mutants and scheduling SPARK29results(15flow14prover) zero unproved/justified. New7000actualadmission+3mutants and10000policycycles pass21668 before oldfixture cache reuse failure; -f fixed underlock. Native22919 LIVE sharedlock: bothDesktopvariants compiled, UI/runtime freeze released toServo; private4CPUworker viewer running /tmp/cubit-completion-budget-native.log. Targetlease concrete proposal docs/compositor-shared-targets.md pending GPUowner agreement; no driver edits. REQUEST GPUowner: review front/pending separation, three exported/importable targets, separate latch/old-front-retirement evidence and disable retirement; current Published/Released ABI cannot safely alias active scanout storage.

2026-10-02 audit found Desktop collectPresentations drain-to-empty unbounded under producer refill. Claim owned Dispatch_Budget completion phase (64count/500us admission), Desktop collectPresentations hook and actual-admission fixture. Stop malformed polling immediately after quarantine. No GPU/driver/ABI edits. Native builds under sharedlock; no shared UI changes. Direct target audit additionally requires separate latched/retired events and front+pending pool roles, not just forwarded grants; contract follow-up remains.

2026-10-02 backend-target83332 TERMINAL0:4CPUworkers120s,26updates6pauses/graph-table/paging/refresh/close during overlap, allworkersdone/faultscan/inputhash/privatebase PASS. Normal44902 +actualhelper59277/asyncrouter62769 +policyproof33728 PASS. All root jobs/waiters terminal; sharedlockreleased. Evidence /tmp/cubit-backend-target-evidence/, docs tests/compositor/backend-targets.md. Backend-target policy now integrated into Display, no extra pixelbuffers/queue, no direct Desktop GPUlease yet. Tested binary remains build/display.svc, staged Display restored; no commits/pushes.

2026-10-02 backend-target44902 TERMINAL0: native fallback cursor4moves/exact restoration +resize3cycles48000pixels and both100sregressions PASS. Native virtio primary Observatory39updates20pauses/graphs/table/page/refresh/close PASS; allinput/privatebasehashes match; stagedDisplay restore/cmp PASS. Evidence /tmp/cubit-backend-target-evidence/. Final4CPUworker gate83332 LIVE single sharedlock /tmp/cubit-target-overload.log; production sources frozen. No pixel storage/queue growth; direct Desktop target lease still unimplemented.

2026-10-02 backend-target integration: 59277 TERMINAL0 native Display compile + actual 2000-frame helper/sealed-IPC/busy/failed-state checks and both mutants. Policy33728 10000 flips + SPARK13results (7flow6prover), zero unproved/justified. Actual async router62769 TERMINAL0 two-output routing/10faults/duplicate/flood bound. First native72049 TERMINAL1 screenshot observer consumed stale reused serial name before new socket; native runner itself passes, stagedDisplay restored. Retry44902 LIVE single sharedlock, fresh target-lifetime-v2 names; /tmp/cubit-target-native-v2.log. Sources frozen during native gates. No driver/GPU ABI changes; source copy remains.

2026-10-02 backend-target audit: GPU framebuffer grants are retained writable by Display and currentGPUdriver APIs expose two indexed backing targets, not Desktop writer leases. Direct aliases would conflate source release with backend target reuse. Claim new lib/display CuBit.Backend_Targets SPARK policy and narrowDisplay prepare/submit/completion/clear integration +ownedtests. Keep active/inflight targets unwritable; failclosed on uncertainflip. No driver/kernel/protocol changes. Future forwardable-target/retirement contract needs GPUowner coordination before zero-copytargetexport.

2026-10-02 Display30572 TERMINAL0: combinednewDesktop+Display under4CPUworkers120s passes26updates/6pauses/graphs/table/paging/refresh/close whileallactive, allworkersdone/faultscan/inputhash/privatebase PASS. Counter40924 TERMINAL0: nativecopy reduction +unchangedbaseline negativecontrol pass. 45412normalcursor/resize/viewer gates andstage restore/cmp pass. All rootjobs/waiters terminal;sharedlockreleased. Display policyreuse no newGPUprotocol/fencebehavior; 98.75%lessrepairtraffic in fixedworkload, sourcecopy/uploadremain. Source changes+testedbinaries ready, no staging promotion/commit/push. /tmp/cubit-display-repair-evidence/ and tests/compositor/display-repair-overlap.md.

2026-10-02 Display45412 TERMINAL0: nativecursor4moves+resize3cycles48000pixels+normalfaultscans PASS; viewer39updates20pauses/graph-table/paging/close PASS. Source/binary/privatebase hashes pass; priorstagedDisplay restored/cmp. Actual2000frame helper+2negativecontrols pass. Native Displayrepairbytes159756320baseline ->1999520new (~98.75%less, comparableworkloadnotclockbenchmark); sourcecopies/GPUuploadsremain. Retained /tmp/cubit-display-repair-evidence/. Combined4worker overload nextlivehandle in /tmp/cubit-display-repair-overload.log; sources frozen, no otherchanges planned.

2026-10-02 Display45412 LIVE sharedlock nativev3: forcebuilt2000frameactualhelper+2negativecontrols PASS, DisplayAda2022compilePASS, nativecursorpixeloraclePASS pendingrunner/resize/viewer. /tmp/cubit-display-repair-native-v3.log; trap restores previousstagedDisplay. RP/desktophashes matchprior85-resultproof/integration baseline. No i915/GPUprotocol/lifetime edits.

2026-10-02 Display56030 TERMINAL1 beforebuild: negativefixture recompiled tooquickly reusedcached binary; forced -f. Retry83580 TERMINAL1: actual2000frames+bothmissingrepair/redundantcopy mutants correctly rejected, nativeGPR lackedAda2022for reusedpolicy. Adding -gnat2022 underlock thennewnativebatch. PreviousstagedDisplay restored/cmp bytrap. No policychange from passedproof.

2026-10-02 Display34759 TERMINAL0:actualprepare/flip2000two-outputframes exactpixels/minimalcopybytes/activeandotheroutputisolation/failedflipretentionPASS. Initial88272 compileerror test-only Integer_Address conversion fixed. Native56030 LIVE single bounded lock waiter initially for adding fixturemutants thencompile/cursor/resize/viewer; /tmp/cubit-display-repair-native.log may notexistuntil firstlockedit proceeds. OwnDisplaymain/gpr andRP source frozen. No i915 edits or staging yet.

2026-10-02 claiming narrow Display.prepareGpuRect copy-elision integration and display.gpr compositor source dependency, plus own tests/compositor actual-helper/native fixtures. Reuse proven RP.Before_Draw with empty cursor to copy previous-minus-new damage only. No i915/GPU protocol/lifetime edits. Other notes show no active Display owner/edit; hold sharedlock through changes/build/tests.

2026-10-02 repair17271 TERMINAL0: optimized Desktop with4CPUworkers120seach passes26updates/6pausecycles/graphs/table/paging/refresh/close whileall4active, allworkersdonepositivechunks/faultscan/hashes/base PASS. 56545cursor+resize+normalnative gates PASS;62790countercheck terminal0;92087actualfastgate15casesPASS. No rootjobs/waiters,sharedlockreleased, UIcompilefreeze releasedearlier. Newsoftwarewriter repairsubtraction is proven and natively integrated: 532repairpx/fastframe vs420730.87 baseline (99.87%area reduction), not hardwaretiming. Evidence /tmp/cubit-repair-evidence/, docs/tests compositor repair-overlap.md. Binaries builtbutnotpromoted; stagedDesktop unchanged. Next: avoid Display previous-damage copy overwritten bynewdamage; no i915/Displayedit yet.

2026-10-02 repair56545 TERMINAL0: bothDesktopvariants compile; nativecursor4moves/exactscanout+resize3cycles/exact48000pixels+standardfaultscans PASS; Observatory39updates20pauses/graph-table/paging/close/inputhashes/base PASS. New actualfastgate92087 TERMINAL0 15invalid/partial/scaled/occludedcases. Nativefast intervals before78frames32817008repairpx vs after77frames40964repairpx (532/frame), unchanged425600redrawpx/frame. Checker62790 running then terminalpoll; baseline artifact retained. Overload17271 LIVE single sharedlock private4CPUworkers+newDesktop; /tmp/cubit-repair-overload.log. No source edits planned while running.

2026-10-02 repair56545 LIVE single sharedlock native job: build metrics-on/off, cursor+resize exact-pixel gates (private explicitbase), Observatory workload. /tmp/cubit-repair-native.log; baseline metricsbinary retained /tmp/cubit-repair-baseline-desktop.svc for comparison. Actualhelper78360 TERMINAL0:1000target+scanout frames incl redraw/cursor-only/drag and both stale-display/history mutants rejected. Owned compositor source frozen; no Display/SDK/UI edits.

2026-10-02 repair33322 TERMINAL0: bounded five-piece subtraction replaces limited whole-rectangle elision (48816 caught reduction target failure, pixels correct).8192exactcoveragegrids +600retained/cursorframes PASS:25168repairpx vs369248baseline. SPARK85analysisresults0unproved/justified incl arbitrary-point exactcoverage, disjoint/subset geometry. Claimactualcursor-helper fixture adaptation +optionalprivatebase in own cursor/resize runners; scripts edited underlock. Native+actualhelper tests next; production fast-client guard now requires valid full-size source to guarantee skipped background is redrawn.

2026-10-02 claiming owned Compositor_Repaint repair-selection policy and Desktop repairDirectWriter hook. Skip stale-area repair fully covered by guaranteed imminent redraw, retaining cursor intersection for clean underlay. No elision for drag/split redraw or cursor-only work. Proof + independent pixel-history + native pixel checks next; no Display/SDK/UI edits.

2026-10-02 stage48297 TERMINAL0:120s4CPU CCLworkspace/faultscan/hashes PASS; realcollector+CCL validates all4stage names/kinds/units/positivecounts/no loss, release3frames5batches checkpoint. SameDesktop nativeviewer39six-row updates/20pausecycles/graphs/table/paging/close PASS. Off71780 TERMINAL0 nativecompile; unstaged. Alljobs/waiters terminal, lockreleased. Evidence /tmp/cubit-stage-metrics-evidence/, -native.log, -off-build.log. User prioritizes compositor tuning now; no further metrics UI expansion. Next owned investigation: repairDirectWriter redraws retained stale areas then flushFrame may paint overlap again. Display prepareGpuRect also copies previous damage even if fully replaced by new damage; READ-ONLY finding, no Display ownership assumed. Need coordinate any Display change. No commits/pushes.

2026-10-02 stage64334 TERMINAL0:40840codec/clock cases,1000batch lifecycles,actualtwo-page114admitted/886drops and18actualDesktopadapter modes PASS. Stage22+batch21SPARKresults zero unproved/justified. Native48297 LIVE sharedlock build +120sCCLworkspace all4stage assertions +privateoverride Observatory run; /tmp/cubit-stage-metrics-native.log. Ownsources frozen. User requests compositor work resume once metrics suffice; no further metrics UI scope planned.

2026-10-02 stage metrics: claiming owned Desktop timing/publisher hooks and new Compositor_Stage_Metrics SPARK encoder. Four handler/draw/submit duration series, existing output release keys preserved; six declarations per batch, same two SDK pages, no queue growth. Editing under shared build lock; hosted proof/adapter and native verification follow. No SDK/service/kernel/UI edits.

2026-10-02 overload16440 TERMINAL0;sharedlockreleased,no rootjobs/waiters. Native4CPU TCG +4priority3CPUworkers each120s,Desktop4/collector2/viewer3.26nonemptyupdates overall,6visiblepausecycles,graph/table/paging/refresh/close PASS; serial-order oracle>=10updates+6pauses+close whileall4workersactive. All4uniquePIDs finishedpositivechunks;finalfaultscan/inputhash/privatebase PASS. New test-only loadfixture/--load; no production/app/UI/staging edits. /tmp/cubit-observatory-load-native.log and -load-evidence/.Functional overload evidence only, no latency/FPS/hardwareclaim.

2026-10-02 preparing actualnativeviewer overloadgate:4priority3CPUworkers (120s each),Desktop4/collector2,viewer3. Own newtests/observatory-metrics/load + --load fixture; require6pausecycles,>=10updates andclose duringall4workersoverlap, thenallworkersfinish/no faults. No production/app/UI edits; binaries/inputs hashed,privateimagesonly. Native singleboundedlockwaiter next.

2026-10-02 formatting52241 TERMINAL0 five native gates PASS: full16row38updates,production39updates,both20visiblepausecycles+graph/tablepixels+close; envelope,row,heldgrantlateReply each11updates/stalestatus/no revival/input/close. Inputs/base unchanged,faultscans PASS.306hostchecks+22SPARKresults0unproved/justified. Native cachecommitswholepages, one formatted row/turn,inputdrainbeforeeach; no queued secondquery. Evidence /tmp/cubit-observatory-format-native.log and -evidence/.4933 reporting-only correction terminal0: full is normaltest, notfaultinputflag; rawreports retainedwithannotation. No rootjobs/waiters,sharedlockreleased. No coreUI/SDK/service edits. GPUhardware/physicalmeasurements/rawcapture+stacks/overload remainopen.

2026-10-02 format52241 LIVE sharedlock full16row +normal+3fault native gates.20203 TERMINAL0:306tests+22SPARKresults0unproved/justified. Nativeapp formatting one row/turn (3CCL evals, finalunit+1), completiondrain beforeeach, stagedtextcache commitsallfields onlywhencomplete; no secondquerywhileformatting. Extra fixedcache~4.8KiB. Dirtypaint includedretrydeadline. No sharedUI/runtime/service edits; fullfixture privatecopiedcollector synthesizes16validkeys (notactualpublishedseries). /tmp/cubit-observatory-format-native.log.

2026-10-02 claiming bounded incrementalCCLformatting: new Observatory_Format_Budget SPARK policy+hostedtests; nativeapp stagingcache so one row/eventturn, inputdrainbeforeeach, atomic visiblepagecommit. Existing all16row burst identified insource; no sharedUI/service edits. Currentnewsourceonly, GPR/scripts underlock next.

2026-10-02 graphs41578 TERMINAL0 all4native cases PASS: normal39nonemptyrefresh/20visiblepausecycles/graph-tablepixelrestore/close; envelope,row,heldgrantlateReply each11updates thenstale/input/close. History25743 PASS519015checks+25SPARKresults0unproved/justified.33947 finalbarpixeloracle passed exactcapture andmissingbarmutants;31530 stablewarningfooter passedall3captures andrejectedoldLivefooter. Evidence /tmp/cubit-observatory-graphs-evidence/, graphs-verified.log. No rootjobs/waiters; sharedlockreleased. No SDK/coreUI/service edits. CORRECTION toearliernotes: visual stale-footer suspicion was mistaken; original90614 pausedfooter SHA matches final65972 byte-for-byte, faultfooter isredandstable. No renderingbug reproduced. Docs corrected. Graph screenshotshowntouser. Overload/paginationincarnation/rawtrace/stacks/GPUhardwareperf remainopen.

2026-10-02 graph41578 LIVE sharedlock finalnormal+envelope/row/stall native gates, ownUIapp rebuild; no coreUI edits. History25743 TERMINAL0:519015checks,25SPARKresults(20proof+5flow)0unproved/justified.64point ring,fullseriesidentity/reset,pausegap; exactproportionalinteger scaling replaces misleadingceil-div approximation and unresolvedconversion proofs. /tmp/cubit-observatory-history-proportional.log. Native79794 earliergraphstestPASS predatesfinalscaling;78393 stoppedonproof beforeboot. /tmp/cubit-observatory-graphs-verified.log forcurrentgate.

2026-10-02 fault80571 TERMINAL0: native envelope,row,held writablegrant+late reply allPASS;11nonemptyrefreshes thenfault12, staleview/input/close survive,no revival. No SDK/service edits (test-only copied collector). Graph history23649 hosted/proofLIVE disjointoutputs; userrequests usefulnativegraphs, preparing cumulativep99+new samples perrefresh with64point ring andselectedseries identity/reset. Native UI app sourceonly; no sharedUI edits.

2026-10-02 viewer fault tests: own isolated copied metricsvc fixtures (envelope,row,held acquisition/late reply), production service unchanged. Preparing test-viewer --fault under sharedlock; actual data for11nonemptyqueries thenfault12. No production edits planned. Root has NO required/current artifacts orjobs under .build-workspaces; its evidence andgeneratedVMs are /tmp, source inmain checkout. Graphics cleanup request acknowledged here (no chat-message authorization).

2026-10-02 visible Observatory65972 TERMINAL0,sharedlock released,no root jobs/waiters. New userspace/apps/observatory native CCL+async collector UI,protected DPI-aware frames,16rows,1s refresh,pause/page/refresh/Escape. Final39refreshes+20visible pause/resume cycles+pausedpixelstability+close/faultscan/inputhash/baseunchanged PASS. /tmp/cubit-observatory-viewer-final.log and -final-evidence/, paused screenshot shown. No staging/SDK/service/coreUI edits.90614 initialtest passedbutvisualreview staleLive footer; improvedfooterpixeloracle,25778 diagnostic/70636 sixcycles passed; final diagnostic removed. No rootcause assertion; stronger testretained. Initial89640 GPR missingdisplay/allocator corrected. UI fault/overload tests, schedulingproof/refinement, rawtrace/flames andhardwareperformance stillpending.

2026-10-02 viewer90614 LIVE sharedlock: native compile existingUI (sourcefreeze requestedServo), then privateISO/disk interaction/screenshots. Initial89640 TERMINAL1 missing GPR display/allocator dirs, now corrected. New observer Cleanup_Pending exposes grant cleanup so failed observer can sleep once retired; no SDK/runtime edits. /tmp/cubit-observatory-viewer-native.log. No production staging.

2026-10-02 claiming new userspace/apps/observatory and own viewer test: actual CCL-backed fixed-size DPI-managed window,16row private cache, async input+collector, one query,1s refresh, pause/page/close. Source-only work while Servo21701 owns native lock. No UI/shared source edits. Build GPR/helper and native gate follow under lock; no compile/PASS yet.

2026-10-02 native CCL37281 TERMINAL0; shared lock released, no root jobs/waiters.120s4CPU workspace/faultscan PASS; actual live collector->retired page->SPARK cache->CCL composed name/p99/unit and sample/loss checks PASS,9queries/18turns/3frames/batches. Missing observe authority rejected; clear removes availability.138source/binary inputs/privatebase unchanged, prior stagedobserver restored/cmp PASS. /tmp/cubit-observatory-native-ccl.log and -evidence/.72hosted tests,41analysis results (30prover+11flow) zero unproved/justified documented. No coreCCL/SDK/service/UI edits. Visible viewer, rawtrace/stack pipeline, GPU/physical performance still pending.

2026-10-02 native CCL37281 LIVE: compiled and booted; serial records PASS live summary expressions,9queries/18eventturns/confirmedretirement,Desktop3frames/3batches. Awaiting same120s workspace/faultscan and inputhash verification; no final PASS yet. /tmp/cubit-observatory-native-ccl.log; evidence /tmp/nix-shell.NyKAxJ/cubit-observatory-native.k2YECL. No UI/core CCL edits, own source frozen.

2026-10-02 CCL binding hosted72 checks PASS, proof78501 terminal0:41 analysis results (30 proved checks+11 flow), zero unproved/justified. Preparing native observer+CCL evaluation gate in own native-check; changes GPR/run-native under single build-lock acquisition, then120s privatebase integration. No SDK/service/UI/core CCL edits. Tests CCL expressions against actual retired collector rows and denied missing authority; not yet visible viewer.

2026-10-02 native async11373 TERMINAL0, sharedlock released, no root jobs/waiters.120s4CPU ccl-workspace+finalfaultscan PASS; actual observer15queries/29eventturns/confirmedgrantretirement; Desktop3frames/3batches zero loss/reject/gaps.14inputs/privatebase hashes unchanged, staged oldobserver restored and cmp verified. /tmp/cubit-observer-native-v2.log and -native.serial. New repository run-native.sh reproduces privatebase/trap/hash orchestration; syntaxchecked. First87563 missingbase preboot only. CCL UI and rawcapture still next; no production UI/SDK/service edits.

2026-10-02 async87563 TERMINAL1 after successful native observer link; headless rejectedmissingdefaultbase before boot. Stage trap restoredtestobserver. Retry privatebase launched via /tmp/cubit-observer-native-v2.sh, one bounded sharedlock waiter; /tmp/cubit-observer-native-v2.log. No source edits.

2026-10-02 async observer87563 LIVE single bounded lock waiter after Servo48871. /tmp/cubit-observer-native-gate.log empty. Learned defaultnvme_disk.img absent; prepared separate /tmp/cubit-observer-native-v2.sh using private verified ext2base for followup only after87563terminal (no duplicatewaiter). Current attempt may compile then failmissingbase. Native sources frozen.

2026-10-02 native async observer gate prepared: new tests/observatory-metrics/native-check and builder; real metricsSvc/Desktop release growth via async CQEs and independently retired writable grants. One bounded lock waiter after Servo v29;120s4CPU ccl-workspace finalfaultscan plus own PASS/hashchecks. Temporarily replaces staged testobserver with trap restoration, no production SDK/service/UI edits. Own observatory sources frozen through build/run.

2026-10-02 observer14461 TERMINAL0, sharedlock released, no root jobs/waiters. New async observer actualadapter hosted1069checks PASS; compile against real native runtime PASS. One volatile4KiB page, sharednonreusedtokens,250msdeadline, reply->revoke->confirmedretirement->validate->Take. Timeout/invalid/close disable; no wait loops; clear only privatepage before regrant. Lifecycle64359 proof10checks0unproved/justified; earlier summary/query19 separate. Logs /tmp/cubit-observatory-observer-native-final.log and -observer-final.log. No SDK/service/UI/staging changes. Native IPC integration and CCL/viewer remain next, not yet verified.

2026-10-02 observer adapter5761 TERMINAL0 hosted1067checks, lifecycle10proofchecks0unproved/justified. New owned async observer reuses Compositor_Requests, one4KiB page, completion->revoke->independent retirement->decode, deadline quarantines. Native compile52442 LIVE single bounded sharedlock waiter; /tmp/cubit-observatory-native-compile.log empty atpoll. No SDK/runtime/UI/service edits. Source frozen for compile; pending improvement: zero safely retired page before regrant to reject stale tails after incomplete writes, then rerun targetedtest/nativecompile. No native execution/UI integration yet.

2026-10-02 query49905 TERMINAL0, no native jobs/waiters. New owned Observatory_Metric_Queries proves reply-envelope/work bounds/cursor progress/all-used-row validity; summary expression moved to private spec for cross-unit proof. Hosted2105checks including actual32series/3page collector traversal PASS; combined19SPARKchecks0unproved/justified. /tmp/cubit-observatory-query-verified.log. Asynchronous IPC/grant lifetime adapter and CCL frontend still absent; SDK/services/UI unchanged. Graphics side-chat reports stale544byte boot-logs reader vs800byte logstore; owns rebuild/image fix, no root overlap.

2026-10-02 graphics/logstore coordination: root child /root/observability is completed and design-only (docs/observability-streams.md); metrics/logstore implementation was supplied by a separate user agent, not a live root child. Root currently edits only new userspace/lib/observatory summary/query consumers and tests/observatory-metrics; no logstore, boot-logs, SDK or kernel changes/jobs. Existing implementation ownership is recorded lower in observability.md. No ownership claim by root over diagnostics/logstore repair.

2026-10-02 observatory summary38225 TERMINAL0, no native jobs/waiters. New owned userspace/lib/observatory summary decoder reuses metric codec, preserves64bitwords, rejects malformed identity/metadata/stats/reserved fields; no SDK/service/UI edits. Hosted actualMetricStore+negative cases64PASS; proof12checks0unproved/justified. tests/observatory-metrics README boundaries: no IPC/UI/rawtimeline yet. Earlier52454 unproved metadataaccess and79397 annotation syntax resolved by expressionpredicate, no assumptions. Screenshots baseline/openmenu from Mesa normal run converted losslessly toPNG and shown; user requests ongoing screenshots.

2026-10-02 partial-text19317 TERMINAL0, shared lock released, no root jobs/waiters. Native metrics+Mesa one-glyph partial-write fault causes exactly one software scene replay, retained software recovery, no restart. Seven services/three keyboard menu cycles/exact252000pixel restoration PASS;12inputs/base unchanged; no fault/quarantine. Prior legacy+metrics stage restored and cmp verified. /tmp/cubit-mesa-partial-native.log and /tmp/cubit-mesa-partial-evidence. UI agent visual-only source window released with previous ACK.

2026-10-02 claiming metrics+Mesa partial text failure native gate. Existing C shim fault commits one glyph then reports quiescent error; test requires one scene replay, retained software recovery, no restart, three menu pixel restoration cycles. Scoped test/helper edits under lock; fault artifact now metrics-suffixed, normal stage restored in trap. No production renderer or Mesa source edits.

2026-10-02 partial-text19317 LIVE single bounded build-lock waiter. Installer/test not yet started (log empty); same handle confirmed live. /tmp/cubit-mesa-partial-native.log. Will build isolated text-fault+metrics artifact, boot three menu cycles, restore prior stage. No production source edits.

2026-10-02 Mesa retry65804 TERMINAL0, shared lock released, no root jobs/waiters. Exact normal startup profile seven services + real Mesa retained-mask text/no fallback +3keyboard menu cycles/exact252000pixel restoration PASS. Twelve recorded inputs/base unchanged, no faults/quarantine. Legacy+metrics stage restored and cmp verified. Evidence /tmp/cubit-mesa-normal-evidence and /tmp/cubit-mesa-normal-retry.log. First46547 one-second timeout retained; actual pixel-transition deadline corrects fixture only. TCG software runtime validation, NOT240Hz/hardwarelatency evidence. Servo notified.

2026-10-02 Mesa boot46547 TERMINAL1: real Mesa text active, initial one-second menu screenshot unchanged. Prior staged Desktop restored/compared. Retry65804 LIVE single bounded lock waiter; fixture now waits up to30s for actual open/close pixel transitions, preserving oracle. /tmp/cubit-mesa-normal-retry.log. First evidence /tmp/nix-shell.S7H9tq/cubit-normal-session-v57o8c8t. No production edits; no PASS yet.

2026-10-02 Mesa normal-profile native gate prepared: normal-session fixture now selects exact legacy/Mesa metrics artifact; Mesa requires retained-mask text marker and rejects fallback. Three keyboard menu cycles/pixel restoration and seven-service/fault checks retained. One bounded shared-lock waiter for edit, Mesa stage, boot, restore prior legacy stage in finally; no production source edits. Servo notified available preceding window.

2026-10-02 variant repair5588 TERMINAL0, shared lock released, no root jobs/waiters. Standard desktop-metrics Mesa then legacy builds PASS, each stage byte comparison PASS; Mesa off/on musl links successful. Existing Mesa archives reused unchanged. Normal stage restored legacy+metrics. /tmp/cubit-mesa-variant-repair.log. No new runtime/GPU claim. Docs standard selection added. Native CCL Observatory is preferred diagnostics direction; raw capture transport and typed record/UI interfaces remain prerequisites.

2026-10-02 variant85333 TERMINAL2: Mesa FFI unresolved by normal Ada linker. Reusing existing musl/Mesa builder via tools/build_mesa_desktop.py, test entry delegates. Normal Makefile/metrics helper select same linker and scenario directory. Repair/build under shared lock; no Mesa/driver source edits.

2026-10-02 variant build fix: new metricshelper incorrectly forcedlegacy and bareDesktop stagedfixedpath. Claim helper+Makefile locked repair to preservevalidated backend/timing/storage/display scenario, exactartifactstaging via shared directoryresolver. NativeMesa+metrics thenlegacy+metrics build/cmp queued; no renderer/driver source changes.

2026-10-02 timeline87337/80656 TERMINAL0, nojobs/waiters. Added strict offline PerfettoJSON exporter+regressions+realimportvalidator. Historical nativecapture produced /tmp/cubit-compositor-native-timeline.json; actual officialchecksum-verifiedTPv58.2 imports673events exactly(names,timestamps,durations),zero parsererrors. 87softwarecompletion spans/247input/84publication/255render instants; no fake CPUstacks/inputcausality. Docs timeline-export.md. No production source changes or native rebuild. Rawtransport/archive ownerhandoff stillpending; this is offlineexportonly.

2026-10-02 trace-retry95674/80089 TERMINAL0, no livejobs/waiters. Actual Main trace verifies retainedclose repeatedserial deliveries; analyzers now count retries and exclude ambiguouswatermark latency without discarding valid source/outputwork. Ordinaryduplicates/kindchanges/reverse/futureclock stillreject. Real historicalcapture reanalysis168draws/247deliveries preserved,0ambig. Docs/evidence updated, no productionchanges. REQUEST observability owner: summarymetrics do not retain raw span/identity events; need rawrecord subscription/archive transport for serial-free compositor timelines (bounded loss, explicit producer incarnation/sequence/gaps/clock). Root can wire compositor producers once ABI/ownership handoff acknowledged; existing SDKfix request stillpending.

2026-10-02 normalboot94329 TERMINAL0, LOCK RELEASED,no root jobs/waiters. Exact normalprofile7services+3keyboard Apps-menu cycles+252000pixel restoration PASS;12recordedinputs+tempbaseunchanged,no faults/quarantine. Evidence /tmp/cubit-normal-session-evidence and native.log; privateISO/disks under /tmp/nix-shell.F8AmP5/cubit-normal-session-yz8qv_up. Normalstartupgate closed for4CPU TCG. Servo v26 finalgatePASS confirmed native log+featuresJSON (4windows isolatedclose/reuse). Fullgoalactive: rawstream/export, moreGPU/scanout/hardwaremetrics remain.

2026-10-02 normalboot94329 LIVE single bounded buildlock waiter after Servo30672. /tmp/cubit-normal-session-native.log still empty atlastpoll; no duplicate. Inputtrace close-event proof24380 TERMINAL0:26checks(7flow19prover),zero unproved/justified. Normalboot fixture prepared+syntaxreviewed, no boot result yet.

2026-10-02 normal-session boot gate prepared: new test-normal-session-boot.py uses exact init-desktop-session profile, privateISO/current stagedkernel+initrd, temporarybase+real normal overlay. Records12inputhashes; asserts7startupservices,3real keyboard Apps-menu cycles/exact252000pixelregionrestoration,no faults,baseunchanged. Oneboundedlockwaiter buildsdesktop-metrics thenboots; no production source edits. CurrentServo30672 respected.

2026-10-02 rollout67016 TERMINAL0 + packaging14712 TERMINAL0, lock released, no rootjobs/waiters. Supported desktop-metrics target/native stagecmp PASS; desktop-session-content selectsit, normalprofilecollector2beforeDesktop4, overlayincludesmetrics. Shared helperpromoted tools/build_desktop_metrics.sh; bare desktopOFF. RealnormalMakefile temp-ext2 oracle extracted3payloadsbyteexact, baseunchanged,fsckPASS. Fullnormalprofileboot remains pending (priornativefixturesdifferent). No productionAda changes; Servo freefornextgate.

2026-10-02 rollout67016 LIVE single bounded buildlock waiter behind Servo88392. /tmp/cubit-metrics-rollout-build.sh installs normaldesktop metrics wiring then makes desktop-metrics/stagecmp. Log /tmp/cubit-metrics-rollout-build.log empty waiting. Prepared real temporary ext2 normalMakefile packaging oracle test-metrics-session-disk.py for subsequentlocked gate; no userdisk writes. No productionAda changes.

2026-10-02 claiming normaldesktop metrics rollout: kernel/Makefile additive desktop-metrics target, desktop-session-content dependency and overlay; init-desktop-session collectorpriority2; shared manifest/build helper tools/build_desktop_metrics.sh replacing duplicate test helper code. Bare desktop staysmetricsOFF forcollectorlessprofiles. One bounded lockwaiter installs/builds underlock, no productionAda changes. Servo v25 window respected. Boot/packaging gate remains afterbuild.

2026-10-02 load68284 TERMINAL0, lock RELEASED, no root jobs/waiters. Corrected baseline-triggered redraw fixture passed120s4CPU ccl-workspace+finalfaultscan,8hashes unchanged. Realcollectorpriority2,Desktop4,4workers/app3;3saves duringoverlap;4workersfinished;82→83frames/75batches/0drops,rejects,gaps. BothDesktopvariants rebuiltwithclosebarrier. Prior68202failed onlyobservernogrowth afterinteractiveend; preservednegativeevidence. Servo notified nextwindowfree. Goalactive; normalstartuprollout, rawtraces/export, nativebrowserclosegate, GPU/NUCtimings remain.

2026-10-02 load68202 TERMINAL1:4workerscompleted,3saves duringoverlap, observer no-growth because interaction ended before delayedquery. No kernel/desktop fault observed; gateNOTPASS. Prepared observerbaseline marker + postbaseline Meta/Esc redraws in ownfixture; next singlelockretry rebuildtestobserver and120s run. ProductionDesktop unchanged; oldfirstgate evidence /tmp/cubit-metrics-load.serial. Validator23447PASS1positive6invalidcaptures.

2026-10-02 metrics-load68202 LIVE owns sharedlock: installed new isolated fixture/profile+additive run.sh load gate; rebuilt defaultDesktop, metrics-on recompiling with closebarrier. Realcollector priority2,4workers/application3,Desktop4. /tmp/cubit-metrics-load-native.log, future serial /tmp/cubit-metrics-load.serial. All production sources frozen duringbuild/gate, no production source edits this slice. No duplicatewaiter.

2026-10-02 preparing metrics scheduling/load gate: collectorpriority2, Desktop4, Workbench+4CPUworkers3. New isolated metrics-load/load-observer/profile + additive run.sh hooks underlock. Rebuild bothDesktopvariants to include repaired close, use realmetricsSvc.120s4CPU gate requires saves during4worker overlap and growing metrics after workersfinish. No production scheduling/source edits; one bounded native waiter next.

2026-10-02 barrier95864 TERMINAL0, lock RELEASED, no root jobs/waiters. Repaired Main coalescing across retained close. Actual dispatch ordering/1000overflow/retry/isolation PASS and wrongcoalescing mutant rejected; actual recovery1000cycles+trace200records PASS; policy5checks0unproved; nativeDesktop build PASS. Four hashes verified /tmp/cubit-close-barrier-built.sha256. Servo notified newDesktop ready for native multiplewindow/titlebarX gate; their v23 stopped on modalbackgroundclick oracle before that coverage, do not claim nativeclose complete. Docs updated.

2026-10-02 barrier95864 LIVE single bounded lock waiter behind Servo v23; production source frozen until lock. Actual enqueue/dequeue harness58962 reproduced motion-across-close coalescing bug. /tmp/cubit-close-barrier-fix.py repairs Close_Policy.May_Coalesce + Main parameter, then actual dispatch/recovery/trace regression, latch proof and nativeDesktop build. Log /tmp/cubit-close-barrier-native.log. Hosted titlebar98763 TERMINAL0: actual closeSurface routes opt-in without buffer/process destruction; legacy/internal both slots + destructive-fallthrough mutant rejected. Added pendingclose recovery/isolation checks; goal active.

2026-10-02 close16970 TERMINAL0, lock released, no root live jobs/waiters. Main/protocol/UI.Input/C validator integration installed. Codec/C parity PASS and229proofchecks0unproved (42149 then failed native style only); formatting fixed underlock, runtime/defaultDesktop build + trace/publication regressions PASS16970. Latch4proofchecks PASS98090. ABI bit256/event10 handed to Servo for browser handler+native titlebar X gate. Root still needs actual Main merge/overflow/owner-dispatch tests; no native close claim yet. Metrics-on binary predatesclose, rebuild beforecombinedgate.

2026-10-02 close integration42149 LIVE single bounded lock waiter. /tmp/cubit-close-native.sh installs /tmp/cubit-close-integration.py underlock then codec tests/proof, runtime/Desktop build, trace regressions. Native log still empty waiting for Servo41518. Additional scopes C desktop input validator + protocol/trace tests/checker to admit event10. Policy98090 TERMINAL0:1000cycles,4proofchecks0unproved. New close-request.md states native pending. No protocol/Main/UI input edits applied yet; failed nonblock lock call exited before installer.

2026-10-02 taking Servo-requested opt-in graceful close: Main + Desktop_Protocol ads/adb + UI.Input event constant + Input_Trace kind bound. New pure Compositor_Close_Request retains one serial outside lossy queue; hosted1000cycles passed, proof98090 pending. ABI proposed feature256/event10 zero payloads. No UI.App/browser/menu edits. Shared integration script will edit under buildlock then native Desktop compile. Metrics rollout temporarily deferred for multi-window correctness.

2026-10-02 native80435 TERMINAL0, LOCK RELEASED, no root jobs/waiters.120s4CPU stalled collector gate+finalfaultscan PASS;10hashes unchanged. Capture92002 terminal0:600held-page comparisons,2live samples+4saves duringhold, batch3 resumed with83drops, no quarantine. Real Desktop/SDK production sources unchanged. Own testfixture+runner stall branch+capturechecker documented. Servo notified windowfree. Remaining: default rollout, collector scheduling underload, rawtraces/export, accelerated scanout/NUC measurements. Fullgoalactive.

2026-10-02 native80435 LIVE owns shared lock, collector compiled, headless boot build underway. Previous69529 TERMINAL1 Nix cache sandbox denial BEFORE installer; retried with approved cache access. New metrics-stall fixture/build helper + additive run.sh stall gate installed underlock. No production sources edited. Native inputs frozen; /tmp/cubit-desktop-metrics-stall-native.log. Separate capture validator passed positive+7negative captures (host36763 terminal0), will require live samples+saves BETWEEN hold markers. No duplicate waiter.

2026-10-02 stalled-grant native session69529 LIVE, single bounded flock timeout600 waiter (last polled live). /tmp/cubit-desktop-metrics-stall-native.sh will install new fixture and additive runner gate underlock, build, run120s4CPU CCL workspace, verify source/binary hashes. Log /tmp/cubit-desktop-metrics-stall-native.log still empty while waiting. Do not duplicate/restart. No production sources changed. Documentation describes gate as pending. Prepared installer /tmp/install-metrics-stall.py.

2026-10-01 preparing native stalled-grant collector: new tests/compositor/metrics-stall + build helper, additive run.sh gate under sharedlock. No production source edits. One bounded lock waiter will install/build/boot120s; verified metrics-enabled Desktop reused. Servo source freeze respected.

2026-10-01 fault67661 TERMINAL0, LOCK RELEASED; no root live jobs/waiters.
Isolated malformed-count collector caused metrics-only quarantine invalid=1,
then Workbench labels/saves/opens and full120s4CPU finalgate/faultscan PASS.
All11hashes match; realmetricsSvc/SDK unchanged. Baseline58905 alsoPASS
(realobserver3frames/3batches,18hashes). DefaultDesktop metricsOFF, enabled
build-metrics/desktop.svc verified. Native publisher object16KiB incl2x4KiB
payload+alignment; offbinary no publisher object. Docs updated. Next: native
stalled-reader/collector-scheduling, normalstartup rollout, rawtrace/export,
hardwaremeasurements. SDKgeneralfix approval still pending; Desktop guards
verified independently. SERVO UI/App source window free underlock, ACK stands.

2026-10-01 fault67661 LIVE locked isolated test: build fake metrics collector
only, boot verified metrics-enabled Desktop in120s4CPU CCL workspace.
Expected malformed count triggers telemetry quarantine; workspace must still
pass. /tmp/cubit-desktop-metrics-fault-native.log, -fault.serial. Source/On
binary frozen; real metrics.svc and runtime sources untouched. Shared run.sh
additive fault mode installed underlock after58905terminal. No other waiter.

2026-10-01 native58905 TERMINAL0, lock released. BothDesktopvariants+
observer built;120s4CPU CCL workspace finalgate/faultscan PASS. Observer
received actual Desktop span growth3frames/3batches, no loss/rejections/
quarantine. All18source/staged hashes match /tmp/cubit-desktop-metrics-native-
inputs.sha256. Defaultstaging metricsOFF; separatebuild-metrics enabled.
Preparing isolated malformed-reply collector gate next; no runtimeSDK edits.
SERVO UI source window currently free underlock; no root UI changes.

2026-10-01 native58905 LIVE locked build+120s4CPU metrics-enabled CCL
workspace gate. BothDesktop variants and observer compiled, boot assembly
in progress. /tmp/cubit-desktop-metrics-native.log. Main/GPR/policy and
run.sh frozen; added guarded CUBIT_DESKTOP_METRICS_TEST=1 profile installs
metrics+observer with generated capability binding. Runtime unchanged.
Hosted83786 TERMINAL0: wake policy21checks0unproved,100001rounded deadlines,
adapter17fault cases and rendertraceglue PASS. No native result yet.
Servo UI/App scoped close/navigation ACK stands; wait sharedlock window.

2026-10-01 build10625 TERMINAL2 before native Desktop link: reserved Ada
identifier Delay in new wake helper; host99832 same compile error. Corrected
to Pause_Us under brief lock; both handles terminal, lock released. SERVO:
UI.App.Close scoped destroy + Widgets navigation ACK, source-idle window now
available under lock. Root no UI edits. Main/GPR now wired metrics-on/off,
separate manifest, CQE route and idledeadline; not yet compiled/boot-verified.
Host deadline proof/regression rerun next; no live native waiter presently.

2026-10-01 claiming next owned Main/GPR metrics integration: separate
metrics-off/on Desktop_Metrics selection, dedicated generated metrics
manifest, validated completion routing, submit/release clocks and bounded
idle flush wake. Main/build-definition edits under shared lock. No runtime
SDK edits: caller guards already verified against current SDK. Default
boot profiles remain until explicit metrics-enabled native gate succeeds.
No native job yet; no peer UI/graphics source changes. Goal active.

2026-10-01 Desktop_Metric_Publisher implemented (not yet wired Main).
Actualadapter+unpatchedSDK68234 PASS17fault/overloadcases, SDKdefects still
reproduce independently. Adapter filters invalid CQEs and blocks disabled
puts, so native caller integration can proceed independently of pending SDK
owner patch; runtime sources unchanged. MC reply48262 PASS2proofchecks
(1functional,1termination), zero unproved. Nativegeneric89515 TERMINAL0
privateobjects compile with real runtime; lock released. No root jobs/waiters.
Docs desktop-metric-publisher.md records scope and next Main/manifest/profile
completion-routing/idle-pump/nativecollector-fault gates. Initial62963 generic
SPARK annotation compile failure corrected before passing68234. Goal active.

2026-10-01 metricbatch87452 TERMINAL0: corrected atomic state update,
14SPARKchecks0unproved/justified;1000batchcases PASS. Initial52423 had
1unproved predicate despiteexit0; not a proof pass. Actual-page stream5800
TERMINAL0:122measurements+4declarations fill2pages,878drops leave both
pages byte-identical, out-of-orderpage2release restoresdecls andreportsloss.
Policy adds count/tick only, no extra queue/page. Docs metric-batch-policy.md.
Not yet native/SDK/Desktop integration. Runtime fix approval still pending;
no runtime source edit, no root native job/waiter. Servo v18 completion
received, UI freeze ended; no root UI edit required. Goal active.

2026-10-01 release-metrics39272 TERMINAL0: new owned pure converter
Compositor_Release_Metrics proves18checks0unproved/justified,20000wirecodec
roundtrips PASS. Actual-store70816 TERMINAL0: separateoutput summaries,
reopen and lease-pressure eviction/redeclaration PASS. Initial67495 compile
operator visibility fixed before pass. Docs tests/compositor/release-metrics.md.
Not yet called by Desktop; no runtime/staging edits or nativejob/waiter.
Producer MUST put declarations in each batch: actual collector evicts idle
source metadata under full16source pressure, then rejects undeclared samples.
User async handoff approval asked for tested SDKpatch; no answer yet, do not
apply runtime changes until user approval or ownerACK. Independent work valid.

2026-10-01 metrics boundary64787 TERMINAL0: full current adapter reproduced
invalid-CQE reuse, disabled Has_Room and disabled Put bugs; private candidate
passes all5cases incl1000disabled drops/no IPC. Candidate7952 TERMINAL0 native
Alire compile against real runtime, privateobjects only; lock released.
REQUEST observability owner: please apply or ACK ownership handoff for
tests/compositor/metrics-publisher-boundary.patch (git apply --check PASS),
then runtime rebuild/native metrics gate. Runtime sources NOT edited by root.
Repro/details tests/compositor/metrics-publisher-boundary.md; evidence
/tmp/cubit-metrics-publisher-before-after.log and-native-compile.log.
No root live job/waiter. Desktop producer integration still pending; this
is adapter fault coverage+native compile, not full native metrics validation.

2026-10-01 Servo visual update ACK: root is idle in cubit-ui.adb and
cubit-ui-widgets.ads/.adb; Servo may make requested scoped Draw_Button/
Draw_Tab/container/quiet-close presentation edits in shared-lock source-idle
window and own validation. No root native job/waiter or UI edits planned.

2026-10-01 metrics adapter prerequisite: rechecked current CuBit.Metrics;
Complete still ignores valid, Put/Has_Room still ignore Off. Preparing
actual-adapter fault regressions in owned tests/compositor plus PRIVATE
candidate patch. Request observability-owner ACK for eventual runtime
ads/adb fix, or owner application of tested patch. Runtime sources remain
untouched pending ownership handoff. This does not block independent work.

2026-10-01 native54528 TERMINAL0, LOCK RELEASED; no root live job/waiter.
Both Desktop variants build;120s4CPU CCL workspace+finalfaultscan PASS.
255renderrecords/22batches:168draws all join84sourcepublications/84frames,
87totalcompletedsubmissions;247inputrecords. Zero missing/invalid/lost/
unsupported/unknown. Nine hashes unchanged. Logs/report/serial/hashes
/tmp/cubit-render-pipeline-native*. Defaultstaging timingOFF. Docs updated.
WORK associations only: redraw of source84 carries >12s-old input; not
physical/input-response latency. Pure33checks0unproved; helper81522 and
offline34692 PASS. No UI/Mesa/metrics adapter changes. Servo v17 completion
received: widget source freeze ended, no root edits needed. Goal active.

2026-10-01 native54528 ACQUIRED lock after Servo v17 PASS. Default/timing
Desktop compilation in progress, then120s4CPU CCL workspace trace gate.
Main/trace frozen; /tmp/cubit-render-pipeline-native.log. No duplicate jobs.

2026-10-01 hosted34692 TERMINAL0: render checker now permits no-input
startup/idle draws with explicit missing/unknown counts; standalone input
checker still requires a match. Loss/unclosed input remains rejected. Both
regressions PASS. Native54528 still LIVE queued (flock500036 observed at
5min behind Servo flock498768); same600s waiter, no restart. Sources frozen.

2026-10-01 native54528 confirmed LIVE QUEUED behind Servo v17 flock498768;
root flock500036, single600s bounded wait, no duplicate. Poll same handle.
Sources frozen. Policy33checks and helper81522/offline78628 PASS; native
render pipeline NOT yet verified. Docs tests/compositor/render-pipeline-trace.md
now record proof boundaries and reproduce commands. Earlier72838 policy
attempt was compilation failure; corrected87225 is the proved result.

2026-10-01 native54528 LIVE: one locked build/default+timing Desktop and
120s4CPU CCL workspace trace gate. /tmp/cubit-render-pipeline-native.log.
Main/trace sources frozen; no UI edits. Actual helper81522 TERMINAL0 with
unacquired-buffer denial. Offline78628PASS12invalid+4partial join cases.

2026-10-01 source edit83894 TERMINAL0 lock released. Acquired guard and
explicit unacquired-source denial test applied. Hosted glue v3 running.
Main/trace sources frozen for planned single120s4CPU native CCL trace gate;
no UI/widget edits. Offline78628PASS incl independent missing joins.

2026-10-01 render trace continuation: previous goal work made progress
(hosted policy/glue evidence); actual sources inspected. Acquired guard was
NOT applied by earlier nonblocking attempt. Single source-edit waiter83894
now live, timeout600, to add it and its denial test under build.lock. No
native build waiter. Servo widget handoff ACK remains valid; not editing UI.
Pure Render_Trace87225 PASS33checks0unproved; actual glue40923 TERMINAL0
before new acquired guard. Native render pipeline evidence still pending.

2026-10-01 Servo widget handoff ACK: Servo owner may apply
/tmp/cubit-servo-tab-container.patch to userspace/lib/ui/cubit-ui-widgets.ads
and .adb in a shared-lock source-idle window. Root is not editing either
file and has no native build or waiter; please hold these sources stable
through your native validation. Retained Tab overload and clipped Button
hit bounds are in the acknowledged scope; Servo owns its rebuild/gate.
Metrics issuance already integrated and native verified; rechecked current
procmgr/policy source hashes against verified boot evidence, both match.

2026-10-01 owns render/submission tracing: bounded pure Render_Trace +
Desktop successful client-draw and accepted capSubmit hooks, keyed by full
output writer ticket (buffer/epoch/serial). Records describe draw work while
preparing submitted buffers, NOT final visibility/occlusion/pixel response.
Legacy logical-copy path explicitly unsupported counter, no guessed output.
Proving policy72838; source edit uses brief lock next. No peer edits.

2026-10-01 input-publication34623 TERMINAL0. LOCK RELEASED; no own
live job/waiter. BothDesktop variants compile;120s4CPU nativeCCL workspace
and finalfaultscan PASS.247dequeues/19batches,84publications,84matches,
0unknown/unmatched/invalid/dropped. Seven hashes unchanged at exit;
/tmp/cubit-input-publication-native-report.json and-v2.log, serial, inputs.sha256.
Defaultstaging timingOFF includes hook+priorresizefix; graphics v12 may use it.
No metrics-service/client edits and no CCL source edits. Forced host CCL-image
rebuild resolved stale binder objects. Next tracing gap: actual per-output
source consumption→frame identity, then transport/metrics integration; no
pixel causality/scanout/photon or hardware-performance claim. Goal active.

2026-10-01 input-publication34623 LIVE actual120sCuBit workspace run.
Forced host CCL-image rebuild succeeded; no CCL source edits. IT/ST batches
observed with zero invalid/drop so far. Final faultscan+join+hash checks
pending; same live handle, no duplicate. Graphics next window after terminal.

2026-10-01 GRAPHICS next window acknowledged here: current holder is root
retry34623 (NOT old75200). Forced ccl-image rebuild/current source then
same timing120sCCL gate; /tmp/cubit-input-publication-native-v2.log.
Main+trace sources frozen; staging will be recaptured by test script.
Will yield next native window to graphics link/image after terminal.
No additional native job will be launched by root while graphics uses it.

2026-10-01 input-publication75200 TERMINAL1 BEFORE QEMU: bothDesktop
variants compiled, but initrd ccl-image binder reports ccl-vm.ads changed
and main/declarations/language/objects-values/vm/catalog/images need recompile.
No CCL source edits by root. Lock released; planning forced host ccl-image
rebuild then retry same native gate, source/staged hashes will be recaptured.
No native trace result yet. Graphics may queue its hardware-image gate.

2026-10-01 input-publication75200 ACQUIRED build.lock after graphics65629.
Default Desktop built/staged; timing-on build/native120sCCL workspace in
progress. /tmp/cubit-input-publication-native.log. Own sources frozen.
Hosted80550 TERMINAL0: inputtrace200records/5batches, source trace regression,
12invalid-capture rejections. Actual queue hook45874 passed before unrelated
trace harness blankline issue; that test parser fixed and80550 passed.
Will release for graphics image link/gate after this native run.

2026-10-01 native75200 LIVE queued600s behind graphics65629 CPUgate:
build default+timing Desktop,120sTCG4CPU CCL workspace, inputdequeue/source
watermark correlation validator. /tmp/cubit-input-publication-native.log.
Main/IT/IQ sources frozen. Hosted actual queue instrumentation PASS45874;
trace-drain test first hit finalblankline parser issue, correctedtest80550.
No native claim yet; default staged binary only changes after build acquires.

2026-10-01 inputtrace edit72689 TERMINAL0; lock RELEASED. Added opt-in
IT64record dequeue hook +batch drain to owned Desktop Main. Hosted queue/
trace/parser tests running. NO native waiter; graphics CPU gate next per
previous note. Default staged Desktop still resize-fixed/no new tracehook
until later compile. Need timing-on120sCCL workspace for correlation evidence.

2026-10-01 GRAPHICS: resize42907 already TERMINAL0, lock released.
Please take your CPU fixture/native gate next; root has NO native waiter
or current native job. Continuing owned Desktop input-dequeue trace glue
and hosted correlation tests, only brief source-edit lock requested.
Resize staging remains fixed. Metrics adapter unchanged.

2026-10-01 resize42907 TERMINAL0, LOCK RELEASED; no own live nativejob.
Desktopbuild+three enlarge/shrink scanout oracles PASS (48000pixels exact
each),100sdesktop-display+finalfaultscan PASS. 3source/stagedhashes unchanged
/tmp/cubit-resize-after-inputs.sha256. Pure policy2results0unproved; actual
glue6868cases PASS. Before oracle found36978stale pixels. Proof/test scope
and software/vertical-only native boundary docs tests/compositor/resize-repair.md.
Current stagedDesktop fixed; Servo full browser scenario not rerun by root.
No graphics/runtime sources edited. Next: finish bounded input-dequeue trace
integration and causal publication join, then telemetry producer integration
once metrics adapter review issues are resolved. Overall goal remains active.

2026-10-01 resize42907 acquired shared lock; Desktop compile/link/stage
PASS with new pure transition policy. Native100s desktop-display +three
resize restoration cycles starting. /tmp/cubit-resize-after-native.log and
/tmp/cubit-resize-after.serial; compositor sources/stagedDesktop frozen.

2026-10-01 graphics runtime long lines repaired by owner. Resize42907
LIVE queued600s shared lock: makeDesktop +100s native three-cycle pixel
regression, /tmp/cubit-resize-after-retry.log. No patch reapplication.
All compositor sources frozen; Main glue6868cases alreadyPASS, policy
51200cases+SPARK2checks0unproved. Servo chat notified of cause/pendinggate.

2026-10-01 GRAPHICS ACTION: resize50143 TERMINAL2 beforeDesktop/QEMU:
new cubit-owned_reservations.ads/.adb fail runtime GNAT style comments/line
length. Full /tmp/cubit-resize-after-run.log, e.g .adb9/11 and.ads5/9/10/11.
Please fix your owned runtime formatting; root did not edit these units.
Lock RELEASED; no own live native job. Resize Main fix applied and actual
release integration6868cases PASS. Need native build/test retry after runtime
repair. Baseline native pixel oracle found36978stale pixels. Pure geometry
proof2results0unproved. Prepared native scripts unchanged; no runner edits.

2026-10-01 resize68289 TERMINAL1 expected: native observer found36978
stale scanout pixels after shrink; ordinary desktop-display/finalfaultscan
PASS. Original Desktop snapshot /tmp/cubit-resize-before-desktop.svc.
Fix/native50143 LIVE queued600s lock: apply transition policy adapter,
actualrelease hosted cases, buildDesktop and three native resizecycles.
Log /tmp/cubit-resize-after-run.log. No kernel/runtime/Servo edits.

2026-10-01 resize pure9292 TERMINAL0:51200cases +SPARK2results
(1functional coverage contract,1termination),0unproved. Actual old release
hosted59649 fails old-footprint coverage as predicted. Native68289 LIVE
queued600s build.lock: original staged Desktop, 100sdesktop-display plus
3enlarge/shrink screenshot cycles. Logs /tmp/cubit-resize-before-run.log.
Main remains unchanged until baseline terminal; prepared fix uses pure policy.

2026-10-01 owns resize-release damage fix in Desktop Main + new pure
compositor_transition policy/tests. Evidence Servo screenshot exposes old
window right/bottom after shrink; release substitutes presented preview for
old actual surface. Must cover old, new AND last presented outline. No
shared runtime/kernel/Servo sources or runner edits; native gate queued later.

2026-10-01 metrics issuance COMPLETE for user request. Native120s4CPU TCG
headless PASS incl final fault scan,1005records/3series and forgedtag/crossrole
denials. Wrapper61236 exit1 only obsolete pre-initrd procmgr hash; verification
55361 TERMINAL0: extracted actual bootISO procmgr matches build+stage, patched
sources and metrics binaries unchanged. /tmp/cubit-procmgr-metrics-boot-evidence/
verified.json; /tmp/cubit-procmgr-metrics-v2.serial/native.log. Lock released;
NO own live native jobs. Patch plus missing trusted-startup exactidentity
registration grant applied; tests/procmgr-metrics covers actual glue and11
pure authority/tag proof assertions,0unproved,3mutants rejected. Owner may
retire candidate patch references; no metrics service/client edits by root.
Default boot profiles/producer manifests unchanged. Docs tests/procmgr-metrics/README.md.

2026-10-01 graphics syscall coordination: compositor does not own/edit
kernel syscall.ads/adb or runtime cubit-messages.ads and has no planned edits
to them in this slice. No objection from compositor to additive123..125
constants; check other agents separately. Metrics native61236 finished:
headless120s gate PASS; wrapper exit1 only pre-initrd procmgr hash mismatch
(initrd target rebuilds it). Boot ISO extraction verification55361 running
under lock; once terminal no native job or lock from compositor. Procmgr and
authority-policy sources unchanged during native test. No metrics adapter edits.

2026-10-01 metrics61236 acquired lock, registration addition applied.
Testing real extracted registration and issuance code, then nativebuild/gate.
No telemetry adapter changes. Procmgr sources frozen for this run.

2026-10-01 metrics39725 TERMINAL1: patch/native builds succeeded, native gate
failed because metrics.svc lacks registration capability (not in suppliedpatch).
13135 hosted authority/tag proof11 assertions PASS0unproved;3mutants rejected.
metrics61236 LIVE queued600s build.lock for scoped procmgr registration grant:
trusted startup AND exact com.cubit.metrics identity only, role24. Then actual
registration/issuance harness and native120s gate; no metrics owner source edits.
Log /tmp/cubit-procmgr-metrics-v2-native.log; initial failure logs preserved.

2026-10-01 metrics issuance native39725 LIVE acquired build.lock: patch
applied scoped to procmgr/policy; actual REQ_SERVICE hosted extraction PASS.
Building procmgr metricsvc metrics-check, then120s4CPU TCG metrics gate.
No other procmgr edits; sources frozen. Log /tmp/cubit-procmgr-metrics-native.log,
serial /tmp/cubit-procmgr-metrics.serial. Hosted proof/mutants13135 independent.

2026-10-01 user explicitly requests procmgr metrics issuance integration.
Claim scoped application of tests/metrics/procmgr-metrics-issuance.patch to
procmgr/main.adb and cubit-authority_policy.ads, plus own tests/procmgr-metrics.
Will preserve graphics launch code; native build + metrics gate under build.lock.
Servo resize remnants acknowledged for follow-up after this permissions task.
Input trace hosted policy94274 terminal0; no Main integration yet.

2026-10-01 next owned work: bounded input-dequeue trace +publication-watermark
join evidence, not physical arrival/delivery/pixel causality. New pure IT policy
and hosted tests first; Main timing opt-in hooks follow. No shared UI/runtime/
runner or metrics-service edits. Servo v14 native window respected.
Metrics adapter review request to observability owner before compositor producer:
Complete currently ignores Completion.valid and treats matching token/status/label
as enough to recycle a granted page. Please reject invalid CQEs without reuse;
Put/Has_Room also ignore Off despite disabled-drop API wording. Page alignment fix
is already present. Native issuance patch still pending; do not label a stage
span input-to-present before source/output visibility correlation exists.

2026-10-01 integrated dispatch38145 TERMINAL0: Desktop/client build/link/stage
PASS;90s4CPU TCG input-stream and90s desktop-protocol PASS with final fault scans.
Inputsourcegap1/reject0/eventdrop0/inputreq35/presentreq0. Exact recovery records
66/100/134/168 at76,50/state0; all14source/staged hashes matched. Kernel hashes
captured separately per profile. /tmp/cubit-dispatch-native.log, -input-stream.serial,
-protocol.serial, -native-inputs.json. Default Desktop now includes DB policy.
No own livejobs/heldlock. Policy21SPARKresults0unproved; actual glue8000scenarios
+3mutants rejected; queue/recovery glue tests pass. No performance/physical claim.
Docs updated. Servo and graphics may use new staged Desktop (same wire ABI).

2026-10-01 integrated dispatch native handle38145 LIVE with bounded600s shared
lock acquisition. Poll38145 only; command /tmp/cubit-dispatch-native.sh, log
/tmp/cubit-dispatch-native.log. Build Desktop/client +two90s4CPU regressions.
Full source freeze for Main, DB/IQ policies, desktop-check and hosted fixtures.
No further edits to these until terminal/hash verification. No result yet.

2026-10-01 actual dispatch integration58022 TERMINAL0:8000real drain scenarios
+3mutants rejected (reset count, ignore pending frame, omit fresh drain). Existing
queue85101 and recovery18193 actual-glue tests also TERMINAL0. Desktop Main now
uses DB policy +independent CuBit.Monotonic.Read, two shared-count input phases,
request count/time admission. No UI/runtime/runner changes. Request native window
after current graphics83261/peers: make desktop desktop-check +90s input-stream
+90s desktop-protocol saturation; /tmp/cubit-dispatch-native.sh. Source frozen
once queued. Servo may reuse staged Desktop after this gate if it finishes first.

2026-10-01 native74508 TERMINAL0: corrected saturation gate plus full90s
four-vCPU TCG desktop-protocol/finalfaultscan PASS. Recovery serials66,100,134,168
all snapshot76,50/state0; capacity32 order/more flags, fresh delivery, stale replay
and second-surface isolation pass. All nine input/staged/kernel hashes verified
/tmp/cubit-input-overload-inputs.json; coordinate-native.log/.serial preserved.
Shared lock released. Now integrating proved dispatch budget into owned Desktop
Main plus hosted actual-glue test; no UI/runtime/runner/kernel or Servo edits.
No native job until source/glue tests ready; graphics77272 next native builder.

2026-10-01 native74508 acquired shared slot; corrected desktop-check build
and standard headless preparation now active. No duplicate native processes.
/tmp/cubit-input-overload-coordinate-native.log records build progress. Sources
frozen; graphics77272 queued behind this test. Continue polling74508.

2026-10-01 corrected native retry handle74508 LIVE (queued or preparing under
bounded600s acquisition). Poll74508; do not create another retry merely because
there is no log output yet. Script rebuilds/stages desktop-check with corrected
76,50 recovery oracle, then90s four-vCPU protocol gate +source/binary hashes.
/tmp/cubit-input-overload-coordinate-native.log and .serial. Sources frozen.

2026-10-01 corrected native saturation retry submitted with bounded600s flock
wait to avoid starving behind repeated shared users. Does not own lock while
queued. Observability QEMU PID211383 and Mesa edit PID211511 verified ahead.
Command /tmp/cubit-input-overload-coordinate-native.sh; log same prefix.log.
No source edits to tested Desktop/client/policy during this queued build/run.
One existing handle only; next entry records handle. No duplicate retries.

2026-10-01 dispatch policy66076 TERMINAL0:10000hosted cycles and21SPARKresults
(3runtime,7contracts,11termination),0unproved/justified. Strengthened phase count
permits only two input openings, preserves64 total events;500us each drain,
1000us requests with32/96 caps. New files compositor_dispatch_budget.* and
isolated tests/dispatch_budget*. Production Main still unchanged; integration
pending corrected native queue gate. Monotonic microsecond FFI must be independent
of opt-in profiling; no handler execution-time bound claimed. Evidence exact
hashes /tmp/cubit-dispatch-budget-inputs.json and -two-phase.log. No ownlivejobs.
Graphics98532 nowterminal1 per owner; yielded next shared slot to Servo v13.

2026-10-01 independent next scheduling work while graphics98532 builds:
new compositor_dispatch_budget.* and isolated hosted tests/proofs only. Preserve
64 total input events across two drains and32/96 request caps; add500us admission
per input drain and1000us request admission, guaranteed first-item opportunity
per phase, fail closed on unavailable/regressing clocks after first item. No
Desktop/UI/runtime/runner integration edits yet; queued native overload fixture
and prior production hashes unchanged. This is admission, not handler WCET.

2026-10-01 coordinate-fix compile attempt refused shared lock (terminal1,
empty log, no compiler ran). Graphics98532 fresh Mesa rebuild is now queued/live
per owner note; Servo v13 still requests next slot. Corrected native fixture is
source-ready, not yet compiled or passed. Reproduction after peers finish:
flock --exclusive --nonblock coordination/build.lock nix develop -c bash
/tmp/cubit-input-overload-coordinate-native.sh >
/tmp/cubit-input-overload-coordinate-native.log 2>&1
Wrapper requires previous production hashes unchanged, rebuilds/stages desktop-check,
updates only its source/binary hashes, runs90s4CPU gate and checks dedicated marker
and final fault scan. No own live process; preserve original failed evidence.

2026-10-01 native36423 TERMINAL1: only four recovery-snapshot assertions
failed; all capacity32 ordering/serial/more, stale-replay, fresh delivery and
second-surface checks reported no failures. Oracle error: clientRect applies
inset4,30 even to Plain_Surface; boot cursor80,80 is client-local76,50. Corrected
exact expectation (not weakened), added actual recovery diagnostic records.
Evidence preserved /tmp/cubit-input-overload-before-{native.log,native.serial,inputs.json}.
No product source changes; fixture rebuild and native retry pending. No livejob
or held lock; yielding requested next window to Servo v13.

2026-10-01 native36423 LIVE under shared nonblocking lock after graphics.
/tmp/cubit-input-overload-native.sh stages compiled client, checks six hashes,
runs90s4CPU TCG desktop-protocol and requires dedicated saturation marker,
final protocol/fault gate and exact staged/Desktop/kernel hashes. Log
/tmp/cubit-input-overload-native.log and .serial. Desktop/UI/runtime/runner
sources unchanged; standard runner kernel/initrd preparation. Own sources frozen.
Servo v13 next after this terminal result. Poll36423, do not duplicate.

2026-10-01 request NEXT shared native slot after graphics69867 for bounded
90s desktop-protocol saturation gate (fixture already compiled; will stage,
run standard preparation, verify dedicated marker and full fault scan). No
source rebuild of Desktop/UI; no runner edits. Servo v12 log now reports resize
fixture failure; preserving its agent's retry ownership and requesting this
short gate before subsequent sustained retry. No root lock/job yet.

2026-10-01 metric visibility repair confirmed; Servo v12 QEMU PID140646
verified live via host ps (shared lock PID135383); graphics69867 queued next.
Root has no live native process and defers saturation run until these finish.
Read-only native queue object audit: Pop uses8bytes static stack, no calls,
no runtime ghost-queue copy. Exact disassembly/stack reports saved under
/tmp/cubit-input-queue-native-{assembly,stack}.txt. Acceptance overview updated
for already verified protected-client density allocation and current trace gates.

2026-10-01 native compile79159 TERMINAL0: correct Alire compiler rebuilt and
linked desktop-check with new input saturation fixture. Shared lock released,
no own live jobs. New client NOT staged yet. Evidence: build-fixed.log and exact
six hashes in /tmp/cubit-input-overload-inputs.json. Next requires stage client
then desktop-protocol QEMU, checking dedicated saturation marker plus final PASS.
Full runner remains blocked by metric_batches native compile error reported by
Servo52325 and graphics36168; request owner repair. Servo retains next full slot.
Native queue gate: capacity32 ordered extents/serials/more flags; four overflows
at33; snapshot80,80/no held state; no stale replay; fresh configure after recovery;
other surface untouched. Snapshot assumes headless profile with no device input.
No new throughput/latency/hardware claim. Existing complete-policy native25223
PASS remains valid for its recorded eight hashes; new saturation gate is pending.

2026-10-01 compile24319 TERMINAL4: fixture compiled but binder found mismatched
exception metadata because direct Nix gprbuild bypassed pinned Alire compiler.
Corrected native79159 LIVE under shared lock: from kernel, Nix alr exec gprbuild
-f desktop_check.gpr, restoring correct project objects. No staging/runtime/kernel
build. /tmp/cubit-input-overload-build-fixed.log. Full native gate not run yet.

2026-10-01 native24319 LIVE: short shared-lock gprbuild of desktop-check
against existing runtime, no staging or kernel/runtime/runner rebuild. New
native input overload fixture source frozen. /tmp/cubit-input-overload-build.log.
Servo52325 terminal1 and graphics terminal1 both report new metric_batches
runtime compilation failure; next full native gate waits for owner repair.

2026-10-01 next native overload gate: editing only own desktop-check main.adb
and tests/desktop-protocol documentation. Exercise 32 queued configure events,
overflow/resync, following fresh configure, stale acknowledgments and second
surface isolation through real IPC. No Desktop/UI/runtime/runner/kernel edits.
Will build/test after current shared users; no live native process yet.

2026-10-01 complete input policy25223 TERMINAL0: Desktop build/link and both
90s four-vCPU TCG input-stream/desktop-protocol PASS, final fault scans included.
All eight hashes in /tmp/cubit-input-complete-inputs.json matched after completion.
Stream sourcegap1/reject0/inputreq34/presentreq0. Docs updated to59SPARK results
and current proof boundaries. No own live jobs or held build lock.
Servo transferred to user-owned chat01a0f968-6b1d-7401-a136-50604ecc4935;
old subagent quiescent. New chat owns existing Servo scope and sustained tests.

2026-10-01 complete input policy25223 LIVE shared lock after Servo17835
terminal1. Desktop build +90s input-stream then90s desktop-protocol native.
/tmp/cubit-input-complete-native.log, -stream.serial, -protocol.serial.
Policy22704 TERMINAL0:59SPARKresults,0unproved/justified; Pop/Has_After,
Recover and Reserve now cover dequeue, forced recovery and close allocation.
Actual enqueue/dequeue10358PASS; recovery/close64948PASS1000cycles. Initial
16890 test-only extractor selected forward declaration; corrected beforePASS.
No inline nextSerial increments remain in Desktop. No UI/runtime/runner
changes; Servo child notified next window after native terminal.

2026-10-01 input28361 TERMINAL0: Desktop build/link/stage and90s4CPU TCG
input-streamPASS with finalfaultscan. Sourcegap1matches publisher, rejects0,
IPC budgetsPASS; hosted actual queue saturation/isolation32276PASS. Exact7
input hashes verified /tmp/cubit-input-queue-inputs.json. Default Desktop now
includes IQ insertion and source-trace integration with timing disabled.
Direct forced-resync/close serial allocation remains unproved legacy glue.
No own livejobs/lock; next window released. Docs updated. Observability
design delivered and reviewed, remains proposal rather than implemented stream.

2026-10-01 input policy integration32276 TERMINAL0: actual enqueue/dequeue/
hasInputAfter plus real IQ policy PASS1000ordering/overflow/per-surface
isolation/stale-ack cycles, counter saturation and serial refusal; wrong
coalescing-kind mutation rejected.62780 policy proof29resultsPASS0unproved.
Desktop now aliases IQ types and invokes proved Push; nextSerial exhaustion
on this enqueue path exits instead of wrapping, queue overflow counters
saturate. Direct force-resync/close serial allocation remains legacy glue.
Native28361 LIVE shared lock: make Desktop +90s input-stream regression.
/tmp/cubit-input-queue-native.log/.serial. No shared UI/runtime/runner edits.
Servo notified next window after terminal. Observability design delivered at
docs/observability-streams.md; new transport not implemented.

2026-10-01 input queue policy69847 TERMINAL0 hosted tests+proof: new
Compositor_Input_Queue preserves newest-only motion replacement, strict
transition barriers,32event bound, overflow recovery and rejects exhausted
serials.1000cycles +fragmented slot ordering PASS. Actual Desktop still uses
inline queue; extraction NOT integrated yet. Files compositor_input_queue.*
and tests/compositor/input_queue*. /tmp/cubit-input-queue-policy.log. No own
nativejobs/lock; Servo v9 fixture13743 live window. User requested dedicated
observability agent: spawned /root/observability for serial-free kernel+
userspace metrics/tracing design, bounded endpoints, overwrite/FIFO semantics,
collector/flamegraphs and scheduler instrumentation. Own new coord note.

2026-10-01 source trace37517 TERMINAL0: timing Desktop compile/linkPASS;
120s4CPU TCG CCL workspace/finalfaultscanPASS. Checker accepts84records over
20closedbatches,0invalid/dropped/unknown. Exact input hashes verified in
/tmp/cubit-source-trace-inputs.json. Default staged Desktop unchanged; only
build-timing variant updated. No own livejobs/lock; Servo notified next window.
User wants subscribable metrics stream: docs now specify typed bounded
collection/asynchronous exporter, preserved us clock-domain timestamps,
producer incarnation/sequences and explicit loss. Existing logstore has
bounded subscriptions, but text/ms schema is not high-rate typed metrics.
Serial remains temporary timing-test export. New transport not implemented.

2026-10-01 source trace native37517 LIVE shared lock after graphics window:
timing Desktop compile +120s4CPU TCG ccl-workspace and new trace checker.
/tmp/cubit-source-trace-native.log/.serial and -native-report.json.
Hosted actual append/drain53372 TERMINAL0:200records/5batches, disabled clock,
overflow/invalid diagnostics rejected. No shared UI/runtime/runner edits.
Default staged Desktop remains cursor fix; timing variant being rebuilt.
Servo child notified next window after this terminal.

2026-10-01 source trace source-only integration: lifetime64 immediate
publication logging replaced by Compositor_Source_Trace bounded64records,
deferred publishTiming drain plus explicit dropped/invalid stats and reset.
Hosted83690 TERMINAL0:26SPARK results (19proverchecks),0unproved/justified;
1000overflow/invalid/reset cycles.4558 TERMINAL0 evidence checker:256batches
and20negative controls. Logs /tmp/cubit-source-trace-{policy,evidence}.log.
No protocol/UI/runtime changes or new native build/staging. Servo23930 live
window; requested next timing Desktop build+120s CCL native verification.
Buffer is diagnostic-only; untrusted input metadata/zero unknown preserved.
Complete batches are not complete process-lifetime trace or photon latency.

2026-10-01 cursor-after1628 TERMINAL0: corrected default Desktop compiled,
linked and staged; full100s native desktop-display PASS with standard input/
drag/maximize checks and finalfaultscan. New observer4exact wallpaper
restorationsPASS; after screenshot visibly clean. Before32399 failure was
1342stale pixels; hosted36592 two omission mutants rejected. All7 after
source/binary hashes match /tmp/cubit-cursor-after-inputs.json. Default staged
Desktop now includes fix; optional Mesa variant unchanged (direct-only fix).
No own livejobs/lock; Servo child notified native window released for v7.
Intel NUC confirmation remains pending. Actual source docs updated.

2026-10-01 cursor-before32399 TERMINAL1: native screenshot visibly shows
repeated cursors, first returned wallpaper ROI differs1342pixels. Standard
desktop-display runner passed; new pixel oracle correctly rejected it.
Now after1628 LIVE locked Desktop build +same100s native regression.
Desktop compile/link/stage completed; source/binary hashes -after-inputs.json.
Logs /tmp/cubit-cursor-after{,-native}.log and .serial. Shared runner unchanged.
Hosted36592 TERMINAL0:1000exact target+scanout frames; display omission mutant
fails frame1, history omission mutant fails target frame2. No proof claim for
Desktop orchestration (SPARKOff); reuses proved bounded Damage/Repaint policy.

2026-10-01 cursor-before32399 LIVE shared native lock after graphics94624
terminal0. Existing pre-fix default Desktop,100s desktop-display plus native
scanout screenshot observer. /tmp/cubit-cursor-before{,-native}.log and
.serial; hashes -before-inputs.json. Source correction still unstaged.
After terminal: compile Desktop-only fix, same observer and standard input
regression. Servo child notified; no shared runner changes.

2026-10-01 cursor repair source fix: actual repairDirectWriter now queues
previous overlay footprint in Display damage AND invalidates it for all slots
before taking writer repair debt. Hosted36445 reproduced clean-target/stale-
scanout defect at frame1; first display-only fix20709 exposed retained-target
artifact; both-boundary fix56787 PASS1000frames. Two independent mutation
controls session36592, log /tmp/cubit-cursor-repair-two-boundaries.log.
New tests/compositor/test-cursor-repair.py and check-cursor-repair.py; native
screenshot observer pending. Native pre-fix attempt refused shared lock while
graphics96722 compiling Mesa. No own nativejobs/lock; no staged Desktop change.
User photo confirms visible artifacts on wallpaper and window edge, not cause.
Private /tmp/cubit-run-cursor-test.py drives existing desktop-display fixture
then own observer; no shared runner edits. Next run before with existing
userspace/services/desktop/build/desktop.svc, then build Desktop and run after.

2026-10-01 idleDPI2633 TERMINAL0: corrected native180s4CPU TCG dual-output
Mesa gate PASS, finalfaultscanPASS. Settings125/150% wakes idle protected
client with logical320x234 unchanged; exact output400x292@(127,140),
480x351@(153,168), returned100%320x234@(102,112); no paints during each
0.7s idle observation, clean close. All9 recorded source/binary hashes match.
Logs /tmp/cubit-idle-dpi-native-v3.log/.serial and phasePPMs; inputs manifest
/tmp/cubit-idle-dpi-fixture-inputs.json updated. No own livejobs/lock. Servo
child notified next native window available. No GPU/text/timing/240Hz claim.
Read-only cursor path audit has not diagnosed the reported NUC trails; no fix.

2026-10-01 native idleDPI v3 session2633 LIVE shared lock, corrected
pixel-center oracle; /tmp/cubit-idle-dpi-native-v3.log and .serial.
Initial sandbox attempt stopped at Nix cache permissions before native work;
escalated same command now booted and executing observer. No source edits.
Servo33106 terminal1 confirmed; child doing local runtime bridge work.

2026-10-01 idleDPI20534 TERMINAL1 after diagnosed fixture expectation; own VM
stopped by exact serial/PID verification. Native client received125% configure
and repainted logical320x234 while untouched. Output exact green box400x292 at
(127,140) matches output pixel-center sampling, NOT allocation ceil293. New
observer derives phase-aware boundaries from unit-scale location. Hosted41142
TERMINAL0:20480 independent center-inequality cases +image rejection controls.
First85815 terminal1 cursor shadow covered1pixel; park cursor fixed for20534.
Current corrected observer NOT native-validated yet. No own livejobs/lock;
Servo33106 now owns retry window; root idleDPI rerun next. Mesa Desktop and
new fixture client native compile/link PASS; default staged Desktop unchanged.
Logs /tmp/cubit-idle-dpi-native{-v2,}.log/.serial; oracle
/tmp/cubit-dpi-pixel-oracle-center.log. Pending input hashes -fixture-inputs.json.

2026-10-01 idleDPI85815 LIVE: native no-timer protected client compiled/linked,
MesaDesktop relink then180s dual mixed-output QEMU observer. New own files
native_dpi_client.adb/gpr, check-idle-dpi.py, dpi_pixels.py, init-desktop-dpi.ccl;
runner opt-in CUBIT_TEST_IDLE_DPI hooks under shared lock. Fixture install
verified by dump+cmp; defaults/Servo branch preserved. Log /tmp/cubit-idle-dpi-
native.log and .serial. Earlier85149 assertion stopped before source edits.
Screenshot oracle67390 TERMINAL0 exact dimensions/colors and stale/truncated/
damaged/oversized negative casesPASS. No mixed-DPI native PASS yet.
Servo child waiting85815 terminal before own next retry; no other root builds.

2026-10-01 DPI73892 TERMINAL0: native Desktop+desktop-check compile and90s
4CPU TCG desktop-protocol PASS finalfaultscan, including visible publication,
resize/frame-pair/retirement checks. /tmp/cubit-dpi-refresh-native.log and
-protocol.serial. Unit-scale legacy backend only: NOT native mixed-DPI wake or
pixel proof. No own livejobs/lock; graphics requested next window. Servo child
notified; authorized narrow fixture unlink-before-write fix under later lock.
Updated hashes /tmp/cubit-configuration-refresh-inputs.json. Hosted rejection
fixture40270 alsoPASS with explicit failure status (old false-positive superseded).

2026-10-01 DPI73892 LIVE locked Desktop+desktop-check native compile then90s
protocol regression, /tmp/cubit-dpi-refresh-native.log and -protocol.serial.
Servo11346 terminal1; next window after this regression requested by graphics.
No scripts/UI/runtime changes. Hosted40270 TERMINAL0 actual service functions
PASS including explicit Resources_Exhausted at16x, no state/wake on rejection,
recovery and closed-lifetime refusal. Initial34495 fixture missed shrunken output;
83289 explicit status caught this. Corrected both X/Y bounds and reran40270.
Authoritative hosted log /tmp/cubit-configuration-refresh-admission-verified.log.

2026-10-01 own Desktop DPI refresh source integration: currentPublication-
Configuration queues configure after an established configuration changes;
refresh managed surface configurations before pending scene paint. Reuses
proved Density/Selection/Surface_State policy, no idle timer or new pixel copy.
Desktop-only edit after Servo confirmed no remaining Desktop compilation;
UI/runtime/scripts/staged binaries unchanged during its native fixture.
Hosted88017 TERMINAL0: actual extracted service functions PASS idle move,
scale change,1000stable seam refreshes, visible/candidate ownership retention,
unmanaged/internal exclusions; missing-notification mutation rejected.
/tmp/cubit-configuration-refresh-hosted.log. Initial31793 failed test-only
operator visibility; fixed. Native Desktop build and mixed-DPI client wake
validation PENDING; Servo11346 owns current native retry window, graphics also
requests next mesa-window slot. No own livejobs/lock.

2026-10-01 provenance21793 TERMINAL0: Mesa Desktop optional ABI relink PASS;
corrected timingCCL120s PASS finalfaultscan.64 exact unique source identities,
nonregressing inputwatermarks incladvances/repeats, validclock; bounded sample
exhausted, NOT complete trace/latency. Logs /tmp/cubit-provenance-timing-native.log,
-timing-workspace.serial, -source-records.json. No own livejobs/lock. Child
Servo notified next native slot available; shared protocol/UI sources stable.
Hashes /tmp/cubit-provenance-inputs.json. Earlier default suites27005 alsoPASS.
Remaining: per-output source correlation, trace loss/coverage, configured DPI
notifications, actual Mesa/GPU native integration and hardware measurements.

2026-10-01 provenance27005 TERMINAL0: coherent native builds and protocol90s /
CCL workspace120s both PASS final scans. Hosted76058 TERMINAL0 both continuous-
input flood casesPASS. Found CCL fixture overwrites CUBIT_DESKTOP_IMAGE with
normal staging; no native timing evidence from that run. Corrected only CCL
image selection under lock. 21793 LIVE: optional MesaDesktop ABI relink then
actual timingCCL120s; /tmp/cubit-provenance-timing-native.log and
/tmp/cubit-provenance-timing-workspace.serial. No other source edits planned.
Servo child owns next native window after21793. Other offered images preserved.

2026-10-01 provenance27005 LIVE: locked coherent Desktop/managed-client rebuild,
then native desktop-protocol90s and timing Desktop ccl-workspace120s. Prior39873
TERMINAL2 caught two explicit Surface aggregates missing new metadata default;
corrected under this lock. Sources frozen. Log /tmp/cubit-provenance-native-
complete.log; serials /tmp/cubit-provenance-{protocol,workspace}.serial.
Permanent hosted publication37145 TERMINAL0:129results0unproved/justified,
new watermark bit/identity boundary cases PASS. Hosted CCL flood76058 running
in disjoint output. Servo child waits for coherent ABI build and next lock slot.

2026-10-01 provenance wire migration: own publication codec/tests, FrameBuffer/
FramePair passthrough, UI.App handler/paint metadata, CCL handler boundaries,
Desktop accepted-source metadata and desktop-check packet adversaries. Shared
edit/build window under lock. Epoch/ticket packedword1,inputserialword2; all
managed clients+Desktop require rebuild together. Servo agent rebuilding bridge
afterward; prior offered graphics images preserved. Native validation pending.

2026-10-01 provenance6166 TERMINAL0: permanent hosted target PASS10000
interleaved cycles; SPARK18results0unproved/justified. Evidence /tmp/cubit-input-
provenance-integrated-final.log and -inputs.json. Runtime/wire integration is
explicitly pending; no native behavior change/latency result. No own livejobs.
Read-only Servo review flagged floor-vs-ceil pointer cell mapping atfractional
DPI; child asked to prove coordinate policy and test roundtrip after native run.

2026-10-01 Client_Input_Provenance promoted under shared lock; permanent
hosted build/proof6166 running in disjoint output. Private47470 TERMINAL0
18checks0unproved +10000 adversarial cycles. Existing UI.App/FramePair/wire
unchanged; this is policy preparation, NOT native input-latency tracing.
Earlier11993 stopped because promotion lock attempt failed, no source writes;
successful promotion was after confirming prior holder gone. No own nativejob.

2026-10-01 Workbench41398 TERMINAL0: hosted actual-loop continuous-input
frozen clock128events/4frames and advancing clock4events/4frames, both4yields,
no timed sleeps. Native build+120s4CPU ccl-workspace PASS publication/live/save/
open/REPL/finalfaultscan. Policy28289 TERMINAL0,8checks0unproved,1188cases+
10000 sustainedbatches. No own live jobs/lock. Evidence /tmp/cubit-input-budget-
{policy,verified}.log, -native.serial, -inputs.json. Earlier78683/19495 were
test-launch/setup failures (wrong alr cwd then _GNU_SOURCE redefinition), fixed.
Sources unchanged after passing run; docsupdated. Servo subagent remainsactive.

2026-10-01 own CCL input budget: Client_Input_Budget new SPARK policy +
CCL common Run batch admission, platform spec/native+host Yield_Input.
32poll/1ms budget; preserve semantic pointer barriers; no timed sleep on
unobserved backlog. Tests input_budget.gpr/adb and hosted actual-loop flood
fixture next. No changes to Servo sources or shared UI.App implementation.

2026-10-01 signed-clip78006 TERMINAL0: native fixture+default NetSurf rebuilt,
120s4CPU TCG netsurf-https PASS realTLS1.3/native shell/finalfaultscan.
No own live build/test or lock. Policy4 checks0unproved +583164 cases,
hosted actual C/Ada ABI6562 sanitizer casesPASS. Source equality verified
against private proof inputs; evidence /tmp/cubit-browser-clip-inputs.json.
Docs/README updated. Servo subagent active on userspace/servo/native bridge
and cubitshell chrome; see coordination/servo-browser.md for its ownership.

2026-10-01 signed clipping promoted under shared lock after private proof
30716 TERMINAL0 (4 checks,0 unproved/justified),583164 policy cases;
7650 TERMINAL0 actual C invalidation/Ada ABI sanitizer6562 cases. Own files
Client_Signed_Clip.*, NetSurf embed invalidation +Browser_Engine spec dependency,
tests/compositor/{signed_clip.gpr,signed_clip_tests.adb,test-browser-invalidation.py}.
Native integration rebuild next. User authorized Servo browser subagent;
subagent owns coordination/servo-browser.md and separate browser milestone.

2026-10-01 NetSurf10083 TERMINAL0: full make netsurf-https-test succeeded,
normal homepage app restored;120s native4CPU TCG HTTPS regression PASS,
TLS1.3 fixture/native shell/final fault scan. Logs /tmp/cubit-netsurf-frame-native
.log/.serial; hashes -inputs.json. Isolated compile17322 also TERMINAL0.
No own live jobs/lock; no source edits after build. Native test is NOT a page
pixel oracle or protected/DPI validation. Found actual framebuffer font path
quantizes bitmap glyphs to1x/2x; density contract/text rasterization still needed
before NetSurf managed adoption. Docs and boundary README updated.

2026-10-01 resumed: prior architecture explanation was no goal progress.
Native isolated NetSurf compile17322 TERMINAL0 with real headers/production
flags. Starting shared-lock make netsurf-https-test then120s native HTTPS
regression; target restores default homepage archive/app after fixture build.
Own boundary sources unchanged; no driver/image handoff edits.

2026-10-01 NetSurf96339 TERMINAL0 actual-function ASan/UBSan140 casesPASS;
26992 TERMINAL0 read-only real-header production-flags syntax checkPASS.
Logs /tmp/cubit-netsurf-frame-{final,syntax}.log; no own jobs/lock. Native object
compile deferred on authoritative peer packaging lock3753109; no archive/app
rebuild or native execution claim. Isolated compile ready at
/tmp/cubit-netsurf-frame/compile.sh (read-only headers, object in/tmp; run under
shared lock). NetSurf pointer lease/caret bounds changed; stilllegacy until
scale-aware page geometry +managed-mode integration. No image/staging edits.

2026-10-01 NetSurf boundary88255 TERMINAL0: actual redraw function extracted
into ASan/UBSan foreign-mock fixture,137 clip/caret/restore/overflow casesPASS.
Adding3 null admission cases. Native-header isolated compile nonblocking lock
attempt exited1 before Nix; authoritative lslocks holder3753109 corresponds
peer repeat-v3 packaging2332. No own native job/lock. Source modified, native
NetSurf archive/application not rebuilt yet; no native execution claim.

2026-10-01 NetSurf frame boundary scope: synchronous temporary libnsfb binding,
restore owned surface/clip after redraw; validate foreign integer extents and
clip caret to damage. Own netsurf-embed-cubit.c redraw only +new isolated
netsurf-frame actual-function sanitizer tests. No fetch/network/engine geometry
or app mode changes. NetSurf remains legacy until density integration validated.
Shared edit under lock; hosted test outputs private.

2026-10-01 CCL46742 TERMINAL0 clean bounded hosted preview PASS (SDL quit
wakeup fixed), capture matches earlier inspected hosted image exactly. Native
69785 ccl-workspace120s PASS and native screenshot inspected; no native source
changes after that run. Shared lock released/no own jobs. Logs
/tmp/cubit-ccl-direct{,-verified,-host-final}.log and .serial; native.png at
/tmp/cubit-ccl-direct.png; source hashes /tmp/cubit-ccl-direct-source.json.
CCL now draws directly into managed candidate, no fixed3.686MB shadow or memcpy.
Only NetSurf remains on UI.App legacy path. CCL input-drain bounding and actual
mixed-DPI/overload/hardware timings remain separate requirements; not claimed.
Do not rerun private edit/finalize scripts (non-idempotent).

2026-10-01 CCL69785 TERMINAL0 native ccl-workspace120s PASS editing/file save/
open/REPL/live-label/finalfaultscan. Native capture40703 terminal0, inspected
/tmp/cubit-ccl-direct.png. Native fixed3,686,400-byte image/row-copy bridge removed.
Host test initially needed forced quit; final fix now queues SDL_QUIT after
last requested preview frame. Hosted rerun with30s guard next. Three CCL native
runner cases now require protected publication marker (present in passing log).
No new native code change after verified run; shared lock held for hosted build.

2026-10-01 CCL4831 TERMINAL0 native direct-frame build/staging PASS. Host99358
terminal2: portable glyph directory caused accidental softpipe.c C compilation;
excluded native bridge in hosted preview GPR. Combined69785 LIVE sharedlock:
host rendered /tmp/cubit-ccl-direct-host.bmp then existing frame-limit test
hung in SDL_WaitEvent. Sent SIGTERM only to owned preview3728608 (verified exact
screenshot env); SDL handled it as quit and chain continued to native regression.
Do NOT claim clean hosted test PASS yet. Native ccl-workspace120s now proceeding;
/tmp/cubit-ccl-direct-verified.log and .serial. Shared sources FROZEN; fix hosted
frame-limit quit event after native terminal, then rerun bounded hosted test.

2026-10-01 CCL direct-frame scope: common Workbench painting acquires candidate
before all draws, repairs older buffers from retained state, no fixed native
pixel array or row memcpy. Platform spec +host/native bodies, common Workbench,
and App Frame_Pending accessor. Native Open opts managed; SDL owns host-only
image. Shared edits/builds locked; freeze sources until verification terminal.
No compiler/VM/driver/runtime changes. Pending paint uses existing wakeup policy.

2026-10-01 managed32931 TERMINAL0: three fresh app builds +managed-ui90s
native startup/publication/finalfaultscan PASS; exact3 first-frame receipts
required by runner. Capture2588 terminal0, foreground Config Inspector visually
inspected at /tmp/cubit-managed-ui.png. Shared lock released/no own jobs.
Logs /tmp/cubit-managed-ui.log and .serial; hashes /tmp/cubit-managed-ui-source.json.
Devices/Config Inspector/Boot Logs now join Files on protected frames. Raw CCL
and NetSurf remain legacy, requiring scale-aware integration before removal.
No configured mixed-DPI/overload/hardware-performance claim from this test.

2026-10-01 managed32931 LIVE shared lock: fresh3app builds PASS, native profile
has all ready markers703-705 and exactly3 protected publications707-709.
Capture2588 TERMINAL0 /tmp/cubit-managed-ui.png visually inspected: Config
Inspector foreground populated correctly, Devices/Boot Logs behind. Native
90s final fault scan pending. /tmp/cubit-managed-ui.log and .serial; sources
FROZEN, no driver/runtime/offered-image changes by compositor.

2026-10-01 managed toolkit rollout: Devices, Config Inspector and Boot Logs
opt into protected_frames. New managed-ui native profile starts all three,
stages current binaries and requires exactly3 first protected publications.
Own three app Open calls +headless/profile changes under shared lock; fresh
build/native90s verification next. Raw CCL/NetSurf callers still legacy.
No new policy/driver/runtime edits. Sources frozen during native build/test.

2026-10-01 App75261 TERMINAL0, shared lock released/no own jobs. Native Files
managed-frame regression90s PASS all scroll/column-resize/refresh/navigate/move
markers+finalfaultscan; first protected publication line655 in
/tmp/cubit-app-frames.serial. Log /tmp/cubit-app-frames-native.log. Other toolkit
consumers compile. Screenshot88909 terminal1 arrived after VM exit; no visual
inspection claimed. Peer driver compile window is free. Docs/persistent marker
only next; ordinary remaining clients still need managed adoption/raw scaling.

2026-10-01 App75261 LIVE command-scoped shared lock: Files managed integration
and shell/config-inspector/devices/boot-logs built; fresh headless --test files
90s build/boot progressing. /tmp/cubit-app-frames-native.log, .serial. Previous
32046 terminal; pending dirty query failure retained, tracker tokens preserved,
Begin input/output alias removed. Wakeup proof2 checks zero unproved/justified.
UI/App/Files/pair sources FROZEN. Peer may take native window once this handle
terminal/released; no source-driver/runtime or offered-image edits by compositor.

2026-10-01 App32046 TERMINAL2: wakeup proof finished; Files compile caught
private Tracker aggregate. Removed reset, preserve token history and refuse
reopen with pending input. Also separated Begin_Paint input/output boxes to
avoid aliasing, retain deferred dirty rectangle across query failure. First
accepted managed frame emits marker. Rebuild +files native regression next;
shared sources frozen under command-scoped lock.

2026-10-01 App ownership integration scope: Window limited, Begin_Paint and
Run acquire/repair/publish via Frame_Pair, bounded pending retry deadline helper;
Files first managed-window adopter. Existing raw-renderer callers remain legacy
pending separate scaling integration. Pair Reset permits only empty terminal
owners. Shared UI/App/Files +new proof GPR edits under lock; freeze for builds.
No driver/runtime/offered-image changes. Full goal still includes migration of
remaining apps and deletion of temporary legacy branch.

2026-10-01 pair3953 TERMINAL0: explicit staged-service90s desktop-protocol PASS
including new pair marker664, density665, fullprotocol676 and finalfaultscan.
No own jobs/lock. Native pair12frames +resize/cancel/incomplete-paint/close and
full retained-pixel checks pass. Log /tmp/cubit-frame-pair-staged.log/.serial;
/tmp/cubit-frame-pair/staged-inputs.json records kernel/initrd/plan/newELF hashes.
Fresh make72394/directboot51654 failed unrelated CCL FUNCTION_VALUE missing
cases in Language/VM; untouched. New fixture itself compiled via direct GPR.
Client_Frame_Pair is audited serialized FFI glue around proved frame/debt/
geometry/protocol cores, NOT a new whole-adapter SPARK proof. UI.App stilllegacy;
next adopt owner in Run/manual clients, including raw-renderer density handling.
Private run-staged.sh explicitly skips kernel/initrd regeneration and retains
all protocol/fault gates; do not present it as fresh whole-system validation.
No offered NUC image changes. Promotion scripts are non-idempotent, do not rerun.

2026-10-01 pair72394 TERMINAL2 before owned compile: existing ccl-language.adb
case missing FUNCTION_VALUE (unrelated work untouched). Native direct51654 LIVE
under command-scoped lock: alr gprbuild desktop_check using existing runtime/
manifest/font artifacts PASS, staged own fixture, headless build/boot underway.
/tmp/cubit-frame-pair-direct.log and /tmp/cubit-frame-pair.serial. Owned sources
FROZEN. Corrected private adapter mixed Ada boolean syntax before compilation.

2026-10-01 pair owner scope: Client_Frame_Pair serialized two-buffer FFI owner,
reuses proved frame/debt/geometry/protocol policy. Native desktop-check fixture
exercises12frames, resize, cancellation, incomplete repair, retained pixels,
allocation bounds and visible-loan close. Shared main/headless-marker edits
under command-scoped lock. UI.App adoption pending; adapter is not SPARK proof.
No driver/Mesa/offered-image edits. Sources frozen while native build/test runs.

2026-10-01 density87777 TERMINAL0: native five-scale/two-face offscreen UI text
oracle PASS marker662; full desktop-protocol PASS672,90s final faultscanPASS.
Shared lock released, no own jobs. /tmp/cubit-native-density.log and .serial;
source hashes /tmp/cubit-native-density-source.json. Actual native Rust masks,
SPARK blend, clipping/padding checked via explicit booleans, not assertions.
No output-DPI negotiation, scanout or latency evidence; normal apps stillunit.
Next UI.App protected two-buffer/configuration adoption. No offered NUC image
changes. Do not rerun /tmp/cubit-native-density/promote.py (not idempotent).

2026-10-01 density87777 LIVE command-scoped shared lock: native desktop-check
built, headless desktop-protocol90s build/boot in progress. All owned UI/test
sources FROZEN. /tmp/cubit-native-density.log and .serial. New required marker
DESKTOP-DENSITY-TEXT-CHECK follows actual five-scale/two-face offscreen oracle;
failures use explicit boolean (native assertions disabled). No scanout claim.

2026-10-01 native density scope: desktop-check offscreen pixel oracle uses
actual UI/Rust at five scales/two faces, checks every pixel and fresh outlines.
Own desktop-check new Desktop_Density_Text +main/GPR, narrow Makefile font
prerequisite and headless required marker. Promoted under command-scoped lock;
build/native desktop-protocol90s next. No normal app/output DPI change, no
scanout/performance claim. Prior57824 is terminal and released.

2026-10-01 blend57824 TERMINAL0: Settings144 PASS, native Desktop/shell/files/
config-inspector/devices/boot-logs builds PASS,90s desktop-display/final fault
scanPASS. Shared lock released, no own jobs. Integrated75 proof checks zero
unproved/justified +16777216channel/38220rectangles +hosted5scale2face pixel
oraclePASS in /tmp/cubit-text-blend-integrated.log. Native log/serial at
/tmp/cubit-text-blend-native.*, exact source hashes verified against
/tmp/cubit-text-blend-source.json. Production Paint frame176bytes static;
no temporary image. Raw mappings/exclusive write ownership remain trusted.
Next native fractional-text execution and UI.App protected-buffer/config
migration; normal native apps are stillunit. Do not rerun promote.py (obsolete
original snapshot). No offered graphics-image changes.

2026-10-01 blend2784 TERMINAL0: integrated proof75 zero unproved/justified,
16777216 channel +38220 rectangle testsPASS; real UI font pixel oraclePASS.
Native57824 LIVE command-scoped shared lock: Settings144, six app builds,
90s desktop-display; log /tmp/cubit-text-blend-native.log and .serial.
UI/core/GPR sources FROZEN. Default native apps stillunit; no native fractional
claim. Previous failure syntax/max-length precondition fixed before promotion.

2026-10-01 blend integration: promoting Client_Glyph_Blend pure core and
CuBit.UI pointer bridge under command-scoped shared lock; Settings closure and
new tests/compositor/client_blend.gpr. Private proof zero unproved; 16777216
channel cases +38220 rectangle cases passed. Running persistent proof and hosted
UI font-pixel oracle under lock; sources frozen until terminal. No native/image
changes this chunk yet. Mapping/exclusive ownership remain bridge assumptions.

2026-10-01 final text evidence/docs saved, no jobs/lock. Hashes in
/tmp/cubit-ui-text-source.json, owner report148 checks zero unproved at
tests/compositor/build/client-glyphs/obj/gnatprove/gnatprove.out. Final native
94494 PASS compatibility (unit scale), hosted5scale2face fresh-mask oracle and
570mask lease/lifetime testsPASS. UI.GlyphState512KiB arena+metadata perprocess,
not total footprint. Next extract imperative mask blend into proved pixel core
and exercise native fractional-density UI text, then migrate UI.App buffer/config
ownership. All statements distinguish policy proof/hosted pixels/nativeunit.
Do not rerun /tmp/cubit-ui-text promotion scripts; shared source already includes
post-promotion strict-warning and Settings-aggregate repairs.

2026-10-01 SHARED WINDOW RELEASED:94494 TERMINAL0 native Desktop/shell/files/
config-inspector/devices/boot-logs builds and90s desktop-display/finalscan PASS.
Desktop/Display staging cmpPASS. Source/proof148 checks zero unproved, owner570
and real-density font-pixel host tests passed earlier. No own live jobs/lock;
graphics may take pending relink window. Detailed docs/manifest updates next,
no further shared build planned this chunk. App scale remainsunit; native
fractional text and protected-buffer UI.App integration still outstanding.

2026-10-01 native94494 LIVE sharedlock. Surface strict compile+runtimePASS
in96583; remaining Settings fixture had old positional Canvas aggregate from
priorprimitive stage. Updated to named fields/default density. Settings/native
apps+90s Desktop validation continuing in /tmp/cubit-ui-text-native-complete.log.
Affine contract-only parameters use scoped warning annotations; no algorithm
changes. Prior64990/96583 terminal1 before native run; own sources FROZEN.

2026-10-01 native64990 LIVE under shared lock after52936 terminal1 on
pre-existing glyph-library strict warnings. Removed redundant type visibility
in glyph_layout/storage/software; marked Affine.Clip contract-only dimensions
unreferenced. No algorithm edits. Persistent owner proof148 passed again.
Continuing surfaces/Settings/native/default90s with log
/tmp/cubit-ui-text-native-final.log. Shared source/GPR FROZEN.

2026-10-01 retry52936 LIVE under shared lock. First69296 terminal1 after
owner570 test, proof148 zero unproved, fullhosted font/density pixelsPASS;
strict surface project rejected redundant Lease/Address visibility in new
client_glyphs.adb. Removed duplicate use clauses only; proof/surface/Settings/
native checks continuing. Log /tmp/cubit-ui-text-integrated-final.log. Sources
FROZEN; no runtime/driver/image changes, app density remainsunit.

2026-10-01 shared69296 LIVE under command-scoped lock. Promoting UI TrueType
physical renderer +Client_Glyphs, four cache budget-preservation contracts,
UI consumer GPR source dependencies, test closure and owner/pixel tests.
Owned sources FROZEN; persistent proof/hosted/native default90s checks running.
Log /tmp/cubit-ui-text-integrated.log; serial -native.serial. No driver/runtime/
Mesa/image edits. Private native61892 TERMINAL0. Apps remain unit-scale until
protected-buffer/configure integration, despite new drawing capability.

2026-10-01 private UI29686 TERMINAL0: TrueType physical-pixel oracle PASS
5 densities x2 faces with clipping/alpha, proves samples differ from nearest
2x enlargement; prior256density primitive andunitfont/control tests alsoPASS.
Private owner93804 TERMINAL0 proof148 zero unproved/justified incl genericcache,
570mask/exhaustion/crossowner/pinned-close testsPASS. Starting isolated native
Desktop compile of /tmp/cubit-ui-text. Shared sources remain unchanged; adopting
this shared will require UI consumers to include compositor/display/allocator
source paths and explicit-source test closures (settings-renderer also needs
prior client_canvas_geometry). No fulltoolkit or nativeDPI claim yet.

2026-10-01 private owner93804 proof148 checks zero unproved/justified and
570 actual-mask cases PASS, including noncopyable views/cross-owner Finish
rejection, warm reuse, held stability, 32-reader exhaustion, delayed terminal
close. Snapshot /tmp/cubit-client-glyphs contains explicit unchanged-limit
postconditions for four existing cache mutators; shared cache source unchanged.
Private UI TrueType binding /tmp/cubit-ui-text uses this owner and direct A8
blending at physical density; first hosted compile53116 lacked test Face
operator visibility, corrected. Five-scale/two-face real-mask oracle retry next.
No shared source/GPR edits, no native claims for this text stage.

2026-10-01 new private /tmp/cubit-client-glyphs: CPU glyph-cache owner reuses
Compositor_Glyph_Cache/Storage/Memory/Arena and existing Rust density rasterizer.
Read leases pin masks; bounded eviction, terminal close waits for own readers.
512KiB arena plus metadata, no Mesa imports. Hosted real-mask/held-reader test
and SPARK proof33501 live, disjoint outputs. Shared sources/GPR untouched.
Next bind into toolkit TrueType physical drawing; no native-density UI claim yet.

2026-10-01 SHARED WINDOW RELEASED:87151 TERMINAL0 DPI primitive integration.
Persistent geometry22 checks zero unproved/justified, 256 density pixel oracle
(alpha bitmap/font8x16/nested views/oversized clips), normalfont/control and
surface routing tests PASS. CCLpreview/nativeDesktop/shell/files build PASS;
90s desktop-display/finalfaultscan PASS, nativeWorkbench window704. Desktop/
Display staging cmpPASS. Logs /tmp/cubit-ui-density-{integrated-final,native-final}.log,
-native.serial; exactsource -source.json. No own live jobs/lock. CCL Value_Text
required narrow existing-formatter cases for new text/character/list kinds;
no VM semantics edits. All applications still unit-scale and UI.App stilllegacy.
Next: direct native-density TrueType masks (reuse existing Rust rasterizer and
bounded/proved glyph policies), then protected two-buffer toolkit/configuration
integration. Do not rerun old promotion script: shared sources now promoted
with subsequent test-syntax and Workbench-case fixes. No image/driver edits.

2026-10-01 native87151 LIVE under shared lock. Hostedfont256density and
surface regressions PASS35762; CCLpreview blocked by pre-existing missing
Text/Character/List enum cases in Workbench Value_Text. Narrow case extension
now delegates all three to existing CCL.VM.Value_Image; no VM/type changes.
CCLpreview/nativeDesktop+shell/files/default90s run continuing; sources FROZEN.
Log /tmp/cubit-ui-density-native-final.log. One retry-script quoting error
terminated before edits/build, corrected before this live run.

2026-10-01 retry35762 LIVE under shared command-scoped lock. First28226
terminal1 after persistent proof22PASS; hosted main -gnatwe rejected new old-style
array aggregates. Fixed only test syntax to Ada2022 brackets. Continuing hosted
font/surface/CCLpreview checks +native/defaultdesktop90s; owned sources FROZEN.
Final log /tmp/cubit-ui-density-integrated-final.log; no policy behavior changed.

2026-10-01 shared28226 LIVE under command-scoped lock after graphics52895
release. Promoted final Canvas density/origin/fills/bitmaps/font8x16/views,
compatibility aggregates, 256-density oracle and client_canvas.gpr. Running
persistent proof, hostedfont/surface/CCLpreview builds, native Desktop/shell/files
and90s desktop-display regression. Owned sources FROZEN; log
/tmp/cubit-ui-density-integrated.log, serial -native.serial. Apps remain unit
scale; native TrueType-density and protected UI.App migration still pending.

2026-10-01 private native60819 TERMINAL0 Desktop compile with final DPI
fill/bitmap/font8x16/view changes. Promotion attempt rejected by busy lock;
graphics52895 packaging window now owns shared state and will notify release.
No shared UI edits/no own jobs/lock. Final private oracle64538 and22-check proof
PASS. Shared docs mark primitive stage private/pending.

2026-10-01 bitmap64538 TERMINAL0: expanded private256-density oracle PASS
alpha bitmaps, clipped nested views, bitmap-font pixels, unit font/control
regressions. Geometry22 checks zero unproved/justified includes inverse sampling.
Private /tmp/cubit-ui-density/native Desktop compile starting; shared sources
unchanged while graphics1148-step Mesa build holds57597. Promotion script now
also includes private bitmap/font8x16 changes. TrueType density still pending.

2026-10-01 private final2935 TERMINAL0:16 geometry checks zero unproved/justified,
256density pixel grids+nested clips+NaturalLast clip overflow controls PASS;
existing font/control checks PASS. Shared UI remains unchanged. Ready script
/tmp/cubit-ui-density/promote.sh requires full shared lock, verifies original
source hashes, promotes UI fields/fills/view mapping +6 aggregate compatibility
updates, persistent proof and tests, native Desktop/shell/files and desktop-display.
Waiting graphics57597 build84583, no own live jobs or locks. Text/bitmap density
is not yet migrated, applications keep unit scale. Logs -checks.log/-final.log
in /tmp/cubit-ui-density prefix; scripts and source in /tmp/cubit-ui-density/.

2026-10-01 private primitive34924 TERMINAL0:256 rational-density pixel grids
including nested view clipping PASS; existing TrueType/control pixels PASS;
geometry10checks zero unproved. Added overflow-safe clip-end geometry and unit
scale fast path, final private rerun pending. Request next native window after
graphics57597 for UI primitive source/GPR promotion and compatibility checks.
No shared UI/App/Desktop source changes yet.

2026-10-01 DPI primitive work private /tmp/cubit-ui-density: explicit logical
canvas density/origin, SPARK pixel-edge geometry, Fill_Rect/Set_Pixel and nested
Surface.View phase preservation. Shared UI sources unchanged; applications will
not enable density until native glyph/bitmap drawing migrate coherently. Hosted
font+256-density pixel oracle and geometry proof starting, no native lock/jobs.

2026-10-01 SHARED WINDOW RELEASED:67881 TERMINAL0 selective client repaint.
Persistent hosted4096 pixel-model cycles +16 SPARK checks zero unproved/justified;
90s native protocol and final fault scan PASS. Native15 frames finalrepair143px,
full51792RGB exact; marker657/protocolPASS673. Desktop/Display staging cmpPASS.
Logs /tmp/cubit-client-damage-integrated.log, -native.serial, -pixels.log;
source manifest -source.json. No own live jobs/lock; no UI.App/runtime/driver/
offered-image changes. Next toolkit must adopt begin-frame ownership and these
repaint debts together with DPI-aware Canvas/font drawing, bounded resize
replacement and nonblocking pending-retirement handling. Manual CCL workbench
Window_Present copies from its retained source and needs explicit migration too.

2026-10-01 acquired command-scoped shared lock, native67881 LIVE: promoted
15-frame selective-repaint fixture and client_damage.gpr. New policy/test/main
FROZEN. Persistent proof+test and90s native protocol/pixel observer running;
logs /tmp/cubit-client-damage-integrated.log, -native.serial, -pixels.log.
No UI.App/runtime/driver/offered-image edits.

2026-10-01 repaint debt proof53738 TERMINAL0:16 checks zero unproved/justified,
4096 pixel-model cycles/3192 accepted publications PASS. Native private compile
83532 TERMINAL0. Shared new client_frame_damage sources+test match proof snapshot;
UI.App and shared fixture unchanged. Lock3380061 still live graphics interlock.
Prepared /tmp/cubit-client-damage-promote.sh (requires shared lock, checks original
fixture hash) to install15-frame selective repaint fixture+GPR and run90s native
protocol/pixels. Final frame must paint143pixels, full51792pixel observer unchanged.
Logs /tmp/cubit-client-damage-{proof,native-compile}.log, source hashes -source.json.
No native selective-repaint execution claimed yet. No own live jobs or lock.

2026-10-01 graphics interlock3380061 confirmed live; request next shared
native window for protected-client repaint fixture/GPR. Working isolated proof
/tmp/cubit-client-damage-private meanwhile; no edits to shared fixture yet.

2026-10-01 new owned scope: client_frame_damage SPARK two-buffer repaint debt,
isolated hosted tests and desktop-check protected fixture selective repaint.
No UI.App migration claim yet; no runtime/driver/image edits. Will take shared
lock for fixture/build-definition edits and native verification.

2026-10-01 SHARED WINDOW RELEASED:30436 TERMINAL0 producer policy8 proof checks
zero unproved/justified +4096 cycles; rebuilt90s native protocol/pixel run PASS,
final fault scan PASS. New protected owned-frame component drives13 frames;
exact51792 RGB pixels including13x11 patch, client marker660/finalPASS672.
Pending-reader release fails/retains backing; destroy then retries reclaim,
reallocation zero-word check passes. Original protocol adversary retained.
Logs /tmp/cubit-client-frame-integrated.log, -native.serial, -pixels.log;
source hashes /tmp/cubit-client-frame-source.json. No own jobs/lock, runtime/
Desktop service/driver/offered images untouched. UI.App still legacy; next wire
component into render-loop ownership + repaint debt and DPI-aware canvases.


2026-10-01 native30436 LIVE under command-scoped shared lock. Promoted new
ui/client_frame_state.* (proof model) and client_frame_buffer.* (owned-memory/
IPC adapter), hosted test/GPR and desktop-check main/GPR. Existing UI.App and
Desktop service unchanged. Running persistent proof/test +90s protocol/pixels;
these owned sources FROZEN. Logs /tmp/cubit-client-frame-integrated.log,
/tmp/cubit-client-frame-native.serial, /tmp/cubit-client-frame-pixels.log.
No runtime/driver/offered-image edits. Prior private native54685 terminalPASS.


2026-10-01 producer adapter ready, private /tmp/cubit-client-frame-vs9z_ebl.
Policy44399 proof8/tests4096 PASS; native54685 compile PASS. Fixture keeps all
previous protocol adversary checks and adds owned/protected13-frame pixel run,
pending-reader release rejection, quarantine reuse denial, and final reclamation.
Request next native window (current live graphics holder3366783) to promote four
new UI component sources + desktop-check main/GPR + hosted project. No existing
UI.App/client behavior changed yet; renderer/runtime/driver/image untouched.


2026-10-01 client-frame work private at /tmp/cubit-client-frame-vs9z_ebl.
Producer policy44399 TERMINAL0 proof8 zero unproved +4096 lifecycle tests.
Adapter/native38903 TERMINAL0 compiles: owned allocation, RO before staging,
exact retirement before RW, revoke/confirm/release, fail-closed unknown replies.
Preparing fixture retaining original protocol adversary plus separate protected
13-frame pixel run. Shared UI/main/GPR unchanged. Request next native window
for new ui/client_frame_{state,buffer}.* and desktop-check fixture/GPR after
current graphics packaging; no runtime/Desktop service changes.


2026-10-01 SHARED WINDOW RELEASED:24020 TERMINAL0 rebuilt native Desktop/check,
90s CuBit/QEMU protocol+pixel observer PASS, final fault scan PASS.13th frame
uses13x11 partial patch;312x166 window51792 RGB pixels exact. FinalPASS669,
serial /tmp/cubit-partial-final.serial, logs .log and -pixels.log; source manifest
/tmp/cubit-partial-source.json. Staged Desktop/Display cmp default PASS.
No own jobs/lock; graphics may take deferred Mesa packaging window. No runtime/
driver/offered-image edits. Source_Damage22 proof checks zero unproved/justified,
191731 interval+602420 sample checks. First98520 catalog preboot failure fixed
by peer; no native partial-damage failure. Next mixed-DPI/client toolkit ownership.


2026-10-01 retry24020 LIVE under shared command-scoped lock after verified
CCL image repair68789 all20 PASS/released66887. Rebuild +90s protocol with
13th-frame partial-patch RGB observer; sources FROZEN. Logs
/tmp/cubit-partial-final.log/.serial and /tmp/cubit-partial-final-pixels.log.
No compositor edits to CCL/driver/runtime or offered images. Prior98520 terminal
pre-boot failure remains historical; policy proof22 and hosted tests alreadyPASS.


2026-10-01 SHARED WINDOW RELEASED:98520 TERMINAL1 before QEMU. Persistent
Source_Damage testsPASS191731/602420, all22 checks proved, native Desktop and
check builds PASS. Headless initrd realization failed CCL TOO_MANY_ITEMS after
catalog growth. Own unused pixel observer3347646 terminated after runner failure;
no QEMU started, no native pixel verdict. Lock released; graphics may fix planner
and rebuild ccl-image. Will wait for verified fix before retry. Sources stable,
no CCL/driver edits from compositor. Log /tmp/cubit-partial-integrated.log.


2026-10-01 native98520 LIVE under command-scoped shared lock after graphics
48174 release. Promoted Source_Damage policy/test/GPR, Desktop integration,
13th-frame partial-patch fixture/checker. Running persistent tests/proof then
rebuild +90s protocol/pixel observer. These sources FROZEN until terminal.
Logs /tmp/cubit-partial-integrated.log, /tmp/cubit-partial-native.serial,
/tmp/cubit-partial-pixels.log. No runtime/driver/offered-image edits.


2026-10-01 private15542 TERMINAL0 native partial-damage units PASS. Ready
promotion script /tmp/cubit-partial-promote.sh, blocked only by live graphics
brief48174/holder3345738 build-definition window. Shared main unchanged. Please
leave next native window free for persistent proof +90s protocol/pixel check.
Mapper private22 checks zero unproved,191731 interval and602420 sample checks.


2026-10-01 source-damage16135 TERMINAL0 private testsPASS191731 intervals/
602420 samples and22 proof checks zero unproved. Isolated partial publication
fixture /tmp/cubit-partial-3fec55z8; first56647 native compile visibility error,
fixed explicit Natural conversion before retry. Shared source unchanged. Native
lock held by graphics3340019; request next window to promote mapper/main/test/
checker/GPR and run persistent proof + exact patch pixel regression.


2026-10-01 SHARED WINDOW RELEASED:75482 TERMINAL0 rebuild +90s CuBit/QEMU
protocol and pixel observer PASS.12 visible replacements, malformed/stale/
duplicate/foreign publish rejection, visible retirement denial, resize retention,
destroy both slots with exact-once loan return. Final frame312x166 at120,130:
51792 exact RGB pixels. Logs /tmp/cubit-publish-native.log/.serial and
/tmp/cubit-publish-pixels.log; source manifest /tmp/cubit-publish-source.json.
Runner final fault scan PASS, configured timeout complete. Staged Desktop/Display
cmp default builds PASS. No own jobs/lock, no runtime/driver/offered-image edits.
Publish now real; logical source extent separate from physical pixels; slot alias
teardown avoids double return. Full surface damage conservative for now. Next
partial-damage mapping, native mixed-DPI validation and toolkit/capability/events.


2026-10-01 native75482 LIVE under command-scoped shared lock after graphics
32189 release. Promoted publish handler + committed logical source extents,
alias-safe teardown, native12 replacement fixture and exact RGB observer.
Desktop/test/checker sources FROZEN through rebuild/QEMU90s+observer. Logs
/tmp/cubit-publish-native.log/.serial and /tmp/cubit-publish-pixels.log.
Private57826 TERMINAL0 native unit compile PASS. No runtime/driver/image edits.


2026-10-01 publish6155 TERMINAL0 isolated native units PASS. Prepared real
12-frame two-buffer visible replacement fixture + exact RGB scanout observer.
Promotion attempt lock-busy holder3332035; shared main unchanged (still stage/
retirement only). Private /tmp/cubit-publish-rakkeqqn, promote script
/tmp/cubit-publish-promote.sh. Request next native window for Desktop/test+
additive tests/desktop-protocol/check-publication-pixels.py. No runtime edits.


2026-10-01 isolated publication integration6155: publish exact epoch/ticket,
logical vs physical source extent, teardown alias cleanup,12 replacements and
visible-window pixel fixture in /tmp/cubit-publish-rakkeqqn. No shared main
edits yet. Preparing next native window after graphics51394 release confirmed
by peer terminal note. Runtime/Mesa/driver untouched; offered images unchanged.


2026-10-01 SHARED WINDOW RELEASED:37675 TERMINAL0, rebuilt native Desktop +
desktop-check and90s CuBit/QEMU protocol PASS; stage-grant marker652, finalPASS664,
runner final fault scan PASS. Configured timeout finished, no manual stop.
Graphics may take next requested intel-gpu/main.adb compile window; no own jobs
or queued native runs. Desktop/runtime sources stable; offered NUC image untouched.
Log /tmp/cubit-stage-grants-final.log/.serial; source hashes
/tmp/cubit-stage-grants-source.json. Staged Desktop/Display match default builds.
Stage/retirement now actual2-slot grant integration,140 cycles, short grant,
owner checks, stale epochs, pending/duplicate receipts, legacy mode exclusion,
destroy release tested on legacy backend. Publish/drawing/toolkit remain next.
First39207 fixture failure historical; its dimensions crossed existing minimum
size clamps, fixed allocation/dimensions (no service admission weakening).


2026-10-01 retry37675 LIVE native build +90s protocol under shared lock.
First39207 TERMINAL1: fixture resized below existing minimum dimensions, so
later required byte extent outgrew its grant; stage correctly refused. Fixed
fixture to128/160x96 and16page grant. Main unchanged, both mains FROZEN until
terminal. Logs /tmp/cubit-stage-grants-final.log/.serial. No driver changes.


2026-10-01 native39207 LIVE under command-scoped shared lock: promoted Desktop
2-slot grant staging/retirement handlers + native desktop-check140-cycle grant
fixture. Owned main sources FROZEN through rebuild/QEMU90s. Private8751 native
unit compile PASS; first11705 subtype error fixed. Logs
/tmp/cubit-stage-grants-native.log and .serial. No runtime/Mesa/image edits.
Publish handler still absent; no immutable drawing/feature negotiation claim.


2026-10-01 pending Desktop main integration: prepared bounded2-slot real grant
staging and retirement query + teardown cleanup; publish remains next. Shared
edit attempt lock-busy (live holder3302835); NO main edits yet. Script
/tmp/cubit-stage-integration.py. Request next Desktop source/compile window.
Prior58361 is TERMINAL0 (proof27/native-unitPASS), contrary stale older entry.
No own live jobs/lock. Runtime codec and graphics unchanged.


2026-10-01 promotion58361 LIVE under shared command-scoped lock, after graphics
release verified. Private96816 TERMINAL0 testsPASS and27 proof checks zero
unproved/justified, both4096 and productionPositiveLast instantiations. Promoted
compositor_surface_state spec/body, state tests/GPR + production model. Running
persistent proof/test + native Desktop unit compile. These sources FROZEN until
terminal; runtime codec and graphics untouched. Log
/tmp/cubit-configure-retirement-integrated.log.


2026-10-01 native lock busy, live graphics holder3294292 verified. No shared
policy edits made. Private37578 LIVE at /tmp/cubit-configure-retire-evf9gku0:
Configure retires stale Candidate, preserves Visible and epoch/ticket identities;
phase matrix checks exhaustion, bad receipts, delayed retirement and readmission.
Request next short idle window for owned compositor_surface_state spec/body,
surface_state_tests promotion/native compile. No graphics/runtime changes.


2026-10-01 SHARED WINDOW RELEASED: codec94317 native runtime PASS;
hosted47301 TERMINAL0 PASS28074 and all128 proof checks, zero unproved/justified;
native5046 TERMINAL0 rebuilt Desktop/desktop-check +60s CuBit/QEMU PASS,
configuration marker649, protocolPASS660, runner final fault scan PASS.
Logs /tmp/cubit-publication-integrated-{hosted,native}.log and native.serial.
Promoted runtime child spec/body now authoritative; staged proved sources cmp
match all6 portable files, /tmp/cubit-publication-promoted.manifest.json.
No own live jobs/lock. No Mesa/driver/offered-image edits. Configuration-only
native integration: stage/publish/retirement/toolkit and mixed-output query
validation remain outstanding. Private incomplete manifests historical.


2026-10-01 promoted codec94317 TERMINAL0 native runtime compile PASS.
Initial41027 failed only mandatory line length; fixed before final compile.
Persistent hosted47301 LIVE (testsPASS28074, proof running); native5046 LIVE
under shared lock rebuilding Desktop + desktop-check then60s protocol QEMU.
Runtime child and Desktop/test main sources FROZEN until native terminal.
Private61277 all128 proved with zero unproved/justified; shared proof pending.
Logs /tmp/cubit-publication-integrated-{hosted,native}.log and native.serial.


2026-10-01 private proof61277 TERMINAL0: all128 checks proved, zero
unproved/justified, /tmp/cubit-publication-materialized.log. Ghost decoded
result materialization resolved final configuration roundtrip; no assumptions.
Claiming child codec runtime spec/body promotion and native compile, then
persistent hosted proof/tests and Desktop protocol native regression under
command-scoped lock. No Mesa/driver/image edits. Prior49210/48818/64612 terminal.


2026-10-01 SHARED WINDOW RELEASED: native74486 TERMINAL0, full rebuilt
Desktop + desktop-check and60s CuBit/QEMU desktop-protocol PASS, including
new configuration query checks (serial line648 and finalPASS660). Runner fault
scan PASS; configured QEMU timeout completed normally, no manual stop. Log
/tmp/cubit-configuration-retry{,-run}.log. Staged Desktop/Display cmp PASS,
GRUB unchanged. No own native jobs/lock. Graphics may take requested Mesa
instance fixture build window; no overlapping edits from compositor.
Shared runtime codec remains frozen; private proof49210 LIVE only.

2026-10-01 native retry74486 LIVE under command-scoped shared lock:
make desktop/desktop-check now passed manifest prerequisite and is compiling
Desktop dependencies, then60s desktop-protocol QEMU. Log
/tmp/cubit-configuration-retry-run.log; serial /tmp/cubit-configuration-retry.log.
Owned main/runtime/library sources FROZEN. Disjoint private proof49816 also
LIVE; it does not mutate runtime sources or native outputs.

2026-10-01 latest private codec10397 TERMINAL0 hosted testsPASS; proof89329
has89 checks,87 proved,2UNPROVED (recovery assertion + config roundtrip).
Publication postcondition depends on unproved recovery, not independently done.
Current source hashes /tmp/cubit-publication-current-private.manifest.json.
Candidate NOT promoted/native-checked. All own jobs terminal; source notes
above retain current native-unit-only status and required whole native run.

2026-10-01 configuration native-unit22076 TERMINAL0: actual freestanding
Desktop and desktop-check main.adb compile (-c -u). No link/QEMU claim; full
85049 failed in current peer CCL manifest compile first. No own jobs live,
shared lock released. Source/query/native test in tree; no image/staging change
from unit-only build. Main files stable. Next complete full regression after
CCL coherence, then wire stage/publish/retirement and toolkit DPI writes.
Private proof attempts41920/93686/65148/58515/89329 all TERMINAL0 with UNPROVED
contracts/assertions, NOT formal passes. Latest private candidate bitwise pack
plus ghost recovery has same unresolved recovery/round-trip issue and has NOT
been promoted; previous private manifest hashes are historical now. Shared
codec remains at native23278 tested source. Logs /tmp/cubit-publication-*-proof.log.

2026-10-01 configuration native85049 TERMINAL2 before Desktop/QEMU:
shared CCL manifest build currently fails (List_Element/From_Element/Make_Node
mismatches in ccl-vm.adb); not a compositor test failure or pass. Log
/tmp/cubit-configuration-native-run.log. Direct unit-only compile attempt was
lock-busy (live holder3223080). REQUEST next short shared window for Desktop
and desktop-check -c -u main.adb compilation, then native regression when CCL
is coherent. Both owned main files FROZEN pending compile. No own native jobs.
Private codec proof89329 LIVE; no shared runtime edits, no claims of proof0.

2026-10-01 native configuration integration active: own Desktop main.adb
and desktop-check/main.adb; add owner-authenticated current per-surface logical
extent/density/layout query with nonwrapping SPARK configuration generations.
No edits to graphics/CCL/native-session sources. Publication codec proofs remain
in private snapshot while native sources stable. New query is preparatory,
not negotiation of working stage/publish handlers. Build under shared lock.

2026-10-01 FINAL CHECKPOINT: private36384 TERMINAL0, testsPASS28074 layouts
plus expanded golden/header tests. Actual proof125 checks,123 proved,2 UNPROVED
(Encode_Configuration/Encode_Publish round trips); no assumptions/justifications.
All82 runtime checks proved. This is NOT a completed formal gate. Clean private
candidate /tmp/cubit-publication-0a5b_xh9; exact hashes and evidence inventory
/tmp/cubit-publication-final-private.manifest.json. Guarded promotion attempt
was lock-busy (verified live holder3190511); no shared source changed. Runtime
still at original native23278 stable codec. No own jobs live; do not confuse
private proof with shared source. Next acquire lock to promote/native-build
reviewed candidate, resolve the2 contracts, then wire actual service/toolkit.
Persistent publication.gpr and expanded test are in tree; docs describe wire
contract and explicitly state native handlers/toolkit are not implemented.

2026-10-01 publication finalization: private proof16655 TERMINAL0 with
unproved diagnostic assertions; NOT a completed proof. Removing temporary
assertion probes, retaining required round-trip contracts and verified primitive
packing contracts. Attempting locked promotion + native compile, followed by
final-source hosted test/proof. Two full round trips remain work, not waived.

2026-10-01 publication hosted regression32351 TERMINAL0:28,074 layouts,
independent wire vectors plus exhaustive six-codec length/flags/reserved fields.
Persistent publication.gpr added under acquired lock after graphics release.
Runtime shared child stays FROZEN at native23278 compiled source; original
proof has6 unproved functional contracts, all runtime checks proved. Private
/tmp/cubit-publication-0a5b_xh9 is being strengthened, not yet copied back;
latest26837 proof LIVE, disjoint output only. No shared build/native jobs.
Do not report private proof counts as evidence for current runtime source.

2026-10-01 PUBLICATION NATIVE FIX STABLE: native runtime23278 TERMINAL0,
shared lock released. Child package now has Ada_2022 pragmas, narrow failure
variant discriminants and required GNAT runtime style. No native compile errors
or style diagnostics. Evidence /tmp/cubit-publication-native.log. Graphics may
retry procmgr/devmgr/devices; publication runtime sources FROZEN while the
isolated hosted proof finishes. Hosted execution already PASS28074 layouts.
No outgoing-thread messaging authorization; coordination remains via this note.

2026-10-01 client publication codec work: owning new portable child package
CuBit.Desktop_Protocol.Publication and hosted test fixture. New child is auto-discovered by native user_runtime.gpr: initial native build
failed on missing Ada_2022 pragma, now corrected. No Desktop behavior changes. Canonical generation,
grant, ticket and complete density/layout configuration wire checks precede
native integration; unsupported new requests remain unsupported until handlers
are wired. Shared GPR write deferred: actual lock holder3148437, peer build60630 terminal2.
Private hosted snapshot /tmp/cubit-publication-0a5b_xh9 used instead.
Initial hosted compile20509 terminal1 (failure-variant aggregate typing); fixed.
Codec source still being validated; avoid native rebuild until stable note.

2026-10-01 native fault/retirement97646 TERMINAL0: injected text-batch
failure recovered with retained software text; Mesa cube9 frames/reuse PASS.
Three registered targets9,437,184 bytes, no scene/drag allocation, all readers
retired and final pixel charge0. Logs /tmp/cubit-no-scene-fault{,-run}.log.
Own build window released; no native jobs live. Graphics launcher scope clear.
Surface close policy/tests97284 TERMINAL0:13 proof checks0unproved;4096
replacement tests plus every valid phase pair/both retirement orders PASS.
Terminal close preserves IDs pending reader confirmation and blocks revival.
Owned edits: Compositor_Surface_State spec/body, hosted fixture and backend
docs. No runtime/procmgr/CCL or shared build-script edits. All own jobs terminal.
Client publication audit recorded: UI.App exposes attached writable pixels;
Present returns before flush and future repairs retain source. Next integrate
configuration generations and immutable publication with actual grants/toolkit;
delaying one reply cannot protect future reads. This policy is not yet native.

2026-10-01 SHARED WINDOW RELEASED: native3806 TERMINAL0 full mixed-DPI/
arrangement/primary/scaling regression PASS. Exact VM quit after observer PASS;
runner0 and command lock released. No compositor source edits or native jobs
pending in this window. Graphics may proceed with requested ccl-configurations
spec/body, procmgr main/GPR and devmgr startup issuance; NO compositor ownership
overlap. Next native fault/retirement check will wait for your window to finish.
No-scene ledger6 targets20,496,384 bytes; previous7 allocations34,209,792.
Parity29,030,400RGB exact, workspace proof8 checks0unproved.

2026-10-01 graphics edit-window request acknowledged here: compositor owns no
ccl-configurations spec/body, procmgr main/GPR, or devmgr startup issuance.
Current own native3806 LIVE under shared lock; allocation ledger confirms six
targets20,496,384 bytes and native scene0. Main compositor/workspace sources
frozen. I will mark the native window released here once terminal. No outgoing
thread message authorization; this ownership note is the coordination surface.

2026-10-01 scene allocation removal active: owned main.adb + new pure
Compositor_Workspace policy/tests. Native keeps privateScene/backBuffer null,
requires logical arithmetic bounds instead of scene byte capacity. Native
text/client readiness checks now use output pass; legacy allocation preserved.
Next hosted proof + native mixed-DPI and retirement/fault validation.

2026-10-01 FINAL direct-preview slice: normal86227 TERMINAL0 full mixed-DPI/
arrangement/primary/scaling native PASS, extra Appearance125% capture86184
TERMINAL0 visually inspected. Fault98354 TERMINAL0 basic dual-output PASS +
retained CPU text recovery. Normal unit parity14,515,200RGB exact; fault vs
earlier normal staged Settings12,902,400RGB exact. Hosted86836 PASS384 real
writer cases +144 Settings; image43981 proof45 checks0unproved/395307 cases;
fine18408 proof130 including dependencies0unproved, oracle49635 PASS139264.
All own VMs/jobs terminal; locks released, staged Desktop/Display cmp PASS,
GRUB unchanged. Source scopes: sampling/image_sampling, wallpaper spec/body,
main settingsWallpaper and removal of obsolete Use_Mesa flag; owned hosted
fixtures only. Settings has no staging now. Private scene reserve still exists;
NEXT decouple logical layout capacity/readiness from pixel storage and remove
that native allocation. GPU/client-density/whole-service proof/timing remain
open. Evidence docs/compositor-backends.md and /tmp/cubit-settings-direct-preview*.

2026-10-01 direct preview normal86227 TERMINAL0: full mixed-DPI native PASS,
manual Appearance125% capture86184 TERMINAL0 verified scale in serial and
visually inspected PNG. All Settings pixels now direct; private scene reserve
still allocated but no longer used for preview. Hosted86836 TERMINAL0 real
writer384 cases/guard padding/damage tiling/legacy parity +144 Settings cases.
Final native text-fault98354 LIVE shared lock held; sources frozen. Logs
/tmp/cubit-settings-direct-preview{,-fault}{,-run}.log. No peer sources edited.

2026-10-01 direct preview86227 LIVE shared build lock held, Desktop build
succeeded; mixed-DPI native regression running. Production sampler/wallpaper/
main sources frozen. Image sampler45 checks0unproved; shared fine transform
130 checks0unproved. Hosted source-grid tests PASS139264 cases, image395307
cases plus fractional centre clamps. Preparing disjoint real-writer guard/parity
hosted fixture while native runs; no other-agent sources edited.

2026-10-01 preview sampler scope: new pure compositor_image_sampling unit
and disjoint hosted tests/proof. Extract aspect-fill/fit/center placement and
subpixel bilinear source indices before replacing wallpaper pointer staging.
No native build or main/UI edits yet; no other-agent sources.

2026-10-01 native Settings complete for controls: normal11098 TERMINAL0 and
final-source fault8348 TERMINAL0, full mixed-output/arrangement/primary/scaling
regressions PASS. Fault markers confirm partial text failure + retained CPU
repaint. Normal/fault twelve captures29,030,400 RGB components exact; both125%
primary PNGs visually inspected. Gradient62432 proof14 checks0unproved and
independent exhaustive oracle25758 TERMINAL0. All own VMs stopped via exact
QMP after observer PASS; runners0, shared locks released. Staged Desktop/Display
cmp PASS, GRUB unchanged. Evidence /tmp/cubit-settings-native-{controls,fault}*
and /tmp/cubit-settings-gradient-*.log; docs updated. Native callbacks draw
Settings controls directly; only236x150 wallpaper preview stages in existing
private allocation. Next direct preview sampler and remove scene allocation /
decouple logical layout capacity, then client density. Full goal active; main
clip/memory glue still trusted, no whole-service proof or hardware/timing claim.

2026-10-01 native11098 TERMINAL0 full mixed-DPI/arrangement/primary/cursor
regression PASS. Settings125% PNG visually inspected: native controls/text.
Integrated proved gradient helper and skipped off-damage preview staging.
Final normal/text builds + full mixed-DPI fault8348 LIVE shared lock held;
main/gradient sources frozen. Logs /tmp/cubit-settings-native-fault{-run}.log.
Only wallpaper preview remains staged; private scene reserve not removed yet.

2026-10-01 native Settings11098 LIVE under shared lock, production sources
frozen. Unit-scale Settings captures match prior mixed-output run exactly.
Gradient62432 TERMINAL0: exhaustive16777216 blend cases, bounded row tests,
SPARK14 checks0unproved. New compositor_gradient unit not wired until native
run ends; plan then add preview physical-damage culling and integrate helper.

2026-10-01 native Settings binding active: own main.adb only. Bind fills,
strokes, gradients, text and shared controls to actual output; keep only
wallpaper preview raster staging as next removal target. No peer source edits.

2026-10-01 control renderer checkpoint: native53546 TERMINAL0; dual-output
functional regression PASS. Exact own VM stopped via QMP after observer PASS;
runner final PASS, command lock released. Hosted37432 TERMINAL0. Six Settings
captures match prior separation run: 12,902,400 RGB components, zero changes
(rows0..699, clock excluded); Displays PNG visually inspected. Scoped diff
check clean, GRUB unchanged. UI control style now shared by canvas and generic
primitive rendering. Native Settings bindings/staging removal still pending.
No new proof/GPU/timing claim; full goal active. Evidence documented in
docs/compositor-backends.md and tests/settings-renderer/README.md.

2026-10-01 control generic: hosted37432 TERMINAL0 all144 Settings cases plus
all button/tab states and tiny controls PASS. Native53546 LIVE shared lock held,
build succeeded, dual-output observer running; production UI sources frozen.
Logs /tmp/cubit-settings-control-{renderer,native-run}.log. No new SPARK claim.

2026-10-01 native Settings control binding prerequisite: own cubit-ui.ads/adb
and tests/settings-renderer. Factor existing button/tab drawing into a generic
primitive renderer; legacy entry points delegate to the same implementation.
No Canvas ABI or runtime edits. Native Settings wiring still pending.

2026-10-01 morning checkpoint: Settings separation native86496 TERMINAL0,
headless desktop-dual-output PASS (drag/maximize/cursor/Settings). Hosted14401
TERMINAL0: 144 page/style/clip cases, all seven callbacks, null pixel target
PASS. Logs /tmp/cubit-settings-render-separation-run.log and
/tmp/cubit-settings-renderer-hosted.log. Both jobs finished; command-scoped
shared lock released. Scoped diff check clean. Native Settings callbacks are
still pending: this separates layout from pixel access but does not remove
Settings staging or add a SPARK proof. Full compositor goal remains active.

2026-10-01 Settings renderer separation active: own desktop_settings spec/body
only. Layout now emits seven required synchronous renderer operations; raw
wallpaper canvas access isolated to explicit legacy adapter. Public Draw keeps
legacy behavior while native output bindings are prepared. No UI/runtime/other
agent edits. Next compile/native Settings regression under shared lock.

2026-10-01 FINAL native-output slice: dual83925 TERMINAL0 all mixed-DPI/
primary/arrangement/drag/maximize/cursor assertions PASS; fault47965 TERMINAL0
194673 cube pixels +9 reuse frames, retained CPU text, teardown charge0;
legacy65328 TERMINAL0 default dual-output regression PASS at original0.8s.
All own VMs/jobs terminal, command locks RELEASED. Staged Desktop/Display cmp
PASS, GRUB unchanged, scoped diff check clean. Main final normal/text builds
precede83925; stable text parity20370 RGB exact, PNGs visually inspected.
Changes: direct per-output scene traversal, actual density text/decorations,
zero whole-scene staging (opt-in), no drag snapshot, no-op title clicks avoid
stale-writer repairs. Settings still CPU staging; clients still unit density.
Existing SPARK policy reused; no new proof or hardware timing claim.
Next remove Settings staging, extract/prove render-scope/pixel-memory glue and
wire client density. Full goal active. Evidence /tmp/cubit-native-output-*.log
and docs/compositor-backends.md final section. No other-agent source edits.

2026-10-01 fault47965 TERMINAL0: final-source partial-text fault PASS194673
cube pixels, nine reuse frames, retained-CPU marker, no uncertain/legacy-text
fallback; targets/readers retired, tracked pixel charge0. Stable text all20370
RGB components equal normal capture; fault PNG visually inspected. Native
legacy regression65328 LIVE under command lock, default dual-output fixture.
Sources frozen; no other-agent edits.

2026-10-01 GRAPHICS WINDOW OPEN: dual83925 TERMINAL0, all native Desktop/
primary/arrangement/125%-150% scaling assertions PASS; exact VM quit after
observer success, final runner PASS. Shared command lock RELEASED, no own jobs.
Zero Desktop staging; native scaled text screenshot visually inspected.
I will use docs/read-only checks while graphics does requested short compile
window, then run already-built text-fault image. No source/build edits pending.

2026-10-01 dual83925 LIVE, normal/text builds succeeded; same native fixture
now passed double-click maximize, split drag, cursor/window restore and
Settings/arrangement stages; primary/scaling still running. Native scene
copy counter zero. Command lock held; will stop own VM immediately after
observer PASS and leave graphics requested short window before fault run.
Log /tmp/cubit-native-output-dual-idle{,-run}.log. Sources frozen.

2026-10-01 dual94963 TERMINAL1, exact VM stopped/lock released. Click-size
invalidations still forced stale-writer full repair (539448px) between title
edges. Now suppressing native no-visual-change title press/release entirely,
retaining original500ms click policy and real movement/focus invalidations.
Normal/text rebuild + same native assertions next. No graphics source edits.

2026-10-01 dual90657 TERMINAL1; exact test VM stopped, shared lock released.
Split drag/restoration passed after duplicate logical repaint removal. Maximize
exposed unnecessary full-window repaint on focused title clicks; fixing no-op
focus/drag invalidation (same click interval), then rerunning. No other sources.

2026-10-01 graphics window OPEN: dual44305 TERMINAL1, own VM stopped via
its exact QMP socket after observer failed. Command lock RELEASED; no own
job/lock. Normal/text builds succeeded. Captures prove fixture sampled before
Workbench first paint/input settling (opened=placeholder, spanning=unmoved;
later settled image correct split at x798). Next adjust functional TCG settle
allowance, rerun mixed DPI and text fault after graphics compile window.
No build of graphics bootstrap sources while its note marks them live.

2026-10-01 next native-output validation nonblocking lock unavailable;
no job created (empty /tmp/cubit-native-output-dual-run.log). Source updates
complete; pending locked normal/text builds plus mixed-DPI native run.

2026-10-01 native-output normal40475 TERMINAL0: live cube194673 pixel
oracle PASS, nine retirement frames, native output scene marker, zero desktop
staging bytes, renderer/readers retired and pixel charge0. Command lock
released. Adding emergency glyph physical blend and CPU-only Settings staging
(no transient Mesa source imports), then final native/fault/mixed-DPI checks.

2026-10-01 native-output build27174 TERMINAL0. Live native cube test40475
RUNNING under command-scoped shared lock (currently prerequisite build).
Sources frozen; log /tmp/cubit-native-output-normal-run.log. Own Desktop main
only. No Mesa/i915/other-agent edits. Will release automatically at terminal.

2026-10-01 native-output integration active: own Desktop main only; output
scene traversal uses existing proved geometry/sampling, physical text and
writer repair. Settings temporarily retains clipped unit-density staging.
Graphics native19313/lock83304 is independent source snapshot; no own jobs
or shared lock. Native builds deferred until lock available.

2026-10-01 final91620 TERMINAL0: fresh native/normal/fault Desktop builds;
native260 Mesa+260 retained-CPU font oracles,384 parity cases and all prior
suites PASS. Live partial-text fault PASS194673 cube pixels, retained software
marker (no legacy-text fallback), target/readers retirement and pixel charge0.
Four text regions match previous normal Mesa capture all20370 RGB components;
PNG visually inspected. Staging Desktop/Display cmp PASS, GRUB unchanged.
Lock71551 explicitly RELEASED; no own jobs/locks. Proof71050 terminal0:
893 checks zero unproved. Next actual per-output scene traversal; glyph
GPU/CPU paths now share fixed backing. Full goal active, no HW/240Hz claim.

2026-10-01 active91620 uses existing native-aee5fe5697d39dd4 softpipe
archives, compositor-owned adapter and font archive; does NOT consume
userspace/mesa/anv/anv_cubit_memory.{c,h} or session-attach-test.c. Shared
lock71551 remains held through native180s + live90s fixture; no source edits
during runs. Native520 owner oracles already PASS; final scans still pending.

2026-10-01 lock71551 HELD, final proof71050 terminal0 with explicit every-
mask-retired postcondition and32distinct warm-mask transition test. Starting
fresh native probe+normal/fault Desktop rebuild, then180s native suite and
90s live partial-text fault session. Sources/scripts frozen during run.
Evidence /tmp/cubit-retained-fallback-native-{run,serial}.log and
/tmp/cubit-retained-fallback-live-serial.log. No other-agent source edits.

2026-10-01 hosted69867 TERMINAL0: retained fallback all runtime cases +
all-unit proof zero unproved (report glyph-renderer/obj/gnatprove). New
Same_Raster handles5/4=10/8, invisible CPU draws skip raster/allocation.
No own jobs or lock; graphics79145 active. Ready to rebuild native/normal/
fault Desktop and run native owner+live partial-text recovery when released.

2026-10-01 native builds18202 terminal0 before final equivalent-scale and
invisible-draw corrections; no native jobs running. Shared lock90158 being
RELEASED now for graphics budget integration window. Hosted69867 live in
disjoint glyph-renderer output: final retained fallback contracts + cases.
All compositor/GPR sources stable during proof. Native rebuild/live fault
fixtures will reacquire later; no other-agent source edits.

2026-10-01 lock90158 HELD; compositor GPR dependencies updated. Retained
software path connected to Mesa Desktop with separate truthful marker and
bounded repaint on partial CPU preparation failure. Hosted75545 live in
/tmp/cubit-retained-fallback-build. Native owner fixture extended260 CPU
font oracles after32queued cancellation/context shutdown. No other-agent
sources edited; no native build started yet.

2026-10-01 retained fallback integration active: own Glyph_Renderer/Storage/
Software and tests. CPU transition retires Mesa masks, keeps existing backing;
CPU paints borrow a checked read lease. No other-agent sources touched.
Graphics18360 currently holds lock; GPR additions/native build deferred.

2026-10-01 native22809 TERMINAL0:384 software/Mesa placement+blend cases
PASS, all prior compositor/glyph-owner/baseline regressions and final runner
scan PASS. Production Desktop/Display staging cmp equal, GRUB unchanged.
Lock59043 explicitly RELEASED, no own jobs/locks. Hosted6461 terminal0:
383 proof checks zero unproved,12288 physical frames +262144 blend oracles.
Evidence /tmp/cubit-glyph-software-{boundaries,native-run,native-serial}.log.
Next connect retained glyph backing to physical software painting, then
per-output scene traversal. Prefer reusing existing512KiB backing, not a
second cache. Live Desktop fallback remains unit-density until integrated.
No hardware/240Hz claims; full goal remains active.

2026-10-01 lock59043 acquired, GPRs promoted. Starting native compositor
software/Mesa parity build and180s fixture; own sources frozen during build.
Hosted6461 terminal0:383 proof checks zero unproved,12288frames/262144blends.
No other-agent source edits; /tmp/cubit-glyph-software-native-{run,serial}.log.

2026-10-01 software92517 terminal0: full proof zero unproved and hosted
12288 frames/262144 blends PASS. Stronger damage-subset contract6461 live.
Native384-case software/Mesa fixture includes translucent tint/coverage;
needs native.gpr source-list addition. Lock36768 terminal1, graphics21681
held. Request next build window for GPR promotion +180s native fixture.

2026-10-01 hosted glyph software runtime PASS12288 frames +262144 blends;
proof82591 live in /tmp/cubit-glyph-software-build. Native placement oracle
now compares software with Mesa/exact expected pixels; GPR addition/native
run pending. Lock13162 terminal1 (graphics factory lock currently held).
No own lock/native jobs. Request next short compositor native-test window.

2026-10-01 active physical-DPI software glyph path: owned new
compositor_glyph_software.{ads,adb}, hosted oracle/proof. Needed before
per-output traversal can preserve native-density fallback. No Mesa runtime
or shared runner edits; graphics31113 owns build window. No own lock/job yet.

2026-10-01 fault15237 TERMINAL0: native mesa-window PASS194673 cube pixels,
partial-text repaint + CPU fallback, targets/readers retired and pixel charge0.
Four normal-vs-fault stable text regions match all20370 RGB components exactly;
captures visually inspected. Default Desktop/Display staging cmp PASS, GRUB
unchanged. Command-scoped lock RELEASED automatically; no own jobs/locks.
Docs record live text integration and proof752 zero unproved. Next: per-output
native-density scene traversal; complete goal remains active, no HW claims.

2026-10-01 fault15237 acquired lock after graphics91842 release; native
partial-text draw fixture now RUNNING, /tmp/cubit-desktop-text-fault-
{run,serial}.log. Lock is scoped to this command and releases automatically
on completion (90s QEMU fixture). No script edits or other-agent sources.

2026-10-01 fault-session attempt85583 terminal1 before launch (shared lock
owned by graphics91842). No own lock/job. Live text docs updated; normal
fixture/proof fully complete. Retrying only after graphics promotion window.

2026-10-01 normal live63715 terminal0, cube oracle194673 pixels, active Mesa
text/client markers, target retirement and pixel charge0 PASS. Staging cmp
PASS and GRUB unchanged. Lock92456 RELEASED (holder terminal0); no own
native jobs. Graphics runner-edit window is now actually open. Next fault
session deferred until that window finishes; docs only meanwhile.

2026-10-01 proof43399 terminal0: 752checks0unproved + owner and Desktop
modes0..6 PASS. Lock46479 was released through proof window for graphics.
Now reacquired92456 for freshly relinked normal/text-fault Desktop and one
normal live cube/target-retirement session; /tmp/cubit-desktop-text-live-
{run,serial}.log. Will release after this chunk before fault session to leave
 another runner-edit window. No other-agent sources or runner edits.

2026-10-01 Desktop normal/text-fault builds23295 terminal0. Shared lock46479
releasing now for graphics requested runner-edit window; no own native jobs.
Hosted proof has three circular invariant checks on private-helper public
Valid calls; replacing with identical private Consistent predicate, then
rechecking in disjoint hosted build. No runtime policy change.

2026-10-01 Desktop text body/main wired: bounded chunk completion, RGB tint
normalization, scene replay before publication and cached-move recovery.
Hosted Desktop modes0..6 PASS; proof47820 running; native normal/text-partial
Desktop builds running under46479. Test-only C macro fails after first actual
glyph draw, for live repaint fixture. No runner modifications/other-agent sources.

2026-10-01 active Desktop text integration. Own Desktop backend interface/
bodies, existing owned main drawing flow, compositor_text clipping policy.
Lock46479 held for GPRs/builds/native/live verification. No other-agent sources.

2026-10-01 glyph owner milestone verified. Hosted88732 terminal0 SPARK694
checks0unproved,3151 terminal0 extended faults/pressure. Native62770 terminal0
PASS260 pixel oracles/eviction,32 pending cancel, pinned payload and genuinely
full rounded arena/recovery, final charge0, all prior regressions/fault scan.
Logs /tmp/cubit-glyph-owner-final-{run,serial}.log. Desktop/Display staging
cmp PASS, no GRUB diff. Lock41058 releasing, no own live jobs or other-agent
edits/commits/pushes. Next wire owner into Mesa Desktop text with explicit
ordering and full fallback repaint, then per-output scene traversal. Full
goal active; no hardware performance claims or live text activation yet.

2026-10-01 final62770 serial PASS complete owner + rounded-arena + allprior
regressions; still awaiting180s runner exit/final scan. Lock41058 held.
Do not start another native job or release until62770 terminal.

2026-10-01 final native62770 ACTIVE after57759 terminal0, added only native
rounded-arena fixture, production proof sources unchanged. Logs
/tmp/cubit-glyph-owner-final-{run,serial}.log. Lock41058 still held.
Hosted3151 terminal0 true rounded-arena and target/density cases PASS.

2026-10-01 native57759 terminal0 complete PASS owner260 pixels/eviction,
32 queued cancel, pinned budget pressure and all regressions. Hosted3151
terminal0 adds true rounded-arena allocation refusal with charge523936 below
512KiB, target rollover and density mismatch. Added same rounded-arena case
to native fixture ONLY AFTER57759 terminal; now final native rerun planned
under still-held41058. No production policy changes after694-check proof.

2026-10-01 owner88732 terminal0: 694checks0unproved, integrated fault tests
PASS; exact queued-slot lease invariant and no-pinned/no-queued backing release
assertions proved. Native57759 active, logs /tmp/cubit-glyph-owner-native-
{run,serial}.log, under lock41058. No source edits while native job runs.
New owner not yet Desktop live activation; docs identify storage/FFI contracts.

2026-10-01 glyph owner hosted96093 terminal0 tests+all-unit proof PASS with
queued-command/exact-slot read-lease invariant and cache reader-frame contracts.
Final proof now adds explicit no-pinned/no-queued backing-release assertions.
Lock41058 held for owned test GPR/native fixture edits. No native job yet.
Native260 raster-oracle/eviction cases + pending-cancel/arena pressure ready.

2026-10-01 active checked glyph renderer owner joining existing cache, arena
backing bridge, Mesa mask imports, placement and batch leases. Own new
compositor_glyph_storage/renderer units; no native jobs or lock yet. No
Desktop activation until failure/retirement/native tests and proof pass.

2026-10-01 placement milestone complete: hosted44568 terminal0, SPARK292
checks0unproved,8192 hosted layouts PASS. Native19686 terminal0 PASS192 exact
placement cases plus prior regressions and final fault scan. Production
Desktop/Display staging cmp PASS; no GRUB diff. Lock18473 releasing; no own
live jobs. New placement not yet live Desktop text. Next join key/arena/lease/
view ownership into one checked glyph renderer using placement and batching.
Full goal active, hardware timing outstanding, no commits or pushes.

2026-10-01 native19686 serialPASS192 exact placement cases and all prior
regressions; still live awaiting180s runner exit/final scan. Lock18473 held.
Do not treat serial PASS as completed runner until handle19686 is terminal.

2026-10-01 hosted44568 terminal0: placement292checks0unproved and8192-case
oracle PASS. Native19686 active under lock18473, build passed and runner
preparing QEMU; /tmp/cubit-glyph-placement-native-{run,serial}.log. No source
edits during native run; final proved sources are those compiled natively.

2026-10-01 graphics59743 release confirmed in note/process check; compositor
now holds18473 for placement native build/test. No shared runner edits.
Hosted44568 proof running disjoint; same immutable placement sources.
Native evidence /tmp/cubit-glyph-placement-native-{run,serial}.log.

2026-10-01 compositor59355 terminal/released. Two native-lock attempts busy;
no own native jobs. Hosted8192-case placement oracle PASS; refining one
modular conversion proof (no suppressions), hosted proof running disjoint.
Native192 exact placement fixture ready, waiting for shared lock.

2026-10-01 lock59355 releasing now after owned placement GPR edits. No native
job started. Graphics can take its requested brief admission_dispatch.gpr
window. Compositor doing disjoint hosted placement checks/proofs meanwhile;
will reacquire later for native192-placement-case verification.

2026-10-01 active physical glyph placement in new owned compositor_glyph_placement
units: snap logical origin once, use raster dimensions at unit scale, rotate/clip.
Lock59355 held for new test GPR and native verification. No other-agent sources.

2026-10-01 mask batching milestone verified: hosted70937 terminal0 SPARK436
checks0unproved, native24363 terminal0 all132 batch cases+prior regressions
and Mesa Desktop link PASS. Lock76441 released, no own live jobs. Production
GRUB restored; Desktop/Display staging cmp PASS. Full goal still active;
no live Desktop text activation or hardware performance claims.

2026-10-01 active bounded mask batching in owned compositor cache/Mesa adapter
and new SPARK batch packet. Shared lock76441 held for compositor GPR edits,
all-unit proof and native verification. Hosted70937 terminal0: 436checks0unproved,
packet/cache batch fault tests PASS. Native build54925 terminal0; QEMU14625
terminal1 /tmp/cubit-mask-batch-native-{run,serial}.log: one-level color
difference from float tile retention versus serial UNORM rounding. Added
fixed-point final-rounding blend oracle; native97533 now active in
/tmp/cubit-mask-batch-oracle-{run,serial}.log. Common Mesa state bound once
per batch. Oracle97533 terminal1 nearest-texel boundary ties; final24363
terminal0: all132 batch oracle cases, prior regressions, final runner scan
and Mesa Desktop link PASS.
Final uses4:1/4:3 geometry avoiding source-boundary ties, not a guarantee of
floating tie choice. Logs /tmp/cubit-mask-batch-final-{run,serial}.log.
Own final runner started before failed oracle runner timeout/cleanup ended;
old97533 terminal confirmed before final probe, no other-agent jobs involved.
Production grub restored verbatim from completed old cleanup backup
/tmp/cubit-mask-batch-production-grub.cfg after24363 terminal0. Final grub,
Desktop/Display staging cmp PASS. Lock76441 releasing; no own live jobs.
No other-agent sources, commits or pushes. Next join key/arena/lease/
view-slot ownership and physical per-output placement before live text activation. No driver/runtime/ANV sources.

2026-10-01 active shared-context mask cache slots and checked import hook in
owned compositor_cache.*, with hosted fault tests and proof. Lock17133 from
prior turn already released; no own shared lock. Graphics has next requested
short native/test-definition window before compositor reacquires.
Graphics74110/25303/36845 completed, lock released per note/message. Compositor
now holds lock6729 for cache/mask wrapper test GPRs and native verification.
Hosted88430 terminal0 tests PASS128 masks/every failed shutdown position;
first all-unit86981 had one missing Shutdown Can_Retire loop invariant.
Fixed with invariant (no runtime behavior change); final6507 terminal0 all-unit
SPARK418checks0unproved plus fault tests and Mesa Desktop link PASS.
Native35784 terminal0 PASS320 raw +320 shared-context mask oracle draws, target
retirement preserving masks, color shader restoration, complete shutdown, prior
regressions. Evidence /tmp/cubit-shared-mask-cache-{hosted,proof,final}.log and
/tmp/cubit-shared-mask-cache-native-{run,serial}.log. Next compose glyph key/
arena/lease policy with these slots, physical text placement and batching;
do not activate dense text via one softpipe flush per glyph as a performance win.
Lock6729 released, no own live jobs; Desktop/Display staging cmp PASS.
No production text activation, hardware measurement, commits or pushes.

2026-10-01 active 64-bit glyph cache/read/arena identity widening and boundary
tests, preserving non-wrapping refusal and stale completion rejection. Own
compositor_identity.ads, glyph policy specs/bodies and test GPRs. Lock17133
held for native build-definition edits/verification; no other-agent sources.
Hosted61789 terminal0: identity boundary/cache/arena/memory tests PASS; cache197
and arena165 SPARK checks0unproved. Memory object now589952B, payload unchanged.
Native70029 terminal0 all320 glyph draws+regressions PASS; production Desktop/
Display staging cmp PASS. Lock17133 released, no own live jobs.
Evidence /tmp/cubit-glyph-identity-checks.log and
/tmp/cubit-glyph-identity-native-{run,serial}.log. Full goal remains active.
Next shared-context integration: use distinct fixed mask slots within existing
Compositor_Cache so Shutdown retires every mask import before context destroy.
Avoid a second Mesa context or untracked external handles; preserve client slots
2..9 separately from mask slots. Glyph key/arena policy and per-output placement
then wire through that same context. No live Desktop text activation yet.

2026-10-01 fixed glyph backing verified. Arena9210 / Memory76560+35409 /
Native17740 all terminal0. SPARK161checks0unproved, 512 hosted direct rasters
and guards, native320 retained A8 draws with actual arena/cache retirement PASS.
Evidence /tmp/cubit-glyph-arena-checks.log, /tmp/cubit-glyph-memory-final.log,
/tmp/cubit-mask-arena-native-{run,serial}.log. Payload524288B, object573568B.
Desktop/Display staging cmp PASS. Lock45390 released, no own live jobs.
Initial80095 failed before boot on runtime style issue fixed by owner; no other
agent sources edited. No production text activation, GPU or240Hz claim.

2026-10-01 active fixed glyph backing arena adapter, reusing Heap_Extents
ownership core unchanged with 128-byte cells (512 KiB payload). Own new
compositor_glyph_arena.* and hosted tests/proofs; no allocator-source edits.
Graphics native window completed per notification; no own lock/jobs yet.
Arena9210 terminal0: hosted reuse/fragmentation/density/boundaries PASS,
SPARK161checks0unproved including unchanged Heap_Extents and shared geometry.
Memory76560 terminal0 512 direct rasters+guards PASS; alignment rerun35409.
Now lock1193 held for native mask oracle using real fixed backing adapter.
Native80095 terminal1: probe compiled/linked, headless initrd refresh stopped
on unrelated userspace/runtime/gnat/cubit-capability_grants.adb:21 style line81
exceeds gnatyM. Graphics-owned runtime edit not changed here. Native execution
pending fix; lock1193 released so runtime owner could repair/build. Owner fixed
line; now lock45390 held for rerun. No other-agent source edits.
Next before production text integration: widen glyph cache/read and arena
identity counters from Natural to 64-bit non-wrapping identities. Current
exhaustion is safe/proved, but ~2.1B read acquisitions is too short for sustained
dense 240Hz workloads. Keep this ahead of enabling the live text path.

2026-10-01 bounded glyph-cache metadata/reader policy in new
compositor_glyph_cache.* and isolated hosted tests/proofs. No runtime/driver/ANV
or Desktop main edits.
Hosted55433/3011 terminal0, 4096 reuse/32 readers/128 slots/256 densities PASS;
final193 SPARK checks0unproved. Evidence /tmp/cubit-glyph-cache-final.log.
Native5178 terminal0, all320 masks PASS with cache lease withholding, zero final
mask charge and prior regressions. Evidence /tmp/cubit-mask-cache-native-{run,serial}.log.
Desktop/Display staging cmp PASS. Lock85461 released, no own live jobs.
Graphics requests next native link window; reserved, compositor will not
reacquire until graphics completes. No cross-thread reply authorization assumed.
Next production bounded backing storage/shared-context mask handles and text
placement; no Desktop activation/hardware/240Hz claim. Full goal active.

2026-10-01 A8 mask import/tinted composition verified in owned softpipe.c and
compositor.h, narrow Mesa mask FFI and native glyph composition oracle. Lock5826
released, no own jobs. No ANV/driver/runtime/font-cache or Desktop main edits.
Native build/test41060 terminal0: 320 tinted A8 draws + prior regressions PASS.
Evidence /tmp/cubit-mask-composition-{run,serial}.log. Initial18002/9822 compile
errors fixed before this verified run. Mesa Desktop link19032 terminal0;
/tmp/cubit-mask-desktop-build.log. Production Desktop/Display staging cmp PASS.
Graphics requested next native window for Intel driver and isolated ANV adapter;
reserved next; compositor will not reacquire until graphics window completes.
No cross-thread reply authorization is assumed. Next bounded retained mask
cache/admission and native-density text placement; no production activation or
hardware/240Hz claim. Geometry reuses existing proved SPARK affine path; shader,
blend and native pointer behavior are audited/regression-tested FFI.

2026-10-01 caller-owned glyph raster ABI verified. Jobs 83989/13151/5407/24038
terminal0; lock79901 released, no own live jobs. Own fonts/src/density.rs
and module declaration, compositor_glyph_ffi, hosted/native glyph tests and native
probe build definitions. Existing font cache/Ada UI/driver/runtime ABI unchanged.
Rust 48,640 reference cases, hosted Ada ABI 512 requests, native CuBit 12 masks
PASS with padding/short-buffer controls; native old pool/affine/pixel tests PASS.
Logs /tmp/cubit-density-raster-{tests,abi,final}.log and
/tmp/cubit-glyph-mask-native-{run,serial}.log. Desktop/Display staging cmp PASS.
Docs distinguish SPARK layout proof from trusted parser/raster/allocator/pointer
boundary. Native builder now rebuilds fonts-native to avoid stale archive seeds.
Next bounded mask texture/cache and per-output text integration; Desktop text
still unchanged. No GPU/240Hz/physical-latency claim; full goal remains active.

2026-10-01 glyph-density storage policy verified;49086terminal0, no jobs/lock.
New Compositor_Glyph_Layout +isolatedtests:256ratios/equivalent densitiesPASS;
SPARK98checks0unproved inclgeometry. Exact emrational, ceilbounds, alignedpitch,
bytecharge<=139264(136KiB extreme16x). No allocation/fontcache/runtime/main/
sharednative edits. Evidence /tmp/cubit-glyph-layout-checks.log. Docs explicit:
this is coverage-mask policy only; existingfontcache supports2sizes, production
textunchanged. Next auditedrasterizer caller-storageentrypoint/metrics, bounded
texturecache andperoutputtext integration; fullgoal remainsactive.

2026-10-01 checked live output integration verified;95461/19086terminal0,
no own jobs; lock99405 released. Main nowusesCompositor_Client_Output SPARKbridge
for allnewcoordinateadditions/narrowing. Hosted171072casesPASS,156checks0unproved.
1G explicitcube/nativephysicalgatePASS194673geometricpixels; physical-output
marker present, noCPUfallback, desktopstagingbytes0, finalpixelcharge0.
Evidence /tmp/cubit-client-output-proof.log and
/tmp/cubit-checked-live-cube-{run,serial}.log +serial-mesa-pixels.log.
39073 wrongfixture(quadsbinary/cubeoracle) rejected; superseded by19086.
Bothbackendsbuild; productionDesktop/DisplaycmpPASS. No driver/runtime/protocol
changes, commits or pushes. Singleunitoutputonly; multioutputnative-density
scene/text+clientprotocol and hardware/240Hz goals remain open.

2026-10-01 live output API wired;58666terminal0 functional Mesa-client frame
and teardownPASS, but512M loggedpipecreationfailure/CPUfallback, so NOT evidence
of live Mesa compositor path. Graphics has next shared native window reserved;
lock97474 released, no own jobs. Do not reacquire until graphics nativebuild done.
Added CUBIT_TEST_PHYSICAL_CLIENT=1 runner gate requiresnewmarker and rejects
fallback. Next rerun QEMU_MEMORY=1G MESA_WINDOW_SCENE=cube withthatgate after
rebuilding Desktop. No API activation claim yet. Existing native128output oracle
and373checks0unproved stand. Prior surface-retirement-native and affine-cache-
desktop logs alsoCPUfallback; correcting docs. Source change main drawClientBuffer
unit directpool only; preserveoldbufferextent+damage, noresize stretch.
No driver/runtime/protocol edits or commits/pushes.

2026-10-01 damage clipping verified;19843/40196/83429 terminal0, no live jobs.
Lock48719 released. New Compositor_Affine.Clip exact intersection/unchanged
transform contract; Draw_Output nowrequiresphysicalDamage. Hosted307200cases
PASS; all-unitcomposition373checks0unproved. Native128cachedrequests8clipshapes
PASS +existingpixel/pool/retirement regressions. Empty requests are no-ops.
Evidence /tmp/cubit-affine-clip-verified.log, native-{run,serial}.log and
legacy-build.log. Initial48382compilevisibilityfailfixed beforeverifiedrun.
LegacyDesktoprebuilt/staged;Desktop+DisplaycmpPASS. No main-loop/runtime/driver
changes or commits/pushes. Next physical-output scene traversal and native
clientdensity/text protocol integration. No hardware/performance claim.

2026-10-01 affine retained-cache integration verified, lock94939 released. Own
Compositor_Cache generic checked-render path, Desktop_Compositor Draw_Output
API/backends, Mesa_Binding.Affine narrow FFI and compositor test GPRs.
No Desktop main/runtime/driver edits. Hosted8903terminal0 354checks0unproved
including Draw_Output and both Render_Checked instantiations; prior cache/fault
tests100reuse,composition2662,damage3200PASS. Native77438terminal0 passed but
new marker used stdout; fixed serial hook and24163 rerun shows128cached draws
PASS plusoldregressions; same jobnormalDesktop Mesa teardown zerocharge seen,
24163terminal0. Both backendsbuilt andproductionstaging cmpPASS.
No live jobs. Evidence /tmp/cubit-affine-cache-hosted.log and
/tmp/cubit-affine-cache-verified-{run,serial}.log,
/tmp/cubit-affine-cache-desktop-serial.log. Main loop integration stillpending;
next addphysicaldamageclip preservingphase, thenperoutputscene traversal.

2026-10-01 live Desktop source retirement verified;50183/68407/82507terminal0.
Only releaseSurfaceBuffer main change: proved ordered renderer-forget then
loan-return; retained attachment +fail-stop on uncertainty, clear afterboth.
NativeMesa196608composedpixelsPASS/clientclose/zerochargeteardown; native
DesktopprotocolPASS140reattachments/pendingrevoke/stalegrant/clientdeath.
Policy80fault/ordercasesPASS,14proofchecks0unproved. Native syscall failure
not injected; documentedboundary. Evidence /tmp/cubit-surface-retirement-
{native-run,native-serial,protocol-run,protocol-serial,policy}.log.
ProductionDesktop/Display cmpPASS. Lock72864 released; no live jobs.
No protocol/ANV/runtime changes or commits/pushes.

Coordination request for upcoming density integration: ANV's native_gpu_presentation
uses Desktop attach/present, and UI currently revokes old grant on Attach success.
I need a coordinated generation/ticket-aware protocol transition, retaining the
old visible buffer until matching Present; packed Grant_References can free one
wire word for configuration epoch. I own Desktop/toolkit/protocol work, but will
not edit the graphics-owned native_gpu_presentation without acknowledgment.
No ABI change yet. Independent physical-output rendering work can proceed.

2026-10-01 surface replacement policy verified;42498 terminal0, no own jobs
or shared lock. New Compositor_Surface_State +isolated tests only. Hosted4096
replacements, stale generations and same-generation tickets, retained readers,
generation/ticket exhaustionPASS. SPARK12checks0unproved including five exact
transition contracts in explicit Surface_State_Model. Earlier59162/79393/93149
compile failures fixed;83131 initial contracts pass, superseded by42498.
Evidence /tmp/cubit-surface-state-complete.log. No runtime/protocol/client/
Desktop/native/staging changes. Service integration next: configuration extent,
scale+layout+generation on attach/present, mapping and renderer retirement;
current Attach immediately replaces and events only carry logical dimensions.
Caller acquisition/producer completion and retirement truth remain trusted
integration obligations, documented. Fullgoal active, no commits/pushes.

2026-10-01 SPARK rational vertices verified;46706/76216 terminal0; no own jobs,
lock56687 released. New Compositor_Transform exact coefficient +corner contracts,
SPARK242checks0unproved incl affine/sampling/geometry. Hosted39936 exact samples,
88byteQuad ABI. Native128affine draws+13wrapper rejects and pool96/retirement32/
old192draw regressionsPASS. C now only rational-to-float conversion +fixed quad
submission; inverse rotation/offset/scale arithmetic moved intoSPARK.
Evidence /tmp/cubit-transform-final.log and /tmp/cubit-transform-native-{run,serial}.log.
Initial84379compilefail fixed Signed visibility;48863 coefficient-only proof
finished207zero but source expanded while live, superseded by final46706.
ProductionDesktop/Display cmpPASS. Docs boundaries updated; no service/driver/
Mesa library edits or commits/pushes. Next native-density protocol/per-output
Desktop integration; fullgoal/hardware240Hz remain outstanding.

2026-10-01 affine bridge verified;16126/37254 terminal0, lock45733 released; no own jobs.
Hosted39936 scissor/reference samples PASS; SPARK157checks0unproved.
Final native128affine draws PASS:3scales,signed offsets,4rotations,padding,
13invalid descriptors unchanged; pool96/retirement32/old192draw regressionsPASS.
Evidence /tmp/cubit-affine-final-checks.log and
/tmp/cubit-affine-native-final-{run,serial}.log. ProductionDesktop/Display
staging cmpPASS. No service/driver/Mesa library changes or commits/pushes.
New mesa_affine_ffi.ads isolates ABI; mesa_ffi.ads unchanged. Proof boundaries
documented docs/compositor-backends.md. C UV normalization/inverse rotation
still tested, not proved; move integer coefficients intoSPARK next. Desktop
integration/native density negotiation and fullgoal remain active.

2026-10-01 output sampling verified;77874 terminal0, no lock/native jobs.
Hosted PASS139264 independent rational/offset cases plus clipped150%seam,
square/non-square rotations, empty/off-target/extreme coordinates. SPARK113
checks0unproved incl geometry; axis exact formula/validity and Map bounds.
Initial12666/11200 syntax failures fixed before verified run. Evidence
/tmp/cubit-output-sampling-verified.log. No Desktop/FFI/runtime/Mesa/staging
edits or commits/pushes. Next affine draw bridge must preserve original source
phase when destination crosses output edges; existing unsigned in-target Draw
cannot represent it directly. Mapper is a reference, not a per-pixel division
performance path. Native-density integration/full goal remain active.

2026-10-01 active phase-preserving physical-output sampling core. Own new
Compositor_Sampling +hosted tests/GPR, no shared lock or native/staging edits.
Audit: current Draw unsigned/in-bounds destination contract cannot represent
off-target affine rectangles; clipping/restarting source would shift phase.
New exact pixel-centre mapping retains rational fractions through final source
index and inverse output rotation. Proof/independent oracle checks pending.

2026-10-01 density selector verified;91962 terminal0, no lock/native jobs.
Hosted PASS74340 rational-pair/rotation/negative-origin cases plus seam/fallback;
SPARK103checks0unproved including shared geometry, exact rational rank and
maximal intersecting density witness/primary fallback. Evidence
/tmp/cubit-density-selection-checks.log. No Desktop/runtime/native/staging edits
or commits/pushes. Native integration remains required: generation-bearing
per-surface configuration +attachment/present validation and per-output physical
composition; a density query alone cannot fix shared unit-scene resampling.
Current GetInformation stillUnitScale; documented gaps and intended contract in
compositor-backends.md. Full goal active; this is a proved policy foundation.

2026-10-01 active native-density output selection foundation, hosted only;
no native lock, jobs or Desktop/runtime/shared-build edits. Own new pure
Compositor_Density_Selection/tests/GPR. Highest rational density of outputs
with positive-area intersection, primary fallback offscreen/empty, exact
integer rank. Desktop currently exposes only global Unit_Scale; per-surface
query/configuration/backing-generation contract still needed before use.

2026-10-01 frame trace verified; all jobs terminal, releasing40282 now.
3770 hosted PASS100 saturation/reset +invalid cases; SPARK29zero unproved
including elapsed. Initial99457 test-only Count collision fixed. 88976 checker
PASS out-of-order outputs +12negative controls. Native68923 desktop-display
PASS;73547 frame +existing histogram checkers PASS21records/6windows/noinvalid
or dropped, production Desktop/Display cmp restored. Logs /tmp/cubit-frame-trace-
{checks-final.log,evidence-tests.log,native-run.log,native-serial.log,
native.json,final-checks.log}. No commits/pushes or driver/runtime/Display edits.
64records/window bounded opt-in diagnostic; overflow explicit, not lossless240Hz
nor physical performance. Missing input source timestamp/app causal serial echo
documented; never label next unrelated frame as keypress response. Goal active.

2026-10-01 active bounded frame trace, lock40282 held. Own new pure
Compositor_Frame_Trace/tests/GPR and timing-enabled Desktop hooks. Store64
validated completed frame records with output/session/frame and submit/observed
completion microseconds; explicit invalid/drop counts, publication outside
completion hot path. No input-causality/photon claim: device timestamp and app
input-serial echo are missing protocol links. No driver/runtime/Display edits.
Hosted proof and native timing build/test pending.

2026-10-01 owned pixel integration verified; all jobs terminal, releasing51877
NOW. Graphics can acquire next native window. No devmgr/GPU/Display/kernel edits.
31936 native Mesa PASS194673 pixels, renderer +readers retirement, five exact
3MiB releases and final zero pixel charge. 30626 native mixed-output PASS full
arrangement/primary/scaling checks,8 allocations47923200bytes. 43539 limited35MiB
PASS split drag/maximize/cleanup/Settings,7 allocations34209792bytes, optional
drag rejected before syscall. 18486 checker PASS9 negative controls. Logs
/tmp/cubit-owned-pixels-{native,mixed,limited}-{run,serial}.log and
evidence-tests.log. 92184 all three evidence checkers PASS, production Desktop
rebuilt/staged; Desktop/Display/devmgr cmp PASS. Last cmp used wrong GPU .svc
suffix; corrected .drv cmp PASS separately. Matched graphics pair preserved.
Scoped diff clean, no commits/pushes. Old unused sbrk ledger/tests removed;
new pure ledger proof unchanged35zero. Actual adapter not whole-Desktop proved.
Remaining: repeated native session reuse/fault injection, native density/GPU
targets, whole-memory admission, causal/physical timing and HW performance.

2026-10-01 active native owned pixel allocation integration, lock51877 held.
Own Desktop main and limited-storage comment, storage-budget evidence checker,
native teardown fixture marker. Exact ticket/address records and page-rounded
owned allocation replace pixel sbrk. Release only after Mesa/Display/grants;
failed allocation retains charge/slot. Native and fault verification pending.
No kernel/runtime/Display/GPU changes; preserve matched staged devmgr/GPU pair.

2026-10-01 allocation identity core verified; all jobs terminal, no lock held.
66929 PASS4096 reuse cycles/live neighbor, stale identity rejection, retained
allocation/release failures, descriptor exhaustion, NaturalLast charge/refund,
7-identity exhaustion fixture. Explicit SPARK instance PASS35 checks0unproved;
initial empty report and intermediate3 unproved contract-evaluation checks were
rejected, fixed with SPARK_Mode and short-circuit preconditions. Final evidence
/tmp/cubit-storage-identities-verified.log. New Compositor_Storage is NOT wired
into Desktop yet. No native/staging/build-script edits, no commits/pushes.
Next integrate exact address/size/ticket records and owned-memory syscall adapter,
retire Mesa +Display/grants before release, refund only confirmed release.
Graphics reports matched current devmgr/intel-gpu pair built/staged; no need for
old-devmgr seed on next native run. Preserve that pair. Full goal remains active.

2026-10-01 active allocation identity/accounting core. Own new compositor
storage ledger and isolated hosted/proof tests only; no shared build lock,
Desktop/native/build script edits this chunk. Graphics has the next native
window. Track eight allocations, nonreused identities, allocation uncertainty,
reader retirement and exact-once confirmed release. Adapter integration follows.

2026-10-01 reader retirement verified; all jobs terminal, releasing44760 NOW.
Graphics can acquire the lock for its devmgr.gpr update/build. No graphics,
devmgr, runtime or kernel source edits made here. Hosted35571 PASS80 ordered
partial-setup/failure/idempotence cases; first proof failed (no SPARK instance).
Explicit instance15713 PASS14 checks zero unproved, trusted callback returns.
Native23908 PASS194673 cube pixels, renderer retirement then real Display/grant
retirement marker; retained staged devmgr seed, current rebuilt kernel/new
Desktop. Initial86718 failed the known devmgr GPR dependency; no native pass
claimed for that run. Logs /tmp/cubit-readers-{checks.log,proof.log,
native-seeded-run.log,native-seeded-serial.log}. Production Desktop/Display
staging cmp-verified, scoped diff clean. No commits/pushes or freed/refunded
storage. Next owned-memory adapter must retain charge on zero allocation result
because kernel prefix-cleanup failure can leave quarantined pages. Goal active.

2026-10-01 request to graphics: native86718 Desktop/Mesa builds passed but
headless initrd failed devmgr: intel_gpu_extent_allocator.adb now depends on
intel_gpu_va_placement, absent from devmgr.gpr Source_Files. Please include its
ads/adb in your integration. No graphics/devmgr edits made here. Testing our
Desktop using existing staged devmgr.svc via MAKEFLAGS='-o devmgr'; kernel and
Desktop remain rebuilt. This is explicitly a retained dependency seed, not a
fresh graphics integration result. Lock44760 still held.

2026-10-01 active Display/grant retirement gate, lock44760 held. Own new
Compositor_Readers generic, isolated hosted/proof tests, Desktop closeOutput
adapter and narrow native teardown marker. Require exact Display release reply
and kernel Retirement_Confirmed for each granted target before clearing its
record. Uncertainty retains state and terminates Desktop; no storage refund/free
yet. No Display/runtime/kernel/GPU source edits. Verification pending.

2026-10-01 target-view retirement verified; all jobs terminal, releasing73859.
61100 explicit all-unit composition proof (-U) PASS120 results zero unproved;
default proof report rejected because it retained stale unvisited Mesa units.
Hosted cache checks PASS target-only retirement, preserved source, idempotence,
reimport and failures on either release. Native89001 PASS32 retire/reimport
cycles with changed stride/exact padding, plus prior pool/192-draw oracles.
Desktop59519 PASS194673 cube pixels then client exit and actual Desktop renderer
retirement marker; initial95128 stopped in CCL dependency, no CCL edits needed.
Logs /tmp/cubit-target-retirement-{proof-all.log,native-run.log,
desktop-retry-run.log,desktop-retry-serial.log}. Production Desktop/Display
staging cmp-verified; scoped diff clean. No commits/pushes or physical storage
free/refund. Next: confirmed Display/grant retirement and owned-memory storage
adapter; no reuse until all readers retire. Full hardware/DPI/timing goal active.

2026-10-01 active target-view retirement; lock73859 held. Own compositor
cache/adapter Forget_Targets, Desktop releaseDisplayBuffer hook, cache tests,
native Mesa oracle/GPR. Retire only two destination views; keep source imports
and context. Source allocations must not alias root-owned targets (existing
Desktop ownership boundary). No storage is freed/refunded by this API alone.
No GPU/driver/kernel/Display changes; proof/native checks pending.

2026-10-01 storage admission verified; all jobs terminal, releasing57542.
49597 hosted PASS131584 attempts, page/overflow/rollback/busy tests; SPARK25
zero unproved. 36577 native final mixed-output PASS8allocs47955968charged,
cap134250496. 89747 limited35MiB fixture PASS7allocs34238464, optional drag
layer rejected before allocator; split drag/maximize/cleanup/Settings stillPASS.
52276 both native evidence checkers PASS, production Desktop rebuilt/staged;
Desktop/Display cmp-verified. Logs /tmp/cubit-storage-budget-{final,limited}-
{run,serial}.log, final-checks.log and restore.log. No commits/pushes.
Ledger distinguishes confirmed sbrk rollback (cancel provisional) from later
grant/setup failure (retain committed bytes); unsettled request blocks reserves.
No committed refund. Whole compositor memory still unaccounted; kernel owned
memory API exists but replacing sbrk needs Mesa/Display retirement integration.
Current DP/owned-memory16MiB cap blocks4K BGRA; coordinate larger-layout contracts
before claiming high-density hardware readiness. No driver/kernel/Display edits.

2026-10-01 final ledger49597 PASS131584 attempts/page edges, SPARK25zero
unproved. Audit found kernel handleSbrk transactional rollback: ledger now
has one provisional request, cancels only confirmed allocation failure, commits
success before grant setup. Native36577 PASS mixed two-output with final derived
cap134250496,8allocs47955968bytes. New Desktop GPR storage-production/limited
policy fixture (35MiB) uses isolated build-limited-storage; native89747 active
to verify optional drag cache denial preserves desktop. Hold57542; no shared
driver/Display/kernel changes. Production staging restoration pending test exit.

2026-10-01 active retained pixel storage admission; lock57542 held. Own new
Compositor_Storage_Budget and isolated storage_budget tests/GPR, Desktop main
allocator adapter. Ceiling derived from eight maximum protocol allocations,
currently128MiB+32KiB (initial512MiB assumption corrected after protocol audit);
not reservation. Charge page
rounding/alignment before every target/private-scene/drag sbrk, never refund
failed or retired setup. Mesa heaps/borrowed app buffers explicitly out of scope
for this ledger. No runtime/kernel/Display/GPU changes. Verification pending.

2026-10-01 deferred repair verified; all jobs terminal, releasing53743.
47993 PASS native held reader/input progress, exactresync1 rejects0 presents6
inputIPC72; wrapper restored production. 61002 PASS Mesa194673pixels,9-frame
reuse,zero staging14submissions/3335172repair_px +evidence checker unit tests.
81407 PASS production desktop-display/window drag,zero staging21submissions.
Logs /tmp/cubit-deferred-repair-{held-final,mesa,default}-{run,serial}.log.
30385 PASS800 exact modeled frames/200 idle+600grids,SPARK77zero unproved.
Production Desktop/Display staged binaries cmp-verified, scoped diff clean.
Acquire now selects safe writer/invalidates cursor underlay, no speculative
paint; preparation after drains is proved idle/full-skip predicate. No whole-
Desktop proof or hardware performance claim. Also repaired stress fixture's
resync assumption using actual successful flagged publications and aggregate
strict counters (new checker +negative tests). No commits/pushes.
Next large gates remain output-local native density, GPU targets, global memory
admission and causal/physical timing; full goal remains active.

2026-10-01 deferred repair30385 hosted PASS800 pixel frames/200 idle +600grids,
SPARK77 zero unproved. Native18326 held-buffer/input-progress PASS, harness
failed stale source_gap=1 literal (observed2; publisher flags retries as resync).
Own narrow input-stress resyncReports counter +headless check-input-stream.py
and runner integration. Require exact published/observed resync totals, zero
rejects, and IPC budgets across all intervals. Check51546 PASS malformed cases.
Held native rerun active under53743; no driver/Display source changes.

2026-10-01 active deferred target repair, lock53743 held. Own Desktop main,
Compositor_Repaint preparation predicate and repaint_tests. Acquisition switches
writer address and invalidates old cursor underlay without painting; after input/
request drains prepare only for pending partial work, skip for full repaint.
No GPU/Display/driver/runtime edits. Hosted proof + native checks pending.

2026-10-01 integration verified; lock98110 RELEASED, all jobs terminal.
19766 PASS full mixed-output Desktop: split drag/maximize/cleanup, Settings,
above/left/below/offset, primary migration,125/150% scaling, workspace floor,
cursor repair/reflow/seams. Log /tmp/cubit-direct-pool-mixed-final-{run,serial}.log.
23309 PASS direct counters on held-reader7submissions and Mesa16submissions,
zero scene->transfer bytes; Mesa29781 exact194673 pixels. Native default and
held-reader input tests passed earlier. Final production Desktop76845 compiled
and staged; Desktop/Display cmp verified. CCL manifest briefly failed on a
concurrent CHARACTER_VALUE case, then current sources compiled successfully;
no CCL edits made here. 9825 used wrong1024-wide scaling fixture; correct refusal
at150%, then final mixed1280x720 run passed. New observer precondition prevents
that misconfiguration. Source changes and proof boundaries documented in
docs/compositor-backends.md; main now uses BP/RP and three Display targets.
Next: deferred/coalesced repair to avoid speculative work, output-local native
density, GPU-owned targets, memory admission and causal/hardware timing.
No commits/pushes, GPU/driver/runtime ABI changes, or native jobs remain.

2026-09-30 lock98110 now held for fixture edit and full native dual-output
arrangement/primary/scaling rerun. Settings selection wraps backward twice
(before final Config Inspector) in both Defaults and system.ccl, rather than
counting program entries. No product/driver changes. Earlier97881 released.

2026-09-30 lock97881 RELEASED; all native jobs terminal. Production Desktop/
Display staging cmp-verified. Mesa29781 PASS194673 exact final pixels, 16
submissions, zero staging; prior 2s screenshot was previous frame105 rather
than115. Runner now uses bounded exact-pixel readiness, unchanged oracle.
Dual66911 passed split drag/maximize/close checks, then Settings navigation
failed: screenshot shows SameBoy, because fixture's hardcoded six Down presses
now select a different menu entry. Read-only fixture diagnosis continues; no
native builds active. Default/held reader already passed. No driver changes.

2026-09-30 active Desktop pool integration, main.adb only. Initial compact
single-output rendering aliases acquired pool writer; cursor/scene catch-up
uses Compositor_Repaint. Scaled/multi-output compatibility still uses private
canvas, now copying per-slot missing damage. Reserved private scene remains
for layout transition; no allocation on that path. Native default95378 PASS window/drag; zero staging bytes. Lock97881 held.
Adding repair-work accounting, then held-reader, Mesa pixel and multi-output
compatibility native tests. No Display/driver source changes this chunk.

2026-09-30 root-owned Display pool verified: all jobs terminal, releasing9957.
Final88291 PASS native firmware display-grants and pool markers. Delayed42501
PASS dual-output pixels +stalled-output responsiveness, pool busy rejection,
correct original completion slot, revoked authority rejection and all pins
returned. Hosted/proof318 results zero unproved including dependencies. Initial
090D/E collision with discovery caught natively and corrected to0910/11/12;
cross-protocol regression added. No GPU endpoint changes. Production Display,
display-check, virtio-gpu and Desktop staging cmp-verified. No commits/pushes.
Logs /tmp/cubit-display-pool-native-final-{run,serial}.log and
/tmp/cubit-display-pool-stalled-{run,serial}.log. New check-display-pool.sh
reproduces portable checks. Next: bind Desktop pool/repaint to registered
targets and remove scene->transfer copy; current Desktop path still unchanged.
Pool native fixture uses output0; simultaneous pools per-output remains a test.
Graphics
confirmed no Display/protocol ownership overlap and no existing writable target
contract. Own new lib/display CuBit.Display_Pool_Protocol/Registry, isolated
tests/compositor/display_pool. Also own Display main and display-check main
integration, runner display-grants required pool marker. No GPU endpoint
changes. Existing Desktop unchanged; new protocol service + native fixture
verified. Physical measurements and GPU-owned targets remain separate.

2026-09-30 async menu refresh verified: all jobs terminal, releasing lock28190.
Final71743 PASS4500 scheduler cycles, SPARK18 results zero unproved, eight
adapter success/fault scenarios; native56898 PASS window/drag, timing and
input-during-launch. Checker33191 PASS nine-entry ready/publish ordering.
Production Desktop/Display staging cmp-verified, no commits/pushes. Logs
/tmp/cubit-menu-refresh-{final-build,native-run,native-serial,check}.log and
timing.json. Next: remaining settings/audio/layout synchronous input calls,
actual output target transport and per-output scene/DPI integration.
Own Desktop_Launch_Refresh, Compositor_Refresh, main integration and isolated
refresh tests. No Config/runtime/kernel/GPU changes. One generation-bearing
grant, sequential requests; quarantine ambiguous errors, publish only closed.

2026-09-30 asynchronous launch verified: lock19652 released, all jobs terminal.
Hosted/proof81818 PASS3000 interleaved launch/display traces and fault cases;
SPARK10 results, zero unproved. Native68776 PASS window/drag and all timing
stages; checker86715 confirms input between submission/completion of same launch
token. Logs /tmp/cubit-async-launch-{build,run,serial,check}.log and timing.json.
Production Desktop/Display staging cmp-verified. No commits/pushes.
New Compositor_Requests
allocates nonreused shared display/launch tokens and protects one launch buffer.
Desktop main now capSubmits launch, routes validated terminal reply separately,
updates single-instance PID by captured program name. No Procmgr/kernel/Display
changes; synchronous menu config refresh preserved. One pending launch, sticky
quarantine retains filename grant on uncertainty. Procmgr terminal reply means
filename read ended by trusted service contract, not grant revocation. Next
input-path work is bounded asynchronous menu refresh with atomic publication;
do not remove existing config refresh or reorder a visible menu under pointer.

2026-09-30 live timing integrated/verified. Default off; on scenario selects
separate build-timing. Native99512 PASS window/drag with instrumented Desktop;
checker23990/17534 accepts all five stages, counts28 input/40 request/30draw/
20submit/20completion, no invalid or drops. Pure elapsed spec SPARK3 results
0unproved; final58407 PASS12 explicit boundary examples, report tests34794 PASS.
Logs /tmp/cubit-timing-{build.log,native-run.log,native-serial.log,
native-report.json,report-tests.log}; /tmp/cubit-elapsed-final-tests.log.
All jobs terminal; releasing lock5192. Production staging cmp-verified off.
New main/GPR+timing-on/off policies, Compositor_Elapsed, tests/checker. Existing
Monotonic and Timing_Histograms reused, no kernel/GPU/Display edits. No commits.
Measurements are wall durations, not causal input-to-photon or GPU timestamps.
Next concrete latency audit: Load_Launch_Menu and trySpawnApplication perform
synchronous RPC from input handling; observed TCG long handlers are motivation,
not proven attribution. Shared Mesa builder timing support still pending.

2026-09-30 repaint/pool native oracle complete. Hosted23352 PASS600 independent
dirty-grid checks; SPARK75 results0unproved including damage/pool dependencies.
Native95880 PASS96 attempts in actual CuBit Mesa, 3 imported targets, 5 partial
render failures repaired, complete pixels exact and simulated Display-held
pixels stable. Repair area19055 vs98304 full-buffer area (not GPU traffic/FPS).
Original192-draw oracle and softpipe1024 pixels/992 triangle PASS. Logs
/tmp/cubit-repaint-{checks.log,native-build.log,native-run.log,native-serial.log}.
All commands terminal; releasing lock9372. Production Desktop/Display staging
cmp-verified. No commits/pushes or Desktop/Display/driver ABI changes.
New Compositor_Repaint queues all scene damage per slot, takes before rendering,
never clears on completion, full-invalidates failed targets. Pool/repaint now
exercised together with native Mesa but not wired into Desktop. Actual shared
output transport, buffer ownership and per-output scene integration remain.

2026-09-30 pool policy verified: hosted52568 PASS3000 held-display/newest-ready
cycles +quiescent failure, concurrent render/present, unknown completion,
stale epoch/frame and reused-slot fence traces. SPARK22 results0unproved,
including distinct slot roles and strict serial ordering. Log
/tmp/cubit-pool-checks.log; report build/pool/obj/gnatprove/gnatprove.out under
tests/compositor. All jobs terminal, no lock held. No native/staging changes.
Own Compositor_Pool +pool_tests.adb/pool.gpr. Not wired to Desktop yet; no
writable output-target transport exists (graphics confirms). Existing app->
Desktop forwarding is owner-authorized read-only, not writable scanout sharing.
Candidate root-owned target contract sent to graphics; agree concrete ABI
before implementing transport. Buffer-age repair and native integration remain.

2026-09-30 lifecycle extraction verified: hosted864 completion cases +replay,
cross-output, busy-submit, uncertainty and wrap traces PASS; SPARK11 results
0unproved. Native98397 held-reader PASS input-stream, stable transfer +input
progress. Mesa67716 PASS194673 geometric pixels +9 retired-buffer frames,
Mesa active. /tmp/cubit-presentation-validation.log,
/tmp/cubit-presentation-delayed-serial.log and
/tmp/cubit-presentation-mesa-{run,serial}.log. Production staging restored and
cmp-verified. All jobs terminal; releasing lock6152. No commits/pushes.
Desktop now uses private Compositor_Presentation.State for submit/retire; no
Display ABI/kernel/driver changes. Pool/direct rendering still pending.
Read-only next constraint: OP_DISPLAY_MAP_BACKBUFFER refuses unsafe derived
loans. Coordinate direct root-owner grants/resource IDs with graphics; do not
re-grant GPU->Display borrowed pages or alias in-flight scene storage.

2026-09-30 user established active compositor goal. Acceptance gates now in
docs/compositor-backends.md. Next owned work: extract actual Desktop output
retirement lifecycle into proved SPARK, preserving existing wire protocol,
then acquired presentation storage/resource contract. Graphics owns i915/ANV;
no hardware acceleration claim from the diagnostic triangle. No build jobs or
lock held in this goal-initialization chunk.

2026-09-30 density planner complete: isolated hosted35832 PASS785754 independent
admission cases; SPARK32 analysis results0unproved, including minimal upward
rounding, aligned BGRA rows, exact byte coverage and admission iff budget fits
within physical extent limits. /tmp/cubit-density-checks.log and
 tests/compositor/build/density/obj/gnatprove/gnatprove.out.
Own new Compositor_Density +density_tests.adb/density.gpr. No native/runtime/
Desktop/protocol changes or staging changes. GPR lock24963 released; all jobs
terminal. Planner is not yet used by clients/Desktop; native-density configure,
allocation and per-output rendering must land together. No commits/pushes.

2026-09-30 cursor batching complete. Native default66393 PASS window/drag;
delayed97519 PASS input-stream with input during stable held frame; final
Mesa45847 PASS exact194673 cube pixels +9-frame retired reuse, Mesa active.
Logs /tmp/cubit-cursor-batch-{default,delayed,mesa}-{run,serial}.log.
Production Desktop/Display staging restored and cmp-verified. All commands
terminal; releasing lock42842. No commits/pushes.
Desktop cursor updates coalesce per bounded input-loop pass instead of4ms timer;
remove unused deferred-frame timer; fast client redraw clears pending cursor
because it already painted it. Scheduler period hint4167us, budget4000us retained
(advisory only). No Display/kernel/i915/ANV edits. Existing SPARK components
unchanged; this is native-tested legacy Desktop integration, not a new whole-
service proof. Native-density DPI remains separate; documented zero-animation
policy and crisp per-output rendering requirements.

2026-09-30: final default Desktop72143 PASS window/drag; sparse4256 vs5928 bytes
in one frame. Log /tmp/cubit-damage-default-final-{run,serial}.log. All commands
terminal, releasing lock33917; production staging restored. No commits/pushes. Mesa53916 PASS
exact194673 pixels/9-frame retirement, Mesa active, sparse827952 vs938176 bytes
in one observed frame. Delayed reader9341 PASS stable fingerprint+input during
hold; fixed test-only once-per-frame logging false-negative. Final default test
PASS; production Desktop/display staging restored after delayed fixture.
SPARK108 results0unproved, hosted3200 damage grids +existing2662 geometry/100cache
cycles. Removed redundant bounding flushes from cursor/fast client redraws.
Sparse output-damage integration: Own new
Compositor_Damage SPARK policy +damage_tests; narrow Desktop flush/pump changes,
shared test GPR update. Graphics confirmed no Desktop conflicts and snapshot done.
Display wire protocol/retirement unchanged; no direct alias to in-flight buffers.

2026-09-30: native integration complete for current software compositor fixture.
Lock66556 released; all native/hosted commands terminal.
No commits/pushes. Default remains legacy; opt-in images are not staged.

Own userspace/lib/compositor/, tests/compositor/, docs/compositor-backends.md,
this note, and agreed narrow Desktop main/gpr hooks. Shared runner changes:
CUBIT_DESKTOP_IMAGE override and validated MESA_WINDOW_ANIMATION_WAIT_SECONDS
(default15, test override45); both approved by graphics. Authorized devmgr.gpr
fix adds required ccl-text_operations ads/adb after CCL owner's TEXT_VALUE fix.
No ANV/i915/kernel/Mesa library changes by compositor.

Native QEMU4CPU TCG evidence:
- Oracle51196 PASS192 draws/3 contexts, changed source reuse, exact opaque
  pixels, premult blend, clipping, unambiguous noninteger scaling, retirement.
  /tmp/cubit-compositor-oracle-{run,serial}.log.
- Actual normal Mesa Desktop89727 PASS cube194673 geometric pixels,9 initial
  frames,36 animated frames, pause/Escape and retirement. Mesa-active marker;
  no fallback/restart/allocation-failure markers. 1GiB and45s animation wait.
  /tmp/cubit-compositor-final-{run,serial}.log.
- Init failure44183 and quiescent draw failure79866 PASS exact cube screenshot
  and9-frame reuse through expected CPU fallback.
  /tmp/cubit-compositor-fault-{init,draw}-{run,serial}.log.
- Default GPU viewer85169 PASS exact synthetic RAM screenshot +root retirement.
  /tmp/cubit-gpu-viewer-native-run.log. Not Intel rendering.

512MiB fixture ENOMEM confirmed by test-only allocator wrappers:262368-byte
Mesa texture cache allocation fails. At1GiB actual backend works. Fixed15s
animation wait missed under TCG;45s is correctness-only. Softpipe is slower
than row copying here; no240Hz/1ms/physical latency claim. Scene/output copies
remain. CPU/memory/latency measurements and hardware path are follow-up work.

SPARK evidence unchanged:82 analysis results zero unproved for new planner,
policy, bounded imported-view cache and actual Mesa cache instantiation.
Hosted2662 geometry cases and100 reuse cycles with failure/bounds tests PASS.
C/Mesa/mapping validity remain trusted boundaries. Unknown draw/release access
requires restart; never return app grant before Mesa view retirement.

Graphics requested ANV hosted tests run under our lock: submission89590 PASS;
slab28503 initially missing prototypes, owner fixed header; retry58327 PASS1539
translated slices. /tmp/cubit-compositor-handoff-slab-retry.log.
