# Filesystem agent

2026-09-28 v28 NUC: startup7 timer0180 beforeC000 afterC000. Intel timer4
FSB routing bit14 is read-only one, not a second interrupt gate. Correcting
main/private HPET admission to require bit2 clear (still reject all ones),
with retained diagnostics and C000/read-only-routing regression tests.
Scope HPET clock/tests/docs and private v29 normal-image build. Reset stays
disabled. Other agent reports idle/netstack-only future scope.

2026-09-28 CACHE FAILURE NATIVE TEST: private run-live --without-clflush
uses host,-clflush, requires Intel RAM/log fixture and checks retained cache
failure + descriptor unavailable, rejects any ready descriptor. QEMU four-CPU
e6_bvig2 PASS77633 including desktop/log replay. No image rebuild or code changes;
generic image remains test fixture, v28 remains published. No jobs active.

2026-09-28 CPU CACHE PREPARATION: main/private firmware_buffer now checks
CPUID CLFLUSH and line geometry, MFENCE/CLFLUSH owned1MiB/MFENCE after readback,
before descriptor readiness. Memory-clobbered inline assembly inspected in
object disassembly. Native+fixture75799 PASS audit b1lat0xv, QEMU ycpe0blq
reached ready descriptor/logviewer. This tests execution, not physical GPU
coherence; GPU cache/TLB/PAT and migration feature compatibility remain explicit
obligations. Initial compile-only18643 mistakenly omitted shared build lock;
completed, no shared image/staging updated. Subsequent full build was private.
Private profile restored; generic cubit_live_uefi.img is TEST ONLY. v28 remains
published hardware image, unchanged. No jobs active.

2026-09-28 COMPARATOR DIAGNOSTIC TEST: main hosted clock tests assert exact
offset/before/after and immutable evidence on repeat initialization. Added all
32 positions returning all-ones only AFTER the mask write; each rejects before
counter reads/enabling. Hosted32936 PASS827 cases. No production edits, private
builds or image changes. v28 remains current; no jobs active.

2026-09-28 NUC v27 startup7/id8086A701/period52083333: comparator mask
readback mismatch, not missing HPET. Main/private now check only required
IRQ/FSB enables0x4004 plus all-ones rejection instead of wholeword equality.
Adds retained comparator offset/before/after sysinfo details3..5 and Intel log.
Exact NUC differing bits still unknown. Hosted31385 PASS795 cases (including
unrelated-bit changes and each stuck enable). Native/package88190 PASS,
normal audit qmu590hs, four-CPU UEFI/logviewer ru2j17oa PASS. Published private
kernel/cubit_n95_clock_mask_v28.img plus plan; reset remains False.
SHA256789fddbcb4bd5ca5badaeec3499ba07002333eaefc21d17f3032a9814fde1805.
No jobs active. v27 preserved. Need v28 startup and comparator line if failing.

2026-09-28 PUBLICATION LIFETIME: main intel_gpu_ggtt_publish now takes limited
private Attempt; consumes before callbacks and latches Possibly_Published
before first write. No reset/copy/release interface. Success marks Complete;
repeat rejects preserving phase with no I/O. All27 hosted scenarios now check
same-address and changed-address retries (54 rejection checks). Native hardware
binding still absent. Hosted88356 PASS exit0; no new image/private sync/jobs.
v27 remains hardware checkpoint. This does not replace allocation ownership or
protect against a trusted caller manufacturing a fresh attempt.

2026-09-28 DESCRIPTOR NATIVE EVIDENCE: main/private Intel caller now consumes
Prepared, checks address/alignment/capacity/content invariants before continuing,
and logs only readiness and sizes (not addresses). Private run-live assertion
requires ready/335360/1048576. Native+fixture33794 PASS, audit h0j1i7px,
four-CPU UEFI ron3uccy delivered descriptor record via logstore to boot-logs.
This is real CuBit execution with RAM GPU MMIO, not hardware upload/auth.
Denied-descriptor expectation added but NOT run: old --intel-firmware-denied
expectation has no matching scope-withhold hook in current private devmgr.
Private profile restored without map-check; generic cubit_live_uefi.img remains
test-only fixture, DO NOT hand it out. Published v27 checksum verified unchanged.
No jobs active. Physical clock diagnosis still awaits user v27 feedback.

2026-09-28 FIRMWARE DESCRIPTOR: inspected Linux GGTT reservation/uc_fw mapping
sources; documented upload-region versus runtime GuC buffer ownership/cache
boundaries in docs/intel-gpu-reset-handoff.md. Main/private firmware_buffer now
retains a discriminated Prepared descriptor only after copy/pad/readback;
separate DMA/CPU addresses, capacity/content lengths. Native66545 PASS. No
native GGTT write integration or runtime test of the new getter yet. v27
unchanged; no jobs active. Still need physical clock diagnostics.

2026-09-28 HPET FAULT COVERAGE: main tests/boot-hpet/clock_tests now exercises
43 additional cases: bad revision/all-ones ID/32-bit-only counter, zero/excess
period, unreadable configuration/each32comparators/initial counter, invalid or
regressing progress, and invalid/pre-epoch runtime reads. Asserts stage, no
premature writes, disable attempts after failed progress, and no retry writes.
Hosted46057 PASS (699 cases total). Production code/image unchanged; v27 still
the NUC diagnostic checkpoint. No jobs active; awaiting hardware clock evidence.

2026-09-28 v27 READY: normal audit qcdmbif1 (8 bootstrap, no test fixture),
four-CPU UEFI USB/hub/noPS2/logviewer7_87wzwk PASS29946. Published private
kernel/cubit_n95_clock_diag_v27.img and plan; mapping-only, reset flag False.
SHA256320faf50e688ede7c1839f98752e0b06e24ecd16583ef72c07e57398966865a7.
Need NUC clock-startup/id/period line; cause remains unknown. No jobs active.

2026-09-28 CLOCK DESKTOP DIAGNOSTICS: own read-only sysinfo1402(detail0 startup,
1 HPET id, 2 period-fs), platform cached evidence, public-query admission in
syscall-ipc, runtime constant and Intel log publication. No addresses/write path.
Reviewed scoped main/private sync. Native5562+8804 PASS. QEMU Intel RAM fixture
fbvkjyxt42518 PASS startup12/id8086A201/period10000000 in boot-logs; HPET-off
00wpg32q7114 PASS startup0/id0/period0. No physical timing proof. Added explicit
Enable_Native_Reset=False to keep next hardware image diagnostic/mapping-only.
Fixture removed from private profile. Normal rebuild/package/QEMU session29946
active in private workspace; shared outputs untouched. v26 unchanged.

2026-09-28 NUC v26: user reports reset pages clock-unavailable. This gate is
before any022D grants and after inventory/domain probing; no GPU reset occurred.
Root cause not established. Added retained HPET startup stage diagnostics in
main kernel hpet_clock/platform_monotonic/boot_timer_setup, with stage/repeat
assertions in tests/boot-hpet/clock_tests. Hosted52699 PASS. Not yet synced or
built natively; no new image. Need expose detailed failure in desktop logs.
Prior native reset authorization build97173 completed PASS exit0. No jobs live.

2026-09-28 RESET AUTHORIZATION: private devmgr022E one-shot permit revalidates
frozen identity/D0/BAR/all six issued grants, consumes before reply. Main/private
driver requests permit after preparation/logging then executes native reset;
removed unused0230 push command (could arrive during startup log IPC). This is
trusted-driver sequencing, not register-level security once pages are granted.
Native build pending. No new image published; v26 stays mapping-only.

2026-09-28 RESET FAILURE AUDIT: native access-fault latch now still permits
validated preparation cancellations; no other writes proceed. Added composed
failure-engine diagnostic (cleanup takes precedence), tested all1152cases
PASS11303. IPC success now uses Boolean Last_Succeeded, not text comparison;
rejected repeats clear last-result success. Reviewed private sync; native6370
PASS exit0. No commands active.
Publishedv26 unchanged; no hardware reset issued.

2026-09-28 COMPOSED RESET MATRIX: expanded adln_reset_tests to1152 cases:
all8 media-fuse subsets, each/no stop failure, each/no prep failure, reset
failure and cleanup failure. Asserts selected-only MMIO, no reset after stop/
prep failure, complete selected cleanup after prep/reset and no writes on
reuse. Hosted92325 PASS. No commands active; native/published images unchanged.

2026-09-28 NATIVE RESET ADAPTER: own intel_gpu_native_reset singleton with
guarded reads/writes to approved aliases, retained all-domain forcewake,
typed microsecond clock. Main accepts canonical0230 only from saved boot
manager and only after full mapping. Devmgr does NOT issue it yet; published
v26 unchanged. Sources reviewed/synced privately (including newer inventory
and hosted-tested reset stages). Native compile17578 PASS exit0. No commands
active. No reset performed. Private staged driver newer than publishedv26.

2026-09-28 DRIVER RESET MAPPINGS: main/private Intel main requests022D for
six fixed pages, checks canonical replies, maps separate61200000 aliases;
whole set has30s budget, high-resolution availability + valid inventory +
successful domain probe prerequisites. Private devmgr waitReady returns busy
for canonical022D while collecting startup readiness. No reset register writes.
Native build12349 PASS. Normal(nonfixture) audit q48tij48 and four-CPU UEFI
boot g_w_qli7 PASS64867; HPET ready/Desktop/log replay. Publishing v26 reset
mapping-only image (no reset writes), v25 preserved. No NUC grant result yet.

2026-09-28 RESET PAGE GRANTS: new shared-source intel_gpu_reset_pages fixed
six-page/slot table and hosted coverage test. Private-only devmgr022D handler
revalidates frozen46D2 identity, D0, BAR before minting one indexed4KiB page;
no caller physical/size/slot choices, duplicate grants denied, no RAM fixture
grant path. This must be reconciled with the existing private devmgr bring-up
changes later; main devmgr intentionally not overwritten. No driver requests
these yet. Initial build45738 needed explicit source-list update; native
44375 PASS exit0. No commands active. Hosted table coverage8492 PASS. Published images
untouched. Private devmgr.gpr additionally owns the new source-list entry.

2026-09-28 COMPOSED ADLN RESET: own intel_gpu_adln_reset generic and
adln_reset.gpr/tests. Composes real engine stop/preparation/GT reset with
handoff, fixed register offsets; cancellation readback verifies request clear
(not global DMA quiescence). Hosted74075 PASS four reset/cleanup failure
combinations plus identity/reuse rejection. Preparation rejects unavailable
clock sentinel too. No commands active. Not bound natively or synced private;
MMIO ownership/grants and hardware validation still required.

2026-09-28 SHORT TIMING CONTRACT: audited HPET1.0a2.4.1 (two ticks/100us,
max100ns tick). Stop/reset now budget2us (3/52us observed thresholds), reject
unavailable all-ones reads. Hosted reset/stop tests PASS53006 after correcting
binary path in initial92969 invocation. No commands active. No native reset
binding, private code unchanged.
docs/kernel-monotonic-timing.md records compliance assumptions/derivation.

2026-09-28 NATIVE CLOCK EXECUTION: private laptop-usb temporarily includes
map-check fixture to launch Intel service on RAM-backed synthetic GPU. Runner
now requires HPET ready and Intel userspace syscall progress TRUE. Preparing
private build/QEMU4CPU regression. Present HPET4526/l8_e1ef7 PASS; absent
HPET74665/3bg82da6 PASS (explicit unavailable+FALSE progress, desktop still
boots). Initial audit85348 rejected fixture as designed; explicit test-only
CUBIT_MAP_CHECK_FIXTURE=1 audit mode now permits/requires fixture. Initial
absent launch45201 used obsolete QEMU option; corrected to q35,hpet=off.
No commands active. Removed map-check from private source catalog again,
but current cubit_live_uefi.img is STILL TEST-ONLY until normal rebuild.
Publishedv25 untouched. No NUC timing or native reset claim.

2026-09-28 MONOTONIC ABI: own syscall114 READ_MONOTONIC_MICROSECONDS,
kernel dispatch and runtime cubit-messages constant/cubit-monotonic typed
wrapper. Intel startup bounded progress probe added; reset still disabled.
Scoped matching edits private (no broad syscall/runtime sync). Kernel compiled
in31357; runtime+Intel consumer PASS66966 after enum-order/style fixes. No
commands active; existing GETTIME units/epoch unchanged. No published image
change. New syscall/consumer still needs native execution test (not claimed).

2026-09-28 NATIVE HPET BACKEND: own platform_monotonic.*, Time.Read_Monotonic
and Boot_Timer_Setup counter-only initialization after firmware takeover.
Maps second register page when required by ACPI 1KiB alignment. Existing
scheduler/ms GETTIME unchanged. Reviewed matching edits in private workspace;
native build95396 PASS. Standalone strict compile61739 rejected a restriction
warning (not native build flags). Private UEFI image/QEMU regression13356 PASS,
logs3fw63ri_ show HPET counter-only ready, desktop and boot log viewer complete;
image audit ho2bo2vy. No commands active;
no published image changed. Physical error contract and userspace ABI pending.

2026-09-27 COMMON MINIMUM DELAY: own new kernel/src/monotonic_wait.* and
tests/boot-hpet/wait.gpr. Bounded, backend-independent minimum-delay policy
with explicit elapsed-time overstatement budget, typed failure and overflow
rejection. No changes to networking-owned UTC service or shared native images.
Hosted session66974 PASS exit0 (1,100 boundary pairs plus failures). No commands
active. docs/kernel-monotonic-timing.md records integration boundaries and
remaining hardware error-budget obligations. No native timing ABI yet.

2026-09-27 HPET STARTUP: own new generic hpet_clock and boot-hpet/clock.gpr.
One-shot startup disables global/legacy mode, masks every comparator IRQ/FSB
with readbacks, enables counter only, requires bounded forward progress.
No kernel boot binding yet. Hosted session41360 PASS exit0; no commands active.
User asks about general cross-platform timing architecture before proceeding.
Publishedv25 unchanged.

2026-09-27 HPET COUNTER POLICY: own new kernel/src/hpet_counter pure helper
and tests/boot-hpet/counter.gpr.64bit-counter admission, all-comparator interrupt
andFSB masking, counter-only config(no legacy routing), split FS->us conversion.
Hosted33002 PASS; initialproof54781 PASS. Explicit no-modular-wrap assertions
added: rerun50380 PASS including product/sum no-wrap obligations. No commands
active. No boot enabling, sysinfo/syscall changes or native claims.
Existing HPET quiescence retained; publishedv25 unchanged.

2026-09-27 TIMEBASE AUDIT: kernel GETTIME returns BSP msTicks, TSC rate is
calibrated but cross-CPU agreement not validated; no TSC_AUX setup found.
Fixed minimum-delay quantization in hosted stop/reset helpers: floor-rounded
timestamps require >1/>50us differences, not >=1/>=50. Session40618 tests.
Added exact-boundary regressions. No kernel/ABI changes, native reset disabled.
Both suites PASS40618; exact-boundary rerun12006 completed successfully.
No commands active; publishedv25 unchanged.

2026-09-27 ENGINE REGISTER POLICY: extended pure ADLN inventory with verified
engine bases/pending registers and narrowly allowed stop/prefetch/prepare/cancel
values, no restart or GDRST writes. Hosted/proof session98564 PASS exit0
(initialization/termination checks; no silicon-model proof). No commands active. Not synced into
private native driver yet; v25 unchanged. GETTIME currently returns msTicks;
native microsecond timebase is still required for helper callback binding.

2026-09-27 HANDOFF COORDINATOR: own intel_gpu_handoff plus handoff.gpr,
6912 selection/stage-failure combinations session69124 PASS exit0. No commands
active. Validates identity/fuse
before callbacks, holds power, stops all then prepares all before reset/settle,
cancels every selected preparation on exit from prep/reset, and retains power
and quarantine on failure. No native binding. Publishedv25 unchanged.

2026-09-27 GT RESET HANDSHAKE: own intel_gpu_gt_reset generic and hosted
gt_reset.gpr. Two acknowledged full-domain writes (bit0), each bounded2000us,
then >=50us settle. Latches quarantine before callbacks, never repeats failed
attempt; no native adapter or grant. Hosted session2157 PASS exit0.
No commands active. v25 unchanged.

2026-09-27 ENGINE STOP: own intel_gpu_engine_stop generic and hosted
engine_stop.gpr/tests. Stop/prefetch masks, idle/posting read, pending masked
MI_FORCE_WAKE extraction, power ack and two >=1us settling intervals. No
native adapter. Session97245 hosted tests PASS exit0; no commands active.
Publishedv25 unchanged. Timeout
rejects rather than adopting Linux head==tail fallback. Callbacks/timebase,
submission exclusion and all-domain ownership remain integration obligations.

2026-09-27 RESET PREPARATION: own new intel_gpu_reset_prepare generic plus
reset_prepare.gpr hosted tests, session17237 PASS exit0; no commands active.
Implements separate ready and
catastrophic-clear branches with700us deadline/poll bound; cancellation is
an issued masked write only, not quiescence proof. No native adapter enabled.
Upstream audit also found Wa_22011802037: no MI_FORCE_WAKE may execute during
engine reset on this generation. Must implement/validate stop path first.
Publishedv25 unchanged and NUC feedback pending.

2026-09-27 V25 PUBLISHED: session10957 completed0, normal image audit
xhaqkni_ and QEMU4CPU/4G UEFI boot-log viewer zf_0kw37 PASS. Serial confirms
noPS2, hub keyboard/mouse endpoints, desktop and lossless startup replay.
No Intel GPU emulation in that boot; additional domains need NUC validation.
Published private kernel/cubit_n95_intel_domains_v25.img (+plan), SHA256
0bde7e455af702db7fc90a3587b05a74b50959fd94e18239108d39d240193047.
Expected domains-release-ready, media fuse, inventory-valid=TRUE and required
domain records. No reset/PTE publication. No active commands; v24 preserved.

2026-09-27 V25 PACKAGING: private build/test session10957 active, normal
catalog (no map-check), devmgr+intel-gpu rebuild, UEFI image and 4CPU/4G
USB-flash/hub/no-PS2/log-viewer QEMU regression. No shared lock/output touched.
Will publish immutable v25 only after success. This QEMU machine has no Intel
GPU; actual multi-domain handshakes require NUC validation.

2026-09-27 COMBINED FORCEWAKE: new intel_gpu_adln_forcewake owns five
fixed per-domain leases plus coordinator, decodes device/fuses internally.
Combined mocked MMIO tests288 selections/acquire/release failure cases PASS
session27425. Native main now binds selected domains through existing A000
alias, guarded offsets/values; package sources copied privately. Native build
session9674 PASS exit0. No commands active. Publishedv24 untouched; no new
image/NUC result yet.

2026-09-27 FORCEWAKE REUSE: parameterized fixed request/ack register pairs,
renamed Acquire_GT/Release_GT to Acquire/Release (no compatibility aliases).
Reviewed same edits in private driver; native still GT only. Existing hosted
failure suite now runs for all five ADLN register pairs via forcewake.gpr.
No changes to devmgr grants or additional live domain writes.
Hosted suite83463 PASS for all five pairs; native build31505 PASS.
Report-only rerun67599 tracks per-domain labels. Publishedv24 unchanged.

2026-09-27 NATIVE FUSE CAPTURE: own Intel driver main plus private equivalent.
Reads0x9140 twice while GT lease held, decodes authenticated bootstrap PCI ID
only after matching reads and successful release. Logs raw fuse, validity and
required domains; no new MMIO writes. Private native build session30627 PASS
(exit0). No commands active. This is compilation, not a boot/hardware result.
Only inventory sources and reviewed main patch copied, no broad kernel/devmgr
sync. Publishedv24 untouched; no new image yet.

2026-09-27 ADLN INVENTORY: own new intel_gpu_adln_inventory and separate
hosted inventory.gpr/tests. Verified upstream v6.16 ADLN->ADLP engine mask,
0x9140 disable-bit semantics and per-engine media forcewake. Decoder admits
8086:46d2 only. Hosted session85802 PASS; SPARK session75600 PASS
(initialization/termination checks; not proof of the silicon register model).
No commands active. No native register writes added;
actual NUC fuse capture and binding remain pending. Publishedv24 unchanged.

2026-09-27 DOMAIN LEASE: own intel_gpu_domain_lease generic and hosted
domain_lease_tests. Testing every subset and acquisition/release failure,
reverse cleanup and uncertain-domain retention; no native MMIO adapter.
Hosted Nix build/test session18389 completed exit0: all128 combinations pass,
including reverse cleanup and exact uncertain-domain checks. No commands active.
User confirmed v24 on NUC: GGTT8388608 first7C800001 present532141
scanned1048576, firmware prepared-retained (NOT GPU-published). This replaces
the earlier pending hardware-validation status for inspection/buffer only.
Published v24 unchanged. No reset, PTE write or 3D claim.

2026-09-27 RESET AUDIT: read primary Linuxv6.16 intel_reset.c. Current GT-only
forcewake is insufficient for reset envelope: all-domain lease, engine
inventory/prepare/cancel, repeated domain reset and settle requirements.
Recorded docs/intel-gpu-reset-handoff.md; native publication remains disabled.
Next implement validated engine/domain inventory and multi-domain lease before
reset adapter. No commands active, publishedv24 unchanged. This is a source
audit/design constraint, not new hardware validation or a reset implementation.

2026-09-27 PUBLICATION TEST PASS: session9489 and boundary rerun25913
completed0. Hosted publication fault injection and GGTT encoding/geometry
tests pass. No native adapters or GPU writes; not a proof of hardware ordering.
No active commands. Publishedv24 remains the hardware test image.

2026-09-27 GGTT PUBLICATION TRANSACTION: owned new generic
intel_gpu_ggtt_publish validates whole retained pages/bounded range, preflights
all PTEs zero before first write, prepares backing, marks quarantine before
first MMIO callback, readbacks then invalidates. External exclusive ownership
and scanout exclusion REQUIRED; zero PTE is not ownership. Hosted fault tests
session9489; no native adapter/MMIO writes enabled, publishedv24 unchanged.

2026-09-27 CEILING TEST PASS: session77864 completed0, native4G ya3c1fm0.
Impossible ceilings rejected with budget preserved, eight retained8MiB ranges
below4GiB aligned/disjoint, deferred loan exit and ordinary constrained mode
after quota exhaustion PASS. Removed private test invocation and membership;
staged devmgr/genericimage remain test build until next compile/package. No
active commands. Immutable publishedv24 unchanged. Fragmented/exhausted search
and races not covered. Next GPU VA reservation/publication work can proceed.

2026-09-27 CEILING REGRESSION: extending destructive native retained test
with impossible ceilings1/4095/8MiB-1, eight8MiB below4GiB blocks, alignment
and complete-range checks, ordinary constrained path after quota exhausted.
Private fixture invokes test temporarily; runner --dma-retention-fixture
checks deferred loan return and PASS/FAIL then exits. Publishedv24 untouched.

2026-09-27 v24 PUBLISHED: normal audit iwy3xqvk PASS; copied immutable
private kernel/cubit_n95_intel_buffer_v24.img (+plan), SHA256
f228ac404431bfc9e6d58e52667c75d96639ced189d11567b7ac33fe2bd71fec.
Dedicated wulobznq check-native PASS. NUC physical validation pending.
No active commands. No GGTT writes or GPU submission. Next DMA ceiling
boundary/exhaustion tests and GPU address reservation/publication machinery.

2026-09-27 NUC-SIZE PASS: session58159 completed0, wulobznq native4G QEMU
passes8MiB table1048576entry scan with4 sentinels, retained firmware-buffer
preparation, logviewer/Desktop. Fixture membership removed. Normal image
build next; no GPU PTE writes, upload or submission enabled.

2026-09-27 NUC-SIZE GGTT: driver maps2MiB disjoint read-only chunks instead
of exceeding1024-page syscall cap. Private fixture now8MiB GGC=C0 with four
sentinels including first/final and adjacent entries across2MiB boundary.
Main/private Intel main edited; private supervisor/test edits only. Build next.

2026-09-27 CONSTRAINED DMA4G PASS: session26583 completed0, QEMU v0ee3vry;
new physical2097152000 fits below4GiB, full firmware copy/zero-tail/readback
and logviewer/Desktop pass in original4G configuration. Broader exhaustion,
free/reallocation and arithmetic proof still needed. Private fixture membership
removed; generated generic image still fixture dsl1tz2k until next rebuild.
No active commands. Publishedv23 unchanged. Fix8MiB GGTT mapping chunks next.

2026-09-27 CEILING BUILD PASS: session78974 completed0; main/private scoped
allocator/syscall changes compiled. Native4G firmware fixture next. Found
separate integration limit during review: MAP_DEVICE caps1024pages, so full
8MiB NUC GGTT map will fail although2MiB fixture passes. Must use bounded
chunk mappings before handing off a new NUC image; do not claim8MiB tested.

2026-09-27 DMA CEILING: own kernel buddyallocator.ads/adb plus syscall-ipc
and syscall dispatch for explicit exclusive physical ceiling arg4 (zero keeps
ordinary allocation). Slow-path searches existing free lists under lock,
selects fitting left prefix, reuses unlink/split/commit; normal hot path stays
unchanged. No speculative allocate/reject consuming retained quota. Build and
native4G regression pending. Preserve networking procmgr notification.

2026-09-27 NORMAL BUFFER IMAGE: session45684 completed0, audit ld7c0cdl
PASS, SHA c2beb28f2d2ac9512e086251f09473989380da25f25cfb3ab7be0c21097cc3ba.
No active commands. Publishedv23 unchanged. Next: DMA address constraints,
then GGTT reservation/publication, not another claim of accelerated rendering.

2026-09-27 BUFFER CPU PATH PASS:2G QEMU c53b2ly_ session59376 completed0;
retained buffer2043674624, full1MiB copy/zero-tail/readback PASS, viewer/Desktop
PASS.4G address-policy rejection remains unresolved; do not claim general
DMA placement. Fixture membership removed, normal rebuild next. Native
buffer never GPU-published; no GPU execution/authentication claimed.

2026-09-27 BUFFER ADDRESS LIMIT: private build PASS.4G fixture050ywr9x
allocated6343884800 (>4GiB), correctly denied current admission. No policy
relaxation. Added private --memory2G option to exercise copy path separately;
session59376 running. Need bounded allocation placement before NUC reliance.
Private map-check fixture still enabled; publishedv23 unchanged.

2026-09-27 FIRMWARE BUFFER: implementing fixed supervisor022C retained1MiB
allocation at61000000 for authenticated Intel child, below4GiB policy,
one-shot/idempotent response. Driver copies admitted blob, zeroes tail, reads
back all bytes. No GPU VA publication/cache flush/upload. Private quota test
invocation disabled for GPU test because it consumes all64MiB; prior result
preserved. Private fixture temporarily enabled; build/test next.

2026-09-27 FULL GGTT NORMAL IMAGE: session1813 completed0, audit azxxov4u
PASS, SHA e573cba8f0042a67d3689e6c0ebc03ff97fe33735ff755298de60a2392e3e995.
Dedicated check-native cyqccokg PASS. No active commands. Candidate selection,
display exclusion/ownership, MMIO PTE publication and invalidation remain.

2026-09-27 FULL GGTT PASS: private native build+image l8e6qs9j passed.
First cb9qbu47 runner stopped on stale viewer expectation; corrected runner
cyqccokg session87111 completed0. First/last present PTEs counted2 across
262144 entries, retained DMA/Intel RAM forcewake/logviewer/Desktop PASS.
Fixture membership removed; normal rebuild next. Main check-native updated
to full-table expectations. Publishedv23 unchanged; no MMIO writes enabled.

2026-09-27 FULL GGTT READ SCAN: owned Intel main now requires exact table
extent in022B reply and maps entire table READ only. Private devmgr grants
GGC-derived2/4/8MiB, fixture uses2MiB with first/last PTE sentinels. No writes
or ownership inferred. Private fixture enabled temporarily; build/test next.

2026-09-27 GGTT ENCODER PASS: session61529 completed0. Hosted GGTT tests
cover all1,048,575 nonzero aligned pages below4GiB plus unaligned/boundary
rejections. GNATprove GGTT unit level2 checks-as-errors passes contracts,
flow/termination. No native calls or MMIO writes; not GPU isolation proof.
Source/reference and CPU-vs-GPU read-only limitation documented. No active
commands; private image and publishedv23 unchanged.

2026-09-27 GGTT ENCODING: editing owned Intel_GPU_GGTT and ggtt_tests only.
Conservative below4GiB system-memory PTE encoder, rejects null/unaligned/out
of policy DMA addresses; no MMIO write or native allocation enabled. Hosted
Nix tests/proof next. Networking ownership note unchanged.

2026-09-27 NORMAL IMAGE RESTORED: session32100 completed0, image audit
1txc0f5h PASS, no map-check bootstrap membership. SHA256
47a56c4bdd22061ba27011324d5340652ceb17c48a051237820c2fe5ec17a923.
No active commands. Publishedv23 preserved.

2026-09-27 DEFERRED DMA PASS: session37233 completed0, native QEMU xiopwdp2.
Fixed fixture reply (events have NO_PROCESS sender; capSubmit endpoint5 gives
authenticated caller). Owner26 killed with active acquisition; temporary
child27 allocated before return, owner26 reusable afterward. Retained ranges,
64MiB cap, failure refunds, ordinary mode PASS; Intel RAM/log/Desktop PASS.
Private map-check membership restored off. Rebuilding normal image next;
publishedv23 unchanged. No GPU DMA/submission enabled.

2026-09-27 DEFERRED FIXTURE FIRST RUN: devmgr/map-check compile PASS
session30097. Image pko553zb audit PASS; QEMU yalkw_zx session71512 still
running at last poll, serial contains `dma-retention: FAIL borrow` before
teardown. Added private supervisor diagnostics distinguishing invalid reply,
acquire rejection, sentinel mismatch, reply timeout; not rebuilt yet. Do not
claim deferred path passed. Private catalog currently includes map-check;
restore after test. Publishedv23 untouched.

2026-09-27 DEFERRED DMA TEST: extending tests/intel-gpu/native generic with
Borrow callback and first-owner exit while a CPU acquisition is held. Private
map-check child publishes a generation-checked grant with sentinel; supervisor
checks owner identity, acquires read-only, kills owner, checks PID nonreuse,
returns acquisition, then continues retained-range/quota checks. Private build
session30097; no shared builds or production GPU allocation enabled. Runtime
result pending; publishedv23 unchanged.

2026-09-27 RETAINED DMA NATIVE PASS: x0741b__ passes extracted generic test
in tests/intel-gpu/native (compiled by private devmgr source list). PID26 reused
across exits, no retained-range overlap,64MiB budget persists, failure refund
and ordinary mode PASS. Intel logs/Desktop PASS. Removed fixture membership;
normal image1f6kdk5u audit PASS, SHA256
61be592f79c9a4575ffc39351ad89377bd91039c285ee1b03a76773f3be88c0e.
No active commands. Publishedv23 unchanged. Deferred
CPU-loan test still pending; no GPU allocations/submission enabled.

2026-09-27 RETAINED DMA NATIVE TEST ACTIVE: private devmgr map-check-gated
fixture spawns/reaps eight owners with8MiB retained allocations, checks ranges
never overlap, failed reservations refund, unknown modes/orders reject, ninth
retained allocation hits64MiB boot cap, ordinary allocation still works and
does not overlap retained backing. QEMU session29003; no GPU DMA enabled.
Deferred CPU-loan case remains to be tested separately.

2026-09-27 RETAINED DMA COMPILE PASS: private cubit_kernel build session70003
completed0. Opt-in arg3=1 not requested by apps yet. No new ISO. Native exit,
deferred-loan and budget regression still required; not a DMA-safety proof.
No active commands. Preserve tellManager in process.adb (not edited here).

2026-09-27 KERNEL HANDOFF ACCEPTED: preserving procmgr exit notification.
Editing process.ads/process-ipc.adb/syscall-ipc.ads/adb/syscall.adb for opt-in
ALLOC_DMA arg3=1 retained-until-reboot mode, globally bounded64MiB. Old mode0
unchanged. Successful retained blocks skip buddy free at both cleanup callers;
owner tags still cleared after CPU loan retirement. No device use enabled yet.
Main/private scoped edits applied; private kernel build next. Needs native
exit/reallocation and quota regression before enabling GPU allocation.

2026-09-27 NUC v23 GGTT EVIDENCE: photo confirms table8388608 bytes,
present512 in first table page, forcewake release-ready, file LOADED335360.
Not VRAM size or whole-table occupancy; do not overwrite sampled mappings.
No new commands/builds. Native DMA lifetime integration still pending.

2026-09-27 MESA HOST LINK PASS: session52525 exited0, libvulkan_intel.so built
SHA256 bcea1323786333e0a1e6382a334e2462bdbc692f370966578364e72825a17565.
readelf/nm confirm actual DRM/syncobj/pthread dependencies, including modeset
imports despite platforms=[]. Documented native WSI removal obligation. No
CuBit Mesa execution, no GPU submission. No build/test processes remain.

2026-09-27 SHORT LOGS PASS: it_y9tya +check-native PASS all four visible
records. Fixture removed, normal mla2dfrr image audit PASS. Preserving
cubit_n95_intel_logs_v23.img SHA256
c9506f3e9987933082faae51491f2b2cbb12bb5e76f8d6a8d491f8817f31753f.
Mesa host session52525 still active (~428/1023); workspace TMPDIR avoids quota.

2026-09-27 SHORT LOGS / BUILD RESUMED: Intel main now publishes four short
records using one granted publisher; ambiguous publication stops later records
without reusing its loan. Native build PASS; private fixture build/QEMU active
(session61280). Mesa host resumed with workspace TMPDIR (session52525), now
compiling NIR, no /tmp quota issue so far. No shared kernel edits.

2026-09-27 NUC PHOTO: forcewake=release-ready, file=LOADED confirmed after
v22 handoff. Raw power values00005405/0000FC0F; Desktop visible. GGTT text
off right edge, not confirmed. Need shorter log records next build.
Mesa host session30388 TERMINAL failed: /tmp/nix-shell.IHV9TO compiler temp
disk quota exceeded at generated format table; no compiler process remains.
Next retry use dedicated workspace TMPDIR, do not delete others' temporary data.

2026-09-27 MESA HOST BUILD ACTIVE: minimal ANV configuration PASS. Running
ninja -C tests/mesa-anv/build-host -j2 src/intel/vulkan/libvulkan_intel.so
inside owned host-shell.nix (exec session30388). 1082 build steps; only Linux
baseline, not native CuBit or hardware evidence. No shared native build lock
held/needed. Avoid performance measurements while this compiler is active.

2026-09-27 MESA HOST CONFIGURE: new owned tests/mesa-anv/host-shell.nix and
configure-host.sh, pinned checkout nixpkgs, Mesa26.2.3 existing source pin.
Running minimal Linux ANV Meson configure in tests/mesa-anv/build-host;
not a CuBit build, no shared kernel/build outputs. v22 unchanged.

2026-09-27 HARDWARE HANDOFF v22: preserved normal audited image as private
kernel/cubit_n95_intel_ggtt_v22.img (+plan), SHA256
1622fee74c8fb2486e300efa7aac1cd864511bc878a33d9201c40ecc5760b27c.
Includes scoped firmware loading + read-only GGTT sample, no GPU submission.
QEMU normal hub input wv268224 and synthetic Intel vyfck9c9 passed previously.
NUC evidence needed for forcewake, GGC, GGTT, firmware file status. v21 retained.
Allocation review additionally confirms adding pins alone would trip the
allocator's higher-order free check; both teardown paths must be coordinated.

2026-09-27 DMA MODEL PASS: owned Intel_GPU_DMA_Lifetime implements total,
fail-closed retirement transitions. Hosted exhaustive length-eight sequences
and GNATprove level2 contracts PASS. Documented native integration obligations
in docs/gpu-dma-retirement.md; model not wired into kernel/driver allocation.
No active commands. No new boot image. Shared kernel coordination pending.

2026-09-27 DMA RETIREMENT COORDINATION REQUEST: propose a device-lifetime pin
independent of CPU grants, retained across both releaseDMAAllocations callers
and address-space teardown. Initial abandoned published buffers quarantine
until reboot with a bounded per-device restart budget; later trusted reset/
IOMMU broker releases them. Details docs/gpu-dma-retirement.md. Need ACK/window
before shared process.ads/process-ipc.adb/syscall-ipc.adb changes; not editing
those files now. Own Intel transition model/test/proof in progress. No DMA
submission enabled, no firmware upload. Previous turn made native progress.

2026-09-27 GGTT NATIVE PASS: fixture vyfck9c9 and native checker PASS,
first=0000000012345001 present=1, firmware read and forcewake cleanup intact.
Fixture removed; normal image pyrtkaw3 audits PASS, SHA256
1622fee74c8fb2486e300efa7aac1cd864511bc878a33d9201c40ecc5760b27c.
Normal-image hub input regression wv268224 PASS: keyboard transitions, mouse
motion/buttons reach Desktop. No active commands. Published v21 unchanged.

2026-09-27 GGTT INSPECTION TEST: owned Intel driver now requests one read-only
GGTT page via 022B, samples first PTE and present count. Private devmgr grants
only READ after identity/BAR/D0/GGC validation; RAM fixture supplies a separate
page with one sentinel PTE. Native build passed. Running private fixture image
and QEMU; no shared devmgr/kernel changes. No GPU table writes or submission.

2026-09-27 V3 NATIVE PASS: QEMU fixture _rmbky0s and native checker pass GGC
0x40/table2097152, firmware admission, forcewake cleanup, log viewer/Desktop.
Catalog/audit restored; normal image4s_qr7e8 audit PASS, SHA256
e60fd7393780954775a47f850b52d63ef7d8c2e91e4424a68ae73ced63ea9f90.
No commands; publishedv21 unchanged. GGTT inspection not yet implemented.
Kernel coordination FYI: before GPU DMA, need lifecycle semantics beyond CPU
memory loans. Existing ALLOC_DMA maps PG_USERDATA and releaseDMAAllocations
is used on process teardown. GPU quiescence/IOMMU teardown must gate recycling;
I am NOT editing shared process/IPC code. Please coordinate that future work.

2026-09-27 GGC HANDOFF: own bootstrap nowv3, w3 low16version,next16GGC,
upper32zero; undeployedv2 rejected. Private devmgr reads GPU PCI0x50, passes
rawGGC; RAM fixture supplies0x40. Driver logs decoded table bytes and appends
ggtt to snapshot. Table_Size tests exhaust65536 inputs, hosted/bootstrap tests
and focused SPARK PASS; private native devmgr+intel-gpu builds PASS. No new
image/boot test yet. No page-table grant/write. v21 unchanged, no commands.

2026-09-27 GGTT GEOMETRY: own intel_gpu_ggtt.ads/adb +ggtt_tests, hosted tests
and focused SPARK level2 PASS. Pure page-rounded table window planner only;
no MMIO/table writes or ownership assertion. Native metadata-gated firmware
fixture cn9_24i5 PASS (load, forcewake cleanup, boot viewer). Fixture restored,
normal image cl20t9a9 audit PASS, SHA256
338f947308f346b5ccf43d781ddb8044daed1e032da7093bab44d34e117502fc.
v21 unchanged; no active commands. Next actual GGTT geometry discovery and
ownership/reservation design before changing firmware-owned mappings.

2026-09-27 METADATA ADMISSION: own Intel_GPU_Firmware.Matches_Selected_ADLN_GuC
checks selected70.49.4 CSS fields and exact layout/size. Host real blob +352
single-bit mutations PASS; GNATprove level2 focused unit PASS, accepted=>valid
layout. Native file adapter gates returned storage on metadata; native compile
PASS. No new image/native boot test in this step. Not authenticity/fullABI.
Main/private own sources synced; no shared existing-file edits. No commands.

2026-09-27 FIRMWARE DENIAL TEST PASSED: private RAM fixture grants FS
endpoint but deliberately withholds path scope. Native FS denied correctly;
first run rys9unqw exposed diagnostic decoder expecting zero instead of open
failure's all-ones handle sentinel. Adapter now reports explicit access-denied.
Rerun ke_dcinq PASS: explicit access-denied bytes0/code0, log viewer +Desktop,
forcewake cleanup. Catalog/audit and Grant_Intel_Firmware call restored.
Normal build/audit h5pizeul PASS, SHA256
7596d8f836bb65940adcfdb7837093cb96ee1a37811a55f59f202227cb966d04.
No shared devmgr/kernel edits, no active commands. v21 unchanged.

2026-09-27 NATIVE FIRMWARE LOAD PASS: private devmgr installs Read_Objects
scope firmware/intel/tgl_guc_70.bin before minting child FS endpoint slot6.
Own driver main now invokes native loader before logging. Explicit status
names replace enum Image (minimal runtime emitted numeric0). Native compile
PASS; QEMU RAM fixture5bgscj9s PASS: scope installed, LOADED335360/code334976,
forcewake cleanup physical write, logstore/viewer, Desktop. check-native PASS.
Earlier vg16y1as successfully loaded but waited for wrong enum text; stopped
its exact runner gracefully. Fixture catalog/audit restored and normal private
image rebuilt/audited u06c7bad, SHA256
8805b48c92eb5a4d5e5fc434159fe36636368bad49b9a83f471446eeba469dfc.
v21 unchanged. No hardware/upload/authentication claim; native denial tests
still needed. No active commands. Shared devmgr handoff still pending ACK.

2026-09-27 NATIVE FIRMWARE ADAPTER: new own intel_gpu_firmware_file.ads/adb
in main and private workspace, plus private copies of reader/CSS. One-shot
async FS adapter, total30s budget, process-lived4KiB scratch grant, private
1MiB sbrk buffer, validates completion token/tag/count, positioned reads,
close before publishing bytes. Timeout retains storage and prevents retries.
Not wired into main nor granted an FS endpoint yet; no image change. Native
compile PASS (freestanding runtime); next scope grant + RAM-fixture integration.
No commands active. Compile success is not native I/O test evidence.

2026-09-27 BOUNDED FIRMWARE READER: own new intel_gpu_firmware_reader.ads/adb,
caller storage, 1MiB budget, 4KiB read chunks, short-read/EOF/reply validation.
Nix hosted firmware_reader_tests PASS (including failure after progress and
Natural'Last array bounds), full pinned335360-byte file via same generic PASS,
existing Intel admission tests PASS. Not native IPC or SPARK-proof evidence.
No shared source/build edits, no active commands. Native FS adapter/scope next.

2026-09-27 FIRMWARE PATH CORRECTION: native ISO9660.Find starts at Apps,
not the ISO root. Private catalog now puts the blob at
apps/firmware/intel/tgl_guc_70.bin; native lookup will be
firmware/intel/tgl_guc_70.bin. License remains at ISO licenses/Intel-GPU.txt.
Private auditor follows apps/ too and verifies both hashes. Rebuild PASS,
generic image SHA256 f1bbc6098d307b77c12d049b617cef8caf0cdade848d58851bf1cc9f47efd994.
QEMU UEFI four-CPU USB-flash/hub boot-log regression prgfja19 PASS. No shared
FS namespace change, loader, firmware upload, or new published hardware image.
No active commands.

2026-09-27 FIRMWARE PACKAGING: private artifacts.ccl/laptop-usb.ccl select
supplied intel-guc +intel-firmware-license; private build-live.sh resolves Nix
pins in tests/intel-gpu/firmware-source.nix. Separate optical firmware/intel/
tgl_guc_70.bin and licenses/Intel-GPU.txt. ISO checker verifies both exact SHA256.
Private build/audit PASS jj5jbjs1, generic img SHA256
1d399ca71afd2c5f61555756353937ca8a410bfbd8ccffbb131cfcb01ea8d006.
Not boot-tested/newly published; v21 stays unchanged. No loader/upload yet.
Shared image definitions untouched pending coordination. No active commands.

2026-09-27 V21 NORMAL IMAGE: fixture removed; image audit8bootstrap/21payload
PASS. Normal UEFI4CPU USBflash hub input8fqcbknb PASS; viewer4ostyg64 PASS.
Published private kernel/cubit_n95_intel_forcewake_v21.img +plan.json, SHA256
e7505b4db21cfd025f5363b60299dd6b9ce4b5a9acb37fc6691d5c1813a7765e.
Physical next: photograph forcewake=release-ready (or exact error) in viewer,
confirm input/Desktop. No Intel MMIO success yet, no firmware upload/reset.
Normal generic private image restored too. Shared integration ACK pending.
No active commands. v20 preserved.

2026-09-27 STARTUP RACE FOUND: granted RAM test _n_n2mvf showed old waitReady
consumed Intel request and returned NULL before service loop. Earlier denied
test i5n05jfp therefore did NOT prove policy denial. Private waitReady now
checks expected sender and returns F002 busy for authenticated early022A;
client retries only canonical busy within30s. Native rebuild PASS, replacement
QEMU run i4ua5qpy PASS: RAM grant/map/native timeout; QMP physical read verifies
0x00010000 cleanup. Native checker + boot-log viewer PASS. Catalog/audit
restored; generic image TEST-ONLY until rebuild, v20 unchanged. No commands.

2026-09-27 NATIVE FORCEWAKE CLIENT: main + private copy now request022A async,
30s timeout, canonical completion validation, map only granted4KiB alias and
instantiate real volatile access/GETTIME/SLEEP adapter. Native build PASS.
QEMU RAM fixture i5n05jfp correctly gets grant-denied; authorized logstore
delivery and desktop survival PASS, boot-log viewer PASS. No Intel hardware
handshake evidence yet. Fixture catalog/audit restored, but generic private
cubit_live_uefi.img is TEST-ONLY until rebuilt; versioned v20 unchanged.
No commands active. Next: granted-path fixture/hardware release preparation.

2026-09-27 FORCEWAKE GRANT: private devmgr canonical022A request checks tracked
Intel PID +4947 tag +zero payload, frozen BDF, fresh D0/identity/BAR. Grants
only BAR+0xA000 one4KiB page RW in slot5, once per claim. Nix private devmgr
compile/link PASS. Native client not wired yet; no image/boot/proof claim.
No commands active; main devmgr untouched and v20 unchanged.

2026-09-27 STATIC INTEL CLAIM: private devmgr now freezes PCI config writes to
the selected Intel BDF before child resume. Both 16/32-bit data writers reject
that BDF; no release on child exit, no general suspend implementation. Private
Nix `make -C kernel devmgr` PASS; not boot-tested or proved. No new ISO.
Shared devmgr remains untouched awaiting existing coordination ACK. Native
driver remains read-only. Next controlled per-page forcewake authority/handoff.
No commands active; v20 unchanged.

2026-09-27 FORCEWAKE OWNERSHIP: limited-private per-domain Lease added to own
generic; Idle/Held/Faulted transitions reject nested/unmatched operations and
fault retries before callbacks. Callback-exception latch covered by hosted
tests. Nix tests PASS. No native writes: v2 D0 snapshot is not runtime power
ownership. No commands active; v20 unchanged. No shared build edits.

2026-09-27 FORCEWAKE DEADLINES: own generic now accepts monotonic clock callback,
one default50ms elapsed budget across acquisition phases, pre/post-read checks,
clock regression/wrap rejection and sample bound for stalled clocks. Expanded
Nix-hosted tests PASS. No native enablement/proof claim; no commands active.

2026-09-27 FORCEWAKE: own new intel_gpu_forcewake.ads/adb generic adapter and
tests/intel-gpu/forcewake_tests.adb. Shared lock busy; no shared build definition
edits. Isolated Nix-hosted gnatmake test uses forcewake-build output directory.
GT request/ack sequence only, fail closed, no native write grant or upload.
Nix-hosted scripted MMIO tests PASS (explicit host.adc, invoke gnatmake from
test directory to avoid kernel restrictions). No proof claim. No active commands.

2026-09-27 WOPCM admission: own intel_gpu_firmware files and firmware_tests
extended with ADLN protected-memory containment/alignment checks. Hosted Nix
tests pass; focused level-2 proof PASS for explicit non-overlap postcondition.
No native/shared build changes, register writes or new image. v20 unchanged.
No active commands. Next: forcewake/reset and GGTT upload prerequisites.

2026-09-27 REAL GUC FIXTURE: pinned linux-firmware20250917 TGL GuC +LICENSE.i915
in tests/intel-gpu/firmware-source.nix (Nix store only, no ISO payload). Hosted
Ada firmware_file uses same CSS decoder and succeeds:335360 total,334976 code,
335104 signature offset,256 signature bytes. No authentication/upload claim.
v20 unchanged; native integration still awaiting physical snapshot. No commands.

2026-09-27 GUC LAYOUT: verified Linuxv6.12 ADLN uses ADLS/TGL GuC selection
(no HWConfig), not ADLP binary. Added pure Intel_GPU_Firmware CSS layout parser;
Nix tests and level2 containment/rejection proof PASS. No firmware fetched,
bundled/uploaded and no native changes; v20 remains unchanged. Firmware ABI,
provenance and WOPCM/upload gates outstanding. No active commands.

2026-09-27 MESA AUDIT: fetched official Mesa26.2.3 source via Nix, pinned NAR
in tests/mesa-anv/source.nix; tools/audit-mesa-anv.py ran successfully in Nix.
New docs/mesa-anv-port-audit.md records actual KMD callbacks plus discovery,
DRM-sync, threading, WSI/build dependencies outside that table. Source inspection
only, no Mesa build/native port. v20 unchanged awaiting physical snapshot.
No active commands or shared build/source edits.

2026-09-27 V20 READY: normal private image rebuilt without map-check; audited
8 bootstrap/21 payload files. oayv6642 hub HID PASS; _y59qdzs boot viewer PASS.
Versioned kernel/cubit_n95_intel_snapshot_v20.img in private workspace; SHA256
dba337806475f19347815f9bce69167949252c18cf93b0c94c0e122c2b41e8ef.
No Intel hardware evidence yet. Physical next: capture snapshot firmware/driver
raw values from startup viewer; firmware scanout remains unchanged. No commands
active. Shared existing sources still not backported pending coordination.

2026-09-27 INTEL LOGSTORE PASS: yc20kdyi native RAM fixture publishes snapshot
via separately tagged devmgr grant (0229, authorityTag4947), budget15 issuance2;
viewer receives it and publication losses=0. Native boot-logs test PASS with
USB hub configured (not a new input stress result). Shared Intel main uses
async publisher with bounded30s startup wait,
process-lived grant backing. Private devmgr only grants tracked Intel PID with
matching cap tag and canonical request. No observer rights. Catalog/audit
restored; generic image still test-only. No active commands, no hardware image.

2026-09-27 NATIVE SNAPSHOT PASS: 9l3mvhjp RAM-backed v2 bootstrap, native
volatile read adapter, both named raw values and later desktop startup PASS;
USB hub input PASS. Enum Image numeric issue fixed using explicit labels.
No Intel hardware validation. Private catalog/audit restored; generic image
is still test-only until rebuilt. No active commands. Logstore wiring next.

2026-09-27 D0 HANDOFF: private devmgr config snapshot + D0-only launch, bootstrap
v2 requires trusted evidence bit; native devmgr/intel builds PASS. Shared pure
bootstrap updated with rejection tests/proof. No native reads or new image.
Old undeployed v1 rejected (no compatibility shim). No active commands.

2026-09-27 PCI POWER: pure intel_gpu_pci_power bounded type-0 config decoder
added, hosted adversarial tests and level2 bounds/termination proof PASS. No
native collection/bootstrap change yet, no MMIO access, no active commands.
Next integrate private devmgr evidence and service handoff; shared edits await
ACK as before. No new image published.

2026-09-27 INTEL OBSERVATION: added intel_gpu_observation staged two-register
capture and hosted recording-reader tests. Nix tests and instantiated SPARK
capture/address/rejection proof pass. No native MMIO enabled: trusted PCI D0
evidence is missing from bootstrap. Next bounded PCI capability/PMCSR discovery,
then connect observation + authorized logstore. No shared kernel/devmgr edits.

2026-09-27 GPU ARCHITECTURE: audited display singleton CAP_SLOT_GPU calls,
acquisitions/submissions, virtio DRIVER_GPU registration and output codec.
Added docs/gpu-rendering-and-presentation.md with mixed-adapter binding/import/
ownership contract and migration gates; linked display/Intel docs. No shared
code/build changes or active commands. RO mapping backport request awaits ACK.

2026-09-27 INTEL BOOTSTRAP PASS: native service s9bbydcr authenticated devmgr
request and mapped RAM fixture read-only; serial checker + hub HID test PASS.
Event-lane fixture rm6khf71 correctly rejected sender=0; switched to capSubmit
NO_COMPLETION_TOKEN (no synchronous driver wait). No MMIO reads yet.
Normal catalog/audit restored (map-check removed); generic image remains a
test artifact until rebuilt. Intel service is now an optical image member.
No active commands; next power-safe register allowlist + logstore diagnostics.

2026-09-27 INTEL SERVICE: main new intel-gpu main/gpr/manifest and pure
bootstrap decoder; hosted tests/proof PASS, native service build PASS.
Private devmgr launches recognized ADLN only, grants READ register region,
asynchronous bootstrap from authenticated devmgr. No MMIO reads or writes yet.
RAM-backed QEMU handoff fixture building/running privately; map-check bootstrap
temporarily present again. Do not publish generic test image. Shared devmgr/
kernel build scripts still untouched, coordination ACK pending.

2026-09-27 NATIVE RO PASS: 4n7sahgp serial checker passes expected write fault
at 0x51000000 after successful read and denied RW/unknown modes. Desktop boots
after fault, hub HID regression PASS. No active commands. Removed map-check
membership from private laptop-usb and restored normal audit; existing generic
image still contains fixture until rebuilt. No hardware image published.

2026-09-27 NATIVE RO FIXTURE: private map-check.app + bootstrap-only devmgr
launcher allocate dedicated RAM, grant READ-only exact page, reject RW and
invalid-mode requests, read via RO alias then deliberately fault a write.
Private laptop-usb catalog/check-image currently includes this TEST artifact;
do NOT publish generic cubit_live_uefi.img as a user diagnostic until removed.
v18/v19 versioned images untouched. Fixture rebuild/run active privately;
shared kernel existing files remain untouched pending ACK.

2026-09-27 DEVICE MAP PRIVATE BUILD/BOOT PASS: isolated kernel build plus
UEFI/4CPU/USB-flash/hub/no-PS2 regression tcdmk_sm PASS, desktop input works.
No active commands; no versioned hardware image published. Native RO write-
fault fixture is next, not covered by this ordinary read/write-driver boot.

2026-09-27 DEVICE MAP PRIVATE: no ACK yet, so experimenting only in private
boot-debug-ts3jrkxp kernel capability/syscall files. Added read-only request
(MAP_DEVICE arg3=1, existing read/write=0), invalid mode/alignment/wrap rejection,
and shared-new pure Device_Memory_Admission helper. Hosted tests + level1 proof
PASS. Private kernel compile/boot regression underway. Not a verified RO mapping
until a native write-fault fixture demonstrates enforcement. Main kernel existing
files untouched; new helper alone does not change main runtime behavior.

2026-09-27 INTEL ADLN REGION: hosted all-page test and level2 proof PASS.
Plan_ADLN_Registers checks fixed BAR encoding/address width and excludes
reserved/GGTT regions. Private devmgr main/gpr integrated PCI-only plan report;
native devmgr build PASS. No MMIO access, image rebuild, or physical validation
of this new report yet. No active build commands. Kernel RO-map request below
is pending; independent GPU protocol/reference work can continue.

2026-09-27 REQUEST kernel/capability owner: Intel inspection requires read-only
uncached device mappings. Current SYSCALL_MAP_DEVICE unconditionally uses
PG_USERIO (writable), and checkDeviceMemAccess requires READ+WRITE together.
Also its base+size <= capBase+capSize comparison can wrap (modular arithmetic).
Request an edit window/ACK for syscall-ipc.adb, capabilities-operations.ad?,
virtmem.ads and matching runtime mapping API to add explicit requested access,
subtraction-based nonoverflow containment and targeted tests. No edits yet.
Until agreed, no inspection service will claim enforced read-only MMIO.

2026-09-27 INTEL RESOURCE PLAN: new intel_gpu_resources.ad? and hosted
resource_tests.adb. Admission validates page-contained non-prefetchable MMIO
requests against a supplied trusted extent, with zeroed rejection. Hosted tests
pass; GNATprove level2 checks-as-errors PASS for contracts/arithmetic, no Assume.
No native GPU mapping or new image. BAR extent provenance is still an adapter
obligation. Native PCI resource handoff remains next; firmware scanout unchanged.

2026-09-27 INTEL GPU: v19 physical startup shows 157 records, zero capture,
publication/service/viewer losses; user confirms normal keyboard/mouse.
Owning userspace/services/intel-gpu/ and tests/intel-gpu/ for resource admission
and hosted proofs. No shared devmgr/kernel edits or MMIO takeover. Preserve
firmware scanout; next native handoff must not infer BAR size from its base.

2026-09-27 USB HARDENING COMPLETE (private): v19 long-log fixture lesionkp
PASS: 133 records, zero capture/publication/service/viewer losses; screenshot
inspected. Delayed-hub HID regression njj2oo95 PASS with PS/2 disabled.
No active commands. Artifact kernel/cubit_n95_usb_hardened_v19.img SHA256
e20bcf653aa0233ab3ff14050749c41f9ffaa6ffdb5db77b494cd4516b4513f6.
Three-second boot delay retained; runtime hotplug lifecycle explicitly deferred.
No shared native changes; private snapshot still excludes latest networking.

2026-09-27 USB HARDENING: user confirms v18 physical mouse AND keyboard work.
Private boot-debug-ts3jrkxp edits only: bounded event-ring drain during startup
settling, capture/viewer capacity 512, explicit viewer eviction count and keep
latest diagnostics. New 30-root-port QEMU fixture exceeds old 128-line limit.
Retaining 3s window pending proper runtime port lifecycle; no early quiet-time
heuristic. Building/testing xhci and boot-logs under private workspace lock.

2026-09-27 v18 READY: private late-hub input test aqjj9gtg PASS; final-image
late-hub automatic viewer j5cdipqc PASS (89 records, no capture/publication loss,
screenshot inspected). Decimal port count corrected. Private artifact
kernel/cubit_n95_usb_discovery_v18.img SHA256
d13e5357ceffabcaa5721a64d2c1821d016e00d3da2c9e9f08134189c5aa8a77.
No active builds/tests; no shared native source edits. Physical timing hypothesis
still unconfirmed. Snapshot excludes newest networking work.

2026-09-27 NUC v17 PHYSICAL: logs show root13 VIA 2109:0812 rejected 0F
(unsupported SuperSpeed hub), root14 090c:1000 storage works. No 2109:2812
USB2 companion seen. Private bounded 3s reconnect-window experiment removes
first-connected early exit and logs all root PORTSCs. QEMU late-hub injection
600ms after controller startup is being tested. Not full runtime hotplug and
not yet proof of physical timing cause. Correcting decimal root count (16
was previously printed as 6). No shared native edits.

2026-09-27 USB LOG VIEWER COMPLETE (private): automatic window and async
startup replay PASS in tqx_v7zu; failure fixture mlvn83o6 rejects hub, has no
input, still shows rejection via logstore. Both UEFI/4CPU/flash/no-i8042.
Screenshot reviewed. v17 artifact cubit_n95_usb_logs_v17.img, SHA256
ee9d0df086805d2884108f94014d709cc4e766ad1c3cc93f65e96c12b9d60e30.
Found missing xHCI->devmgr cap15 in this snapshot; added explicit endpoint.
Early grant request also disturbed startup mailbox handshake, so it is deferred
until logstore registration. General bootstrap message demultiplexing needs audit.
No shared native edits; all builds/tests done. See docs/usb-boot-diagnostics.md.

2026-09-27 USB LOG VIEWER: private boot-debug-ts3jrkxp only: new boot-logs
UI app (observer only), boot_log in xHCI (bounded 128x96 startup capture,
async publisher), devmgr grants only authenticated xHCI caller a log publisher
endpoint once collector exists. Reserved diagnostic budget 15/tag issuance 1.
Private startup/image/Makefile auto-launch viewer and logstore retention 128.
Native build in progress under private lock; shared runtime/devmgr untouched.

2026-09-27 KEYBOARD DELIVERY: private xhci.adb/.ads/main.adb now configures
one boot keyboard, maintains its own interrupt-IN ring cursor and publishes
decoded transitions through existing Input Source_Report/set-1 desktop path.
Private hub/no-i8042 integration test running; no shared native sources changed.
Initial bridge excludes keypad/media/PrintScreen/Pause and key repeat; composite
devices exposing both keyboard and mouse still select one interface per slot.
Hub/no-PS2 native test PASS (2eh2926n), 18 key bytes, no input drops/gaps/resyncs.
Direct-device regression running privately. New image d21e9970ac99133748f32b76703fb09f91a375329768d82e332150707722268f.
Direct-device regression PASS (zytd6_yp); no active commands. Stable private
artifact kernel/cubit_n95_hub_keyboard_v16.img has that checksum. This snapshot
does not incorporate the networking agent's newest work; hardware input test only.

2026-09-27 KEYBOARD/EP0: owning new usb_keyboards.ad? and hosted tests.
Pure boot-report transition decoder handles modifiers/rollover; not yet wired
to native reports. Private xhci EP0 now reads first 8 descriptor bytes, validates
speed-specific packet size and evaluates changed full-speed context before the
full descriptor. Native regression running privately; no shared outputs touched.
Completed: hosted keyboard tests/proof PASS (7 added initialization/postcondition
obligations, 103 cached total); native hub/no-PS2 y1_mgk1b and direct flash/app
4pwmns8o PASS. QEMU fixtures do not exercise changed-EP0-size Evaluate Context.
No active commands. Next: independent keyboard queue and native key delivery.

2026-09-27 DOWNSTREAM ENUMERATION: private xhci.adb separates root reset
from route-based device enumeration; hub slot context and sequential child
addressing in progress. Native tests use only boot-debug-ts3jrkxp private
lock/artifacts. No shared native source edits. Four-port fixture now checks
downstream mouse motion/buttons and keyboard discovery (not keyboard delivery).
PASS: hub input with i8042 disabled (to5dcdxn), plus direct USB-flash/app
regression (_g1auh7i). No commands active. Native implementation is still private;
keyboard reports, hotplug, multi-TT and EP0 packet-size negotiation remain.

2026-09-27 HUB PORT PROBE: owning new usb_hubs.ad? and hub_tests.adb in the
isolated hosted test project. Parser/status tests cover 255 port counts and
all U16 status values; combined cached SPARK report 96 discharged, none
unproved/justified. Native config/power/reset probe remains PRIVATE in
boot-debug-ts3jrkxp. Four-port QEMU statuses reach 0x103 on ports 1/4;
Harness PASS after replacing numeric enum Image with explicit READY
(k4yiv225). Direct USB-flash/app regression also PASS (znu26hiu).
No active builds/tests remain.
No child addressing, TT context configuration or input delivery yet. Private
native build/test only, no shared build outputs or devmgr edits.

2026-09-27 HUB ROUTES: xhci_topology.ad? private-path model and tests added.
13 topology obligations discharged, no Assume/SPARK-Off. Private root-slot
integration preserves direct USB-flash/app regression (xnxnlz6e). New private
run-live.py --hub-discovery identifies QEMU hub + mouse/keyboard children
without blocking desktop (zukc89zc); explicitly NOT an input-delivery test.
No shared harness changes or active shared build. Next: bounded hub descriptor/
port state machine and xHCI hub/TT context setup. SuperSpeed hubs, MTT setup,
device lifetime and downstream report delivery are not implemented by this model.

2026-09-27 COORDINATION ACK: networking may take procmgr's network-scope
decode/install branch for the requested 128-bit format, plus the earlier
network-owner-release exit/reuse cleanup. No current procmgr edits by me.
Please preserve boot diagnostics/Config paths. My native changes remain
private devmgr/xHCI; shared new scope is xhci_topology.ad? and its hosted tests.
Still requesting shared devmgr inventory/backport edit window acknowledgment.

2026-09-27 INPUT + OUTPUT: physical photos confirm GPU 8086:46d2 and VIA
2109:2812 USB2 hub with 045e:0823 mouse (port1) and 24f0:0140 keyboard
(port4), both full-speed composite HID. Flash is directly attached USB3.
Owning usb_configurations.ad? plus new tests/usb-input-discovery/ and hardware
inventory note; no overlap with networking. Native adapter experiments stay
private until shared devmgr edit window acknowledged. USB2 hub enumeration,
TT routing and boot keyboard delivery are NOT implemented yet.
Decoder discovery now implemented (boot keyboard + USB2 hub); hosted new and
existing descriptor regressions PASS, 59/59 SPARK obligations discharged.
Private xHCI native build PASS, diagnostics identify unsupported hub/keyboard
delivery explicitly. No active builds, no new burn image advertised. Hardware
evidence and next native steps: docs/n95-reference-hardware.md.

2026-09-27 INTEL INVENTORY PRIVATE: awaiting devmgr acknowledgment, integration
is only in .build-workspaces/boot-debug-ts3jrkxp/userspace/services/devmgr/main.adb
and devmgr.gpr. Reads PCI identity/revision/command/BARs; no register mapping,
size-probe writes, bus-master enable, or display takeover. Shared intel-gpu
pure BAR decoder + tests now pass all 15 SPARK obligations. Private native
build/image audit pass; USB-flash desktop/app regression PASS (1hqqjjau).
No active build/test. Private cubit_live_uefi.img SHA256:
76a776772483f5056fd00229f99fecd95d74be854e48afc846ac7e2bace66370.
This validates the non-Intel path, not Intel register access. No physical
Intel inventory result yet; no GPU MMIO code is enabled.

2026-09-27 INTEL GPU FOUNDATION: user confirms v15 boots physical N95 desktop.
USB mouse is behind keyboard hub; keyboard/hub support remain separate work.
User requests Intel graphics next. Owning NEW userspace/services/intel-gpu/
pure probe model, tests/intel-gpu/, docs/intel-gpu-bringup.md only for now.
No devmgr/kernel/scheduler/build-script edits in this slice. Hosted tests use
disjoint output. Native PCI handoff and source backports require coordination.
Hosted probe tests and level-1 SPARK pass (10 obligations; no unproved/Assume).
REQUEST: next native slice needs narrow devmgr PCI inventory reporting of Intel
display identity/revision/BARs, then an authorized resource handoff. Please
acknowledge idle window/ownership for scanPCI and setup path before I edit them;
your setupVirtioNet and network grant changes will remain untouched. No active
build/test now. Current Intel code is not wired into any image.

2026-09-27 USB FLASH FIX READY PRIVATELY: 512-byte read-only SCSI sector
translation + ISO signature admission fixes reproduced N95 boot symptom.
v15 image SHA 476af6642a4975199c74b742207a32099ba724c5490caaba3f463984ca0030e1
in .build-workspaces/boot-debug-ts3jrkxp/kernel/cubit_n95_usb_flash_v15.img.
Flash and CD UEFI desktop/app regressions pass; pure codec 31/31 SPARK checks
pass. Main build lock was unavailable, so implementation backport deferred;
see tests/usb-optical/NUC-FLASH.md. Request short idle edit window for USB
source/test backport + narrow devmgr procmgr failure reporting. No changes
to your scheduler, IPC, network grant lifecycle, or setupVirtioNet.

2026-09-27 NUC FAILURE REPRODUCED: unchanged v14 on QEMU scsi-hd USB BOT
(512-byte sectors) rejects the medium, cannot load config/procmgr, prints
startup complete anyway. Logs private tmp/cubit-usb-live.5cqf4h3s. Fixing
read-only ISO-on-USB-disk sector translation and required-launch diagnostics
privately; no scheduler/IPC change. Read networking's 2026-09-27 wake-latency
data and wake-affine proposal: follow-up after physical NUC confirmation is
current=NO_THREAD alarm/dispatch audit, then measured soft locality policy.
Please retain/share your traces; no competing scheduler edits from me now.

2026-09-27 CORRECTION / NETWORKING COORDINATION: user requests NUC boot first,
then joint scheduler/locality work with networking. Prior notes claiming
devmgr startup complete proves a procmgr handshake are WRONG: the message is
unconditional even when spawnFromBootStorage returns 0. No IPC-return bug has
been established. Investigating private workspace USB flash vs CD-ROM mismatch:
NUC uses dd-written USB stick; run-live.py uses media=cdrom. Main scheduler,
process and IPC source remain yours; please record your current scheduler bugs
and changes here in your own note for later integration. I will use private
devmgr/xHCI/storage diagnostics and coordinate any main-source backport.
CCL language/launch parameter work has no overlap with my current boot work;
you may proceed on those files. Procmgr exit cleanup request acknowledged,
deferred until NUC boot; please coordinate before overlapping procmgr edits.

2026-09-27 NUC RETAINED-LOG TRACE ACTIVE: physical N95 visibly reaches NUC
trace 12, with devmgr's post-procmgr-ready diagnostic. Its QR cannot distinguish
that label from the preceding stage because both compress to `Starting ser`.
Private v13 proved whole-string delivery, but its v13 physical QR still omitted
the procmgr line: the best-effort Try_Enter could discard the entire bounded
line during a concurrent panel paint. Private v14 gives ordinary process-context
strings a bounded panel-lock wait; panic/character paths remain fail-open.
Normal serial/video output and all scheduler/IPC/authority behavior are
unchanged. Image `.build-workspaces/boot-debug-ts3jrkxp/kernel/cubit_n95_retained_log_v14.img`,
SHA256 `f05ab123dbdf5569b00a6cba9bc2ebaea9fa89d8a85318a252e56d144d5bb6fe`.
It labels its current stage `T14 retained boot log`, which is QR-distinct.
Private kernel/image audit and UEFI 4-vCPU desktop/DOOM/Workbench regression
pass. No shared source backport or active job.

2026-09-27 STARTUP-HANDOFF TRACE ACTIVE: QR capsule on physical N95 decodes
to `C=Starting services; L=Starting userspace; D=devmgr: startup complete`.
That is a successful kernel/devmgr handoff, not an i915 prerequisite. I own
only diagnostic presentation plus markers in `userspace/services/procmgr/main.adb`:
the boot panel now retains completed procmgr/display bootstrap lines as its
current step, while normal output remains transient detail. Procmgr emits a
bounded ordinal/name marker immediately before and outcome marker immediately
after each trusted `init.ccl` launch. The same narrow changes are in the private
N95 workspace for an image build. No spawn, authority, catalog, networking, or
driver semantics change. After the image is tested, I will release this scope.

2026-09-27 SERVICE TRACE 6 READY: physical PS2 PROBE 5 reached devmgr complete.
New procmgr post-ready markers bracket SBRK launch buffer, FS grant, Config
grant, and init.ccl read. Private UEFI 4CPU desktop/DOOM/Workbench/Files pass
(3ux7genh); updated procmgr source backported under lock 94115, released.
Image .build-workspaces/boot-debug-ts3jrkxp/kernel/cubit_n95_service_trace_v6.img
SHA256 033d7d8f421b1c1c185e9727d083cb23cd8f5d1aba970e52062d7c9dece4b5c9.
No active jobs. This is diagnostic-only with no behavior or authority change.

2026-09-27 SERVICE HANDOFF TRACE ACTIVE: physical N95 PS2 PROBE 5 reaches
`devmgr: startup complete, entering service loop`. That implies procmgr sent
its ready reply. Adding private `userspace/services/procmgr/main.adb` trace
markers only around post-ready ELF-buffer allocation, FS/Config grants and
trusted init.ccl processing; then a new image. No changes to spawn semantics,
CCL parsing, procmgr authority policy, kernel scheduler, or networking files.
Networking: this is diagnostic-only; I will not touch your requested network
owner-release work or general parameter-launch work.

2026-09-26 PS2 PROBE 5 READY: absent-i8042 QEMU reproduces N95's last line
with v4 image (zqu7vx_7). Bounded startup probe now sends OP_NOT_PRESENT and
exits, allowing xHCI startup. Hosted absence/drain fixtures pass; native USB-only
desktop + mouse movement/buttons pass (p2nc01uj); normal PS2 desktop/DOOM/
Workbench/Files pass (gpn2pzhp). Initial USB-keyboard app test intentionally
stopped after discovering current xHCI rejects boot keyboard (result 0F);
no USB keyboard claim. All source/test edits backported under locks 16341/84429,
released; no active jobs. Image:
.build-workspaces/boot-debug-ts3jrkxp/kernel/cubit_n95_ps2_probe_v5.img
SHA256 7e3e94854c7dc0d12c51e40a0e023c3d9b39bc5d6519c0f40658100c5a8ed31b.
Physical N95 result pending. See tests/ps2-probe/README.md; no commits/pushes.

2026-09-26 PS2 ABSENCE ACTIVE: N95 SMP MAP 4 advances to devmgr: PS/2 driver
started, then stops. PS2 flushPS2 loops forever if absent port reads FF;
devmgr waits for readiness before xHCI startup. Working privately in
userspace/services/ps2/, tests/ps2-probe and USB-live runner no-i8042 fixture.
No edits to networking/devmgr/process sources planned; no shared build outputs.

2026-09-26 SMP MAP 4 READY: backported topology/startup/IPI fixes under lock
64098, released. Focused topology tests and all 20 SPARK checks pass. Private
QEMU runs: sparse APIC IDs (0,1,2,4,5,6) + PIT-free fixture passed desktop/apps
(ujbjgfih); halted AP diagnostic passed (loaiyn3j); previous v3 image reproduces
sparse-ID stall after CPU 2 (y5g9inzx); normal 4-CPU desktop/apps passed (0_0dcc_3).
Image .build-workspaces/boot-debug-ts3jrkxp/kernel/cubit_n95_smp_map_v4.img
SHA256 e53404ee61073f922868f9a88e7b59803a97a253b1ba7ff69c84dbabc6103dee.
Heading SMP MAP 4. Physical N95 confirmation pending. No active build/test jobs.
Private snapshot still predates networking's scheduler edits; not overwritten.
See tests/cpu-topology/README.md for scope and remaining device-routing limits.

2026-09-26 SMP STARTUP ACTIVE: PIT-FREE 3 reaches Live system image loaded
on N95, stalls at Starting SMP CPUs. Working privately on acpi.ad?/kmain.ad?,
boot.asm/lapic.adb plus CPU topology unit and ipi.adb destination translation.
MADT currently discards APIC IDs and boot/reschedule assume logical ID equals
APIC ID; will preserve hardware mapping. No edits to networking-owned process,
IPC/futex scheduler or idle files. Main backport only while holding build lock.
Networking: your noted ipi.adb logical-ID bug is included in this fix.

2026-09-26 PIT-FREE 3 VALIDATED: changes backported under short lock61774,
released. No shared image rebuild; private snapshot still isolates networking's
new scheduler work. New boot_timer_rates pure unit: 1000 CPUID combinations,
LAPIC boundary fixtures, all 10 SPARK checks pass. Panel tests/18 checks pass.
Native UEFI/KVM desktop/DOOM/Workbench/Files pass with (1) no PIT + coherent
N95-rate CPUID cache fixture, 4 vCPUs; (2) invalid denominator + PIT fallback,
1 vCPU; (3) ordinary CPU/PIT, 4 vCPUs. Absent PIT + absent CPU-rate information
still fails bounded with expected diagnostic. Details tests/boot-timer-rates/README.md.
Candidate image: .build-workspaces/boot-debug-ts3jrkxp/kernel/cubit_n95_pit_free_v3.img.
Top marker PIT-FREE 3. Native N95 result pending. LINT0/1 mask AND->OR fixed
in same LAPIC handoff; does not explain earlier pre-LAPIC N95 failure.
Avoid process/scheduler files owned by networking. No commits/pushes.

2026-09-26 PIT-INDEPENDENT BOOT ACTIVE (user requested): private workspace
.build-workspaces/boot-debug-ts3jrkxp. Scope time.ad?/lapic.adb/kmain.adb,
cpuid.ad? crystal-field rename, boot_timer_rates.ad? (pure checked arithmetic),
tests/boot-timer-rates and native boot regression. Will backport under short
main lock. Avoiding networking-owned process/scheduler sources. Native timer
calibration will use validated CPUID.15 where available, otherwise bounded PIT;
LAPIC countdown calibration will wait on TSC, never require PIT interrupts.

2026-09-26 PHYSICAL N95 IRQ2 RESULT: n=7 ret=7 pic=7 h=1089C gap=1F90F5D2.
At CPUID.15 TSC rate 1,689,600,000 Hz: max Ada-handler 40.092 us,
max IRQ-entry gap 313.443 ms. All observed handlers returned and PIC ISR0
confirmed each entry. Delay is outside measured Ada handler; exact chipset/
firmware/interrupt-routing cause still unknown, clock gating NOT established.
Read-only boot audit: Time.calibrateTSC waits 100 PIT ticks; Lapic.calibrateAPICTimer
also waits 10 PIT ticks. Both dependencies must be removed for a PIT-free path.
CPUID already records leaf15 ratio/crystal (tscFreqHz is misleadingly named:
it holds ECX crystal Hz, not derived TSC Hz). Candidate next change: validated
CPUID TSC rate + bounded LAPIC countdown calibration against that TSC;
test boot with QEMU PIT absent. No implementation started this turn.
Networking now owns process.adb/process-ipc.adb/process-futex.adb,
services-idle.adb/scheduler_timing.ads; avoid those files for timer work.

2026-09-26 IRQ PROBE 2 READY: private kernel/cubit_n95_irq_probe_v2.img under
.build-workspaces/boot-debug-ts3jrkxp; SHA256
8c7405fefb1d6bd6c6bcfd1748465886dc9f86bb9f8f54b6a69e58c27575fcd5.
Top heading IRQ PROBE 2, final Latest Diagnostic compact IRQ2 summary. Verified
in actual QEMU failure screenshot; dropped-EOI test checks duplicated numbers
agree and fit 96 columns. Normal four-vCPU live boot passed; panel tests/proofs
pass. Three code/test changes backported under short lock90583, now released.
No locks held. Physical missing-row cause still unknown. No timer fix claimed.

2026-09-26 IRQ PROBE VISIBILITY: user reports no new rows even on fallback.
Extracted exact image kernel and verified SHA256 matches private kernel
70fb6d77b5fc8d6af5427b1dadcd9de3127d71b990d0f54a3a3c280a9afb2a04.
It contains both new headings; those draw at Setup, before timer calibration.
Preparing private v2 with top-of-panel marker and summary in Latest Diagnostic,
plus distinct artifact filename. Only boot_panel.adb, boot_timer_diagnostics.adb,
and probe test assertions affected. Shared outputs untouched.

2026-09-26 N95 DIAGNOSTIC IMAGE READY: reviewed IRQ probe/panel/test/docs changes
backported from private workspace under shared lock36440, now released.
Image .build-workspaces/boot-debug-ts3jrkxp/kernel/cubit_live_uefi.img SHA256
dd1b28640015b5a07df78a057b28eab6a55717ce51ad1f845368f3c9167583e1.
New IRQ timing/source rows visually checked on QEMU failure screenshot.
Slow-PIT, dropped-EOI, masked-IRQ fixtures and normal UEFI/KVM 4-vCPU live boot
pass; 299592 panel transitions, hosted renderer bounds tests, all 18 panel/font
SPARK checks pass. IRQ hardware observations explicitly remain SPARK Off.
No main images overwritten. No N95 fix claimed; awaiting physical probe data.

2026-09-26 N95 IRQ PROBE: developing/testing in existing private workspace
.build-workspaces/boot-debug-ts3jrkxp only. New boot_timer_probe.ad? and edits
interrupts.adb, boot_timer_diagnostics.adb, boot_panel.ads, boot_diagnostics.adb,
tests/boot-panel/adapter/main.adb, tests/timer-boot/probe.py. Will request/hold
main lock for short reviewed backport. No main builds or source edits yet.
Probe measures entry spacing, Ada-handler time, pre-EOI PIC ISR; no routing
change. Intel AlderLakeN FSP documents optional PIT clock gating; Linux avoids
mandatory PIT on modern discoverable-timer platforms. N95 cause unconfirmed.

2026-09-26 PRIVATE BUILD VALIDATED: no shared or private lock held now.
Workspace .build-workspaces/boot-debug-ts3jrkxp completed a fresh kernel/runtime
build, UEFI image audit (8 bootstrap / 21 CD payload files), and 4-vCPU KVM
USB live boot regression. Image is private kernel/cubit_live_uefi.img, SHA256
6c9a242203b144c39610b2504e18264d4c3b6bb015d243e32eabc529426d447d.
Main UEFI/laptop image hashes stayed unchanged. Main kernel/build.ads changed
after our private kernel finished (concurrent shared activity); we do not claim
the entire shared output tree remained unchanged. Copies use independent files.
New helper/protocol: tools/build-workspace.py and coordination/README.md.
9 hosted tests pass, including private execution while the main lock is held.
Other agents may use separate snapshots now; no long shared lock for private
native builds/package/QEMU. Not yet a full make-world dependency snapshot.
This validates build isolation, NOT a fix for the N95 PIT delivery failure.

ACTIVE 2026-09-26 PRIVATE BUILD WORKSPACES: user authorized isolated builds.
Holding shared lock70328 briefly for initial stable source/artifact snapshot
and tooling/docs changes. Scope new tools/build-workspace.py, tests for it,
coordination protocol, ignore rule. Private builds/QEMU will then use their
OWN workspace lock and copied sources/seed binaries, not the shared lock.
No networking, Makefile or existing test-runner edits. Timer IRQ measurements
resume after workspace validation; no shared kernel source edits this round.

2026-09-26 POST-TAKEOVER TIMING CONFIRMED: span=F29D8AE4,
first=1F7DCF8A,last=DD2BD96C,ticks7. At CPUID-reported1.6896GHz:
span2409.097ms,first312.701ms,last2196.164ms; mean first-to-last interval
313.911ms versus313.896ms before takeover. Individual IRQ gaps still unknown.
Pattern essentially unchanged; need handler-entry/exit/source observations.
Build lock still unavailable; no kernel edits/new image this turn.

REQUEST 2026-09-26 BOOT IRQ WINDOW: still waiting for build.lock; user confirms
ticks remain7 after takeover. Need edit interrupts.adb + boot-only diagnostics
to record IRQ entry/exit TSC, pre-EOI PIC ISR, and handler duration; rebuild/test
UEFI image. No process/thread/netstack changes. Please yield after current
native test; no shared edits started. User asked to relay this request.
Read-only research also found Linux c8c4076723daca08bf35ccd68f22ea1c6219e207:
modern Intel PIT clock gating motivated optional PIT initialization. Not yet
proven the N95 cause; IRQ duration/source measurements come first.

2026-09-26 N95 STILL FAILS: user reports incomplete ticks with HPET1->0,
T0=00008030, LVTT00020005->00030020. HPET legacy replacement was OFF;
that hypothesis does NOT explain this hardware boot. Readbacks show takeover
worked. Checking IRQ arrival/handler duration rather than increasing timeout.
Build lock currently held elsewhere: no kernel edits yet; read-only audit.

HANDOFF 2026-09-26 TIMER TAKEOVER COMPLETE: releasing lock84856. All jobs
terminal. Boot_Timer_Setup now quiesces HPET (clear only config enable/legacy
bits, preserve reserved fields, verify readback) and masks/stops inherited
LAPIC timer before PIC/PIT/STI. No speculative EOIs or LINT routing changes.
Checked HPET ACPI decoder accepts bounded mapped register window; malformed
or duplicate HPET fails closed. Retained fourth evidence row before/after.
146444 hosted HPET checks, all23 SPARK checks; Boot_Panel all18 plus hosted
renderer/model pass. MMIO/MSR adapters remain trusted, NOT hardware proofs.
QEMU injected314ms HPET replacement + skip repair reproduces normal PIT range,
late sparse ticks and clean PIC FA/IRR0/ISR0, like N95. Same injection with
takeover enabled gives HPET3->0 and successful PIT/LAPIC calibration. Normal
UEFI4 and BIOS1 desktop/DOOM/Workbench/Files pass. Missing and masked PIT still
fail with expected bounded diagnostics. Final recovery rerun passes after
address-limit/label changes. Failure screenshot visually checked, allrowsfit.
Final UEFI image tests/boot-hpet/build/image-final.log; BIOS image-bios-final.log.
QEMU artifacts under tests/config-turso/build/tmp/nix-shell.{uKzpan,5UUtiJ,
KwLSex,tOoCUO,ZQxWtY,kja8T4}; final recovery kja8T4/timer-boot-6dxh9epp.
Physical N95 cause still UNCONFIRMED pending new image test. No host USB writes,
commit/push, networking changes. Kernel sources available by coordination.

ACTIVE 2026-09-26 TIMER TAKEOVER: holding lock84856 now. Isolated HPET
decoder/register policy proved (23 checks); integrating checked ACPI HPET
discovery, early HPET/LAPIC timer quiescence, retained evidence and QEMU
replacement-timer injection. Scope kernel acpi/kmain/boot diagnostics + new
boot_timer_setup, shared Firmware_Tables.HPET, tests/boot-hpet/timer-boot.
No Makefile, netstack or runtime edits.

2026-09-26 N95 CONFIRMED: seven ticks in 2.409s; first0.313s/last2.196s,
PIT B4/B4 count1..1193, PIC FA/IRR0/ISR0/IF1, LVTT00020005,
APIC ISR/IRR empty/PPR0. Investigating HPET replacement and firmware timer
takeover. Build lock currently unavailable; NO kernel/shared build edits yet.
Working on isolated pure HPET descriptor/register policy tests first.
Networking Makefile prove-tcp-session-only change IS ACKNOWLEDGED (see prior
notes); please proceed under lock. Need kernel/native build window afterward.

HANDOFF 2026-09-26 TIMER EVIDENCE READY: releasing lock36752; all builds and
QEMU jobs done. New .img diagnostics contain three retained evidence rows:
first/last observed tick offsets and span/budget, PIT status/min/max/CPUID15,
xAPIC/x2APIC SVR/LINT0/LVTT/highest ISR+IRR/PPR. APIC inspected read-only on
failure (device-page mapping only); no routing/acknowledgement fix yet.
No IRQ-handler hot-path instrumentation. Still need physical N95 panel photo.
Pure Boot_Panel evidence freezing: 299592 transitions, all18 SPARK checks;
renderer bounds/retirement/busy tests pass including 1024x768 full-width text.
Slow PIT fixture (reload65536) gives incomplete ticks with clean PIC like N95;
suppressed seventh EOI gives seven ticks but IRR/ISR01, UNLIKE N95's zeroes.
Both timing assertions pass; missing-PIT and masked-PIC regressions pass.
UEFI4 and BIOS1 normal live tests pass; final font-scale-only refresh reran
hosted renderer tests, stopped-tick fixture and UEFI4 live test successfully.
Current UEFI195MiB SHA256 7bc8e1fb134e2437b46a2d0f899954fe3484af17015f471efb2d56bb57376ecc;
BIOS191MiB b70653b690ab44b01b5a717abcb1185349baa640caa8e6866fb0bc3ef0f07966.
Artifacts tests/config-turso/build/tmp/nix-shell.0k9qMo/timer-boot-{a_irafld,0h5ty70d}
(slow/stopped; failure screenshot visually reviewed), .0wvKlc/ (missing/masked,
UEFI4,BIOS1), .sCqlBa/ (final stopped and UEFI4). Logs in tests/multiboot2/build/
timer-evidence*. No networking/Makefile edits, commit/push, or host disk writes.

ACTIVE 2026-09-26 TIMER FOLLOW-UP: holding lock36752. Adding bounded panel
evidence rows, first/last tick timestamps, PIT status/count and APIC state
snapshots; QEMU slow/stopped-delivery injections; rebuild live .img files.
Scope boot_panel/boot_diagnostics/boot_timer_diagnostics/kmain plus isolated
tests/boot-panel and tests/timer-boot. No Makefile, networking or runtime edits.

HANDOFF 2026-09-26 IMG FILENAMES DONE: releasing lock52477. Live builder now
outputs cubit_live_uefi.img and cubit_laptop_usb.img; format remains ISO9660.
Updated run-usb-live paths, live/timer runners, docs, image-plan ignore rule;
realizer accepts .img or .iso for an optical plan, still .img for bootstrap.
18 CCL image tests and both regenerated payload audits pass. No kernel code
changes in this filename-only round; next N95 timing diagnostics still pending.
New UEFI195MiB SHA256 c41891fa7455adf407954fbf8749961e961100bc33622a3f2e970e0412166cf3;
BIOS191MiB fe245b77c4a931c509f0987e35781ccc1fc71bf6bfb0ee4cdafdd3332a291a8e.
Make target names unchanged. No commit/push or host USB writes. Networking's
prove-tcp-session Makefile-only edit remains acknowledged; no overlap there.

ACTIVE 2026-09-26 LIVE IMAGE EXTENSION: holding build lock52477 for user-requested
.iso -> .img output filenames (format unchanged). Scope USB live wrapper,
run-usb-live Makefile paths (NOT prove-tcp-session), live/timer test paths,
current live-image docs. Existing built images will be renamed, no rebuild of
services/networking needed. This briefly supersedes the pending timer work.

2026-09-26 N95 TIMER FOLLOW-UP, WAITING FOR BUILD WINDOW: physical N95
reports PIT moved=Y, ticks=7, mask=FA, IRR/ISR=0, IF=1, incomplete delivery;
failure about two seconds into boot. Need first/last tick cycle stamps and
PIT mode/count readback in next diagnostic ISO. Build lock unavailable;
NO kernel/shared-source edits or native build started this turn.
ACK networking's requested Makefile prove-tcp-session-only update to
tcp_slots.adb tcp_wire.adb / level1: yours to edit under lock. I will not
touch Makefile. Please post when native builds/tests release the window;
my next scope is boot_timer_diagnostics only + ISO packaging.

HANDOFF 2026-09-26 N95 TIMER DIAGNOSTICS COMPLETE: releasing lock90268;
all native/test processes finished. Scope only kernel kmain/time and new
boot_timer_diagnostics, tests/timer-boot. No routing fix yet: physical cause
requires next N95 panel photo. Bounded boot wait reports PIT movement, ticks,
PIC mask/IRR/ISR and IF, retained on failure. No SPARK proof claimed for
hardware observation. QEMU missing PIT reproduced old exact wait RIP at
time.adb:52; final absent PIT and guest-OUT-masked IRQ0 assertions pass.
HMP PIC write injection was ineffective; rejected. LINT0 physical GDB write
also ineffective, rejected/removed. Normal new UEFI4 and BIOS1 live tests pass
(desktop/DOOM/Workbench/Files). Nix for all builds/tests.
Current UEFI ISO195MiB SHA256 e5e7d3812a3826c13cf96472d15b482ecd63d9ab76ea14c6d1e94a2364a3ee94;
BIOS USB ISO191MiB a2f00424c0e3d2835221ca8fc2db70d032a6fd1e1cefd749e09495de1ff371dd.
Logs/screens: tests/config-turso/build/tmp/nix-shell.FL2Yup/{timer-boot-4zold6xo,
timer-boot-h_hczwmr,cubit-usb-live.8tlz386s}; UEFI4 at nix-shell.41vRr2/
cubit-usb-live.2sav41ay. Absent-PIT panel visually reviewed in .41vRr2/
timer-boot-hf70vslm/boot.png. No host USB writes. No commit/push.

ACTIVE 2026-09-26 N95 timer diagnosis: holding build lock90268. Kernel boot
reaches ACPI then stalls at PIT setup. Scope: kernel timer/boot diagnostics,
isolated tests/timer-boot, rebuild kernel + live ISOs using staged services.
No networking/runtime edits, no host disk writes. Reproduce missing PIT and
masked PIC delivery in QEMU before claiming a physical-hardware cause.

HANDOFF UEFI COMPLETE (2026-09-26): Multiboot2 UEFI optical boot passes OVMF
one/four CPU, plus BIOS one-CPU regression. Current cubit_live_uefi.iso195MiB
SHA256 a6be877571e430c6d7279a57137900271e784c62a833305b1ea3d421f4d69fb8;
cubit_laptop_usb.iso191MiB e65912181e47558b470535754b2e24be6dec04a316d8bad2d00a883f8c3715fe.
UEFI4 exercised desktop, DOOM, Workbench native Config write/read42, Files and
Servo's existing rectangle demo (font/page root remains networking follow-up).
Visual review of desktop/DOOM/Config42 screenshots done. Artifacts now at
tests/multiboot2/build/artifacts/{uefi-four-cpu,uefi-one-cpu,bios-one-cpu}.
No host USB/disk writes. Ordinary dd USB stick support STILL MISSING: only
optical LUNs admitted; NUC physical hardware validation not yet performed.

New Multiboot2_Info pure decoder uses limited/by-reference snapshot and explicit
element initialization; native Parse stack272 bytes STATIC, no secondary-stack
or allocator references. 130551 hosted checks, SPARK164 all discharged. Shared
Firmware_Tables 26674 checks/SPARK57 pass final sources. Existing boot-entry
169416, framebuffer17427, module1000460 tests pass; image18 tests and both
8-bootstrap/21-optical payload audits pass. Both GRUB boot headers recognized.
Native raw adapters retain mapping/backing/immutability assumptions, not SPARK
proof of all ACPI or IOMMU. ACPI gets explicit RSDP under EFI, bounded EBDA/ROM
scan under BIOS, retained-backing/length/checksum admission, exact RSDT width,
MADT subrecord and MCFG size checks. AML/userspace service and VT-d not added.

Touched kernel boot/acpi/multiboot, kernel/cubit.gpr/Makefile (shared sources),
USB live GRUB/runner/audit and docs; no networking/runtime/syscall edits.
Image realizer and runner now honor TMPDIR (hardcoded /tmp hit user quota).
Moved only completed/lsof-checked old luckj4pn,ydnakdks,axubo2ob directories
into tests/multiboot2/build/artifacts/, preserving contents. Initial full image
build26904 failed packaging quota AFTER all service builds; subsequent direct
BIOS/UEFI realization60721 passes using fresh kernel/services. Logs in
tests/multiboot2/build/. All test/build/proof sessions terminal; releasing
lock82682 now. No commit/push. Other agent backups untouched. Kernel handoff
no longer actively editing; coordinate before overlapping follow-up.

ACTIVE UEFI INTEGRATION (2026-09-26): ACK networking's kernel handoff. Holding
build lock82682 for boot/ACPI/build edits and native tests. Scope kernel/src/
boot.asm, multiboot*, acpi*, cubit.gpr/Makefile plus USB live GRUB/test staging,
shared/firmware and isolated tests/multiboot2/. Adding a checked Multiboot2
handoff, keeping BIOS Multiboot1 tested; no DMA syscall/network/runtime edits.
Other agent backups remain untouched. No host USB drive writes authorized or
planned. Plain USB disk support remains separate from USB optical live boot.

ACTIVE (2026-09-26): user approved ACPI/IOMMU work, with AML in userspace.
REQUEST networking owner: narrow handoff of kernel/src/acpi.*, boot.asm,
multiboot* and corresponding kernel build integration for validated UEFI ACPI
handoff. Please ACK before I edit existing kernel sources. No DMA syscall or
driver changes without further coordination; your network work remains yours.
While awaiting handoff I own new shared firmware parsing units in
shared/firmware/ and isolated tests/acpi-tables/, plus docs/acpi-userspace.md.
Starting with bounded RSDP/SDT admission, no raw-address reads, no AML interpreter
in the kernel. Hosted Nix tests/proofs use isolated outputs; no native build
or shared build-script edits yet. Config activation work paused, not abandoned.

PARSER HANDOFF: shared/firmware/firmware_tables.ads/adb and tests/acpi-tables/
complete first bounded RSDP/SDT admission slice. Nix hosted26674 checks PASS;
SPARK57 (39 runtime,8 functional,6 initialization,4 termination), no unproved,
justified, Assume or SPARK-Off. Latest session6569 done0; no running jobs.
Raw-memory adapter, UEFI handoff, DMAR body and IOMMU remain unimplemented.
No existing kernel sources or ISO artifacts touched. Awaiting kernel handoff
ACK above before native integration; docs/acpi-userspace.md records boundary.
Earlier parser-only proof had modular-to-index reasoning failures, resolved by
nonwrapping signed length admission (no added guard/assumption). Final proof
tests/acpi-tables/build/obj/gnatprove/gnatprove.out. No commit/push.

HANDOFF (2026-09-26): refreshed live ISO; activation work paused at user request.
kernel/cubit_laptop_usb.iso (BIOS,191MiB) includes current Config/Turso,
Workbench/samples/Inspector and existing Servo plus demos. No ATA/NVMe admitted.
Config explicitly selects :memory:, loses data on worker restart/reboot; no
automatic disk-failure fallback and no weakened flushes or manifest expansion.
New Store::open_volatile uses :memory: URI to avoid Turso's file-ID registry
aliasing separate MemoryIOs; hosted example catches that. All51 Rust tests and
18 CCL image tests pass. Native BIOS4CPU tests pass desktop/DOOM/Workbench,
Config write and read42 assertion, Files and Servo frame. Final logs/screens:
/tmp/cubit-usb-live.luckj4pn; earlier nf9gwnqe/ydnakdks pass. First a9qxyvk_
fixture failed typing uppercase; fixed keyboard mapping, not an OS bug.
Latest BIOS SHA256 ddbeb614cc96cc21983eab7b6e6d327cf84cd8f9138f63bfbcbfc36d7dd98f90.

IMPORTANT UEFI BLOCKER: added usb-live-uefi-iso companion and pinned EFI modules/
mtools in flake, explicit GRUB module-directory in realizer with narrowly tested
generated EFI loader allowlist, insmod all_video for GOP. UEFI catalog validated.
kernel/cubit_live_uefi.iso195MiB reaches kernel under OVMF but PANICS at ACPI
RSDP not found (kernel/src/acpi.adb:findRSDP scans only E0000..FFFFF). This is NOT
a passing UEFI artifact. Latest failed test /tmp/cubit-usb-live.axubo2ob; earlier
dniu_o4v missing GOP fixed by all_video. No kernel source touched. Need coordinated
validated UEFI RSDP handoff, NOT arbitrary physical memory scan. Asking user
before expanding into kernel bring-up. N95 without CSM remains blocked.

SERVO LIMITATION/REQUEST for networking owner: binary loads from optical and
renders built-in rectangle, but libc's SYSTEM_VOLUME=@nvme:0 prevents font/page
reads on live media. Need boot-volume binding (or native configurable root), not
FS fake-NVMe alias/broader authority. No Servo/libc source changes by us. Added
live menu Servo entry; not a full live-media browser. No system.ccl/desktop/
netstack edits, user disks touched, commits or pushes. Shared live-image edits
and builds/tests held lock92413; releasing at handoff. No proofs run this slice.

HANDOFF (2026-09-26): source-bound Config activation controller complete as a
shared foundation, NOT native activation. New config_activation.ads/adb and
tests/config-activation/:126 hosted checks, SPARK43 (five functional), none
unproved/justified. Explicit Activate_Config uses existing scoped rule engine;
Read_Write, bootstrap wildcard and wire mask3 do NOT gain activation. Normal
collection handles reject activation even when explicitly granted. Owned exact
CCL source/target/base/reviewer/grant revision, recheck before dispatch, private
snapshot, separate storage-selected/consumer-applied states, uncertain work
blocks retry. Auth linearizes at Begin; later revoke doesn't cancel accepted IO.
One machine-context setting; v1 normalized strings, not typed persistence yet.
Next: trusted managed-schema storage adapter, atomic source+value revision, CAS,
recovery/reconciliation and native issuer/IPC/UI join. No managed Set bypass,
database-format change, source provisioning, appearance migration or native
activation endpoint. Consumer incarnation identity/sender authentication remain
shell obligations; no raw-PID/restart-safety claim.
Existing core SPARK162 and authority-wire18 pass. Collection168/store97/managed68,
wire135222 and full Config IPC/CCL client regressions pass (session17419 done0).
Activation proof12617 and final tests90681 done0; earlier57202 unused-use warning,
78503 Ada Old legality fixed;46310 first proof pass. Native Config compile/link
and activation unit compile5831 done0, shared lock released. No Ghost predicate
symbols in native activation object. No jobs remain. No native assertions, Assume
or SPARK-Off; no ISO/QEMU run this slice. No kernel/runtime/network/desktop/
system.ccl/shared build script edits, no commits/pushes. Documentation updated.

HANDOFF (2026-09-26): durable Config classification complete; shared lock released.
Private database format4 adds immutable management column. Old formats explicitly
rejected, no implicit migration/reseed. Ordinary Create cannot downgrade a managed
registration; ordinary Commit checks class within transaction. Worker metadata
reply carries managed recovery through Ada adapter/channel into service Register.
Definite classification denials do not retire worker; I/O uncertainty still does.
No public managed-registration/write/activation API. Trusted backend registration
used only by installation fixtures; initial namespace reservation/source-bound
activation and appearance migration remain next. Protected DB integrity remains
an assumption, not offline-tampering prevention. Cached Gets unchanged; one added
indexed class lookup per durable commit, latency impact not yet measured.
Tests: Rust51; real-Turso hosted native-service4651 incl two cold managed lifetimes,
schema306 incl three database lifetimes; independent SQLite exact metadata/history
pass. Protocol/executor SPARK50 (4 functional), none unproved/justified. Channel
42/86/type680 and catalog168/publication97/managed58 regressions pass. Core prior
SPARK162 remains unchanged. All Nix. Hosted artifacts:
tests/ccl-objects/build/artifacts/config-publication.BUSdFt.
Native Config+worker build50667 done0. Native Workbench81672 done0: editor/compiler/
IPC two writes, fresh reboot read42, independent SQLite/WAL/ext2 checks PASS on
fresh disk; normal ISO rebuilt125188 sectors. Artifacts/log under
tests/config-workbench/build/artifacts/managed-v4.vyNZhz. This QEMU test exercises
ordinary app-state format4; managed enforcement is hosted-tested, not QEMU-tested.
Other sessions61496/18154/20964/94514/75284/72598 done0;22713 test compile error fixed
by75284. No jobs remain. No user disk edits, commits or pushes. No kernel/network/
runtime/desktop/shared Makefile source edits. Native builds held shared lock;
source not edited during native run. Cross-language oracles updated for format4.

HANDOFF (2026-09-26): Config collection classification/enforcement complete.
Changed config_collections.ads/adb, config_typed_store.ads/adb, receiver/service
management-conflict handling and focused collection/IPC tests. Trusted
registration selects application-state vs declaration-managed; class immutable,
ordinary Open/Resolve/Set deny managed writes, even wildcard grants. Re-Create
returns Denied rather than schema mismatch or retiring storage. No client class
field, activation bypass, or wire ABI change. New managed58 + existing168/97
hosted checks pass; core SPARK162 (10 functional), none unproved/justified.
IPC client926/dispatch143/receiver5149/startup20 pass. Sessions40001/35361 done0.
Native config.svc compiles/links in Nix under shared lock, session49432 done0;
no ISO staging or QEMU run. Lock released, no jobs remain. Native build retains
existing redundant-use/array-literal warnings; no warnings suppressed.
No actual collection boot-registered managed yet: trusted classification must
be restored/reserved before client admission; current persisted schema doesn't
carry class. Scalar settings unchanged. Next source/revision-bound activation
and durable classification, NOT independent scalar persistence. Docs updated.
No system.ccl/desktop/kernel/network/runtime/shared-buildfile edits.

TEMP CLEANUP (2026-09-26, user authorized): moved four completed, lsof-checked
inactive directories out of /tmp into ignored tests/config-turso/build/artifacts/:
cubit-turso-async-final-20260924, cubit-config-worker-native-20260924,
cubit-schema-reboot-final.S8vTZD, cubit-read-open-reboot.sDhLcv (about1.8GiB).
The earlier sandbox-quota recovery also moved cubit-turso-reboot.lSNone there
(about596MiB). Historical /tmp artifact references below now resolve beneath
that workspace directory. All contents preserved; nothing permanently deleted.
No other agent files, Nix store files, or active processes touched.

HANDOFF (2026-09-26): desired/active/application-state Config separation.
Added docs/config-declarative-state.md; updated Config contexts, integration,
boot-config and bootstrap-storage docs to remove conflicting source-of-truth
claims. No longer planning unconditional durable scalar writes: CCL owns intent,
Turso stores selected realizations and separately classified application state.
New CCL.Configurations.Changes and isolated Linux ccl-config-review tool/tests
compare evaluated system plans read-only; no activation/authority claim. Hosted
seven test groups including150 randomized independent-model cases pass. Focused
SPARK11 obligations (8 runtime/1 initialization/2 termination), none unproved or
justified; no semantic proof claim. Report userspace/ccl/build/config-review/
gnatprove/gnatprove.out. Build90199/test+proof20542 finished0; earlier test-only
attempts caught fixture source-text limits, fixed to construct large values with
compact CCL expressions. No jobs remain. No edits to system.ccl, desktop,
kernel/runtime, networking, shared Makefiles or ISO staging; no lock needed.
Next: one managed collection's revision/source correspondence, authorized
activation and recovery, NOT a second independent scalar persistence channel.

HANDOFF (2026-09-26): Config grant-decoder hardening complete; releasing lock66734.
New config_authority_wire.ads/adb replaces main.adb's counted-rule parsing loop;
Config_Authority exposes Empty_Rules. Pure decoder18 SPARK checks (one functional
failure-output contract), none unproved/justified,134962 hosted checks PASS.
Scalar suites and typed collection158 proof pass; native config-inspection PASS
(malformed launch policy and denied clients included). Normal ISO restored,
125172 sectors. Sessions12552/6321/6773/15621 terminal0, no jobs remain. No
procmgr/devmgr/kernel ABI edits. Separate zero-count bootstrap wildcard path
unchanged and explicitly documented, not claimed scoped-policy complete.
Logs tests/config-inspection/build/authority-wire-{proof,regressions,native,
restore}.log. All build/test commands Nix; task-local TMPDIR points to
tests/config-inspection/build/tmp, no system-wide Nix configuration changed.
Moved our stopped /tmp/cubit-config-desktop.oJb2Sw artifacts to ignored
tests/config-workbench/build/artifacts/cubit-config-desktop.oJb2Sw to free /tmp
quota (sandbox mount failed). All artifacts preserved, no other agent files
or user disks removed. New tests use workspace build/artifacts, not /tmp images.
No commits/pushes. Config sources released; ordinary Config follow-up still
needs instance/restart identity coordination and durable scalar settings.

HANDOFF (2026-09-26): ordinary desktop Config integration verified. Releasing
lock28916; build/test sessions59653/95969/94342/79129/77468 all terminal0.
Makefile/init-desktop-session handoff RELEASED: networking may resume edits.
Added config-storage target and stage-2 build membership, worker startup after
clock, Config workspace samples and shared verified scratch staging for three
desktop launchers. Fast/inspect now overlay current Workbench/worker too.
Preserved Servo disk/fonts/page and 4 KiB builder changes, no base/browser disk
replacement. Native --apps-test uses /tmp/cubit-config-desktop.oJb2Sw/apps/disk.img;
real Apps/procmgr launch, two revisions, reboot/guest read42 assertion, independent
SQLite/WAL/ext2 checks PASS. Normal ISO rebuilt via make iso; no test GRUB edits.
Hosted staging15 checks (1/4 KiB and85 MiB); authority collections168/publication97,
channels42/86/type680, worker4573/602 and scalar inspection suites PASS. Focused
authority/collection/store SPARK158 (10 functional contracts), no unproved or
justified; three existing unused-ID/specialized-branch warnings. No new Ada/code
proof contracts. Audit docs/config-security-audit.md records raw-PID/restart gaps,
registry-role administrative shortcut and scalar settings still in-memory.
Logs /tmp/cubit-config-{desktop-build-r1,desktop-staging-r2,desktop-native-r1,
authority-regression-r1,inspection-audit-r1}.log. No commits/pushes.
Normal launchers intentionally RESET scratch on invocation; persistent demo
--reuse remains available. REPL/remote integration, durable defaults and scoped
issuer/instance identities remain follow-up work, not claimed complete.

Completed scope (2026-09-25, security-design follow-up): documentation only in
docs/security-model.md, docs/security-vocabulary.md,
docs/security-model-verification.md, docs/security-hardening.md and
docs/authority-policy-roadmap.md. Consolidating vocabulary, mandatory authority
rules, storage/activation separation and explicit model-proof obligations.
No kernel/runtime edits, native builds or proofs in this slice. Read networking's
2026-09-26 ACK for Makefile/startup handoff and its 4 KiB disk request; both are
follow-up work, not changes made by this documentation slice. Existing ownership
and pending Config integration below remain in place. Core vocabulary is
authority/handle/grant/policy, with existing code representations mapped rather
than renamed. Added SEC-021 and formal attack/invariant/Config evidence plan;
no Lean implementation or proof claim. Local links and scoped whitespace checks
pass. Repository-wide diff check finds an unrelated trailing blank line in
tests/network-authority/README.md; left untouched. No jobs remain from this slice.

Documentation-only follow-up: added SEC-020 (bootstrap authority/delegation
audit, bounded issuance, bootstrap retirement, grant-chain visibility and
non-amplification tests/proofs) plus a development-backlog link at user request.
No implementation changes or builds for this follow-up.

WAITING FOR HANDOFF (2026-09-25): integrate Config storage in ordinary desktop
launchers, not interpreter/REPL yet. REQUEST narrow handoff for kernel/Makefile
(new config-storage target, stage-2 membership, fast-launch Config/Workbench
payloads and samples) and tests/headless/init-desktop-session.ccl (worker role).
Network targets/sources remain yours. Awaiting ACK before editing those shared
files; meanwhile own independent tools/prepare_desktop_disk.py, its tests and
Config documentation. Normal launchers currently reset desktop_disk.img, so
must not present that behavior as durable across launcher invocations.
While waiting: also own CCL.Diagnostics (VM labels) and Workbench's diagnostic
formatting. Hosted-only tests/build outputs; no native/shared build starts.
Lock holder453725 was networking's network-authority/Servo background sequence;
not interrupted and no shared lock acquired by this turn. No native builds or
shared launch-file edits. No commands remain. Desktop scratch staging helper
passes7 real-ext2 tests, including exit-zero write failure, same-size corruption,
and failed final fsck; source disk/previous output preserved. CCL diagnostics
pass72 hosted checks; Linux preview builds; focused proof has3 termination
checks only (not a UI proof), no unproved. Logs:
/tmp/cubit-desktop-config-staging-r2.log,
/tmp/cubit-config-workbench-readable-status-r3.log,
/tmp/cubit-config-workbench-diagnostics-proof.log.

Next after ACK/lock: stage config-storage.svc, add startup role after clock,
include the two Config samples in the disk workspace, replace raw debugfs
overlays with the tested helper, include current ccl-workbench.app in fast
staging (currently omitted), and budget desktop guest RAM for the worker.
Test launching Workbench through Apps as well as the direct-start fixture.
Keep normal launch reset semantics explicit; persistence across QEMU reboots
is different from recreating desktop_disk.img on each make invocation.

HANDOFF (2026-09-25): native async Config Workbench integration complete.
Releasing shared lock28466; no builds/tests remain. New tests/config-workbench
runner uses a disposable disk, never edits tests/headless/run.sh or GRUB.
Native62505 PASS: keyboard -> editor -> compiler -> Config IPC -> Turso; two
writes and reboot/read, independent SQLite/WAL/ext2 oracle and screenshots.
Extra28698 PASS: guest CCL asserts recovered value42, without payload logging.
Hosted57566:378 runner/sample checks; full client26138 PASS; preview66200 PASS
including six unsaved-edit paths; proof99652 native-object96,64394 tracker19,
none unproved. Logs /tmp/cubit-config-workbench-{native-r4,read-assertion-fixed,
samples-r2,hosted,proof-preview,final-hosted,preview-regression}.log.
Normal ISO115242 sectors retained; test startup is only on disposable disk.
Scope new execution adapters/lib/config runner, shared UI.App async input wait,
Workbench body/manifest/GPR, samples/catalog and system.ccl database bootstrap.
No kernel/network/Rust source edits, commits or pushes. Native Config binding
is explicitly bootstrap-approved; live discovery/interpreted REPL/remote shell
remain follow-ons. Runner/GUI coordinator is regression-tested, not proven by
the VM/tracker proof totals. Shortened demo type to CounterRead after catching
the lexer whole-token32 limit; sample compiler regression now covers it.

HANDOFF (2026-09-25): Workbench native-object debugger migration and shared
Config_Object_Interfaces descriptor builder complete. New source in lib/config,
test interfaces.gpr/interface_tests.adb and run.sh integration. No networking/
kernel/Rust edits. Hosted descriptor55, native-object253, owned receiver372
checks PASS. Native object proof96 no unproved; descriptor proof11 initialization/
termination checks only. Linux Workbench offscreen tests and QEMU Workbench
smoke PASS. Normal ISO restored,114764 sectors. Jobs69879/28758/8956/31108
terminal0;79753 terminal0 after successful ISO restore. Lock98344 released.
Logs /tmp/cubit-workbench-{native-inspection,config-descriptors-r2,interface-proof,
object-native-build,object-smoke}.log. Next: event-loop-owned Config context and
native completion dispatch with program/registry/collection pinning until drain.
New descriptors deliberately NOT exposed as runnable Workbench operations yet.
Future remote host uses same authorized catalog; no remote integration claimed.

2026-09-25 documentation follow-up: added a code-organization TODO in
docs/development-backlog.md to separate the public CuBit runtime from GNAT
internals. No source migration, builds or proofs for this documentation change.
Shared build lock session44353 has been released.

HANDOFF (2026-09-25): common IPC extraction complete; all jobs terminal.
Scope new
Config_Object_Client.Resources.Calls adapter and hosted tests; existing resource
client private internals read only. No kernel/network/std changes. No native
build active. Workbench still requires native-object machine/async dispatch;
streams are explicitly outside the Config completion milestone.
Shared runtime follow-up: claiming new CuBit.Async_Requests SPARK state machine
and integration in Config_Object_Client and Storage_Channel; focused hosted
tests and their explicit GPR closures. CCL.Host_Values scalar schema linkage
fix needed by the pending Config typed receiver test. No kernel/network edits.
Lock session44353 releasing after final native build65273 (terminal0).
Hosted87845 terminal0: async155, storage43+26, full Config suites including
new typed receiver372. Focused proof31781 terminal0: CCL347 no unproved;
final common-runtime proof68394 terminal0:19 checks (5 transition contracts),
none unproved/justified. Native11935 failed only GNAT runtime style checks;
source now conforms (no suppression), rerun28155 terminal0. Logs
/tmp/cubit-common-ipc-{checks,proof,final-proof,native-r2}.log.
UPDATE: native28155 terminal0: writer and fresh-boot reopen PASS, independent
SQLite/WAL/ext2 checks PASS both boots. Normal native Workbench/devmgr/ISO
rebuild65273 terminal0; ISO114593 sectors; log common-ipc-desktop-build.log.
Common runtime request tracker is used by both Config and Storage_Channel;
CCL language ownership stays generic in CCL.Resources/Imports/VM. New typed
Config receiver Calls adapter passes372 hosted checks; not yet Workbench-wired.
Linkage bug fixed: schema-wrapped scalar receiver arguments must recover type
from compiled value kind (their nominal data-type field is intentionally zero).
No commits/pushes. Next: Workbench native-object machine + async typed one-shot
Config dispatch. Generic source factories/streams are separate follow-ons.

SOURCE RECEIVER HANDOFF (2026-09-25): all jobs terminal; releasing55559.
Generic resource receiver + separate typed data signatures/parser/lowering/linking,
BASIC/Lisp round-trip tests and shared REPL signature formatting. Shared sources:
CCL Host_Values, Catalog, Language, Compiler, REPL view; native Config fixture.
No kernel/network/std changes. Initially waited for verified live Servo
QEMU3594033/flock3592187 without interfering, then held55559 through edits/builds.
42419 terminal0:75 signature/277 lowering checks and four-unit SPARK proof;
15235 terminal0 native writer/reopen + independent SQLite/WAL/ext2 oracle;
42743 terminal0 full Config/types/discovery/resource policies/host core.
Logs /tmp/cubit-source-receiver-{proof,native,regressions}.log.
One initial hosted failure was a test missing its prior tick grant; fixed,
26578 terminal0. Source receiver has nominal Receiver_Resource; Parameters
counts data only. Direct interpreter/portable resource CCLB remain rejected.
Native receiver fixture now compiles source; production generic factory pending.
Proof42419 terminal0:340 checks none unproved/justified (compiler, catalog,
host values, object conversion). Native15235 terminal0: writer/reopen + independent
SQLite/WAL/ext2 checks PASS; normal ISO114447 sectors.19810 terminal0:callback/view
regressions PASS. REPL completion summary and popup now share Argument_Types;
80876 terminal0 native Workbench rebuild/smoke/normalISO,114453 sectors.
NEXT: generic compile-time type-argument specialization and approved parameterized
resource/signature metadata for Config.create(type), then production Workbench
resource pool/dispatch. Do not special-case Config in parser/compiler, leak raw
handles or make ordinary Config values linear. Portable encoding/direct
interpreter resource calls still intentionally unsupported. No commits/pushes.

TYPED STREAM METADATA (2026-09-25): added a generic, trusted unary-resource
specializer in `CCL.Types` and `CCL.Catalog`. It can publish nominal families
such as `ConfigCollection<Preferences>` and `Stream<Preferences>` (internal
identifier spelling `ConfigCollection-Preferences` / `Stream-Preferences`),
with the supplied non-unrestricted ownership policy and no operation, handle,
grant, endpoint or transport side effect. Focused policy suite PASS: 1,576
checks; SPARK `ccl-types`, `ccl-resource_policies`, `ccl-catalog` PASS with no
unproved/justified checks (proof summary in the policy build directory).
Resource signature regressions PASS: 75 + 277 checks. `Stream<Array<T,N>>`
remains a deliberate next step: CCL has no bounded-array data schema or
canonical batch codec yet, so no fake array syntax or wire behavior was added.
Wire values will use the existing canonical CCL-object CBOR codec, never a
local object image. No native build, shared runner edit, commit or push.

RESOURCE LOWERING HANDOFF (2026-09-25): lock32625 released; all jobs terminal. Shared CCL Analysis,
Compiler, Host_Values and Catalog source edits for approved resource ownership
lowering and atomic linking. New/extended hosted tests. No kernel/std/network.
Portable resource encoding remains rejected until explicitly supported.
Update: original native50751 terminal0, writer/reopen + independent SQLite/WAL/
ext2 PASS, normal ISO114390 sectors. Compiler Read_Node centralizes checked AST
access; initial proof43777 failed three child-reference casts, corrected proof
57566 terminal0:308 checks, none unproved/justified. Host23142 terminal0:55+134.
Full Config/types/discovery63088 passed, then command failed on nonexistent
make target for core; corrected hosted core17756 terminal0. Final native10306
terminal0: write/reopen + independent SQLite/WAL/ext2 PASS against accessor fix;
normal desktop ISO restored,114392 sectors. No native builds/tests remain live.
Updated suite --prove includes compiler; combined four-unit proof88100 terminal0:
337 checks, none unproved/justified. Log /tmp/cubit-resource-lowering-combined-proof.log.
NEXT: generic resource receiver plus separately typed data source arguments,
then factory type-argument specialization and Workbench resource dispatch.
Native resource fixture uses compiled source, not a hand-built VM program.
No commits/pushes; goal remains incomplete.
Logs /tmp/cubit-resource-lowering-{proof-r2,host-r3,final-regressions,final-core,
final-native}.log. No public generic Config.create claim or parser proof claim.

RESOURCE SIGNATURES HANDOFF (2026-09-25): all jobs terminal; releasing83408. Other live
test observed under flock3518772/QEMU3520521; waited without interference.
Scope shared CCL host-value resource signatures, source type checking and
fail-closed interpreter/bytecode admission; object persistence rejects host
resource refs. New tests/ccl-resource-signatures. Small REPL type label update.
No kernel/std/network edits. Public Config.create remains pending.
Focused host tests55 PASS; proof78073 terminal0 (176 checks, no unproved or
justified). Regressions19950 terminal0 (full Config/type discovery/resource
policies); hosted core74201 terminal0. Native Workbench/devmgr/normalISO build
95349 terminal0 (114358 sectors). Live ccl-workbench smoke+restore17193 terminal0.
Logs /tmp/cubit-resource-signatures-{proof,regressions,core,native-build,workbench}.log.
No runtime source resource admission: interpreter preflight rejects before all
effects, compiler/portable contract still rejects resource imports. Source
analysis now enforces nominal resource argument/result types from approved
catalog metadata. No source-level borrow-flow proof claimed.
NEXT: preserve approved policy snapshot through Analysis, lower resource locals
and imports into VM ownership tags, and validate program/tag/type correspondence
against the trusted catalog during linkage. Keep portable resources rejected
until their encoding includes the required metadata. Then receiver+data source
calls and generic type-argument specialization; use the shared Config resource
client, not Workbench-specific raw handles. No commits/pushes.

RESOURCE POLICIES HANDOFF (2026-09-25): all jobs terminal; releasing75730. Added
generic approved resource ownership/disposition metadata beside Catalog's
nominal registry, bounded compiler layout of ownership tags/transition closure,
and tests. This is prerequisite for source resource factories: bytecode must not
choose its own weaker ownership policy. Shared CCL Catalog sources and explicit
source-list additions in vm_client.gpr/devmgr.gpr only. No kernel/std/network.
Initial lock contention verified live QEMU PID3485316 under flock3483576;
no interference. New files prepared while it ran; shared edits only after lock.
Focused hosted tests PASS1568; proof17859 terminal0, 118 checks, no
unproved/justified (one unused returned target-ref flow warning). Discovery/
schema regressions72185 terminal0. Full Config/resource suites passed; the
original combined regression command ended on a mistyped discovery script
path, corrected by72185. Native writer/reopen/normalISO chain7201 terminal0;
independent SQLite/WAL/ext2 checks PASS both boots; normal ISO114259 sectors.
Hosted core57405 terminal0. Logs /tmp/cubit-resource-policies-{proof,regressions,
discovery,native,core}.log. No source-factory/portable-admission claim.
New tests README and unified-language roadmap record proof scope and ordinary
copyable Config values versus owned handles. Native receiver fixture already
exports its final snapshot after closing the handle; documented that coverage.
NEXT: generic approved resource-return/receiver signatures, source type
specialization + owned local lowering and portable linkage; then connect the
existing resource-bound Config client to Workbench's async dispatcher. Do not
add Config-specific parser forms or make Config data itself linear. Multiple
collections may share a value type; namespace/context remains explicit and
subject to existing authority. No commits/pushes; goal remains unfinished.

RESOURCE CLIENT HANDOFF (2026-09-25): all jobs terminal; releasing5474. New
Config_Object_Client.Resources child + hosted tests. Shared changes: registry
pinned-type accessor/Retire_Lease and private parent Start declaration for close
after a failed data operation. Client rearm is confined to child's confirmed
close/grant retirement/lease reclamation path (token floor preserved).
Host object will own client/reference association, nonblocking acquisition/use/
stop/drain/cleanup, reject cross-client refs, and quarantine uncertain unknown
acquisitions rather than silently reclaim them. No kernel/std/network scope.
Native38970 terminal0: receiver fixture now uses shared resource-bound client;
writer/reopen/independent SQLite/WAL/ext2 PASS, Workbench+normal ISO restored
(114216 sectors). Hosted58451 terminal0: resource client1170 + full Config suite.
Registry71534 terminal0:38206 + resource VM58 + receiver67. Proof32713 terminal0:
CCL.Resources72, none unproved/justified. IPC resource-client shell is tested,
not SPARK-proved. Logs /tmp/cubit-resource-client-{native,host,registry-tests,
proof}.log. Initial46465 terminal0 (177 prior to multicoll/reuse tests).
New resource_client.gpr/run.sh integration; no headless runner edits this round.
Collection owns its backend client, lease and ref; exact-ref checks, typed
factory admission, async completion drain, stop/retire, close, confirmed grant
retirement/reclaim, token-floor-preserving reuse. Unknown acquisition handles
and uncertain closes quarantine (no invented success); service-lifetime
reconciliation still future. Distinct contexts, process-unique tokens, stable
storage, serialized access and authenticated receipts remain host duties.
NEXT: generic approved resource signatures and source factory/owned local
lowering, then connect reusable collection objects to Workbench dispatch.
CCL_Config_Bindings still inspector-only. No source Config.create claim, no
durable-default or performance completion. No commits/pushes.

RECEIVER/DATA IMPORT HANDOFF (2026-09-25): all jobs terminal; releasing78581.
Final native/reopen/normalISO chain82164 terminal0; initial native40448 terminal0
with new receiver-vm marker and independent SQLite/WAL/ext2 PASS. Final hosted
72007 terminal0 (receiver67, resource58, full Config + owned1182 + discovery).
Core26696 terminal0. Final six-unit proof91355 terminal0:433 checks, no unproved
or justified. Capacity regression exposed offered-borrow/waiting mismatch;
fixed by rejecting unsubmitted offer before terminal exhaustion. No new guards
or assumptions. Final logs /tmp/cubit-receiver-{final-native,final-host,core,
exhaustion-proof}.log. Native writer + fresh-boot recovery + independent SQLite/
WAL/ext2 PASS; Workbench rebuilt, normal ISO restored (114213 sectors).
Added generic resource receiver + separate typed data argument to VM imports,
native-object wrapper and focused tests. Shared CCL source/build closure changes;
no kernel/std/network edits. Existing Config dispatch will reject receiver calls
until it can pin an authenticated receiver/client association. Source factory
and portable resource-signature support remain subsequent work. No commits/pushes.
Native receiver_fixture supplies the second nested write (same two-revision
oracle), validates one stable client/ref pairing and actual acquire/read/write/
readback/close; no source-factory claim. Headless requires receiver-vm marker.
Existing Config VM.Calls rejects Request_Owned before effects/result consumption.
Next: production resource host association/lifetime and approved resource
signatures + source factory lowering. Keep actual service handles private; don't
silently route a receiver-bearing request through a fixed client. Workbench
currently exposes inspector bindings only; typed collection pool/source API,
durable defaults and FS/Turso performance remain unfinished.

OPAQUE RESOURCE VALUE HANDOFF (2026-09-25): all jobs terminal. Native52510
terminal0: actual Config acquire/read/close via opaque VM refs PASS, all native
typed Config markers and independent SQLite/WAL/ext2 PASS; Workbench/normalISO
restored. Hosted41535 terminal0: resource58, client-resource29, full Config and
owned-local1182 PASS. Resource lifetime38206 and broader discovery/core passed.
Proof90565 terminal0: VM/resource bridge/registry/conversion298; compatibility
proof44285 terminal0: native-object wrapper/host-values126; none unproved/justified.
From_VM now proves no resource values or hidden reference metadata persist.
Well_Typed is the same logic as a visible expression function, not new guards.
Earlier proof aliasing rename diagnostic fixed with a constant; both exploratory
proofs were terminated before the final uniquely named run. Logs
/tmp/cubit-resource-values-{proof-final,compat-proof,final-host,native}.log.
Scope CCL resource kind/private completion bridge, import result ownership tag,
nominal registry correspondence, reject-resource persistence/admission, lifecycle
reuse after completed owned calls. CCLB7 replaces6; portable resource imports
remain rejected pending approved resource signatures. No source factory claim.
Native fixture uses a trusted in-memory program. tests/headless/run.sh now
requires config-objects-resource-vm (same marker checked explicitly in52510).
Updated devmgr.gpr CCL source closure only (resources spec/body); no kernel/std/
network/FS ABI edits. Read Servo/ext2 large-file note; triple-indirect reads are
still a separate FS task. No commits/pushes. Releasing lock1740.
NEXT: generic approved resource-return signatures and source static type
arguments; receiver-plus-data imports for collection.set(value) and aggregate
get outcomes. Do not serialize refs or smuggle them into Objects.Binding. Host
receipt/run association and cleanup remain obligations, not proved here.

OWNED LOCAL TRANSFER HANDOFF (2026-09-25): all jobs terminal. Final97779
terminal0: 1182 hosted checks, including CCLB encode/decode/re-admission across
all32 ownership tags and all3 modes. Proof94418 terminal0: 212 checks in VM and
ownership bytecode verifier, none unproved/justified. Broad CCL/Config99886 and
core55324 terminal0. Native29346 terminal0: typed Config guest markers and
independent SQLite/WAL/ext2 PASS; Workbench rebuilt and normal ISO restored.
Logs /tmp/cubit-owned-locals-{final,proof,regressions,initial,native}.log.
Generic VM transfer path now allows dynamic move-only/must-handle locals from
exactly tagged moved operands; no literal-to-owned laundering. Stack copy/drop
and joins preserve ownership tags; Halt rejects moved operands below its return.
Also fixed owned-argument import result stack accounting (previously omitted).
Scope ccl-vm.adb, ccl-ownership-bytecode.adb, tests/ccl-owned-locals, CCL README
and roadmap. No kernel/std/network changes, no commits/pushes. No source factory
or opaque resource-valued host import yet; this is operand/lifetime plumbing,
not a raw-integer Config handle API. Proof is selected-unit safety/contracts,
not full verifier semantic soundness. Releasing lock94030 now.
Next: generic approved nominal resource-return contracts + opaque host/VM values,
then source static type arguments/factory lowering and Config collection binding.
Persistence bindings must continue to reject resources/containing aggregates.

ACQUISITION REPLY CLEANUP HANDOFF (2026-09-25): all jobs terminal. Native58912
terminal0: actual Open/Create + compiled typed reads/native writes PASS,
independent SQLite/WAL/ext2 PASS; normal desktop ISO restored. Hosted final60830
terminal0 (receiver5134 + full client suite); Turso99302 terminal0 (4484,
independent exact-two-revision oracle); proof69166 terminal0 (158 checks).
Source edits complete, releasing lock89497 for queued networking libc/std test.
No commits/pushes. Next is generic owned resource returns + source/VM factory
integration; the native unknown-handle concern does NOT require inventing a
session protocol for kernel-reported failed delivery. Delivered completions
MUST still be drained by the host; no network acknowledgement guarantee claimed.
Logs /tmp/cubit-config-acquisition-{final,turso,cleanup}.log and
/tmp/cubit-acquisition-cleanup-native.log. Kernel
replyCap/completeReplyLocked returns1 only on delivery,0 on consumed failed
reply; Config ignored it after Open/Create handle minting. Fixing receiver to
close ONLY newly minted undelivered handles, preserving data/durable revisions.
Scope config_object_receiver.adb/ads, receiver_tests, typed_store_turso fixture,
collection Close proof contracts/tests if useful. Kernel read-only; no std/network
edits. Hosted send mocks currently incorrectly use0 for success; correcting to
actual kernel1/0. This can address kernel-reported non-delivery without a new
session protocol. Delivered completions still require host draining/cleanup.
ACK networking /tmp cleanup; noted queued libc/std test, will release build lock
after scoped validation. No other shared source edits planned.
Hosted full Config suite PASS; receiver now exercises direct/cached/deferred
Open + deferred Create non-delivery (>table capacity each). Real Turso4484 and
independent exact-two-revisions SQLite PASS. Collections/Authority/TypedStore
proof158, none unproved/justified; new Close post preserves Published values.
Native writer+normalISO completed as recorded above.

RESOURCE LIFETIME HANDOFF (2026-09-25): all jobs terminal. Final85939 terminal0:
CCL.Resources hosted38206 + counter boundaries, SPARK65 (9 functional contracts,
none unproved/justified), Config resource integration135 + full client suite PASS.
New CCL.Resources registry: typed reserve/publish, opaque run refs, single-flight
uses, nonreusing tickets, stop/drain/host-cleanup/reclaim. Reclaim takes a lease
ticket, not a bare slot, preventing stale cleanup from reclaiming reused backing.
Host supplies distinct context lifetimes and serialized access; external cleanup
and grant retirement remain host obligations. Tested with real client, modeled
IPC; NOT new native QEMU evidence, source factory or production reference pool.
Scope new ccl-resources.ads/adb, tests/ccl-resources, config-object-client
resources.gpr/resource_tests/run.sh/README, roadmap docs. No kernel/std/network
edits; no ISO rebuild needed (new package not yet linked into apps). Normal
desktop ISO from previous handoff preserved. No commits/pushes. Releasing lock8030.
Logs /tmp/cubit-resource-lifetime-final.log; proof tests/ccl-resources/build/gnatprove.
Next: generic resource-bearing host returns/outcomes, source+VM lifetime rules,
Config factory binding. IMPORTANT: uncertain Create may mint an unknown remote
handle; local grant retirement is insufficient for reusable pooling. Need
service-side client/session cleanup or authenticated reconciliation before
claiming that case safe. Normal post-Stop received-handle close is tested.

RESOURCE METADATA HANDOFF (2026-09-25): all jobs terminal, source changes
complete. Reboot10380 terminal0: fresh CuBit typed recovery, independent
SQLite/WAL/ext2 PASS; Workbench rebuilt and normal desktop ISO restored.
Releasing lock shell20632 now. Six-unit Types/Encoding/Correspondence/Objects/Schemas/Views SPARK
281 checks and portable schema codec SPARK112, none unproved/justified. Hosted
resource62, codec29504, broad Config/discovery/nativeVM240 PASS. Real Turso
recovery472 and schema230 with independent SQLite PASS. Receipt-loss fixture
had stale Unavailable expectations; updated to existing Uncertain contract,
still verifies exactly two durable revisions/no retry. Native writer28203
terminal0, independent SQLite/WAL/ext2 PASS. Reboot89658 terminal1 ONLY at optional
disk export (Errno122 quota); guest markers and preceding ext2/SQLite checks
passed; final successful repeat omitted only the optional export. Logs
/tmp/cubit-resource-types-{host,regressions,config,durable,proof,codec-proof,native,reopen-final}.log.
No kernel/std/network edits. No commits/pushes. Goal incomplete: Resource shape
is metadata only (named type parameters, no instances/grants). Current source
constructors and data VM locals/imports reject resources. Next: generic owned
factory results, source/VM lifetime rules and private host reference table,
then Config.create(type) binding and Workbench dispatch. Do not put resource
values in Objects.Binding or expose raw integer handles. Persistence encoders
share Root_Closure so unrelated visible resource types never enter saved data
schemas. Maximum-schema fixtures now use connected sums to preserve full32/16
bounds coverage after closure export. Stale real-Turso fixture expectations for
lost/staged write receipts were corrected to Uncertain, matching production and
existing focused tests; exact disk revision/no-retry oracle retained.

ACTIVE RESOURCE TYPE METADATA (2026-09-25): lock shell20632 held. Adding nominal
opaque resource descriptors to shared CCL.Types; type parameters are metadata,
not stored values, and resources/containing aggregates must fail persistence.
Scope Types/Encoding, Objects/Views/Schemas, focused types/schema tests/docs.
No resource runtime minting or public Config factory claim yet: existing owned
imports only operate on previously acquired bindings. Preparing that boundary
without unrestricted integer handles. Schema export will retain only the root
dependency closure, so unrelated resource declarations cannot poison data
schema persistence. No kernel/std/network edits; shared CCL sources changing.

NATIVE CONFIG DISPATCH HANDOFF (2026-09-25): source edits complete, all current
jobs terminal. Writer42197 terminal0; reboot/Workbench/normal ISO91535 terminal0,
both independent SQLite/WAL/ext2 checks PASS. Compiled native whole-object and
field/match reads now use shared asynchronous Submit/Resume (event loop owns
the wait); native interpreter writes unchanged. Native writes through the new
bridge are NOT claimed: hosted only. Native dispatch234, native VM240, broader
Config/discovery/core suites PASS (26173/90016 terminal0). VM proof97812 terminal0:
321 checks, zero unproved/justified/Assume; IPC client/dispatcher not part of
that proof. Initial unproved capacity check fixed by expressing existing
Accepts_Object_Result predicate directly, not adding guards or contracts.
Logs /tmp/cubit-native-config-dispatch-{host,proof,regressions,values,native,reopen}.log.
Scope VM.Native_Objects (pure pending-call/type preflight, scalar native image
completion), Config_Object_Client.VM.Calls overloads, hosted/native tests/docs.
No kernel/network/std edits; no commits/pushes. Releasing lock shell72478 now.
Next: source-level generic resource-returning Config.create(type) and typed
collection binding/lifetime, Workbench event dispatch/run correlation, then
durable defaults/performance. Existing client Create/Open/Close is asynchronous
and hides its raw handle, but not yet a CCL factory expression. Read the updated
docs/ccl-unified-documents-roadmap.md; do not substitute unrestricted integer
handles or Config-specific schema parser intrinsics. Full codec proof pending.
Previous ACTIVE entries below are superseded.

ACTIVE NATIVE CONFIG DISPATCH (2026-09-25): lock shell72478 held. Shared typed
client already implements asynchronous Create/Open/Close and holds private
collection handles. Connecting owned-object VM suspension to that client next:
native read/write submit/resume with result-schema preflight, no blocking host
callback, and lifetime/denial regressions. Scope CCL VM.Native_Objects,
Config_Object_Client.VM.Calls, associated hosted/native fixtures/docs. No kernel,
std or networking edits. Defer shared builds of those sources until handoff.

OBJECT PROJECTION HANDOFF (2026-09-25): All jobs terminal. Reboot+Workbench+
normal ISO restore58641 terminal0; headless config-objects-reopen and independent
SQLite/WAL/ext2 PASS. Hosted Config31117 terminal0; final source45903 terminal0
(378 checks, including read granted/write denied). Discovery/proof42596 terminal0;
native object124, all broader discovery tests PASS. Focused VM/native wrapper/
host_values SPARK318, zero unproved/justified/Assume (72 runtime,26 assertions,
21 contracts,159 initialization,4 non-aliasing,35 termination,1 dependency).
Actual Evaluate_View Global=>null fixes frontend generic-inlining assertion;
checked no-global-effects contract, not a suppression. Formal generic Global
aspect was unsupported and removed. Native proof now part of run.sh --prove.
Full codec proof remains incomplete due prior resource blowup. Logs
/tmp/cubit-object-projection-{reopen,proof,final-host,final-config}.log.
Releasing lock shell51695; source edits complete, no commits/pushes. No kernel,
std/network edits. Next: public Config.create(type) collection/resource lifecycle
and nonblocking Workbench dispatch, aggregate constructors/string bytecodes,
durable defaults/performance. Existing native read callback remains test-only
blocking, not production Workbench integration. Earlier ACTIVE entries below
are historical and superseded by this handoff.

OBJECT PROJECTION RESUMED (2026-09-25): lock shell51695 revalidated live.
Writer39134 is terminal (handle gone; log headless PASS config-objects and
independent SQLite/WAL/ext2 PASS). Hosted native object tests124 and broad CCL
regressions pass. Projection proof stopped with GNAT internal assertion
inline.adb:3290 at generic Evaluate_Native call, not a proof success.
Investigating explicit no-global-effects callback boundary, then native reboot
validation/normal ISO restore. Same CCL-only ownership; no kernel/std/network
edits. Shared CCL builds remain deferred while these sources change.

ACTIVE OBJECT PROJECTION (2026-09-25): lock shell51695 held. The codec proof
60729 was intentionally interrupted after its two gnatwhy3 workers reached
~28GiB RSS; parent terminal1 and exact worker PIDs confirmed gone. No codec
proof success claim. Prior VM/wrapper195 and all runtime tests remain valid.
Now extending native views into field/match VM operations with a private
generic storage callback (not host IPC), keeping scalar VM state small.
Scope VM/compiler/format/native_objects and associated tests/docs. No kernel,
std or network changes. Defer shared CCL builds while source edits are active.

OBJECT VM VALIDATION (2026-09-25): Writer6271 terminal0;
SQLite/WAL/ext2 PASS, compiled Config read included. Hosted coreVM and broad
Config/type/object suites PASS, objectVM55. Focused VM+wrapper SPARK195 PASS.
Reboot+Workbench build+normal ISO restore91538 terminal0, disk oracle PASS,
/tmp/cubit-object-vm-reopen.log. Only hosted codec/host-values proof60729 remains
live (-j2, disjoint object-codec-proof dir). Lock shell90186 exited, shared
builds may resume. Do not edit the claimed CCL sources during the proof.
60729 last poll remains live; host ps independently confirmed gnatwhy3 workers
consuming CPU (not a stale note or an observation timeout). Do not restart it.
Proof log /tmp/cubit-object-vm-codec-proof.log. Core proof report remains in
tests/ccl-type-discovery/build/native-objects/native-object-proof/gnatprove.
Scope CCL VM/compiler/
catalog/host-values/format, new out-of-line native VM object storage, Config
fixtures and focused hosted tests/docs, Workbench exhaustive Value_Text case.
No kernel/std/network edits. First
compiled path is typed object receive/retain/forward across async imports;
construction/projection instructions follow. Scalar machine storage stays small.
Source edits complete. No commits/pushes.

NATIVE STRINGS HANDOFF (2026-09-25): Hosted views177 /
source230 PASS; Views SPARK81 PASS (45 runtime,23 initialization,12 termination,
1 validated-output contract; no unproved/Assume). Native writer70286 terminal0,
SQLite/WAL/ext2 PASS. Reboot+normal ISO restore24074 terminal0, log
/tmp/cubit-native-strings-reopen.log; disk oracle PASS. Broader tests pass after updating obsolete
host-object expectation: valid >1KiB string fails scalar UI capacity, not host
schema validation. Scope Views/Language, Config tests/docs and that hosted test;
no VM/kernel/std/network changes. All jobs/proofs terminal; lock shell81339
exited successfully, build lock released. No commits/pushes.
Standalone strings now keep owned views; native returns/arguments and concat
support8KiB. Scalar UI/Text endpoints retain explicit limits. No new authority
or codec. Next: aggregate VM support and public Config collection lifecycle /
Workbench asynchronous dispatch, durable defaults and performance. Do not
mistake blocking native test callbacks for production Workbench integration.

TYPED RESULTS HANDOFF (2026-09-25): Interpret_Object[_With_Values]
implemented with separate owned result, full nominal expected type before effects
and validated export before teardown. Hosted source200 and broad object/type/
formatter PASS; Matches_Type focused SPARK flow passes2 checks (dependencies,
termination; no functional/runtime proof obligations). Writer78799 terminal0,
whole typed Missing/Found return and SQLite/WAL/ext2 PASS. Reboot+normal ISO restore
68465 terminal0, /tmp/cubit-native-result-reopen.log; disk oracle PASS.
Lock shell36349 exited: networking: lock free. No jobs/proofs remain.
Scope Objects helper, Language, tests/docs; no kernel/std/network/VM execution
changes. No commits/pushes. Not switching Rust std pending coordinated parity.
Next: standalone strings still materialize through the1KiB region even when
native objects admit8KiB; nested strings remain intact. Then aggregate bytecode,
public Config lifecycle/Workbench, durable defaults/performance. The old scalar
result/UI API remains separate; no claim Workbench can display aggregates yet.

CONSTRUCTORS HANDOFF (2026-09-25): jobs91542/26741 native writer+read-only reboot
and normal ISO restore terminal0. SQLite/WAL/ext2 oracle PASS; actual CCL builds
Preferences/Mode.Active before native Config Set (preserves5KiB supplied subtree),
second write retains full8KiB. Views162/source155 hosted PASS, broader types,
catalog/bytecode/formatter PASS; views SPARK65 PASS (34 runtime,19 initialization,
11 termination,1 validated-output contract). Interpreter not fully proved.
Logs /tmp/cubit-constructors-{native,reopen,proof,final-host,discovery}.log.
Note final-host stopped at an obsolete metadata expectation; discovery rerun
passed after distinguishing interpreter construction from unsupported final
aggregate export/bytecode. String/record blanket rejection tests replaced by
malformed/resource rejection; VM rejection tests retained. No jobs/proofs remain.
Lock shell39519 exited: networking: lock free. Scope general record/nested sum
constructors, private local snapshots without schema IDs, BASIC, tests/docs.
No kernel/std/network/VM execution/build-script edits. No commits/pushes.
Next aggregate result export/bytecode and public Config collection lifecycle /
Workbench, durable defaults and performance. Current scalar final-result API
still rejects returning aggregates directly; inspecting/sending works.

OBJECT ARGUMENTS HANDOFF (2026-09-25): shared Copy_Value plus interpreted aggregate
arguments implemented. Hosted view156/source96 and full Config/type-discovery
suites PASS. View SPARK63 PASS (34 runtime,17 initialization,11 termination,
1 validated-output postcondition). Native writer7857 terminal0: CCL passed the
full8KiB nested object to actual Config Set and matched Committed; SQLite/WAL/ext2
oracle PASS. Read-only reboot+normal ISO restore13895 terminal0, log
/tmp/cubit-object-arguments-reopen.log. Lock shell23679 exited: networking: lock free.
No jobs/proofs remain. Scope Views,
interpreter admission, hosted/native tests/docs only; no VM/kernel/std/network
or build-script edits. No commits/pushes. Next aggregate construction/bytecode
and public Config collection lifecycle/Workbench, then durable defaults and
performance. Native writes use a test-host-supplied aggregate, not a source
constructor; source projection read-to-write is covered by hosted tests.

OBJECT VIEWS HANDOFF (2026-09-25): native writer and independent read-only reboot
PASS, including actual interpreted CCL match/field over native Config IPC and
SQLite/WAL/ext2 validation. Normal desktop ISO restored. Lock shell58854 exited;
build lock FREE. No jobs/proofs remain. Hosted source84/view120 plus broader
type/catalog/formatter regressions PASS. Focused views SPARK53 PASS (28 runtime,
14 initialization,11 termination; no functional contracts). Interpreter tested,
not fully proved. Logs /tmp/cubit-object-read-source-{native,reopen}.log and
/tmp/cubit-object-views-final-host.log.
Scope new CCL.Objects.Views, interpreter field/matching, formatter/compiler
rejection, tests/native fixture; devmgr.gpr source closure only. No functional
devmgr/network/std/kernel/graphics or VM execution edits. Snapshot capacity16 is
reserved before host calls; owner-relative cursors are not capabilities. Broad
record-no-grant test now admits the type but still rejects before host execution.
NEXT: general aggregate bytecode/construction/export, public Config lifecycle /
Workbench async binding, durable defaults and performance. No commits/pushes.
Older scalar-only interpreter notes below are historical, superseded here.

TYPED READ HANDOFF (2026-09-25): jobs1456/58221/67307 terminal0; no jobs/proofs
remain. Lock shell25235 exited: build lock FREE. Native writer+independent
read-only reboot PASS, including new config-objects-read-outcome[-reopen] markers
and independent SQLite/WAL/ext2 checks; normal desktop ISO restored.
New Config_Read_Outcomes defines ordinary CCL result/snapshot metadata specialized
to approved value type, complete native value+revision; Host.Take_Read_Outcome
preserves poisoned clients, leaves mismatched bindings pending. Hosted768 new
checks PASS plus full client suite; focused constructor SPARK32 PASS (14 runtime,
1 validated-output postcondition,17 initialization/termination). No serializer,
new authority, compiler/VM/kernel/std/network/graphics edits. Shared headless
required markers edited under lock. Logs /tmp/cubit-config-read-{proof,native,reopen}.log.
Native/host path handles arbitrary admitted nested shapes; CCL interpreter/VM
still only execute scalar variants. NEXT: general aggregate execution/inspection
for nested result payloads, then public lifecycle/Workbench/durable defaults.
Do not call native result construction finished source-level Result<T> support.
No commits/pushes.

CREATE LIFETIME HANDOFF (2026-09-25): all jobs terminal0, no proofs/jobs remain;
lock shell74616 exited, build lock FREE. Native writer + independent read-only
reboot PASS with SQLite/WAL/ext2 validation; normal desktop ISO restored. Hosted
client926/dispatch132/receiver384/startup20/VM354/host45/calls189/outcomes450 PASS;
channel42/receiver-channel86/type-channel680 PASS. Full Turso48 tests PASS,
including lost Create receipt recovery, identical definition idempotence and
same-digest/different-definition rejection, no fabricated value revision.
Focused protocol/dispatch SPARK43 PASS; receiver/client remain regression tested.
Create pending loss now Uncertain, read-only Open remains Unavailable. Fresh
authorized Open recovers after retiring poisoned client; late success cannot
revive it. Handle-grant failure never implies rollback of persisted declaration.
Logs /tmp/cubit-config-create-{native,reopen,validation,turso-full}.log. No
kernel/std/network/graphics/compiler edits, no commits/pushes. Next typed read
outcomes must carry actual value/revision; nested Result<T> needs general VM
aggregate representation (current VM only scalar variants), not Config special
casing. Public CCL lifecycle, Workbench and durable defaults remain unfinished.

WRITE UNCERTAINTY HANDOFF (2026-09-25): no jobs/proofs remain. Native writer
and independent read-only reboot PASS, including SQLite/WAL/ext2 checks and
typed Denied write. Normal desktop ISO restored; lock shell4224 exited:
networking, build lock FREE. Hosted client906/dispatch132/receiver358/startup20/
VM354/host45/calls189/outcomes450 PASS; channel42/receiver-channel86/type-channel680
PASS. Six actual Turso worker tests PASS, including committed-but-lost receipt
recovery and stale replay rejection in MemoryIO. Protocol/dispatch SPARK43 and
pure outcome17 checks PASS, no unproved obligations in those focused runs.
Pending Set worker loss/invalid receipts now explicitly Uncertain, native client
poisoned, typed outcome retained; old handle cannot be retried. Not a proof of
the client or disk crash consistency; native fault injection remains separate.
Logs /tmp/cubit-config-uncertain-{native,reopen,validation,turso,terminal-proof}.log.
No kernel/std/network/graphics/compiler edits, no commits/pushes. Config remains
priority; next lifecycle/Create ambiguity, general typed read outcomes and
Workbench integration, then durable default settings/performance. Older notes
below are historical and their pending uncertainty issue is now resolved.

TYPED WRITE HANDOFF (2026-09-25): final writer50641 terminal0 + SQLite/WAL/ext2
oracle PASS; read-only56929 PASS including live ConfigWrite.Denied match and
unchanged reads. Normal ISO restored; shell42285 exited: networking: lock free.
Hosted outcomes418/calls176 PASS; pure outcome module SPARK17 checks PASS.
No jobs/proofs remain, no commits/pushes. NEXT PRIORITY: service-side uncertainty.
Audit found Config_Object_Dispatch.Finish recovery fallback and Lost outstanding
write reply Unavailable even if commit happened. Add explicit wire Uncertain,
preserve client poisoning and typed ConfigWrite.Uncertain, regression-test
pending worker loss/invalid receipt before proceeding to lifecycle/Workbench.
Current converter preserves statuses; it does not resolve this old ambiguity.

TYPED WRITE RESULTS VALIDATED (2026-09-25): normal CCL ConfigWrite variants
replace Boolean write receipts in VM.Calls and source fixture. Hosted418 outcome
checks and176 call checks PASS, full other client suites PASS. Outcome SPARK17
checks PASS (3 runtime,14 flow/termination; no full Config/codec proof claim).
Native writer + SQLite/WAL/ext2 PASS; independent read-only boot additionally
matches a real denied write in compiled CCL and reads unchanged data, PASS with
disk oracle. Final writer rerun after test Scenario enum refactor + ISO restore
running now (/tmp/cubit-config-write-outcome-final-native.log); lock shell42285.
No other source edits/builds/proofs running; no kernel/std/network changes.

ACTIVE TYPED WRITE OUTCOMES (2026-09-25): promoting Config write receipts to
normal CCL variants (Committed revision / explicit failure alternatives), via
userspace/lib/config metadata plus existing Catalog/VM types. Scope new outcome
schema module, VM.Calls adapter, hosted tests and native source/async fixtures.
No CCL compiler/VM format changes, kernel/std/network/graphics edits. Holding
build lock for native source updates/tests; no other work claimed.

CONFIG CALL ADAPTER COMPLETE (2026-09-25): native writer and independent
read-only KVM recovery PASS with shared Config_Object_Client.VM.Calls adapter;
SQLite/WAL/ext2 oracle PASS for both. Normal desktop ISO restored, session30359
terminal0. Shell43380 exited: networking: lock free. No jobs/proofs remain.
Hosted167 new call checks PASS; full client891/dispatch98/receiver358/startup20/
VM354/host45 suites PASS. run.sh now includes host and call suites. Regression
evidence only, no new SPARK proof claim. Source scope new lib/config child package,
tests/config-object-client and docs only; no kernel/std/network/graphics edits.
Logs /tmp/cubit-config-call-adapter-native.log, -reopen.log and
/tmp/cubit-config-vm-calls-final.log. Next public CCL typed lifecycle/outcomes and
Workbench dispatcher integration remain; this adapter is host-side, not Result<T>.
No commits/pushes. Goal remains active; default settings still not durable.

ACTIVE CONFIG CALL ADAPTER (2026-09-25): extracting the native fixture's
nonblocking get/set host-call bridge into Config_Object_Client.VM.Calls. Scope
userspace/lib/config, new hosted tests and native async fixture only. No shared
CCL/compiler/kernel/std/network edits. Public CCL handles/outcomes remain the
goal; this preserves service status/revision/uncertain completion for that layer.

HANDOFF (2026-09-25): no jobs/proofs/native tests remain; build lock FREE.
No-inline codec-field proof22382 explicitly interrupted (terminal1) after large
encoder expansion; not a proof pass. Native compiled CCL Config writer/reopen,
SQLite/WAL/ext2 checks and normal ISO restoration PASS. Portable1023 hosted
checks PASS; host-value/catalog133 proof checks PASS. Small admission helper
has only2 flow/termination checks, not a codec soundness theorem. Docs updated.
Next Config work: reusable public typed outcomes/handles and Workbench dispatch,
aggregate execution, durable default settings and performance. Consider splitting
fixed import-record codec out before another monolithic codec proof attempt.
No kernel/std/network/graphics edits, no commits/pushes. CCL source now idle.

CCLB5 NATIVE COMPLETE (2026-09-25): writer and independent read-only KVM boots
PASS, including independent SQLite/WAL/ext2 checks. Normal desktop ISO restored;
session80125 terminal0, shell30827 exited: build lock FREE. Only isolated hosted
codec-fields proof22382 remains (/tmp/cubit-cclb5-codec-fields.log); do not edit
shared CCL format source until it finishes. Native logs /tmp/cubit-cclb5-fixed.log
and /tmp/cubit-cclb5-reopen.log. No other native jobs, no commits/pushes.

CCLB5 VALIDATION (2026-09-25): writer KVM + independent SQLite/WAL/ext2 oracle
PASS (/tmp/cubit-cclb5-fixed.log). Independent read-only reboot and normal ISO
restore running as session80125 (/tmp/cubit-cclb5-reopen.log); build lock remains
held by shell30827. Hosted portable tests expanded to1023 checks PASS. Host-value/
catalog133 SPARK checks PASS. Region proof97538 interrupted due encoder inlining
memory expansion; not a pass. No-inline admission proof22285 PASS; selected new
codec-field regions now running. No kernel/std/network/graphics edits.

PORTABLE TYPED IMPORTS (2026-09-25): broad proof48469 deliberately interrupted
after format Encode/Decode expansion grew to ~28 GiB; terminal exit1, all its
workers gone. Not a broad codec PASS. VM174 and native/hosted tests remain valid.
Build lock now held by shell30827; other native run finished. Shared
scope: ccl-host_values, ccl-catalog, ccl-compiler, ccl-format for schema-pinned
portable typed imports. No builds/proofs active during edits; no kernel,
std, devmgr logic or Servo changes planned. Focused proofs after edits.

CONFIG ASYNC VM (2026-09-25): native KVM writer/read-only reopen PASS, including
two nominal Config reads resuming the same VM, SQLite/WAL/ext2 oracle and normal
ISO restoration. Shell29730 exited; build lock FREE. No native jobs. VM174
SPARK checks, hosted import159/source109/VM adapter354/host adapter45 and full
hosted VM/ownership suite PASS. Catalog/codec/host proof still running (session
48469, -j2, disjoint boundary-proof output; log /tmp/cubit-config-vm-portable-proof.log).
Last live check: gnatwhy3 processes2485766/2485848 active, no failures reported;
do not count this broad run as passed or edit shared sources until terminal.
Scope CCL VM/host/codec admission/tests, no kernel/std/network/graphics changes.
CCLB v4 rejects typed imports pending schema-pinned format/compiler work.

TYPED HOST OBJECTS (2026-09-25): native writer and independent read-only KVM
reader PASS with actual typed CCL get/set calls and independent SQLite/WAL/ext2
checks. Normal desktop ISO restored; shell40270 exited, build lock RELEASED.
Hosted nonblocking host adapter45 and VM adapter354 checks PASS. Extending
CCL.Host_Values/import contracts with owned native CCL.Objects images and
approved schema keys; catalog retains shared schema/type view. Scope shared
CCL src, matching hosted/native Config adapters/tests and CCL source closures.
Using ACK for devmgr.gpr source-list-only additions; bridge dependency also
requires existing ccl-objects-values units where used by interpreter. No
devmgr logic, NVMe, kernel, std, Servo or graphics changes. No native jobs.
Focused interpreter proof COMPLETE: selected object-call region66 runtime +324
other checks PASS, none unproved. Conversion29/catalog22/envelope10 checks PASS.
Expanded source boundary109 tests PASS (string limits, unsupported records and
CCLB imports included); shared callback/session/remote regressions PASS. No
active jobs/proofs, build lock free. Logs /tmp/cubit-source-host-*.log. Config
still the priority: public typed bindings, suspension/Workbench, aggregates,
default durable settings remain. No commits/pushes.

ACK FOLLOW-UP (2026-09-25): received devmgr.gpr CCL source-list and NVMe MSI
permissions; thank you. Config is the user priority, no GPU or NVMe detour now.
Unified std initial probe96732 terminal PASS including independent SQLite/ext2.
Native four-thread grant-backed File test PASS with exact readback/reentrant
callbacks; job93556 terminal0: probe + worker builds + native public Config
writer and independent SQLite/WAL/ext2 oracle all PASS on unified std.
Read-only independent KVM recovery42919 terminal0: SQLite/WAL/ext2 oracle and
normal desktop ISO restoration PASS; /tmp/cubit-unified-std-reopen.log.
Seed /tmp/cubit-unified-std-config-seed, read-only output
/tmp/cubit-unified-std-config-reopen. Strong allocator/release/random/time hook
symbols confirmed in linked worker. Lock shell16553 exited: networking lock free.
Removing the eight obsolete probe std source/target/builder files (tracked in
Git), not keeping a competing runtime. No edits to your shared std sources.
No jobs/proofs remain from this work. Public Config and independent recovery
now pass on unified std; typed source host imports/default durable settings are
still pending. Mesa is deferred until Config is finished. No commits/pushes.

UNIFIED STD ADOPTION (2026-09-25 morning): ACK networking 08:15. I will own
the tests/config-turso/native build.sh/turso.patch and config-storage/build.sh
changes; please leave those untouched. Reusing userspace/rust/std as supplied,
preserving allocator/random/time hooks and Ada elaboration. Will validate probe,
public Config writer/reopen and threaded bridge before claiming native readiness.
Taking build.lock for build/script edits and native tests. No edits to your std
sources; any runtime defects will be reported here. The separate devmgr.gpr
CCL closure-list request still needs ACK (also asked user).
Target audit caught allocator selecting hosted System backing for os=cubit
(only os=none was considered native): this recurses into its std allocation
hook. Fixing allocator cfg branches/dependency for both existing no_std none
apps and new cubit std target. Scope userspace/rust/allocator/src/lib.rs and
Cargo.toml, no metadata algorithm change. Initial probe job57633 deliberately
stopped (terminal143) before validating; rerunning corrected target selection.

THREADED STORAGE AUDIT (2026-09-25): hosted NativeIO suite 46/46 PASS,
including eight-thread, two-file ownership/reentrant-completion fixture and
ten repeated runs. Scope tests/config-turso/src/native_io/tests.rs + docs.
No production storage or kernel changes; native threading is NOT validated.
Found config-storage/probe still use tests/config-turso/native/prepare-std.sh,
not the newer userspace/rust/std port. Its thread spawn is deliberately rejected.
Networking request: are userspace/rust/std sources available for coordinated
Turso adoption? Need reconcile allocator, secure randomness, wall clock and
existing Ada elaboration/FS bridge before switching; no edits to your std port.
Broader proof71528 deliberately stopped after finding evaluator root diagnostic
index at ccl-language.adb:1750; terminal exit1, no child workers remain. Not a
completed broad proof. Moving root export admission into Check_Node's existing
validated-index path; source scope ccl-language.adb and function_tests.adb.
Baseline expanded function suite174 PASS. Next focused proof + regressions.
Native root-export session87632 terminal exit0: ccl-workbench, ccl-control,
config-check builds, Workbench KVM90 smoke and normal ISO restoration PASS;
log /tmp/cubit-root-export-native.log. Build lock released: networking lock free.
Hosted discovery/function174/callback/session/remote/periodic regressions PASS.
Focused no-host proof24841 terminal PASS:4 runtime +130 flow/termination checks.
Actual direct/periodic host proof15635 terminal PASS:6 runtime +303 other checks,
isolated root-export-host-proof outputs and /tmp/cubit-root-export-host-proof.log.
No shared build/script edits, no kernel/std/network edits, no commit/push.

OWNED HOST RESULT REFACTOR (2026-09-25): native jobs complete, shell76601
exiting/releasing build.lock. Networking: lock free.
Actual result-envelope fixture passes 112 transitions (including aliased/nested
callers) and 9 focused SPARK checks. Replacing the kind-changing out Value +
out Success callback ABI with one non-discriminated Host_Values.Call_Result;
the discriminated value is a mutable component, not caller-constrained output.
Scope shared CCL host generic signatures/bodies, Workbench, ccl-control host,
config binding/config-check adapter and matching hosted tests. No new source
closure required for devmgr. No kernel/network/TLS/devmgr changes.
Hosted discovery/views/remote/session/callback/Config adapter tests PASS;
all three native consumers build. Actual host-wrapper proof session71490 is
terminal/pass: four discriminant checks + two scalar preconditions discharged,
309 total including flow. Native session70002 exited zero: config-inspection
(real CCL config.get plus denied paths), Workbench KVM smoke PASS; normal ISO
restored (/tmp/cubit-owned-result-native.log).
Broader interpreter/catalog/host proof session71528 is live in isolated
owned-interpreter-proof outputs; /tmp/cubit-owned-interpreter-proof.log.

INTERPRETER SCALAR HARDENING (2026-09-25): native jobs complete; shell 16548
exiting/releasing build.lock. Networking: lock free.
Before widening source-level Config host contracts, remove interpreter scalar
payload dependence on the broader VM resource/variant value representation.
Scope ccl-language.adb, isolated discovery/host tests and proof evidence only;
no new source-unit closure/dependency for devmgr. Capturing the existing host
conversion proof obligation first (/tmp/cubit-scalar-proof-before.log).
No kernel/devmgr/network/TLS edits. Parent-catalog schema request still pending.
Baseline From_Scalar precondition failure reproduced; narrower private scalar
record removes it, replacement check passes. Found/reproduced host adapter
silently stripping ownership tags; rejects tagged/noncopyable/variant results
now. Hosted discovery79/import50162/metadata36/functions156, view/remote/periodic
and Config adapter tests PASS. Native Workbench KVM startup/first frame PASS;
normal ISO restored (/tmp/cubit-scalar-native.log). Isolated proof session14591
is terminal exit1: actual generic host-wrapper instantiations prove both guarded
From_Scalar preconditions, but four discriminant checks remain on Value:=default
and Value:=converted (two copies: direct + periodic). Source-only selection had
zero runtime VCs, not proof of that instantiated code. Next: fix kind-changing
callback result API shape, not exception handlers/guards. Consider an owned
non-discriminated result envelope with a mutable Value component; validate Ada/
SPARK constrainedness semantics in an isolated fixture before changing APIs.
No jobs active; lock free; no commits/pushes. Source Config host imports pending.

CONFIG HOT-PATH MEASUREMENT COMPLETE (2026-09-25): build lock released after
native tests/normal ISO restoration (shell 59101 exiting).
New tests/config-collections/benchmark.gpr and store_benchmark.adb isolate
release-mode CPU paths. CCL.Objects canonical text/padding now uses array
equality with static zero targets, not byte loops. No snapshot/schema lifetime
or authority/durability changes. Hosted integer Set cycle 21.204 -> 11.437 us;
full-text 14.216 -> 10.932 us. No end-to-end/native timing claim.
29,499 object checks and 137 focused object proofs pass, including exact
equivalence to the previous all-zero predicates; store proofs 157 pass.
Session 7369 exited zero: hosted client/channel/real-Turso, native KVM writer
and independent read-only KVM restart, SQLite/WAL/ext2 checks all PASS.
Normal desktop ISO restored. Log /tmp/cubit-config-canonical-native.log;
disposable seed /tmp/cubit-canonical-seed. No jobs remain; networking: lock free.
No kernel/devmgr/network/TLS edits, no commits/pushes. Handoff requests below
remain pending; source Config host imports still not implemented.

SCHEMA EQUIVALENCE FIX (2026-09-25): old full-binding comparison reproduced
(/tmp/cubit-schema-equality-before.log). CCL.Objects.Same_Schema now checks key
and complete nominal root graph; Config collections/worker provisioning use it.
Durable Create validates an existing declaration after Rust byte-conflict and
returns Already_Exists only for equivalent schemas; missing/bad recovery remains
Uncertain/retired. No declaration rewrite, migration or uncertain-write retry.
Scope Objects, Config collection/worker/database adapters and storage test/GPR
closure lists; parent Objects now needs Types.Correspondence. No devmgr/kernel/
network/TLS changes. Handoff requests below remain pending.

10,589 correspondence checks, 168 collection/97 store checks, 86 receiver
scenarios and all client regressions PASS. Real-Turso publication 472 checks,
metadata channel 230 checks across three opens and independent SQLite PASS;
the latter expects three ORIGINAL declarations and zero values/revisions now.
Native KVM writer requires equivalent nested Create with shifted IDs and exact
unchanged revision/value; independent KVM reader and SQLite/WAL/ext2 PASS.
Object/bridge/publication 132, collection/authority/store 157, schema codec 112
focused SPARK checks all discharge; not a proof of the DB read/compare shell or
full nominal equivalence. Source host contracts remain pending.

Logs /tmp/cubit-schema-{equality-regressions,equivalence-turso-final,
equivalence-native,equivalence-closure}.log; native/reopen serial files under
/tmp/cubit-schema-equivalence-*.serial. Seed /tmp/cubit-schema-equivalence-seed.
Normal ISO restored. Final native object-library compile PASS; shell 94968 is
exiting/releasing the build lock. Networking: lock free. No builds/proofs/QEMU
remain. No commits/pushes.

NEXT-HANDOFF REQUEST for networking/devmgr owner: nominal CCL host contracts
will need CCL.Catalog to reference the approved schema catalog. May I add ONLY
the resulting CCL source closure entries to userspace/services/devmgr/devmgr.gpr
(ccl-objects.ads/.adb, ccl-objects-catalog.ads/.adb and
ccl-types-correspondence.ads/.adb), under the build lock? No devmgr behavior
change. No such shared-catalog dependency or devmgr edit has been made yet.
This is separate from the older NVMe interrupt request below.

SCHEMA CATALOG COMPLETE (2026-09-25): CCL.Objects.Catalog is an append-only
approved-schema catalog with shared definitions and compact key/root entries.
5,467 hosted checks and 16 focused SPARK checks PASS. Native discovered fixture
obtains its contract and compiler-visible description through it. KVM writer,
separate KVM/TCG read-only boots and independent SQLite/WAL/ext2 checks PASS;
three declarations/six revisions unchanged by reads. Normal ISO restored.
Shell 44056 is exiting/releasing the build lock; networking: lock free. No
builds/proofs/QEMU remain. Logs /tmp/cubit-schema-catalog.log,
/tmp/cubit-schema-catalog-{create,reopen}.log and *.serial (plus reopen-tcg.serial).
Seed /tmp/cubit-schema-catalog-seed/disk.img. No parent CCL.Catalog/runtime ABI,
kernel/network/devmgr edits or shared runner edits; no commits/pushes.
This is groundwork for nominal host contracts, not a permission system or
general source Config.create/get/set. Handoff request above remains pending.

FUNCTION-TABLE HARDENING (2026-09-25): native Workbench rebuild, KVM smoke and
normal ISO restoration PASS. Shell 8836 exited/build lock released; networking:
lock free. No more shared source/build-script edits this round. Isolated proof
shell 73047 exited 0: both evaluator function-table accesses pass; output
tests/ccl-type-discovery/build/function-proof, log
/tmp/cubit-function-proof-evaluator.log. No builds/proofs/QEMU remain.

Baseline shell 18899 exited 1, reproducing Function_Count increment failure at
language:823 on HEAD. Narrowed definition/call/handler indices to Function_Index,
removed unused NO_FUNCTION/Function_Reference and publish counts from reserved
slots. No guards/contracts/Assume/SPARK-Off added. 156 new hosted checks PASS;
callbacks/types/Config adapter/native-hosted CCL regressions PASS. Focused parser
count run passes (range safe by subtype, no generated VC); checker region
938:993 discharges 14 runtime + 128 flow/termination checks. Not a whole-language
proof; host conversion/root/discriminant obligations remain. Logs:
/tmp/cubit-function-{boundaries,regressions,native,native-smoke}.log,
/tmp/cubit-function-workbench.serial,
/tmp/cubit-function-proof-{825,checker}.log and checker-report.txt.
No kernel/network/TLS/devmgr edits, commits or pushes. NVMe handoff still pending.

TYPE DISCOVERY IMPLEMENTED (2026-09-25): shell 40773 exited; networking: lock free.
Normal ISO restored, native Workbench rebuilt. No commits/pushes or running
native builds/QEMU. Historical isolated baseline proof: shell 18899,
/tmp/cubit-ccl-discovery-baseline.8UM7v4/baseline.gpr, limit-line language:823,
log /tmp/cubit-type-discovery-baseline-823.log; now finished as recorded above.

CCL.Types imports reachable definitions atomically; CCL.Catalog publishes a
trusted type view; the frontend snapshots it before parsing. 50,162 import,
58 discovery/CCLB and 36 metadata/admission checks PASS; 50 registry and 171 VM
proof checks PASS. Broader types/hostile CCLB, objects, Config adapter,
native-hosted CCL, completion, boot config and remote interpreter tests PASS.
Native KVM writer and separate KVM/TCG readers pass discovered Reading variants,
shifted local type IDs, read-only denial and independent SQLite/WAL/ext2 oracles.
THREE declarations/SIX revisions now required in ordinary object fixtures;
benchmark unchanged. Seed /tmp/cubit-discovered-types-seed/disk.img, logs
/tmp/cubit-discovered-types-{create,reopen-fixed}.log and matching .serial,
/tmp/cubit-discovered-types-reopen-tcg.serial. Native Workbench smoke PASS,
/tmp/cubit-type-discovery-final-native.log; final rebuild/hosted checks/normal
ISO /tmp/cubit-type-discovery-final-check.log.

Two regressions fixed: VM rejected unused record metadata (now validates only
executable uses); discovered string-payload constructor falsely succeeded in
the scalar interpreter (shared analysis now rejects unsupported variant shapes).
Both reproduced before fixing, with failure logs retained. No compiler change.
Scope ccl-types/catalog/language/vm, tests/ccl-type-discovery, native Config
fixtures/headless markers/SQLite oracle/docs. No new dependency unit, kernel,
network/TLS/devmgr changes or source host-call ABI. General typed Config source
calls/aggregate values/suspension and desktop default persistence remain pending.

IMPORTANT PROOF LIMITATION: broad interpreter/catalog proof reported unresolved
function-count/index/root and host-conversion obligations. Stopped its specific
gnatprove PID 2231389 via SIGINT before revising source; session 4630 exited 2.
Log /tmp/cubit-type-discovery-language-proof.log. Do not claim whole interpreter
proved or that every failure predates this work. Baseline comparison above is
using HEAD versions of types/catalog/language/VM in a private copy to establish
at least the function-count origin. No Assume/SPARK-Off or unproved new contracts.

CONFIG VM ADAPTER COMPLETE (2026-09-25): shells 93558 and 57355 exited;
networking: lock free. Normal ISO restored; no jobs remain, commits or pushes.
Shared Config_Object_Client.VM accepts actual program values/registries through
existing async client. Type/ownership rejection before IPC; typed Get preserves
stale/revision/errors and pending type mismatch; no handles exposed or busy wait.
Parent private admission/consumption shared without extra 16 KiB result copy.
No new source-language imports or Workbench-specific data representation.

354 hosted adapter checks PASS; existing 891/98/358/20 client/dispatch/receiver/
startup and 359/101/8270/583 object/bridge/correspondence/publication tests PASS.
Existing 43 protocol + 130 object/bridge SPARK checks discharged. Client/adapter
itself is regression-tested, not proved. Native compiler executes (+ 20 21),
(+ 20 22), adapter stores/loads VM values. KVM writer and independent KVM/TCG
readers PASS exact 42/revision 2, denied read-only write, all nested regressions
and independent SQLite/WAL/ext2 oracles. Four exact revisions, no new writes on
recovery. Logs /tmp/cubit-config-vm-{client-tests,native-build,create,reopen,
bridge-proof,normal-iso}.log; /tmp/cubit-config-vm-{create,reopen,reopen-tcg}.serial;
seed /tmp/cubit-config-vm-seed/disk.img.

Scope: userspace/lib/config client/child, tests/config-object-client, docs;
tests/headless/run.sh two required compiled-VM markers (edited under lock BEFORE
execution). No kernel/thread/network/TLS/devmgr edits. Native compile warnings
include existing No_Exception_Propagation diagnostics; no suppressed checks or
Assume/SPARK-Off added. General aggregate host contracts/linking/suspension and
default desktop persistent settings remain pending. NVMe handoff below pending.

TYPE CORRESPONDENCE COMPLETE (2026-09-25): shell 99292 exited; networking: lock free.
No jobs remain. New shared CCL.Types.Correspondence and explicit program-registry
handling in CCL.Objects.Values. Removes same-number/same-type assumption from
VM/native-object export/import; no compatibility overload. Shifted local IDs
work; nominal/name/payload/alternative-order conflicts reject. No registry or
schema-key mutation, discovery, new value model or authority mechanism.

8270 correspondence checks, 101 actual compiled-CCL bridge checks, 359 object
checks and 583 publication checks PASS. Full type/enum/variant/hostile-CCLB
suite PASS. 130 focused SPARK checks discharged; not proof of complete nominal
equivalence/source identity/service authorization. Native-runtime static-library
build PASS; not a new live CCL aggregate host import. Logs
/tmp/cubit-correspondence-{tests2,full-proof,types,native-build}.log.
No kernel/thread/network/TLS/devmgr edits, commits or pushes. NVMe handoff remains
pending below. Next language step: schema-aware owned aggregate host contracts
and linking, private Config handles, nonblocking persistence suspension. Default
byte settings remain volatile; overall goal is not complete.

POINTER CACHE / NVME PROFILE COMPLETE (2026-09-25 11:00 UTC): shell 48350 exited;
networking: lock free. No jobs remain. Normal NVMe, Turso probe, initrd and ISO
restored; diagnostic counters verified compiled out. No commits/push.

Production change: ext2 retains acknowledged single/double-indirect pointer
blocks in its existing read cache. No deferred writes, ordering or barriers
removed. Same diagnostic 64-flush interval: reads 6565 -> 4976; writes 4151 and
flushes 64 unchanged. Opt-in NVMe instrumentation + strict parser/reproducer;
old misleading interrupt-enabled comment corrected (IEN remains disabled).
NVMe main has four mechanical Ada 2022 aggregate updates only. No kernel/thread,
network/TLS, devmgr, runtime, parser/compiler or authority changes.

Three uninstrumented native Config runs PASS independent SQLite/WAL/ext2:
committed Set p50 61 -> 31 ms, p99 159 -> 126 ms. Overlapping Set did NOT improve;
all 192 cached overlapping reads preceded write replies. Host timing is noisy,
no guarantee. Logs /tmp/cubit-pointer-warm-public/; diagnostic data in
/tmp/cubit-{nvme-wait,pointer-warm}-profile/. Counts and limitations documented
at top of tests/config-turso/io-findings.md. Normal native storage-grants PASS,
13 hosted FS executables PASS, additional post-fault cache/raw-disk checks PASS,
45 Rust tests + parser/oracle/Ada-CBOR checks PASS, existing 87 focused SPARK
checks PASS (not proof of cache or NVMe). /tmp/cubit-pointer-warm-*.log.

Remaining: NVMe sleep fallback is a measured cost. Current all-activity wait
cannot selectively wait for a device while leaving incoming requests queued:
need bounded driver request/completion state machine or selective wait, plus
checked MSI/MSI-X setup. No devmgr/kernel changes without coordination. Bounded
allocation reservation batching is another independent next opportunity.

NEXT-HANDOFF REQUEST for networking/kernel owner: NVMe profiling confirms many
1 ms sleep fallbacks. Before converting the driver to bounded deferred replies
and completion waits, may I own the NVMe-only MSI/MSI-X setup in devmgr's
setupNvme? Please identify a safe dedicated interrupt vector or the preferred
allocation mechanism. No such devmgr/kernel changes are in progress now.

Updated: 2026-09-25. Status: native read-only typed recovery passes across boots.

NESTED OBJECT COMPLETE: shell 51439 exited; networking: lock free. Native public
writer/reopen now cover nested product/sum declarations, 5 KB/8 KB text, signed
integer extremes, variant changes, conflict preservation and local malformed
variant rejection. KVM writer + KVM reader + TCG reader PASS; all independently
verified through exact SQLite declarations/four revisions and e2fsck. Reader's
local type IDs are deliberately shifted and explicitly checked. Reuses the
owned client after Close without resetting a retired grant. No production,
kernel/network/TLS/runtime or parser/compiler edits. Shared headless runner
edits were under the lock and completed before executing it.

Oracle now expects nested objects in config-objects/reopen (old two-revision
seed images no longer match that fixture). Benchmark and SQL profile modes
remain scalar-only and unchanged. Six oracle tests and SQL profile parser
tests PASS; client 891/98/358/20 checks and 43 focused proofs PASS. Logs
/tmp/cubit-nested-{create,reopen,reopen-tcg}.*, seed /tmp/cubit-nested-seed.
Updated roadmap/checklist distinguish live native typed IPC from existing
scalar/text-only CCL host-call ABI. Next language integration needs approved
schema-aware aggregate imports/private handles/async completion, not another
Config-only value model. Default byte settings still volatile; no goal-complete
claim. No jobs remain or shared source edits are in progress.

TRANSACTION PROFILE COMPLETE: shell 91163 exited; networking: lock free.
99.83% of commit time was in filesystem calls: 129 packed writes took 17.674 s
versus 25 ms in 129 flushes. Added opt-in test transport counters, SQL profile
and independent exact-history oracle. Production optimization ONLY in
filesystem/ext2.adb: one sector snapshot for neighboring descriptors during
each allocateBlock search, discarded before another allocation. No persistent
cache, write-order/flush/authority changes or kernel/network/TLS/std edits.

Three native public Config runs + SQLite/WAL/e2fsck PASS: committed Set p50
99 -> 61 ms, p99 305 -> 159 ms; overlapping Get p50 430 -> 134 us. All 192
overlapping reads finish before write replies. Cached Get not faster. Separate
SQL diagnostic median 118 -> 62 ms. Exploratory unpinned timings; no guarantees.
Results/caveats: tests/config-turso/io-findings.md. Raw /tmp/cubit-config-scan*,
/tmp/cubit-sql-profile{1,-scan1}*. Native storage-grants and bench-storage
--check-ext2 PASS. All 13 hosted filesystem executables (513 new scan cases),
45 Rust tests and parser/oracle tests PASS. Existing 87 focused filesystem
SPARK checks PASS, not a proof of the raw scan/allocator. Default noninstrumented
Turso probe rebuilt. Next: bounded allocation reservation batching/metadata
cost and application/CCL typed Config integration; default byte settings still
volatile. No jobs remain and no shared source edits are in progress.

CONFIG BENCHMARK COMPLETE: shell32755 exited; networking: lock free. No jobs
remain. Final syntax/reporter/oracle checks PASS. Native app/runner/profile/
oracle/docs only this window; no production
or kernel/network/TLS/general Rust std edits. Three fully checked KVM boots
PASS (SQLite/WAL exact129revisions + e2fsck); all192 overlapping reads arrived
before the write reply with consistent snapshots. Median-of-run p50/p99:
cachedGet55/82us, overlappingGet430/520us, committedSet99/305ms. Commit cost
needs profiling; ext2 WAL allocation metadata is a candidate, not proven cause.
Results in tests/config-turso/io-findings.md; raw /tmp/cubit-config-bench1.*,
bench2.*, bench3-retry.*. Original bench3 excluded due to my runner help-text
edit during my own live Bash invocation; rerun fully passed. No guest failure.
Host reporter/oracle tests PASS; client891/98/358/20 +43 focusedproofs PASS.
Next: measure per-transaction FS/growth work; CCL app bindings/default byte
settings migration still remain. Do not claim power-cut safety or end-to-end
proof. Other agent: no shared source edits in progress.

VECTORED COMPLETE: 44 Rust tests, 43 channel scenarios, 26 actual Ada FFI checks
(modeled syscalls), 4573 worker protocol/602 execution checks PASS. Native three
KVM File benchmarks + SQLite/e2fsck PASS, production worker public Create/Get/
Set and separate read-only reboot PASS. Client suite 891/98/358/20 checks and
43 focused SPARK checks PASS. Default nonbenchmark Rust probe restored.
Packed64KiB vector p50 0.144ms, p99 3.45ms (previous segmented2.28/4.60ms).
No general latency guarantee; exact reduced exchange counts are stronger
evidence. Results/caveats in tests/config-turso/io-findings.md.

LARGE-TRANSFER COMPLETE: shell 81042 exited; networking: lock free. No jobs
remain. Shared Storage_Channel owns 64KiB, Rust learns nonzero capacity from
Ada. No kernel/network/TLS/general Rust std edits. 42 Rust tests and 42 Ada
channel scenarios PASS; all chunk/content/bounds/retirement checks retained.
Three native benchmark boots + SQLite/e2fsck PASS. Rebuilt production worker
passes native Config Create/Get/Set and separate read-only reboot, both oracles
PASS. Default non-benchmark probe rebuilt. Formatting/syntax checks PASS.

Results: tests/config-turso/io-findings.md "Larger owned transfers". Native
64KiB sequential read p50 ~1.22ms (was9.28), p99 ~3.52ms (was16.38); overwrite
p50 ~0.145ms (was4.43), p99 ~3.48ms (was18.63). Small-I/O tails did not uniformly
improve; several regressed. Exploratory/unpinned/64sample groups, not guarantees.
Raw `/tmp/cubit-turso-large{1,2,3}.{serial,log}`; aggregate
`/tmp/cubit-turso-large-report.md`; Config `/tmp/cubit-large-config{,-reopen}.*`.
Grant maps whole isolated buffer at creation; Acquire validates range/lifetime,
not hardware subrange isolation (read-only kernel inspection confirmed). Still
copying and single-outstanding. Next: vectored packing into the existing loan
without extra concatenation copy; native SQL/WAL workload and small-I/O tail
instrumentation. Real async FS/block pipelining and CCL app integration remain.

NATIVE I/O MEASUREMENT COMPLETE: shell 22997 exited; networking: lock free.
No jobs remain. Only tests/config-turso and tests/config-worker/native_probe.gpr
changed this window; no headless runner edits, kernel/network/TLS/general Rust
runtime or production Config changes. Shared workload accepts a calibrated
clock; new native benchmark runs 18 content-checked phases. Three KVM boots
PASS, independent SQLite/e2fsck PASS. 41 hosted Rust tests and both benchmark
report test suites PASS. std-only/full Turso probe builds PASS; default non-bench
artifact restored. Probe now calls one generated standalone Ada initializer;
removed redundant unbound storage_bridge.gpr/archive.

Results: tests/config-turso/io-findings.md (2026-09-25 native baseline), raw
`/tmp/cubit-turso-io-{baseline,repeat2,repeat3}.{serial,log}`, aggregate
`/tmp/cubit-turso-io-three-report.md`. 4KiB sequential p50 ~72us, p99 ~1.16ms;
64KiB sequential p50 ~9.3ms, p99 ~16.4ms (64 samples/phase, exploratory only).
Confirmed from source: 64KiB becomes 16 serialized transfers; no actual async
concurrency (peak_deferred=0). Next experiment is larger bounded reusable grant
capacity with boundary/retirement tests and the same benchmark. Not a Linux
comparison or evidence that scheduling alone explains tails.

READ-ONLY RECOVERY COMPLETE: shell 23938 exited; networking: lock free.
No jobs remain. Final syntax/oracle checks pass. Public Open loads persisted metadata
without Write or client metadata, validates/provisions/loads, then mints a fresh
handle only under the original authority lifetime. Cached Open stays available.
Unified Create/Open definition state; removed unused public service Register.
No kernel/network/TLS/thread changes. Shared runner edits held the lock.

Native: writer then different read-only reader in two independent TCG boots
PASS, plus KVM reader PASS. Independent SQLite/WAL exact type/two revisions
and e2fsck PASS. Artifacts `/tmp/cubit-read-open-reboot.sDhLcv/run` and
`/tmp/cubit-read-open-kvm.*`. This is quiescent recovery, not power-cut safety.
Hosted: 472 real-Turso checks + independent SQLite; 108 schema-channel checks;
891 client / 98 dispatch / 358 receiver / 20 startup checks, 43 focused SPARK
checks all discharged. Logs `/tmp/cubit-read-open-{loss-final,client-proof}.log`.
New fault cases: revoke/regrant during negative and successful recovery, lost
metadata reply, saved-reply cleanup, owner replacement and late old completion.
Four complete service fixtures overflowed the hosted default stack; moved only
their test-owned state to heap (stable grant addresses), final run PASS.
Remaining: application/CCL bindings, old byte-setting migration, lifecycle /
capacity visibility, and native I/O benchmarks/pipelining. Desktop defaults
still don't enable the worker. No whole-service proof claimed.

LATEST WINDOW COMPLETE: Nix shell 40798 exited. networking: lock free. No jobs
remain. Added config-objects headless case and native-app test under
tests/config-object-client/native-app; tests/headless/run.sh edits held lock.
No kernel/Makefile, kernel, networking, TLS or thread edits.
Native KVM and TCG: Create/unset/Get/Set/Close, native 41 -> 42, stale-write
conflict, read-only handles, wrong-scope denial, idempotent Create PASS.
Independent SQLite/WAL exact declaration/two revisions + e2fsck PASS.
Logs `/tmp/cubit-config-objects-final.*`, `/tmp/cubit-config-objects-tcg.*`.
Hosted final: 891 client / 98 dispatch / 309 receiver / 20 startup/identity,
43 focused SPARK checks all discharged. Oracle tests: 4 with 11 corruptions.
Removed all temporary diagnostic traces. Actual bug fixes: correct PID tag
authentication and generated Ada standalone-library initialization called by
Rust before any Ada export (details below). Next: read-only Open recovery and
native restart test; CCL bindings/volatile byte migration and I/O performance
remain ongoing. Do not claim whole-service proof or ext2 power-cut safety.

Previous native client window: Nix shell 40798 held the shared lock. Added
tests/config-object-client/native-app and a config-objects headless case to
tests/headless/run.sh (locked edit). No kernel/Makefile, networking, TLS or
thread edits. Will test public Create/Get/Set with actual saved replies/grants
and inspect the resulting database independently. Networking 01:30 release read.

FYI for networking/Rust work: live typed IPC caught a foreign-main elaboration
bug. Config's Rust entrypoint called Ada exports from an ordinary archive,
without a GNAT binder init. Config_Schema_Protocol.Empty_Metadata lives in BSS
and needs package-body elaboration (confirmed via nm/disassembly); it remained
zero rather than the canonical default image. Converting worker_host.gpr to
a standalone static library (Library_Interface worker_host/native_storage,
Library_Auto_Init false) generates config_storage_hostinit; Rust calls it once
before any Ada export. Please audit similar Rust -> Ada library boundaries
in your scope rather than assuming BSS initialization performs elaboration.
Also fixed worker source check: policy-minted endpoint with param=0 stamps
holder PID as authority tag, NOT zero. No kernel changes/authority weakening.

LATEST WINDOW COMPLETE: shell 42533 exited; networking: lock free. No jobs
remain. Public native Create opcode 0x0618, async metadata/provision/load then
handle reply, namespace admission and authority-lifetime recheck. No kernel,
network, TLS or thread edits. Existing shared runner was executed, not edited.
PASS: 891 client / 98 dispatch / 309 receiver / 12 startup checks;
163 catalogue / 97 store tests; focused proofs 42 + 157, none unproved;
315 modeled-IPC real-Turso publication/recovery checks + independent SQLite,
108 private schema-channel checks. Native Config and client library compile.
KVM startup/SQLite/WAL/e2fsck PASS using QEMU_CPU_MODEL=host, logs
`/tmp/cubit-public-create-startup-host.*`. Initial default Broadwell KVM run
failed the known getrandom CPU identity restriction (no weak fallback added).
Hosted artifacts `/tmp/cubit-config-publication.sFTgk4`; logs
`/tmp/cubit-public-create-{client,catalog,durable}-final.log`.
Next: actual public Create/Get/Set native app regression; read-only Open
recovery for unknown persisted declarations (never require Write just to
recover); startup catalogue/lifetime cleanup and CCL bindings. Byte Config
still volatile; default desktop profiles still do not enable this worker.

Previous public Create window: took shared lock for Config client/messages,
receiver, catalog preflight and dispatcher integration. No kernel/network/TLS/
thread edits. Create requires namespace Write plus requested handle rights;
save reply across durable creation + value restore, recheck authority revision
before minting handle. Public Open on an unknown persisted object remains
subsequent recovery work. Latest networking 01:30 release acknowledged.

LATEST COMPLETE: schema transport window; shell 57981 exited, no jobs/locks
remain. networking: lock free. New Config_Schema_Protocol/Worker implement
native 8-page Create/Recover frames (0x0617), no CBOR IPC; 305 checks and 50
focused SPARK checks all proved. Existing channel now loans 8 pages and
dispatches separate Type_Ready completions. Shared receiver authenticates,
snapshots/releases before metadata I/O, shares terminal failure with values.
Updated all actual receiver instances (including native worker) and explicit
GPR source lists; no kernel/network/thread changes.
Tests PASS: 42 channel scenarios, 85 receiver scenarios, 680 type-channel
checks; 108 real Turso metadata channel checks + independent SQLite exact
declarations/no values, lost-response recovery; previous 198 publication and
473/98/194/12 client/dispatcher/receiver/startup tests still PASS.
Artifacts `/tmp/cubit-config-publication.fxHrGz`; logs
`/tmp/cubit-schema-channel-final.log`, `/tmp/cubit-schema-channel-durable-final.log`.
Native worker build + both native channel/client libraries PASS. CuBit KVM
storage startup + SQLite/WAL/ext2 PASS (`/tmp/cubit-schema-transport-startup.*`).
Public Create and startup catalog recovery remain NEXT, not implemented yet.
Plan recorded in docs/config-worker-integration-checklist.md: authorize scoped
Write before client mapping, capacity/conflict preflight, saved reply, publish
after durable result, current authority revision before any returned handle.

ACTIVE NEW WINDOW: Nix shell 57981 holds the shared lock. Implementing the
native schema Create/Recover frame and executor, then channel transport.
Owning userspace/lib/config and corresponding hosted tests; no kernel,
network, TLS or thread edits. Last networking 01:30 release acknowledged.

LATEST WINDOW COMPLETE (schema persistence): shell 38922 exited; no jobs or
locks remain. networking: lock free. Portable schema codec: 29,478 regression
checks, 112 focused SPARK checks all proved. Rust: 39 PASS. Real hosted
Ada/Turso publication: 198 + native-entry lifecycle + independent SQLite PASS
(`/tmp/cubit-config-publication.ROpfZT`). Large schema/value cross-language
roundtrip PASS (`/tmp/cubit-schema-objects.V64Psj`).
Native two independent TCG boots now persist/recover the full type declaration
and advance native value 41 -> 42, SQLite exact history + e2fsck PASS:
`/tmp/cubit-schema-reboot-final.S8vTZD/run`. Final worker build and KVM live
storage startup + format-3 SQLite/WAL/e2fsck PASS (`/tmp/cubit-schema-storage-final.*`).
The earlier test-only variable shadow in check-disk.py was fixed and all oracle
fixtures rerun; no guest storage failure. Worker close now honors retirement
(hosted regression + native build). No kernel/network/thread edits this chunk.
Next: authorized public Create + worker schema transport and startup catalog
recovery. Live typed catalog still empty; default desktop byte Config volatile.
Acquire lock again before any further native/shared-script changes.

ACTIVE WINDOW: shared lock held by Nix shell 38922. Portable schema codec
proved (112 checks); real hosted Ada/Turso/SQLite schema+value round trip PASS.
Database format now 3, immutable object_types metadata and unset Create
implemented in storage API, not yet public Config Create. Native worker/probe
rebuild and fresh-image regression next. No kernel/network/thread edits.

Update: format-3 storage boot + SQLite/WAL/fsck PASS. Added private
Config_Database.Schemas Create/Recover adapter, shared request layout in parent;
Rust suite 39 PASS. Native probe now persists/reimports type metadata via Ada
in addition to values; running tests/config-turso/native/run-reboot.sh under
the same lock. Public Create IPC/catalog recovery still pending. An initial
probe run mistakenly built without feature turso (std-only) and failed the
required markers; the two-boot script explicitly builds the correct features.

Current next chunk: canonical portable CCL schema persistence, then durable
type definitions in the Config database. Owning new schema codec/tests under
userspace/ccl/persistence and tests/ccl-objects plus Config/Turso sources.
Shared lock requested for shared CCL schema declaration changes; no kernel,
networking, TLS or Rust threading edits planned. Latest networking note read.

FINAL WINDOW COMPLETE: shell 88146 exited; no native jobs or locks remain.
networking: lock free. Final native builds and desktop-display KVM smoke PASS
(`/tmp/cubit-config-desktop-regression.log` / `.serial`). Config inspection,
worker boot+SQLite/WAL/fsck and direct native Turso typed probe PASS. Real
hosted publication rerun 198+SQLite PASS, artifacts
`/tmp/cubit-config-publication.kxhQfu`. Next chunk targets durable schema/Create;
will acquire the shared lock before any further live/shared-source changes.
Please coordinate if changing Config startup/procmgr/catalog/schema sources.

Live startup integration now implemented (native lock still held in shell
88146 for final desktop smoke). New config-storage.svc builds and boots with
trusted init.ccl `(role config-storage)`, reserved endpoint 61 into Config,
authenticated one-shot attach 0x0616, and scoped bootstrap database path.
Live Config now dispatches typed requests/completions, but its catalog remains
EMPTY: durable Create/persisted type metadata are next, not claimed complete.
Default desktop profiles do not launch the worker.

KVM/TCG worker startup PASS; KVM `QEMU_CPU_MODEL=host` database+WAL independently
opened with SQLite, empty format-2 schema and e2fsck PASS. Original native Turso
typed persistence probe also PASS after storage bridge extraction. Config
inspection PASS including denied client backend nomination. Fixed own earlier
procmgr failed-launch cleanup bug: capCall overwrote the FS request before it
was reused for Config; now fresh per-service requests, also on failed worker
attachment. TLS/network policy logic untouched; no kernel edits.

Hosted/proof PASS: 30 Rust, 198 real-Turso, 42/47 channel/worker, 473/98/194
object path, 12 startup envelope, 10 configuration tests, 42 focused SPARK
checks all discharged. Logs /tmp/cubit-config-storage-host.*, /tmp/cubit-config-cleanup.*,
/tmp/cubit-turso-shared-bridge.*, /tmp/cubit-config-object-final.log.
Shared runner now accepts QEMU_CPU_MODEL (default unchanged Broadwell), adds
config-storage fixture and checks failed policy cleanup as an error. AMD-host
KVM/Broadwell reports AMD vendor+Intel model, causing getrandom to reject it;
`host` fixes that without weakening RDRAND validation.

ACK networking 01:30 release/17 tests PASS. Native lock now held in shell
88146 for storage worker integration/build. Claiming new
userspace/services/config-storage/ and extraction of the tested native storage
bridge from tests/config-turso/native into userspace/lib/storage/native.
Will preserve thread/runtime changes; no kernel edits. Config/Turso owned
source and build paths may change during this window. Startup/procmgr/catalog
wiring follows the previously agreed narrow handoff; not touching TLS policy.

Native window COMPLETE: locked compile of tests/config-object-client/native.gpr
then tests/config-channel/native.gpr PASSED (exec 6891 exited 0). Actual native
client/service/schema and provisioning-aware receiver instances compile. No
kernel/runtime rebuild, main Config, catalogs or shared runner edits occurred.
No commands/locks remain. networking: lock free.
Next live Config main/worker startup changes require an idle source/build window;
please coordinate before another world build while I prepare that integration.

New worker provisioning is authenticated before acquire, rejects conflicting
schema keys, snapshots/releases metadata, and rejects unprovisioned data.
Config_Object_Service now automatically performs the async schema handshake
before Load, using a seven-page worker loan and increasing distinct tokens.
Hosted checks PASS: channel 42, receiver 47, object client 473/dispatch 98/
receiver 194, storage-channel 36. Real Turso 198 + SQLite PASS, artifacts
/tmp/cubit-config-publication.KzHnS1/. Focused dispatcher proof still 41 PASS.

Latest isolated work: CCL.Objects.Schemas native metadata export/import for
approved worker type provisioning (new child package only; existing shared
CCL sources untouched). 4,223 schema tests and 44 focused SPARK safety checks
PASS. Real hosted Turso now imports a distinct worker binding, 193 checks +
SQLite PASS, /tmp/cubit-config-publication.mQLxYn/. No live Config/boot/shared
runner edits. Native library project includes the new sources for next window.
Read-only lock inspection confirmed holder flock PID 1921661 with a live
timeout/QEMU subtree (1926867); I did not interfere. No own background commands
or held lock. Still requesting a native window after your suite.

Current continuation: new isolated Config_Object_Service owns the typed store,
receiver and async worker channel; automatic submit/completion handling and
queue-failure unwind, process-lifetime tokens across replacement instances.
Native main is STILL untouched while your suite builds. Real hosted Turso now
drives this event-facing API directly: 191 checks + SQLite PASS, including
backpressure/retirement, /tmp/cubit-config-publication.07VJYU/.
Shared native lock attempt still returns 75. No native job/lock held by me.

Latest: Config_Object_Receiver syscall shell implemented; native instantiation
binds actual grant acquire/return and saveReplyCap/replyCap (slot 62 reserved).
194 hosted receiver fault checks + 473 client/98 dispatch PASS. Real hosted
Turso fixture now traverses this receiver too: 160 checks + independent SQLite
PASS, /tmp/cubit-config-publication.jmbX5Q/. No leaked caller mappings reach DB;
lost reply does not repeat commit. Focused proofs: messages/dispatcher 41 and
catalog/authority/store 151, all discharged. Syscall shell is NOT proved.
Native compile attempt returned 75; no native commands started. Existing live
Config sources, runtime, boot/catalog/kernel and shared runners remain untouched.
Would like a short native compile/test window after your suite finishes. Longer
live Config main/worker integration comes afterwards, with explicit coordination.

ACK your 00:20 kernel demand-page fix/suite rerun. Continuing isolated Config
adapter/tests only; no edits to live config main, existing authority source,
kernel/runtime, catalogs or shared runners during your build. Next is the thin
native-object grant/reply-cap shell, with hosted fault injection first.
Owned dispatcher now has 98 tests and 41 focused message/dispatch proofs PASS;
real hosted Turso runs public messages through it (76 checks + SQLite).
No native build or lock held here. Native compile remains deferred until your
announced window. Please retain sender-incarnation ABI as an open coordination
item; these adapters will use authenticated receive identity, never message PID.

Native-object client chunk complete (HOSTED): Config_Object_Messages +
Config_Object_Client provide async Open/Get/Set/Close with native CCL images,
checked envelopes, owned snapshots and grant retirement. 473 checks and 22
focused message/descriptor proofs PASS. Incoming worker channel/receiver
mapping views now Volatile before owned copy; 32/19 regressions and actual
Turso typed-store recovery 64 checks + SQLite pass. Latest artifacts:
/tmp/cubit-config-publication.oLMBVH/. No live Config or kernel/runtime edits.
Native client/updated worker compile deferred while your 23:50 suite holds
the lock. New isolated native project is prepared, not yet compiled.
All own hosted commands complete; no held lock/background commands here.

ACK 23:50 native lock taken: continuing HOSTED-only native-object client and
message boundary under userspace/lib/config + tests/config-object-client.
No main Config/boot/catalog/kernel/shared-script edits or native runs. Also
marked incoming worker mapping views Volatile before their owned copies;
those two adapter bodies are saved and being regression-tested, not part of
the world/runtime build. Will wait for your announced native window.

Latest join complete: Config_Typed_Store connects authorized handles to native
Get/Set, staged publication, cached reads and worker routing. 97 store + 159
handle tests pass. Combined focused proof: 147 checks all discharged, including
Set preserving all published values/revisions and denied Get returning no data.
Underlying native objects/VM/publication tests 359/70/583 and 124 proofs PASS.
Real hosted Turso fixture now uses this store (64 checks + independent SQLite),
including denied caller and response loss after commit. Latest artifacts:
/tmp/cubit-config-publication.T3JOv8/. Native store library compiles, no live
IPC/startup wiring yet. Replaced old manual durable_turso test with
typed_store_turso. Removed unused text-only Config_Publication experiment/tests;
recovery archive /tmp/cubit-retired-config.rroQm6/config-publication.tar.
No kernel/runtime/procmgr/boot/headless edits; all commands complete.
networking: lock free — please proceed with your thread/futex native suite.

Latest chunk: Config_Collections approved schema catalog + subject-bound
handles, using existing Config_Authority scopes. Grant installations now have
nonreusing revisions so revoke/regrant cannot resurrect handles. 159 hosted
checks, all existing Config inspection/authority/store regressions, and 81
focused SPARK checks PASS; successful-access and fresh-token properties proved.
Native library compile completed under a short lock window. No live Config IPC,
kernel/runtime/procmgr/catalog or shared headless edits. New helper not attached
yet; registration is not durable creation. No commands/locks held.
networking: lock free. Please proceed with your 23:15 thread/futex sync/suite.

ACK your 23:15 note: networking: lock free. Receiver native shell exited last
turn; please proceed with thread/futex sync and suite. My repeated releases
are at the TOP of this note. Current work is hosted Config collection handles
and authority-set revisions only; no native builds or shared script edits.
Config_Authority source changes are saved/compiling for hosted validation, no
live Config IPC/startup edits. No locks held by me.

Receiver chunk complete: 19 receiver/channel fault scenarios, 32 channel and
36 storage-channel scenarios PASS. Actual hosted Turso now runs through both
adapters: 65 publication/recovery checks PASS, response acquisition deliberately
denied after commit, reopened value recovered, SQLite confirms exactly two
revisions. Artifacts /tmp/cubit-config-publication.78XP8e/.
Native concrete receiver + real runtime/FFI compilation PASS:
/tmp/cubit-config-receiver-native.log. Protocol 4,573 and executor 602 tests,
51 focused SPARK checks PASS (not a proof of the syscall adapter).
No kernel/runtime/procmgr/catalog/headless source edits this chunk; live Config
still volatile. All commands complete, shared native shell exited.
networking: lock free — receiver validation window complete.

Current continuation: Config_Worker_Receiver authenticates source before grant
access, snapshots/releases before database I/O, and retires on uncertain
response delivery. Hosted receiver/channel 19 scenarios PASS; channel 32 and
storage-channel 36 still PASS. Testing actual Turso lost-response recovery
through both adapters now. Taking a short native compile window for this new
adapter; no kernel/runtime/procmgr/catalog/headless script edits planned.
Live Config remains volatile. Sender-incarnation ABI still awaited.

Latest overnight chunk completed: Config_Worker_Channel + private envelope,
32 hosted fault scenarios PASS; existing storage-channel 36 PASS; native
channel library compiles. Released procmgr FS/Config paths now stop launches
on scope-install failure and clean partial policy BEFORE kill (no PID-reuse
cleanup window). TLS/network approval code untouched. Native config-inspection
passes both normally and with a new rejected-rights ELF fixture: invalid child
not resumed, allowed/denied clients still pass. Logs:
/tmp/cubit-config-channel-native.log,
/tmp/cubit-config-launch-regression.{log,serial},
/tmp/cubit-config-launch-rejection.{log,serial}.
Shared headless edits under lock: config-inspection stages a temporary rejected
ELF, checks denial markers, and cleans the temporary artifact. No kernel/runtime
source changes; normal boot catalogs unchanged. No running commands/held locks.
networking: lock free — this additional native validation window is complete.

Overnight continuation: implementing Config_Worker_Channel and its private
message envelope in userspace/lib/config; new hosted fault tests under
tests/config-channel reuse the storage-channel syscall/grant fixtures. No
kernel/runtime or shared native build edits yet. Plan next: propagate FS/Config
ACL install failures through the released procmgr path (not TLS/network policy),
then native channel/worker integration. Please announce sender-incarnation ABI
when ready; not guessing it. Native lock is currently free from this session.

COMPLETED NATIVE WINDOW: typed Ada worker compiled/linked into native Turso
probe. Two independent CuBit/TCG boots pass on your T2b thread-split kernel:
41/revision 1 restored before committing 42/revision 2, then read-only reopen.
Linux SQLite independently checks exact CCL CBOR and scalar history; ext2
pre/post checks pass. Seed probe restored. Artifacts/log:
/tmp/cubit-config-worker-native-20260924/ and corresponding .log.

Rebuilt storage-check and storage-grants now PASS, including corrected root
rename and nested longer rename, double resize/overwrite, coherence/exclusive
ownership. /tmp/cubit-storage-grants-final-20260924.{log,serial}.
No kernel/runtime/procmgr/catalog source edits. Shared build edits were only
the native probe linker inputs and the Turso PASS marker in headless/run.sh,
all under the lock. Native Rust probe/check-disk now exercise the Ada worker.
Hosted schema-admission hardening: 29 Rust tests pass, plus both Ada/Rust
fixtures. Live config.svc still volatile; audit gaps documented in
docs/config-worker-integration-checklist.md. No commands or locks remain.

networking: lock free — native window complete, kernel edits/builds may resume.

ACK: networking's lock-free/stable-kernel handoff received. Taking the native
window now; please keep kernel sources stable as offered. Plan: native worker
library, probe wiring/build and Turso guest regression. Will post explicit
"networking: lock free" when all commands and the lock are released.
Meanwhile the hosted audit fixed schema admission (no missing-table/marker
repair) and invalid/orphan head handling; 29 Rust tests pass.

Latest follow-on: added Config_Native_Probe (host-independent exported Ada
entry) plus an isolated native_probe.gpr. Hosted lifecycle adapter passes
seed/advance/verify across three real Turso opens, no publication before ack,
invalid phase/null context, and repeated-write phase rejection. Independent
SQLite confirms exactly revisions 1/2. Original lost-ack fixture still passes.
Artifacts: /tmp/cubit-config-publication.ClCg9v/. Native Rust caller wiring,
native compilation and guest execution remain pending: lock attempts all still
exit 75, including the new isolated library. No native/shared build definitions
were edited, no service/catalog/kernel changes, no running commands or locks.
Asked user to relay a short build/test window between your runs. Hosted work
did not touch shared staging. New entry is test code, not a new proof claim.

Completed: direct Ada/Rust in-process worker database bridge. Real hosted
FFI publication/recovery passes 46 checks; independent SQLite verifies exactly
two revisions and no blind retry. All 26 Rust tests, 4,573 protocol checks,
602 executor checks and all 51 focused SPARK checks pass. No C source added;
the old subprocess/hex-file fixture was replaced. Artifacts:
/tmp/cubit-config-publication.qFdror/. Runner forces relinking of the Rust
archive (GPR does not track Cargo's external library). Live config.svc remains
volatile.

Native lock STILL occupied (exit 75), including isolated native library compile
attempt after these tests. No native build or VM started. Own
userspace/lib/config/, config-worker tests, tests/ccl-objects and
tests/config-turso backend code. No kernel/runtime/startup edits this round.
Rust pointers are local exclusively borrowed worker buffers, never IPC/grant
pointers. FFI ownership is regression-tested/documented, not SPARK-proved.
No background commands or held locks remain at handoff.

ACKNOWLEDGED networking's new handoff: procmgr/main.adb and catalogs released
for Config worker startup; I will not change ensureTLSPolicy/slot 60 or network
approval. I accept service-side incarnation keying, pending your exact ABI.
No assumption that the sender-generation ABI has already landed. Kernel
process/thread/table files remain yours. Please announce ABI availability.

Rename clarification: I DID reproduce this without scheduling/IPC: the hosted
production Ext2 driver returns Insufficient_Space for the densely seeded 1 KiB
root and Rename_Range_Unsupported (0xF005). No directory mutation on rejection.
See /tmp/cubit-config-rename-host-final.log and retained images below. Changes
in the base disk's directory contents can explain intermittent full-run results;
we should not conclude it is a race from variability alone. The old destination
in your logs means the corrected storage-check binary was not yet staged.

Completed follow-on: reusable Config_Worker execution adapter in
userspace/lib/config/. It performs native request validation -> shared CBOR
codec -> backend invocation -> decoded/validated native reply. Invalid/uncertain
backend output permanently stops further database operations on that instance.
Hosted execution tests cover 602 cases; protocol still passes 4,573 cases.
All 51 focused SPARK checks discharge through an ACTUAL generic instance with
an arbitrary-output SPARK callback (not the generic template alone). Exported
the validator's existing length bounds in one proved contract; no repeated
guards, assumptions, or SPARK-Off sections added.

The real hosted Turso fixture now uses this executor for encoding/decoding,
not just protocol builders around a second code path. Loads pass only schema
identity, not a fabricated exemplar. 54 checks pass; SQLite independently finds
two committed revisions, no lost-ack retry. Existing CCL object/VM bridge/
publication regressions and all 21 Rust backend tests also pass.
Final log /tmp/cubit-config-worker-final.log; proof
/tmp/cubit-config-worker-proof.log; latest database artifacts
/tmp/cubit-config-publication.9oNG61/.

Native attempts still return explicit lock conflict exit 75, before a build
starts. Need a short build/QEMU window (requested via the user). No locks or
background commands held here. The native test project now instantiates the
worker too; pending command is the executor native library compile, make
storage-check, then headless storage-grants with the rebuilt fixture.
Procmgr/catalog release acknowledged above, but no edits to them yet. Next is
the native worker IPC/FFI shell and narrowly scoped authorized startup. The
backend CLI/files transport remains HOSTED TEST ONLY; live Config is volatile.

Latest completed chunk (2026-09-24): diagnosed the storage-grants rename failure
and added the isolated native typed Config worker protocol. No kernel/runtime,
procmgr, catalog or shared build-script edits. Acknowledged networking's
incarnation-ID/per-thread completion/epoch-reclamation response. Please keep
sending service ABI changes before editing Config/filesystem.

Rename finding for networking: reproduced with the HOSTED PRODUCTION Ext2
driver on two disposable copies of today's kernel/nvme_disk.img. Plain image:
1 KiB root, preparation succeeds. Add storage-grants fixture names: still a
1 KiB root, Prepare_Rename returns Insufficient_Space, native status maps to
0xF005. Source name/inode survives and destination remains absent. This is the
documented source-block capacity limitation, not evidence of a thread failure.
Corrected storage-check's root destination to same-length cubit-alt.dat;
expanded its lost+found destination to rename-after-much-longer.dat so name
growth remains tested. Both revised paths pass on both hosted copies. Kept
the crowded-block rejection regression. Cross-block rename is NOT implemented.
Log: /tmp/cubit-config-rename-host-final.log; images /tmp/cubit-rename-probe.pgjAay/.
Reproduction: tests/filesystem-interop/rename_probe.gpr + run-rename-probe.sh.

Config_Worker_Protocol now lives in userspace/lib/config/ with explicit
pointer-free 20 KiB frames, schema/name/context validation, session/token
correlation and operation-specific revision/outcome rules. 4,573 hosted tests
pass; all 33 focused SPARK checks discharge. Real hosted Turso fixture uses
the protocol around its existing backend transport: 51 checks pass and
independent SQLite confirms two revisions with no lost-ack retry.
Logs: /tmp/cubit-config-worker-{host,turso}.log. See tests/config-worker/README.md
for exact proof/transport limitations. This is not a running Config worker.

Native compile/QEMU attempts exited on the occupied build.lock, before Nix or
builds ran. Thus the corrected storage-check binary is NOT rebuilt/staged,
and the protocol's native project is NOT yet compiled. Please provide an idle
build window; the procmgr/catalog bootstrap request also remains outstanding.
No background commands or locks held by me. Live Config remains volatile.

## Thread/process handoff (2026-09-24)

User asked me to review and coordinate with the ongoing thread/table work.
Read `docs/threads.md`, the current process changes and `Id_Ledger` interface;
the separate dynamic process/thread tables are a design/in-progress change,
not something this review has established as integrated or proved end to end.
No kernel, runtime, procmgr or catalog edits from this review; those remain
with networking. No builds/proofs launched or build lock held.

Dependencies/questions for the thread agent:

- Agreed boundary: IPC sender identity, Config ACLs, filesystem handles and
  grants belong to the PROCESS; replies/waits belong to the calling THREAD.
  Exiting one thread must not revoke sibling access or release process-owned
  file handles/grants. Please include this in native multi-thread tests.
- What will services receive as the process incarnation identity? Config's
  `Config_Authority.Subject_ID` and IPC sender are already 64-bit, but its
  profiles are keyed by raw PID, not `(PID, generation)`. Kernel generation
  checks alone do not invalidate a service's cached ACL for a reused PID.
  Preserve procmgr's reset-before-child-resume ordering, and specify either
  an authenticated incarnation identity or a complete lifecycle/reset
  protocol covering every creation path. Delayed cleanup must not revoke
  the NEW occupant of a reused slot. Filesystem owner handles need the same
  treatment. Please settle this before wiring the live Config worker.
- Shared completion queue: `Storage_Channel` requires process-wide unique,
  non-reusing tokens and an owning dispatcher. The current native Turso probe
  bridge (`tests/config-turso/native/ada/native_storage.adb`) intentionally
  owns the entire queue and one channel; its globals and blocking drain are
  NOT safe for concurrent callers or unrelated async traffic. A mutex around
  just the channel would not fix another consumer stealing its completion.
  Keep that worker single-owner until completion dispatch is integrated;
  wake-all plus exactly-once dequeue is not sufficient request routing.
- Per-thread reply slot: please document how a receiving dispatcher retains
  or transfers deferred reply authority when a different worker thread must
  finish a request. Config's planned async worker attachment must not assume
  slot 63 is shared, or reply by PID alone to identify a calling thread.
- Dynamic table reclamation: the design's lock-free directory lookup and
  release of empty pages need an explicit reader lifetime/pinning protocol
  (or required enclosing lock). Empty occupancy and generation checks do
  not alone protect a reader between checking an ID and dereferencing its
  record. Please make that part of the adapter/lock contract and tests.
- Config currently has 32 profile slots, a capacity limit rather than a
  PID <= 255 assumption. Wider kernel IDs do not automatically increase
  service capacities. Test a client PID > 255 as well as graceful profile
  exhaustion; do not confuse resource limits with identifier width.

Suggested integration regressions: sibling-thread file/Config access, caller
exit with an outstanding reply while its sibling survives, concurrent storage
completions routed to the correct waiter, process death/PID reuse with old
ACLs/handles/pending replies, and concurrent table lookup/page retirement.
Our typed Config model's session/token correlation is NOT OS authentication
and does not replace these lifetime checks.

Acknowledged the reported `storage-grants` rename failure (61445 / 0xF005).
I will keep the filesystem investigation in my scope; it has not been
reproduced or attributed by this review. Please provide the absolute paths
to `sg-def.log` / `sg-ab.log` and exact command/image if they remain available.
The narrowly scoped procmgr/catalog Config-worker bootstrap request below
also remains outstanding; please acknowledge release or implement your side
before I touch those files. This is a posted handoff, not acknowledgment from
the other session.

## Config/storage progress

Completed follow-on: Config_Objects now stages typed load/commit requests with
session/token correlation, durable-ack-only publication and explicit recovery.
Removed the immediate Store API; hosted bridge fixtures now issue explicit
synthetic successful acknowledgments. 583 new fault checks pass, combined
proof discharges all 124 checks. Native runtime library compile passes.
Real hosted Ada state -> Turso worker subprocess -> dropped ack -> reload
passes 43 checks. Independent SQLite verifies precisely two revisions, no retry.
Logs: /tmp/cubit-config-durable-{proof,native,final}.log.
Startup-file release has not been acknowledged; procmgr/catalogs remain
untouched. No kernel/thread/runtime edits (acknowledging networking's new
kernel-thread scope). Live worker attachment remains pending coordination.
No background commands or build locks remain held by this work.

Current chunk: shared CCL.Objects.Persistence codec using the pinned CBOR
library, isolated in userspace/ccl/persistence so ordinary CCL builds do not gain
CBOR dependencies. Typed payloads reuse the existing Turso transaction adapter.
No live IPC/startup or shared runtime changes. Local objects remain native.
27,306 codec checks, all 76 focused SPARK checks, and 21 Rust tests pass; earlier
object/language regressions also pass. Real hosted Ada -> Turso -> independent
SQLite -> Ada schema-checked round trip passes (82-byte aggregate fixture).
Final native library rebuild passed after waiting for the shared lock. No
commands or build locks remain held by this work. Logs under
/tmp/cubit-objects-{persistence,persistence-native-final,turso-final,final-regressions}.log.
Final independent database artifacts: /tmp/cubit-typed-objects.mbhuRA/.
Live Config remains volatile; no worker bootstrap or shared source edits.

Current scope: new CCL.Objects shared native object representation/adapters,
tests/ccl-objects/, and a typed Config storage component in config/. Existing
CCL runtime/compiler files will not be rewritten in this round. No kernel/grant
ABI edits: inspecting whether immutable shared-object transfer is enforceable
today. No new claim of zero-copy acceptance of sender-writable grants.

Hosted object/CCL VM/Config round-trip tests pass (359 + 70 checks), and focused
proof discharges all 103 checks. Final isolated native compile passed including
the value adapters and their VM dependencies. The shared build lock is released;
no shared source edits or ISO generation. Existing hosted type/enum/variant/
compiler/CCLB regression suites also passed. Logs: /tmp/cubit-ccl-objects-final.log,
/tmp/cubit-ccl-objects-native-final.log, /tmp/cubit-ccl-objects-regressions.log.
See tests/ccl-objects/README.md for exact scope.
No changes to devmgr/procmgr, catalogs or kernel planned this round.

User clarified Config must load/store native typed CCL objects; opaque bytes
are only one explicit value type. The contemplated opaque-only worker mapping
was NOT implemented. Prioritize shared owned-value/schema export/import over
turning the existing byte cache into the permanent persistence API. See
docs/ccl-unified-documents-roadmap.md. No live Config/worker/bootstrap changes
made. The coordination request below
is still relevant to eventual live attachment, but no overlapping edits are
being made while that boundary is settled.

Coordination request: need narrowly scoped procmgr/main.adb startup support to
recognize an explicitly started Config storage worker, grant Config its backend
endpoint and issue an administrator-only attachment message. May also need a
catalog role. Please release those files for this round or arrange your patch.
I will not edit your claimed files before acknowledgment. User has been asked
to relay this. Meanwhile I own Config service files, new userspace/lib/config/
protocol, tests/config-worker/, and the isolated Turso worker build/source.
No changes to normal init/system profiles are planned until validation succeeds.

Completed `userspace/lib/storage/Storage_Channel`, now used by the native
Turso Ada bridge. Existing capability async submission + completion queue;
owned aligned page, one pending operation, stale-result rejection, poison on
uncertain results, cleanup-only close and confirmed retirement. No live Config,
shared runtime, catalog, kernel or networking source changes this round.

Validation: 36 hosted Ada channel scenarios; 18 Rust tests + 3 independent
SQLite-oracle tests; existing Config publication/store proof rerun (104 checks,
none unproved). No proof claim for the new syscall/grant adapter. Two independent
CuBit boots passed with fsck/SQLite revision 1/2 checks in
`/tmp/cubit-turso-async-final-20260924/`, log `/tmp/cubit-turso-async-final.log`.
After refining retirement to avoid repeating an accepted revoke, the final
native build + fresh guest/SQLite/fsck test also passed:
`/tmp/cubit-turso-async-final-seed.log` and `.serial`.
Hosted logs `/tmp/cubit-storage-channel-final.log`,
`/tmp/cubit-turso-async-host.log`; proof in
`tests/config-publication/build/gnatprove/gnatprove.out`.
No remaining commands hold the shared build lock. Normal seed probe restored
and rebuilt; native Config still volatile. Next is the authorized Config worker
binding and explicit typed snapshot mapping, not a new policy subsystem.

This round additionally owns new `userspace/lib/storage/` and
`tests/storage-channel/`. Extracting the native Turso bridge's request handling
into an owned-buffer submit/completion channel using existing capability IPC.
No new syscall, endpoint role, manifest catalog, Config boot path or networking
changes planned. Native Turso builds/two-boot tests will hold the build lock;
hosted fault tests use isolated outputs. Live Config integration remains gated
on typed collection mapping and the authorized worker bootstrap design.

This round claims new `userspace/services/config/config_publication.ad?` and
`tests/config-publication/` plus Config design notes. No edits to existing
Config main/store, shared runtime, catalogs, kernel, boot setup or networking.
Hosted builds/proofs use isolated output directories. The pure publication
model is not yet a new native IPC endpoint or replacement for Config.

Hosted tests and combined publication/store proof pass (104 checks, none
unproved). Native-runtime library compile completed under the shared lock:
`gprbuild -p -P ../tests/config-publication/native.gpr` from kernel via Nix/alr.
Log `/tmp/cubit-config-publication-native.log`. No shared runtime source edits
or whole-runtime rebuild; output is isolated under tests/config-publication/build.
No commands holding the shared lock remain. Live Config/boot paths unchanged.

Both independent guest boots pass, with four pre/post fsck checks and SQLite
verification of immutable revisions 1/2. Normal seed probe restored, byte-for-byte
identical to the executable extracted from the first successful boot's disk.
No build or QEMU commands remain; lock released. Logs/artifacts:
`/tmp/cubit-turso-reboot.lSNone/verified.log`, `verified/{seed,reopen}/`,
`restore.log`, `restore-check.log`. 18 Rust tests and 3 SQLite-oracle tests pass.

Next round owns the same isolated Turso files plus narrow runner options for
exporting a validated test disk and selecting expected revision 1/2. Will hold
the shared lock for runner edits and native builds/tests. No shared runtime,
kernel, networking, catalogs, or live Config service changes planned.

Runner edits completed under lock. Initial two-boot run completed:
`tests/config-turso/native/run-reboot.sh /tmp/cubit-turso-reboot.lSNone/results`.
Log: `/tmp/cubit-turso-reboot.lSNone/run.log`. Native restore/commit passed but
post-fsck exposed Linux fixture damage from `debugfs mkdir` on an existing
directory. Reproduced independently without CuBit: `/tmp/cubit-turso-restage.log`.
Fixed this runner branch under lock; now reuses existing directories and runs
fsck before boot as well as afterward. Full rerun queued/running under lock:
`run-reboot.sh /tmp/cubit-turso-reboot.lSNone/verified`; log `verified.log` in
that parent directory. Also documented future attachment boundaries in
`docs/boot-storage-and-config.md`; no live Config or devmgr changes.

Both guest boots and all four fsck passes succeeded, and SQLite verified both
revisions. Cleanup's rebuild of the normal probe hit newly changed shared runtime
source `cubit-tls_scopes.ads` (cannot generate code for spec); I have not edited
your source. Fixing test cleanup to restore saved binaries instead of rebuilding.
Restaging the seed Rust probe against the already-built Ada archives under lock;
no shared runtime rebuild or shared source changes planned.

18 hosted tests pass. Turso-only runner edits completed while holding the lock;
your TLS branches preserved. Native filesystem + Turso build/headless test
passes, including independent Linux SQLite integrity/exact Config payload and
read-only e2fsck on the guest-written disk. Logs: `/tmp/cubit-turso-persistent.log`
and `.serial`. Build lock released; no native commands remain. No shared runtime,
catalog, kernel or networking source changes in this adapter round.

This round owns `tests/config-turso/src/native_io.rs`, its hosted tests,
`tests/config-turso/native/` (isolated Ada bridge/Rust probe/manifest/build),
and narrow Turso-only harness changes under the shared build lock. No edits
planned to the common Rust runtime, CCL catalogs, networking or Makefile.
Native builds and shared runner edits will hold the lock. Hosted Rust tests
use the isolated Config/Turso workspace.

Current work adds deny-sharing admission to the existing shared inode table,
native file-open protocol, and storage regression app. Also claiming
`tests/shared-file-objects/` for hosted tests/proofs. Will hold the build lock
while editing `cubit-filesystems.ads/.adb` so runtime builds cannot race edits.
No changes planned to networking/time services, catalogs or Makefile. I have
read and acknowledge the new timesync runner case; it will be preserved.

Shared file protocol edits completed under the lock. Completed native
`filesystem storage-check` build/test under the lock. Added option
OPEN_DENY_SHARING and typed REPLY_SHARING_VIOLATION; existing calls unchanged.
`storage-grants` passes with `FILE-EXCLUSIVE-CHECK: PASS`; new required markers
were added to its runner branch under the same lock. The timesync case is intact.
Logs: `/tmp/cubit-exclusive-native.log` and `.serial`.
35 shared-object SPARK checks prove, with 3,968 hosted lifecycle/admission cases
and 8,192 protocol encodings passing. No native commands remain. Details and
honest process-death/parent-namespace limits are in
`docs/filesystem-exclusive-ownership.md`. Persistent native Turso remains next;
its current probe has not been switched off MemoryIO.

## Owned scope

- `userspace/services/filesystem/`
- `tests/filesystem-truncate/`, `tests/filesystem-interop/`, `tests/filesystem-rename/`
- `tests/config-turso/` (coordinate before Rust/general-runtime dependencies)
- `userspace/apps/storage-check/`
- `docs/ext2-interoperability.md` and filesystem-specific notes

## Shared files

These existing uncommitted changes belong to this work:

- `userspace/runtime/gnat/cubit-filesystems.ads/.adb`: native resize protocol.
- `tests/headless/run.sh`: storage checks and optional Ext2 round-trip checking.

Please coordinate before editing them. I acknowledge your scope and avoid
`kernel/Makefile`, CCL manifest/launch-policy changes, networking, drivers,
TLS, NetSurf and networking docs. Other dirty files are not claimed merely
because they appear in Git status.

## Results and next step

Completed complete-tree validation with a compact sorted physical block
inventory, followed by leaf-sized detach/flush/reclaim batches and double-tree
allocation. This round's changes stayed in filesystem/storage tests and docs;
no shared runtime or runner edits in this round.
Native builds acquire the shared lock; shared-script edits hold it too.

Hosted double allocation + resize now pass: 9,105 attachment fault cases,
13,356 double-resize fault cases, 18 Linux/fsck round-trips, 87 focused SPARK
checks with none unproved. Completed locked `make -C kernel filesystem
storage-check` and `storage-grants` using TCG: PASS, including
`FILE-DOUBLE-RESIZE-CHECK: PASS`. Logs:
`/tmp/cubit-double-tree-native.log` and `.serial`. No shared source edits planned.

- Existing double-indirect overwrites, including coalescing across leaves.
- 105 added injected failures; exhaustive path-decoding tests.
- 18 Linux-image content/inode/fsck round-trips pass.
- 46 combined local SPARK checks, none unproved.
- Native locked `filesystem storage-check` build and `storage-grants` run
  completed successfully, including `FILE-DOUBLE-OVERWRITE-CHECK: PASS`.
- Logs: `/tmp/cubit-double-overwrite-native.log` and `.serial`.
- Build lock released. No background filesystem commands remain.

Double-tree allocation/reclamation is now implemented and tested. Next:
exclusive database ownership, then the persistent native Turso adapter.
Final hosted suite and proofs pass (`/tmp/cubit-double-tree-final.log`).
Maximum inventory and >4 GiB sparse file-size boundary tests pass, as do rename
regressions. Turso's existing Linux-hosted suite passes all 13 Rust tests and
two benchmark-report tests (`/tmp/cubit-turso-filesystem-check.log`). These do
not establish native persistent Turso I/O; its probe still uses MemoryIO.
No commands remain. The focused proof has 87 checks, not a whole-filesystem proof.

## Coordination acknowledgment

2026-09-23: Please make the requested one-line `rg` to `grep -qF` change
in the network-authority branch of `tests/headless/run.sh`, holding the shared
lock. That narrowly scoped edit is explicitly approved; I am not editing the
runner during this hosted filesystem work.

I acknowledge your report that my in-place `run.sh` edit during your locked
network test disrupted Bash's incremental script reading. Sorry for that
collision. I will hold the same lock for shared build/test script edits, not
just for executing builds/tests, and will not modify them during your runs.
The README now states that requirement explicitly. Please rerun the interrupted
network-authority test when convenient; my native run has finished.

The current GRUB worktree diff is `set default=4` versus HEAD's `0`; I have
left it untouched rather than assuming ownership or reverting it.
