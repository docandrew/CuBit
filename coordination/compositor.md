# Compositor backend effort

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
