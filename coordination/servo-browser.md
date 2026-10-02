# Servo browser continuation

2026-10-02 Penny spacing queued, NOT applied: sharedlock75 and incoming Desktop/Mesa compile freeze honored. Prepared narrow Widgets navigation reload/centering plus browser tabs/dialog spacing; shared UI/runtime/fonts unchanged. No ownjobs/waiters. Will apply after freeze release underlock.

2026-10-02 Penny54254 TERMINAL0: native build/secondary-stack link check PASS; two disposable native boots Ctrl+D/save/reload/edit generation2 and exact favicon bytes PASS, clean browser close/fault scan PASS. Artifact /tmp/nix-shell.LrFBOQ/cubit-browser-bookmarks-vdoinedb; screenshots /tmp/penny-about-0.png and /tmp/saved-0.png visually inspected. Launcher labels now Penny in system.ccl +hardware system-live.ccl; Config/build identities stable. Sharedlockreleased, no ownjobs/waiters. Session/tab restoration remains unimplemented; current evidence is bookmark persistence only.

2026-10-02 Penny54254 LIVE sharedlock: launch labels updated, native browser build + two-boot bookmark/favicon regression, including About screenshot. Logs /tmp/cubit-penny-build.log and /tmp/cubit-penny-native.log. Own browser sources frozen; no shared UI/runtime edits.

2026-10-02 Desktop completion-drain compile freeze: incoming compositor notice received. Shared UI/runtime sources remain stable; no edits or build jobs from this task. Penny-only artwork/chrome edits complete and isolated compile passed. Native browser validation and launch-label update remain pending shared lock; no own waiter.

2026-10-02 Penny logo: user selected copper globe01. Saved generated transparent master plus editable filter-free SVG and five PNG sizes in assets/penny; reproducible render.py emits static Penny_Artwork16/32 bitmaps, used in chrome/About. Isolated native chrome compile85377 PASS. Native integrated build/two-boot bookmarks/screenshot and launch-label changes remain pending sharedlock (latestnonblock75); no own nativejobs/waiters.

2026-10-02 Penny naming: user selected Penny. Own browser chrome/default page/About/globe branding; retain internal Servo build/config identities. Narrow launch label updates in system.ccl and hardware system-live.ccl next under lock. Bookmark fixed-buffer serialization native verification still pending; storage uses alternating checksummed browser-0.dat/browser-1.dat snapshots, not replace-rename.

2026-10-02 bookmarks implementation: own new Servo_Bookmark_Model/Servo_Bookmarks native packages, Servo_Session integration, cubitshell bookmarks.rs bridge, new tests/browser-bookmarks.64bounded entries with nested folders, native modal editor/tree, menu/Ctrl+D; versioned bounded file only /Bookmarks/browser.dat via sync+replace. No shared toolkit/Config/service edits; preferences remain Config. Shared native build later underlock.

2026-10-02 compositor reports Desktop/native observer compilation complete and releases UI source freeze. Its final private Observatory VM still holds shared build lock. No own edits/jobs/waiters; UI sources remain stable.

2026-10-02 Desktop metrics compile window: received compositor request to keep shared UI stable. All UI/Combo/Tree edits complete;98471 terminal, no edit/build in flight and no own jobs/waiters. Shared UI sources frozen for upcoming Desktop metrics compile until owner reports compilation finished.

2026-10-02 disabled98471 TERMINAL0: flat disabled glyph moves1px right, enabled/pressed unchanged. Hosted preview compiled/rendered/visually checked /tmp/cubit-disabled-arrow-centered.png. Shared lock released, no livejobs/waiters; no native rebuild claim.

2026-10-02 disabled arrow correction: user requests glyph1px right for flat disabled frame. Shared Draw_Arrow_Button cx conditional only; enabled/pressed positioning unchanged. Hosted tree preview under lock next.

2026-10-02 sharedarrows72668 TERMINAL0: combo+vertical/horizontal scrollbar call shared Draw_Arrow_Button, duplicated arrow/frame code removed. Glyph1px left; same raised/pressed/disabled states. Hosted200clip/3380tiny +combo100pointercycles/609tiny/10palette-density +existingeditor/scrollbarPASS. /tmp/cubit-shared-arrow-buttons.png visually inspected. No native rebuild claimed; sharedlock released,no jobs/waiters,sources stable.

2026-10-02 user explicitly requests shared scrollbar/combo buttons: own UI ads/adb additive Draw_Arrow_Button +direction enum and Combo_Boxes call; removes duplicated glyph/frame code. One-pixel left optical shift, shared pressed/disabled/raised styling. Apply/build hosted under shared lock; no native staging.

2026-10-02 flush18525 TERMINAL0: gutter highlight replaced with shadow edge; track uses full trackFrame so thumb meets buttons at both ends (vertical/horizontal). Hosted200clips/3380tiny +existing editor/scrollbar interactionsPASS; unusedwarnings suppression only. Preview /tmp/cubit-scrollbar-flush.png visually inspected. No native rebuild claimed; lock released, no jobs/waiters, source stable.

2026-10-02 scrollbar flush ends: user requests remove bright far edge and thumb gap. Own UI body gutter strokes neutral shadow, vertical/horizontal travel fills trackFrame (no2px end inset). Hosted polish +existing editor scrollbar regression and preview under sharedlock next.

2026-10-02 gutter85574 TERMINAL0: vertical/horizontal trough now slightly darker fill+single recessed edge; fullwidththumb/input geometry unchanged, one fewer stroke per gutter. Hosted200clip/3380tiny PASS; light/dark preview visually inspected /tmp/cubit-scrollbar-gutter-refined.png. No native rebuild claimed; sharedlock released, no jobs/waiters, source stable.

2026-10-02 scrollbar gutter visual pass: own existing visual-only UI body scope, vertical/horizontal track fill+stroke only. Single recessed edge and subtly darker trough instead of bright double bevel; geometry/input unchanged. Apply/build hosted under nonblocking shared lock.

2026-10-02 combo optical80462 TERMINAL0: glyph1px left, button geometry unchanged. Hosted preview compiled/rendered and visually inspected, /tmp/cubit-combo-arrow-optical-center.png. No native rebuild; no jobs/waiters, sources stable.

2026-10-02 combo glyph optical centering: user requests down arrow1px left. Own Combo_Boxes glyph x only, button unchanged; hosted preview next. No native job.

2026-10-02 alignedcombo12772 TERMINAL0: combo100pointercycles/609tiny/10palette-densityclips PASS. Arrow16px square,4px right margin, integer center matches scrollbar; light/dark preview visually verified /tmp/cubit-combo-scrollbar-aligned.png. No native rebuild claimed; no jobs/waiters, sources stable.

2026-10-02 combo alignment: own Combo_Boxes body only. Match gallery scrollbar16px square,4px container margin and integer-centered7px arrow; retain raised button primitive. Hosted combo regression/preview next, no native build, no UI compile overlap observed.

2026-10-02 leaf88025 TERMINAL0: hosted preview build/render PASS, light/dark leaf icons and iconless captions visually checked. Leaves shift13px left into unused disclosure slot; branches unchanged. /tmp/cubit-tree-compact-leaves.png. No native rebuild claimed; no livejobs/waiters, source stable.

2026-10-02 leaf spacing: user requests no reserved disclosure slot on leaves. Own cubit-ui-trees.adb icon/caption origin only; shift leaves13px left while preserving depth/branch lines/hit bounds. No active UI compilation observed. Isolated hosted preview next; no native staging.

2026-10-02 disclosure94175 TERMINAL0: hosted tree preview build/render PASS, light/dark visually inspected. +/- boxes now uniform muted 1px closed outline (no bevel), same fill/stroke count. /tmp/cubit-tree-enclosed-disclosures.png. No native rebuild claimed; no live jobs/waiters, source stable.

2026-10-02 disclosure outline: user requests flat, fully enclosed 1px tree +/- boxes. Own cubit-ui-trees.adb Draw_Disclosure only; replace split bevel with uniform muted border, same size and primitive count. Observed native owner already in QEMU (no UI compile); isolated hosted tree preview next, no native staging/build job.

2026-10-02 Devices82461 TERMINAL0 native fixPASS: six down/up+hover/leave cycles exact treepixelstability, immediate scroll, initialtree roundtrip, nofaults, base/inputhashesunchanged. Baseline6233 reproduced17,480 pixel corruption. Devices handles scrollbar before rows +visible-page thumb. Combo39577 TERMINAL0 hosted100pointercycles/609tiny/10palette-densityclipsPASS; arrow18px/inset2px. Screenshots /tmp/cubit-devices-hover-fixed.png and /tmp/cubit-combo-arrow-tree.png. No livejobs/waiters; lockreleased, sourcesstable. User asks smoothscroll possibility: existingclip/partialrender supports pixel offsets, Devices currentlyrows; no smoothscroll implementation claimed.

2026-10-02 native6233 baseline TERMINAL0: reproduced Devices old rows after scrollbar then 17,480 tree pixels change on hover/leave. Fixed Devices render order (consume scrollbar before rows) + actual visible-page thumb sizing. Native retry nonblock busy; no waiter. Combo arrow18px/inset2px host39577 LIVE, isolated outputs. Need next shared-lock native rebuild/six-cycle pixel regression; new tests/devices-hover/run.py.

2026-10-02 Devices hover investigation: claim Devices main.adb render ordering and own tests/ui-polish + new tests/devices-hover fixture; combo arrow 24 -> 18px. Suspected retained scrollbar value consumed after rows painted; reproducing native before fixing. No core UI/App/Controls changes. Native baseline under nonblocking build lock; no .build-workspaces dependencies.

2026-10-02 compactcombo/tree86822 TERMINAL0:combo100cycles+609tiny+10palette/densityclips PASS, existingeditor/scrollbarcommandsPASS with unrelatedunusedwarnings suppressed. Defaultcomboheight26px (~81%of32). Existingtree renderer shown unchanged /tmp/cubit-tree-preview.png; fullgallery /tmp/cubit-ui-combo-compact.png. Prior27172 nativeCombo compile+Servo linkPASS; no newnativebootclaim. Fullwidthaccent/14pxroundradio/field+2px/widerthumb allimplemented. Nojobs/waiters; sharedsourcesstable.

2026-10-02 combo27172 native compile+Servo link TERMINAL0.8435 combo10densityclips PASS afterfix layout-vsdamageclip; existingeditor buildblocked unrelated display_geometry unusedwarnings under-gnatwe. Userrequests26pxcombos +treepreview.86822 sharedlockLIVE heightconstant/gallery+existingTreeViewpreview+hostcombo+editorchecks(-gnatwU unusedwarning suppression only). No Tree implementationedits. No .build-workspaces dependencies.

2026-10-02 combo19969 TERMINAL0 hosted100retainedcycles+609tiny+overflow/wheel/flipPASS; general200/3380PASS; screenshot /tmp/cubit-ui-combo.png shown.77529 finalhostLIVE adds10popupdensityclips +existingeditor/scrollbarregressions. Nativecompilepending sharedcleanup1200774; no ownwaiter. NewCombo childAPI only, existingUIbody stable. No .build-workspaces dependency.

2026-10-02 finaldetails91555 TERMINAL0 fullhostedPASS: fullwidthtabaccent,14pxcachedcircularradio,textfield+2px. Useraddsnativecombo/fullwidthscrollthumb. Own new child cubit-ui-combo_boxes.ads/adb+tests/ui-polish combo_tests; no existingpublicAPI edits.24610 initialcompilefailed reservedDelta; fixedWheel_Delta, retrypending. Scrollcrossaxis inset removed underlock. No jobs/artifacts depend on .build-workspaces (cleanup coordination).

2026-10-02 status26397 TERMINAL0:200palette/density/clip+3380tiny+status/keylane checksPASS. Hostedpreview /tmp/cubit-ui-status-bevel.png inspected. SharedUIbody stable; no livejobs/waiters ornative/stagingchanges. Statusbar sunken2-edge frame restored peruser.

2026-10-02 statusbevel APPLIED after90614viewerPASS; confirmed active1112749 is read-only USBQEMU1117124 and queued1115590 viewer has nochild/WRITE* (noUIcompile), so sourceedit completed beforequeuedcompile. Only Draw_Status_Bar body:2thin insetborders+3px textclip. Source stable now;26397 isolatedhostpolish checksLIVE. No sharedstaging/nativejob. /tmp/cubit-status-bevel.log.

2026-10-02 user requests sunkenstatusbar. Small cubit-ui.adb Draw_Status_Bar edit prepared but NOTapplied (nonblock75; Mesa/log holder+Observatoryviewer waiter). Awaiting nextsource-idle lock toapply+isolatedpolish test/gallery. No nativejob/waiter orsourceeditsyet. /tmp/cubit-status-bevel.py.

2026-10-02 finalv30 native21701 TERMINAL0 full180s4CPU TCG interaction+faultscanPASS. Sharedlockreleased, no ownjobs/waiters. User approved framedgradient +underlinedmnemonics; nativefilemenu screenshotshown. Finalinputs /tmp/cubit-servo-browser-v30-inputs.sha256, log /tmp/cubit-servo-browser-v30-run.log. Hostedgradient200/3380/menu/font/144SettingsPASS; underline/gradientpreservationtestsPASS. Docsupdated. Allvisualsourcesstable.

2026-10-02 mnemonic86723 TERMINAL0 nativeMenus keyboard/pointer/clip/gradient/underlinePASS; gallerybuilt/shown. Native21701 LIVE sharedlock finalv30 Desktop/browserbuild+180s gate; sourcefrozen. /tmp/cubit-servo-browser-v30-run.log. Finalstyleframedgradient+underlines, raisedbuttons, top/side-onlytabs, insetlists.

2026-10-02 gradient7078 TERMINAL0 fullhostedPASS. Root1068202 observatory/CCL-only native gate nowownslock; no UI compile perowner note. Mnemonic body+tests applied,86723 isolatedhost LIVE. No API/glyph/cache changes. Nativev30 nextlock pending, no waiter. /tmp/cubit-menu-gradient-final-host.log.

2026-10-02 user chose framedstrip +subtleverticalgradient, added visiblemnemonicunderlines. Gradientapplied under7078 sharedlock hostedchecks LIVE. Uses existing opaque rowgradient+transparenttext; no glyph/cache/API changes. Mnemonic bodyedit prepared for nextsourceidle, matches controller keys. Nativev30 pending finalhost.

2026-10-02 menuoptions65332 TERMINAL0 updated200clip/3380tiny/menus/fonts/144SettingsPASS. Screenshot /tmp/cubit-menu-options.png (3choices, idle/hover/open, light/dark), correctedtabs /tmp/cubit-ui-tabs-no-bottom-bevel.png. User selection pending; nativev30 notrun andno lockwaiter. Latest productionmenu flatbaseline, tabs no bottombevel, list/scroll inset. Earlier v29nativePASS predatesstrongerdepth. Alljobs terminal.

2026-10-02 latestuser wants NO bottomtabbevel and menuoptions (buttons rejectedfor menus). Implemented top/side-only tabs, flatmenu baseline withhover/open outline;65332 isolatedhostLIVE includes3-optionactualprimitivegallery. Nativev30 deliberately deferred until userchooses menu; no nativewaiter/staging. Prior200/3380/menu/font/SettingsPASS, updatedchecks underway.

2026-10-02 raisedmenus/tabs/sunkenlist95281 TERMINAL0:200clips+3380tiny+lanes, menus/fonts/144SettingsPASS. Gallery /tmp/cubit-ui-raised-controls.png shared. Finalbodies stable; no API/glyph/cache edits. External1010769 stillholdslock through ccl-workspace300s thenlog-authority180s; v30pending no waiter.

2026-10-02 user explicitly wants raisedmenu/tabs +sunkenlist; final UI body/widgets edits applied during external alreadybuiltQEMU (confirmed PID1014025), no concurrentUIcompile. Hosted95281 LIVE isolatedoutput; addsviewport to200clips/3380tiny. PublicAPI unchanged. v30native notstarted; pending sharedlock afterexternaltests.

2026-10-02 v30 blocked nonblock75 by external PID1010769: logstore/log-check/ccl-workbench/nvme_disk build +300s ccl-workspace +180s log-authority, no corresponding current ownernote. No ownwaiter. Allfinaldepth hostedtestsPASS; UI sources stable; next native180s stillpending.

2026-10-02 finaldepth23512 TERMINAL0:190clips+3211tiny+lanes, menus/fonts/144SettingsPASS; finalgallery /tmp/cubit-ui-depth-final.png shared. UI source edits complete/frozen. v30 nonblock75 twice while observer native gate holdslock; no native job/waiter. Request next window for incremental Desktop/browserbuild+180s final screenshots. v29 PASS used earlier weaker bevel; no claim finaldepthnative yet.

2026-10-02 depth95243 TERMINAL0 allhosttestsPASS. Rootobserver ownslock; declared builds onlyobservatory, noUIcompilation, so final checkbox/slider/tab accents +isolated hostedchecks proceeding without staging (per disjointhost rule). No native lockwaiter; pendingv30 afterrootgate. /tmp/cubit-ui-depth-final.log.

2026-10-02 v29 retry48871 TERMINAL0 full180s/native interaction/fault scan PASS. User requests stronger depth after hosted preview: own95243 LIVE sharedlock applies two-edge bevel/rim +small opaque button edge bands, field inset; hosted regression underway. No staging changes until later ownlock. Preparing v30 final native; compositor observer may use next lock window, source APIs/glyph paths unchanged.

2026-10-02 native57420 TERMINAL1 after successful Desktop/browser builds: shared base nvme_disk.img absent. Retry48871 LIVE sharedlock using private verified /tmp/cubit-servo-v29-base.img (six staged services/apps +startup; runner installs fonts/pages/TLS). No user/base disk changes. Full180s v29 gate pending, /tmp/cubit-servo-browser-v29-retry.log. UI sources stable.

2026-10-02 UI polish hosted62593 TERMINAL0: 190 clip/density/theme +3211 tiny bounds +status/key-value isolation, menu/font/144Settings regressions PASS. Final subtle raised/inset bevels and continuous dividers applied. Native57420 LIVE shared lock: desktop-metrics/browser build +180s v29 gate/screenshots; UI sources frozen. /tmp/cubit-servo-browser-v29-run.log.

USER steering: restore subtle3D button/text-control bevels for usability. Prepared revision preserves singlefill+single1pxstroke budget; raisednormal/insetpressed and textfield, no effects. Spacing/clipping/status/menu fixes retained. Awaiting graphics editlock then applyfinaladjustment.

2026-10-02 graphics boot-logsv13 may take next lock window. Own93393/26097 TERMINAL (productionUIcompiles;190clip+3211tinyPASS,menustestsPASS; oldfonttabcolorassertneeds updated semanticoracle). Small continuousdivider/source correction preparedbut NOT applied (nonblock75), UI sources stable until nextownlocked edit. UserNUC repair priority respected. No ownlockwaiter.

2026-10-02 toolkitpolish93393 LIVE sharedlock; coordinated source edits nowapplied. Hostedgallery/regressions underway. UI source frozen for currentcompile; /tmp/cubit-ui-polish-host.log.

2026-10-02 polish source NOT applied yet: nonblock75 twice, lockowner910318 ccl-workbench+ccl-workspace300s gate (hostps confirmed). Root UI ACK valid but waiting source-idle sharedlock. /tmp/cubit-ui-polish-run.sh prepared; newtests only edited.

2026-10-02 root19317TERMINAL0/source window RELEASED and broadvisualACK received. Applying prepared UI polish under sharedlock, newgallery/170clip+2873tiny cases and operationbudget, existingmenus/fonts/settings-renderer. No glyph/raster/cache/API/protocol/Main edits. /tmp/cubit-ui-polish-host.log.

2026-10-02 USER requests general toolkit visual sweep: spacing/borders/padding/overlap, speed first, no effects. REQUEST compositor owner extend prior UI visual ACK to visual-only cubit-ui.adb (frames/buttons/tabs/menu/status/panes/fields/check/radio/list/progress), cubit-ui-widgets.adb and completed nativeMenus body; no glyph/raster/cache/API/protocol/Main changes. Plan fewer flat primitive calls, shared renderer preserved, hosted clip/density/pixel-operation tests + native screenshots. Preparing new tests/ui-polish gallery/baseline while awaiting scope acknowledgment. No shared UI edits/buildjob yet.

2026-10-02 v28 session50001 TERMINAL0 sharedlock RELEASED to root65804; no Servo jobs/waiters. Native180s finalfaultscan+menu-modalkeyboardhandoff+outsideclickblocking+Configreopen PASS. v27 full360s menus/16tabs/4windows passed. HostedMenus/address/geometry pass,39proofresults0unproved. Finalbuiltmanifest onlyBookmarks/Downloads writablePASS. Browser97d75aa3..., screenshotsv28-file-menu.png/v28-edit-menu.png shared withuser. Docs updated. Userwants screenshots asfeaturesland. No further native job planned.

2026-10-02 v28 session50001 LIVE sharedlock180s, focusedmenu/modal handoff. Production sources frozen; /tmp/cubit-servo-browser-v28-run.log. v27 full16tabs/4windows menubarevidencePASS.

2026-10-02 v27 session83111 TERMINAL0 full360s gate/faultscan+featuresvalidator PASS. Applied own menuinput fix clearing mnemonic suppression when modal takes ownership and canceling old chrome capture when menu opens. Preparing focusedv28 native180s onecycle+newkeyboard/mouse/typing regression. No rootwaiter in latestnote; nonblock lock only.

User preference: send native screenshots as browser features land. File/Edit captures fromv27 shared inline. v27 interactive phase PASS; awaiting360s finalscan before small menu-modal keyboard suppression fix and focusedv28. No new shared UI/Main edits.

2026-10-02 v27 session83111 LIVE sharedlock360s, final menu sources frozen; /tmp/cubit-servo-browser-v27-run.log. Hosted geometry/address/nativeMenus passed; geometryproof30197 finishing.

2026-10-02 v27 launch nonblocking BUSY75; no nativejob/waiter. Browser menubar source final fill-offset correction; hosted/proof outputs isolated. Request nextlock window after owner variant repair.

2026-10-02 menubarbuild49535 TERMINAL0 nativebuild passes. File/Edit/View compiled; fixture usesFile>NewTab,Edit>Settings(mouse/AltE+S),View>Reload, F10+Right/outside dismissal. Preparing v27 native360s+hostgeometry/proof, no shared source edits.

2026-10-02 user requests actual browser File/Edit/View menubar. Own ServoSession+geometry integrate native Menus API; remove Settings/Window+ toolbar buttons, Edit>Settings modal. Add24px menurow. No shared UI/Main/source changes. Nativebuild/test pending nonblockinglock; updating owned fixture geometry.

2026-10-02 v26 session30672 TERMINAL0, sharedlock RELEASED to root94329; no browser jobs/waiters. Full360s nativeheadless/faultscan PASS, featurevalidator PASS16tabs/2orientations/4windows/capacity/isolatedclose/reuse. Actual titlebarX with Settings open, sibling survival, Config browser-reopen allpass. Browser7293c4fe..., Desktop9bf69450... unchanged duringgate. Finalhost75260 PASS manifest onlyBookmarks/Downloads writable +1000actualaddresscycles +validatorpositive/4negativecaptures. Toolkitmenushosttests pass, not browserwired. Docs updated; no further nativejob planned.

2026-10-02 v26 LIVE: actual fourth-window TITLEBAR X with SETTINGS OPEN passed169.443s; original DOM wheel/Esc remainsresponsive173.562s; closedslot reused174.418s. Remaining siblingclose/Configreopen/finalfaultscan pending. Sharedlock stillheld.

2026-10-02 v26 session30672 LIVE sharedlock,360s gate; sources frozen. /tmp/cubit-servo-browser-v26-run.log. No duplicatewaiter.

2026-10-02 v25 TERMINAL1 lockreleased. 16tabs/Settings and4windows/capacity passed; native input_resync interrupted typing during cold window load (screenshot partial URL, submission correctly suppressed). Fixed error-status priority, fixture waits each tabparking and loaded-window-ready. Preparing v26 same360s feature gate, no shared source edits.

2026-10-02 v25 session88392 LIVE owns shared lock, one-cycle features360s. Sources frozen. /tmp/cubit-servo-browser-v25-run.log; tests include actual titlebar close with Settings open.

2026-10-02 v25 nonblocking lock BUSY exit75; no browser native job/waiter. Request next free build window after compositor metrics gate for one-cycle diagnosis/feature gate (~360s).

2026-10-02 v24 TERMINAL1, lock free: first navigation missing URL callback; no native fault. Added fixture-only submitted-URL diagnostic in owned Servo main. Preparing v25 one-cycle/feature gate; nonblocking shared lock only.

2026-10-02 v24 session43950 LIVE shared lock; confirmed repaired Desktop
eb7e986394a3f20fe5354bb4c6e0af12ae25a539ceb058f6b7c6a3bbb8a3d33b
and browser874ee306ce5b7db8defc54b0e58f8888850542c7cdba484d47a2409be2c0b53a.
Native startup/font/allocator/frame checks passed; full gate running.
Sources frozen; no additional production edits planned.

2026-10-02 v24 starts ONLY if sharedlock available after barrier repair; full
480s native gate with Settings-open event acknowledgment (FFI28, preserves
Configure/release behavior) before background-input checks. Own source now
ready/frozen. /tmp/cubit-servo-browser-v24-run.log. No additional shared edits.

2026-10-02 v23 native22752 TERMINAL1; shared lock RELEASED for root barrier95864.
Sustained3cycles>180s passed; added modal background-click gate failed under
16-tab workload (investigating click/input timing vs modal routing).
No current native job/waiter. Shared UI/protocol edits remain owner scope.

2026-10-02 ACK via shared note: compositor close coalescing-barrier repair has
NEXT lock window after Servo v23 session22752. v23 native gate remains LIVE
and sources frozen. Parent will recheck browser lifecycle against repaired
Desktop after owner build terminal; no duplicate native waiter queued.

2026-10-02 v23 full gate NOW starts: normal browser build + native480s
(>=180s cycles, Settings mouse/keyboard+shielding,16tabs,4windows, real
TITLE-BAR X with modal open, native sibling close and reuse, Config reopen).
Uses freshly built Desktop08936f35... and opt-in Graceful_Close/event10.
Shared UI/protocol/browser sources FROZEN; /tmp/cubit-servo-browser-v23-run.log.
Menus additive child finished hosted tests, no live jobs.

2026-10-02 exact Close_Request ABI adopted in OWN Servo source: DP.Graceful_Close
feature via Feature_Bits, UI.Input.INPUT_CLOSE_REQUEST routed to Rust Close
BEFORE modal interception. Await compositor build terminal; no own native job.
Next fixture uses title-bar X for fourth window WITH SETTINGS OPEN and for
remaining siblings; adds per-window-close callbacks. Previous remaining-window
cleanup clicked resize-border slop, leaving window2 alive (captured screenshot).

2026-10-02 v22 native41518 TERMINAL1; lock RELEASED for compositor42149.
Settings modal,16tabs/overflow,4windows, original DOM responsiveness and slot
reuse passed. Final process-close marker missing: investigating fixture
remaining-window focus coordinates. No native job/waiter now; no shared edits.
Await compositor exact close-request ABI before next native build.

2026-10-02 Settings build46602 TERMINAL0. v22 native41518 LIVE under shared
lock (360s, one cycle + Settings +16tabs+4windows/reuse). Browser/native UI
source FROZEN during gate. /tmp/cubit-servo-browser-v22-run.log.
Received compositor opt-in Close_Request ownership plan; parent leaves
Desktop/protocol/UI input constants to that owner. Will adopt published ABI
in owned Servo session after v22 terminal. Menu child only additive sources.

2026-10-02 v21 TERMINAL1, lock released; four cycles233s+16tabs+4windows
passed. Remaining window oracle incorrectly waited for unchanged title:
Servo WebView.set_page_title suppresses equality. Corrected to fresh wheel+Esc.
REQUEST compositor owner: Main.closeSurface still kills whole PID on title-bar
X. Need opt-in graceful Close_Request event/per-window route for browser, while
preserving other apps. No Main/protocol edits by Servo. User permission to
message owner pending after auto-review rejection. Settings modal own source
draft in progress; parent leaves lock window for compositor80435.

2026-10-01 user requested subagent for native Desktop menubars; live child
native_menubars owns additive CuBit.UI.Menus files/tests, no existing UI edits.
Parent moves browser layout toggle into Settings modal after v21 ends.
Compositor waiter69529 has NEXT lock window for metrics fixture; parent will
not queue a competing native run until that waiter has acquired/completed.

2026-10-01 v20 native95242 TERMINAL1, lock released. 16tabs/overflow PASS,
4windows/capacity PASS. Real engine focus missing after native-window raise:
fixed own Rust input dispatch to focus target WebView before page input.
No shared UI changes. Next v21 full180s+features underlock, sources frozen.

2026-10-01 v19 build79807 TERMINAL0; native34642 LIVE shared lock. 16tabs
and both overflow orientations passed; fixture cleanup assumption corrected
for retry (close selects first live slot). Hosted router, frame guard, native
icons and scope audit PASS; 39SPARK results0unproved. UI/browser frozen.
Cross-chat notification rejected by auto-review, no retry/workaround.

2026-10-01 v19: scoped UI.App.Close Destroy_Surface and Widgets.Navigation_Button
ACK received from compositor owner, applied under lock (waiter61116 TERMINAL0).
32-tab/4-session Rust+Ada source draft complete; native build next, shared UI
and Servo sources FROZEN during build. No Main/protocol changes.

2026-10-01 next browser slice:32tabs with shrinking/overflow controls, native
Back/Forward icons+captions,4independent windows. Own Servo sources/tests.
REQUEST UI-owner ACK App.Close scoped Destroy_Surface (existing protocol),
Widgets icon-button addition. Found current Goodbye destroys ALL PID surfaces;
will preserve denied/uncertain-close retention and per-window protected frames.
Shared edits await acknowledgment; own source work can proceed. No native job.

2026-10-01 visual polish v18 session91184 TERMINAL0, LOCK RELEASED.
Shared UI sources stable; freeze ENDED. Native180.839s/3cycles/132callbacks,
Config browser-reopen layout, 3x31930exact resize pixels and360s finalgate PASS.
Shared Settings144cases,256density/font/clipping, widget200cycles and address
1000cycles PASS; geometry exhaustive and SPARK32results0unproved/justified.
Native screenshots /tmp/cubit-servo-polished-tabs-{horizontal,vertical}.png;
light/dark actual-widget preview /tmp/cubit-tab-style-preview.png inspected.
Browser9fcf5b6e0a01194a161dfae9c4a67350b402931733f2a15e5e45088c879ff790.
Report /tmp/cubit-servo-browser-v18-stability.json. No live job or waiter;
no commits/pushes. Soft borders/corners, quiet closebuttons, adaptive220px tabs.

2026-10-01 v18 session91184 LIVE under sharedlock:2complete native cycles
pass, actual polished horizontal/vertical screenshots inspected. Captions now
wider, closebuttons quiet, navcontrols soft borders. Exact resize pixel checks
PASS. Shared38929 TERMINAL0:144Settings renderer cases,256density/font/clipping
suite, actualaddress1000cycles. Await remaining >=180s/browser-reopen/finalscan;
shared UI/browser sources frozen. No new shared edits during gate.

2026-10-01 visual polish sources ready: shared native button renderer uses
soft corners/subtle single border; selected tabs retain orientation accent;
quiet optional native Button used for close. Horizontal tabs adapt to live
count up to220px. Actual light/dark preview inspected:
/tmp/cubit-tab-style-preview.png. Hosted6393 PASS200widget cycles, exhaustive
width/count/rank geometry, SPARK32results0unproved/justified. Shared renderer/
font regressions running; next v18 normal build+360s native underlock.
Shared UI/browser sources frozen; no other shared edits planned.

2026-10-01 user requests tab/button visual polish. UI-owner ACK in
compositor.md for cubit-ui.adb and widgets.ads/.adb. Scoped soft corners,
subtle one-pixel button borders, clear selected tab accent, quiet closebuttons.
Own geometry will size horizontal tabs by live count (up to220px) with existing
bounded8slots; page/viewports unchanged. Shared source-edit lock next; hosted
light/dark preview, geometry proof and native regression follow. No animations.
Previous v17 native completed232.777s/4cycles/172callbacks, finalgate PASS,
lock released (old top live note below is stale; peer got completion message).

2026-10-01 v17 session91647 live under lock: first complete native cycle
59.77s PASS, native nav/tab widgets, inactive-child close isolation, retained
input both orientations and exact31930wallpaper pixels after resize. Horizontal
and vertical PNGs /tmp/cubit-servo-native-tabs-{horizontal,vertical}.png visually
inspected; no prior outside-window resize ghost. Await >=180s and Config reopen
plus finalfaultscan. Sources unchanged/frozen; not yet final native PASS.

2026-10-01 v16 84747 TERMINAL1, lock RELEASED. Browser startup/render PASS;
new baseline capture fixture read file before HMP command finished. QEMU sends
banner, initial prompt, character echoes, completion prompt separately; fixed
owned command() to drain both prompts. Actual-fragmented-socket test78914 PASS.
Stopped only failed v16 VM/observer; no browser fault. v17 retry now queues
normal build+360s gate; widgets/browser native sources UNCHANGED/frozen.
Screenshots now captured synchronously inside fixture (no observer races).

2026-10-01 v16 session84747 ACQUIRED lock; approved Widgets patch APPLIED,
normal production Servo build/link/stage PASS, QEMU full360s gate now live.
Shared widget sources exactly match hosted/native private candidate; frozen.
Hosted34950: actual native-widget200cycles/address1000/checker36controls and
actual built ELF narrow authority audit PASS. Observer96886 waits vertical
screenshot. /tmp/cubit-servo-browser-v16-run.log; no duplicate jobs.

2026-10-01 UI owner ACK received in compositor.md for both widget files,
including Button clip fix. Private candidate56076 TERMINAL101 beforeQEMU:
Rust build.rs derives runtime relative to native-dir, yielding /runtime/adalib.
No staging/test happened. Switching to normal production build after applying
approved patch under shared lock; v16 full360s gate queues now. Own sources
frozen, shared widget sources will remain frozen through gate.

2026-10-01 user authorized cross-chat coordination; request sent to compositor
UI owner, acknowledgment pending. To avoid blocking validation, v15 now queues
PRIVATE Ada widget candidate build + Servo link/stage +360s native under shared
lock, existing shared Widgets sources UNMODIFIED. Candidate source exactly
/tmp/cubit-servo-tab-container.patch applied to copies. Native bridge already
compiled. /tmp/cubit-servo-candidate-build-run.sh; after ACK apply patch and
verify final production relink against tested ELF. Own sources frozen.

2026-10-01 native candidate67642 TERMINAL0 (Alire native compiler/private
objects), shared widget source still UNMODIFIED awaiting ACK. Hosted40357
terminal0; evidence checker86020 terminal0 incl36negative controls andpixel
oracle mutations. Own Servo migration/test changes frozen; v15 build+360s
script ready /tmp/cubit-servo-native-widgets-build-run.sh, not launched.
Asked user authorization to message UI chat (cross-chat messaging not implied).
Pending exact patch /tmp/cubit-servo-tab-container.patch. Own source currently
requires that additive overload, so apply patch before any Servo native build.

2026-10-01 candidate v2 hosted40357 PASS incl fully clipped child button.
Same two Widgets files: Tab overload plus Button registration now clamps its
hit bounds to Canvas clip (2calls), necessary for empty tab contents: existing
empty damage means unbounded, previously invisible child could remain clickable.
Native candidate6659 first used wrong hosted GNAT ABI; retry67642 uses kernel
alr exec compiler with private objects. Waiting UI-owner ACK; existing source
untouched. Patch /tmp/cubit-servo-tab-container.patch includes complete fix.

2026-10-01 candidate /tmp/cubit-servo-tab-container.patch ready: additive native
Widgets.Tab overload with clipped content Canvas +matching content Theme,
retained parent ID, native Horizontal/Vertical Draw_Tab. No existing signature
changes. Hosted22095 PASS200actual-widget child/parent/cancel/repaint/clipping
cycles and1000actual-address cycles. NEED UI owner ACK to apply two widget files
in source-idle window. Servo chrome migration prepared in owned source, generic
Controls/App routing instead of hand hit tests. No shared edits yet. Can native
compile privately against candidate while awaiting ACK; v15 gate not launched.

2026-10-01 USER requests actual native tabs and reusable tab content container
(icon/caption/close children). REQUEST compositor/UI owner acknowledgment for
scoped extension userspace/lib/ui/cubit-ui-widgets.ads/.adb: add Tab overload
returning clipped content Canvas, orientation, retained input; existing label
Tab delegates to common native frame. Child controls registered after parent
win hits; close remains separate ID/action. No Controls/App changes planned.
Will prepare patch privately and migrate owned Servo source meanwhile; do not
apply shared widget patch until acknowledgment/source-idle window. v15 native
rerun deferred until this widget migration complete; no native job currently.

2026-10-01 compositor resize42907 fix staged8231f815... and native-verified
with Workbench; starting separate Servo v15 validation, unchanged browser.
Fixture adds exact baseline wallpaper comparison outside restored browser:
31930pixels across right/bottom exposed regions each completed cycle.
No shared source changes. Will run full360s browser/tabs/Config gate and
retain visual evidence; not substituting peer Workbench test for Servo.

2026-10-01 v14 session94712 TERMINAL0, shared lock RELEASED. Native tabbed
browser PASS final fault scan:230.103s browser alive,4complete cycles,170
callbacks. Real nav buttons, new/close buttons+shortcuts, retained DOM input,
both orientations, vertical pointer mapping, resize/restore, blank/history
pool reuse, idle+fresh callback, close/relaunch vertical Config preference.
Evidence -v14-stability.json/run.log/serial timeline and exact3binary hashes.
Actual ELF authority audit96027 PASS only2FS write scopes Bookmarks/Downloads,
read-only fonts/servo/tls and Config browser.servo. Screenshots retain resize
ghost issue for compositor owner (see note below). No hardware/GPU/total-memory
claim. Browser restart Config persistence passed; OS reboot persistence pending.

2026-10-01 v14 live:3complete cycles/173s, newtabs/switch/retained input,
vertical pointer mapping, close+blank/history pool reuse, resize all pass sofar.
VISUAL ISSUE for compositor owner: after enlarge854x610 then restore800x600,
stale old outer window remains x906..960/y716..726. Evidence screenshots
/tmp/cubit-servo-tabs-{vertical,horizontal}.png, serial v14; current staged
Desktop8a49a7dc...; browserce169003.... No peer edits. Functional native gate
still running; screenshot is not visual-cleanliness PASS.

2026-10-01 v14 session94712 ACQUIRED shared lock; log now381KiB, native
build executing. Previous same-minute empty-log note raced acquisition.
Holding lock through build/staging and360s native regression, own sources
frozen. Graphics kindly yielded next window; no duplicate job needed.

2026-10-01 v14 session94712 still waiting, run log empty. Please yield
next build+360s browser window after already queued compositor38145 and
graphics41170; repeated queued peer jobs otherwise starve nonblocking retry.
Own source/tests frozen and user-visible features await native verification.
Manifest84286 compiled privately; exact narrow scope object retained in/tmp.

2026-10-01 v14 session94712 LIVE queued via bounded nonblocking retries
(up to600s, no lock while waiting), then corrected build+stage+360s native
browser test under one lock. Do NOT launch duplicates. Own sources/fixtures
frozen. /tmp/cubit-servo-browser-v14-run.log; exact staged hashes saved after
build. User-required tabs, Config preference and restricted FS grants included.

2026-10-01 build21456 TERMINAL101: Ada bridge compiled; Rust new-tab
constructor required Rc<SwglContext> instead of &SwglContext. Corrected
run_window argument; retry lock refused, no own native job/staging. Request
next short Servo relink then sustained test window. Logs native[-v2].log.

2026-10-01 native tabs build21456 LIVE under shared nonblocking lock:
build-cubitshell.sh +stage. Own sources frozen; no shared sources edited.
Log /tmp/cubit-servo-tabs-native.log. Hosted39378 PASS30tab/geometry and
59framecopy SPARKresults,0unproved/justified; actual editor1000cycles.
Checker28264 PASS34negative controls; frameguard1000cyclesPASS. After native
compile, sustained v14 will test tabs/layout/navigation/Config browser-reopen.
Config scalar API is in-memory across OS lifetime; durable typed preferences
remain separate required follow-up, no filesystem profile fallback.

2026-10-01 requested tabs implementation source ready for native compile.
Need next shared build/test window after graphics77272 and peers. No own live
native job. Hosted tab policy56223 PASS26SPARKresults,0unproved; expanded
geometry/frame-copy/editor39378 active in isolated outputs. Manifest now
requests only Bookmarks/Downloads writes and browser.servo Config namespace.
Baseline resize actual854x546 follows finalcursor minus outer origin/insets;
fixture will use exact native geometry, not requested drag delta.

2026-10-01 baseline33804 TERMINAL1 v13: wheel+scroll pass; resize to
860x548 callback absent at57.2s. No sustained pass. Evidence retained; native
lock released. User-requested tabs source work active, new build not staged.
Investigating resize delivery alongside tab implementation.

2026-10-01 v13 baseline session33804 LIVE under shared lock; fixture and
staged binaries frozen. User now requests tabs +horizontal/vertical switch,
Config-service preferences and filesystem writes ONLY Bookmarks/Downloads.
Own Servo source-only work proceeds; no shared sources/staging during run.
Fixed resident view pool design avoids treating WebViewClosed as retirement;
parking requires blank document +cleared-history acknowledgment before reuse.
No total pipeline/memory bound claim.

2026-10-01 v13 retry still NOT launched: nonblocking lock refused after
compositor36423 terminal, graphics98532 queued rebuild appears next holder.
Please preserve next Servo >=360s test window after that build. Corrected
fixture ready; no own jobs or source edits, sustained requirement outstanding.

2026-10-01 v13 lock attempt refused; no live Servo job. Yielding to
graphics69867 then requested compositor90s desktop-protocol gate, before
v13 sustained retry. Corrected resize checker18856 PASS23 negative controls.

2026-10-01 v12 session54230 TERMINAL1: navigation/DOM/history/reload and
corrected wheel+scroll PASS, then resize assertion failed at57.1s without
fault/resync. Screenshot confirms window unchanged: UI.App.Open declares
800x600 minimum, so fixture shrink was invalid. Fixture now enlarges60x12
within output work area, requires860x548 DOM viewport, restores800x536.
Browser binary unchanged. Preparing v13 sustained retry; v12 evidence kept.

2026-10-01 v12 session54230 LIVE shared-lock sustained retry after metric
visibility repair. Native preparation +360s four-vCPU TCG with unchanged
staged Servo; browser must stay open >=180s with complete repeated actions.
Logs /tmp/cubit-servo-browser-v12-run.log and -v12.serial.log; hashes retained.
No own source edits; shared kernel/runtime/UI/runner freeze during preparation.

2026-10-01 v11 session52325 TERMINAL1 before QEMU: stage-1 runtime
compile fails cubit-metric_batches.ads:56 Slot_Words and:69 Page_Words
equality visibility. No browser launch/stability verdict. Lock released;
request runtime/metric owner repair and stable-source window for v12 retry.
Evidence /tmp/cubit-servo-browser-v11-run.log and input/kernel hashes retained.
No changes to peer sources; graphics sees same blocker.

2026-10-01 v11 launch52325 active under shared lock, Nix cache access
resolved with approved execution permissions. Graphics yielded slot. Prior
attempts included Nix read-only cache failure, not only lock refusal. Exact
staged hashes captured by command; no source rebuild requested beyond runner
preparation. Shared sources should remain stable during preparation/run.

2026-10-01 dedicated chat requests NEXT shared native slot for sustained v11.
Nonblocking lock attempts refused; v11 has NOT launched and its run log is
empty. Please yield a >=360s QEMU window after current graphics work, with
kernel/runtime sources stable through preparation. Hosted checker44408 PASS
23 negative controls. Corrected historical docs: v9 was short browser lifetime.
Aware graphics36168 hit metric batch compile errors; no peer-source edits.

2026-10-01 ownership transferred to dedicated Servo chat
01a0f968-6b1d-7401-a136-50604ecc4935. Previous subagent quiescent.
Preserving all uncommitted work; scope unchanged. Preparing corrected v11
sustained browser test with existing staged binary, shared nonblocking lock.
Request shared kernel/UI/runtime/runner sources frozen during preparation
and native run; no own native source edits or rebuild requested. Graphics
native42 queue acknowledged; lock refusal will defer our test. Evidence
/tmp/cubit-servo-browser-v11-{run.log,inputs.sha256,kernel.sha256} and
-v11.serial-browser.timeline.jsonl. No commits/pushes.

2026-10-01 hosted61011 TERMINAL0: sustained evidence checker PASS complete
timeline+23missing-phase/short-duration/early-close/failure controls. It uses
browser-open lifetime, required repeated actions and post-idle callbacks;
cannot substitute total VM duration. Corrected sustained v11 ready, awaiting
root25223 then graphics92762 queued link. No own native job/lock.

2026-10-01 v10 wheel fixture cause confirmed in QEMU primary source
ui/ui-hmp-cmds.c: positive HMP dz emits WHEEL_UP, negative DOWN, magnitude
ignored. Earlier+3 at page top correctly did not scroll. Corrected-1 and
requires actual DOM deltaY sign+1 before scroll oracle. No browser binary
change; waiting root25223 terminal for corrected sustained run. Not a crash.

2026-10-01 sustained17835 TERMINAL1, lock yielded to root queue validation.
Browser responsive54.4s through navigation/input/history/reload, then wheel
scroll callback missing; NO crash/fault/resync. Desktop wheel=1 confirms
delivery; screenshot page at top. Investigating HMP direction with actual DOM
wheel delta sign diagnostic, not suppressing failure/slowing test. Timeline
v10 retained; sustained stability NOT passed. Kernel staged hash also captured.

2026-10-01 native17835 LIVE sustained v10 build+360s4CPU TCG under lock.
Graphics35518 terminal0. Explicit user stability requirement: keep browser
OPEN >=180s after render gate, repeat navigation/pointer-focus/typing/history/
reload/wheel/resize+restore/idle+fresh response. Actual interval and callback
timeline recorded-v10.serial-browser.timeline.jsonl; logs-v10-run.log,
-v10.serial.log, exact Desktop/app hashes. Existing v9 was a SHORT sequence,
not180s browser stability. Wheel now matches Servo shell76devicepixels/notch;
source audit found current renderer ignores DeltaLine units. Sources frozen.

2026-10-01 native13743 TERMINAL0:180s4CPU TCG full servo/finalfaultscan PASS.
First functional browser milestone: data/HTTP/HTTPS text; protected frames;
native address navigation; actual pointer-focus and DOM typingabc; Back/
Forward traversal plus restored DOM callbacks; reload; close. No resync/fault.
Screenshot /tmp/cubit-servo-browser-v9-inspect.png. Exact Desktop9ba7cb05...
and app929ee9d4... hashes in-v9-inputs.sha256; serial/run logs v9. Lock released.
Software SWGL readback remains;300ms functional keys are NOT overload/latency
evidence. Continuing scroll/resize/DPI and bounded tabs; no full-browser claim.

2026-10-01 native13743 LIVE v9 fixture-only180s4CPU under shared lock.
No compile or shared source edits; Desktop/browser exact hashes recorded
/tmp/cubit-servo-browser-v9-inputs.sha256; logs -v9-run.log/-v9.serial.log.
Adds actual pointer focus DOM oracle before keyboard typing. Parent informed.

2026-10-01 native26333 TERMINAL1; lock released. Native address navigation
to browser-a succeeds, readable complete800x600 window, no resync/fault.
DOM typing failed because input lacked focus (autofocus assumption). Fixture
now clicks fixed-position input with real PS/2 move/button and requires DOM
click title before typing. Screenshot /tmp/cubit-servo-browser-v8-inspect.png.
Next v9 fixture retry requested; current binary unchanged for that retry.

2026-10-01 native26333 LIVE shared lock v8 rebuild+180s4CPU browser.
Root37517 terminal0; Desktop default cursor-fix staging unchanged. Logs
/tmp/cubit-servo-browser-v8-run.log and -v8.serial.log; hashes recorded.
False-dirty suppression/resync address rejection, visible guidance,800x600
initial window fits1024x768, functional300ms pacing. Own sources frozen.

2026-10-01 hosted64610 TERMINAL0: actual extracted shell editor/resync
branches + real Editor PASS1000cycles; KEY_UP/printable presses skip redraw,
configure preserves edits, resync blocks Enter through subsequent typing,
Ctrl+L restores authoritative URL and allows restart. Native v8 pending root
37517 timing slot. Failure fixture now captures screenshot and ends only its
own QEMU on callback timeout, preventing an unnecessary remaining180s wait.

2026-10-01 native23930 TERMINAL1; shared lock yielded to root timing run.
All3 page render gate PASS: ink22667/31999/5847 including HTTPS text; no
faults. Browser typed navigation failed with input_resync2, screenshot shows
https://0browser-a/ rather than controlled full address. False dirty flags on
address KEY_UP/unrelated KEY_DOWN now removed; functional TCG fixture pacing
300ms/key explicitly not overload evidence. Native confirmation pending.
Screenshot /tmp/cubit-servo-browser-v7-inspect.png; logs v7 as below.

2026-10-01 native23930 LIVE shared lock v7 build+180s4CPU TCG browser.
Root1628 cursor regression terminal0; tested Desktop hash recorded in
/tmp/cubit-servo-browser-v7-desktop.sha256. Log -v7-run.log, serial
-v7.serial.log. Mandatory secondary-stack ELF check promoted in dedicated
build script under lock. FreeType private map and corrected history oracle;
own source frozen until terminal, no shared UI/runtime/runner changes.

2026-10-01 hosted47786 TERMINAL0: patch regression and actual adapter
other-thread/reentry rejection PASS; fixture Python syntax PASS. History
oracle now uses Servo traversal-complete plus restored document Escape/title
handler, since retained history need not load again. Waiting root1628 terminal;
prepared /tmp/cubit-servo-v7-build-test.sh for locked build/ELF check/180s test.

2026-10-01 hosted29296 TERMINAL0: private actual patcher regression PASS
old cache/upstream FreeType+FontData+graphics and byte/mtime idempotence.
CuBit FreeType map_copy_read_only uses PROT_READ|MAP_PRIVATE, preserving
existing Arc<Mmap> face/table lifetime. Waiting root cursor native window;
next locked build also makes secondary-stack ELF verification mandatory.

2026-10-01 native73086 TERMINAL1, lock released to root. Protected publication
and colored page rendering now work with the standalone secondary stack;
fonts6/allocator32/cancel2 PASS, data ink20000/HTTP ink24000, text-only HTTPS
ink0. No faults. Second FreeType MAP_SHARED path identified; CuBit-only
map_copy_read_only patch preserves Arc<Mmap> face/table ownership. Private
patch regression next; no own native jobs or shared-source edits.

2026-10-01 native73086 LIVE locked v6 retry. Actual CuBit runtime C-hosted
harness PASS pre-binder init +1000mark/allocate/release cycles; actual final
Servo ELF all4SS calls wrapped,16aligned32784byte static scratch verified.
Native relink PASS7.78s, fixture boot starting. /tmp/cubit-servo-browser-v6-
run.log and -v6.serial.log; Desktop/app hashes captured. Root Desktop source
may advance during test, staging/UI/runtime/runner remain frozen.

2026-10-01 root20534 terminal; native33106 LIVE locked v5 retry build+180s
4CPU TCG. Log /tmp/cubit-servo-browser-v5-run.log, -v5.serial.log; Desktop/app
hash files -v5-{desktop,app}.sha256. Supported2048max/1024atlas+image tiling,
FontData::from_vec, verified fixture; own sources frozen. Release to root DPI
retry after terminal. No browser milestone PASS yet.

2026-10-01 v4 session97166 TERMINAL1, lock yielded to root85815 DPI test.
Verified image fonts6/allocator32/cancel2PASS; WebRender rejects internal
texture limit1024 (minimum2048). Corrected2048 with1024RGBA/glyph atlases and
1024image tiling; hosted19713 constraintsPASS. Font cause identified:
memmap2 MAP_SHARED conflicts with libc private-only; CuBit FontData now
owns fs::read Vec directly, avoiding map+copy. Hosted36244 actualpatcher
old-cache/upstream/idempotencePASS after fixing overlapping cache anchor.
No own live jobs; await root85815terminal before v5 retry.

2026-10-01 graphics96017 terminalPASS; native97166 LIVE shared lock:
authorized Servo-only runner verified install hook applied, bash syntaxPASS;
build+180s4CPU TCG v4 retry. Logs /tmp/cubit-servo-browser-v4-run.log and
-v4.serial.log; Desktop/app SHA files recorded. SWGL1024settings +native
font/page fixtures. Own sources frozen; root runner edits wait for terminal.

2026-10-01 hosted3381 TERMINAL0 verified fixture installer regression PASS:
actual private ext2 replacement/empty/256KiB bytes, bad destination rejected.
Awaiting graphics requested native slot after root73892; no native build or
shared runner edit. Root authorized narrow runner install verification next.

2026-10-01 native11346 TERMINAL1, lock yielded to root73892 then graphics.
GC allocator/cancel fixtures pass and page0 loaded; SWGL texture realloc
assert(new_buf) then aborts. Default2048RGBA atlas equals16MiB before malloc
overhead; libc mappings cap16MiB. Prepared SWGL-only1024texture/atlas options
using WebRender settings (future hardware unchanged), no native build yet.
Added font path/read-dir/header and stale-pages fixture guards. Need later
shared-runner lock to unlink fonts/pages/hosts before debugfs writes, because
existing files otherwise survive silently. No functional browser PASS yet.

2026-10-01 v2 session61612 TERMINAL1; v3 session11346 LIVE locked retry
build/stage180s4CPU TCG browser. Log /tmp/cubit-servo-browser-v3-run.log;
serial -v3.serial.log; tested Desktop hash -v3-desktop.sha256 (root Desktop
source now ahead with unstaged DPI wake change, shared UI/runtime stable).
App hash recorded after build. Yield lock after this retry to root/graphics.

2026-10-01 v2 native61612 browser startup fault after allocator32/cancel2 PASS.
RIP0x3B43D95 musl get_meta/free from GC InitMemorySubsystem mmap probe. Broad
UnmapInternal free was incorrect; narrowed to paired UnmapPages only, generic
UnmapInternal restored. Hosted34166 TERMINAL0 fresh/old-cache/idempotence PASS;
288aligned cases+9ENOMEM+32mmap probe cases ASan/UBSan PASS. Failed native
fixture finishing timeout; need corrected v3 rebuild, no browser PASS yet.

2026-10-01 native61612 LIVE locked dedicated Servo rebuild, staging and180s
4CPU TCG browser v2 regression. Own sources frozen. Root ABI/UI.App stable.
Log /tmp/cubit-servo-browser-v2-run.log; serial -v2.serial.log. Includes
GC aligned allocator fix, signed pointer geometry, prepare/cancel/readback
optimization and coordinated publication wire. No integration PASS yet.

2026-10-01 prepare-before-readback source ready, native validation pending.
Rust scoped frame guard calls Ada Prepare/Present/Cancel; deferral skips SWGL
readback, viewport mismatch cancels with repair debt retained. Native fixture
adds two acquire/cancel/reacquire cycles and nested-acquisition rejection.
Hosted46031 TERMINAL0: actual Rust adapter with FFI fault mocks passes
1000cancel/reacquire/present cycles, deferred/nested rejection and close order.
Waiting for root21793 corrected timingCCL lock window; no shared sources edited.
Async Servo page input watermark remains unknown0; mixed-output scale-change
wake remains an integration gap, not papered over with configuration polling.

2026-10-01 native19987 TERMINAL1: first-page SpiderMonkey null crash is
MOZ_RELEASE_ASSERT in MapAlignedPagesSlow after partial munmap returnsEINVAL;
current libc correctly permits whole-owned-region release only. No browser PASS.
Exact evidence /tmp/cubit-servo-browser.serial.log, -run.log, -fault.txt.
No own live native jobs/lock. Root is migrating publication wire and will signal
coherent Desktop/client staging; next Servo build must consume that new ABI.
Own GC port patch now uses existing posix_memalign/free strategy, accounting
retained, actual mprotect unchanged; no libc/kernel edits. Fresh/cache/idempotent
host patch6412PASS. Aligned allocation/protect/free/reuse native fixture added.
Signed pointer helper77346PASS233728cases,8SPARKchecks0unproved/justified,
now used in bridge; no floor-as-cell-origin claim. Copy/admission58checksPASS.

2026-10-01 native50578 TERMINAL0:86MiB cubitshell.app linked/staged; font
rasterizer reused as Cargo rlib within Servo runtime (no duplicate allocator).
Native19987 LIVE shared-lock180s4CPU TCG SERVO_BROWSER_CHECK+SERVO_DESKTOP
regression. /tmp/cubit-servo-browser-run.log and .serial.log. Sources frozen.
Scalar admission added: hosted28943 TERMINAL0, SPARK58checks0unproved/justified,
13950pixel cases+malformed-layout cases PASS; /tmp/cubit-servo-frame-admission.log.
Shared runner edits authorized by root, applied under lock: fixture flag and
dedicated browser_input.py hook; defaults preserved. No image packaging.

2026-10-01 native47536 LIVE: shared-lock make -C kernel servo; libc/fonts +
standalone Ada bridge built, Servo Rust recompilation in progress. Sources
frozen during build. /tmp/cubit-servo-browser-native.log. No image packaging.
Hosted29815 TERMINAL0: Servo_Frame_Copy SPARK51 total checks, zero unproved/
justified (48 prover,3 flow); pixel oracle13950cases PASS. Evidence in
/tmp/cubit-servo-frame-proof-final.log and tests/servo/build/frame-copy/obj/
gnatprove/gnatprove.out. Whole FFI shell remains SPARK-Off audited boundary.
Reuse Client_Input_Budget from root,32poll/1ms, no duplicated policy.

2026-10-01: authorized child of compositor agent; read AGENTS and coordination
README plus compositor/filesystem/networking notes. Own userspace/servo/native,
userspace/servo/overlay/ports/cubitshell, Servo-specific tests, docs/servo-port.md
and dedicated Servo build integration. No shared compositor/UI, NetSurf, driver
or other owner-note edits. No commits/pushes or cross-thread messages.

Current milestone: functional native browser chrome (address editing, navigation,
history/reload) around Servo, replacing cubitshell's permanently writable legacy
attachment with a narrow Ada bridge to UI.App/Client_Frame_Pair. Reuse native
density-aware chrome and proven canvas geometry. Preserve software SWGL fallback;
readback remains explicitly temporary, no GPU or zero-copy claim. Convert its
bottom-up RGBA directly into the acquired protected frame (remove intermediate
conversion image). Repair full current page+chrome and return safely on deferral.
Servo physical viewport/HiDPI input contracts must be checked from engine source.

Then continue browser tabs, complete input/wakeup/close behavior, fault/overload,
configured DPI tests and hardware rendering/import. Full browser goal is not met
by the initial chrome milestone. Native builds/staging use shared build.lock;
root currently owns native NetSurf regression. No own native job or lock yet.
