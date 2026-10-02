# Servo browser integration tests

Run builds and tests inside the repository's Nix shell. Native builds, staging
and QEMU fixtures require `coordination/build.lock` for their entire duration.

The native browser fixture is opt-in:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c '
  make -C kernel servo &&
  SERVO_DESKTOP=1 SERVO_BROWSER_CHECK=1 bash tests/headless/run.sh \
    --test servo --cpus 4 --accel tcg,thread=multi --timeout 360 --keep-logs'
```

`SERVO_BROWSER_CHECK=1` installs a fixture flag in the guest's copied disk.
The shell then checks aligned allocation, subrange protection and allocation
reuse using the real CuBit libc. It emits test-only callbacks for the controlled
HTTP fixture. `browser_input.py` injects QEMU keyboard events through the normal
input stack, edits the native address field, navigates to two pages, types into
an actual DOM input, traverses history, reloads, scrolls and resizes. The full
cycle repeats until the browser has remained open for at least 180 seconds
after its initial render gate. Each cycle includes an idle interval followed
by a fresh DOM response. Only then does it capture and close the browser.
The DOM input changes the page title; its engine callback is required.
This establishes substantially more than accepting toolbar commands, but does
not establish pixel-perfect rendering, mixed-output DPI, GPU execution or
input-to-photon latency. Screenshot inspection is a separate check.

The screenshot's `.timeline.jsonl` companion records host-monotonic callback,
cycle, idle and close timestamps, plus the measured browser-open interval.
Harness duration alone is not browser stability evidence. The earlier v9 pass
closed the browser early and only validates its short functional sequence.
The extended tabbed v14 gate passes230.103seconds of browser-open time and
four complete cycles, plus browser-restart Config preference restoration. For fixture
development only, `SERVO_BROWSER_STABILITY_SECONDS=0` runs one complete cycle;
that shorter mode must never be reported as the sustained stability gate.
The corrected v11 attempt (2026-10-01) stopped before QEMU because the shared
runtime's `cubit-metric_batches.ads` failed equality-operator visibility checks
at lines 56 and 69. Its run log and staged input/kernel hashes are retained
under `/tmp/cubit-servo-browser-v11-*`; it supplies no browser-alive evidence.
The runtime owner repaired the build; v12 reached the browser and passed the
corrected wheel/scroll checks, then failed an invalid shrink expectation:
`UI.App.Open` registers the initial 800x600 client size as its minimum. The
fixture now enlarges to 860x612 and restores 800x600, requiring DOM viewports
860x548 and 800x536 respectively. The v14 tabbed test subsequently passes using the corrected final-pointer
sizing described below.
`check_stability.py <timeline> <run-log>` verifies the browser-open duration,
complete repeated interaction cycles, renewed responses after idle, close
ordering and the native runner's final PASS. Its hosted negative controls run
with `nix develop -c python3 tests/servo/test_stability_checker.py`.

All `servo` test sessions install `/servo/batch-test` to retain the existing
data/HTTP/HTTPS page and nonempty-frame checks before interactive testing.
Ordinary desktop launches enter the interactive loop during the first page
load instead of waiting for that batch oracle.

Hosted pure-policy and pixel tests use disjoint outputs and may run separately:

```sh
nix develop -c bash -c 'cd kernel &&
  alr exec -- gprbuild -p -P ../tests/servo/frame_copy.gpr &&
  ../tests/servo/build/frame-copy/frame_copy_tests &&
  alr exec -- gnatprove -P ../tests/servo/frame_copy.gpr \
    -u servo_frame_copy.adb --level=2 --timeout=20 -j2 --report=all &&
  alr exec -- gprbuild -p -P ../tests/servo/input_geometry.gpr &&
  ../tests/servo/build/input-geometry/input_geometry_tests &&
  alr exec -- gnatprove -P ../tests/servo/input_geometry.gpr \
    -u servo_input_geometry.adb --level=2 --timeout=20 -j2 --report=all'
nix develop -c python3 tests/servo/test_port_patches.py
```

`frame_copy_tests` independently checks every converted pixel and untouched
padding across 13,950 layouts, plus malformed scalar admission. Its SPARK
boundary proves bounded indexing/arithmetic, termination and preservation
outside the destination rectangle; exact RGB conversion is covered by the
independent oracle, not a full-image functional postcondition.

`input_geometry_tests` checks 233,728 signed, extreme, fractional and nonempty
logical-cell round trips. The pure helper uses signed ceiling arithmetic,
subtracts the containing canvas's physical origin and saturates only at the
foreign signed32 bounds. The full native input dispatcher is not SPARK-proved.

`test_port_patches.py` privately checks both fresh and previously patched
dependency caches. The GC adaptation uses aligned libc allocations and paired
`free`, preserving accounting and real page-protection calls. It does not
pretend that CuBit supports partial `munmap`. No active dependency tree is
modified by this hosted test.

The browser fixture also checks scoped frame cancellation and reacquisition
before loading Servo. Native `Prepare` acquires storage before SWGL readback;
deferral skips readback, and a Rust guard cancels an abandoned paint through
`UI.App.Cancel_Paint`. No writable frame address crosses into Rust. The
startup cancellation checks have passed in native runs; the complete browser
fixture remains required for the navigation milestone.

`frame_guard.rs` compiles the actual Rust adapter against hosted FFI mocks.
It checks failed/nested acquisition never cancels another lease, abandoned
frames cancel once, successful publication does not cancel again, and close
follows lease completion (1,000 cycles). Run with `nix develop -c bash -c
'rustc --edition=2021 tests/servo/frame_guard.rs -o /tmp/servo-frame-guard &&
/tmp/servo-frame-guard'`. This does not exercise native IPC or protection.

`install_fixture.py` replaces each controlled guest file and dumps it back for
SHA-256 comparison. `test_fixture_install.py` exercises actual debugfs on a
private ext2 image, including its false-success failure mode. Run with
`nix develop -c python3 tests/servo/test_fixture_install.py`.

`test_render_limits.py` compares the production SWGL options against actual
WebRender minimums and CuBit mapping limits, including atlas allocator margin
and image tiling. It does not bound every page-dependent engine allocation.
`test_overlay_patch.py` runs the real patcher against private source copies,
checks old-cache and upstream graphics/font paths, and verifies idempotent
bytes and timestamps. Both run as `nix develop -c python3 tests/servo/<name>`.

`secondary_stack_host.c` exercises the actual standalone Ada/runtime objects
on Linux: getter before binder initialization, then 1,000 aligned bounded
mark/allocate/release cycles. `check_secondary_stack_link.py` inspects the
linked ELF to require all four runtime secondary-stack users to call the local
wrapper and checks the static scratch object's alignment and capacity. This
is audited runtime/FFI code, not SPARK-proved. The Rust adapter rejects other
threads and reentrant FFI entry; its `--test` build exercises both failures.

Native v7 rendered data/HTTP/HTTPS pages including text (ink counts
22,667/31,999/5,847) through protected publication, without faults. FreeType's
second mapped-font loading path retains its existing `Arc<Mmap>` lifetime using
a private read-only mapping. The subsequent 35ms/key TCG navigation input
overflowed (two resyncs), losing address characters. Functional pacing is now
300ms/key; it does not validate overload or hardware latency. Back/Forward
checks wait for actual traversal completion plus a restored DOM key handler,
because retained history need not issue another load-complete callback. Native
v9 passes a short functional sequence within a 180-second four-CPU TCG
session and its final fault scan: navigation,
real pointer click/focus, DOM typing, history, reload, and close. It uses the
800x600 initial window so all controls fit the 1024x768 fixture output.

`nix develop -c python3 tests/servo/test_address_input.py` compiles the actual
shell resync/editor branches with the existing editor in a private hosted
fixture. It checks 1,000 cycles: release/printable-press skip redraw, configure
preserves edits, resync blocks Enter even after further typing, and restarting
editing restores the authoritative URL before accepting a new address. This
tests the audited shell logic; it does not exercise native input delivery.

The tabbed-shell regression additionally uses the actual toolbar buttons,
alternates the new/close tab buttons and shortcuts, verifies retained DOM input
across tab switches, exercises vertical page-pointer mapping and returns to the
horizontal layout. It leaves the vertical preference in Config, closes the
browser, relaunches from Apps and requires the corresponding initial viewport.
This checks browser-restart persistence in the same OS session, not reboot
persistence of the current in-memory scalar Config API.

The resize oracle uses the compositor's final-pointer sizing: dragging from
(900,714) to (960,726) yields a client size of854x610, hence854x506 beneath the
horizontal chrome. Restore begins inside the new exclusive border at(958,724).
The v13 baseline produced854x546 with its older40px toolbar; it failed the
fixture's incorrect860x548 expectation, not an observed crash.

`tabs.gpr` tests the actual SPARK slot policy for capacity, no early reuse,
selection and closing the last tab, plus tab/page geometry. The current proof
has39results with no unproved/justified checks. The offset-aware frame admission
has59results and an independent pixel oracle. Neither proves the Servo engine
or the native FFI adapter.

Native v14 passed the full360-second runner and final fault scan, with230.103s
browser-open time,4complete interaction cycles and170callbacks. Report:
`/tmp/cubit-servo-browser-v14-stability.json`. The tested final ELF is audited by
`check_browser_manifest.py`: exactly Bookmarks/Downloads filesystem write/create,
fonts/servo/tls read-only, and browser.servo Config read/write. This verifies
embedded grants; it does not re-prove the filesystem service's enforcement.
Visual inspection still finds old outer-window pixels after shrinking, reported
to the compositor owner with horizontal/vertical screenshots in `/tmp`.

Native tab containers use `CuBit.UI.Widgets.Tab` with a clipped child canvas,
a caption and a separately registered close button. Generic retained controls
route activation before paint; the tab body and close action never both fire.
The child canvas can also contain artwork without a browser-specific widget.
`native_tabs.gpr` exercises the actual toolkit widgets in both orientations,
including child/parent isolation, repaint while pressed, cancellation, clipping
and an empty child region. This is hosted toolkit evidence, not native IPC.

The current sustained fixture also compares 31,930 wallpaper pixels outside
the restored browser after every enlarge/shrink cycle. Its baseline and final
PPM captures are retained alongside the timeline; missing pixel comparisons
fail the stability checker. Historical v14 predates this additional oracle.

Native v17 validates the tab-container migration: 232.777 seconds browser
interaction, four complete cycles, 172 callbacks, four exact resize pixel
comparisons, Config browser restart and final fault scan PASS. The mouse-close
cycles close an inactive tab and reject a spurious parent-selection callback.
`test_monitor_command.py` exercises the actual fixture command against a local
socket sending fragmented banners/echoes/prompts and delayed screenshot data.

`tab_style_preview.adb` renders the actual native buttons and tab containers in
both Alloy themes to `/tmp/cubit-tab-style-preview.ppm`; it is a hosted visual
preview, not a native browser screenshot. Adaptive horizontal tab geometry is
checked over all supported widths and live count/rank combinations by
`tabs_tests.adb`, in addition to its SPARK contracts. The existing shared
Settings callback-renderer and UI font/density/clipping suites cover the native
renderer changes. Close-button activation and capture semantics are unchanged.

The expanded browser supports 32 resident tabs per window and four resident
window contexts. Set `SERVO_BROWSER_FEATURES=1` alongside the browser fixture
to exercise 16 live tabs, horizontal/vertical overflow controls, four native
windows, capacity denial, independent closure, and window reuse. The normal
stability loop and Config reopen check still run.

`test_window_router.py` compiles the production Ada router with fault-injected
session bodies to verify independent state, bounded admission, pinned frame
leases, denied close and slot reuse. `frame_guard.rs` also rejects failed-open
cleanup (no destructor is called for a nonexistent window). Native widget
tests cover enabled icon/caption routing and disabled-control clipping.

Tab-layout changes now go through the native Settings modal. The native
fixture opens Settings, attempts Ctrl+T behind it (which must be consumed),
opens Edit > Settings by pointer or Alt+E/S, toggles the native checkbox,
captures the modal, then closes it with Escape or keyboard Done.
`check_features.py` requires actual overflow/window completion markers; Servo
suppresses unchanged page titles, so the retained-window oracle generates a
fresh wheel title before restoring it with Escape. Window reuse waits for the
blank-history parking acknowledgment.

The feature fixture also closes a window with its native title-bar × while
Settings is open, then checks the original page and reuses the retired slot.
It waits for each tab's parking acknowledgment and each new window's loaded
callback before typing; this functional gate does not test overload latency.
The timeline records four individual sibling closes and two whole-browser
shutdowns. `check_features.py` requires both kinds, plus Config reopen.

Native v26 passes the 360-second headless/fault-scan gate and expanded feature
validator: 16 tabs, two overflow orientations, four simultaneous windows,
capacity denial, isolated closure and reuse. Its ordinary interaction loop is
one 66.144-second cycle (`SERVO_BROWSER_STABILITY_SECONDS=0`), followed by the
feature phase. The retained layout survives browser relaunch. See
`/tmp/cubit-servo-browser-v26-features.json` and the corresponding serial,
timeline and run logs. The built ELF authority audit and 1000-cycle actual
address-editor regression also pass (`/tmp/cubit-servo-v26-final-host.log`).

The menu integration fixture invokes File > New tab with the pointer and
View > Reload by mnemonic, exercises F10/Right, and checks that dismissing
an open menu over a page input does not click through. It captures both File
and Edit popups. The 24-pixel menu row leaves horizontal/vertical initial page
viewports at 800x472 and 608x512, respectively.

Native v27 passes the expanded menu/tab/window sequence and 360-second final
gate. A focused keyboard-to-modal regression opens Edit > Settings with Alt+E/S,
clicks Done, and requires the very next typed character to reach the page. This
catches stale menu key suppression surviving a mouse-dismissed dialog. Menu
opening also cancels old chrome pointer capture and releases held page buttons.

The final v28 focused native gate passes for 180 seconds including the final
fault scan, `menu-modal-keyboard-handoff-pass`, `menubar-input-pass`, and Config
reopen. This follows v27's full 16-tab/four-window menu integration gate. The
final built authority audit passes in `/tmp/cubit-servo-v28-authority.json`.
