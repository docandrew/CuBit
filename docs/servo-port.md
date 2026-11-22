# Penny: Servo on CuBit

The native browser is named **Penny**. Its Help → About Penny dialog credits
Servo. Internal package names, executable `cubitshell.app`, and Config scope
`browser.servo` remain stable so branding does not discard settings or grants.

Status (2026-10-01): Servo renders readable data, HTTP and HTTPS pages in
an interactive CuBit desktop window with an address bar, history and reload.
The native v9 test validates a short functional sequence; its 180-second VM
runtime is not 180 seconds of browser-alive evidence. The v14 tabbed build passes230.103seconds open across four native QEMU
interaction cycles, including scrolling, resize and both tab orientations. Rendering uses software SWGL
readback, not GPU acceleration or zero-copy. Bounded tabs, complete resource
retirement, persistent storage and broader browser behavior remain unfinished.
See [Native browser shell continuation](#native-browser-shell-continuation-2026-10-01).
The following audit and early port results are historical.

Servo checked out at `8319b662` (2026-09-25), 1,153 crates, Rust 1.97.1
(CuBit's pinned toolchain is 1.98.1). The audit below was the first
`cargo check -p servo --keep-going` for `x86_64-unknown-cubit` against the
Motor-based std (`userspace/rust/std`), before decision B.

## Audit

Crates that failed to compile (the first layer: crates that depend on a
failing crate were not reached):

| Crate | Why | Class |
| --- | --- | --- |
| `mio` (47 errors), `socket2` (9) | event loop and sockets under tokio/hyper: Unix/Windows only | networking |
| `getrandom` 0.4 | no randomness backend for an unknown OS | small |
| `libz-sys` | wants `libc` types (C zlib) | libc |
| `servo-allocator` | wants `libc::malloc`, `malloc_usable_size` | libc |
| `wr_glyph_rasterizer` | glyph backends are FreeType-on-Unix, macOS, Windows | fonts |
| `stylo` (2) | thread handles are Unix/Windows | small |
| `imsz` (1) | a cfg-dependent code path | small |
| `gaol`, `arboard` | sandbox and clipboard: platform code | disable |
| `surfman` | GL platform layer | rendering |
| `aws-lc-sys` | C/CMake crypto build (rustls provider) | C toolchain |
| `mozjs_sys` | SpiderMonkey, C++ | C++ toolchain |

Native code Servo needs on CuBit regardless of the choice below:
SpiderMonkey (C++), HarfBuzz (C++), FreeType (C), OTS (`fontsan`, C++),
a crypto backend (`ring` or `aws-lc`, C/assembly), SQLite, zlib, zstd, and
a software GL for WebRender (SWGL, C++). Platform crates for Android,
OpenHarmony, Windows, Wayland/X11 and GStreamer media are not needed.

Rendering: WebRender draws through OpenGL. Servo's own software path uses
surfman's software adapter (OSMesa/llvmpipe, i.e. Mesa). The smaller route
is SWGL, Firefox's software GL written for WebRender, behind Servo's
`RenderingContext` trait, presenting into the Ada chrome's Surface widget.

## The decision: how Unix-like CuBit looks to ported code

**A. Stay non-Unix** (`target_os = "cubit"`, today's std). Give each
platform crate a CuBit backend: mio over CuBit IPC/netstack, socket2,
getrandom, the allocator, WebRender's glyph backend, surfman. Build a C
runtime only as large as the C/C++ libraries need.
- For: capabilities stay explicit; no POSIX emulation in the system.
- Against: a long tail of crate patches to maintain; the C/C++ libraries
  still need most of POSIX (threads, `mmap`, files, time) anyway.

**B. A CuBit libc, and `target_family = "unix"`** (Redox's approach with
relibc). One POSIX layer over CuBit syscalls and services (pthreads on
threads/futexes, `mmap` on a new region API, files over the filesystem
service, sockets over netstack, `poll` over IPC activity), used by C, C++
and Rust alike; Rust std uses its Unix platform layer on top.
- For: most of the 1,153 crates and all C/C++ dependencies build as they
  are; one place to maintain.
- Against: a large upfront layer; ambient-authority POSIX APIs (paths,
  sockets) must be mapped onto capabilities carefully.

**Decision (2026-09-25): B, a WASI-style CuBit libc.** It is based on
musl (MIT). musl funnels every OS call through one syscall macro layer; CuBit
replaces that layer with an in-process implementation over CuBit syscalls
and services:
- threads: `clone` with TLS and a child-clear-tid word, which is exactly
  THREAD_CREATE's contract;
- futexes, time, exit, write and memory: direct CuBit syscalls;
- files and sockets: CuBit services, with paths resolved only inside
  capabilities granted at launch, like WASI preopens;
- no signals.

musl's pthreads, stdio, malloc, locale and math come along unmodified. The
Rust target triple on top (a CuBit Unix target, or the musl ABI directly) is
chosen once the libc runs.

**Status (2026-09-25):** the CuBit libc runs natively (userspace/libc):
C with pthreads and `__thread`, and C++ (libstdc++: exceptions,
`std::thread`, `thread_local`, iostreams) pass the `libc` headless case.

Either way the next foundations are the same:
1. an address-space region API (`map`/`unmap`/`protect`) — SpiderMonkey's
   GC and JIT and Rust stack guards need it;
2. a C/C++ toolchain for CuBit (clang, a libc, libc++ and libc++abi);
3. networking below tokio (mio's backend or a libc socket layer);
4. SWGL presenting into a Surface.

## Port status

Tooling, all in `userspace/servo/` (Servo's own files are patched only by
these scripts, on a checkout):
- `servo-cargo.sh`: cargo for the Unix-family `x86_64-unknown-cubit`
  target (std from `userspace/rust/std/unix`, C/C++ through
  `userspace/libc/cubit-cc`/`cubit-c++`), no default features, `bundled`
  (baked-in resources, bundled FreeType); no JIT, multiprocess, sandbox,
  clipboard, WebGL, WebGPU or WebXR.
- `crate_fixes.py`: crates.io crates that only describe the Linux/musl C
  ABI take their Linux arms (`libc`, `socket2`, `libloading`), plus exact
  edits with reasons (`rustix`, `mio`, `tokio` ucred, `mozjs_sys`
  building SpiderMonkey for the musl ABI and statically linking libstdc++,
  `ipc-channel` in-process).
- `patch_servo.py` and `overlay/`: Servo edits (no `gaol` sandbox; fonts
  through FreeType with a CuBit font list, not fontconfig; `navigator.platform`
  "CuBit" and a CuBit user agent; the `cubitshell` workspace member).
- Servo's build state lives in `userspace/rust/build/servo-work/`
  (checkout, `CARGO_HOME`, target; ~16 GB, not in /tmp).

Where it stands:
- `cargo check -p servo` passes for `x86_64-unknown-cubit`, SpiderMonkey
  (C++) included.
- `cubitshell` (`overlay/ports/cubitshell`) is the CuBit embedder: a
  `RenderingContext` over SWGL, WebRender's software GL, compiled for CuBit;
  no surfman platform backend, GPU or windowing system. It loads one page
  and reports the frame (hosted: writes a PPM). Presenting into a CuBit
  surface is the next stage.
- Linux-hosted demonstration (not CuBit): the same `cubitshell`, built for
  `x86_64-unknown-linux-gnu`, renders a test page (heading text and a
  coloured box) through SWGL into an 800x600 frame and writes it as a PPM
  (first frame in ~130 ms). It found one SWGL incompatibility: WebRender's
  quad clears use `GL_ALWAYS` depth testing, which SWGL lacks; Servo's
  painter now turns them off on SWGL, as Gecko does for software rendering
  (`patch_servo.py`).
- On CuBit, given a desktop capability, `cubitshell` opens a 1024x700
  window on desktop.svc (lent BGRA buffer, as NetSurf's frontend does),
  presents each frame there and forwards pointer, wheel, text and editing
  keys to the page; without one it reports and exits.
- **Servo links as a native CuBit program**:
  `userspace/servo/build-cubitshell.sh` builds `cubitshell.app` (85 MB
  stripped, static, release; manifest sections added after linking, so a
  manifest change does not rebuild Servo; 8 MiB main stack).
- Headless case `servo` (`tests/headless/init-servo.ccl`, 4 GB guest,
  fonts installed under `@nvme:0/fonts`). First run: procmgr could not read
  the image, because the development disk's 1 KiB ext2 blocks reach ~64 MiB
  without triple-indirect blocks, which CuBit's ext2 did not read. The case
  now rebuilds its disk copy with 4 KiB blocks. (Triple-indirect support was
  added 2026-09-28; see ext2-interoperability.md. The 4 KiB copy is unchanged.)
- **Native result (2026-09-25): `headless: PASS servo`.** cubitshell loads a
  data: URL test page, renders it through SWGL into an 800x600 frame
  (22,376 inked pixels; 22,817 in the Linux-hosted run; first frame
  189 ms) and exits. The pixel count, not an image, is checked; the frame
  is not yet shown anywhere.
- What it took, in order of the failures found by running natively:
  gcc's split `_init` (crti/crtn) lost to `--gc-sections` (link.ld KEEP);
  mozglue's `dlsym(RTLD_NEXT)` interposers (left out of static builds);
  mio's pipe waker with no CuBit arm (returned fd -1 as success); the libc
  gained in-process pipes, socket pairs, `dup`, a blocking `poll`, thread
  names and `gettid`; `inventory`'s `.init_array` registration (baked-in
  resources); getrandom through the libc's `getrandom(2)`; Servo's storage
  threads use their in-memory engines without a granted config directory
  (no ambient /tmp); TLS roots from webpki-roots until Servo uses CuBit's
  root store.
- **Networking (2026-09-25):** the libc's TCP sockets go over netstack
  (`userspace/libc/overlay/src/cubit/net.c`; names are resolved by netstack
  inside the program's scope, never by the program). The `servo` case now
  loads a data: page and then `http://10.0.2.2:18470/servo-test.html` from a
  host fixture (`tests/servo/http_server.py`); both render (22,376 and
  31,509 inked pixels).
- **HTTPS (2026-09-25):** a third page, `https://tls-test.cubit.internal:18460/`
  (the TLS test fixture), is fetched over TLS 1.3 by rustls inside Servo over
  the same sockets and renders; the fixture confirms the verified session
  and Host header. Trust: on CuBit, Servo's roots come only from the system
  trust store (`@nvme:0/tls/roots.der`, the DER bundle tls.svc reads;
  intended to become a trust store service), not its built-in list. Wall
  time for certificate validity comes from clock.svc through the libc.
- **Public sites (2026-09-25, opt-in: `SERVO_EXTRA_PAGES` for the `servo`
  case; needs the host's internet):** https://example.com/ and
  https://www.wikipedia.org/ load over real HTTPS (netstack DNS, rustls,
  the system trust store) and render natively; frames checked by eye
  (`SERVO_DUMP_FRAMES=1` dumps them to the serial log;
  `tests/servo/frame_from_log.py` extracts them). Wikipedia first needed
  netstack's transmit queue raised: dropped segments stalled connections,
  since netstack's TCP has no retransmission yet (docs/netstack-redesign.md).
- **On the desktop (2026-09-26):** `SERVO_DESKTOP=1` for the `servo` case
  boots the desktop session, opens Apps with the keyboard and launches Servo
  from the menu, which now comes from Config (`desktop.launch.*` settings in
  system.ccl, read by desktop.svc); procmgr grants cubitshell.app the same
  desktop-launched outbound rule as NetSurf. cubitshell opens a 1024x700
  window, renders each page, presents it (RGBA read back and swizzled to the
  window's BGRA; presented after every paint) and forwards input; the
  screenshot shows the HTTPS test page in the window (PASS). Manual sessions
  still need the desktop disk to hold the 85 MB binary (4 KiB blocks).
- Denied, correctly, and tolerated by their callers: `/proc/self/*`,
  `/sys/devices/system/cpu/*`, `/dev/urandom`, `/dev/sysgenid`.
- Fonts: the CuBit font list scans `/fonts` (`@nvme:0/fonts`), which the
  libc now reads through filesystem.svc (read-only).

Runtime gaps known before the first native run: sockets, a memory region
API (anonymous `mmap` grows the heap; `munmap` does not return memory),
writing files, and presenting frames.

## Native browser shell continuation (2026-10-01)

The browser shell is moving from the proof-of-concept's raw, permanently
writable Desktop attachment to `userspace/servo/native/servo_shell.*`. This
standalone Ada library owns native chrome and calls `CuBit.UI.App` with protected
frames. Rust owns Servo/WebRender/SWGL; a narrow, serialized C ABI transfers
configuration, input, engine notifications and a borrowed page readback. A
writable destination pointer or grant never enters the Rust browser engine.

The first functional milestone is one tab with an editable address field,
Back/Forward/Reload, Ctrl+L, Ctrl+R, Alt+Left/Right and Ctrl+W. Native chrome uses
the existing text editor and density-aware UI rasterizer; the latter links as
a Rust library inside Servo's runtime rather than importing a second allocator
and panic handler from the standalone font archive. Normal desktop sessions
remain interactive while the initial page loads. Explicit fixture sessions
retain the earlier batch page/ink checks.

The software presentation boundary has one full SWGL RGBA readback. Ada converts
bottom-up RGBA directly into the currently acquired frame's page region and
draws current chrome into the same buffer. The old full-size BGRA conversion
allocation and following full-frame copy are removed. The current fallback
reconstructs the whole candidate to cover both frame slots' repair debt; it is
not zero-copy or damage-limited WebRender rendering. Unavailable frames defer
publication and preserve pending work through the existing owner.

Viewport extents use `Client_Canvas_Geometry.Relative`, including the toolbar's
fractional origin phase. Servo receives physical viewport dimensions and its
separate HiDPI scale factor. Input uses a proved signed cell-edge mapping into
device pixels, matching Servo's `WebViewPoint::Device` contract. Pointer capture
coordinates can remain negative; only foreign signed32 overflow saturates.
The existing proved `Client_Input_Budget` bounds admission before returning to
engine work and presentation. Servo notifications unpark the main thread;
Desktop input still needs a combined completion/input wake path, so the current
idle fallback polls at 1 ms. This is not a latency measurement or guarantee.

Proof boundaries: pixel conversion/admission and signed input geometry are
SPARK packages, with independent hosted pixel and round-trip oracles. Existing
frame, damage, configuration and input-budget policies are reused. The complete
Ada shell, pointer bindings, IPC, Rust engine and Mesa are outside these proofs.
Rust promises readable, non-aliasing source slices during a synchronous call;
the native owner promises valid exclusive destination mappings. The shell does
not yet opt into causal input provenance for asynchronously handled page events.

A native rebuild exposed a platform compatibility gap before the first frame:
SpiderMonkey's Unix GC alignment path trims mappings using partial `munmap`,
where CuBit currently accepts exact owned-region release only. The CuBit crate
patch now selects SpiderMonkey's existing `posix_memalign`/`free` strategy for
aligned GC regions, preserving accounting and real protection operations.
The second native attempt passed 32 allocator/protection cycles and two frame
cancellation cycles, then exposed an overly broad unmap adaptation during the
engine address-width probe. The corrected patch leaves `UnmapInternal` intact
and uses `free` only in the paired GC `UnmapPages` path. Hosted ASan/UBSan now
covers 32 ordinary mmap/unmap probes as well as 288 aligned allocation pairs
and nine allocation failures. It does not add a no-op unmap or weaken `mprotect`. A third native run reached page-load completion, then SWGL failed a texture
allocation. The software backend now requests a supported 2048-pixel internal texture limit
and smaller 1024-pixel RGBA atlases through existing WebRender options: the previous 2048-pixel RGBA
atlas alone matched the current 16 MiB mapping limit before malloc overhead.
This bounds those atlas dimensions, not total engine memory, and does not
change future hardware-context defaults. The subsequent v9 native test validates the corrected bridge with readable
data, HTTP and HTTPS rendering and a short interactive sequence. Sustained
browser stability remains a separate pending gate. See `tests/servo/README.md` for the dedicated regression.

The shell acquires its protected frame before requesting SWGL readback. A
serialized Rust guard cancels abandoned paints through the Ada owner, including
a changed viewport; deferred acquisition therefore avoids another full-page
readback. Hosted FFI mocks cover guard failure paths, and native startup checks have
passed two cancellation/reacquisition cycles. The subsequent v9 test also validates functional navigation; startup checks
alone do not establish it or sustained browser stability.

Remaining browser work includes sustained native validation, tabs and
bounded tab lifetime, richer navigation/error/dialog/download behavior,
clipboard/selection and accessibility, configured mixed-DPI tests, overload and
failure recovery, and a hardware rendering backend. Servo's current WebRender
context consumes a GL API: hardware integration must use the existing Mesa
stack through an appropriate context/import bridge, not merely claim success
from the separate Vulkan or Linux-hosted demonstrations. Software fallback and
the same protected publication contract must remain available.

The standalone Ada bridge uses its own aligned 32 KiB secondary-stack storage:
the shared runtime's normal getter addresses a location outside Rust's 8 MiB
main stack. Linker wrapping redirects every secondary-stack runtime caller to
the local getter, which initializes storage before returning it, including a
pre-binder call. An ELF check verifies this interposition. The existing binder
default pool still exists; the private scratch size is not the total runtime
footprint. Rust enforces single-threaded, non-reentrant bridge entry. These are
audited FFI/runtime boundaries, not proved compositor policy.

The corrected native bridge now publishes colored page content and readable
text from data, HTTP and HTTPS pages. CuBit font loading uses owned bytes
for WebRender's FontData and a private read-only map for FreeType's retained
face/table backing; shared file mappings remain unsupported. The first
35ms/key QEMU TCG run reported
input resynchronization and lost address characters. The functional fixture
now uses 300ms/key; that pacing is explicitly not overload or latency evidence.
Chrome avoids repainting on key releases or printable key presses before their
text event. Input resynchronization invalidates an in-progress address and
blocks submission until Ctrl+L restarts editing, with visible recovery guidance;
ordinary resize/configure preserves the edit. The hosted actual editor/resync
branches pass 1,000 cycles. The corrected native v9 run passes a short functional sequence within a
180-second four-CPU TCG session and its final fault scan: address navigation, real
pointer click/focus, DOM typing, Back/Forward with restored-document handlers,
reload and close. The screenshot shows readable page content and native chrome.
This is the first functional software browser milestone; scrolling, browser
resize/DPI coverage, bounded tabs and sustained overload readiness remain open.

### Tabbed shell and browser authority

The native shell retains Back, Forward and Reload and adds horizontal tabs or a
vertical side rail, a new-tab button, per-tab close buttons, Ctrl+T, Ctrl+Tab,
Ctrl+Shift+Tab, Ctrl+W and Ctrl+Shift+W. The tab-layout preference lives in
the native Settings modal, which blocks underlying browser/page input and
supports pointer selection, Tab, Space/Enter and Escape. Changes take effect
immediately and save through Config. The
selected view supplies the address, loading state and history controls; each
view retains its own page and navigation history. Each window admits up to 32
resident WebView containers sharing its own SWGL rendering context. Only its
selected view is shown. Up to four browser windows have independent native
chrome, input state, addresses and tab collections. Ctrl+N or File > New window
opens a window; Ctrl+Shift+W closes the current window. Native UI.App.Close uses
Desktop Destroy_Surface so sibling windows in the process survive. The
browser opts into DP.Graceful_Close (bit256) and routes INPUT_CLOSE_REQUEST
(event10) to window closure before Settings intercepts input. Desktop retains
that request outside its lossy input queue; legacy non-opted-in clients keep
their existing close behavior. Failed
closure retains the native slot and frame retirement state for retry.

Horizontal tabs shrink to a 44-pixel minimum with separate close buttons.
Previous/next buttons appear when either orientation has more tabs than fit;
keyboard tab cycling also keeps the active tab visible. Back and Forward use
native arrow icons with captions and disabled history states.

Closed window contexts remain resident and are reused only after every view
has completed the same blank-load/history-clear handshake as closed tabs.
The event loop services each window in bounded input batches. Native frame
leases pin the selected session until presentation or cancellation. Hosted
router tests cover capacity, independent state, pinned frames, denied close
and reuse. The 32-tab policy and geometry have 39 SPARK results, none unproved
or justified.

Native v26 passes the complete 360-second headless gate and final fault scan.
Its feature sequence exercises 16 live tabs, overflow in both orientations,
four simultaneous windows, capacity rejection, title-bar closure with Settings
open, sibling survival, closed-window reuse, and Config preference retention
across browser relaunch. Settings blocks both Ctrl+T and background clicks;
Escape and keyboard activation of Done dismiss it. The resize oracle checks
31,930 restored wallpaper pixels. This run contains one 66.144-second ordinary
interaction cycle followed by the expanded feature sequence; it is not a fresh
180-second sustained-interaction claim. Earlier sustained evidence below stays
scoped to its corresponding build.

The fixture waits for parked tabs and loaded windows before typing. A prior
cold-window run overflowed the input stream; the shell refused to submit its
partial address. That interruption now takes priority over capacity messages
in the status bar. These tests do not establish overload or hardware latency.
Evidence: `/tmp/cubit-servo-browser-v26.serial.log`, its adjacent
`serial-browser.timeline.jsonl`, `/tmp/cubit-servo-browser-v26-features.json`,
and `/tmp/cubit-servo-browser-v26-run.log`. Tested browser SHA-256:
`7293c4feba8b13ae2b0b6034a86bcc5898042505090156922d7078297fc09bb0`.

The browser uses the [native menubar toolkit](native-menus.md) for a dedicated
File/Edit/View row above navigation. File contains New tab, New window, Close
tab, and Close window; Edit contains Select address and Settings; View contains
Back, Forward, Reload, Previous tab, and Next tab. Settings and New window no
longer occupy toolbar buttons. F10 activates the menu and Alt+F/E/V opens a
title; arrows, Enter, Escape, and item mnemonics route through native menus.
Disabled history actions cannot activate. Popups intercept page input and an
outside dismissal consumes its press and release. Menus use their own retained
control map; Settings remains the modal owner when opened. The new menubar
consumes 24 logical pixels in both tab orientations.

Native v27 validates the integrated menu row through the full 360-second
headless gate and final fault scan, including 16 tabs, both overflow layouts,
four windows, isolated closure/reuse, and Config reopen. The fixture invokes
File > New tab by mouse, Edit > Settings by mouse and mnemonic, View > Reload
by mnemonic, and F10/Right with an outside page click that must only dismiss.
Actual native captures are `/tmp/cubit-servo-v27-file-menu.png` and
`/tmp/cubit-servo-v27-edit-menu.png`; the feature report is
`/tmp/cubit-servo-browser-v27-features.json`.

The final v28 build adds explicit menu-to-dialog keyboard ownership and clears
old chrome pointer capture when a menu opens. Its focused 180-second native
gate/fault scan passes, including opening Settings by mnemonic, clicking Done,
and immediately typing into the page without losing the first character.
Menu dismissal does not click through. Config reopen and the built ELF authority
audit pass. Browser SHA-256:
`97d75aa3f68cef7cf638066cf5407bd1191e6ca64e7ada92ab55a18b4cb18b61`.
Final native menu screenshots: `/tmp/cubit-servo-v28-file-menu.png` and
`/tmp/cubit-servo-v28-edit-menu.png`. Tab/geometry proof remains 39 results with
no unproved or justified checks; this is not a proof of the menu event bridge.

The subsequent toolkit spacing pass uses eight-pixel toolbar gaps, compact
menu separator rows, padded/clipped labels, continuous dividers, and an
unshadowed Settings frame. Its v29 native 180-second gate passes menu/modal
keyboard handoff, navigation, DOM input, tab orientation, resize, browser
reopening, and the final fault scan. The missing shared development disk was
replaced for this test by a private, verified fixture under `/tmp`; no user disk
was modified. The stronger raised/inset control treatment and its hosted
checks are described in [UI polish](ui-polish.md).

The final v30 native build includes the approved framed gradient menu strip,
always-visible model-driven mnemonic underlines, stronger button/field bevels,
tabs with no bottom bevel, and inset list/scroll containers. Its full 180-second
four-vCPU TCG gate and final fault scan pass, including keyboard menu activation,
Settings handoff, navigation, resize, tab orientation and Config recovery after
browser reopening. Native captures are `/tmp/cubit-servo-v30-file-menu.png` and
`/tmp/cubit-servo-v30-edit-menu.png`; inputs are recorded in
`/tmp/cubit-servo-browser-v30-inputs.sha256`. This is QEMU integration evidence,
not a hardware latency benchmark.


The SPARK slot policy does not reuse a closing slot until the audited Rust
adapter has observed an `about:blank` load followed by a cleared-history
notification. The view container remains resident and can then be reused.
This avoids repeated container creation and does not interpret `WebViewClosed`
as retirement. It does not bound all script pipelines or total Servo memory.
Native v14 confirms retained page input, navigation controls and pointer
mapping in both orientations, tab close/reuse and browser-restart Config
preference recovery. The tested ELF scope audit confirms exactly these grants.

The shell now uses the native `CuBit.UI.Widgets.Tab` container overload for
both orientations. It returns a clipped child canvas and matching theme for
captions, icons and independently registered controls. Servo supplies a caption
and native close button; favicon fetching is not part of this change. Shared
retained control routing handles pointer capture and activation before paint;
a close click cannot also select its containing tab. Button hit bounds respect
the canvas clip, including an empty content area. Hosted widget tests cover
200 interaction cycles, repaint during capture, cancellation and clipping;
the production native bridge compiles. Native v17 validates this widget
migration through four interaction cycles, including inactive-tab close-button
isolation, both orientations and retained DOM input.

The shared native renderer now uses small logical-pixel corners and subtle
single borders for buttons, with distinct hover/pressed/disabled states.
Selected tabs retain an orientation-specific accent; close buttons use the
native widget's quiet presentation until hovered or pressed. Horizontal tabs
adapt to the live tab count with a 220-pixel maximum instead of reserving eight
narrow slots. The tab geometry remains bounded for every supported width and
count; its SPARK run reports 32 results with none unproved or justified. These
visuals use the existing theme, font and renderer callbacks, with no animations.
The actual light/dark widget preview is `/tmp/cubit-tab-style-preview.png`.
Native v18 validates this visual update for 180.839 seconds of browser
interaction across three complete cycles and 132 callbacks, followed by Config
layout recovery after reopening. All three 31,930-pixel resize comparisons and
the full 360-second QEMU runner/final fault scan pass. The native captures are
`/tmp/cubit-servo-polished-tabs-{horizontal,vertical}.png`; the report is
`/tmp/cubit-servo-browser-v18-stability.json`. Existing shared-renderer tests
pass 144 Settings cases and the 256-density/font/clipping suite. Tested browser
SHA-256: `9fcf5b6e0a01194a161dfae9c4a67350b402931733f2a15e5e45088c879ff790`.

The layout key is `browser.servo.vertical-tabs`, accessed through Config with
read/write authority limited to `browser.servo`. The current scalar Config API
keeps values across browser restarts in the same OS session, but is in-memory.
Durable typed Config preferences are still required for reboot persistence;
no filesystem preference/profile fallback is permitted.

The browser manifest's only writable filesystem scopes are
`@nvme:0/Bookmarks` and `@nvme:0/Downloads`. Fonts, Servo fixtures/start pages and
TLS roots remain read-only. The filesystem service's read/write *endpoint*
permission allows request/reply transport; the separate path scopes determine
file authority. These grants do not implement bookmarks/download UI or create
the folders automatically. No writable cache or profile directory is granted.

Native v14 keeps the browser open for230.103seconds after the render gate,
completes four full interaction cycles and170callbacks, then closes and
relaunches to verify Config layout recovery. The full360-second four-vCPU TCG
runner and final fault scan pass. Evidence is retained under
`/tmp/cubit-servo-browser-v14-*`; this is native CuBit in QEMU, not NUC hardware
or input-latency evidence. Visual inspection finds stale pixels outside the
restored window after a resize; this has been reported through coordination to
the compositor owner. Functional responsiveness is not a clean-pixel verdict.

Native v17 keeps the browser responsive for 232.777 seconds, completes four
cycles and 172 callbacks, then verifies vertical-tab Config recovery after a
browser restart. The full 360-second, four-vCPU TCG runner and final fault scan
pass. Each cycle also compares 31,930 wallpaper pixels against the pre-resize
baseline, confirming the compositor's resize repair in those exposed regions.
This supersedes v14's resize artifact finding for this tested scenario.
Evidence: `/tmp/cubit-servo-browser-v17-stability.json`, matching serial/run logs,
and `/tmp/cubit-servo-native-tabs-{horizontal,vertical}.png`. Tested browser
SHA-256: `4b97709c69714147e3216c019268f52c7ec57c5eef97959cab8e0daa22ee88a1`.
The fixture now waits through QEMU's fragmented banner, command echo and final
prompt before reading a screenshot; its actual command implementation has a
fragmented-socket regression test. These remain functional QEMU checks, not
hardware performance or whole-engine memory bounds.

### Tab retirement boundary (source audit, 2026-10-01)

At the pinned Servo revision, `WebViewInner::drop` sends `CloseWebView` and
removes the view from Paint. Constellation sends `WebViewClosed` after requesting
browsing-context closure, while `close_pipeline` explicitly retains pipelines
until exit messages arrive. `handle_pipeline_exited` subsequently removes the
pipeline and sends a further message to Paint. Paint removal also submits
WebRender transactions. Therefore neither dropping the frontend handle nor
receiving `WebViewClosed` proves that all engine work and allocations retired.

A future tab admission policy must charge closing tabs until an audited engine
retirement boundary completes, including pending pipelines and rendering work.
Proving the slot counter alone would not prove that foreign resources satisfy
that boundary. Total memory also needs separate accounting for session history,
shared caches and page-dependent allocations; the SWGL atlas limits do not
supply that bound. Resident tab/window reuse is guarded by the blank-history
handshake described above; this does not establish full engine retirement or
a total-memory bound. Sustained interaction tests are responsiveness gates,
not memory-bound proofs.

## Licenses

CuBit is GPLv3. Everything planned is GPLv3-compatible; each import keeps
its license text and notices, and is listed with its license when vendored.

| Component | License | With GPLv3 |
| --- | --- | --- |
| musl (CuBit libc base) | MIT, parts BSD-2-Clause / public domain | compatible |
| moto-rt (std runtime table, forked), dlmalloc | MIT OR Apache-2.0 | compatible (Apache-2.0 is GPLv3-compatible) |
| Servo, Stylo, WebRender, SWGL, SpiderMonkey | MPL-2.0 | compatible (MPL 2.0 §3.3 secondary-license combination; files marked "Incompatible With Secondary Licenses" must be checked, none expected) |
| HarfBuzz | MIT ("Old MIT") | compatible |
| FreeType | FreeType License or GPLv2 (used under the FTL) | compatible (FSF: FTL is GPLv3-compatible, not GPLv2) |
| OTS (`fontsan`) | BSD-3-Clause | compatible |
| SQLite | public domain | compatible |
| zlib, zstd | zlib, BSD | compatible |
| webpki-roots (Mozilla's root list; TLS roots until Servo uses CuBit's root store) | CDLA-Permissive-2.0 (data) | compatible (permissive) |
| ring / aws-lc (rustls crypto) | Apache-2.0 AND ISC (ring 0.17); ISC/Apache-2.0/MIT/BSD (aws-lc 0.45) | compatible (neither carries the old OpenSSL license any more; recheck on upgrade) |

Rules for later imports: no GPLv2-only code (FreeType must stay under its
FTL option), no original OpenSSL/SSLeay-licensed code, and no "proprietary
blob" dependencies. `sparktls`/`sparkentropy` are not modified.

### Penny connection inspector

View -> Connection information displays the active main document's negotiated
TLS version, cipher and ALPN, plus the server-supplied certificate chain with
subject/issuer common names, validity dates and SHA-256 fingerprints. Wheel or
Page Up/Down scroll the chain. This is a connection inspector, not an assertion
that every subresource is secure or that the full page has finished loading.

The reproducible Servo patch carries its existing rustls handshake metadata
with the Document and restores it through the same activation path as the page
title. New navigation/reload clears the previous WebView metadata; the inspector
can display the committed document at HeadParsed while subresources still load.
URL matching prevents another navigation's certificate from being presented.
HTTP/local documents and failed TLS loads show no verified certificate details.

Penny reuses the already linked AWS-LC provider for parsing and SHA-256. Eight
narrow declarations match the pinned aws-lc-sys 0.45.0 generated x86-64 ABI;
upgrades must review those declarations and the symbol prefix. No SPARKTLS,
second crypto provider, separate probe connection, or certificate bypass is
added. Certificate display parsing does not establish trust: rustls remains the
verifier. Formatting is cached until URL, load status or handshake data changes.
The boundary limits the chain to eight certificates, each at most 64 KiB, and
uses bounded, control-character-sanitized native text buffers.

The negative-certificate regression exposed WebRender's default 2048-square
shared RGBA render target exceeding CuBit's 16 MiB mapping limit with allocation
overhead. CuBit software rendering now uses 1024-square shared targets through
WebRender's existing option; hardware defaults remain unchanged. This bound is
not a claim that every page-dependent engine allocation fits the platform.
Native fixture and public-site smoke-test instructions are in
`tests/servo/README.md`.

Native validation on 2026-10-02: the local leaf fingerprint matched exactly;
wrong-host, expired and untrusted certificates were rejected; HTTP navigation
cleared the previous certificate details. Five-tab navigation and final close
passed the fault scan. Fresh-session GitHub and Hacker News checks also closed
cleanly. Reproducible patch tests passed against both the current cache and
clean upstream source, with second-run bytes/mtimes unchanged.

Public-site observations (software rendering under 4-CPU QEMU TCG):

| Site | TLS | Rendering observed |
| --- | --- | --- |
| Wikipedia | Verified, TLS 1.3 / h2 | Rendered and reached load complete; some CJK glyphs lack font coverage. |
| DuckDuckGo | Verified | Partial content, subresources still loading at capture. |
| GitHub | Verified in a fresh session | Title received but body blank at the 15-second capture. |
| Hacker News | Verified in a fresh session | Article list rendered, still reported loading at capture. |

After DuckDuckGo in the same multi-tab process, GitHub and Hacker News did not
commit within 90 seconds. Both established TLS when started directly in fresh
processes. The underlying loading/resource-retention cause is not isolated;
these are not full site-compatibility passes. Evidence is in the ignored
`tests/servo/build/tls-native.log`, `tls-github-fresh.log`, `tls-hn-fresh.log`,
and their printed artifact directories. Inspector plus related rendering fix
increased the stripped app from 89,834,360 to 89,959,416 bytes (125,056 bytes,
0.139%). No additional TLS implementation was linked.

### Connection retention and process memory telemetry

Penny now requests 32 TCP connections (previously 16), within the current shared
64-channel netstack capacity. This is a ceiling, not a reservation. The pinned
Hyper client previously had no pool timer, so expired idle sockets were not
proactively removed. The CuBit-only patch supplies the existing Tokio timer,
five-second idle expiry and at most two idle connections per origin. Active
requests are not interrupted; simultaneous active requests can still exhaust
the bounded grant. No additional TLS provider or networking library is linked.

`SYSINFO_MEM_OWNED_SELF` (1602) exposes the calling process's tracked physical
frame count multiplied by page size. Syscall dispatch derives the process from
the running thread; the detail argument cannot select another process. This
constant-time live sample conveys no cross-process inspection authority. It is
owned physical memory rather than RSS: borrowed mappings, page tables and
service-side allocations are outside this count. Penny's opt-in performance
fixture samples every five seconds, records its observed peak, and verifies a
2 MiB mapping charge/release before worker threads start. Exact peak tracking,
shared-memory attribution and privileged system-wide monitoring remain separate
work; no leak-free or physical-hardware-performance claim follows from a short
QEMU run.

Current performance evidence (2026-10-02): the native owned-memory oracle passes
its 2 MiB allocation/release check. A 202.008-second interaction run completed
three full navigation/edit/history/reload/scroll/resize/tab-reuse cycles, each
checking 31,930 wallpaper pixels. Its later overflow phase exposed an obsolete
test assumption: the compact vertical rail fits 16 tabs, so the current fixture
uses 20. This was not a complete suite pass.

The HTTP endurance gate still fails intermittently. A captured failing flow
completed SYN/SYN-ACK/ACK but sent no HTTP payload before the eight-second
watchdog. Replacing the full service image or just netstack changed the failure
point but did not eliminate it; service-image causality is unproven. Direct
std/libc socket stress now separates connection completion/readiness from
Servo, Hyper, TLS and rendering. These results do not establish daily-driver
network reliability or absence of memory leaks.

The subsequent 20-tab/four-window run passed tab overflow and window admission,
then aborted during navigation at about 406 seconds. `JS_NewContext` returned
null and mozjs unwrapped it; the fault address resolves to `mozalloc_abort`.
The last owned-memory sample was 1,511,911,424 bytes. This establishes a context-creation
failure during sustained use, not its cause or a proven memory leak.

The first direct socket probe could not begin stress: Rust's timed-connect
helper hit unsupported `ioctl(FIONBIO)` (`ENOTTY`). The libc dispatch now supports
it, preserving descriptor flags, and the probe again uses standard timed
connect/nonblocking APIs. A second libc fix serializes opportunistic and blocking
completion collection and wakes followers after releasing collection ownership.
Deterministic host schedules reproduce the old lost handoff and concurrent
collectors and pass with the fix, including failed WAIT submission and an idle
no-self-wake check. Flag/error tests pass too. These changes await a fresh native
libc/Penny build, isolated socket stress and the HTTP gate; they do not yet
establish that the observed browser stall is resolved.


### Daily-driver acceptance work (active goal)

The goal is a modern daily browser, not the current fixed-capacity shell.
Acceptance still requires dynamically sized tab/window metadata with safe
resource retirement and pressure handling, tab/session restoration through
Config with a clean-shutdown record, and everyday browsing features that work
across representative public sites. The existing 32-tab/four-window limits do
not satisfy the arbitrary-tab requirement.

Memory acceptance needs repeated navigation, history, tab, window and media
lifetimes, allocation ownership accounting and steady-state baselines after
retirement. The observed 1.41 GiB/context-creation failure contradicts readiness;
short runs and history-cleared callbacks are insufficient. Current upstream
`handle_clear_session_history` only clears diffs, so its referenced pipeline
retirement is the next browser-side audit.

Performance acceptance needs JavaScript workloads with checked results,
load/interaction/scroll/video measurements, and Linux/Windows comparisons under
matched conditions. TCG timings cannot establish physical-hardware parity.
Measure interpreted SpiderMonkey before deciding whether a JIT is required.
Executable-memory work must preserve W^X, be capability-gated and explicitly
authorized; no broad executable mapping permission is enabled by this plan.

Capability acceptance includes malicious script/resource-exhaustion cases,
process and origin isolation, denied filesystem/config/device access, and
bounded CPU/memory/network/render use that cannot disrupt unrelated CuBit
clients. The current single-process Servo embedding and broad browser-level
outbound network grant alone do not establish that property. Only Bookmarks
and Downloads remain writable filesystem scopes; preferences belong to Config.

Graphics/compositor coordination is authorized. Their current contracts do not
yet provide an untrusted-client cross-process GPU image import or hardware
video-decode interface. Trusted rendering demos do not authorize browser
MMIO/DMA/render access. GPU rendering and streaming-video acceptance require
capability-scoped resources, explicit completion/epoch/retirement semantics,
malformed-client tests and native presentation evidence.

### Native JavaScript baseline (2026-10-02)

`tests/servo/run-js-benchmark.py` loads the shared `js-benchmark.html` fixture
in Penny and validates 30 title callbacks against independently computed
checksums. `js_benchmark_report.py` rejects missing, duplicate, reordered,
failed, invalid-clock and wrong-checksum samples. The runner saves exact
binary/fixture hashes, QEMU settings, host load, memory samples and a screenshot.
Use Nix and `coordination/build.lock` with default staged inputs. Alternatively,
`PENNY_STAGE` and `PENNY_APP` may point to a wholly private, coherent binary
snapshot; that run does not modify shared build state.

The first native baseline passed all six workloads, the owned-memory
charge/release oracle and the browser close callback. It used preserved
binaries from the earlier socket fixture, **before** the subsequent completion
ownership and history-retirement fixes. Artifact directory:
`tests/servo/build/perf-tmp/nix-shell.v4TO07/penny-js-p8odltwd`.

| Fixed workload | Median milliseconds (five samples) |
| --- | ---: |
| Integer arithmetic | 87.19 |
| Typed array | 952.78 |
| Object traversal | 515.32 |
| JSON roundtrip | 172.35 |
| Regular expressions | 59.98 |
| Numeric sort | 48.39 |

These are **QEMU TCG diagnostics on a shared host**, not physical-platform
performance or evidence of Linux/Windows parity. The build configuration
selects no JIT, but the preserved binary has no runtime execution-mode
attestation; the report explicitly records that limit. This microbenchmark
does not establish real-site responsiveness or justify enabling executable
memory. The screenshot captured a near-final frame because script title
callbacks precede display presentation; the complete timing results are in
`results.json` and the serial log. Peak sampled caller-owned physical memory
was 234,999,808 bytes, with the exclusions documented in `memory.json`; this
short run does not establish leak freedom.

### Completion ownership validation (2026-10-02)

The rebuilt libc/Penny pair passed the native socket fixture: 128 sequential
lifetimes, 256 across eight workers, then 32 held connections, excess-budget
rejection, idle reuse, and recovery. The prior build stalled during the
concurrent phase. Current evidence is under
`tests/servo/build/perf-tmp/nix-shell.qbIxm6/penny-sockets-1ig8xpb8` with exact
input hashes. A private snapshot of those binaries then passed all 80 ordinary
HTTP fetches across 40 origins (two sweeps), with zero failed responses and a
clean browser-close callback, in 109.80 emulated wall seconds including startup
and deliberate pacing. Its evidence is under
`tests/servo/build/perf-tmp/nix-shell.kFpGr6/penny-endurance-kblrotb5`.
This validates the tested concurrency/reuse path; it is not a throughput
benchmark or a guarantee that every network failure mode is resolved.
The history-retirement patch compiled in that build; its longer tab/window
memory regression is separate and was still running when this entry was added.

### History retirement: native interaction result (2026-10-02)

The current build completed the full native interaction fixture in 456.91
seconds: sustained navigation/input, 20 live tabs with overflow in both
orientations, four simultaneous windows, isolated window close/reuse, final
close, and reopening with the saved vertical-tab preference. The prior run
aborted during fresh navigation in the four-window phase. Evidence is under
`tests/servo/build/perf-tmp/nix-shell.yni310/penny-interaction-_23w_uze`.
The private fixture changed only input binary paths and launcher navigation
from four to five Down presses, derived from that image's `system.ccl`;
`owner-seed-cfc2grs6/interaction-adaptation.json` records the adaptation.

The original browser's 73 owned-frame samples started at 259,670,016 bytes,
peaked at 961,064,960 bytes (916.54 MiB), and ended at 923,320,320 bytes before
process exit. The reopened instance's 259,063,808-byte sample must **not** be
interpreted as reclamation within the original process. The reporter now
separates runs at each startup memory self-check; `memory-v2.json` applies the
corrected analysis to the unchanged serial log. Hosted tests cover restart
separation and invalid/missing measurements. Old pipeline exit messages are
observed, but closed blank WebView containers and closed-window contexts remain
resident for reuse. This passes the tested failure scenario, not leak freedom
or arbitrary-tab admission. The next scaling work must separate tab metadata
from resident engine resources and implement complete retirement accounting.

The four-window screenshot captured the generic child-surface placeholder
before the browser's first visible frame. `window ready` currently denotes a
loaded page, not scanout completion, so that capture is not visual readiness
proof. This has been reported to the compositor owner. The tab-overflow capture
shows the live tab strip; actual input/title callbacks validate subsequent
window navigation and reuse.

### Shared empty tabs: native isolation result (2026-10-02)

Untouched empty tabs now share one `about:blank` WebView per browser window.
Explicit navigation first replaces the selected tab's shared handle with a
new dedicated WebView. Back, forward and reload leave the shared document
unchanged. Closing an untouched empty tab drops only its frontend reference;
loaded-page parking and history retirement retain their existing protocol.
This is a step toward scalable tab storage, not removal of the 32-tab model.

The real Rust source passed metadata checking against the exact cached CuBit
Cargo dependency fingerprints, then linked privately with the corresponding
native libraries and passed the four-caller secondary-stack check. This private
build reused engine/Ada/libc dependencies; it was not a full shared-tree rebuild.
`tests/servo/build/blank-link-8uh2rhiu/inputs.json` records the link command and
source hash. Snapshot `blank-seed-ehvu_mhn` records the preserved kernel/services,
unchanged manifest and source hashes. The normal builder consumes the same
source changes when the shared build slot is available.

The normal 0.3-second typing fixture first failed before reaching tab creation:
Desktop reported `input_resync=1`, and Penny refused to submit a partially lost
URL. Preserve this as an unresolved input reliability failure, not a shared-tab
pass. Evidence: `perf-tmp/nix-shell.zqFZIR/penny-interaction-mzvz5k_9`. The
compositor owner identified the per-surface 32-event queue and the client's
1ms polling budget as the relevant delivery path; batched delivery/fairness
still needs implementation and validation.

A separate isolation-only run used 1.0-second character injection, recorded in
`blank-seed-ehvu_mhn/paced-input-adaptation.json`. It passed sustained interaction,
20-tab overflow in both orientations, four windows and isolated close/reuse,
and close/reopen with the saved preference in 605.56 seconds. The new
`shared_blank_checks.py` additionally verified 20 logical tabs with three
frontend WebViews, navigation of tab20 increasing that count to four, and tab19
remaining `about:blank` before and after reload while tab20 retained its URL.
These frontend counts include retained dedicated views and the blank cache;
they are not counts of all asynchronous pipelines or rendering resources.

Evidence is under
`tests/servo/build/perf-tmp/nix-shell.QEaqqo/penny-interaction-xmd52a5x`.
The original process's 100 samples peaked at 544,882,688 owned bytes (519.64 MiB)
and ended at 507,564,032 bytes. The reopened process is a separate report run.
Different typing pace and elapsed time mean this is not a controlled percentage
improvement over the earlier run. It establishes the tested empty-tab isolation
and functional behavior, not leak freedom, normal-speed input reliability,
physical performance parity, or arbitrary tab counts.

### Dynamic logical-tab migration (hosted foundation only)

`overlay/ports/cubitshell/src/tab_model.rs` separates logical ownership from
engine residency. It uses ordered, monotonically assigned 64-bit IDs rather
than reusable widget slots. Closing returns the value to the engine owner for
separate retirement. Selection ignores retired IDs; ID exhaustion fails without
wrapping. The projection visits at most 32 entries around the selected tab, with
O(log(total) + visible) work, and fills available space after closes or resizing.
The logical collection has no fixed count limit; allocation still uses Rust's
normal allocator policy, not a memory-pressure admission controller.

`nix develop -c python3 tests/servo/test_tab_model.py` passes four hosted tests,
including 10,000 simultaneous tabs, 30,000 mixed operations against an ordered
reference, stale IDs, reopening after last close, and integer exhaustion.
Source hashes and output: `tests/servo/build/tab-model-22l1owtx`.

This module is not yet wired into the browser. The existing Ada/Rust bridge
still enforces 32 tabs. The next migration must replace native slot ownership
with an atomic bounded projection carrying stable IDs, route new/cycle/close
requests to Rust, and separate live logical entries from pending engine
retirement. Native input must not reuse a captured row after its mapping changes.
Keep native geometry bounds for visible rows; do not raise the old slot cap.
Native overflow/selection/close tests above 32 tabs, plus real-page isolation
and memory measurements, remain required.

The next bridge layer now has matching Rust and Ada snapshot representations
(`tab_projection.rs`, `servo_tab_projection.ads/adb`). One transaction contains
total count, active logical ID, and up to 32 visible IDs/captions. Native
validation rejects duplicate, unordered or zero IDs, an absent active ID,
overlong captions, inconsistent counts and noncanonical unused fields. Rejected
publication leaves the previous snapshot intact. Mapping comparison distinguishes
caption changes from row reassignment, which requires cancelling pointer capture
when integrated with ServoSession.

`nix develop -c python3 tests/servo/test_tab_projection.py` passes a hosted
cross-language check: Rust creates a 10,000-tab model and emits its actual C-layout
bytes; Ada reads those bytes and verifies row IDs, active selection, title and
atomic rejection of malformed updates. Artifact: `tab-projection-bc389vtp`. This
is regression evidence, not a SPARK proof or live native browser integration.
The existing browser ABI and its fixed admission limit are still unchanged.

### Dynamic tab integration (2026-10-02)

The production Rust/Ada source now uses the dynamic model. Rust owns the full
logical collection and selection; Ada receives the visible snapshot and emits
new/select/close/cycle requests carrying stable logical IDs. Local widget IDs
still range over 32 visible rows. Reassigning rows cancels chrome pointer capture.
The old per-slot title/parking exports have been replaced by the capacity query
and atomic snapshot export. Visible titles are fetched only for projected rows.

Closed real WebViews move to a separate parking collection and retain the
existing about:blank/history-clear acknowledgment before reuse. Untouched blank
tabs release their frontend reference immediately. New logical IDs are assigned
even when an engine container is reused. This removes fixed tab-count admission
from the production browser; allocation/resource limits still apply. Parked
engine containers are still retained, so this is not leak freedom or a bounded
real-page residency policy.

Exact cached-dependency Rust typecheck passed (`blank-typecheck-ysqc0f62`).
Full private Ada archive build passed using the repository's Alire toolchain
(`dynamic-native-YBChBH5V`); a preceding system-GNAT invocation failed at binding
because its exception ABI differed from the runtime. Private native link
`blank-link-mlhgqvnc` and secondary-stack link checks passed. Snapshot
`dynamic-seed-fkwxx1nx` contains the new Rust/Ada shell with unchanged capability
manifest and preserved kernel/services. The shared staged application was not
replaced. Hosted frame-guard, five logical/snapshot tests and cross-language
projection checks passed (`dynamic-hosted.log`).

The dedicated native fixture passed opening 65 tabs, horizontal/vertical overflow
selection, wraparound, tab64 staying blank after navigation in tab65, closing all
extra tabs, and creating fresh IDs66/67 after close/reuse. Evidence:
`perf-tmp/nix-shell.nyhDmL/penny-interaction-gom7mudy`; 191.06 seconds including
startup. Before real-page navigation, 65 logical tabs used two frontend WebViews;
afterwards three. Its 31 memory samples peaked at 317,038,592 owned bytes and
ended at 309,096,448 bytes. This is a five-second sampled caller-owned metric,
not RSS, exact high-water mark, memory convergence or a cross-browser benchmark.
The paced URL fixture and preserved services do not resolve input-overflow issues.

The first screenshots preceded final software presentation despite callbacks
passing. A second identical run adds five-second visual settling intervals;
its screenshots require inspection before any visual completion claim.
Canonical interaction fixtures now use monotonically increasing IDs and request
65 tabs; the checker retains old-artifact compatibility. Its 36 existing negative
controls and four stale-ID controls pass. The expanded full sustained/four-window
fixture still needs a native run with the new bridge. Old Servo_Tabs and its
test-only fixed-pool suite remain for removal after the replacement gates settle.

The second dedicated native run passed as well; both settled screenshots were
visually inspected and show the loaded controlled page, selected end tab and
correct horizontal/vertical layout. Evidence:
`perf-tmp/nix-shell.iZ568F/penny-interaction-_nripiuo`,
`interaction-65-tabs.png` and `interaction-65-vertical.png`. This validates
those rendered states; the added settling delay does not measure frame latency.
All private build and VM jobs for this step are terminal. The shared staged app
is unchanged; the private tested binary is in `dynamic-seed-fkwxx1nx`.

### Closed-view ownership audit

At pinned Servo revision `8319b662d03f884a3e874c54deea0f09bae669f3`,
`WebViewInner::drop` requests `CloseWebView` and removes the corresponding paint
view. `Paint::remove_webview` removes an empty painter; `Painter::drop` makes
its own rendering context current, stops/shuts down WebRender and deinitializes
the renderer. Removing a single view from a shared painter submits pipeline
removal and a replacement root display list. These are existing upstream paths,
not missing CuBit patches.

Servo stores weak frontend handles and calls `clean_up_destroyed_webview_handles`
after each event-loop spin. That function already removes dead entries; do not
add duplicate cleanup based on the weak-map declaration alone. Its hash table
can retain peak capacity, which is a separate bounded-capacity/performance issue.
Late pipeline-exit paint messages use `maybe_painter_mut`; a missing painter is
handled. `WebViewClosed` is emitted before all pipeline exits, so it cannot be
used as an all-resources-reclaimed acknowledgment.

Penny currently retains every parked real view after blank-history cleanup.
There is also an extra root WebView reference in `main`: `run_window` borrows it
and clones it into its first BrowserWindow. Removing that extra reference by
transferring ownership is required before a retired first tab can actually drop.
Next implementation: transfer that ownership, keep only a small reusable ready
pool, and drop excess ready containers through the normal Servo API outside
paint/delegate borrows. Keep the shared blank alive while logical blank tabs use
it; preserve asynchronous pending retirement. Validate per-view repeated
navigation/close, root-tab close while another view survives, window-context
close/reopen, pipeline-count convergence, and same-process memory after idle.
No changes to kernel executable-memory policy or capabilities are needed for
this frontend lifetime correction. The current full native regression remains
on the pre-eviction binary to establish its baseline.

Penny's vertical tabs now match Desktop Settings' contiguous placement: the
existing native tab container still calls the same `CuBit.UI.Draw_Tab`, but
vertical stride is 26 pixels instead of 30 for a 26-pixel header. The 4-pixel
inter-row gap is gone; default-height capacity grows from 16 to 19 visible tabs.
Existing hosted geometry checks pass. Private native build/capture passed with
26 tabs and clean close; inspected screenshot:
`perf-tmp/nix-shell.4kSiep/penny-interaction-t2e22sd2/interaction.png`.
The tested app is `compact-seed-owfy801u`, including a privately rebuilt runtime
needed after the shared protocol declaration changed. Shared staging is unchanged.

The earlier expanded full regression (`dynamic-full-native.log`) passed two
interaction cycles and 65-tab close/overflow before failing during fourth-window
URL input. Serial line2810 reports `input_resync=1`; the screenshot shows the
truncated URL `http://10.0/browser-a` and the input-interrupted status. It does
not establish four-window completion. The compositor owner received this evidence
for the pending batch-input transport work; even 1-second typing did not avoid
this overflow with four windows under the tested software-rendering workload.

The closed-view correction now transfers the initial WebView into `run_window`
instead of retaining a second reference in main. The parking collection retains
at most one ready reusable real view; pending blank/history handshakes remain
untouched. Excess ready containers drop through Servo's normal API outside
paint/delegate borrows, with the corresponding SWGL context current. Diagnostic
`retire-request` markers report the request only. Rust metadata and private
link/secondary-stack checks passed; the new native lifetime fixture is running
against `retirement-seed-kbn00hj9`. Its idle-period serial traces are saved before
process shutdown so exit-time cleanup cannot substitute for live reclamation.
Native memory convergence and root-tab retirement remain unverified until that
run is inspected. No new renderer permissions or executable mappings are used.


Native retirement validation completed (`retirement-native.log`, session85321):
seven loaded tabs across two close cycles plus retirement of the original tab
while another tab survives. Both cycle boundaries reach five pipeline entries,
three contexts and three views before shutdown; retiring the original tab reaches
two of each. The remaining pipeline count must not be confused with frontend
view count. The analysis artifact records bounded pre-shutdown log ranges and the
serial SHA256 at
`perf-tmp/nix-shell.TQdwVs/penny-interaction-fbhjxbdh/retirement-analysis.json`.

The three 5-second owned-frame samples in each 15-second idle window were
323,035,136 bytes after cycle1, 340,852,736 after cycle2, and 284,872,704 after
root retirement. Thus normal-drop reclamation is observed before process exit,
but steady-state memory convergence is NOT established: the second cycle is
17,817,600 bytes higher. These samples exclude service allocations, borrowed
mappings and page tables. A private repeat with identical binary/workload and
60-second idle windows is running from `retirement-idle-qr4hsl7p` to distinguish
delayed reclamation from retention. This functional TCG run is not a physical
hardware performance measurement or a general leak-freedom result.

Source attribution for the five pipelines: the private runner loads three startup
URLs into the same root WebView before interaction; `main` calls `view.load` for
the later pages and preserves their session history. Three root history entries
plus the shared blank and one parked view are consistent with five pipelines.
Their reduction to two when the original tab retires supports that explanation;
individual pipeline identities were not instrumented, so it remains an inference.
Closed-window contexts are still retained for reuse, and main still owns its
initial SWGL context. Window-context reclamation needs a separate change and
native close/reopen validation; the tab retirement result does not cover it.


Window-context retirement implementation now transfers the initial SWGL context
into the window loop. Closed windows remain only until their blank/history
handshakes finish; after handling requests that reference current vector indices,
the loop drops their parked/shared-blank views with the context current and removes
the window. Opening another window creates a fresh context. Pending closed windows
still count against the existing window limit. Rust metadata and private linking,
manifest packaging and secondary-stack verification pass; native repeated-window
validation is prepared in `window-retirement-seed-2w1w5uhg` but has not run yet.
This does not remove the four-window limit or establish complete memory reclamation.


CPU media integration audit (pinned Servo source, not implemented):
`components/media/backends/gstreamer/render.rs` already has a non-GL path.
For CuBit the platform constructor returns None, so setup_video_sink requests
BGRA video/x-raw from appsink; get_frame_from_sample wraps a readable CPU frame.
No EGL/X11/Wayland renderer is required for that path. GStreamer/GLib, its player
and selected codec/demux plugins still need native dependency ports; enabling a
Cargo feature alone is insufficient. Current media remains DummyBackend.

CuBit already exposes `userspace/c/cubit_audio.h`: serialized, nonblocking
48 kHz stereo signed-16-bit writes through CAP_SLOT_MIXER, with partial-frame
acceptance and stream-local volume. Penny currently does not request mixer
service authority. A media adapter must honor partial writes, prefill/start,
audio/video clock synchronization and teardown; multiple tab streams require
explicit mixing/ownership rather than treating this single-stream API as a
per-player handle. No capability manifest change has been made.

GStreamer player.rs enables progressive downloading on platforms other than
Windows/Android when requested, which uses a local temporary file. CuBit must
disable that path/use bounded in-memory buffering to preserve Penny's existing
Bookmarks/Downloads-only write policy. Do not grant a writable cache directory
just to make upstream temporary-file behavior succeed. Plugin selection/static
registration and dependency footprint also need validation before choosing a
production backend. Ordinary file playback, seeking, A/V synchronization, live
streaming and YouTube's player are separate native acceptance gates.


The 60-second-idle retirement repeat completed successfully (58478). Its two
cycle windows each contain twelve identical owned-frame samples: 322,928,640
and 340,877,312 bytes, respectively, a 17,948,672-byte difference. The final root
retirement window contains eleven identical samples at 285,057,024 bytes before
shutdown. This contradicts a simple short-idle explanation for the difference;
it does not distinguish allocator/cache retention from a leak. Recorded hashes
and samples: `perf-tmp/nix-shell.Mhc7AL/penny-interaction-9gve8e76/idle-analysis.json`.
Repeated steady-state cycles and allocation attribution remain required. The new
window-retirement binary is now under its separate native close/reopen fixture.


First native window-retirement run (35380) failed after its first successful
loaded secondary-window close. Before shutdown, lifetime counts returned to the
original root only: three history pipelines, one context and one view. The next
Ctrl+N did not produce a window-ready callback; the saved screenshot shows the
surviving original window. This does not prove a reclamation crash or a focus
bug. An explicit root-titlebar-focus repeat is running against the same binary;
the original failure remains at
`perf-tmp/nix-shell.KKOqbF/penny-interaction-zv448sjx`. The compositor owner received
this evidence with the preserved older Desktop binary qualification.

Allocator audit: Penny uses the musl port, not userspace/c/cubit_mem.c. The built
musl config selects mallocng, whose glue.h defines USE_MADV_FREE=0. Although CuBit
accepts madvise without effect, the disabled mallocng free-slot advisory path
cannot directly account for this run's inter-cycle memory difference. No allocator
or kernel changes have been made on that hypothesis.


Explicit-focus window retirement validation passed all three loaded secondary
windows and surviving-root new-tab navigation, followed by clean exit (20444).
Each closure returned to three root history pipelines, one context and one view.
Idle owned-byte samples were 300,412,928, 313,139,200 and 325,644,288, four identical
samples per window. Thus engine topology converges but physical memory does not.
The hash-bound analysis is
`perf-tmp/nix-shell.HOH360/penny-interaction-vyb0degt/window-retirement-analysis.json`.
The no-click focus failure remains open and was handed to the compositor owner.

A concrete independent reclamation defect was found: libc's overridden
`__unmapself.s` exits detached threads without releasing their stack mappings;
its comment explicitly documents the old absence of munmap. Current kernel
`reclaimThread` frees the kernel stack and thread-table state, not that separately
owned user mapping. Musl passes map_base/map_size to __unmapself after TLS/list
cleanup; ordinary whole-owned-allocation release now exists. A stackless release
then exit sequence needs return-path/clear-TID auditing and a native detached
thread churn test before implementation can be called safe or attributed to the
browser measurements. No kernel ABI or executable-memory policy was changed.
The prepared six-cycle browser test is deferred in favor of this focused defect.


Detached-thread stack reclamation is now fixed in libc's __unmapself override:
it issues existing RELEASE_OWNED_MEMORY for musl's exact stack/TLS allocation,
then THREAD_EXIT without touching stack or TLS. The kernel syscall return uses
its own stack; the clear-tid word is the process-global musl thread-list lock.
No new kernel API, capability or executable-memory permission is involved.

Native baseline/candidate testing and a repeat against rebuilt production libc
both passed the regression/control oracle: after warmup, 32 detached workers with
2 MiB stacks retained 67,502,080 bytes with the old routine and zero additional
owned bytes with the fix (2,121,728 before and after). The canonical regression
source is `userspace/libc/tests/detached-reclaim.c`; it also exercises joined
threads, TLS and stack pages, and polls asynchronous memory convergence. Its
joined-thread step is not treated as a detached-exit acknowledgment. Artifacts:
`tests/servo/build/detached-retirement/production-run.log` and
`production-inputs.json` (the latter identifies the canonical source and rebuilt
libc; the runner's historical check.c hash is not the production test source).
Penny relinking is underway; these focused results do not yet prove that all
browser-cycle memory growth is resolved.


Penny's post-fix three-window comparison passed (46470), using byte-identical
kernel/service seeds and explicit-focus fixture. Post-close owned bytes were
283,545,600, 283,525,120 and 283,496,448, versus 300,412,928, 313,139,200 and
325,644,288 before the libc fix. Each cycle returned to root-only engine topology
(three history pipelines, one context, one view). This workload's previous
roughly 12 MiB-per-cycle growth is gone. It does not establish general leak freedom.
Analysis: `perf-tmp/nix-shell.dlSE2C/penny-interaction-n0mc5nwv/window-retirement-analysis.json`.
Six repeated tab cycles are now running on this corrected binary.

Penny opts into the compositor's new synchronous input batching through
`App.Open(... batched_input => True)`. Its public Poll_Input event contract is
unchanged. Private runtime/native archive build passed; linking is underway.
Native validation must use the new Desktop service (SHA82512165bc0d640a1ea8e17f0b33b16dd1cdae7b751364b8c674b4208db76ede)
and establish actual batch delivery, since fallback-only success cannot validate
the transport. Original-rate four-window input and no-click focus remain open.

### GC page-discard audit (2026-10-02)

The current CuBit libc `SYS_madvise` adapter returns success without an
effect (`userspace/libc/overlay/src/cubit/syscall.c`). This is a concrete
reclamation candidate, not a measured explanation of the retained-memory delta.
The pinned SpiderMonkey 153.3.0-0 `js/src/gc/Memory.cpp` enables decommit when
system and GC page sizes match. Its Unix `MarkPagesUnusedSoft` calls
`madvise(..., MADV_DONTNEED)`; hard discard delegates to that path. Unix
`MarkPagesInUseSoft/Hard` does not explicitly remap discarded pages. CuBit builds
SpiderMonkey against its Linux/musl ABI, and `crate_fixes.py` does not replace
these discard/reuse paths. Native call counts and discarded ranges have not yet
been measured.

This differs from musl mallocng's free-slot advisory path: the actual
`glue.h` defines `USE_MADV_FREE=0`, so that disabled path cannot explain the
browser's retained pages. Whole unused mappings can still be released normally.

Do not implement MADV_DONTNEED by releasing the containing allocation: the GC
expects its virtual addresses to remain usable, and CuBit's aligned GC chunks
are currently obtained with posix_memalign/free. An interior range may share
its containing allocation with allocator metadata or other live contents.
A correct physical-discard implementation needs validated caller-owned ranges,
preservation of neighboring bytes and allocator metadata, safe zero-filled
reuse at the same virtual addresses, grant/alias semantics, concurrent TLB
safety, and correct owned-frame accounting. No executable permission or W^X
change is required. First measure calls/bytes by advice and correlate them
with controlled GC-heavy workloads and existing physical-frame samples; byte
totals alone can repeatedly count the same range and are not reclaimable RAM.

The native six-cycle discard probe completed successfully on 2026-10-02
(`tests/servo/build/perf-tmp/nix-shell.uXbV9E/penny-interaction-n92_9w78/discard-analysis.json`).
The six post-close idle samples were 318877696, 336564224, 336891904,
337088512, 337428480, and 336916480 caller-owned bytes; retiring the original
root page reduced the final idle sample to 281108480 bytes before exit.
585 DONTNEED requests cumulatively described 492040192 bytes, including
repeated ranges. They do not establish that much reclaimable RAM. The later
cycles were approximately stable in this workload, not a general leak proof.
Compiled direct call sites were the two SpiderMonkey GC discard functions
and AWS-LC fork detection; the latter uses advice -1 and 18, not DONTNEED.

This exposed an independent libc contract bug: invalid advice returned success.
The syscall adapter now rejects unsupported advice with EINVAL while retaining
the existing advisory no-ops for NORMAL/RANDOM/SEQUENTIAL/WILLNEED/DONTNEED/FREE.
The actual-dispatch hosted regression and full CuBit-header object compilation
pass. Native runtime validation passed against rebuilt production libc: startup,
HTTP navigation, exact resize pixel restoration, idle, and clean shutdown, with
both AWS-LC unsupported probes rejected and GC hints accepted. Evidence:
`tests/servo/build/perf-tmp/nix-shell.hC7RI1/penny-interaction-a_89cjhg/advice-result.json`. This does not
implement physical-page discard or change the old run's results.

### Native stack attribution (2026-10-02)

A private helper, compiled against the actual musl pthread layout, snapshots
the live thread list under `__tl_lock`, the same lock used by creation and
exit. It allocates/logs nothing under that lock and bounds traversal at 1024.
Hosted tests cover exact totals for 1..1024 nodes, over-limit/broken-list
rejection and lock release. The native page/navigation/resize/clean-close
fixture passed with 39 live entries and 87531520 bytes (83.5 MiB) of
library-owned stack/TLS mappings. Stack bytes were 87068528 and guard bytes
311296. The main loader stack and retired joinable mappings absent from the
list are excluded. Evidence: `tests/servo/build/perf-tmp/nix-shell.tYjG7T/
penny-interaction-dbfjscrg/stack-analysis.json`.

These are mapping sizes, not observed stack high-water marks. CuBit's current
owned-allocation path backs the pages eagerly, making demand-backed stack
mappings a substantial optimization candidate without reducing advertised
stack capacity. No stack-size reduction or lazy-backing change is implemented
by this diagnostic.


### Private demand-backed pthread stack experiment (2026-10-02)

A matched native Penny comparison reduced stable idle caller-owned physical
memory from **255,918,080 bytes (244.1 MiB)** to **169,353,216 bytes (161.5 MiB)**:
**86,564,864 bytes (82.55 MiB), or 33.83% less** in this workload. Both runs had
six identical five-second samples during the final 30-second idle interval.
Navigation, exact resize restoration and clean close passed in both runs;
787,814 nonempty page-interior pixels matched exactly across variants.
This counts caller-owned frames, not RSS or whole-system memory, and excludes
borrowed mappings, page tables and service allocations. It is not a hardware
speed comparison or general proof of leak freedom.

Evidence: `tests/servo/build/penny-demand-compare-fg52m4ub/comparison.json`.
Baseline artifacts: `tests/servo/build/perf-tmp/penny-interaction-7t0c6j96`.
Demand artifacts: `tests/servo/build/perf-tmp/penny-interaction-vc6bgopn`.
Both use identical private kernel, service seeds and application capability
sections. Application sources and musl headers are frozen under
`tests/servo/build/demand-libc-8wm6kn3e`; only the MAP_STACK allocation argument
changes between the two compiled syscall overrides. Both use the same private
libc archive with pthread_create's two internal mmap calls marked MAP_STACK.
Other anonymous allocations remain eager. Installed libc and default Penny
are unchanged.

The private kernel is `.build-workspaces/penny-demand-nq9qtuvx`. Allocation115
arg1=1 creates a demand reservation with one charged metadata frame; arg1=0
retains eager semantics. No executable mapping or capability is added. Sparse
protection/release, explicit fault read/write access and checked user-memory
resolution are integrated there. Native tests cover first-touch zeroing,
read-only/guard/instruction denial and process reclamation; eight rounds of
four synchronized workers over 256 pages with exact frame counts; futex
first-touch reads, thread-exit stores and checked copies; and 4,096-slot
exhaustion, rejection without allocation, hole reuse and complete retirement.
The registry address search was also changed privately to continue scanning
after an overlap instead of restarting for every preceding allocation.

Further native validation passed for the private candidate:

- Six loaded-tab retirement cycles, followed by root-page retirement, 65 live
  tabs in both orientations, four simultaneous windows, window close/reuse,
  and clean browser close/reopen. Each retirement idle interval had six stable
  samples; final root-retirement idle was 172,589,056 owned bytes. Evidence:
  `tests/servo/build/perf-tmp/penny-interaction-_valdtxv/demand-lifecycle-evidence.json`.
- Sixteen ordinary children tested exact resource quotas: metadata allocation
  refusal without charge, release/refund/retry, and over-limit first-touch
  termination with owned-region retirement. Evidence: private workspace
  `tests/owned-demand/quota/evidence.json`.
- Three physical-memory exhaustion/recovery cycles in a 128 MiB VM. Each
  uncapped ordinary child reached an explicit Physical_Memory_Exhausted
  allocation result and was reclaimed; the supervisor then allocated and
  released 16 MiB eagerly and returned to its original owned-frame count.
  Evidence: private workspace `tests/owned-demand/physical-oom/evidence.json`.
  The first runner rejected a numeric enum diagnostic; its artifacts remain
  in `build-before-diagnostic`. The explicit diagnostic rerun passed.

The private candidate also builds libc normally from pinned musl 1.2.6:
a two-hunk upstream patch marks pthread's mmap calls MAP_STACK, and the
CuBit syscall overlay selects demand allocation for that flag. No symbol
wrapping or archive member replacement is needed. Native testing of this
archive passed 32 cycles of four concurrent 8 MiB stacks, touching 64 KiB
per worker, with live owned growth between 256 KiB and 1 MiB and exact final
reclamation (2,125,824 bytes before and after). Evidence: private workspace
`tests/owned-demand/libc/evidence.json`. The private build uses an explicit
`path:` flake reference so Nix can build an untracked snapshot directory.
These build changes remain unpublished pending the process owner's handoff.

These are bounded functional regressions, not proof of general leak freedom
or performance parity. This remains a private integration candidate pending
source coordination and reproducible production integration. It does not
implement MADV_DONTNEED reclamation or change ownership/pinning of eager GPU
and grant-backed allocations.

### Thread-startup race found during demand-stack validation (2026-10-02)

The normal-archive detached-thread regression exposed a clone-wrapper race:
THREAD_CREATE makes the child runnable before the parent-side assembly stores
its thread ID. An immediate-return child can reach musl's thread-list lock
with ID zero, defeating exclusion and permitting premature stack release.
Observed failures included an instruction fetch at zero and a return from
`__wake` on an unmapped stack.

The private clone wrapper now preserves a startup-frame pointer across the
syscall and publishes a flag after writing the child ID. The child waits for
that flag before entering musl. On x86, store ordering ensures the ID is
visible before the flag. A forced parent yield after THREAD_CREATE reproduced
a zero child ID with the old wrapper; the corrected wrapper passed 128
immediate-start/exit cycles with the same yield. The original detached test
also passed 32 cycles with exactly 2,125,824 owned bytes before and after.
Evidence: `.build-workspaces/penny-demand-nq9qtuvx/tests/owned-demand/thread-startup/evidence.json`.
The narrow clone fix and `userspace/libc/tests/thread-startup.c` were
published under the shared build lock after checking both old and new sources
against the tested variants. Installed libc and statically linked applications
still require rebuilding; there is no kernel ABI or manifest change.

The post-handshake Penny relink against the normally built libc passed native
page-pixel, exact resize-restoration, and clean-close checks. The final six
idle samples were all 169,361,408 caller-owned bytes (~161.5 MiB), preserving
the experimental stack savings without syscall wrapping. Evidence:
`tests/servo/build/perf-tmp/penny-interaction-hxgumvzp/normal-libc-evidence.json`.
The rebuilt archive subsequently passed the full 65-tab/four-window lifecycle
suite: both tab layouts, six loaded-tab retirement cycles, root retirement,
window close/reuse, saved-layout reopen and a second clean close. Root-retirement
idle held at 172,171,264 owned bytes for six samples. Evidence:
`tests/servo/build/perf-tmp/penny-interaction-esv3c76m/normal-libc-lifecycle-evidence.json`.
The clone source fix is published; demand-paging integration remains private.

### Native CPU-media dependency probe (2026-10-02)

A minimal static GLib 2.88.3 build, from the pinned Nix source, now links with
CuBit libc and passes a native four-thread/mutex/main-loop timer smoke test:
4,000 synchronized increments and a requested 25 ms timer observed at 28,791
microseconds. GLib's eventfd2 probe receives unsupported and its fallback
succeeds. This is a private compatibility fixture, not video playback or
production deployment. Evidence: private workspace
`tests/media/glib-evidence.json`. A static GStreamer core build is the next
step; codecs, player integration, A/V synchronization and capability-scoped
audio remain unimplemented.

Static GStreamer 1.28.5 core also passes a native ordinary-process pipeline:
64 zero-filled 4 KiB buffers cross a queue bounded to four buffers/16 KiB;
the sink checks every byte, receives EOS, and successfully returns the pipeline
to NULL before releasing it. The child receives no capability grants from the
disposable supervisor. Missing service endpoints and readlink/getppid probes
are nonfatal in this workload; this is not broad isolation evidence. Static
PCRE2 is built with JIT disabled. Exact archive and executable hashes are in
private `tests/media/gstreamer-evidence.json`. This still uses a synthetic
source: codec decode, audio output, browser integration and repeated-pipeline
memory convergence remain unverified.

A repeated native core-pipeline test then passed four warm-up cycles plus
32 measured cycles (2,048 verified buffers), with GStreamer's default worker
cache policy unchanged. After 20-second idle intervals, caller-owned memory
was exactly 15,818,752 bytes both before and after. NULL state was confirmed
before releasing each pipeline. This is bounded synthetic-pipeline evidence,
not general leak freedom. See private `tests/media/gstreamer-lifetime-evidence.json`.
The next private build selects app, video conversion, typefinding and playback
components with ORC disabled; no runtime executable-memory authority is added.
