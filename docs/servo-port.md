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
