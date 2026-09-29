# Porting Servo to CuBit

Status: Servo renders real web pages natively on CuBit, over HTTP and
HTTPS (Wikipedia, example.com; headless case `servo`, 2026-09-25). Not
yet: a desktop disk that holds it for manual sessions, an address bar,
persistent storage,
returning memory to the system, a robust TCP (netstack redesign).
Date: 2026-09-25. Current state: [Port status](#port-status).

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
