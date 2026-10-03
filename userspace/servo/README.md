# Penny browser

Penny is CuBit's native browser, powered by Servo. Commit the sources in this
directory, `assets/penny`, browser tests, and the matching native UI changes.
Do not vendor Servo, Cargo's registry, generated libraries, or disk images.

`UPSTREAM_REVISION` records the Servo revision used for the native validation.
Our `patch_servo.py`, `crate_fixes.py`, and `overlay/` contain the port changes;
`native/` contains the CuBit UI and protected-frame bridge. Patches are applied
to the disposable checkout, not maintained by editing that checkout manually.

## Preparing an upstream checkout

From the CuBit repository root, inside `nix develop`:

```sh
mkdir -p userspace/rust/build/servo-work
git clone https://github.com/servo/servo.git userspace/rust/build/servo-work/servo
git -C userspace/rust/build/servo-work/servo checkout --detach "$(cat userspace/servo/UPSTREAM_REVISION)"
export CARGO_HOME="$PWD/userspace/rust/build/servo-work/cargo-home"
cargo fetch --manifest-path userspace/rust/build/servo-work/servo/Cargo.toml
```

These commands are for a new checkout. Do not overwrite an existing development
checkout. All downloaded source and build output stays under the ignored
`userspace/rust/build/` directory. The upstream checkout retains Servo's licenses
and third-party notices; Penny credits Servo in Help → About Penny.

## Building and checking

From the CuBit repository root:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c make -C kernel servo
flock --exclusive --nonblock coordination/build.lock nix develop -c python3 tests/browser-bookmarks/run-native.py
```

The existing `servo` make target builds the libc prerequisites, invokes
`build-cubitshell.sh`, and stages `cubitshell.app`. The script builds the native
Ada chrome and checks the final ELF's bounded secondary-stack linkage. The
internal executable and Config identity remain unchanged by the Penny rename.
See `docs/servo-port.md` and `tests/servo/README.md` for the broader port and
native regression procedures; native tests require the matching CuBit boot
services already built/staged. Initial builds require network access and many
gigabytes of build space. A pristine, empty-cache rebuild has not been repeated
for this staging pass.

Penny currently depends on the in-development protected Desktop publication
API (`Client_Frame_Pair`, `Client_Input_Budget`, `CuBit.Desktop_Protocol.Publication`)
and matching runtime/Desktop implementation. Those shared platform changes must
be committed together with, or before, the Penny commit. A browser-only commit
on the older Desktop ABI is not a standalone buildable release.

The verified snapshot includes bookmarks/folders and favicon persistence,
native menus/settings, tabs/windows, and the copper globe assets. The working-tree spacing pass adds an icon-only navigation toolbar, tabs that
meet the page edge, and separated bookmark status text. Session/tab restoration
remains pending.

## CPU video build

The default media build uses statically registered GStreamer plugins for WebM,
VP8 and VP9, linked with CuBit libc. From the repository root:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c make -C kernel servo
```

`media/` defines the dependencies using the repository-pinned nixpkgs input.
The build wrapper realizes a Nix JSON artifact containing the exact static
archives and pkg-config paths; no machine-specific paths or temporary metadata
files need to be committed. Keep these definitions, the overlay and patcher;
do not vendor the upstream libraries or generated archives.

Native CuBit validation covered VP8/VP9 playback, decoded canvas pixels,
forward/backward seeking with HTTP range requests, and reuse after malformed
media errors. The recovery regression is `tests/servo/media-recovery.html`:
serve `/vp9.webm` as the deterministic 64x48 fixture, `/truncated.webm` as its
first 32 bytes, `/invalid.webm` as plain text with a video/webm content type,
and `/missing.webm` as HTTP 404. `/report?` receives progress through GET requests.
The page checks unsupported AudioContext creation, handles all three errors,
then immediately plays valid VP9 on the same element and checks decoded pixels.

Media initialization disables plugin scanning and registry-cache writes.
The patcher disables temporary download buffering on CuBit. This adds no
capability grants. Audio output, Media Source Extensions and end-to-end YouTube
playback remain unfinished; native tests do not establish leak freedom or
performance parity. Set `CUBIT_SERVO_MEDIA=0` only to build the previous backend for diagnosis.

## Release build policy

`build-cubitshell.sh` builds the native chrome and invokes Cargo with
`build --release`; the final app is stripped before manifest sections are added.
The Servo workspace uses Cargo's standard release optimization (`opt-level=3`,
debug assertions off). Keep the speed-oriented release profile: upstream's
separate `production` profile uses size optimization (`opt-level="s"`) and full
LTO, and is not automatically a faster browser. `servo-cargo.sh` disables
incremental compilation by default.

SpiderMonkey's generated native configuration was audited on 2026-10-02:
`MOZ_OPTIMIZE=1`, `MOZ_OPTIMIZE_FLAGS=-O3`, and `NDEBUG TRIMMED`. SWGL and other
cc-rs dependencies inherit Cargo's release optimization; SWGL supplies its own
shader-specific math flags. Do not apply fast-math globally to browser or TLS
code.

Ada chrome/UI and the runtime use `-O2`; the compiled chrome `.ali` records
confirm these switches. Chrome retains its runtime checks. Do not add blanket
`-gnatp` to application code merely to call a build release. The low-level
runtime and allocator have their own existing check-suppression policy.
CuBit's separately built Rust font library uses its workspace release profile:
`opt-level=2`, `panic="abort"`, and overflow checks enabled. Libc uses `-O2`.
These are optimized builds; intermediate `-g`/Rust `debug=1` symbols support
diagnostics and are removed from the packaged Penny app.

No new optimization flags were needed in this audit. Cross-crate LTO, fewer
codegen units, and switching O2 components to O3 require native correctness,
size, and timing comparisons before adoption. Ambient CFLAGS/CXXFLAGS/RUSTFLAGS
and Cargo profile overrides can change a developer build; record them when
reporting benchmark results. This audit does not establish browser performance
parity or certify every historical cached artifact.


### Loading milestones and fatal diagnostics

The status strip has read-only Request, HTML, Resources and Frame checkboxes,
plus elapsed whole seconds. Request means navigation was issued; HTML means
Servo reported `HeadParsed`; Resources means its document load completed;
Frame means a frame notification arrived after HTML parsing. These are not
sequential blocking phases: frames can precede resource completion. They do
not yet isolate DNS, TCP, TLS or download time, or prove screen presentation.
Browser navigation/reload starts the timer immediately, including connection
wait; repeated engine Started notifications before parsing retain that clock.
The counter stops at document load completion and updates at most once a second.
Indicators are per tab and do not consume pointer or keyboard events. At narrow
widths, labels that do not fit are omitted. Existing error messages take priority.

CuBit fatal aborts now emit bounded raw return addresses directly to the debug
channel before calling the original abort routine. The trace uses fixed stack
storage, avoiding formatted heap allocations and stderr stream locks in the
wrapper. Symbolize against the exact unstripped executable from that build;
this reports a failure and does not recover from it.

Chrome redraws (menus, address editing, status changes) now reuse the current
SWGL page image. Servo paints when a new frame arrives, a tab is selected, or
the viewport size changes. The existing guarded copy and native presentation
remain in use. With `/servo/perf-check`, `PENNY-FRAME` reports whether Servo was
called, wall-clock milliseconds spent in paint, and total paint/presentation
elapsed time. These guest monotonic-clock measurements include waiting and
are not CPU time, hardware GPU timings, or an end-to-end load benchmark.

### Native player retirement

Native media retirement now transfers the Play and signal-adapter ownership
from PlayerInner to the existing backend shutdown worker. The worker retains
a strong Play reference while explicit disposal quits and joins the playback
thread, then releases the native objects. This also handles native GstMessages
that outlive the last Rust callback reference. GstPlay disposal tolerates its
message bus already being cleared when GObject disposes it again at final unref.
No additional shutdown thread is created. Terminal owner loss aborts instead
of destroying the returned native objects on an unsafe callback thread.

The forced lifetime test holds the Rust state callback for 750 ms, removes the
video from script, then holds its native callback message for another 750 ms.
The initial queued-drop implementation failed this test even though all DOM
cycles completed. Explicit worker disposal passed all eight cases, with bus
cleanup preceding every player weak notification. check-player-retirement.py
rejects the unsafe traces and empty evidence. This is a targeted lifetime
regression, not proof that all browser crashes or memory leaks are fixed.


### Parent origins during navigation

A native load/resize run reproduced an assertion in WindowProxy ancestor-origin
creation: a proxy need not have a locally active pipeline when its parent lives
on another script thread. Resolve remote parents by browsing-context identity
through the constellation, then retrieve their actual origin details. Local
documents are queried directly to avoid sending a synchronous request to the
same script thread. Unavailable parent details do not invent an origin; CSP
navigation checks fail closed instead of silently shortening the ancestor chain.

The release build passed 36 resizes with Wikipedia/htmx navigations, reloads,
menu interactions and opening/closing another window. Separate cross-origin
fixtures check ancestorOrigins plus CSP allow/deny behavior; these static cases
also pass on the old build and are not independently a crash reproducer.
See tests/servo/origin-csp for the reusable security fixture. This is targeted
regression evidence, not a guarantee of general crash freedom.
