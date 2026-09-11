# Headless Integration Tests

Status: current

The headless suite boots CuBit under QEMU without a display and treats the
serial log as the test oracle. It is meant to cover regressions that unit tests
cannot see: boot sequencing, service registration, process spawning, disk image
contents, and IPC paths exercised by the early userspace stack.

## Run

For calibrated IPC/audio distributions, queue-depth sweeps and busy-peer
checks, see [performance measurements](../performance/README.md). Use explicit
`--accel kvm` for hardware-accelerated timing; TCG is not a comparable baseline.

From the repository root:

```sh
tests/headless/run.sh --build
```

From `kernel/`:

```sh
make headless-test
```

The capability-policy gate has a smaller, CI-oriented build that does not
compile desktop applications, NetSurf, or DOOM:

```sh
nix develop --command make -C kernel capability-security-image
nix develop --command make -C kernel prove-capability-policy
nix develop --command tests/headless/run.sh \
  --test capability-security \
  --disk kernel/capability_security_disk.img \
  --accel tcg,thread=multi \
  --timeout 30
```

The runner accepts `--disk PATH` so focused tests can use purpose-built disk
images without modifying or rebuilding the normal `nvme_disk.img`.
CI explicitly selects QEMU's multithreaded TCG accelerator, so the gate does
not depend on nested virtualization or access to `/dev/kvm`.

The suite currently includes:

- `boot-shell-nvme`: boots the normal NVMe shell profile.
- `async-ipc`: boots a test server, a departing caller, and a surviving client.
  Build `nix develop -c make -C kernel ipctest-server ipctest-client` first;
  the runner installs those current binaries into its temporary disk image.
- `bench-ipc`: boots a benchmark init profile that starts an IPC benchmark
  server and client, then emits compact timing summaries.
- `ccl-vm`: runs the freestanding CCL bytecode VM and source interpreter inside
  CuBit and checks their in-guest self-test markers.
- `ccl-workbench`: boots the native CCL Workbench and checks its first
  presented frame.
- `ccl-remote`: boots the clock service and listen-scoped control app. For a
  live browser lab use `nix develop -c make -C kernel ccl-remote-lab`, then
  `nix develop -c make -C kernel ccl-web-preview` in another terminal and open
  `http://127.0.0.1:8787/`. The runner checks listener readiness; run
  `nix develop -c node tests/ccl-remote/smoke.mjs` against the running guest for
  actual HTTP/CBOR, evaluator, rejection and request-deadline checks. Host port
  18445 is bound only to loopback. This is plaintext lab access, not production
  remote management. See the Observatory README for limits and authority scope.
- `ccl-workspace`: drives native Open/Save dialogs through QEMU, verifies
  dirty-source protection, selected-file reopening, and overwrite rejection.
  First starts the default live-clock label with F7, checks successful samples
  across an idle timer wake, then stops it with keyboard input during a wait.
  With `--keep-logs`, also retains a `*-live-label.ppm` screenshot beside the
  serial log. The shared periodic runner separately tests fuel exhaustion,
  missing authority, coalescing, and no further calls after Stop.
  Checks two revision files plus chosen `clock.ccl` and `quoted.ccl` names on
  the temporary test disk. The latter must contain the exact quoted string
  entered through QEMU's keyboard, exercising desktop Shift/text composition.
  Also submits expressions in the REPL. Allow 50 seconds for the expanded
  interaction sequence. Requires a base image
  whose `work` folder has no pre-existing CCL revision files.
- `capability-security`: boots an authorityless adversarial app and verifies
  that it cannot acquire filesystem, input, process-management, or capability-
  minting authority that was absent from its manifest-derived capability
  space.
  It also checks actual syscall register preservation on normal, unknown,
  denied, and blocking calls, and completion of a full endpoint-table scan.
- `network-authority`: checks manifest approval and scoped inbound/outbound TCP
  in the real guest. A loopback peer exercises twelve outbound lifetimes and
  four inbound accepts, including fragmented writes, peer half-close, stale
  handles, listener cancellation, and backlog/accept deadlines. Uses local ports
  18443/18444 and needs `--timeout 90` because an idle accept intentionally waits
  30 seconds. See `tests/network-authority/README.md`; this does not test an
  internet-ready or authenticated management endpoint.
- `storage-grants`: exercises generation-tagged acquire/use/return, access and
  range denial, pending revocation, stale-reference rejection, and a writable
  ext2 transfer. Also checks child-directory navigation, stale directory handles,
  malformed lookup/page results, and an independently manifest-scoped client:
  read-only versus read/write/create scopes, sibling-prefix denial, rejection of
  self-grant attempts, and independence from ext2 user/group/world mode bits.
  Rename checks cover collision preservation, nested paths, open-handle
  continuity, and rejection of unsupported moves/selectors. Hosted
  `make -C kernel test-filesystem-policy prove-filesystem-policy` also exercises
  directory preparation and injected write/restoration failures, and proves
  the bounded preparation helper. Rename is not yet a crash-safe replacement.
- `audio-grants`: verifies that the mixer acquires HDA's isolated PCM-period
  grant through its endpoint capability.
- `devices`: boots the read-only hardware inspector and checks its inventory
  and native window.
- `files`: boots the native filesystem browser, drags its first shared-table
  column divider and sends a wheel notch through QEMU's real PS/2 input path,
  then verifies the final width, viewport movement, and post-release keyboard
  liveness. It also enters `lost+found/nested`, refreshes in that folder, and
  navigates Back through retained directory handles.
- `desktop-display`: boots a test init profile that starts `display.svc` and
  `desktop.svc`, verifies the display backend status handshake, and injects
  QEMU i8042 keyboard and pointer input through the real PS/2 driver path.
  It opens the internal console through Apps, drags its caption to y=0, then
  requires a stationary triple-click to maximize exactly once and a subsequent
  double-click to restore exactly once. Both normal and maximized captions
  must consume the gesture. Build the current `desktop` and `shell` first.
- `input-stream`: publishes authenticated, sequenced keyboard and relative-
  pointer reports through the kernel event lane, forces one explicit recovery
  boundary, and checks that motion over the CCL Workbench editor does not
  regress into one full client-surface presentation per report.
- `desktop-protocol`: runs an unprivileged native adversary against the real
  desktop service: malformed create/present requests, foreign-surface denial,
  clipping, table exhaustion and recovery. Also tests generation-checked buffer
  attachment, 140 balanced replacements, short/stale grants, deferred revocation,
  release on destroy, and release after owner exit followed by an injected repaint.
  Geometry/control cases cover checked resize and limits, all cursor styles,
  foreign/missing targets, malformed headers/reserved payload, and contradictory
  bounds. Rejected limits preserve prior bounds; malformed destruction leaves
  the surface usable, and repeated valid destruction returns `Bad_Object`.
  Title cases cover all supported lengths, empty captions, ownership, malformed
  headers, oversized lengths and nonzero padding after the declared text.
  Session cases cover exact handshake revision, information queries, malformed
  goodbye preserving surfaces/acquisitions, caller-scoped cleanup and repetition.
  Input cases require rejection before queue mutation, foreign/missing-target
  errors, repeated finite waits that cannot return before their deadlines,
  saved-reply channel reuse, and configure delivery after timeout.
  An async wait followed by a malformed poll and surface destruction must
  produce three correctly correlated, nonduplicate completions; destruction
  resolves the waiter before its acknowledgement and permits channel reuse.
  Build `make -C kernel desktop-check desktop` inside the Nix shell first.
  See [the protocol specification](../../docs/desktop-protocol.md)
  for the separate portable SPARK proof and hosted codec tests.
- `display-grants` / `display-grants-virtio-vga`: dedicated display endpoint
  adversary, without direct framebuffer authority or a concurrent desktop
  session. Exercises lease admission, canonical requests, 140 balanced
  same-reference replacements, byte-range rejection, pinned revocation,
  replacement/release cleanup, and stale generation rejection after slot reuse.
  Runs against the firmware framebuffer or primary virtio-GPU respectively.
  Build `make -C kernel display display-check` inside Nix first; the runner
  installs current test/service binaries into its temporary disk.
  See [display buffer lifetimes](../../docs/display-buffer-lifetimes.md).
- `desktop-doom`: installs the current `doom.elf`, boots `display.svc`,
  `clock.svc`, and `desktop.svc` on primary `virtio-vga`, then launches DOOM
  through Apps. Uses QEMU's WAV audio backend to require a real HDA period
  interrupt and capture compositor/display/audio telemetry. Framebuffer checks
  require game pixels and a responsive keyboard-opened Apps menu after launch.
  `CUBIT_DOOM_MULTIAPP=1` additionally opens/closes Workbench and NetSurf first
  (allow `--timeout 60` and provide current app images). This covers the stale
  client poll flood that previously starved queued DOOM frames and input;
  see [IPC receive fairness](../../docs/ipc-receive-fairness.md).
- `virtio-gpu`: boots with QEMU's `virtio-gpu-pci` device and verifies the
  modern virtio-gpu command path reaches scanout presentation.
- `virtio-vga-primary`: boots with primary QEMU `virtio-vga` under
  `-display none` and verifies that `display.svc` selects the GPU backend.

`boot-shell-nvme` waits for these milestones:

- `devmgr` has finished stage-1 service startup.
- `procmgr` is receiving spawn requests.
- the shell has started with `@nvme:0/` as its working directory.
- PS/2 input has registered a consumer.

The runner also fails the test if the serial log contains obvious fatal
signatures such as panics, assertion failures, triple faults, general
protection faults, or deadlock reports.

`desktop-display` waits for these milestones:

- `display.svc` detected that the separate VirtIO-GPU device is not the
  primary visible adapter and kept the `linear-fb` backend.
- `desktop.svc` activated its integrated shell and attached its grant-backed
  compositor buffer.
- `desktop.svc` queried `OP_DISPLAY_GET_STATUS` and saw backend `1`,
  capability mask `3` (`copy-present | vblank-wait`).
- the regular shell still starts with `@nvme:0/` as its working directory.
- QEMU-injected PS/2 reports cross the hardware IRQ doorbell, `ps2.drv`, the
  publication-capability check, typed source decoding, and desktop dispatch
  with decoded key transitions, relative cursor movement away from its known
  starting coordinate, a complete left-button press/release pair, and zero
  rejected sources.

The runner rebuilds every stage-1 service copied into `initrd.img`. This is a
security property as well as build hygiene: a new publisher must never run
beside an old policy service that did not grant the authority required by the
new protocol.

`input-stream` uses a test-only publisher with separate role-scoped keyboard
and pointer publication authorities. It requires one intentional source gap,
zero rejected normalized reports, a live Workbench frame, and at most twenty
client surface-present requests while 128 paced motion reports cross the rich
editor. It also bounds input requests so an isolated wake cannot regress into
both a successful wait and an immediately-following empty poll. Compositor-owned
software-cursor presents are not counted as client surface submissions.

`virtio-gpu` waits for these milestones:

- `devmgr` discovers the virtio-gpu PCI function.
- `devmgr` parses the modern virtio PCI transport and grants MMIO/DMA/IRQ
  authority.
- `virtio-gpu.drv` configures the control virtqueue.
- the driver creates, attaches, transfers, and flushes a scanout test frame.

`virtio-vga-primary` waits for the same driver milestones, then verifies that
the primary scanout is `1024x768` and `display.svc` reports the VirtIO-GPU
backend. This is the safe headless probe for the experimental
`make run-virtio-vga` path.

## Design

Each headless test should be deterministic from the host side:

- boot one explicit GRUB profile;
- write serial output to a test-owned file;
- run under a bounded timeout;
- require positive success markers;
- reject known fatal markers.

Guest-side regression apps should print one stable `TEST: PASS <name>` marker
only after all assertions have completed. That keeps the host runner simple
while still allowing richer in-guest tests for IPC, capabilities, filesystems,
and service behavior.

The `async-ipc` test follows that pattern. It covers:

- Queued one-way request progress under hot synchronous polling through all
  four request/mixed receive variants, without minting reply authority for the
  one-way message.

- endpoint capability denial through an empty capability slot;
- one-way async submit using `NO_COMPLETION_TOKEN`, with no reply cap minted
  and no completion delivered;
- saved reply capability single-use semantics;
- the first two async requests of a fresh process must remain distinguishable
  (the departing caller's barrier must not complete its held request's token);
- caller death with an outstanding saved reply: delivery fails, a second use
  fails, and the same slot can save and complete a surviving caller's request;
- a reply attempt through an endpoint capability fails without destroying that
  endpoint (subsequent calls still succeed);
- deferred `replyCap` replies;
- reverse-order async completions matched by request ID, token, and payload
  identity;
- pending async request pressure: fill the current 16-entry pending request
  limit, verify the next completion-bearing submit fails cleanly, drain all
  completions, then verify a new submit succeeds afterward;
- target-death lifecycle behavior: a completion-bearing request to a dying
  server returns a `COMPLETION_TARGET_DIED` status instead of hanging or
  leaking a pending request slot.

The departing fixture shares the client's manifest and test-only endpoint scope.
Its two submissions use the same async lane; the barrier acknowledgement means
the held reply has been saved. A test-only 50 ms scheduling allowance precedes
retirement; elapsed time alone is not a pass condition. Delivery rejection and
successful same-slot reuse are required. This is an integration regression,
not exhaustive SMP interleaving verification. See
[IPC request lifetimes](../../docs/ipc-request-lifetimes.md) for the sequential
proofs and the lock boundary that connects them to kernel IPC.

## Benchmarks

Benchmarks should be added early, but used as regression smoke tests rather
than final performance claims. At this stage, prefer serial-reported timings
for stable guest scenarios such as boot-to-shell, async round trips, and queue
pressure recovery. The useful signal is large regressions or timeouts; exact
latency targets should wait until the IPC object model and scheduler behavior
settle.

Run the IPC benchmark directly with:

```sh
tests/headless/run.sh --test bench-ipc --timeout 30 --keep-logs
```

The guest prints stable summary lines:

```text
BENCH: ipc sync count=2000 total_ms=<n> avg_us=<n>
BENCH: ipc async submitted=512 completed=512 total_ms=<n> avg_us=<n>
BENCH: PASS ipc
TRACE: summary begin
TRACE: event=syscall_enter count=<n>
TRACE: event=schedule_run count=<n>
TRACE: event=schedule_stop count=<n>
TRACE: hist=syscall_tsc le_tsc=<n> count=<n>
TRACE: hist=run_tsc le_tsc=<n> count=<n>
TRACE: hist=ready_latency_tsc le_tsc=<n> count=<n>
TRACE: total=<n>
TRACE: summary end
```

The synchronous result measures `capCall` round trips. The async result keeps
up to 16 requests in flight and measures sustained submit/completion throughput
over 512 requests. The trace summary is emitted after the measured loops, so
serial output does not dominate the benchmark. The current trace view is still
coarse, but it gives immediate syscall counts, scheduler-transition counts, and
raw-TSC histograms for syscall body time, process run duration, and ready-to-run
latency. These are diagnostic buckets, not portable performance claims.

Run the desktop DOOM telemetry profile with:

```sh
tests/headless/run.sh --test desktop-doom --timeout 45 --keep-logs
```

Useful lines include `desktop: stats`, `display: stats`, and `mixer: stats`.
For graphics work, compare compositor `frames`, `fast`, `present_req`,
`input_req`, `draw_ms`, and `submit_ms` against display `presents` and
`copy_ms`. The profile is deliberately headless, so treat the numbers as
relative regression signals for QEMU/TCG rather than final hardware claims.
