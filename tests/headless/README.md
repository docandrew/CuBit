# Headless Integration Tests

Status: current

The headless suite boots CuBit under QEMU without a display and treats the
serial log as the test oracle. It is meant to cover regressions that unit tests
cannot see: boot sequencing, service registration, process spawning, disk image
contents, and IPC paths exercised by the early userspace stack.

## Run

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
- `async-ipc`: boots a test init profile that starts an IPC test server and
  client from the NVMe image.
- `bench-ipc`: boots a benchmark init profile that starts an IPC benchmark
  server and client, then emits compact timing summaries.
- `ccl-vm`: runs the freestanding CCL bytecode VM and source interpreter inside
  CuBit and checks their in-guest self-test markers.
- `capability-security`: boots an authorityless adversarial app and verifies
  that it cannot acquire filesystem, input, process-management, or capability-
  minting authority that was absent from its manifest-derived capability
  space.
- `desktop-display`: boots a test init profile that starts `display.svc` and
  `desktop.svc`, then verifies the display backend status handshake.
- `desktop-doom`: installs the current `doom.elf`, boots `display.svc`,
  `desktop.svc`, and DOOM on primary `virtio-vga`, then uses QEMU's WAV audio
  backend to require a real HDA period interrupt and capture
  compositor/display/audio telemetry.
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

- endpoint capability denial through an empty capability slot;
- one-way async submit using `NO_COMPLETION_TOKEN`, with no reply cap minted
  and no completion delivered;
- saved reply capability single-use semantics;
- deferred `replyCap` replies;
- reverse-order async completions matched by request ID, token, and payload
  identity;
- pending async request pressure: fill the current 16-entry pending request
  limit, verify the next completion-bearing submit fails cleanly, drain all
  completions, then verify a new submit succeeds afterward;
- target-death lifecycle behavior: a completion-bearing request to a dying
  server returns a `COMPLETION_TARGET_DIED` status instead of hanging or
  leaking a pending request slot.

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
