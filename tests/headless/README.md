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

The suite currently includes:

- `boot-shell-nvme`: boots the normal NVMe shell profile.
- `async-ipc`: boots a test init profile that starts an IPC test server and
  client from the NVMe image.

`boot-shell-nvme` waits for these milestones:

- `devmgr` has finished stage-1 service startup.
- `procmgr` is receiving spawn requests.
- the shell has started with `@nvme:0/` as its working directory.
- PS/2 input has registered a consumer.

The runner also fails the test if the serial log contains obvious fatal
signatures such as panics, assertion failures, triple faults, general
protection faults, or deadlock reports.

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
