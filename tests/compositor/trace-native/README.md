# Native Desktop trace capture

This fixture runs actual Desktop hooks with metrics on and serial timing off.
It requires input/source/draw/submission/completion events from the authenticated
Desktop publisher, monotonic event IDs and loss counters, and confirmed raw
observer grant retirement. A tiny idle toolkit client uses protected publication;
the existing Mesa window uses legacy attachments and checks Mesa pixels. The
runner also verifies menus, summary metrics, logsvc, and 100%/125% cursor damage.
It waits for visible cursor arrival before recording the scaled reference.

Use the repository's Nix environment. Build native prerequisites normally first:
runtime (including allocator), startup object, fonts, metricsvc, and the CCL
manifest compiler. Hold `coordination/build.lock` while snapshotting a shared
checkout. Build the collector, publication client and timing-off summary observer
into a fresh directory outside the checkout:

```sh
python3 tests/compositor/trace-native/build.py --root "$PWD" --toolchain-root "$PWD" --manifest-compiler "$PWD/userspace/ccl/build/manifest/ccl-manifest" --catalog "$PWD/userspace/ccl/catalogs/native-runtime-services.ccl" --schema "$PWD/userspace/ccl/interfaces/executable-manifest.ccl" /absolute/private/trace-fixtures
```

`result.json` records the three output paths and hashes. Runtime/font/startup
objects are explicitly prebuilt seeds, not represented as rebuilt by this helper.
Build Desktop through `tools/build_desktop_vulkan_compositor.py` with metrics on,
timing off, and a verified combined Mesa bundle whose runtime matches exactly.
The helper keeps all existing bundle/runtime checks. See that tool's `--help`.

Prepare a seed directory containing `metrics.svc` from a native build supporting
raw trace groups and `desktop-metrics-observer.app` from the fixture's summary
subdirectory. The platform seed must contain `cubit_kernel`, `initrd.img`,
`display.svc`, `clock.svc` and `logstore.svc`. Create the trace-enabled private seed:

```sh
python3 tests/compositor/trace-native/prepare-seed.py --platform-seed /absolute/platform-seed --metrics-seed /absolute/metrics-seed --trace-observer /absolute/trace-fixtures/tree/tests/compositor/trace-native/build/desktop-trace-observer.app --publication-client /absolute/trace-fixtures/tree/tests/compositor/trace-native/build/client/trace-publication.app /absolute/private/trace-seed
```

The seed helper modifies only the `desktop.metrics.trace=true` setting inside
`system.ccl`. It rejects an already configured trace seed, checks all other CPIO
entries remain identical, and records hashes. It never edits a production image.
Run the visual/trace fixture with explicit prebuilt dependencies:

```sh
python3 tests/compositor/trace-native/run.py /absolute/linked-desktop /absolute/private/trace-seed/seed /absolute/private/native-run --cursor-motion --scaled-cursor-motion --metrics-seed /absolute/private/trace-seed/metrics --mesa-client /absolute/native-mesa-window.app --observer-build /absolute/native-log-observer-build --platform-root /absolute/frozen-platform
```

The Mesa client needs its verified `fixture.json` and referenced inputs; the log
observer needs its `result.json` and `inputs.json`. The runner records all seeds,
checks exact pixels and completion markers, and terminates its own VM on exit.
Use disjoint private output directories. Its functional QEMU timings are not
supported-hardware performance measurements. The raw collector's loss snapshot
ends at its exit; telemetry is diagnostic and never an ownership fence.

Acceptance: `../trace-wire-evidence/desktop-native188.json`. The first run used
only a legacy Mesa client and correctly failed missing source/draw events. A
subsequent run passed traces but detected a stale scaled-cursor reference; the
arrival oracle retains positive and negative controls. Neither failure was
resolved by weakening event, pixel or grant-retirement requirements.

The packaged builder/seed helper/runner also passed in run194, using freshly
linked fixtures: `../trace-wire-evidence/desktop-packaged194.json`. The capture
reported 4 dropped producer records, zero batch gaps/history loss/rejected rows,
and confirmed observer grant retirement.


## Saving a bounded capture

Pass `--collector archive` to `build.py` to select the file-writing observer.
The default remains `raw`. The archive variant alone requests filesystem
read/write/create authority scoped to `@nvme:0/work/`. It exclusively creates
`@nvme:0/work/desktop-trace.cubittrace`; an existing file is preserved and
causes failure. Use a fresh private test disk for each run.

Prepare the seed with the resulting observer as above. Build the checked hosted
reader using `../trace-archive/README.md`, then add these arguments to `run.py`:

```sh
--archive-mode complete --archive-reader /absolute/archive-build/archive_reader
```

The collector retains at most 256 events using one 4 KiB write page. It checks
positioned-write status and exact byte counts, flush, close and grant retirement
before reporting saved success. File calls run only in the observer process;
Desktop keeps its existing bounded, lossy metrics publication path. This is a
bounded native integration fixture, not an on-device capture application.

The runner performs the normal Desktop/Mesa/menu/cursor checks, stops its own
VM, extracts the capture with `debugfs`, and runs the independent reader.
`NATIVE_PASS` means guest checks passed but archive extraction/validation has
not completed. Only final `PASS` includes the requested file validation.

## Interrupted collector

Build a separate output with `--collector interrupted`, prepare a separate
seed, and run with `--archive-mode interrupted` and the same checked reader.
This mode intentionally exits the collector immediately after its first
acknowledged 4 KiB write, before a footer, flush, close or explicit grant
retirement. The generated `Archive_Test_Control` package makes the injection
an explicit build choice; normal save mode disables it.

The native oracle requires the exit marker, exactly one released filesystem
handle for that PID, no saved-success marker, and continued Desktop visual
checks. After stopping the VM it requires a 4 KiB partial capture and an
`INCOMPLETE` reader result. This tests collector process exit, not power loss,
interruption inside an in-flight filesystem operation, or complete kernel
resource reclamation. Metrics observer restart remains separate pending work.

Paired packaged save/interruption evidence: `archive/evidence.json`. Both runs
use explicit frozen platform/runtime/Mesa seeds and QEMU software rendering.
No hardware latency or sustained refresh-rate claim follows from these tests.
