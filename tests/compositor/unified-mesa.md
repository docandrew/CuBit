# Unified Mesa desktop integration

The runtime Vulkan-dispatch compositor uses the same SPARK software-Mesa renderer
as the standalone Mesa facade. The GPU body is unchanged. Platform readiness
selects the renderer. The emergency CPU renderer remains available after a
known-quiescent Mesa failure. Normal builds now default to this runtime dispatch
and automatically build or reuse a verified combined Mesa dependency. Existing
images need rebuilding to contain it. See `docs/compositor-backends.md` for the
current build controls. The verified wrapper also accepts `--timing on` or
`CUBIT_COMPOSITOR_TIMING=on`; it checks the requested timing identity before
replacing that variant's output. Native timing-on functional evidence is in
`unified-mesa-evidence/timing-native80.json`. Serial diagnostic counters are not
physical latency measurements.

Bootstrap text and fills bypass Mesa until the bootstrap writer has retired and
renderer selection has run. Eight retained source-image imports are recycled in
bounded round-robin order. Replacement releases the old foreign view before
importing another; failed retirement quarantines the context. Pixel allocations
remain owned by callers. Targets and glyph masks are outside this eviction set.

## Evidence and boundaries

`unified-mesa-evidence` records native CuBit/QEMU software execution: successful
Mesa text/client draws, exact 512x384 client pixels, nine frames using two alternating client
buffers, three menu restorations, 32 cursor moves at 100% and 16 at 125% DPI.
The normal run rejects any late CPU fallback. Separate init, draw, and partial
text failure builds passed the same pixel/input workload with explicit recovery
markers. These are functional tests, not hardware, 240 Hz, tear-free scanout, or
physical keypress-to-photon measurements.

The cache instance has 45 SPARK checks (17 flow, 28 proof), zero unproved. The
shared renderer facade proof reports are 148 checks for Mesa and 280 for Vulkan,
zero unproved in each; overlapping reports must not be added together. The main
Desktop integration, C/Mesa implementation, mapped-memory validity, and honesty
of foreign completion/retirement remain outside these proofs. The cache tests
cover 80 replacements, reuse, target/mask preservation, and failed-release
quarantine; the old implementation fails the ninth-source regression.

`--workload-observer` in `build-native-metrics-fixtures.py` selects a distinct
observer that permits explicitly reported, monotonic producer drops. It still
requires authenticated publisher-family tags, exact names/kinds/units, zero
schema rejections/batch gaps, increasing frame completions and input/draw/submit
samples. Loss is a snapshot at observer exit, not full-run sampling completeness.
The original `--transfer-observer` retains its zero-loss check. Twenty hosted
controls exercised the unchanged workload observer with injected query replies;
these test validation, not the kernel's transport authentication.

## Reproduction

Run in the pinned `tests/compositor/vulkan-affine-shell.nix` environment using
stable source/platform snapshots or the shared build lock. Build a verified
combined softpipe/Intel Mesa bundle with `tools/build_native_mesa.py`. Pass its
source, native build and bundle to `tools/build_desktop_vulkan_compositor.py`,
including `--software-mesa-build`, `--metrics on`, and `--input-overlay off`.
The runtime sources/ALI/archive must match the bundle inventory. The builder
records explicit `--software-fault none|init|draw|text-partial` variants; never
install a fault-injected artifact as the normal desktop.

Build metrics fixtures with `--workload-observer`, an explicit matched runtime
platform, and a fresh output directory. The native runner also requires a
matched boot seed, authenticated log observer, and non-cube Mesa window fixture
with its sibling `fixture.json` input/output inventory. Run
`run-native-unified-mesa.py ARTIFACT BOOT_SEED OUTPUT --approve-render
--cursor-motion --scaled-cursor-motion --metrics-seed METRICS_SEED
--observer-build LOG_OBSERVER --platform-root PLATFORM --mesa-client CLIENT`.
Use `run-native-unified-mesa-fault.py` for each explicit fault build. Each output
must be new. Artifact, fixture and input hashes are checked before and after the
run. Kernels/services in these fixtures are explicit prebuilt seeds, not claimed
as rebuilt from the compositor snapshot. All runs use private test disks.

## Full normal-build integration

`unified-mesa-evidence/current-desktop85.json` records a passing 90-second,
four-vCPU KVM `tests/headless/run.sh --test desktop-display` run at 1 GiB guest
RAM. Normal Make targets rebuilt Desktop/Display/runtime/libc and automatic
combined Mesa; the runner rebuilt the kernel and all 15 stage-one services.
Their packed initrd contents were hash-checked. Other stage-two applications
were explicit snapshot seeds. The supplemental checks require actual Mesa
text/imported-surface activation and reject late retained-CPU fallback. The
snapshot predates Graphics' later incremental Intel insertion publication.

For private workspace reproduction, Desktop's isolated build needs TMPDIR
outside its input source tree. The headless QEMU monitor needs a short temporary
path (Unix sockets are limited to 108 bytes). Use `build_development_disk.py`
with the test's complete stage-two payloads and pass that verified disk using
`--disk`; the 8 MiB laptop writable-data image is unsuitable. Prior preboot
socket and undersized-disk failures remain preserved in native83/native84.
The private libc build also used an explicit `path:` flake reference to avoid
Nix treating the nested snapshot as part of its parent's Git tree.

This is native functional integration coverage, not a supported-hardware GPU,
tear-free, 240 Hz, physical-latency or quiet-host performance measurement.

## Current ownership and sparse-transfer proof (2026-10-08)

The selected current-source proof covers `Compositor_Damage`,
`Vulkan_Owned_Targets`, `Desktop_Vulkan_Startup`, and `Desktop_Readback_Output`.
It passes 512 checks: 262 flow and 250 prover, with zero justified or unproved
checks. All 132 discovered Ada source dependencies were byte-matched against
the working tree after the run and again at publication. See
[source identities and boundaries](unified-mesa-evidence/policy-proof129.json)
and the [GNATprove report](unified-mesa-evidence/policy-proof129.out).

The run uses level 2, 30-second prover timeouts and two workers. Counterexample
generation is disabled; no contract or proof obligation is removed. Five
unused-result warnings in cancellation/cleanup remain recorded in the evidence.
Child owners retain refused or uncertain releases, and device closure still
requires context retirement. This selected result supersedes the older
frozen-source 451-check report for these units; overlapping counts are not additive.

This does not prove the main event loop, C/Mesa implementation, validity and
nonaliasing of mapped memory, or honesty of foreign completion/retirement.
Native GPU fault recovery, scanout retirement, tear-free output and physical
latency still require separate integration and hardware evidence.
