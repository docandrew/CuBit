# Retained writer repair before a guaranteed redraw

Desktop previously repainted each stale region of its acquired output writer,
then repainted pending frame damage. Repeated client updates therefore rebuilt
wallpaper/window scene pixels immediately before overwriting them with the
latest client buffer. The new `Compositor_Repaint.Before_Draw` policy removes
that overlap from the first pass, except pixels needed for the current cursor's
clean saved underlay.

For each stale rectangle, the policy emits at most five fixed rectangles: top,
bottom, left and right outside the intersection with the upcoming redraw, plus
the intersection that must be repaired for the cursor. Empty entries are ignored.
The regions are pairwise disjoint and contained in the original stale rectangle.
No pixel queue, buffer allocation, foreign call or additional frame is introduced.
With the existing eight damage regions, the pass performs at most 40 clipped
scene draws. Region splitting can increase traversal overhead; CPU cost still
needs workload measurements rather than inference from pixel counts alone.

## Proof and caller boundary

SPARK proves bounds, pairwise disjointness and exact point coverage. The ghost
`Coverage_Lemma` uses arbitrary coordinates: repaired pixels are exactly those
in the stale rectangle that are either outside the guaranteed redraw or inside
the current cursor. This gives a universal geometric property without executing
an enormous coordinate quantifier at runtime. Session 33322 produced 85 SPARK
analysis results (47 prover checks, 38 flow results), zero unproved or justified.

The legacy Desktop adapter must finish the promised redraw before submitting
the writer. `repairDirectWriter` runs immediately before `flushFrame`, with no
input drain or presentation between them. It elides only ordinary pending frame
work; drag/split redraw paths and cursor-only updates retain full repair. The
fast client path now requires a nonempty valid BGRA source whose logical extent
covers the client. Smaller or absent sources take the complete scene/background
path. This prevents stale pixels from surviving a partial source blit.

Cursor footprint invalidation, writer authorization, grant lifetime and final
presentation are unchanged. The existing acquired writer is the only target
modified. Empty cursor footprints and touching edges are valid cases. The
surrounding legacy Desktop event loop is not newly proved; native pixel tests
and the actual-helper test cover that integration. Mesa's output-density path
is not changed by this direct software-writer optimization.

## Hosted evidence

Within Nix, from `kernel`:

```sh
alr exec -- gprbuild -q -p -P ../tests/compositor/repaint.gpr
../tests/compositor/build/repaint/repaint_tests
alr exec -- gnatprove -P ../tests/compositor/repaint.gpr -u compositor_repaint.adb --level=2 --report=all --checks-as-errors=on -j1
alr exec -- python3 ../tests/compositor/test-cursor-repair.py
```

The policy suite checks 8,192 independent coverage grids and extreme coordinate
edges. A separate poisoned-buffer/three-target model checks 600 exact frames,
including cursor save/draw/restore and reuse. It performs 25,168 repair pixels
versus 369,248 for repairing the same stale-region lists without subtraction
(93.2% less repair work in that model). The 600 dirty-grid checks, 800 earlier
exact deferred frames and 200 idle gaps also pass.

Session 78360 tests the actual extracted Desktop repair, flush and cursor
restore routines with independent scene, target and scanout images. It passes
1,000 frames mixing imminent redraw, sparse updates, cursor-only work and drag
admission. Removing old-cursor display damage or retained-target invalidation
still makes the tests fail at frames 1 and 2 respectively. Logs:
`/tmp/cubit-repair-split-host.log`, `/tmp/cubit-repair-actual-host.log`.

The initial whole-rectangle-only implementation preserved pixels but failed
its intended reduction threshold because cursor damage enlarged stale regions.
It was replaced by exact bounded subtraction before native verification.
Native results are recorded separately; these are not hardware latency figures.


## Native integration and repair-work comparison

Session 56545 completed with exit 0. Both metrics-on and metrics-off Desktop
variants compiled. The native cursor regression passed four moves with exact
scanout restoration. Resize passed three enlarge/shrink cycles and exact
48,000-pixel wallpaper restoration. Both complete desktop-display runs passed
their normal input checks and final fault scans. The Observatory workload then
passed 39 six-row updates, 20 visible pause/resume cycles, graph/table pixel
restoration, paging, refresh and close. Compositor source/binary hashes and the
private base image hash were unchanged; no user disk was used.

The fixed 760×560 client redraw workload provides a work-count comparison:

| Selected intervals | Before | After |
|---|---:|---:|
| Complete fast-redraw intervals | 36 | 36 |
| Frames in those intervals | 78 | 77 |
| Client pixels per frame | 425,600 | 425,600 |
| Total repair pixels | 32,817,008 | 40,964 |
| Repair pixels per frame | 420,730.87 | 532 |

This is 99.87% less repair area per frame in this workload. The remaining area
protects the cursor. These are reported repair-area counts, not total memory
writes or a hardware speedup. The workloads have different scheduling, and
TCG clock/percentile results include startup; no latency improvement or FPS
claim is derived from this comparison.

`check-repair-overlap.py` requires complete fault-free native viewer logs, at
least 20 fast frames in each, the same client area per frame, and at least a
90% reduction in repair area per frame. Session 62790 passed against the retained
before/after logs. `test-repair-fast-source.py` session 92087 separately passed
15 cases using the actual fast-redraw function: incomplete physical/logical
sources, bad pitch/format/address, zero dimensions, density scaling, occlusion,
and popup/background readiness. These complement the actual repair pixel test.

Evidence: `/tmp/cubit-repair-native.log`,
`/tmp/cubit-cursor-repair-split-native.log`,
`/tmp/cubit-resize-repair-split-native.log`,
`/tmp/cubit-repair-fast-source.log`, `/tmp/cubit-repair-overlap-counts.log`,
`/tmp/cubit-repair-evidence/viewer/` and `comparison.json`.
The baseline executable is `/tmp/cubit-repair-baseline-desktop.svc` with its hash
in `/tmp/cubit-repair-baseline.sha256`. The new executable is
`userspace/services/desktop/build-metrics/desktop.svc`, bound by the native input
manifest. Neither was promoted to the staged Desktop by this work.


Native overload session 17271 also completed with exit 0. Four priority-3 CPU
workers ran for 120 guest seconds each alongside Desktop priority 4, collector
priority 2 and Observatory priority 3. The viewer completed 26 updates, six
visible pause/resume cycles, positive graph bars, graph/table restoration,
paging, refresh and close. The serial-order oracle requires at least ten updates,
six pauses and close while all four workers overlap. All workers finished with
positive work counts, and final faults, source/input hashes and private base
integrity checks passed. Evidence: `/tmp/cubit-repair-overload.log` and
`/tmp/cubit-repair-evidence/overload/`. This is functional overload evidence,
not a scheduler fairness, worst-case execution time, or 240 Hz hardware claim.

Counter-check negative control 67428 passed: comparing the baseline log to
itself is rejected. `/tmp/cubit-repair-overlap-negative.log`.
