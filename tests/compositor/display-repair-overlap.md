# Display repair copies outside new frame damage

`Display.prepareGpuRect` previously copied the entire previous damage rectangle
from the active GPU backing buffer to the inactive one, then copied the new
source rectangle over their overlap. It now reuses
`Compositor_Repaint.Before_Draw` with an empty cursor rectangle to copy exactly
previous damage minus new damage. At most four nonempty repair strips result;
no page, heap allocation, queue or extra GPU submission is added.

The backend transfer rectangle remains the union of old and new damage. All
CPU repair strips and new source pixels are ready before the single GPU request.
The active-buffer index and remembered damage still advance only on confirmed
successful completion. Failed/uncertain operations retain the existing fault
and lifetime behavior; there is no new retry or fallback. The i915 driver,
Mesa/Vulkan stack, GPU protocol and presentation-completion validation are
unchanged.

This eliminates CPU repair copies that would immediately be overwritten. It
does not eliminate the source-to-backend copy or the GPU upload, introduce direct
scanout/imported GPU targets, or establish vblank/physical-photon timing.

## Proven policy and actual adapter boundary

The reused policy has exact arbitrary-point coverage, subset bounds and pairwise
disjointness proofs. With an empty cursor its result covers exactly the old area
outside the upcoming area. Its existing 85-result SPARK run has zero unproved
or justified results; the policy source hashes still match that tested version.
Display's source/target mappings, serialized submission and device ownership
remain boundary assumptions. Display's GPR now includes the compositor policy
source directory and enables Ada 2022, matching the reused policy syntax.

`test-display-repair.py` extracts the actual `prepareGpuRect` and
`copyAndFlipGpuRect` functions, using independent pixel arrays for two outputs,
two backing buffers each, and current sources. Memory copies and GPU IPC are
mocked. Across 2,000 frames it checks exact final target pixels, byte-minimal
repair outside new damage, untouched active and other-output buffers, packed
transfer geometry and output routing. Failed flips must preserve active index,
remembered damage and active pixels. Empty/outside damage, unavailable source,
and firmware backend must perform no copies.

Negative controls omit repair or reintroduce the entire old rectangle copy;
both are rejected. Each variant is force rebuilt (`gprbuild -f`) to avoid
same-second source timestamp reuse. The first negative-control run exposed that
fixture build-cache issue; it was corrected before proceeding. A subsequent
native compilation exposed Display's missing Ada 2022 project flag, also fixed.

Run within Nix from `kernel`:

```sh
alr exec -- python3 ../tests/compositor/test-display-repair.py
```

Hosted production logic passed in session 34759; the force-rebuilt production and
both negative cases pass in the native batch log. Native integration results
are recorded separately and cannot be inferred from hosted pixel memory.


## Native integration

Session 45412 completed successfully. The native cursor oracle checked four
moves with exact scanout restoration, followed by the complete desktop input
regression and final fault scan. Resize checked three enlarge/shrink cycles,
with exact restoration of 48,000 wallpaper pixels; its full runner also passed.
The subsequent Observatory workload completed 39 updates, 20 visible pause/resume
cycles, graph/table restoration, refresh, paging and close. All recorded source,
binary and private-base hashes matched. The prior staged Display was restored
and byte-compared after the tests; the private override selected the new binary.

The existing Graphics counters report:

| Traffic in comparable native workload | Before | After |
|---|---:|---:|
| Source-to-backend bytes | 161,619,104 | 163,481,888 |
| Previous-damage repair bytes | 159,756,320 | 1,999,520 |
| GPU upload-request bytes | 181,210,048 | 183,072,832 |
| Source copy regions | 85 | 86 |
| Repair copy regions | 84 | 6 |

Repair-copy traffic fell by about 98.75%. Total tracked CPU source-plus-repair
traffic fell from 321,375,424 to 165,481,408 bytes (about 48.5%). The after run
has one additional source-copy region; these are comparable work-count captures,
not identical wall-time trials. GPU upload volume remains, as expected. No
physical GPU/scanout, frame-rate or latency improvement is inferred from them.

Evidence: `/tmp/cubit-display-repair-native-v3.log`,
`/tmp/cubit-cursor-display-copy-native.log`,
`/tmp/cubit-resize-display-copy-native.log`, and
`/tmp/cubit-display-repair-evidence/` (manifests, native viewer log/result,
copy-count JSON and screenshot). The baseline log is
`/tmp/cubit-repair-evidence/viewer/serial.log` from the preceding Desktop-only
optimization, so this comparison isolates the Display copy change at source
level. Host scheduling and input timing still differ.


Combined overload session 30572 passed with four priority-3 CPU workers active
for 120 guest seconds each, Desktop priority 4 and collector priority 2. The
viewer made 26 updates and six visible pause/resume cycles, checked graphs/table
restoration, paging and refresh, and closed while all four workers overlapped.
All workers completed with positive work counts; the final fault scan and
input/private-base integrity checks passed. This is functional overload evidence,
not a latency percentile, fairness proof or 240 Hz result. Retained evidence:
`/tmp/cubit-display-repair-evidence/overload/` and
`/tmp/cubit-display-repair-overload.log`.

`check-display-repair-overlap.py` validates complete fault-free native captures,
monotonic unsaturated counters, substantial presentation work, comparable source
and upload volumes (within 10%), and at least a 90% reduction in repair traffic
normalized to source bytes. Session 40924 passed the real before/after captures
and rejected the unchanged-baseline negative control.
`/tmp/cubit-display-copy-counts.log` records that check. No staged binary or
user image is promoted by these tests.
