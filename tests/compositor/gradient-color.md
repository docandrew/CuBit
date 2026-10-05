# Equal-color gradient bands

Desktop Settings and `Vulkan_Scene.Append_Gradient` use
`Compositor_Gradient.Color_Run_Last` to merge adjacent equal final RGB colors.
The helper walks at most 256 existing blend-weight groups and returns a
contiguous span. Its SPARK contract proves that every row in the returned span
has exactly the starting row's color. Original gradient origin, integer blend
rounding, single-row alpha behavior, clipping and output transforms are retained.
There are no pixel buffers, image copies, new foreign interfaces or allocations.

For a 2,160-row gradient, the deterministic tests establish:

| Endpoints | Previous weight bands | Color bands |
| --- | ---: | ---: |
| Black to black | 256 | 1 |
| `#202020` to `#303030` | 256 | 17 |
| Black to white | 256 | 256 |

These are command-count results, not measured frame-rate or latency gains.
Planning does compare neighboring colors; it remains bounded independently of
pixel height. The software fallback and Vulkan scene use the same helper.
Vulkan's fixed 512-entry scene and 4,096-draw budget are unchanged. Tests verify
that flat and subtle gradients fit with only one and 17 scene slots remaining,
while genuinely insufficient capacity still rejects the entire snapshot.

Run the independent scalar pixel oracle and proof in Nix:

```sh
nix develop -c bash -c '
  set -e
  gprbuild -p -P tests/compositor/gradient_color.gpr
  tests/compositor/build/gradient-color/gradient_color_tests
  cd kernel
  alr exec -- gnatprove -P ../tests/compositor/gradient_color.gpr \
    -u compositor_gradient.adb --mode=all --level=2 --report=all \
    --checks-as-errors=on -j1
'
```

On 2026-10-02, 270 clipped cases and 117,570 exact pixels passed, including
ascending, descending, mixed-channel, constant and single-channel changes.
Near-`Natural'Last` checks use short spans so runtime quantified assertions
remain bounded; the proof covers the full supported range. The focused report
contains 60 results (eight flow, 52 prover), zero unproved or justified checks.
The existing 16,777,216 channel cases and row/band tests through height 4,096
also pass.

The scene mock tests and combined proof report pass (539 results including
unchanged dependent units). Real hosted Mesa Vulkan passes 712 submissions and
546,816 output-pixel checks, including 168 scaled/rotated gradient scenes and
22,832 exact gradient pixels, with zero validation errors. This uses hosted
software Vulkan and simulated display retirement, not CuBit GPU scanout.

The production Mesa Desktop compiles and links. Its native four-CPU TCG/1 GiB
regression passed all four suites: primary-output changes, 125%/150% scaling,
arrangements and exact cursor/window restoration. The independent Settings
scanout oracle checked 940 pixels across all 20 gradient rows exactly. The
fixture used the established two-second functional settling allowance; its VM
was closed after all four suites passed, before the 300-second deadline. This
was a functional run, not a soak or latency benchmark.

An earlier invocation omitted the extended DPI flags and used the default
0.8-second settling allowance. It captured maximization before completion.
The same VM later changed all 49,700 checked secondary pixels as expected;
that evidence was saved before closing the failed run and rerunning with the
established Mesa profile. No production or observer code changed for the rerun.

Evidence is retained in `build/gradient-color-evidence/`, including source/image
hashes, native logs, proof reports and `primary-125.png` (the actual primary
output at 125%, not an image of an unscaled settings panel for another output).
No physical 240 Hz or keypress-to-photon result follows from these tests.
