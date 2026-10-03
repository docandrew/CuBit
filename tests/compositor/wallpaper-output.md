# Direct wallpaper preview sampling

Run hosted checks in Nix, with separate output directories:

```sh
cd kernel
alr exec -- gprbuild -p -P ../tests/compositor/image_sampling.gpr
../tests/compositor/build/image-sampling/image_sampling_tests
alr exec -- gnatprove -P ../tests/compositor/image_sampling.gpr -u compositor_image_sampling.adb --level=2 -j1 --report=all
alr exec -- gprbuild -p -P ../tests/compositor/sampling.gpr
../tests/compositor/build/sampling/sampling_tests
alr exec -- gnatprove -P ../tests/compositor/sampling.gpr -u compositor_sampling.adb --level=2 -j1 --report=all
alr exec -- gprbuild -p -P ../tests/compositor/wallpaper_output.gpr
../tests/compositor/build/wallpaper-output/wallpaper_output_tests
```

`Compositor_Image_Sampling` proves Fill/Fit extent bounds, centered placement,
clamped pixel-centre conversion and in-bounds bilinear source indices. Its
hosted tests exercise 395,307 placement/sample cases and fractional centre
clamping for dimensions 1 through 255. The sampling fixture checks fine-grid
coordinates against a floating reference and preserves the existing coarse
pixel interface across rational scales, rotations, offsets and extremes.

The real `Desktop_Wallpaper.Paint_Output` is linked to synthetic exported
atlases in the writer fixture. It checks 384 style/scale/rotation combinations,
negative origins, four-way damage tiling, leading/trailing guard pixels and row
padding. At unit density it compares every target and guard pixel with the
legacy wallpaper painter. This exercises actual imported source reads and target
pointer writes; it does not replace the painter with a host-side imitation.

The SPARK units do not dereference pointers or allocate storage. The writer's
imported atlas correspondence, target pointer/pitch/allocation authority and
pixel encoding remain trusted integration boundaries. Native validation uses
the Mesa-selected `desktop-dual-output` fixture under the shared build lock;
hosted rotations do not assert that native output rotation is admitted.

No GPU execution, hardware refresh or latency measurement follows from these
checks. The native backend now separates logical workspace admission from pixel
storage and no longer reserves a private scene image; see `workspace.md` for its
allocation-ledger checks and remaining validation scope.

## Physical wallpaper strip cache (2026-10-02)

`Desktop_Wallpaper.Paint` now uses the same proved placement sampler and caches
horizontal samples in strips of at most 64 columns. The cache payload is 1,024
bytes (not a claim about total stack use); no framebuffer or pixel copy is added.
Solid backdrops skip image sampling. Existing integer blend rounding is retained.
Invalid target extents, pitch and damage bounds return before writing pixels.

`wallpaper_strips.gpr` tests 1,728 independent scalar-reference cases, including
widths around strip boundaries, every style/placement, clipped damage, padding
and sentinels. Eight invalid-call cases also preserve the entire destination.
The existing 384 output-painter and 395,307 sampling cases pass. The sampling and
strip proof reports 66 checks, zero unproved or justified checks. This proves
sampling/index/cache equivalence, not pointer authority or all of Paint.

Evidence is in `build/wallpaper-strips-preview-sneodx7t`: `result.json`,
`measurements.json`, and `published-source-verification.json`. The latter records
byte equality of the published sampler, strip policy, painter and test with the
accepted private snapshot. Both native Desktop backends compile, recorded in
`build/wallpaper-strips-native-r1.log`. The new native boot regression remains
pending: its first lock attempt exited 75 before building or booting.

An alternating hosted CPU microbenchmark used 800x600 synthetic assets, two
warmups and twelve timed frames per sample, three samples per variant. Matching
output checksums were required. Median CPU milliseconds per full wallpaper:

| Mode | Scalar | Strip cache | Reduction |
| --- | ---: | ---: | ---: |
| Wallpaper Fill | 6.422 | 6.056 | 5.7% |
| Wallpaper Fit | 2.578 | 2.565 | 0.5% |
| Wallpaper Center | 6.104 | 5.737 | 6.0% |
| Cubie Fill | 6.239 | 5.540 | 11.2% |
| Cubie Fit | 4.811 | 4.169 | 13.3% |
| Cubie Center | 6.313 | 5.375 | 14.9% |

Fit's 0.5% difference is effectively unchanged. These are hosted process CPU
measurements, not quiet-host wall times, NUC frame timings, GPU measurements or
physical input latency. They do not establish a 240 Hz deadline. The benchmark
uses `wallpaper_benchmark.gpr`; the preserved scalar baseline and separate old/new
executables are recorded in the private snapshot.
