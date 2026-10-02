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
