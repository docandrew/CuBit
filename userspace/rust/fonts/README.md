# CuBit runtime TrueType

The shared Ada toolkit and desktop compositor now rasterize bundled IBM Plex
Sans Regular / Mono Regular through `ab_glyph_rasterizer` and `ttf-parser`, in Rust
userspace. Native builds are `no_std + alloc`, static, and use CuBit's SPARK-core
allocator adapter. The Linux preview uses the same glyph code with the host
allocator. There is no C font engine, font syscall, or separate font service.

## Boundary and rendering

`CuBit.Fonts` is the small Ada interface. Only two bundled faces and two raster
sizes are accepted; there is no caller-supplied font parser input or arbitrary
output pointer. The C calling convention is an ABI, not a C implementation.
The renderer returns a stable read-only glyph pointer after release/acquire
publication. A bounded cache owns 380 cells (two faces x two sizes x 95 ASCII
characters); published cells never move or get evicted. The cache occupies
about 432 KiB of BSS, not an unbounded heap or a payload arena.

The default remains 13-pixel em / 17-pixel line, with 8-pixel monospace cells.
Geometry, hit testing and editor caret positions are unchanged. Grayscale
coverage blends over opaque widget faces or the actual title gradient.
Native title bars use the same Alloy gradient colors as the toolkit. Legacy
8x16 console drawing remains separate; the obsolete generated Plex/Noto UI
tables and their generator have been removed.

Warm lookup is allocation-free and lock-free (an atomic readiness load).
Cold raster work allocates temporarily and is not hard-real-time. Outlines
stream directly from the parser into coverage scratch, without collecting a
heap vector of curves. All 380 cells are regression-compared against `ab_glyph`
(at most one grayscale level of rounding difference).
The native allocator now acquires small-object backing in 1 MiB chunks so a
416-byte font allocation does not demand a fresh 16 MiB arena.

## Build and test

Use Nix; it supplies the exact font files and Cargo vendor tree. Builds remain
`--locked --offline` after Nix supplies dependencies. Normal desktop build
targets and the Linux preview build the correct static archive automatically.

```
nix develop -c make -C kernel run-desktop
nix develop -c make -C kernel ccl-ui-preview
nix develop -c make -C kernel test-ui-fonts test-procmgr-reads
nix develop -c make -C userspace/rust fonts-bench
```

Hosted tests cover all glyphs/sizes, concurrent publication, metrics, ABI layout,
grayscale output, clipping, pitch padding and transparent drawing. The Linux
dialog/editor/theme regressions and native QEMU runs cover toolkit consumers.
These tests do **not** formally verify the rasterizer, parser, unsafe ABI or
allocator pointer/synchronization boundary. The existing metadata proof is
separate and unchanged. See [third-party notices](THIRD_PARTY.md).

One hosted optimized run on this workstation measured 190 cold normal-size
glyphs in 0.668 ms and two million warm lookups in 7.37 ms (~3.7 ns/lookup).
The largest scratch request fell from 4,608 to 544 bytes; scratch is released
after each glyph. Both sizes have a regression ceiling of 4,096 bytes per
allocation to avoid triggering the native large-object arena. This is a
cache microbenchmark, not native repaint time or an input-latency guarantee.

## Native bring-up fixes

The desktop ELF grew beyond a single 16 MiB grant. Procman now reads exact
1 MiB windows directly into the final buffer, refusing partial/error replies
and quarantining it if retirement cannot be confirmed. The development disk
also exhausted its old 64 MiB capacity: `debugfs` left truncated payloads while
returning success. Its new builder sizes from the payload, verifies every file
by SHA-256 readback, and atomically publishes only the complete image.

## Deliberate limits / next work

- Printable ASCII; other Character values map to `?`. No UTF-8 shaping,
  ligatures, bidi, kerning, font chooser or untrusted font files yet.
- The 26-pixel raster is tested groundwork, not connected to per-output DPI.
  SDL's existing integer canvas scaling still scales the whole surface. This
  does not yet solve fractional/mixed-DPI monitor rendering.
- Grayscale outlines, not a TrueType hinting bytecode interpreter or subpixel
  LCD rasterizer. Retain grayscale for rotated displays and compositing.
- Static font copies/cache per process; no cross-process cache or shared font
  service. Large-object allocator backing remains coarse-grained.
- Fatal native Rust panics are logged without allocation, then terminate that
  process. Recoverable OOM inside dependency rasterization remains future work.
