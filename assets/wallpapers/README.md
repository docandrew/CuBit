# Alloy desktop wallpaper

`cubit-alloy.png` is the unmodified 1024x768 image supplied by the project
owner as `cubit-alloy-cybersecurity-anime-wallpaper-v3-1024x768.png` on
2026-09-12. It is not a Bluecurve or IBM asset. No third-party license or
independent provenance verification is asserted here; confirm redistribution
terms with the project owner before including this artwork in a public release.

The current default is the owner's unmodified `wallpaper2.png` (5120x1440).
The same provenance/redistribution caveat above applies to it.
`tools/prepare_wallpaper.py` validates its size/opacity and generates a bounded
2048x576 read-only BGRA raster linked into `desktop.svc`. The original PNG stays
unchanged and is not parsed in CuBit. Bilinear aspect-fill rendering centers and
crops the image to cover the display, without letterboxing or stretching.
It touches only the compositor's clipped damage rectangle; cursor overlays and
the retained window-drag layer continue to use existing cached pixels. There is
no separate full-screen wallpaper allocation. Exposed-background paints and
drag-layer reconstruction do resample the image; this is not a claim of zero
additional painting cost.

`cubit-girl-wallpaper-4k-3840x2160.png` is the owner's new Cubie artwork,
with the same provenance/redistribution caveat. Its original remains unchanged;
the build produces a 2048x1152 immutable BGRA raster (9 MiB) using the same
Lanczos reduction. The desktop uses the same bilinear, aspect-preserving
renderer for both images. Apps → Settings offers **Cubes** and **Cubie**, plus
solid Slate/Ocean backgrounds and Fill/Fit/Center placement. Fill covers the
screen without stretching; Fit can leave bars. Cubes remains the default.

With USB live boot, desktop.svc (including this asset) loads from the optical
filesystem rather than the initrd. The ROM directory remains a separate,
explicitly opted-in local build input, and cartridges are not committed here.
