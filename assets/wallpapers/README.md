# Desktop wallpapers

The current default is the owner's unmodified `wallpaper2.png` (5120x1440).
It is supplied by the project owner; no third-party license or independent
provenance verification is asserted here, so confirm redistribution terms with
the project owner before including this artwork in a public release.
`tools/build_wallpaper_assets.py` validates its size/opacity, reduces it to a
2048x576 raster (Lanczos) and writes it losslessly as `cubes.qoi` in the asset
package `Assets/cubit-wallpapers/1/` on each image's system volume
(docs/assets.md). The original PNG stays unchanged and is not parsed in CuBit;
Desktop decodes the QOI file once with the proved streaming decoder.
Bilinear aspect-fill rendering centers and crops the image to cover the
display, without letterboxing or stretching. Desktop keeps one pre-scaled
copy per output (about 8 MB at 1080p) and repaints damage by row copies.

`cubit-girl-wallpaper-4k-3840x2160.png` is the owner's new Cubie artwork,
with the same provenance/redistribution caveat. Its original remains unchanged;
the build produces a 2048x1152 raster (9 MiB decoded, `cubie.qoi` 3.7 MB on
disk) using the same Lanczos reduction. The desktop uses the same bilinear, aspect-preserving
renderer for both images. Apps → Settings offers **Cubes** and **Cubie**, plus
solid Slate/Ocean backgrounds and Fill/Fit/Center placement. Fill covers the
screen without stretching; Fit can leave bars. Cubes remains the default.

With USB live boot, desktop.svc and the wallpaper package load from the
optical filesystem (`@cd:0/Assets/`) rather than the initrd. The ROM directory remains a separate,
explicitly opted-in local build input, and cartridges are not committed here.
