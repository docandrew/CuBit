# Desktop wallpapers

`wallpapers.ccl` is the one place a wallpaper is declared: a typed CCL
`Wallpaper_Sources` (`CCL.Interfaces.Wallpapers`) listing each picture's
stable name, the label Settings shows, its source PNG here, the raster the
build reduces it to, and its attribution. The first entry is the default.
Adding or removing a picture is an edit to that list (and the PNG); no
desktop code, Makefile rule or image manifest names a picture.

`tools/build_wallpaper_assets.py` (`make -C kernel wallpaper-assets`) builds
the package `Assets/cubit-wallpapers/2/` on each image's system volume:
`ccl-manifest --wallpaper-catalog` checks the list with the CCL type system
and writes `catalog.ccl` (one canonical `Wallpaper` per line), and each
source is validated as opaque, reduced with Lanczos to its declared raster
and written losslessly as `<name>.qoi`. The originals here stay unchanged
and are never parsed in CuBit. Desktop reads the catalog at startup, takes
each image's size from its QOI header, decodes it once into CuAlloc memory
with the proved streaming decoder (all wallpapers together within a named
decoded-memory ceiling) and keeps one pre-scaled copy per output. Settings
lists every catalog wallpaper plus the solid Slate and Ocean backgrounds,
with Fill/Fit/Center placement.

The package ships one wallpaper, **Cubes** (`cubes`): the project owner's
unmodified `wallpaper2.png` (5120x1440), reduced to a 2048x576 raster.
It is not a Bluecurve or IBM asset. No third-party license or independent
provenance verification is asserted here; confirm redistribution terms with
the project owner before including this artwork in a public release. Its
catalog entry carries the same note.

With USB live boot, desktop.svc and the wallpaper package load from the
optical filesystem (`@cd:0/Assets/`) rather than the initrd. The ROM
directory remains a separate, explicitly opted-in local build input, and
cartridges are not committed here.
