# Wallpaper catalog

The wallpaper package's typed catalog, its build and its readers. Run in the
Nix environment:

```sh
nix develop -c tests/wallpaper-catalog/run.sh           # build checks + hosted tests
nix develop -c systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
  tests/wallpaper-catalog/run.sh --prove                  # SPARK, see below
```

Build checks (`ccl-manifest`):
- the generated decoder in the tree (`userspace/ccl/src/ccl-asset_codecs*`)
  is exactly what `--asset-codecs` generates from
  `CCL.Interfaces.Wallpapers.TYPE_SOURCE`;
- the real source list (`assets/wallpapers/wallpapers.ccl`) builds a
  one-line catalog and the raster plan the QOI build follows;
- source lists the type system or the catalog rules refuse: a duplicate
  name, a missing source file, a name that is not a catalog name, no
  attribution, an empty list, an ill-typed raster size, a path out of the
  source directory;
- three pictures make a three-line catalog, in order.

Hosted tests (`wallpaper_catalog_tests`, real CuAlloc and generated decoders):
- a catalog line is the CCL checker's canonical `Wallpaper`;
- catalogs of 1, 3 and 8 wallpapers, with and without the last line feed and
  fed one byte at a time; malformed lines (not CCL, empty, fields out of
  order, wrong spacing, a missing field, trailing data, over a typed value's
  budget), duplicate names, bad names, files and labels, no attribution, an
  empty catalog and a failed read all load nothing and keep the catalog in
  use;
- the store sized to the catalog: images of 64 x 18, 33 x 41 and 7 x 5 (each
  size from its QOI header, decoded in 1- and 13-byte chunks), a bad QOI
  header refused before any memory is taken, an 8192 x 4096 image refused at
  its header with `CAPACITY` (limit and demand), a second 32 MiB image
  refused beside the first (the ceiling is for all images together), a file
  that cannot be read whole (memory returned);
- choosing: a catalog name, no name, an unknown name and a missing file all
  show a wallpaper (the default where needed), at its decoded size and in the
  requested placement;
- `desktop.appearance.v2`: every scheme x placement x kind x colour encodes
  to exactly the checker's canonical form and decodes back; a stored name the
  catalog no longer holds (the old Cubie selection) still decodes and settles
  to the default wallpaper, keeping scheme and placement; a name no catalog
  could hold and the old three-digit encoding are refused.
- `fallback` (a second run, the store is per process): with the default
  wallpaper unavailable, a missing file shows the flat colour, a solid
  colour shows itself.

The Settings wallpaper list (one row per catalog wallpaper) is tested in
`tests/settings-renderer`; the renderers against every catalog wallpaper of
the real package in `tests/desktop-wallpaper` and `tests/compositor`
(`wallpaper_output`, `wallpaper_strips`, `backdrop_pixels`,
`backdrop_owner_tests` scenario 4: a third image and slot eviction).

Proved (`--prove`): `CuBit.QOI` (level 1, as before, including `Inspect`),
`Desktop_Wallpaper_Store` (level 2: the header-first sizing, the ceiling
arithmetic and the load state machine, over the audited CuAlloc bridge
`Desktop_Wallpaper_Store.Rasters` whose SPARK_Mode Off body is assumed),
`Desktop_Backdrop_Owner` (including `Evict`), `Desktop_Backdrop_Upload` and
`Desktop_GPU_Scene.Backdrop` (level 2) and `CuBit.Appearance` (level 2).
Not proved: the catalog reader and the settings codec (tested above), the
raw-pointer pixel copy and the renderer itself.
