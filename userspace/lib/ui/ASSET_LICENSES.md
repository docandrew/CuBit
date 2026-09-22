# CuBit UI Asset Provenance

This directory contains generated UI assets used by CuBit userspace apps.

## IBM Plex TrueType Fonts

The shared toolkit and native desktop use unmodified IBM Plex Sans Regular
and IBM Plex Mono Regular TrueType files from Nix's pinned `ibm-plex` package.
The files are embedded into `userspace/rust/fonts` and rasterized at runtime
by pure-Rust `ttf-parser` and `ab_glyph_rasterizer`. The default remains a 13-pixel em in a
17-pixel line; the editor uses an 8-pixel monospace cell. Grayscale coverage
is blended by Ada against the actual background. The retired generated
IBM Plex/Noto tables are no longer shipped.

Upstream source:

https://github.com/IBM/plex

IBM Plex is licensed under the SIL Open Font License 1.1. The complete
copyright notice and license are in `licenses/IBM_PLEX_OFL.txt`.

Rust component versions and checksums are pinned in `userspace/rust/Cargo.lock`.
See `userspace/rust/fonts/THIRD_PARTY.md` and its `licenses/` directory for
the rasterizer, TrueType parser, and math implementation notices.

## Bluecurve Icon Atlas

`userspace/services/desktop/desktop_icons.ads` was generated from 24x24 PNG
icons from the Bluecurve repository:

https://github.com/neeeeow/Bluecurve

The generated atlas currently includes the Start menu, CuBASIC Console, legacy
UI Lab and Security Center glyphs, DOOM, Files, and Power icons. The source
repository declares
GPL-3.0, and Fedora packages the Bluecurve icon theme as GPL-2.0-or-later.

`userspace/services/desktop/desktop_window_icons.ads` was generated from the
Bluecurve Metacity window control PNGs in `themes/Bluecurve/metacity-1`. The
generated atlas currently includes Close, Minimize, Maximize, Restore, and Menu
icons.

## Bluecurve Toolbar Atlas

`cubit-ui-widgets-bluecurve.ads` is generated from the unmodified 16x16 SVGs
vendored in `assets/bluecurve`, from `icons/icon-set/Bluecurve/16x16` at
`neeeeow/Bluecurve` commit `013ba225e78d9767b274ac6f16a67cb19f0673c6`.
These are rasterized at their original size with librsvg, with no runtime
SVG loader. The upstream GPL-3.0 license is included in
`licenses/BLUECURVE_GPL-3.0.txt`.

The shared toolbar uses `stock-open`, `stock-save`, `stock-execute` (interpret),
`icon-development` (compile), `stock-media-play`, `stock-media-pause`,
`stock-media-stop`, `stock-go-down` (step into), and `stock-redo` (step over).
All are from `actions/`, except `icon-development`, from `apps/`.
The step icons are stock directional artwork, not upstream debugger-specific
icons. Existing tooltips and shortcuts retain their debugger meaning.

Regenerate with:

```sh
nix develop -c sh -c 'python3 tools/generate_toolbar_atlas.py > userspace/lib/ui/cubit-ui-widgets-bluecurve.ads'
```

The renderer preserves transparency, centers icons within the existing button
geometry, shifts pressed icons by one pixel, and desaturates/fades disabled
icons. This applies to native CuBit and the Linux preview.

## Bluecurve Cursor Atlas

`userspace/services/desktop/desktop_cursors.ads` was generated from the nominal
24-pixel Xcursor images in `icons/icon-set/Bluecurve/cursors` at Bluecurve
commit `013ba225e78d9767b274ac6f16a67cb19f0673c6`. It includes `left_ptr`,
`xterm`, `sb_h_double_arrow`, `sb_v_double_arrow`, and
`bottom_right_corner`. The original dimensions, hotspots, antialiasing, and
premultiplied ARGB pixels are retained; CuBit does not parse Xcursor files at
runtime.

Regenerate the atlas with:

```sh
python3 tools/generate_cursor_atlas.py \
  --source-dir /path/to/Bluecurve/icons/icon-set/Bluecurve/cursors \
  --output userspace/services/desktop/desktop_cursors.ads \
  --source-label 'neeeeow/Bluecurve@013ba225 icons/icon-set/Bluecurve/cursors'
```

The source repository declares GPL-3.0. Fedora distributes the corresponding
Bluecurve cursor theme as GPL-2.0-or-later.
