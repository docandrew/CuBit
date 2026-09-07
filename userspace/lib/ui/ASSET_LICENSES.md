# CuBit UI Asset Provenance

This directory contains generated UI assets used by CuBit userspace apps.

## IBM Plex Bitmap Fonts

`cubit-ui-fonts_ibm_plex_sans_11.ads` and
`cubit-ui-fonts_ibm_plex_mono_11.ads` were generated from IBM Plex Sans
Regular and IBM Plex Mono Regular with `tools/generate_ui_font.py`. They
contain rasterized ASCII glyphs 32..126 at 13 pixels in a 17-pixel line.
CuBit does not parse or load the TTF files at runtime.

Upstream source:

https://github.com/IBM/plex

IBM Plex is licensed under the SIL Open Font License 1.1. The complete
copyright notice and license are in `licenses/IBM_PLEX_OFL.txt`.

## Noto Sans Bitmap Font

`cubit-ui-fonts_noto_sans_11.ads` and
`userspace/services/desktop/desktop_ui_font.ads` were generated from
Noto Sans Regular:

https://github.com/notofonts/latin-greek-cyrillic

The generated Ada packages are bitmap renderings of ASCII glyphs 32..126 at
11 pt. CuBit does not parse or load the TTF at runtime.

Noto Sans is licensed under the SIL Open Font License 1.1.

```text
Copyright 2022 The Noto Project Authors

This Font Software is licensed under the SIL Open Font License, Version 1.1.
```

## Noto Sans Mono Bitmap Font

`cubit-ui-fonts_noto_sans_mono_11.ads` was generated from Noto Sans Mono
Regular with `tools/generate_ui_font.py`. It contains a fixed-width rendering
of ASCII glyphs 32..126 for source editors, disassembly, logs, and other code
surfaces. CuBit does not parse or load the TTF at runtime.

Noto Sans Mono is part of the Noto project and is licensed under the SIL Open
Font License 1.1.

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
