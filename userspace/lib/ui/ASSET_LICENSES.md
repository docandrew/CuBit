# CuBit UI Asset Provenance

This directory contains generated UI assets used by CuBit userspace apps.

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

## Bluecurve Icon Atlas

`userspace/services/desktop/desktop_icons.ads` was generated from 24x24 PNG
icons from the Bluecurve repository:

https://github.com/neeeeow/Bluecurve

The generated atlas currently includes the Start menu, CuBASIC Console, UI Lab,
DOOM, Security Center, Files, and Power icons. The source repository declares
GPL-3.0, and Fedora packages the Bluecurve icon theme as GPL-2.0-or-later.

`userspace/services/desktop/desktop_window_icons.ads` was generated from the
Bluecurve Metacity window control PNGs in `themes/Bluecurve/metacity-1`. The
generated atlas currently includes Close, Minimize, Maximize, Restore, and Menu
icons.
