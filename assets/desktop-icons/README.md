# CuBit desktop icons

Icons for the desktop's Apps menu, its categories, the Apps button and Power,
drawn for CuBit in the Haiku/BeOS manner: bright colours, isometric shapes,
light from the upper left, and a dark outline that holds at 24 pixels.

- `make.py`: the source. Each icon is a small function that writes an
  editable SVG (paths, gradients and circles only: no filters, text or
  embedded rasters).
- `svg/`: those SVGs. `png/<name>-{16,24,32,48,64}.png`: exports from them.
- The Apps button shows the CuBit logo (`cubit`): a purple cube with one
  teal face.
- Penny keeps her own artwork (`assets/penny`).

Regenerate the exports and then the desktop atlas
(`userspace/services/desktop/desktop_icons.ads`) from the repository root:

    nix develop -c python3 assets/desktop-icons/make.py
    nix develop -c python3 tools/generate_desktop_icons.py

These are original CuBit artwork under the project's licence.
