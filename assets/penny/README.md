# Penny identity

Penny uses the copper globe selected from logo direction 01: a coin rim and
raised latitude/longitude lines, with no currency text or national symbols.

- `penny-copper-globe-master.png`: original generated transparent master.
- `penny.svg`: editable vector interpretation, using only paths, circles,
  strokes, clipping and gradients. No embedded raster, external resources,
  fonts, blur or shadow filters.
- `penny-{16,24,32,64,256}.png`: transparent icon exports from the SVG.

Regenerate exports and `userspace/servo/native/penny_artwork.ads` with
`nix develop -c python3 assets/penny/render.py` from the repository root.

The SVG deliberately simplifies the master's surface texture for small icons.
Native UI artwork is rendered ahead of time; no SVG parsing or metallic effects
are required during repaint. Servo remains credited as the browser engine.
