# CuBit icon set

Original CuBit artwork (see `../../ASSET_LICENSES.md`). It is bright, bold and
volumetric, and leans on the cube: isometric cubes recur throughout (programs,
archives, network nodes, the house, the CCL script badge), in the
violet/teal of the CuBit wallpaper.

Layout: `<size>/<name>.svg` for sizes 16, 24, 32 and 48. Each size is its own
hand-placed drawing, not a scaled copy. `make_icons.py` is the authoring source:
one function per icon, with per-size geometry and shared primitives (palette,
outline underlay, extrusion, isometric box, page). Edit the function, then run
`python3 make_icons.py` to rewrite every SVG. Do not hand-edit the SVGs; the
next run overwrites them. The SVGs are plain: no raster, no external
references, no filters, only paths and linear gradients.

The toolkit atlas can be built from this set with
`python3 tools/generate_icon_atlas.py --set cubit`, or by setting `ICON_SET`
in that tool. No app uses it yet.

## Style rules

**Projection.** Volumetric objects (program cube, archive box, drive, house,
network nodes) use true 2:1 isometric: edges run 2 px across per 1 px down,
and box extents are even so vertices land on whole pixels. Flat objects
(folders, pages, star, clock, padlock) and action glyphs are drawn face-on,
then *extruded* down and to the right by the extrusion depth. This gives them
a solid side in the deep shade of their colour.

**Light.** The light comes from the top left. On isometric boxes the top face is
light, the left face mid and the right face dark. Gradients run from light at
the top left to mid at the bottom right (`x2=.35 y2=1`). Extrusions and right
faces use the dark/deep shades. Edges facing the light get an optional 1 px
white highlight at 45-70 % opacity.

**Outline.** Ink `#1d1530` (deep violet-black, never pure black) is drawn as an
underlay: each outlined shape is stroked at twice the outline width behind its
fill, so the outline sits wholly outside the fill edge. Every layer gets its
own outline, so an emblem on a folder still has one. Internal divisions use
colour changes or ink hairlines at 40-85 % opacity, not full outlines.

| size | outline | extrusion | drawable fill area | notes |
|-----:|--------:|----------:|--------------------|-------|
| 16 | 1 px | 1 px | x,y in [1, 14] (+1 extrusion) | essentials only: no text lines, ticks or facets; 1 px hairlines on .5 centres |
| 24 | 1 px | 1 px | [1, 22] | first level of detail (text lines, windows) |
| 32 | 2 px | 2 px | [2, 28] | facets and highlights |
| 48 | 2 px | 3 px | [2, 43] | full detail |

**Pixel grid.** Fill edges sit on integer coordinates and hairlines on half
pixels. Circles are centred so their extreme points fall on pixel edges. Each
16 px drawing is checked at 8x zoom against the grid.

**Silhouette first.** Each icon has to be identifiable from silhouette and
colour alone. Related concepts get different shapes: a generic file is a
plain dog-eared page, a document adds violet text lines, an image is a
landscape photo card (not a page), source code has teal chevrons, and a CCL
script is a dark console card with a cube and a lime prompt.

## Palette

Each family has four shades: light, mid, dark and deep. A plain family
gradient runs light to mid.

| family | light | mid | dark | deep | meaning |
|--------|-------|-----|------|------|---------|
| violet | `#c3adff` | `#8457ff` | `#5a32d6` | `#3b1e96` | CuBit / containers (folders), text accents |
| teal   | `#8af5e8` | `#1fc8c3` | `#0e8f99` | `#0a5f6b` | navigation, system, views, links |
| amber  | `#ffdc80` | `#ffa41f` | `#d9700b` | `#9a4a07` | physical objects: box, stick, clipboard, lock, star |
| lime   | `#d8ff85` | `#8fdc1f` | `#56a00c` | `#386b08` | go / add / success (plus badges, refresh, LEDs) |
| red    | `#ff9e90` | `#f2353f` | `#b5172a` | `#7c0e1c` | destructive only (delete); small accents (ribbon, eraser) |
| paper  | `#ffffff` | `#ebeef6` | `#c6cddd` | `#959db3` | pages, windows |
| steel  | `#eef1f8` | `#b4bccf` | `#7d879f` | `#4f5870` | hardware, tools, metal |
| slate  | `#5d5778` | `#3a3452` | `#2a2540` | `#1d1530` | console, media, close |
| ink    | | | | `#1d1530` | outline |

Use only these colours. A new concept should take its colour from the
meaning column, not from a new hue.

## Icons (batch 1)

Places/objects: `folder`, `folder-open`, `home`, `computer`, `drive`,
`removable`, `network`, `trash-empty`, `trash-full`, `bookmark`, `recent`.

File types: `file`, `file-document`, `file-image`, `file-audio`, `file-video`,
`file-archive`, `file-executable`, `file-source`, `file-ccl`, `file-book`,
`link`, `locked`.

Actions: `go-back`, `go-forward`, `go-up`, `refresh`, `new-folder`, `copy`,
`cut`, `paste`, `delete`, `rename`, `view`, `columns`, `sidebar`, `split`,
`new-tab`, `search`, `settings`, `info`, `close`.

## Adding an icon

1. Write `def my_icon(c):` in `make_icons.py`. Branch on `c.s` for per-size
   geometry, reuse `page`, `iso_box`, `window_frame` or `badge_plus` where
   they fit, and colour by family name (`"teal"`, `"teal:2"`).
2. Add it to `ICONS`, run `python3 make_icons.py`, and check all four sizes
   on light and dark backgrounds, with 16 and 24 zoomed, before committing.
