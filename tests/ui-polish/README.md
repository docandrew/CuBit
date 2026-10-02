# Toolkit visual regression checks

This hosted gallery uses the production `CuBit.UI` drawing code with the
existing light and dark palettes. It is not a native CuBit screenshot.

Run from the repository root inside the Nix environment:

```sh
nix develop -c gprbuild -p -P tests/ui-polish/polish.gpr -j2
nix develop -c tests/ui-polish/build/polish_tests
nix develop -c tests/ui-polish/build/combo_tests
nix develop -c tests/ui-polish/build/tree_preview
nix develop -c tests/ui-polish/build/polish_preview /tmp/cubit-ui-polish.ppm
nix develop -c tests/ui-polish/build/menu_options_preview /tmp/cubit-menu-options.ppm
```

Coordinate shared source compilation according to `coordination/README.md`.

The regression checks cover 20 controls in both palettes at five densities
(200 clipped/full-render comparisons), 3,380 tiny or empty bounds, and isolation
between status/key labels and their adjacent values. The tests assert pixels
outside the control or clip are untouched, and partial paints match full paints.

A callback counter limits a representative 116-by-32 button frame to one face fill, two small edge bands,
and two strokes: 4,744 conservative logical pixel writes. The previous frame
used six overlapping fills, totaling 19,192. This is an operation/overdraw
comparison, not a timing or hardware performance benchmark. Captions and the
unchanged font renderer are excluded from this count.

Related regressions: `tests/ui-menus/menus.gpr`,
`tests/ui-fonts/fonts_tests.gpr`, and
`tests/settings-renderer/settings_renderer.gpr`.

The gallery includes the bounded [native combo box](../../docs/native-combo-boxes.md),
with separate keyboard, retained pointer, popup density/clip, and scrollbar
geometry checks in `combo_tests`.

`tree_preview` renders the existing native tree (branches, disclosure boxes,
selection and icons) with the 26-pixel combo box and full-width scrollbar
thumbs to `/tmp/cubit-tree-preview.ppm`. It does not change the tree renderer.
