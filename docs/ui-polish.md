# Native toolkit visual proportions

The shared toolkit uses shallow, two-edge bevels to distinguish raised buttons
from inset text fields. Buttons also have narrow highlight and shade bands;
pressed buttons reverse the bevel and omit those raised-face bands. Checkboxes
and slider thumbs share the inset and raised edges. Hover and selection use
restrained colors from the existing palette; selected tabs keep their
orientation-specific accent. The horizontal accent spans the full tab width,
with one-pixel light/dark ends matching the side bevel. Drawing uses opaque fills and strokes, with no
shadows, blur, animation, or new rendering backend.

Button captions keep a stable baseline when pressed. Single-line fields move
the text, selection, and caret down two pixels where height allows it. Text controls use eight
logical pixels of horizontal padding, buttons six, and content clips prevent
long labels from painting over frames or adjacent controls. Status text and
key/value labels have separate lanes. Table headers share single dividers.
Menu and tab dividers remain continuous under individual items.

The menu bar uses one framed strip with a subtle vertical gradient, drawn as
opaque horizontal rows using the existing gradient primitive. Titles remain
flat until hovered or opened; transparent text preserves the gradient beneath
letters. The palette supplies the endpoints, so both light and dark schemes
retain the same proportions. Menu titles and commands underline the first
case-insensitive match for their declared mnemonic; blank or absent matches
have no underline. The renderer uses the same model as keyboard dispatch.
Tabs have top and side bevels, including unselected tabs, but no bottom
bevel. Selected tabs retain their accent and open edge into the page. Lists and scroll areas use the shared sunken
viewport frame, with at least two pixels reserved for the rim.

Menus use four pixels of popup padding and compact eight-pixel separator rows;
hit rectangles follow the rendered rows. Groups and panels use simple outlines. Status bars use a sunken two-edge
frame inside a narrow panel margin, retaining separate clipped text lanes. Radio buttons use a 14-pixel circular shape with precomputed coverage and
coalesced opaque spans. Checkbox marks use fixed opaque spans. Scrollbar thumbs
fill the cross-axis width of the track in both orientations.

The [native combo box](native-combo-boxes.md) combines the inset field and raised
arrow treatment with a clipped popup, retained pointer input, and keyboard
selection.

The browser adopts eight-pixel toolbar gaps and the shared field/button drawing.
Settings remains available from Edit > Settings; tab orientation is still
stored through Config. No public toolkit API, glyph/cache path, or browser
filesystem grant changes are required by this visual pass.

Hosted regression coverage and the reproducible widget gallery are described
in [tests/ui-polish](../tests/ui-polish/README.md). These checks cover rendering
bounds, partial repaint equivalence, and primitive counts; visual inspection
and native browser interaction are separate validation steps.
