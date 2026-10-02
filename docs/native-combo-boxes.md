# Native combo boxes

`CuBit.UI.Combo_Boxes` is a non-editable selection widget with an inset value
field, a raised arrow button, and a bounded popup.
Combo boxes and both scrollbar orientations use `CuBit.UI.Draw_Arrow_Button`
for the same frame, disabled/pressed treatment, and optical glyph centering. The arrow button is a 16-pixel square with a four-pixel right inset, aligned with a standard scrollbar inside a tree frame. `Default_Height` is 26
logical pixels; callers can supply a different height in `Bounds`. It uses the toolkit theme,
font, density, clipping, and retained control map. It performs no allocations
or IPC. A model holds up to 64 caller-owned caption strings and per-choice
enabled flags. Keep those strings alive for the model's lifetime.

Keep one `Combo_State` per widget. Call `Set_Selection` to initialize or update
its value; zero clears it, and invalid/disabled indices also clear it. Read
`Selection` after a handler reports `Changed`. Revalidate selection with
`Set_Selection` when replacing the model. Drawing never commits a value.

Reserve `Base .. Base + 65` in the application's control map (field, choices,
popup shield). Draw the combo after surrounding content and registrations so
its popup paints and receives hits above them. Draw other overlapping popups
last, and keep only one popup owner active. Rebuild the map after state, model,
or geometry changes. The popup shows at most eight rows by default, follows
the highlighted choice, and opens above the field when that offers more room.
Only visible enabled rows are registered; the bounded control-map capacity
still applies across the whole application.

Route focused keyboard input to `Handle_Key`:

- Alt+Down or F4: `Toggle`.
- Up/Down/Home/End: corresponding keys. Closed lists commit immediately;
  open lists move the highlight until Enter/Space commits.
- Enter/Space: `Commit`, opening the list if closed.
- Escape: `Cancel`, preserving the committed value.
- Tab: `Tab_Key`, dismissing the popup without consuming focus traversal.
- Printable letters: `Type_Character`, cycling through enabled captions with
  that initial letter, case-insensitively.

Pass the same enabled state to drawing and input handlers. Empty or entirely
disabled models are inert. Disabled choices are muted and skipped by keyboard
navigation. `Handle_Wheel` moves an open popup by one choice per wheel event;
it does not iterate over an arbitrary wheel delta or commit the choice.

Pointer routing follows the menu controller contract. While open, route an
outside **press** to `Handle_Pointer` before the app dispatches it, and consume
it so dismissing a list cannot activate a background control. Route other
pointer events through `App.Apply_Pointer_Event` (or `Controls.Dispatch_Pointer`)
first, then the combo handler, using the current `Controls.Hit` target rather
than the captured ID. Mouse press/release selects a row; dragging from the
field into a row also selects it. Pointer cancellation dismisses the popup
without changing selection. Repaint the exposed popup area when dismissing.

`tests/ui-polish/combo_tests.adb` exercises keyboard commit/cancel, disabled and
empty models, 100 retained mouse cycles including repaint during capture,
outside dismissal, drag selection, wheel/64-choice overflow, upward placement,
609 tiny fields, and partial-render equivalence in two palettes at five scales.
The toolkit gallery includes both closed and open examples. These are hosted
renderer/controller checks, not a claim of integration into an application.
