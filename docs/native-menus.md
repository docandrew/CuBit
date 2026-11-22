# Native menubars

`CuBit.UI.Menus` adds a bounded menu controller to the Desktop UI toolkit. It
uses the existing native menubar/title renderer, themes, density-aware fonts
and clipped canvas, and the same retained `Controls.Control_Map` as `UI.App`.
No application callback runs during drawing. The caller receives a numeric
command and implements the action itself.

The model supports eight top-level menus and 64 total items. Each item has a
parent menu, caption, optional shortcut caption and mnemonic, command, enabled
state, checked state and separator flag. Text points to caller-owned constant
strings; those strings must outlive the model. No allocation or IPC is done by
the menu controller. `Valid` rejects orphaned items and nonseparator command 0.
Invalid models draw and activate nothing.

The strip has a shallow outer frame and an opaque vertical gradient. Titles
remain flat at rest. Declared title and item mnemonics are always underlined;
the first case-insensitive caption match is marked, including on disabled rows
in their muted color. Blank or absent matches are not marked. Underlines share
the caption clip and density mapping, and shortcut captions remain separate.

## Integration

1. Keep one `Menu_State` and model per window. Reserve a contiguous range of 73
   control IDs: `Base .. Base + 72`. IDs must not overlap the application's
   other controls. Titles use the first eight, items the next 64, and the last
   ID is the popup background shield. The map's usual total registration limit
   of 128 still applies; only the current popup's visible enabled items are
   registered. Separators and disabled rows hit the shield.
2. Clear/rebuild the usual control map, draw/register application content, then
   call `Menus.Draw` last. Supply a logical menubar rectangle and the full window
   canvas so the popup can extend below the bar. Its background and row controls
   override the underlying content's hits. Pointer hit geometry and rendered
   pixels use the same clipped canvas. `Popup_Width` defaults to 260 and rows
   to 28 logical units.
3. Resolve the pointer target with the committed map. While a menu is open,
   offer **nonmenu presses** to `Handle_Pointer` before `App.Apply_Pointer_Event`
   and consume the event. `Is_Menu_Control` identifies the reserved range.
   An outside press dismisses the menu without giving an underlying control a
   press capture. Do not forward that press to the page/editor. Menu targets and all moves/releases
   go through normal `App.Apply_Pointer_Event` first, then `Handle_Pointer`,
   which consumes retained item activation. Pass the current `Controls.Hit`
   result as `Target`, **not** the captured control ID: during a title drag App
   retains title capture, while the menu uses the item currently under the
   pointer as the release destination. In particular, an outside release must
   still reach App to clear its captured title/item; consume the event after
   the controller handles it so application content cannot act on it. The application's pointer state
   and capture must still be cancelled/resynchronized on input discontinuity;
   `Pointer_Cancel` dismisses the menu.
4. Repaint/rebuild before another pointer event whenever menu state, model,
   layout or density changes. This removes the old popup registry and pixels.
   Menu controls request whole-canvas action damage, because opening, closing,
   switching and scrolling can change pixels outside the original title.
   Consume a returned command once, then dispatch it in the application.
5. Map F10 (or the platform's Alt activation) to `Activate`, arrows to their
   matching keys, and Home/End, Enter/Space, Escape and Tab appropriately.
   Alt+letter maps to `Mnemonic` when closed; plain letters map to it when open.
   When `Handled` is false, pass the key to the normal editor/page input. When
   true, consume it and repaint. Shortcut captions are display text; applications
   continue routing global accelerators such as Ctrl+N themselves.
6. Dismiss on focus loss, modal opening, window close and input-stream reset.
   Application preferences belong in the Config service. For example, File →
   Settings opens the application's settings dialog; the dialog persists the
   side-tabs preference and the model can show its checked state if needed.

`Open_Menu` and `Selected_Item` expose read-only indexes for diagnostics.
`Dismiss` clears the state. Menus open on a title press; a second press closes
that title. Moving over another title switches the open popup. Items activate
on native retained press/release. Pressing a title, dragging to an enabled item
and releasing also activates; App retains title capture while the controller
tracks the menu gesture. Every release or cancellation clears that drag state.
Releasing outside dismisses without activation, and disabled rows cannot fire. Disabled items and separators are skipped by keyboard traversal.
Up/Down and Left/Right wrap; Home/End select the first/last enabled item; Escape
and Tab dismiss. A unique enabled mnemonic activates; duplicate mnemonics cycle
through matches for a subsequent Enter. An empty menu safely accepts dismissal.

Popup placement is bounded by the clipped canvas. Long menus scroll the selected
keyboard row into view and register only visible rows. This initial API provides
one level of popup menus; hierarchical submenus and mouse-wheel scrolling are not implemented. Long menus remain fully
accessible with the keyboard. Mnemonic letters are explicitly supplied; captions
do not interpret ampersand markup.

## Verification and preview

From the repository root:

```sh
nix develop -c gprbuild -p -P tests/ui-menus/menus.gpr -j2
nix develop -c tests/ui-menus/build/menus_tests
nix develop -c tests/ui-menus/build/menus_preview
```

These are **Linux-hosted tests of the actual native toolkit**, not a booted
Desktop integration claim or SPARK proof. Tests cover 100 retained pointer/redraw
cycles, click activation, outside dismissal without underlying capture, disabled
and separator shielding, pointer cancellation and release outside, title press-drag-release with
disabled/outside/cancel/stale-release negatives, keyboard
wrap/skipping/Home/End/Enter/Escape/Tab, unique and duplicate mnemonics, all 64
items through keyboard overflow, invalid/empty models, and exact sentinel pixel
clipping at 100% and 200% density. The preview writes
`/tmp/cubit-native-menus-preview.ppm`, showing the real light/dark toolkit renderer.

The existing externally built native-font host library must be present, as for
`tests/servo/native_tabs.gpr`. Hosted build outputs stay in `tests/ui-menus/build`.
