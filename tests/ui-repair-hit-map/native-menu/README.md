# Native menu partial-repaint regression

This fixture uses UI.App.Run with protected frames and actual retained menu
controls. F6 requests an unrelated 8×8 content repair while the menu is open.
Render verifies the item remains in the hit map and reports when the actual
repair is small. Native input then selects command101 by pointer and command202
with F10, Down, Down, Enter. Each command must occur exactly once.

The paired run removes only the seven hit-map repair changes, preserving the
current size API, identical fixture source and identical input runner. It fails
with `lost item after repair`; repaired source passes. `paired-evidence.json`
records binary identities, original compile/link commands, complete input
inventories and interaction runners. Build this main.adb with the matching
UI/runtime and its generated manifest bindings; the recorded native builds
reserve the runtime-required16MiB stack. No binary is staged into user images.

Coverage is native100%DPI software composition. The fixture has no underlying
controls, so it does not establish outside-click isolation; that remains in
the hosted menu suite. Native scaled menus and physical GPU behavior are not
claimed. The first keyboard oracle and an API-mismatched negative build remain
recorded as failures and are not counted as successful runs.
