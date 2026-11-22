# Settings renderer separation

Run from the repository root in the Nix development environment:

```sh
nix develop -c bash -c 'make -C kernel time-zones && mkdir -p tests/settings-renderer/build/protocol && cp userspace/runtime/gnat/cubit-desktop_protocol.ad? userspace/runtime/gnat/cubit-grant_references.ads tests/settings-renderer/build/protocol/ && cd kernel && alr exec -- gprbuild -p -P ../tests/settings-renderer/settings_renderer.gpr && ../tests/settings-renderer/build/renderer_tests'
```

This hosted regression invokes the real Settings layout with a null pixel
address, first recording its seven operations and then decomposing its buttons
and tabs through `CuBit.UI.Control_Renderer`. The layout is CuBit.UI.Boxes
(UI-033), measured with the real UI font: the tests take the window's size from
`Desktop_Settings.Measure` and address controls and list rows by identity
(`Control_Bounds`, `Row_Bounds`), never by pixel constants (TEST-007). Every
page is checked at its natural size, at the layout's minimum and at twice the
natural size: each control inside the window, no two overlapping, no text run
leaving the window or running into another on its line (the clipped subtitle
and "138.780 MHlink type ..." run-in of the laptop photo), and the minimum is
tight (some page does not fit one pixel narrower). Long refusal reasons wrap in the
mode list's status column (rows grow) rather than widening the window; Page
Up/Down move by the rows a placed list shows; the mouse wheel scrolls the list
under the pointer without a click (UI-035); and `Publish_Layout` names every
widget (Desktop logs it for check-dual-desktop.py). It covers all three pages, every
appearance preference combination, and full, partial and disjoint clips (288
cases), then drives the Date & Time zone list (letter jump, arrows, Page
Down, Home/End, a row click, Apply and Revert) and the Monitor page's mode
list (DISP-016: measured columns under a header row; refused rows skipped by
keys and clicks, paging, Apply,
Revert to the saved mode, the "Keep this display mode?" question with only
Keep and Revert reachable). `build/protocol` holds hosted copies of the
freestanding protocol units the mode list's types come from. Additional calls cover every button style and tab interaction state,
both tab orientations, and empty/tiny control rectangles. The wallpaper body
raises if the renderer accidentally bypasses its supplied wallpaper callback.

The generic interfaces have no default drawing operations. The physical-output
adapter must explicitly provide every primitive; the existing canvas APIs
instantiate the same control implementation with the software canvas primitives.
No global callback installation or Canvas ABI change is involved.

This checks control-flow separation and runtime assertions, not pixel parity,
SPARK proof, GPU execution or presentation timing. Desktop now binds the
controls and wallpaper preview directly to the current output writer. The
wallpaper writer has its own guarded-buffer fixture in
`tests/compositor/wallpaper_output.gpr`. Native regression uses the `desktop-dual-output`
headless fixture under `coordination/build.lock`.
