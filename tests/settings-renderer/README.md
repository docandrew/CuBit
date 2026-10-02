# Settings renderer separation

Run from the repository root in the Nix development environment:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/settings-renderer/settings_renderer.gpr && ../tests/settings-renderer/build/renderer_tests'
```

This hosted regression invokes the real Settings layout with a null pixel
address, first recording its seven operations and then decomposing its buttons
and tabs through `CuBit.UI.Control_Renderer`. It covers both pages, every
appearance preference combination, and full, partial and disjoint clips (144
cases). Additional calls cover every button style and tab interaction state,
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
