# Hosted toolbar rendering checks

```sh
nix develop -c sh -c 'cd kernel && alr exec -- gprbuild -p -P ../userspace/lib/ui/tests/toolbar/toolbar_test.gpr && ../userspace/lib/ui/tests/toolbar/build/main /tmp/cubit-toolbar-states.ppm'
```

The optional output argument writes a PPM gallery: normal, pressed, and disabled
rows in `Toolbar_Icon` order. Assertions exercise straight-alpha blending,
transparency, nonzero image array bounds, desaturation, empty/off-canvas clips,
button and parent clipping, tiny bounds, and disabled buttons ignoring presses.
This is a Linux-hosted pixel-buffer test; it does not open a window or need SDL.
