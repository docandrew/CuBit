# Wallpaper regression

Run in the Nix environment after building the desktop's asset:

```sh
nix develop -c make -C kernel desktop
nix develop -c bash -lc 'ulimit -s 65536; cd kernel && alr exec -- gprbuild -p -P ../tests/desktop-wallpaper/wallpaper.gpr && ../tests/desktop-wallpaper/build/main'
```

Tests the native renderer against the real embedded asset: exact pixels at
2048x576, exact centered cropping, widescreen and portrait aspect fill,
downscaling, a one-pixel viewport, odd byte pitch, untouched padding and
surrounding canaries. Tiled dirty paints must reproduce a full paint exactly.
The host-only stack limit accommodates the largest guarded framebuffer fixture.
This is executable testing, not a SPARK proof or a compositor latency benchmark.
