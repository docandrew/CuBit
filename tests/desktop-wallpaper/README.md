# Wallpaper regression

Run in the Nix environment after building the wallpaper asset package
(docs/assets.md):

```sh
nix develop -c make -C kernel wallpaper-assets
nix develop -c bash -lc 'ulimit -s 65536; cd kernel && alr exec -- gprbuild -p -P ../tests/desktop-wallpaper/wallpaper.gpr && ../tests/desktop-wallpaper/build/main'
```

Decodes the real QOI assets (`kernel/build/Assets/cubit-wallpapers/1`, or the
directory given as the first argument) into Desktop_Wallpaper_Store exactly as
Desktop does, then tests the native renderer against them: exact pixels at
2048x576, exact centered cropping, widescreen and portrait aspect fill,
downscaling, a one-pixel viewport, odd byte pitch, untouched padding and
surrounding canaries. Tiled dirty paints must reproduce a full paint exactly.
The host-only stack limit accommodates the largest guarded framebuffer fixture.
This is executable testing, not a SPARK proof or a compositor latency benchmark.
