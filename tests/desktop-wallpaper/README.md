# Wallpaper regression

Run in the Nix environment after building the wallpaper asset package
(docs/assets.md):

```sh
nix develop -c make -C kernel wallpaper-assets
nix develop -c bash -lc 'ulimit -s 65536; cd kernel && alr exec -- gprbuild -p -P ../tests/desktop-wallpaper/wallpaper.gpr && ../tests/desktop-wallpaper/build/main'
```

Reads the real package's catalog (`kernel/build/Assets/cubit-wallpapers/2`, or
the directory given as the first argument) with Desktop_Wallpaper_Catalog and
decodes every wallpaper it names into Desktop_Wallpaper_Store exactly as
Desktop does (each at the size its QOI header declares), then tests the
native renderer against each: exact pixels at its own size, exact centered
cropping, widescreen and portrait aspect fill,
downscaling, a one-pixel viewport, odd byte pitch, untouched padding and
surrounding canaries. Tiled dirty paints must reproduce a full paint exactly.
The host-only stack limit accommodates the largest guarded framebuffer fixture.
This is executable testing, not a SPARK proof or a compositor latency benchmark.
