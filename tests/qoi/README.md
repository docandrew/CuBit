# QOI decoder and wallpaper store

`CuBit.QOI` (`userspace/lib/image/`) decodes QOI streams for asset packages
(`docs/assets.md`). `Desktop_Wallpaper_Store` holds Desktop's decoded
wallpapers.

```sh
nix develop -c make -C kernel test-qoi    # hosted tests
nix develop -c make -C kernel prove-qoi   # gnatprove --level=1, both units
```

`make_fixtures.py` writes the fixtures. Each valid image is encoded by both
`tools/qoi.py` and Pillow's independent encoder, and comes with its RGBA
pixels. Malformed streams name the failure the decoder must report.

`qoi_tests` checks:
- each valid fixture in chunks of 0 (all at once), 1, 2, 3, 5, 13, 64 and
  4096 bytes, with exact pixels;
- that a larger caller limit leaves the rest of the buffer untouched;
- that every proper prefix is `Truncated`;
- that each malformed stream is rejected with its expected failure, in three
  chunkings;
- 2,000 deterministic one-to-three-bit-flip mutants per fixture. Each must
  end `Complete` or `Failed`, with `Written <= Total <= Limit`. Built with
  `-gnata -gnato`, so a run-time check failure would also fail the test;
- the real wallpaper package, decoded and compared pixel-for-pixel with the
  rasters it was built from (`--reference`), plus its limit check and 40
  mutants.

Proved: absence of run-time errors and the stated contracts of `CuBit.QOI`
and `Desktop_Wallpaper_Store`. Not proved: that decoding equals the QOI
specification. The tests cover that against two encoders.

## Icon atlas store

`Desktop_Icon_Store` holds Desktop's application icon atlas
(`Assets/cubit-icons/1/applications.qoi`, docs/assets.md). `make test-qoi`
also builds the atlas with its RGBA oracle (`tools/build_icon_assets.py
--reference`) and runs `icon_store_tests` once per case
(`run-icon-store.sh`; the store loads once per process):
- the real atlas, fed at once and in 1, 7 and 4096-byte chunks, equals the
  source PNGs pixel for pixel, for every icon;
- a truncated, corrupt (bad magic) or wrong-size atlas (`make_icon_fixtures.py`),
  or a failed read, never loads, and then every icon is the fallback glyph;
- before any load every icon is the fallback glyph, and the glyph has its
  transparent margin, frame, cross and fill.

Proved (`make prove-qoi`, level 2): absence of run-time errors in
`Desktop_Icon_Store` and that an icon read before or without a complete
atlas is the fallback glyph (`Pixel`'s postcondition).
