# Runtime font components

The native static archive uses Rust only: no FreeType, HarfBuzz, C allocator,
Linux libc, or dynamic linker. Linux-hosted previews use the same rasterizer
with the host Rust allocator. Exact sources/checksums are in `../Cargo.lock`,
fetched reproducibly by Nix's `importCargoLock`.

Production uses `ttf-parser`, `ab_glyph_rasterizer`, and `libm` directly,
plus `core_maths` for the parser's no-std floating-point operations.
`ab_glyph` and `owned_ttf_parser` are hosted test dependencies
only, providing an independent outline-collection path for pixel comparison.

| Component | Version | License | Upstream |
| --- | --- | --- | --- |
| ab_glyph | 0.2.32 | Apache-2.0 | https://github.com/alexheretic/ab-glyph |
| ab_glyph_rasterizer | 0.1.10 | Apache-2.0 | https://github.com/alexheretic/ab-glyph |
| owned_ttf_parser | 0.25.1 | Apache-2.0 | https://github.com/alexheretic/owned-ttf-parser |
| ttf-parser | 0.25.1 | MIT OR Apache-2.0 | https://github.com/harfbuzz/ttf-parser |
| core_maths | 0.1.1 | MIT | https://github.com/robertbastian/core_maths |
| libm | 0.2.16 | MIT | https://github.com/rust-lang/libm |

Unmodified upstream license texts are in `licenses/`. Preserve these notices
when distributing the linked components. `libm`'s complete license text also
contains its inherited third-party notices.

Unmodified IBM Plex Sans Regular and IBM Plex Mono Regular are embedded from
the pinned Nix `ibm-plex` package. Their SIL OFL 1.1 notice is retained at
`../../lib/ui/licenses/IBM_PLEX_OFL.txt`; see the UI asset provenance document.
No externally supplied font file is accepted by this first runtime boundary.
