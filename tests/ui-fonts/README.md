# Runtime TrueType validation

All commands run from the repository root through Nix. No native kernel
assertions are enabled by these hosted test projects.

```
nix develop -c make -C kernel test-ui-fonts test-procmgr-reads
nix develop -c python3 tests/ui-fonts/test_development_disk.py
nix develop -c bash tests/ccl-file-dialog/run-preview.sh
nix develop -c make -C kernel ui-editor-test test-appearance
nix develop -c bash tests/userspace-allocator/runtime.sh
nix develop -c bash tests/userspace-allocator/run.sh
```

The font suite checks every supported face/size/character, pixel agreement
with the reference renderer, concurrent cache publication, grayscale output,
Ada/Rust record layout, clipping, surface pitch, and transparent blending.
The measured scratch ceiling prevents a font update from silently requiring
the native allocator's coarse large-object arena. Current largest allocations
are 544 bytes at normal size and 1,948 bytes at double size; both release all
scratch after each glyph. Warm cache lookup allocates nothing.

The disk-builder tests inject corrupted readback despite a successful tool
exit and unchanged file size, checking that the old image remains untouched.
The separate [reader suite](../procmgr-reads/README.md) tests 150 transfer
sequences and has a focused SPARK arithmetic/termination harness.

## Native CuBit

Build before running headless tests. Run native tests **sequentially**: they
temporarily modify shared boot-image inputs. Four-CPU KVM runs use 128 MiB RAM.

```
nix develop -c make -C kernel desktop-session-content nvme_disk.img iso
nix develop -c bash tests/headless/run.sh --test ccl-workspace --accel kvm --cpus 4 --timeout 45 --keep-logs
nix develop -c bash tests/headless/run.sh --test files --accel kvm --cpus 4 --timeout 40 --keep-logs
nix develop -c bash tests/headless/run.sh --test desktop-display --accel kvm --cpus 4 --timeout 35 --keep-logs
```

The native Workbench test exercises first paint, the live clock, REPL input,
save/open, quoted text, and canceling the unsaved-edits prompt. With logs kept,
it also captures a real QEMU screenshot while the clock is running. Files
covers scrolling and column interactions; desktop-display covers input,
dragging and title-bar double-click maximize/restore.

For interactive use, run `nix develop -c make -C kernel run-desktop` first,
not the no-build fast target. Tests leave a fixture ISO; the normal launcher
rebuilds the image with the desktop profile.

## Evidence boundaries

The allocator metadata proof still passes across its five selected units
(215 proof diagnostics in this run), without skipped analysis or assumptions.
That does not prove the Rust backing-pointer mapping, cache synchronization,
font parser/rasterizer, or desktop IPC. Those boundaries have regression tests.
Cold glyph timing and warm lookup measurements are hosted microbenchmarks,
not native input-to-photon guarantees. Physical-laptop validation remains a
separate follow-up.

## Logical-to-physical canvas primitives

The Ada pixel suite also covers all 256 density numerator/denominator pairs
from 1..16. It checks adjacent filled cells, padded rows, nested fractional-origin
views, clip preservation, alpha bitmap composition and the 8x16 bitmap font.
Oversized rectangle/clip controls exercise `Natural'Last` without arithmetic
wrap. The normal-scale TrueType/control tests remain enabled. Geometry proof is
in `tests/compositor/client_canvas.gpr`: 22 checks, zero unproved/justified.

Native compatibility: Desktop, desktop-shell and Files build; the 90-second
`desktop-display` CuBit/QEMU run and final fault scan pass. Logs are
`/tmp/cubit-ui-density-native-final.log` and `-native.serial`; hosted font/surface
results are `/tmp/cubit-ui-density-integrated-final.log`. Non-unit DPI remains
disabled in application canvases until buffer configuration/lifetime integration
is complete. Hosted TrueType mask rendering is now covered below. This is not a native mixed-DPI
or performance result.

## Density TrueType and bounded glyph ownership

The pixel oracle also compares toolkit text with fresh Rust rasterizer masks at
5/4, 3/2, 2/1, 16/1 and 1/2 density for Sans and Monospace. It checks clipped
transparent/opaque output and verifies that 2x masks differ from enlarged normal
glyphs. `tests/compositor/client_glyphs.gpr` checks 570 real masks, warm reuse,
held-reader stability during eviction, foreign-owner finish rejection, the
32-reader bound, terminal close and final reclamation. Its 148 SPARK checks,
including instantiated policy, all pass without unproved or justified checks.

The backing cache is fixed at 512 KiB plus metadata per process; calls are
serialized. Raw memory/font operations remain outside the owner proof.
The density loop now uses `Client_Glyph_Blend`: its separate SPARK proof covers
bounds, channel arithmetic, termination and unchanged pixels outside damage.
Run `gnatprove -P tests/compositor/client_blend.gpr -u client_glyph_blend.adb
--level=2 --report=all -j2` inside Nix (75 checks, none unproved/justified).
Build the same project and run `tests/compositor/build/client-blend/client_glyph_blend_tests`
for 16,777,216 channel combinations and 38,220 clipped/padded/partial-row cases.
The bridge checks virtual non-overlap and lengths; valid mappings and exclusive
writable ownership are still assumptions. Native builds and the normal-scale 90-second desktop-display boot
regression pass. Native fractional-density text now also passes in the
`desktop-protocol` fixture: five scales, two faces, fresh masks, clipping,
padding and outline comparison. See `/tmp/cubit-native-density.log` and `.serial`.
This is native offscreen rendering; configured per-output client scaling and
physical presentation remain separate integration gates.
Evidence is `/tmp/cubit-ui-text-integrated.log`, `-integrated-final.log`,
`-native-final2.log`, `-native-complete.log` and `-native.serial`.
