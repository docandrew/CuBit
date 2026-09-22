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
