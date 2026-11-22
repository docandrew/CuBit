# Devices scroll / hover regression

Run from the repository root in the Nix environment, holding the shared build
lock for the native build and test:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c \
  'make -C kernel devices && python3 tests/devices-hover/run.py'
```

The test builds a disposable disk and ISO from the staged kernel/services and
fresh Devices binary. It starts a 1024x768, four-CPU TCG guest, adds sixteen
virtual PCI RNG devices to force tree overflow, and performs six scrollbar
arrow / row-hover / pointer-leave cycles. It requires scrolling to change the
visible rows immediately and the entire tree region to remain byte-identical
after hover styling disappears. Screenshots, input hashes and a JSON result
are retained in the printed artifacts directory. The base disk and build
inputs must remain unchanged, and guest fault markers fail the test.

Before the fix, Devices consumed a pending retained scrollbar value *after*
drawing its rows. Hover subsequently repainted individual rows using the new
offset, mixing scroll positions. The native baseline changed 17,480 tree
pixels after hover/leave. Devices now handles the scrollbar before drawing
rows; its thumb also reflects the visible row count.

This is a native software-rendered QEMU regression, not a physical GPU or
smooth-scrolling performance measurement.
