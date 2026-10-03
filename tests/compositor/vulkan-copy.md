# Vulkan opaque client-copy boundary

This is a GPU command primitive for the existing unscaled, opaque client-blit
path. It is not yet selected by Desktop. No direct target transport, Vulkan
image import, scanout or accelerated desktop is activated by these files.

`Vulkan_Copy_Binding.Draw_Client` calls the existing SPARK
`Desktop_Composition.Plan`, then the narrow `Vulkan_Copy_FFI` and
`cubit_vulkan_record_copy`. The latter translates one checked region into
`vkCmdCopyImage` on borrowed Vulkan images and a borrowed recording command
buffer. Mesa supplies that dispatch function. There are no GPU packets, pixel
allocations, mappings, staging uploads, readbacks, device creation, submission,
waits or hidden queue work in the production adapter. Copying client pixels
into a composite is still work; this avoids a CPU intermediary for that draw.
The current production Desktop copies are unchanged.

Empty geometry records nothing. Null dispatch/command/image handles, identical
source/target handles, and invalid integer extents are rejected before dispatch.
Distinct handles do not prove nonaliasing: allocation authority, bound-memory
nonaliasing, exact image dimensions, format, transfer usage, GENERAL layouts,
queue ownership and synchronization remain caller obligations. See the full
borrowed-object contract in `userspace/lib/compositor/vulkan_copy.h`.

`Recorded` means only that the void Vulkan recording call was issued. It never
means rendered, submitted, GPU complete, latched or retired. Callers must retain
all referenced storage and command state through actual completion and retain
scanout storage through presentation retirement. Device loss and unknown
completion must not authorize reuse. This must be bound to the compositor pool
and authoritative driver events before enabling a production path.

## Proof and foreign boundary

The binding and the unchanged clipping planner have 41 SPARK analysis results:
7 flow and 34 prover results, none unproved or justified. This establishes the
existing planner's geometry contracts and the binding's nonempty, bounded
integer preconditions. It does not prove Vulkan behavior, GPU memory authority,
image layout, hardware synchronization, or completion. The Ada ABI conversion,
C translation, Vulkan/Mesa, and borrowed pointers are trusted boundaries.
`Global => null` abstracts exclusively borrowed native command state; it does
not mean the physical GPU command buffer is unchanged. New lifecycle/scheduling
policy must not be hidden inside this foreign adapter.

Reproduce the proof inside the repository's Nix environment:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/compositor/vulkan_copy.gpr -u vulkan_copy_binding.adb desktop_composition.adb --mode=all --level=4 --report=all --checks-as-errors=on -j1'
```

## Hosted execution evidence

```sh
nix-shell tests/compositor/vulkan-copy-shell.nix --run 'bash tests/compositor/test-vulkan-copy.sh'
```

The fixture calls the actual Ada binding and C adapter. Pinned Mesa lavapipe
executes 160 submissions against two separately allocated images. An independent
pixel-membership oracle uses the original destination/clip inputs, not the
adapter's planned rectangle. All 122,880 target pixels, including undamaged
sentinels, match. There are 74 actual copy commands, 80 empty draws and six
rejections (null context, absent dispatch, absent command, absent target, absent
source and identical images). Extreme coordinates and empty extents admit no
command. Vulkan synchronization validation reports zero errors for the valid
run. A second run deliberately omits the clear-to-copy barrier and must fail
with a write-after-write hazard, proving that validation is active.

Test-only upload/readback buffers initialize inputs and inspect results; they
are not part of the production adapter. This is Linux CPU Mesa execution, not
native CuBit GPU, i915, refresh-rate, latency or tear-free presentation evidence.
The first negative-control runner expected a different diagnostic spelling and
failed despite detecting the correct hazard; the corrected gate passes.

## Native compilation

With `coordination/build.lock` held, run inside `nix develop`:

```sh
(cd kernel && alr exec -- gprbuild -q -p -c -P ../tests/compositor/vulkan_copy_native.gpr)
python3 tests/mesa-software/compile-native-probe.py userspace/mesa/build/native-aee5fe5697d39dd4 userspace/lib/compositor/vulkan_copy.c tests/compositor/build/vulkan-copy-native/vulkan-copy.o
```

This uses CuBit's Ada runtime and the existing Mesa musl compile configuration.
Native compilation passed for both layers; the C object has no undefined
external references (dispatch is indirect). This does not run a Vulkan device
inside CuBit. Logs, source hashes and proof summaries are retained locally in
`/tmp/cubit-vulkan-copy-evidence/`.

## Remaining integration

The next renderer work is affine/DPI sampling, blending and masks using the
existing proved geometry, plus a bounded command-resource lifecycle. Importable
writable targets, source image authority, coherency and authoritative render /
latch / retirement events still need the shared-target contract. A CPU mapping
or a read-only completed application grant is not a Vulkan image import.
Keep the software fallback until all these paths are verified natively. Hardware
performance and physical keypress-to-photon measurements remain outstanding.
