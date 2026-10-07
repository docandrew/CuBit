# Bounded monitor metadata

`CuBit.Monitor_EDID` is a shared SPARK package in `userspace/lib/display`.
It validates the 128-byte base block header, checksum and supported version
(EDID 1.3/1.4), then decodes the first preferred detailed timing. Interlaced,
stereo, bordered, missing and malformed timings return typed failures. Physical
dimensions may be unspecified. Reported millihertz is the advertised nominal
pixel-clock/total calculation, **not measured refresh**.

The decoded timing now retains horizontal/vertical front porches and sync pulse
widths. `Sync_Info` distinguishes analog composite, bipolar analog composite,
digital composite and digital separate semantics; only the separate variant
exposes independent H/V polarities. These are decoded values, not an MMIO or
wire-layout overlay. `Decode` proves each porch plus pulse fits its blanking
interval. Hardware admission must still decide which sync kinds it supports.

VESA A2 table 3.22 defines bit 0 as don't-care when stereo bits 6:5 are zero;
both encodings are accepted. Interlaced and stereo modes remain unsupported.
Cross-check: Linux v6.16 `drm_mode_detailed` in
`drivers/gpu/drm/drm_edid.c` retains the same offset/pulse fields. CuBit continues
to reject invalid sync geometry rather than adopting Linux's clamping behavior.

The native virtio driver negotiates EDID support, validates completion identity
and full response length, bounds the declared blob length, snapshots its base
block, and chooses the preferred virtual resource size only when two separately
page-aligned buffers fit its owned 8 MiB DMA bank. Its ring/command region is
64 KiB. The current native UI policy admits at least 800x600. Smaller preferences
(including GTK's startup 640x480 placeholder) retain 1024x768. The boot-console
head also retains its existing mode when EDID differs from the firmware geometry:
the broker's boot mapping must be retired safely before that transition can be
supported. Other heads can choose different bounded modes. The virtual driver
does not program a physical panel's timing or infer permission from EDID.

This first decoder does **not** validate extension blocks, enumerate all modes,
decode DisplayID/CTA, authenticate a monitor identity, or calculate safe physical
link settings. A future Intel backend must intersect parsed timings with its own
connector/link/PLL limits before modesetting. EDID physical size informs an
optional DPI suggestion; it must never override explicit UI-scale preference.

References: [VESA E-EDID 1.4, sections 3.10 and 3.12](https://glenwing.github.io/docs/VESA-EEDID-A2.pdf),
and the [OASIS virtio GPU protocol](https://github.com/oasis-tcs/virtio-spec/blob/master/device-types/gpu/description.tex).
Implementation is Ada/SPARK, not imported C driver code.

Run in Nix:

```
make -C kernel test-monitor-edid prove-monitor-edid
```

Hosted tests cover a known 720p60 timing, physical-size fields, all 1,024
single-bit corruptions, unsupported/malformed timing cases, and all 16,769,025
pairs of legal DTD extents for page-rounded storage. The strict proof audit
requires full SPARK coverage, no skipped/assumed obligations, absence of runtime
errors and the buffer-size/alignment postcondition. It does not prove EDID
standards completeness, device correctness or the DMA adapter.

Additional sync regressions cover all 256 descriptor flag bytes and maximal
10-bit horizontal / 6-bit vertical offset and pulse fields. The isolated
2026-10-06 Nix run passed hosted tests and the strict proof audit (35 proof
diagnostics, no skips or assumptions), including the new decoded-geometry
postcondition. This does not validate a physical monitor or set a native mode.
The existing virtio-GPU `main.adb` consumer also compiled with GNAT16 against
the expanded record in an isolated object directory; that was a compile check,
not a native link or boot test.

Native QEMU/KVM tests additionally exercise actual 1024x768 + 1280x720 scanouts:

```
CUBIT_TEST_MIXED_OUTPUTS=1 tests/headless/run.sh --test desktop-dual-output \
  --accel kvm --cpus 4 --timeout 50 --serial /tmp/mixed.log --keep-logs
```

The observer checks image dimensions, split-window pixels, per-output maximize,
pointer confinement below the short output, and exact cursor/window restoration.
It does not measure photon timing or establish physical hardware compatibility.

QEMU GTK synthesizes EDID from the same mutable window geometry used for display
hints; it can replace requested per-head preferences before the guest starts.
The mixed-mode fixture therefore runs without a graphical frontend. A separate
GTK fixture checks stable fallback on both heads and the native Settings page.
This is not yet an interactive mode selector: runtime mode requests and CCL
Config policy are the next layer. See QEMU's
[EDID generation and UI-info handling](https://github.com/qemu/qemu/blob/master/hw/display/virtio-gpu-base.c).
