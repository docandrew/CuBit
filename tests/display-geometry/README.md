# Shared multi-output geometry foundation

```
nix develop -c make -C kernel test-display-geometry prove-display-geometry
```

`CuBit.Display_Geometry` is a portable SPARK core, not a second display protocol
or a native multi-monitor compositor. The existing single-output rendering path
is unchanged. Later output descriptors and the toolkit will use this same core.

An output has native pixel dimensions, independent rational UI scale, clockwise
content rotation, and signed desktop origin. Placement and rotation are separate:
a monitor can sit above/left of another and independently use portrait content.
Four rotations and numerator/denominator values 1..16 are representable; these
are numeric bounds, not the eventual Settings choices. Modes are bounded to
65535 pixels per axis and origins to +/-16777216 logical units. Admission must
still enforce actual hardware limits, buffer budgets and configuration authority.

Logical bounds round the final unit outward when dimensions are not divisible
by scale. Damage intersects that viewport, scales outward conservatively, then
rotates into native storage and clips to real pixels. Empty/inverted damage
returns empty. No desktop-sized allocation is implied by gaps between outputs.

`To_Desktop` inverse-maps native pixel centers to integer logical **hit-test**
coordinates. It is not a relative-motion integrator: preserve subpixel pointer
position separately and do not feed these rounded coordinates back into cursor
motion. Likewise outward damage rectangles are not widget layout rounding.
Rendering shared edges and smooth cross-output pointer motion remain integration
work, rather than being silently approximated by this first core.

## Evidence

The test checks 191488 native pixels across all four rotations and all 256
scale fractions, with negative origins. Each input pixel must be covered by
redrawing its mapped logical unit. Independent corner tests establish rotation
direction; other cases cover fractional last units, partial/outside/inverted
damage, maximum dimensions, extreme origins and invalid input coordinates.

GNATprove proves arithmetic/range/division checks, initialization/termination,
ordered in-output damage bounds, and containment of successful input mappings.
The proof audit requires nonempty coverage and rejects assumptions/skips. The
pixel-to-damage containment relationship is regression-tested, not yet expressed
as a universally proved functional property. No proof of compositor, GPU DMA,
input delivery or monitor hardware is implied.

## Native QEMU multi-output discovery

```
nix develop -c make -C kernel virtio-gpu
nix develop -c bash tests/headless/run.sh --test virtio-gpu-multi-output --accel kvm --cpus 1 --timeout 25 --keep-logs
```

Verified with the Nix QEMU 11.1 build on 2026-09-21. One virtio GPU advertises
three enabled scanouts, 1024x768, 1920x1080 and 1080x1920. The test checks the
actual guest GET_DISPLAY_INFO response and successful scanout-zero startup.
Per-head modes use QEMU's `outputs` device property; merely setting
`max_outputs=3` does not enable three useful modes.

This is **discovery**, not three independently rendered CuBit desktops. The
driver still creates/presents resources for scanout zero only. A portrait-shaped
mode does not test software rotation; that is covered by the geometry suite.
Scale and desktop placement are CuBit policy, not host-window zoom settings.

Next: generation-bound output/session registry, output-local resource lifetime,
then compositor/toolkit adoption and a real three-output presentation test.
Hot-unplug, reconfiguration failure, stale input/frame generations, separate
refresh queues and multi-adapter copying need additional fixtures. QEMU cannot
establish real panel timing or physical input-to-photon guarantees.
