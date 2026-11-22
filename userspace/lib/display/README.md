# Shared display models

Architecture-neutral geometry, layout admission, placement, output lifetime and
discovery code lives here, not in the GNAT runtime. Native consumers include this
source directory and compile only their dependencies with their normal optimized
application switches. These units are not compiler runtime implementation units.

Hosted tests and SPARK proof targets stage the same sources with a minimal CuBit
parent package; they must not put the freestanding GNAT runtime on the host's
source path. The existing `test/prove-display-geometry`, `display-layouts`,
`window-placement`, `output-registry`, and `output-discovery` Makefile targets
exercise this boundary.

Geometry and layouts describe policy-neutral coordinates. Desktop owns layout,
primary-display and window-placement policy; the display broker owns output
sessions; hardware drivers own scanout and supported modes. A monitor identity
or EDID is descriptive input, never an authority or a lifetime token.

`CuBit.Display_Arrangement` supplies the Settings editor's pure edge-snapping
operation. It proposes origins, preserves output identity and modes, and admits
only connected layouts through the shared validator. Its hosted regression and
SPARK proof targets are documented in
[`tests/display-layouts`](../../../tests/display-layouts/README.md).

The IPC protocol definitions and other CuBit support libraries still in
`runtime/gnat` are a separate, incremental extraction. This move does not change
wire formats, rendering, or the scope of existing proofs.

New monitor metadata lives here too: `CuBit.Monitor_EDID` provides bounded
preferred-timing decoding and page-rounded buffer sizing. See
[`tests/monitor-edid`](../../../tests/monitor-edid/README.md) for its intentionally
limited standards scope, hosted proof/test evidence, and native integration.
