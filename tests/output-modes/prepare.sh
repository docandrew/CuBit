#!/usr/bin/env bash
# Copy the freestanding output-mode units and their pure dependencies for
# hosted tests and proofs; outputs stay under build/source.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
mkdir -p "$HERE/build/source"
cp "$ROOT"/userspace/runtime/gnat/cubit.ads \
   "$ROOT"/userspace/runtime/gnat/cubit-desktop_protocol.ad? \
   "$ROOT"/userspace/runtime/gnat/cubit-grant_references.ads \
   "$ROOT"/userspace/lib/display/cubit-output_discovery.ad? \
   "$ROOT"/userspace/lib/display/cubit-output_modes.ad? \
   "$ROOT"/userspace/lib/display/cubit-display_geometry.ad? \
   "$ROOT"/userspace/lib/display/cubit-display_layouts.ad? \
   "$ROOT"/userspace/lib/compositor/compositor_layout_resize.ad? \
   "$HERE/build/source/"
