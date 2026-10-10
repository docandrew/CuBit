#!/usr/bin/env bash
# Copy the freestanding display plane units and their pure dependencies for
# hosted tests and proofs; outputs stay under build/source. CUBIT_ROOT names
# the checkout providing the runtime units (default: this checkout).
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
RUNTIME_ROOT="${CUBIT_ROOT:-$ROOT}"
mkdir -p "$HERE/build/source"
cp "$RUNTIME_ROOT"/userspace/runtime/gnat/cubit.ads \
   "$RUNTIME_ROOT"/userspace/runtime/gnat/cubit-display_protocol.ad? \
   "$RUNTIME_ROOT"/userspace/runtime/gnat/cubit-desktop_protocol.ad? \
   "$RUNTIME_ROOT"/userspace/runtime/gnat/cubit-grant_references.ads \
   "$RUNTIME_ROOT"/userspace/lib/display/cubit-display_pool_protocol.ad? \
   "$ROOT"/userspace/lib/display/cubit-display_planes.ad? \
   "$ROOT"/userspace/lib/display/cubit-display_plane_protocol.ad? \
   "$ROOT"/userspace/lib/display/cubit-gpu_plane_protocol.ad? \
   "$HERE/build/source/"
