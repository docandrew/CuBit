#!/usr/bin/env bash
#  Hosted tests for the surface ring (IPC-004): the element codec and the
#  request channel instance over in-memory regions; --prove also runs
#  gnatprove (level 2) on the codec. Run in the Nix shell; proofs under
#  systemd-run MemoryMax.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/surface_ring.gpr"
"$here/build/main" | tee "$here/build/result.txt"
grep -q '^SURFACE-RING: PASS' "$here/build/result.txt"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/surface_ring.gpr" -u compositor_surface_wire.adb
fi
