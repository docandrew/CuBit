#!/usr/bin/env bash
# Linux-hosted checks of Neon Blast's dynamic resolution (APP-008). The
# game's portable units built for the host; nothing here runs on CuBit.
# Run inside nix develop with TMPDIR outside /tmp:
#   tests/neon-dynres/run.sh          controller, scaler, buffer reuse, and
#                                     the game unchanged by the render size
#   tests/neon-dynres/run.sh --bench  also each level's cost on a 4K-class
#                                     window and the closed loop (timings
#                                     of this host, not of CuBit)
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
(cd "$root/kernel" &&
   alr exec -- gprbuild -p -q -j8 -P "$here/neon_dynres.gpr" &&
   alr exec -- gprbuild -p -q -j8 -XNEON_HOST_BUILD=release -P "$here/neon_dynres.gpr")

"$here/build/dynres_tests"
"$here/build/release/dynres_replay" same 20

if [ "${1:-}" = "--bench" ]; then
  "$here/build/release/dynres_replay" cost 2560 1600 5
  "$here/build/release/dynres_replay" cost 3840 2160 5
  "$here/build/release/dynres_replay" loop 3840 2160 8 1024 640
fi
