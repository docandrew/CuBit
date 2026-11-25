#!/usr/bin/env bash
# Linux-hosted checks of the renderer report (GPU-014): the wire round trip,
# posts replacing their subject, stale entries, pages and refusals. Nothing
# here runs on CuBit. Inside nix develop with TMPDIR outside /tmp.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
(cd "$root/kernel" && alr exec -- gprbuild -p -q -j8 -P "$here/graphics_report_host.gpr")
"$here/build/graphics_report_tests"
