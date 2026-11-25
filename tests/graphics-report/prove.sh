#!/usr/bin/env bash
# SPARK proof (level 2, all checks as errors) of the renderer report
# (GPU-014): CuBit.Graphics_Report (the wire) and Devmgr_Graphics_Report
# (devmgr's copy). Inside nix develop; memory capped.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
cd "$root/kernel"
exec systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
  alr exec -- gnatprove -P "$here/prove.gpr" --level=2 --report=fail --checks-as-errors=on --counterexamples=off -j2 \
  -u cubit-graphics_report.adb -u devmgr_graphics_report.adb
