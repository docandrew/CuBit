#!/usr/bin/env bash
# SPARK proof (level 2, all checks as errors) of Neon Blast's dynamic
# resolution: Neon_Resolution (the controller) and Neon_Upscale (the
# present scaler), built with -gnatp natively on this proof. Inside nix
# develop; memory capped.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
cd "$root/kernel"
exec systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
  alr exec -- gnatprove -P "$here/prove.gpr" --level=2 --report=fail --checks-as-errors=on --counterexamples=off -j2 \
  -u neon_resolution.adb -u neon_upscale.adb
