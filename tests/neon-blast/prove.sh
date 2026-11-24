#!/usr/bin/env bash
# SPARK proof (level 2, all checks as errors) of Neon Blast's proved units:
# Neon_Keys, Neon_Random and Neon_Style (Neon_Math, Neon_Time and
# Neon_Notes are analysed for the contracts they lend, not proved here).
# Inside nix develop; memory capped.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
cd "$root/kernel"
exec systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
  alr exec -- gnatprove -P "$here/prove.gpr" --level=2 --report=fail --checks-as-errors=on --counterexamples=off -j2 \
  -u neon_keys.adb -u neon_random.adb -u neon_style.adb
