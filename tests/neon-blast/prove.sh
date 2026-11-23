#!/usr/bin/env bash
# SPARK proof (level 2, all checks as errors) of Neon Blast's proved units:
# Neon_Keys and Neon_Random. Inside nix develop; memory capped.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
cd "$root/kernel"
exec systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
  alr exec -- gnatprove -P "$here/prove.gpr" --level=2 --report=fail --checks-as-errors=on -j2
