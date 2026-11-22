#!/usr/bin/env bash
set -euo pipefail
: "${IN_NIX_SHELL:?Run inside nix develop}"
here=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$here/build/source"
for name in cubit.ads cubit-audio_ring.ads; do
  cp "$here/../../userspace/runtime/gnat/$name" "$here/build/source/$name"
done
gprbuild -p -P "$here/ring.gpr"
"$here/build/main"
if [[ ${1:-} == --negative ]]; then
  python3 "$here/check-old-geometry.py"
fi
