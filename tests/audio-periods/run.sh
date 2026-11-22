#!/usr/bin/env bash
set -euo pipefail
: "${IN_NIX_SHELL:?Run inside nix develop}"
here=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$here/build/source"
for name in cubit.ads cubit-audio_periods.ads cubit-audio_periods.adb; do
  cp "$here/../../userspace/runtime/gnat/$name" "$here/build/source/$name"
done
gprbuild -p -P "$here/periods.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
  gnatprove -P "$here/periods.gpr" -u cubit-audio_periods.adb --mode=all --level=2 -j2 --subdirs=proof
fi
