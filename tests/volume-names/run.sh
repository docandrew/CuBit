#!/usr/bin/env bash
#  Hosted tests for the filesystem service's volume names, including the
#  fixed "@boot" and "@cd:0" stores; with --prove gnatprove. Run in the Nix
#  shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/volume_names.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/volume_names.gpr" -u volume_list.adb \
        --level=2 -j4 --checks-as-errors=on
fi
