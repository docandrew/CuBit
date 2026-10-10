#!/usr/bin/env bash
#  Hosted tests for the clock publication (shared/time); --prove also runs
#  gnatprove at level 2 on the shared unit and the seqlock model.
#  Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/fast_clock.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/fast_clock.gpr" \
        -u clock_publication.adb -u seqlock_model.adb -u sample_instance.ads \
        --level=2 -j4 --checks-as-errors=on
fi
