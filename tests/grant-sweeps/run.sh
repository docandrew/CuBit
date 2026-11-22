#!/usr/bin/env bash
#  Hosted tests for Grant_Sweeps; --prove also runs gnatprove (level 2) on
#  the protocol model (Sweep_Model). Run in the Nix shell. Proofs belong
#  under systemd-run MemoryMax (see README).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/grant_sweeps.gpr"
"$here/build/main" "${SEED:-20261018}"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/sweep_model.gpr" \
        --level=2 -j2 --checks-as-errors=on
fi
