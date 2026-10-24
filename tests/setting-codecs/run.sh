#!/usr/bin/env bash
#  Hosted tests of the generated typed-setting decoders (CCL.Setting_Codecs,
#  ccl-manifest --setting-codecs) against CCL.Typed_Settings; --prove also
#  runs gnatprove (level 2) over CCL.Canonical_Readers and the decoders
#  (under a memory cap: docs/proofs, cap-proof-memory).
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
cd "$root/kernel"
alr exec -- gprbuild -p -q -j8 -P "$here/setting_codecs_tests.gpr"
#  The CCL reference path needs a few MiB of stack per check.
(ulimit -s unlimited && "$here/build/tests/setting_codecs_tests")
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
        alr exec -- gnatprove -P "$here/prove.gpr" -j2 --level=2 --checks-as-errors=on
fi
