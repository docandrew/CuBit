#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
#  SELF-003 step 2: tests of the inflater (userspace/lib/compress). Builds
#  the driver twice: as services build (-gnatp -O3), which runs every
#  case, and with -gnata, which runs a quick subset with every contract
#  checked on every call. check.py makes all inputs at test time (the
#  host's zlib, and streams written bit by bit). --prove adds gnatprove
#  level 2 (no unproved check, nothing justified). Run in the Nix shell,
#  proofs under a memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/inflate/run.sh --prove
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/inflate_fast.gpr"
alr exec -- gprbuild -p -q -P "$here/inflate_tests.gpr"
rm -rf "$here/build/cases" "$here/build/cases-checked"
python3 -I "$here/check.py" "$here/build/inflate_tool_fast" "$here/build/cases" --fuzz "${INFLATE_FUZZ:-20000}"
python3 -I "$here/check.py" "$here/build/inflate_tool" "$here/build/cases-checked" --fuzz 200 --quick
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/inflate_tests.gpr" -u inflate_streams.adb --level=2 -j2 \
        --checks-as-errors=on --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error|warning):" "$here/build/proof.log" ||
       grep -qs "pragma Annotate\|Justif\|pragma Assume" "${INFLATE_SOURCES:-$here/../../userspace/lib/compress}/inflate_streams.ads" \
         "${INFLATE_SOURCES:-$here/../../userspace/lib/compress}/inflate_streams.adb"; then
        echo "inflate: proof FAILED" >&2
        exit 1
    fi
    echo "inflate: proof OK"
fi
