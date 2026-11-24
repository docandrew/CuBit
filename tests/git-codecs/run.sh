#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
#  SELF-003 step 2: tests of the git codecs (userspace/lib/git): pkt-lines,
#  the protocol v2 answers, packs, deltas, object names and trees, against
#  the host's git. Builds the driver as services build (-gnatp -O3) for
#  every case, and with -gnata for a quick subset with every contract
#  checked. --prove adds gnatprove level 2 (no unproved check, nothing
#  justified). Run in the Nix shell, proofs under a memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/git-codecs/run.sh --prove
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/git_codecs_fast.gpr"
alr exec -- gprbuild -p -q -P "$here/git_codecs_tests.gpr"
python3 -I "$here/check.py" "$here/build/git_tool_fast" "$here/build/work"
python3 -I "$here/check.py" "$here/build/git_tool" "$here/build/work-checked" --quick
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/git_codecs_tests.gpr" -u git_packets.adb -u git_protocol.adb -u git_packs.adb \
        -u git_deltas.adb -u git_objects.adb --level=2 -j2 --checks-as-errors=on --report=fail 2>&1 |
        tee "$here/build/proof.log"
    src=${GIT_SOURCES:-$here/../../userspace/lib/git}
    if grep -qE "(medium|high|error|warning):" "$here/build/proof.log" ||
       grep -qs "pragma Annotate\|Justif\|pragma Assume" "$src"/*.ads "$src"/*.adb; then
        echo "git-codecs: proof FAILED" >&2
        exit 1
    fi
    echo "git-codecs: proof OK"
fi
