#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
#  SELF-003 step 1: hosted tests of net.fetch's URL and HTTP/1.1 response
#  codecs (userspace/lib/fetch); --prove adds gnatprove level 2 (no
#  unproved check, nothing justified). Run in the Nix shell, proofs under a
#  memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/fetch-codecs/run.sh --prove
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/fetch_codecs_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/fetch_codecs_tests.gpr" -u fetch_urls.adb -u http_responses.adb \
        --level=2 -j2 --checks-as-errors=on --report=fail 2>&1 | tee "$here/build/proof.log"
    lib=$here/../../userspace/lib/fetch
    if grep -qE "(medium|high|error|warning):" "$here/build/proof.log" ||
       grep -qs "pragma Annotate\|Justif" "$lib"/*.ad?; then
        echo "fetch-codecs: proof FAILED" >&2
        exit 1
    fi
    echo "fetch-codecs: proof OK"
fi
