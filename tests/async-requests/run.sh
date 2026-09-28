#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/async-requests/requests.gpr
../tests/async-requests/build/main
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P ../tests/async-requests/requests.gpr \
        -u cubit-async_requests.adb --level=2 -j2 --checks-as-errors=on
fi
