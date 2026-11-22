#!/usr/bin/env bash
# Hosted test and SPARK proof of logstore's channel choice (Publisher_Slots).
# Run inside the Nix shell: nix develop -c bash tests/log-publisher-slots/run.sh
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
cd "$here/../../kernel"
alr exec -- gprbuild -q -p -P "$here/slots.gpr"
"$here/build/main"
alr exec -- gnatprove -P "$here/prove.gpr" -u publisher_slots.adb slots_proof.ads \
    --level=2 --report=fail --checks-as-errors=on -j2
