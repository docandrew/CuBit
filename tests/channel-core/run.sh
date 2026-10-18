#!/usr/bin/env bash
#  Hosted differential tests for the channel core (CAP-002 step 3); --prove
#  also runs gnatprove (level 2). Run in the Nix shell; proofs under
#  systemd-run MemoryMax (README).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/channel_core.gpr"
"$here/build/main" "${SEED:-20261017}"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/channel_core.gpr" -u core_instances.ads -u wake_handshake_model.adb \
        -u cubit-bell_sets.adb -u cubit-channel_admission.adb \
        -u cubit-channel_schedule.adb -u cubit-request_tags.adb -u cubit-channel_declarations.adb
fi
