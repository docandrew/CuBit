#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
mkdir -p ../tests/log-fanout/build/source
cp ../userspace/runtime/gnat/cubit.ads ../userspace/runtime/gnat/cubit-log_protocol.ads \
   ../userspace/runtime/gnat/cubit-authority_policy.ads \
   ../userspace/runtime/gnat/cubit-protocols.ads \
   ../userspace/runtime/gnat/cubit-log_records.ads \
   ../userspace/runtime/gnat/cubit-log_records.adb \
   ../userspace/services/logstore/log_fanout.ads ../userspace/services/logstore/log_fanout.adb \
   ../userspace/services/logstore/log_budgets.ads ../userspace/services/logstore/log_budgets.adb \
   ../tests/log-fanout/build/source/
alr exec -- gprbuild -p -P ../tests/log-fanout/fanout.gpr
../tests/log-fanout/build/main
alr exec -- gnatprove -P ../tests/log-fanout/fanout.gpr -u log_budgets.adb \
    --level=2 --report=all --checks-as-errors=on -j2
alr exec -- gnatprove -P ../tests/log-fanout/fanout.gpr -u cubit-log_protocol.ads \
    --level=2 --report=all --checks-as-errors=on -j2
alr exec -- gnatprove -P ../tests/log-fanout/fanout.gpr -u log_fanout.adb \
    --level=2 --report=all --checks-as-errors=on -j2
alr exec -- gnatprove -P ../tests/log-fanout/fanout.gpr -u policy_proof.adb \
    --level=2 --report=all --checks-as-errors=on -j2
alr exec -- gnatprove -P ../tests/log-fanout/fanout.gpr -u cubit-authority_policy.ads \
    --level=2 --report=all --checks-as-errors=on -j2
