#!/usr/bin/env bash
#  Hosted tests of Start_Child's pure parts (SUP-001,
#  docs/otp-supervision-study.md): the spec codec, the fact/policy merge
#  checked by CCL's type checker against hid-i2c's HID_Parameters record,
#  the startup plan's (driver-class ...) blocks, and the supervision
#  edges. --prove also runs gnatprove (level 2) over the codec, the merge
#  and the edges, under a memory cap (-j2).
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
links="$here/build/links"
rm -rf "$links"
mkdir -p "$links"
for unit in userspace/runtime/gnat/cubit-child_specs.ads userspace/runtime/gnat/cubit-child_specs.adb \
            userspace/services/procmgr/child_parameters.ads userspace/services/procmgr/child_parameters.adb \
            userspace/services/procmgr/supervision_edges.ads userspace/services/procmgr/supervision_edges.adb \
            userspace/services/procmgr/class_launch_checks.ads userspace/services/procmgr/class_launch_checks.adb; do
    ln -s "$root/$unit" "$links/"
done
cd "$root/kernel"
alr exec -- gprbuild -p -q -j8 -P "$here/supervision_tests.gpr"
#  The CCL reference path needs a few MiB of stack per check.
(cd "$here" && ulimit -s unlimited && "$here/build/tests/supervision_tests")
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
        alr exec -- gnatprove -P "$here/prove.gpr" -j2 --level=2 --checks-as-errors=on
fi
