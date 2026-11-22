#!/usr/bin/env bash
#  Hosted tests for Desktop_Launch_Notice; --prove also runs gnatprove
#  (level 2). Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/desktop_launch_notice_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/desktop_launch_notice_tests.gpr" \
        -u desktop_launch_notice.adb -u cubit-launch_arguments.adb \
        -u cubit-launch_failure_text.ads -j2
fi
