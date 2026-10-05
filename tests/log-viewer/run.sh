#!/usr/bin/env bash
# Run inside nix develop: hosted tests of the Logs app's view.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
cd "$here/../../kernel"
mkdir -p "$here/build/source"
for unit in cubit.ads cubit-protocols.ads cubit-log_records.ads cubit-log_records.adb cubit-log_protocol.ads; do
  cp ../userspace/runtime/gnat/$unit "$here/build/source/"
done
alr exec -- gprbuild -p -q -P "$here/log_viewer_tests.gpr"
cd "$here" && build/log_viewer_tests
