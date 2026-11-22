#!/usr/bin/env bash
# Hosted tests and SPARK proof of desktop keyboard auto-repeat
# (userspace/services/desktop/key_repeat.ads). Run inside nix develop:
#   tests/key-repeat/run.sh            state machine + typed setting tests
#   tests/key-repeat/run.sh --prove    prove Key_Repeat at level 2
#   tests/key-repeat/run.sh --mutate   mutation checks (mutations.sh)
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
cd "$root/kernel"
case "${1:-}" in
  --prove)
    alr exec -- gnatprove -P "$here/prove.gpr" --mode=all -u key_repeat.adb ;;
  --mutate)
    bash "$here/mutations.sh" ;;
  *)
    alr exec -- gprbuild -p -q -P "$here/tests.gpr"
    "$here/build/key_repeat_tests"
    alr exec -- gprbuild -p -q -j8 -P "$here/settings.gpr" \
      --relocate-build-tree="$here/build/tree" --root-dir="$root"
    "$here/build/tree/tests/key-repeat/build/settings/settings_tests" ;;
esac
