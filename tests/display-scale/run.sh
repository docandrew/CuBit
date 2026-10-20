#!/usr/bin/env bash
# Hosted tests and SPARK proof of the automatic display scale
# (userspace/services/desktop/desktop_auto_scale.ads) and the typed
# desktop.display.scale and desktop.display.mode.<monitor> settings. Run
# inside nix develop:
#   tests/display-scale/run.sh           selection + typed setting tests
#   tests/display-scale/run.sh --prove   prove Desktop_Auto_Scale and
#                                         Desktop_Mode_Trial at level 2
#                                         (under systemd-run MemoryMax=24G)
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
cd "$root/kernel"
case "${1:-}" in
  --prove)
    alr exec -- gnatprove -P "$here/prove.gpr" --mode=all -u desktop_auto_scale.adb
    mkdir -p "$here/build/protocol"
    cp "$root"/userspace/runtime/gnat/cubit-desktop_protocol.ad? \
       "$root"/userspace/runtime/gnat/cubit-grant_references.ads "$here/build/protocol/"
    alr exec -- gnatprove -P "$here/prove_trial.gpr" --mode=all -u desktop_mode_trial.adb ;;
  *)
    alr exec -- gprbuild -p -q -P "$here/tests.gpr"
    "$here/build/auto_scale_tests"
    mkdir -p "$here/build/protocol"
    cp \
       "$root"/userspace/runtime/gnat/cubit-desktop_protocol.ad? \
       "$root"/userspace/runtime/gnat/cubit-grant_references.ads "$here/build/protocol/"
    alr exec -- gprbuild -p -q -j8 -P "$here/settings.gpr" \
      --relocate-build-tree="$here/build/tree" --root-dir="$root"
    "$here/build/tree/tests/display-scale/build/settings/settings_tests"
    "$here/build/tree/tests/display-scale/build/settings/mode_settings_tests" ;;
esac
