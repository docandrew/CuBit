#!/usr/bin/env bash
# Hosted tests and SPARK proofs of the toolkit's keys (UI-049, UI-051,
# UI-052). Run inside nix develop, TMPDIR outside /tmp:
#   tests/ui-keys/run.sh            build and run the hosted tests
#   tests/ui-keys/run.sh --prove    prove the policy units at level 2
# (the proof under the memory cap: systemd-run --user --scope -p MemoryMax=24G ...)
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
# Desktop's environment keymap (its Super chords), tested beside the toolkit's.
mkdir -p "$here/build/source"
for unit in desktop_super_keys.ads desktop_super_keys.adb; do
  cmp -s "$root/userspace/services/desktop/$unit" "$here/build/source/$unit" ||
    cp "$root/userspace/services/desktop/$unit" "$here/build/source/"
done
cd "$root/kernel"
case "${1:-}" in
  --prove)
    alr exec -- gnatprove -P "$here/prove.gpr" -j2 \
      -u cubit-ui-keys.adb cubit-ui-modifier_reveal.adb cubit-ui-button_bar_layout.adb keymap_instance.ads ;;
  *)
    alr exec -- gprbuild -p -q -P "$here/tests.gpr"
    "$here/build/keys_tests" ;;
esac
