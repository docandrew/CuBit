#!/usr/bin/env bash
# Hosted tests and SPARK proofs of the desktop clipboard (docs/clipboard.md).
# Run inside nix develop:
#   tests/clipboard/run.sh            build and run the hosted tests
#   tests/clipboard/run.sh --prove    prove the units at level 2
#   tests/clipboard/run.sh --mutate   mutation checks (tests/clipboard/mutations.sh)
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
runtime="$root/userspace/runtime/gnat"
mkdir -p "$here/build/source"
for f in cubit.ads cubit-desktop_protocol.ads cubit-desktop_protocol.adb \
         cubit-grant_references.ads; do
  cmp -s "$runtime/$f" "$here/build/source/$f" || cp "$runtime/$f" "$here/build/source/$f"
done
proved_units="clipboard_store.adb clipboard_gestures.adb clipboard_menu.adb compositor_clipboard_protocol.adb clipboard_chords.adb clipboard_text.adb"
cd "$root/kernel"
case "${1:-}" in
  --prove)
    alr exec -- gnatprove -P "$here/prove.gpr" --mode=all -u $proved_units ;;
  --mutate)
    bash "$here/mutations.sh" ;;
  *)
    alr exec -- gprbuild -p -q -P "$here/tests.gpr"
    "$here/build/clipboard_tests"
    alr exec -- gprbuild -p -q -P "$here/editor_keys.gpr"
    "$here/build/editor_keys_tests" ;;
esac
