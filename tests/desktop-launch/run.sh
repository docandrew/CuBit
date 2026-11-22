#!/usr/bin/env bash
# Hosted tests of the desktop Apps menu model and typed launch settings.
# Run inside nix develop. Objects go under this directory only.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
cd "$root/kernel"
alr exec -- gprbuild -p -q -j8 -P "$here/desktop_launch_tests.gpr" \
  --relocate-build-tree="$here/build/tree" --root-dir="$root"
"$here/build/tree/tests/desktop-launch/build/desktop_launch_test"
