#!/usr/bin/env bash
# Hosted tests of other CuBit.UI users, rebuilt into this test's own tree
# (build/toolkit) so their checked-in build directories are not touched.
# Run inside nix develop after changing a toolkit unit.
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
out="$here/build/toolkit"
mkdir -p "$out"
cd "$root/kernel"
status=0
build() {  # project, then mains to run
  local project=$1; shift
  alr exec -- gprbuild -p -q -j4 -P "$root/$project" --relocate-build-tree="$out" --root-dir="$root" || { echo "BUILD FAIL $project"; status=1; return; }
  local dir="$out/$(dirname "$project")/build"
  for main in "$@"; do
    (cd "$root/$(dirname "$project")" && "$dir/$main" "$out/$main.ppm" > "$out/$main.log" 2>&1) &&
      echo "PASS $project $main: $(tail -1 "$out/$main.log")" ||
      { echo "FAIL $project $main: $(tail -3 "$out/$main.log")"; status=1; }
  done
}
build tests/ui-polish/polish.gpr polish_tests combo_tests
build tests/ui-menus/menus.gpr menus_tests
build tests/log-viewer/log_viewer_tests.gpr log_viewer_tests
exit $status
