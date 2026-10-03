#!/usr/bin/env bash
# Hosted CCL console tests, plus the golden highlighter vectors the browser
# Observatory is checked against. Run inside nix develop from kernel/.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$here/build"
alr exec -- gprbuild -q -p -P "$here/console_tests.gpr"
(cd "$here" && ./build/main)
golden="$here/../../userspace/ccl/tools/ccl-observatory/highlight-vectors.json"
"$here/build/vectors" > "$here/build/highlight-vectors.json"
if ! cmp -s "$here/build/highlight-vectors.json" "$golden"; then
  echo "FAIL: CCL.Highlighting changed; regenerate $golden and run the Observatory tests" >&2
  diff "$golden" "$here/build/highlight-vectors.json" | head -20 >&2
  exit 1
fi
echo "PASS: browser highlighter vectors match CCL.Highlighting"
units="$here/../../userspace/ccl/tools/ccl-observatory/units-vectors.json"
"$here/build/units_vectors" > "$here/build/units-vectors.json"
if ! cmp -s "$here/build/units-vectors.json" "$units"; then
  echo "FAIL: CCL.Units changed; regenerate $units and run the Observatory tests" >&2
  diff "$units" "$here/build/units-vectors.json" | head -20 >&2
  exit 1
fi
echo "PASS: browser unit vectors match CCL.Units"
python3 "$here/check_interface_keys.py"
