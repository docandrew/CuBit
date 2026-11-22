#!/usr/bin/env bash
# Run inside nix develop. No native output or shared build lock required.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root"
source_tree=${1:?prepared Mesa source directory required}
test_parent=$(mktemp -d "${TMPDIR:-/tmp}/cubit-mesa-notices.XXXXXX")
bash userspace/mesa/stage-notices.sh "$source_tree" "$test_parent/mesa"
diff -r "$source_tree/licenses" "$test_parent/mesa/licenses"
cmp "$source_tree/docs/license.rst" "$test_parent/mesa/UPSTREAM-LICENSE.rst"
cmp "$source_tree/VERSION" "$test_parent/mesa/MESA-VERSION"
cmp tests/mesa-anv/source.nix "$test_parent/mesa/SOURCE.nix"
cmp tests/mesa-software/cubit-platform.patch "$test_parent/mesa/CUBIT-PLATFORM.patch"
cmp userspace/mesa/NOTICE.md "$test_parent/mesa/README.md"
tar -xOf "$test_parent/mesa/MESA-SOURCE.tar.gz" ./docs/license.rst |
    cmp "$source_tree/docs/license.rst" -
tar -xOf "$test_parent/mesa/MESA-SOURCE.tar.gz" ./src/util/detect_os.h |
    cmp "$source_tree/src/util/detect_os.h" -
if bash userspace/mesa/stage-notices.sh "$source_tree" "$test_parent/mesa"; then
    echo 'FAIL: existing destination accepted' >&2
    exit 1
fi
if bash userspace/mesa/stage-notices.sh "$test_parent/missing" "$test_parent/bad"; then
    echo 'FAIL: missing source accepted' >&2
    exit 1
fi
test ! -e "$test_parent/bad"
echo "PASS: exact copies, overwrite and missing-source rejection ($test_parent)"
