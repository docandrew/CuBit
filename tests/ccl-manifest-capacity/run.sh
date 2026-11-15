#!/usr/bin/env bash
#  CCL storage sized by its input (no fixed program limit), hosted:
#  - capacity_main: the large-manifest-probe manifest with its schema, as the
#    manifest compiler evaluates it, is analysed, compiled and verified past
#    the rooms the engine used to stop at; a plain program twenty times past
#    them runs to its value;
#  - the manifest compiler compiles that manifest and Desktop's typed one;
#  - a manifest over a named ceiling is refused with its typed capacity
#    error, never truncated.
#  Run in the Nix shell from anywhere; builds make -C kernel ccl-manifest.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
cd "$root/kernel"
make -s ccl-manifest >/dev/null
alr exec -- gprbuild -p -q -P "$here/capacity_tests.gpr"
schema="$root/userspace/ccl/interfaces/executable-manifest.ccl"
"$here/build/capacity_main" "$schema" "$root/tests/large-manifest-probe/manifest.ccl"
python3 "$here/test-capacity.py"
