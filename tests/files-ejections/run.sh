#!/usr/bin/env bash
# Files_Ejections (FS-034): hosted cases, then its SPARK proof.
#   tests/files-ejections/run.sh          (inside nix develop)
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
cd "$here"
gprbuild -q -p -P files_ejections_tests.gpr
build/main
gnatprove -P files_ejections_tests.gpr -u files_ejections.ads --level=2 -j2 --report=fail --checks-as-errors=on
echo "files-ejections: proof OK"
