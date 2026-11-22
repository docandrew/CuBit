#!/usr/bin/env bash
# Build and run the Icon_Edge_Choice hosted tests (inside nix develop).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
gprbuild -q -p -P "$here/icon_edge_choice.gpr"
"$here/build/icon_edge_choice_tests"
