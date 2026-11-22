#!/usr/bin/env bash
# WEB-001 compatibility survey, one command (from the repository root, on
# the measurement host, after `make -C kernel servo` and
# userspace/servo/build-cubitshell-hosted.sh):
#
#     tests/servo/compat/run-survey.sh OUT [run-survey.py options]
#
# Runs every engine (run-survey.py), then the analysis (analyze.py): the
# table, failure causes and gallery land in OUT/report. The survey tools
# come from tools.nix (the repository's pinned nixpkgs).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
out=${1:?usage: run-survey.sh OUT [run-survey.py options]}; shift
tools=$(nix build --no-link --print-out-paths -f "$here/tools.nix")
browsers=$(nix build --no-link --print-out-paths -f "$here/tools.nix" playwrightBrowsers)
export PATH=$tools/bin:$PATH PLAYWRIGHT_BROWSERS_PATH=$browsers CHROMIUM_EXECUTABLE=$tools/bin/chromium
export PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=true
. "$here/wpe-env.sh"
python3 "$here/run-survey.py" --out "$out" "$@"
python3 "$here/analyze.py" "$out"
