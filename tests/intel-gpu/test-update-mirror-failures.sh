#!/usr/bin/env bash
# Run inside the Nix development environment. Hosted fault injection only.
set -euo pipefail
cd "$(dirname "$0")/../.."
gprbuild -P tests/intel-gpu/application_state.gpr
probe=tests/intel-gpu/build-application-state/update_mirror_failure_tests
baseline=$("$probe")
echo "$baseline"
read -r marker steps commits <<<"$baseline"
[[ "$marker" == MIRROR-BASELINE && "$steps" =~ ^[0-9]+$ && "$commits" =~ ^[0-9]+$ ]]
(( steps > 0 && steps <= 500 && commits > 0 && commits <= steps ))
for ((step=1; step<=steps; step++)); do "$probe" "$step" 0; done
for ((commit=1; commit<=commits; commit++)); do "$probe" 0 "$commit"; done
echo "Mirror matrix PASS: $steps yield boundaries and $commits commit revocations"
