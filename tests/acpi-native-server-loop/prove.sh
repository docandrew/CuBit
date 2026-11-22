#!/usr/bin/env bash
set -euo pipefail
here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
bash "$here/run.sh" --validate-only
if [ "${1:-}" = --validate-only ]; then test "$#" = 1; exit 0; fi
test "$#" = 0
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
mkdir -p "$here/results"
out=$(mktemp -d "$here/results/policy-proof-XXXXXXXX")
printf '%s\n' "$out"
path=$(realpath "$(command -v gnatprove)")
case "$path" in /nix/store/*) ;; *) exit 1;; esac
sha256sum "$path" > "$out/tool.sha256"
nice -n 19 timeout --kill-after=2s 180 gnatprove -P "$here/observer_policy.gpr" "-XOBSERVER_PROOF_BUILD=$out" -u acpi_observer_endpoint.adb --mode=all --level=2 --timeout=5 --memlimit=2000 --prover=cvc5,z3 --checks-as-errors=on -j1 2>&1 | head -c 1048576 > "$out/proof.log"
bash "$here/run.sh" --validate-only
printf '%s\n' 'Scoped observer policy proof completed; inspect summary and assumptions, not a whole-driver claim.'
