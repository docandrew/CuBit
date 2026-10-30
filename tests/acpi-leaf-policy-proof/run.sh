#!/usr/bin/env bash
set -euo pipefail
here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
root=$(cd "$here/../.." && pwd)
cd "$root"
sha256sum --check --quiet "$here/inputs.sha256"
if [ "${1:-}" = --validate-only ]; then printf '%s\n' 'Leaf proof inputs verified; no prover.'; exit 0; fi
test "$#" = 0
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
mkdir -p "$here/results"
out=$(mktemp -d "$here/results/run-XXXXXXXX")
printf '%s\n' "$out"
tool_path=$(realpath "$(command -v gnatprove)")
case "$tool_path" in /nix/store/*) ;; *) exit 1;; esac
sha256sum "$tool_path" > "$out/tool.sha256"
for unit in acpi_battery_status hid_i2c_wire; do
 build="$out/$unit"
 mkdir -p "$build/obj"
 nice -n 19 timeout --kill-after=2s 180 gnatprove -P "$here/proof.gpr" "-XACPI_LEAF_PROOF_BUILD=$build" -u "$unit.adb" --mode=prove --level=2 --timeout=5 -j1 --report=all 2>&1 | head -c 1048576 > "$out/$unit.log"
done
bash "$here/audit.sh" "$out"
sha256sum --check --quiet "$here/inputs.sha256"
find "$out" -type f ! -name artifacts.sha256 -print0 | sort -z | xargs -0 sha256sum > "$out/artifacts.sha256"
