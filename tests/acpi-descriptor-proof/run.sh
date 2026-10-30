#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
task_root=$(cd "$task_here/../.." && pwd -P)
task_manifest="$task_here/source-manifest.json"
task_hash=$(sha256sum "$task_manifest"); task_hash=${task_hash%% *}
verify_inputs () {
 local task_actual
 task_actual=$(sha256sum "$task_manifest"); test "${task_actual%% *}" = "$task_hash"
 jq -e 'length > 4 and all(to_entries[]; (.value|test("^[0-9a-f]{64}$")) and (.key|startswith("/")|not) and (.key|split("/")|all(. != ".." and . != "." and length>0)) and (.key|test("[\\\\\n\r]")|not))' "$task_manifest" >/dev/null
 (cd "$task_root"; jq -r 'to_entries[]|.value+"  "+.key' "$task_manifest" | sha256sum --check --quiet --strict)
}
verify_inputs
if test "${1:-}" = --validate-only; then test "$#" = 1; printf 'Descriptor proof source/recipe pins verified; no proof run.\n'; exit 0; fi
test "$#" = 1
task_phase=$1
case "$task_phase" in check|flow|prove) ;; *) exit 2;; esac
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
test -n "${IN_NIX_SHELL:-}"
test "$(ps -o ni= -p $$)" -ge 19
task_prover=$(realpath "$(command -v gnatprove)")
case "$task_prover" in /nix/store/*/bin/gnatprove) ;; *) exit 2;; esac
mkdir -p "$task_here/results"
task_output=$(mktemp -d "$task_here/results/$task_phase-XXXXXXXX")
mkdir "$task_output/obj"
cp "$task_manifest" "$task_output/inputs.json"
sha256sum "$task_prover" > "$task_output/prover.sha256"
task_command=("$task_prover" -P "$task_here/descriptor_proof.gpr" "-XDESCRIPTOR_PROOF_OBJECTS=$task_output/obj" -u acpi_resource_data.adb acpi_resource_data-basic_descriptors.adb acpi_resource_data-extended_interrupts.adb acpi_resource_data-address_spaces.adb "--mode=$task_phase" -j1 --report=all)
if test "$task_phase" = prove; then task_command+=(--level=1 --prover=cvc5,z3 --timeout=5); fi
jq -n --args '$ARGS.positional' -- "${task_command[@]}" > "$task_output/command.json"
printf '%s\n' "$task_output"
ulimit -s 65536
task_status=0
python3 "$task_here/capture_phase.py" "$task_output/$task_phase.log" "${task_command[@]}" || task_status=$?
verify_inputs
sha256sum --check --quiet --strict "$task_output/prover.sha256"
jq -n --argjson status "$task_status" --arg phase "$task_phase" '{process_status:$status,phase:$phase,accepted:false,scope:"Process status only; inspect proof results and dependencies before accepting."}' > "$task_output/process.json"
exit "$task_status"
