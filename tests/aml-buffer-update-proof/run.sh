#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_root=$(cd "$task_here/../.." && pwd)
task_phase=${1:?check,flow,prove}
task_selected=${2:-store}
case "$task_selected" in store)task_symbol=Store_Buffer;;map)task_symbol=Occupancy_Map;;count)task_symbol=Occupied_Total;;item)task_symbol=Valid_Item;;records)task_symbol=Valid_Records;;grow)task_symbol=Extend_Records;;replace)task_symbol=Replace_Record;;unpack)task_symbol=Unpack_Valid;;reassemble)task_symbol=Reassemble_Valid;;frame)task_symbol=Preserve_Valid_Bytes;;eq-occ)task_symbol=Equal_Occupancy;;eq-objects)task_symbol=Equal_Objects;;eq-elements)task_symbol=Equal_Elements;;*)exit 2;;esac
case "$task_phase" in check|flow|prove) ;;*)exit 2;;esac
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
test -n "${IN_NIX_SHELL:-}"
task_output=$(mktemp -d "$task_here/result-$task_phase-XXXXXXXX")
task_manifest_hash=$(sha256sum "$task_here/source-manifest.json" | cut -d' ' -f1)
task_tools_hash=$(sha256sum "$task_here/proof-tools.json" | cut -d' ' -f1)
verify () {
 test "$(sha256sum "$task_here/source-manifest.json" | cut -d' ' -f1)" = "$task_manifest_hash"
 test "$(sha256sum "$task_here/proof-tools.json" | cut -d' ' -f1)" = "$task_tools_hash"
 (cd "$task_root";jq -r 'to_entries[]|.value+"  "+.key' "$task_here/source-manifest.json" | sha256sum --check --quiet)
 while IFS=$'\t' read -r task_path task_sha; do test "$(sha256sum "$task_path" | cut -d' ' -f1)" = "$task_sha";done < <(jq -r '.[]|[.path,.sha256]|@tsv' "$task_here/proof-tools.json")
}
verify
mkdir "$task_output/obj"
task_prover=$(jq -r '.gnatprove.path' "$task_here/proof-tools.json")
test "$(realpath "$(command -v gnatprove)")" = "$(realpath "$task_prover")"
task_line=$(awk -v symbol="$task_symbol" '$0 ~ "^   (procedure|function) " symbol "([ (]|$)" {print NR;exit}' "$task_root/userspace/services/acpi/lib/aml_objects.adb")
test -n "$task_line"
task_command=("$task_prover" -P "$task_here/buffer_update_proof.gpr" "-XBUFFER_UPDATE_PROOF_OBJECTS=$task_output/obj" -u aml_objects.adb "--limit-subp=aml_objects.adb:$task_line" "--mode=$task_phase" -j1 --report=all --assumptions)
if test "$task_phase" = prove;then task_command+=(--level=1 --prover=cvc5,z3 --timeout=5);fi
jq -n --args '$ARGS.positional' -- "${task_command[@]}" > "$task_output/command.json"
printf '%s\n' "$task_output"
ulimit -s 65536
task_status=0
python3 "$task_root/tests/acpi-client-proof/capture_phase.py" "$task_output/$task_phase.log" "${task_command[@]}" || task_status=$?
verify
find "$task_output" -type f ! -name artifacts.json ! -name audit.json -print | sort | while IFS= read -r task_path;do printf '%s\t%s\n' "${task_path#"$task_output/"}" "$(sha256sum "$task_path" | cut -d' ' -f1)";done | jq -Rn '[inputs|split("\t")|{key:.[0],value:.[1]}]|from_entries' > "$task_output/artifacts.json"
jq -n --arg selected "$task_selected" --arg phase "$task_phase" --argjson status "$task_status" --slurpfile inputs "$task_here/source-manifest.json" --slurpfile tools "$task_here/proof-tools.json" --slurpfile artifacts "$task_output/artifacts.json" '{selected:$selected,phase:$phase,process_status:$status,accepted:false,inputs:$inputs[0],tools:$tools[0],artifacts:$artifacts[0],scope:"Actual selected AML_Objects routine; process result only, inspect proof/flow trees before acceptance."}' > "$task_output/audit.json"
printf 'process=%s; inspect result before next phase\n' "$task_status"
exit "$task_status"
