#!/usr/bin/env bash
# Keep source and artifact inventories independent of the invoking locale.
export LC_ALL=C
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_root=$(cd -- "$task_here/../.." && pwd)
task_unit=${1:?Expected datums, plans or alignment}
task_phase=${2:?Expected check, flow or prove}
case "$task_unit" in
 datums) task_body=aml_field_datums; task_entities=15; task_checks=58;;
 plans) task_body=aml_field_plans; task_entities=10; task_checks=8;;
 alignment) task_body=acpi_fadt-transactions; task_entities=6; task_checks=3;;
 *) exit 2;;
esac
case "$task_phase" in check|flow|prove) ;; *) exit 2;; esac
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
test -n "${IN_NIX_SHELL:-}"
test "$(ps -o ni= -p $$)" -ge 19
task_output=$(mktemp -d "$task_here/proof-$task_unit-$task_phase-XXXXXXXX")
task_status=125
task_accepted=false
task_manifest_hash=$(sha256sum "$task_here/source-manifest.json")
task_manifest_hash=${task_manifest_hash%% *}
verify () {
 task_actual=$(sha256sum "$task_here/source-manifest.json")
 test "${task_actual%% *}" = "$task_manifest_hash"
 while IFS=$'\t' read -r task_path task_hash; do
  test ! -L "$task_root/$task_path"
  test "$(realpath "$task_root/$task_path")" = "$task_root/$task_path"
  task_actual=$(sha256sum "$task_root/$task_path")
  test "${task_actual%% *}" = "$task_hash"
 done < <(jq -r 'to_entries[]|[.key,.value]|@tsv' "$task_here/source-manifest.json")
 while IFS=$'\t' read -r task_path task_hash; do
  task_actual=$(sha256sum "$task_path"); test "${task_actual%% *}" = "$task_hash"
 done < <(jq -r '.[]|[.path,.sha256]|@tsv' "$task_here/proof-tools.json")
}
finish () {
 task_shell_status=$?
 verify || { task_status=126; task_accepted=false; }
 find "$task_output" -type f ! -name audit.json ! -name artifacts.json -print | sort |
 while IFS= read -r task_path; do
  task_hash=$(sha256sum "$task_path")
  printf '%s\t%s\n' "${task_path#"$task_output/"}" "${task_hash%% *}"
 done | jq -Rn '[inputs|split("\t")|{key:.[0],value:.[1]}]|from_entries' > "$task_output/artifacts.json"
 jq -n --arg unit "$task_unit" --arg phase "$task_phase" --argjson status "$task_status" --argjson shell_status "$task_shell_status" --argjson accepted "$task_accepted" --slurpfile inputs "$task_here/source-manifest.json" --slurpfile tools "$task_here/proof-tools.json" --slurpfile artifacts "$task_output/artifacts.json" \
  '{unit:$unit,phase:$phase,process_status:$status,shell_status:$shell_status,accepted:$accepted,inputs:$inputs[0],tools:$tools[0],artifacts:$artifacts[0],scope:"Selected helper safety/contracts only. Dependencies/effects and trusted Ada runtime models require separate review; no full interpreter or hardware proof."}' > "$task_output/audit.json"
 printf '%s process=%s accepted=%s\n' "$task_output" "$task_status" "$task_accepted"
 trap - EXIT
 if test "$task_shell_status" != 0; then exit "$task_shell_status"; fi
 test "$task_accepted" = true
 exit "$task_status"
}
trap finish EXIT
verify
task_gnatprove=$(jq -r '.gnatprove.path' "$task_here/proof-tools.json")
test "$(readlink -f "$(command -v gnatprove)")" = "$(readlink -f "$task_gnatprove")"
mkdir "$task_output/obj"
task_file="$task_body.adb"
if test "$task_unit" = alignment; then task_file="$task_body.ads"; fi
task_command=("$task_gnatprove" -P "$task_here/geometry_proof.gpr" "-XPLAN_PROOF_OBJECTS=$task_output/obj" -u "$task_file" "--mode=$task_phase" -j1 --report=all)
if test "$task_unit" = alignment; then task_command+=(--limit-region=acpi_fadt-transactions.ads:7:14); fi
if test "$task_phase" = prove; then task_command+=(--level=1 --prover=cvc5,z3 --timeout=5); fi
jq -n --args '$ARGS.positional' -- "${task_command[@]}" > "$task_output/command.json"
printf '%s\n' "$task_output"
ulimit -s 65536
ulimit -f 1024
set +e
timeout --signal=TERM --kill-after=3s 180s "${task_command[@]}" > "$task_output/$task_phase.log" 2>&1
task_status=$?
set -e
test "$task_status" = 0
task_spark="$task_output/obj/gnatprove/$task_body.spark"
jq -e --argjson entities "$task_entities" '(.spark|length)==$entities and all(.spark[];.=="all") and (.skip_proof|length)==0 and (.skip_flow_proof|length)==0' "$task_spark" >/dev/null
if test "$task_phase" != check; then jq -e 'all(.flow[]?;.severity=="info")' "$task_spark" >/dev/null; fi
if test "$task_phase" = prove; then
 jq -e --argjson checks "$task_checks" '(.proof|length)==$checks and all(.proof[];.severity=="info" and .how_proved=="prover" and (.check_tree|length)>0 and all(.check_tree[];any(.proof_attempts[]?;.result=="Valid")))' "$task_spark" >/dev/null
fi
task_accepted=true
