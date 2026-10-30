#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
task_root=$(cd "$task_here/../.." && pwd -P)
test "$#" = 1
task_output=$(realpath "$1")
case "$task_output" in "$task_here"/results/*) ;; *) exit 2;; esac
task_unit=$(jq -er .unit "$task_output/process.json")
test "$(jq -er .process_status "$task_output/process.json")" = 0
test "$(jq -er .phase "$task_output/process.json")" = prove
case "$task_unit" in
 acpi_region_protocol) task_checks=10; task_goals=24;;
 client_proof_model) task_checks=75; task_goals=164;;
 aml_identity-issuer) task_checks=4; task_goals=14;;
 acpi_fadt-transactions) task_checks=3; task_goals=3;;
 aml_field_datums) task_checks=2; task_goals=4;;
 *) exit 2;;
esac
# No unchecked success from process exit alone or from a generic template.
bash "$task_here/run.sh" --validate-only
cmp "$task_here/source-manifest.json" "$task_output/inputs.json"
sha256sum --check --quiet --strict "$task_output/prover.sha256"
task_spark="$task_output/obj/gnatprove/$task_unit.spark"
jq -e --argjson checks "$task_checks" --argjson goals "$task_goals" '
 (.spark|length)>0 and all(.spark[];.=="all") and
 (.skip_proof|length)==0 and (.skip_flow_proof|length)==0 and
 all(.flow[]?;.severity=="info") and
 (.proof|length)==$checks and ([.proof[].check_tree[]]|length)==$goals and
 all(.proof[];.severity=="info" and .how_proved=="prover" and
   (.check_tree|length)>0 and
   all(.check_tree[];any(.proof_attempts[]?;.result=="Valid")))
 ' "$task_spark" >/dev/null
task_hash=$(sha256sum "$task_spark")
jq -n --arg unit "$task_unit" --arg hash "${task_hash%% *}" \
 --argjson checks "$task_checks" --argjson goals "$task_goals" \
 '{unit:$unit,spark_sha256:$hash,proof_checks:$checks,valid_goals:$goals,
 unresolved:0,skipped:0,scope:"Selected checks only; see README.md for model and dependency assumptions."}' \
 > "$task_output/reviewed.json"
cat "$task_output/reviewed.json"
