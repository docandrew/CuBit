#!/usr/bin/env bash
set -euo pipefail
task_source=${1:?source}
task_output=$(realpath "${2:?output}")
task_receipt=${3:?receipt}
case "$task_output" in "$task_source"/tests/acpi-resource-layout-proof/results/*) ;; *) exit 2;; esac
test "$(jq -er .process_status "$task_output/process.json")" = 0
test "$(jq -er .phase "$task_output/process.json")" = prove
bash "$task_source/tests/acpi-resource-layout-proof/run.sh" --validate-only
cmp "$task_source/tests/acpi-resource-layout-proof/source-manifest.json" "$task_output/inputs.json"
sha256sum --check --quiet --strict "$task_output/prover.sha256"
if rg 'medium:|high:|warning:|error:' "$task_output/prove.log"; then exit 1; fi
printf '[]\n' > "$task_receipt"
for task_unit in acpi_resource_data-dma_descriptors acpi_resource_data-dependent_groups acpi_resource_data-address_spaces-extended_descriptors; do
 task_spark="$task_output/obj/gnatprove/$task_unit.spark"
 jq -e '
  (.spark|length)>=2 and all(.spark[];.=="all") and
  (.skip_proof|length)==0 and (.skip_flow_proof|length)==0 and
  (.pragma_assume|length)==0 and .stop_reason=="STOP_REASON_NONE" and
  (.flow|length)>0 and all(.flow[];.severity=="info") and
  (.proof|length)>0 and all(.proof[];.severity=="info" and .how_proved=="prover" and
   (.check_tree|length)>0 and all(.check_tree[];any(.proof_attempts[]?;.result=="Valid")))
 ' "$task_spark" >/dev/null
 task_hash=$(sha256sum "$task_spark")
 jq --arg unit "$task_unit" --arg hash "${task_hash%% *}" --slurpfile spark "$task_spark" '.+[{unit:$unit,spark_sha256:$hash,entities:($spark[0].spark|length),flow_checks:($spark[0].flow|length),proof_checks:($spark[0].proof|length),valid_goals:([$spark[0].proof[].check_tree[]]|length),unresolved:0,skipped:0,assume:0}]' "$task_receipt" > "$task_receipt.new"
 mv "$task_receipt.new" "$task_receipt"
done
jq --arg source "$task_source" --arg output "$task_output" '{source:$source,output:$output,units:.,totals:{entities:([.[].entities]|add),flow_checks:([.[].flow_checks]|add),proof_checks:([.[].proof_checks]|add),valid_goals:([.[].valid_goals]|add)},scope:"Selected DMA, dependent-group and Extended Address runtime safety, decoder extent and group output contracts, initialization/dependencies and loop progress; not grammar completeness, full descriptor field-value/checksum equivalence, allocation/bridge/translation/resource authority or whole AML correctness."}' "$task_receipt" > "$task_receipt.new"
mv "$task_receipt.new" "$task_receipt"
cat "$task_receipt"
