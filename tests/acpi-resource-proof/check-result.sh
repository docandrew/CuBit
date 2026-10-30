#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
task_output=$(realpath "${1:?result directory}")
test "$#" = 1
case "$task_output" in "$task_here"/results/*) ;; *) exit 2;; esac
test "$(jq -er .process_status "$task_output/process.json")" = 0
test "$(jq -er .phase "$task_output/process.json")" = prove
bash "$task_here/run.sh" --validate-only
cmp "$task_here/source-manifest.json" "$task_output/inputs.json"
sha256sum --check --quiet --strict "$task_output/prover.sha256"
task_spark="$task_output/obj/gnatprove/acpi_resource_data.spark"
jq -e '
 (.spark|length)==3 and all(.spark[];.=="all") and
 (.skip_proof|length)==0 and (.skip_flow_proof|length)==0 and
 (.pragma_assume|length)==0 and .stop_reason=="STOP_REASON_NONE" and
 (.flow|length)==10 and all(.flow[];.severity=="info") and
 (.proof|length)==51 and ([.proof[].check_tree[]]|length)==93 and
 all(.proof[];.severity=="info" and .how_proved=="prover" and
   (.check_tree|length)>0 and all(.check_tree[];any(.proof_attempts[]?;.result=="Valid")))
' "$task_spark" >/dev/null
task_hash=$(sha256sum "$task_spark")
jq -n --arg hash "${task_hash%% *}" '{unit:"acpi_resource_data",spark_sha256:$hash,proof_checks:51,valid_goals:93,flow_checks:10,unresolved:0,skipped:0,scope:"Runtime safety, frame geometry/progress and scan bounds/quota/termination; not descriptor semantics, checksum functional equivalence, authority or whole interpreter correctness."}' > "$task_output/reviewed.json"
cat "$task_output/reviewed.json"
