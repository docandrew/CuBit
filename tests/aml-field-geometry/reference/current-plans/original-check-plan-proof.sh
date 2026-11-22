#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_root=$(cd -- "$task_here/../.." && pwd)
task_record="$task_here/CURRENT-PLAN-PROOF.json"
# Preserve the original proof recipe and accept only the explicit directory
# relocation in the current recipe. Production source bytes must still match.
task_original_recipe="$task_here/reference/current-plans/original-geometry-proof.gpr"
cmp "$task_here/geometry_proof.gpr" <(sed 's@userspace/lib/acpi@userspace/services/acpi/lib@g' "$task_original_recipe")
jq -e '.schema==1 and .unit=="aml_field_plans" and .level==1 and .entities==10 and .flow_results==11 and .valid_goals==8 and .unresolved==0 and (.source_sha256|length)==17 and all((.source_sha256+.retained_sha256)|keys[]; (startswith("/")|not) and (split("/")|all(.!="..")))' "$task_record" >/dev/null
while IFS=$'\t' read -r task_path task_hash; do
 task_current="$task_root/${task_path/#userspace\/lib\/acpi\//userspace/services/acpi/lib/}"
 if test "$task_path" = tests/aml-field-geometry/geometry_proof.gpr; then task_current="$task_original_recipe"; fi
 test ! -L "$task_current"
 task_actual=$(sha256sum "$task_current"); test "${task_actual%% *}" = "$task_hash"
done < <(jq -r '.source_sha256+.retained_sha256|to_entries[]|[.key,.value]|@tsv' "$task_record")
for task_phase in check flow prove; do
 task_dir="$task_here/reference/current-plans/$task_phase"
 jq -e --arg phase "$task_phase" '.unit=="plans" and .phase==$phase and .process_status==0 and .shell_status==0 and .accepted==true' "$task_dir/audit.json" >/dev/null
 jq -e --slurpfile original "$task_dir/audit.json" 'all(.source_sha256|to_entries[]; $original[0].inputs[.key]==.value)' "$task_record" >/dev/null
 jq -e --arg prefix "tests/aml-field-geometry/reference/current-plans/$task_phase/" --arg log "$task_phase.log" --slurpfile original "$task_dir/audit.json" '.retained_sha256 as $retained | $original[0].artifacts as $artifacts | $retained[$prefix+"unit.spark"]==$artifacts["obj/gnatprove/aml_field_plans.spark"] and $retained[$prefix+"command.json"]==$artifacts["command.json"] and $retained[$prefix+$log]==$artifacts[$log]' "$task_record" >/dev/null
 jq -e --arg mode "--mode=$task_phase" 'index("-u")!=null and index("aml_field_plans.adb")!=null and index($mode)!=null and index("-j1")!=null' "$task_dir/command.json" >/dev/null
 jq -e '(.spark|length)==10 and all(.spark[];.=="all") and (.skip_proof|length)==0 and (.skip_flow_proof|length)==0' "$task_dir/unit.spark" >/dev/null
 if test "$task_phase" != check; then jq -e '(.flow|length)==11 and all(.flow[];.severity=="info")' "$task_dir/unit.spark" >/dev/null; fi
 if test "$task_phase" = prove; then
  jq -e 'index("--level=1")!=null and index("--timeout=5")!=null and index("--prover=cvc5,z3")!=null' "$task_dir/command.json" >/dev/null
  jq -e --slurpfile record "$task_record" '(.proof|length)==8 and all(.proof[];.severity=="info" and .how_proved=="prover" and (.check_tree|length)>0 and all(.check_tree[];any(.proof_attempts[]?;.result=="Valid"))) and ([.proof[]|{file,line,col,rule}]==$record[0].obligations)' "$task_dir/unit.spark" >/dev/null
 fi
done
printf '%s\n' 'Cached current field-plan proof: 10 SPARK entities, 11 flow results, 8 proved safety/contract obligations; exact selected sources and retained trees verified. No prover execution or whole-interpreter proof.'
