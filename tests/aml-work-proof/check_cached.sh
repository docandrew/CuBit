#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_root=$(cd -- "$task_here/../.." && pwd)
task_record="$task_here/CURRENT-PROOF.json"
# Keep the recorded proof inputs immutable. The current recipe differs only
# by the service-local library path; compare the exact historical recipe too.
task_original_recipe="$task_here/reference/current/original-work-proof.gpr"
cmp "$task_here/work_proof.gpr" <(sed 's@userspace/lib/acpi@userspace/services/acpi/lib@g' "$task_original_recipe")
jq -e '.schema==1 and .unit=="AML_Execution_Limits.Charged_Field_Work" and .entities==2 and .flow_results==2 and .valid_goals==2 and .unresolved==0 and .level==1' "$task_record" >/dev/null
while IFS=$'\t' read -r task_path task_hash; do
 task_current="$task_root/${task_path/#userspace\/lib\/acpi\//userspace/services/acpi/lib/}"
 if test "$task_path" = tests/aml-work-proof/work_proof.gpr; then task_current="$task_original_recipe"; fi
 test ! -L "$task_current"
 test "$(sha256sum "$task_current" | cut -d ' ' -f 1)" = "$task_hash"
done < <(jq -r '.source_sha256+.retained_sha256|to_entries[]|[.key,.value]|@tsv' "$task_record")
for task_phase in check flow prove; do
 task_dir="$task_here/reference/current/$task_phase"
 jq -e --arg phase "$task_phase" '.phase==$phase and .process_status==0 and .shell_status==0 and .accepted==true' "$task_dir/audit.json" >/dev/null
 jq -e --slurpfile origin "$task_dir/audit.json" 'all(.source_sha256|to_entries[];$origin[0].inputs[.key]==.value)' "$task_record" >/dev/null
 jq -e --arg prefix "tests/aml-work-proof/reference/current/$task_phase/" --arg log "$task_phase.log" --slurpfile original "$task_dir/audit.json" '.retained_sha256 as $retained | $original[0].artifacts as $artifacts | $retained[$prefix+"unit.spark"]==$artifacts["obj/gnatprove/aml_execution_limits.spark"] and $retained[$prefix+"command.json"]==$artifacts["command.json"] and $retained[$prefix+$log]==$artifacts[$log]' "$task_record" >/dev/null
 jq -e --arg mode "--mode=$task_phase" 'index("-u")!=null and index("aml_execution_limits.ads")!=null and index($mode)!=null and index("-j1")!=null' "$task_dir/command.json" >/dev/null
 jq -e '(.spark|length)==2 and all(.spark[];.=="all") and (.skip_proof|length)==0 and (.skip_flow_proof|length)==0 and any(.entities[];.name=="AML_Execution_Limits.Charged_Field_Work")' "$task_dir/unit.spark" >/dev/null
 if test "$task_phase" != check; then jq -e '(.flow|length)==2 and all(.flow[];.severity=="info")' "$task_dir/unit.spark" >/dev/null; fi
 if test "$task_phase" = prove; then
  jq -e 'index("--level=1")!=null and index("--timeout=5")!=null and index("--prover=cvc5,z3")!=null' "$task_dir/command.json" >/dev/null
  jq -e --slurpfile record "$task_record" '(.proof|length)==2 and all(.proof[];.severity=="info" and .how_proved=="prover" and (.check_tree|length)>0 and all(.check_tree[];any(.proof_attempts[]?;.result=="Valid"))) and ([.proof[]|{file,line,col,rule}]==$record[0].obligations) and all(.assumptions[];(.assumptions|length)==0)' "$task_dir/unit.spark" >/dev/null
 fi
done
printf '%s\n' 'Cached production work-charge proof: two SPARK entities, two flow results, two proved range/postcondition obligations; exact code/recipe and retained proof trees verified. No router/interpreter/native proof claim.'
