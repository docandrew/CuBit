#!/usr/bin/env bash
# Keep source and artifact inventories independent of the invoking locale.
export LC_ALL=C
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_root=$(cd -- "$task_here/../.." && pwd)
task_phase=${1:?check, flow or prove}
case "$task_phase" in check|flow|prove) ;; *) exit 2;; esac
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
test -n "${IN_NIX_SHELL:-}"
task_output=$(mktemp -d "$task_here/result-$task_phase-XXXXXXXX")
task_status=125; task_accepted=false
task_manifest=$(sha256sum "$task_here/source-manifest.json"); task_manifest=${task_manifest%% *}
verify () {
 test "$(sha256sum "$task_here/source-manifest.json" | cut -d ' ' -f 1)" = "$task_manifest"
 while IFS=$'\t' read -r task_path task_hash; do
  test ! -L "$task_root/$task_path"
  test "$(realpath "$task_root/$task_path")" = "$task_root/$task_path"
  test "$(sha256sum "$task_root/$task_path" | cut -d ' ' -f 1)" = "$task_hash"
 done < <(jq -r 'to_entries[]|[.key,.value]|@tsv' "$task_here/source-manifest.json")
 while IFS=$'\t' read -r task_path task_hash; do
  test "$(sha256sum "$task_path" | cut -d ' ' -f 1)" = "$task_hash"
 done < <(jq -r '.[]|[.path,.sha256]|@tsv' "$task_here/proof-tools.json")
}
finish () {
 task_shell=$?
 verify || { task_status=126; task_accepted=false; }
 find "$task_output" -type f ! -name audit.json ! -name artifacts.json -print | sort |
 while IFS= read -r task_path; do
  printf '%s\t%s\n' "${task_path#"$task_output/"}" "$(sha256sum "$task_path" | cut -d ' ' -f 1)"
 done | jq -Rn '[inputs|split("\t")|{key:.[0],value:.[1]}]|from_entries' > "$task_output/artifacts.json"
 jq -n --arg phase "$task_phase" --argjson status "$task_status" --argjson shell "$task_shell" --argjson accepted "$task_accepted" --slurpfile inputs "$task_here/source-manifest.json" --slurpfile tools "$task_here/proof-tools.json" --slurpfile artifacts "$task_output/artifacts.json" '{phase:$phase,process_status:$status,shell_status:$shell,accepted:$accepted,inputs:$inputs[0],tools:$tools[0],artifacts:$artifacts[0],scope:"Pure Charged_Field_Work function and its local contracts/range only; not router, namespace, interpreter or native correctness."}' > "$task_output/audit.json"
 printf '%s process=%s accepted=%s\n' "$task_output" "$task_status" "$task_accepted"
 trap - EXIT
 if test "$task_shell" != 0; then exit "$task_shell"; fi
 test "$task_accepted" = true
 exit "$task_status"
}
trap finish EXIT
verify
mkdir "$task_output/obj"
task_gnatprove=$(jq -r '.gnatprove.path' "$task_here/proof-tools.json")
test "$(readlink -f "$(command -v gnatprove)")" = "$(readlink -f "$task_gnatprove")"
task_command=("$task_gnatprove" -P "$task_here/work_proof.gpr" "-XWORK_PROOF_OBJECTS=$task_output/obj" -u aml_execution_limits.ads "--mode=$task_phase" -j1 --report=all)
if test "$task_phase" = prove; then task_command+=(--level=1 --prover=cvc5,z3 --timeout=5); fi
jq -n --args '$ARGS.positional' -- "${task_command[@]}" > "$task_output/command.json"
printf '%s\n' "$task_output"
ulimit -s 65536; ulimit -f 1024
set +e
timeout --signal=TERM --kill-after=3s 180s "${task_command[@]}" > "$task_output/$task_phase.log" 2>&1
task_status=$?
set -e
test "$task_status" = 0
task_spark="$task_output/obj/gnatprove/aml_execution_limits.spark"
jq -e '(.spark|length)>0 and all(.spark[];.=="all") and (.skip_proof|length)==0 and (.skip_flow_proof|length)==0 and any(.entities[];.name=="AML_Execution_Limits.Charged_Field_Work")' "$task_spark" >/dev/null
if test "$task_phase" != check; then jq -e 'all(.flow[]?;.severity=="info")' "$task_spark" >/dev/null; fi
if test "$task_phase" = prove; then
 jq -e '(.proof|length)>0 and all(.proof[];.severity=="info" and .how_proved=="prover" and (.check_tree|length)>0 and all(.check_tree[];any(.proof_attempts[]?;.result=="Valid")))' "$task_spark" >/dev/null
fi
task_accepted=true
