#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_root=$(cd -- "$task_here/../.." && pwd)
task_phase=${1:?Expected check, flow or prove}
case "$task_phase" in check|flow|prove) ;; *) exit 2;; esac
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
test -n "${IN_NIX_SHELL:-}"
test "$(ps -o ni= -p $$)" -ge 19
task_output=$(mktemp -d "$task_here/result-$task_phase-XXXXXXXX")
task_status=125
verify () {
 while IFS=$'\t' read -r task_path task_hash; do
  task_actual=$(sha256sum "$task_root/$task_path"); test "${task_actual%% *}" = "$task_hash"
 done < <(jq -r 'to_entries[]|[.key,.value]|@tsv' "$task_here/source-manifest.json")
 while IFS=$'\t' read -r task_path task_hash; do
  task_actual=$(sha256sum "$task_path"); test "${task_actual%% *}" = "$task_hash"
 done < <(jq -r '.[]|[.path,.sha256]|@tsv' "$task_here/proof-tools.json")
}
finish () {
 task_shell_status=$?
 verify || task_status=126
 find "$task_output" -type f ! -name audit.json ! -name artifacts.json -print0 | sort -z | xargs -0 sha256sum | sed "s@  $task_output/@  @" | jq -Rn '[inputs|capture("^(?<hash>[0-9a-f]{64})  (?<path>.*)$")|{key:.path,value:.hash}]|from_entries' > "$task_output/artifacts.json"
 jq -n --arg phase "$task_phase" --argjson process "$task_status" --argjson shell "$task_shell_status" --slurpfile sources "$task_here/source-manifest.json" --slurpfile artifacts "$task_output/artifacts.json" '{phase:$phase,process_status:$process,shell_status:$shell,needs_review:true,sources:$sources[0],artifacts:$artifacts[0],scope:"Selected constructor in namespace instantiation; process success is not proof acceptance. Inspect target coverage, SPARK exclusions, flow and proof trees."}' > "$task_output/audit.json"
 printf '%s process=%s needs-review\n' "$task_output" "$task_status"
 trap - EXIT
 if test "$task_shell_status" != 0; then exit "$task_shell_status"; fi
 exit "$task_status"
}
trap finish EXIT
verify
task_prover=$(jq -r '.gnatprove.path' "$task_here/proof-tools.json")
test "$(readlink -f "$(command -v gnatprove)")" = "$(readlink -f "$task_prover")"
mkdir "$task_output/obj"
task_command=("$task_prover" -P "$task_here/namespace_proof.gpr" "-XBANK_NAMESPACE_PROOF_OBJECTS=$task_output/obj" -U --limit-subp=aml_namespace.adb:1287 "--mode=$task_phase" -j1 --report=all --assumptions)
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
