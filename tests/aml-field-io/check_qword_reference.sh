#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_root=$(cd -- "$task_here/../.." && pwd)
task_oracle="$task_here/qword_oracle.ads"
task_reference="$task_here/reference/qword"
task_temp=$(mktemp -d)
trap 'rm -rf "$task_temp"' EXIT
while IFS=$'\t' read -r task_path task_expected; do task_hash=$(sha256sum "$task_root/$task_path"); test "${task_hash%% *}" = "$task_expected"; done < <(jq -r 'to_entries[]|[.key,.value]|@tsv' "$task_here/reference-manifest.json")
jq -e 'length==36 and all(.[];.compile_status==0 and .disassembly_status==0 and .runtime_status==0 and .opcode_classification=="region_and_field_in_disassembly")' "$task_reference/cases.json" >/dev/null
while IFS= read -r task_label; do
 awk '/^Evaluating /{name=$2} /\[Integer\] = / && name ~ /^\\P[0-9][0-9][0-9]$/ {print name "\t" $3}' "$task_reference/$task_label-acpiexec.log" |
 while IFS=$'\t' read -r task_name task_hex; do printf '%s\t%s\n' "$task_name" "$((16#$task_hex))"; done |
 jq -Rn '[inputs|split("\t")|{path:.[0],value:(.[1]|tonumber)}]' > "$task_temp/actual.json"
 jq --arg label "$task_label" '.[]|select(.label==$label)|.bytes' "$task_reference/observed-bytes.json" > "$task_temp/expected.json"
 cmp "$task_temp/actual.json" "$task_temp/expected.json"
 for task_suffix in aml dsl; do
  task_key=aml_sha256
  if test "$task_suffix" = dsl; then task_key=disassembly_sha256; fi
  task_hash=$(sha256sum "$task_reference/$task_label.$task_suffix")
  test "${task_hash%% *}" = "$(jq -r --arg label "$task_label" --arg key "$task_key" '.[]|select(.label==$label)|.[$key]' "$task_reference/cases.json")"
 done
done < <(jq -r '.[].label' "$task_reference/cases.json")
jq -f "$task_reference/golden-rows.jq" "$task_reference/observed-bytes.json" > "$task_temp/rows.json"
{
 printf 'pragma Ada_2022;\nwith Interfaces;\npackage Qword_Oracle is\n type Memory is array (Natural range 0 .. 15) of Interfaces.Unsigned_8;\n type Corpus is array (Natural range 0 .. 89) of Memory;\n Expected : constant Corpus := [\n'
 jq -r 'to_entries[]|"   " + (.value.key|tostring) + " => [" + (.value.bytes|map(tostring)|join(",")) + "]" + (if .key==89 then "];" else "," end)' "$task_temp/rows.json"
 printf 'end Qword_Oracle;\n'
} > "$task_temp/qword_oracle.ads"
cmp "$task_temp/qword_oracle.ads" "$task_oracle"
printf 'Qword cached ACPICA reference verified:36runs/108methods/1728bytes;90store goldens reproduced. No ACPICA execution.\n'
