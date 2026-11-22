#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_root=$(cd -- "$task_here/../.." && pwd)
task_reference="$task_here/reference/partial-write"
task_output=$(mktemp -d)
trap 'rm -rf "$task_output"' EXIT
while IFS=$'\t' read -r task_path task_expected; do
    task_actual=$(sha256sum "$task_root/$task_path")
    test "${task_actual%% *}" = "$task_expected"
done < <(jq -r 'to_entries[]|[.key,.value]|@tsv' "$task_here/reference-manifest.json")
for task_kind in scalar objects; do
    task_cases="$task_reference/$task_kind/cases.json"
    jq -e 'length==24 and all(.[];.compile_status==0 and .disassembly_status==0 and .runtime_status==0 and .opcode_classification=="region_and_field_in_disassembly")' "$task_cases" >/dev/null
    while IFS= read -r task_label; do
        task_log="$task_reference/$task_kind/$task_label-acpiexec.log"
        awk '/^Evaluating / {name=$2}
            /\[Integer\] = / && name ~ /^\\P[0-9][0-9][0-9]$/ {print name "\t" $3}' "$task_log" |
        while IFS=$'\t' read -r task_name task_hex; do
            printf '%s\t%s\n' "$task_name" "$((16#$task_hex))"
        done | jq -Rn '[inputs|split("\t")|[.[0],(.[1]|tonumber)]]' > "$task_output/actual.json"
        jq --arg label "$task_label" '[.[]|select(.label==$label)|.observations[]|select(.path|test("^\\\\P[0-9]{3}$"))|[.path,.value]]' "$task_reference/$task_kind/observations.json" > "$task_output/expected.json"
        cmp "$task_output/actual.json" "$task_output/expected.json"
        task_aml=$(sha256sum "$task_reference/$task_kind/$task_label.aml")
        test "${task_aml%% *}" = "$(jq -r --arg label "$task_label" '.[]|select(.label==$label)|.aml_sha256' "$task_cases")"
        task_dsl=$(sha256sum "$task_reference/$task_kind/$task_label.dsl")
        test "${task_dsl%% *}" = "$(jq -r --arg label "$task_label" '.[]|select(.label==$label)|.disassembly_sha256' "$task_cases")"
    done < <(jq -r '.[].label' "$task_cases")
done
jq -s -f "$task_reference/rows.jq" "$task_reference/scalar/observations.json" "$task_reference/objects/observations.json" > "$task_output/rows.json"
{
    printf 'pragma Ada_2022;\nwith Interfaces;\npackage Field_Write_Oracle is\n   type Memory is array (Natural range 0 .. 11) of Interfaces.Unsigned_8;\n   type Corpus is array (Natural range 0 .. 119) of Memory;\n   Expected : constant Corpus := [\n'
    jq -r 'to_entries[]|"      " + (.value.key|tostring) + " => [" + (.value.bytes|map(tostring)|join(",")) + "]" + (if .key==119 then "];" else "," end)' "$task_output/rows.json"
    printf 'end Field_Write_Oracle;\n'
} > "$task_output/field_write_oracle.ads"
cmp "$task_output/field_write_oracle.ads" "$task_here/field_write_oracle.ads"
printf 'Cached normal ACPICA write reference: 48 tables, 120 stores, 1440 byte observations verified. No ACPICA execution.\n'
