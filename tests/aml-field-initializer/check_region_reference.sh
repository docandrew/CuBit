#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "${1:-$(dirname -- "${BASH_SOURCE[0]}")}" && pwd -P)
test "$#" -le 1
task_reference="$task_here/region-reference"
task_spec="$task_here/region_reg_oracle.ads"
test "$(cat "$task_reference/status.txt")" = "$(printf 'iasl-r1 0\nacpiexec-r1 0\niasl-r2 0\nacpiexec-r2 0')"
for task_revision in 1 2; do
 task_prefix="$task_reference/region-r$task_revision"
 task_bytes=$(od -An -v -tu1 "$task_prefix.aml" | awk '{for(i=1;i<=NF;i++){printf "%s%s",separator,$i;separator=","}}')
 task_registered=$(awk -F '[][]' -v name="Table_R$task_revision" '$0 ~ name " :" {print $2}' "$task_spec")
 test -n "$task_registered" && test "$task_bytes" = "$task_registered"
 read -r task_b0 task_b1 task_b2 task_b3 < <(od -An -v -tu1 -j4 -N4 "$task_prefix.aml")
 test "$((task_b0 + (task_b1 << 8) + (task_b2 << 16) + (task_b3 << 24)))" = "$(stat -c %s "$task_prefix.aml")"
 test "$(od -An -v -tu1 -j8 -N1 "$task_prefix.aml" | tr -d ' ')" = "$task_revision"
 od -An -v -tu1 "$task_prefix.aml" | awk '{for(i=1;i<=NF;i++) sum+=$i} END {exit(sum%256!=0)}'
 task_values=$(awk '/^Evaluating / {name=$2;gsub(/\\/,"",name)} /\[Integer\] = / {if(name ~ /^Q(CNT|SPC|CON|VAL|DON|ROT|SEQ)$/) print name "\t" $3}' "$task_prefix-acpiexec.log")
 test "$task_values" = "$(cat "$task_prefix-values.tsv")"
 test "$(printf '%s\n' "$task_values" | wc -l)" = 14
 printf '%s\n' "$task_values" | awk -F '\t' 'BEGIN {split("QCNT QSPC QCON QVAL QDON QROT QSEQ",names," ")} {if($1!=names[(NR-1)%7+1] || length($2)!=16 || $2 !~ /^[0-9A-F]+$/) exit 1}'
 task_expected=$(printf '%s\n' "$task_values" | awk -F '\t' '{printf "%s16#%s#",separator,$2;separator=","}')
 task_registered=$(awk -F '[][]' -v name="Values_R$task_revision" '$0 ~ name " :" {print $2}' "$task_spec")
 test -n "$task_registered" && test "$task_expected" = "$task_registered"
 task_unexpected=$(rg '^ACPI (Error|Exception):' "$task_prefix-acpiexec.log" | rg -v '^ACPI Error: 4 \(0x4\) Outstanding cache allocations ' || true)
 test -z "$task_unexpected"
done
printf 'Cached normal _REG reference verified: two exact AML images and 28 getter values. Teardown cache diagnostics retained; no ACPICA execution or full-startup claim.\n'
