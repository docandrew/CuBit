#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "${1:-$(dirname -- "${BASH_SOURCE[0]}")}" && pwd -P)
test "$#" -le 1
task_reference="$task_here/lid-reference"
task_bytes=$(od -An -v -tu1 "$task_reference/lid.aml" | awk '{for(i=1;i<=NF;i++){printf "%s%s",separator,$i;separator=","}}')
task_registered=$(sed -n '/Raw : constant Bytes := \[/,/];/p' "$task_here/field_initializer_tests.adb" | tr -cd '0-9,')
test -n "$task_registered" && test "$task_bytes" = "$task_registered"
test "$(stat -c %s "$task_reference/lid.aml")" = 156
od -An -v -tu1 "$task_reference/lid.aml" | awk '{for(i=1;i<=NF;i++) sum+=$i} END {exit(sum%256!=0)}'
task_values=$(awk '/^Evaluating / {name=$2;gsub(/\\/,"",name)} /\[Integer\] = / {print name "\t" $3}' "$task_reference/execute.log")
test "$task_values" = "$(cat "$task_reference/values.tsv")"
test "$(rg -c 'Received a Device Notify on \[LID0\].*Value 0x80' "$task_reference/execute.log")" = 3
task_unexpected=$(rg '^ACPI (Error|Exception):' "$task_reference/execute.log" | rg -v '^ACPI Error: 4 \(0x4\) Outstanding cache allocations ' || true)
test -z "$task_unexpected"
printf 'Cached lid reference verified: exact compiled AML, seven integer results and three notifications; known ACPICA teardown cache diagnostic retained.\n'
