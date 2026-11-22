#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_root=$(cd -- "$task_here/../.." && pwd)
task_output=$(mktemp -d)
trap 'rm -rf -- "$task_output"' EXIT
(cd "$task_here/reference"; sha256sum --check --quiet evidence.sha256)
test "$(cat "$task_here/reference/status.tsv")" = $'compile\t0\nruntime\t0'
awk -f "$task_root/tests/aml-bank-declarations/extract_observations.awk" "$task_here/reference/reference.log" > "$task_output/actual.tsv"
cmp "$task_here/reference/expected.tsv" "$task_output/actual.tsv"
test "$(wc -l < "$task_output/actual.tsv")" = 13
! rg -q 'Evaluation of .* failed|Aborting method|Could not resolve symbol' "$task_here/reference/reference.log"
rg -q 'Compilation successful. 0 Errors, 0 Warnings, 0 Remarks' "$task_here/reference/compile.log"
od -An -v -tu1 "$task_here/reference/bank.aml" | awk '{for(i=1;i<=NF;i++){n++;sum=(sum+$i)%256;if(n>=5&&n<=8)header+=$i*256^(n-5)}} END{exit !(n==222&&header==n&&sum==0)}'
od -An -v -tu1 "$task_here/reference/bank.aml" | awk '{for(i=1;i<=NF;i++)print $i}' > "$task_output/bytes"
awk '/Raw_Table.*:= \[/{reading=1;next} /\];/{reading=0} reading{gsub(/[^0-9]+/," ");for(i=1;i<=NF;i++)print $i}' "$task_here/bank_access_oracle.ads" > "$task_output/embedded"
cmp "$task_output/bytes" "$task_output/embedded"
printf '%s\n' 'Cached ACPICA: 13 ordered Integer returns; exact embedded 222-byte table/checksum verified.'
