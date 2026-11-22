#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_root=$(cd -- "$task_here/../.." && pwd)
task_output=$(mktemp -d)
trap 'rm -rf -- "$task_output"' EXIT
test "$(cat "$task_here/reference/status.tsv")" = $'compile128\t0\nruntime128\t0\ncompile256\t0\nruntime256\t0'
for task_depth in 128 256; do
 rg -q 'Compilation successful. 0 Errors, 0 Warnings, 0 Remarks' "$task_here/reference/compile$task_depth.log"
 od -An -v -tu1 "$task_here/reference/depth$task_depth.aml" | awk '{for(i=1;i<=NF;i++){n++;sum=(sum+$i)%256;if(n>=5&&n<=8)header+=$i*256^(n-5)}} END{exit !(header==n&&sum==0)}'
 od -An -v -tu1 "$task_here/reference/depth$task_depth.aml" | awk '{for(i=1;i<=NF;i++)print $i}' > "$task_output/raw$task_depth"
 awk -v name="Table$task_depth" '$0~name" .*:= \\["{reading=1;next} /\];/{reading=0} reading{gsub(/[^0-9]+/," ");for(i=1;i<=NF;i++)print $i}' "$task_here/bank_access_oracle.ads" > "$task_output/embedded$task_depth"
 cmp "$task_output/raw$task_depth" "$task_output/embedded$task_depth"
done
awk -f "$task_root/tests/aml-bank-declarations/extract_observations.awk" "$task_here/reference/reference128.log" > "$task_output/actual128.tsv"
cmp "$task_here/reference/expected.tsv" "$task_output/actual128.tsv"
cmp "$task_here/reference/actual128.tsv" "$task_output/actual128.tsv"
test "$(wc -l < "$task_output/actual128.tsv")" = 3
! rg -q 'Evaluation of .* failed|Aborting method|Could not resolve symbol' "$task_here/reference/reference128.log"
test ! -s "$task_here/reference/actual256.tsv"
test "$(stat -c %s "$task_here/reference/reference256.log")" = 1048576
rg -q 'Large Reference Count' "$task_here/reference/reference256.log"
! rg -q '^  \[Integer\] = ' "$task_here/reference/reference256.log"
printf '%s\n' 'Cached depth128 ACPICA: three exact Integer observations. Both embedded tables/checksums verified. Depth256 runtime reference INCOMPLETE (bounded log filled); not counted as compatibility success.'
