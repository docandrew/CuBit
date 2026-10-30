#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_output=$(mktemp -d)
trap 'rm -rf -- "$task_output"' EXIT
(cd "$task_here/reference"; sha256sum --check --quiet evidence.sha256)
test "$(cat "$task_here/reference/status.tsv")" = $'compile\t0\nwidths\t0\nwidths32\t0'
for task_revision in 64 32; do
 task_file=widths
 if test "$task_revision" = 32; then task_file=widths32; fi
 awk -f "$task_here/extract_reference.awk" "$task_here/reference/$task_file.log" > "$task_output/$task_revision.tsv"
 cmp "$task_here/reference/expected$task_revision.tsv" "$task_output/$task_revision.tsv"
 test "$(wc -l < "$task_output/$task_revision.tsv")" = 24
 ! rg -q 'Evaluation of .* failed|Aborting method|Could not resolve symbol' "$task_here/reference/$task_file.log"
 od -An -v -tu1 "$task_here/reference/$task_file.aml" | awk '{for(i=1;i<=NF;i++){n++;sum=(sum+$i)%256;if(n>=5&&n<=8)header+=$i*256^(n-5)}} END{exit !(n==485&&header==n&&sum==0)}'
done
cmp <(tail -c +37 "$task_here/reference/widths.aml") <(tail -c +37 "$task_here/reference/widths32.aml")
test "$(od -An -j8 -N1 -tu1 "$task_here/reference/widths.aml" | xargs)" = 2
test "$(od -An -j8 -N1 -tu1 "$task_here/reference/widths32.aml" | xargs)" = 1
{
 printf 'with Firmware_Tables;\npackage Bank_Width_Oracle is\n'
 for task_revision in 64 32; do
  task_file=widths.aml
  if test "$task_revision" = 32; then task_file=widths32.aml; fi
  printf '   Table%s : constant Firmware_Tables.Bytes := [\n' "$task_revision"
  od -An -v -tu1 "$task_here/reference/$task_file" | awk '{for(i=1;i<=NF;i++){if(n++)printf ",";if(n%16==1)printf "\n      ";printf "%s",$i}} END{printf "\n   ];\n"}'
 done
 printf 'end Bank_Width_Oracle;\n'
} > "$task_output/oracle.ads"
cmp "$task_output/oracle.ads" "$task_here/bank_width_oracle.ads"
printf '%s\n' 'Bank widths: 24 ordered typed observations per revision (including four AML32 Buffers), exact embedded AML and checksums verified.'
