#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_out=$(mktemp -d)
trap 'rm -rf -- "$task_out"' EXIT
for r in 64 32; do
 n=bank; if test "$r" = 32; then n=bank32; fi
 (cd "$task_here/reference"; sha256sum --check --quiet "$n-evidence.sha256")
 test "$(cat "$task_here/reference/$n-status.tsv")" = $'compile\t0\nruntime\t0'
 rg -q 'Compilation successful. 0 Errors, 0 Warnings, 0 Remarks' "$task_here/reference/$n-compile.log"
 ! rg -q 'Evaluation of .* failed|Aborting method|Could not resolve symbol' "$task_here/reference/$n-reference.log"
 awk -f "$task_here/extract_observations.awk" "$task_here/reference/$n-reference.log" > "$task_out/$r.tsv"
 cmp "$task_here/reference/expected$r.tsv" "$task_out/$r.tsv"
 od -An -v -tu1 "$task_here/reference/$n.aml" | awk '{for(i=1;i<=NF;i++){n++;sum=(sum+$i)%256;if(n>=5&&n<=8)header+=$i*256^(n-5)}} END{exit !(n>36 && n==header && sum==0)}'
done
cmp <(tail -c +37 "$task_here/reference/bank.aml") <(tail -c +37 "$task_here/reference/bank32.aml")
{
 printf 'with Firmware_Tables;\npackage Index_Width_Oracle is\n'
 for r in 64 32; do
  n=bank; if test "$r" = 32; then n=bank32; fi
  printf '   Table%s : constant Firmware_Tables.Bytes := [\n' "$r"
  od -An -v -tu1 "$task_here/reference/$n.aml" | awk '{for(i=1;i<=NF;i++){if(n++)printf ",";if(n%16==1)printf "\n      ";printf "%s",$i}} END{printf "\n   ];\n"}'
 done
 printf 'end Index_Width_Oracle;\n'
} > "$task_out/index_width_oracle.ads"
cmp "$task_out/index_width_oracle.ads" "$task_here/index_width_oracle.ads"
printf 'QWord data: 28 typed observations/revision; exact AML bodies, embedding and checksums verified.\n'
