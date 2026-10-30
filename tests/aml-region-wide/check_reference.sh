#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_out=$(mktemp -d)
trap 'rm -rf -- "$task_out"' EXIT
for r in 64 32; do
 n=region; if test "$r" = 32; then n=region32; fi
 (cd "$task_here/reference"; sha256sum --check --quiet "$n-evidence.sha256"; sha256sum --check --quiet "$n-inputs.sha256")
 test "$(cat "$task_here/reference/$n-status.tsv")" = $'compile\t0\nruntime\t0'
 rg -q 'Compilation successful. 0 Errors, 0 Warnings, 0 Remarks' "$task_here/reference/$n-compile.log"
 ! rg -q 'Evaluation of .* failed|Aborting method|Could not resolve symbol' "$task_here/reference/$n-reference.log"
 awk -f "$task_here/extract_observations.awk" "$task_here/reference/$n-reference.log" > "$task_out/$r.tsv"
 cmp "$task_here/reference/$n-expected.tsv" "$task_out/$r.tsv"
 awk -F '\t' 'BEGIN{split("RD80 RU80 RP73 RW80 RQ80 RB80 RBU0 RBP3 RBW0 RBQ0 SD80 SB80 SDU0 SBU0 SDO0 SBO0 SDZ0 SBZ0 SDIN SBIN SDSH SBSH SW80 SQ80 SBW0 SBQ0",names," ")} NR%2==1 {if($1!="INIT"||$2!="Integer"||$3!="0000000000000000")exit 1} NR%2==0 {if($1!=names[NR/2]||$2!="Buffer"||split($3,b," ")!=10)exit 1} END{if(NR!=52)exit 1}' "$task_out/$r.tsv"
 od -An -v -tu1 "$task_here/reference/$n.aml" | awk '{for(i=1;i<=NF;i++){n++;sum=(sum+$i)%256;if(n>=5&&n<=8)header+=$i*256^(n-5)}} END{exit !(n==1074 && n==header && sum==0)}'
done
cmp "$task_out/64.tsv" "$task_out/32.tsv"
cmp <(tail -c +37 "$task_here/reference/region.aml") <(tail -c +37 "$task_here/reference/region32.aml")
{
 printf 'with Firmware_Tables;\npackage Index_Width_Oracle is\n'
 for r in 64 32; do
  n=region; if test "$r" = 32; then n=region32; fi
  printf '   Table%s : constant Firmware_Tables.Bytes := [\n' "$r"
  od -An -v -tu1 "$task_here/reference/$n.aml" | awk '{for(i=1;i<=NF;i++){if(n++)printf ",";if(n%16==1)printf "\n      ";printf "%s",$i}} END{printf "\n   ];\n"}'
 done
 awk -F '\t' '$2=="Buffer" {printf "   %s : constant Firmware_Tables.Bytes := [",$1; n=split($3,b," ");for(i=1;i<=n;i++)printf "%s16#%s#",(i>1?",":""),b[i];print "];"}' "$task_here/reference/region-expected.tsv"
 printf 'end Index_Width_Oracle;\n'
} > "$task_out/index_width_oracle.ads"
cmp "$task_out/index_width_oracle.ads" "$task_here/index_width_oracle.ads"
printf 'Region Fields:52 typed observations/revision; exact returned Buffers, normal compiles, table/checksum and embedding verified.\n'
