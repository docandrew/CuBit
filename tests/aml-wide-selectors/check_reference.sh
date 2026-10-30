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
 awk -v revision="$r" -F '\t' 'BEGIN{split("INIT BRED INIT BWRT INIT NRED INIT NWRT INIT WFUL INIT WONE INIT WZER",names," ")}
 {if($1!=names[NR])exit 1
  if(NR==10||NR==12||NR==14){
   want=(NR==10?"5A 00 00 00 00 00 00 00 00 00":NR==12?"D7 FF FF FF FF FF FF FF 00 00":"10 00 00 00 00 00 00 00 00 00")
   if($2!="Buffer"||$3!=want)exit 1
  }else if($2!="Integer"||$3!=(NR==2?"00000000000000B0":NR==6?(revision==64?"00000000000000B4":"0000000000000000"):"0000000000000000"))exit 1
 } END{if(NR!=14)exit 1}' "$task_out/$r.tsv"
 od -An -v -tu1 "$task_here/reference/$n.aml" | awk '{for(i=1;i<=NF;i++){n++;sum=(sum+$i)%256;if(n>=5&&n<=8)header+=$i*256^(n-5)}} END{exit !(n>36 && n==header && sum==0)}'
done
# AML32 initialization zero-extends its four-byte Integer stores.
{
 printf 'with Firmware_Tables;\nwith AML_Decode;\npackage Index_Width_Oracle is\n'
 for r in 64 32; do
  n=region; if test "$r" = 32; then n=region32; fi
  printf '   Table%s : constant Firmware_Tables.Bytes := [\n' "$r"
  od -An -v -tu1 "$task_here/reference/$n.aml" | awk '{for(i=1;i<=NF;i++){if(n++)printf ",";if(n%16==1)printf "\n      ";printf "%s",$i}} END{printf "\n   ];\n"}'
 done
 for r in 64 32; do
  n=region; if test "$r" = 32; then n=region32; fi
  awk -v r="$r" -F '\t' '$1!="INIT"&&$2=="Integer" {printf "   %s%s : constant AML_Decode.Integer_Value := 16#%s#;\n",$1,r,$3}
   $2=="Buffer" {printf "   %s%s : constant Firmware_Tables.Bytes := [",$1,r;n=split($3,b," ");for(i=1;i<=n;i++)printf "%s16#%s#",(i>1?",":""),b[i];print "];"}' "$task_here/reference/$n-expected.tsv"
 done
 printf 'end Index_Width_Oracle;\n'
} > "$task_out/index_width_oracle.ads"
if test "${1:-}" = --generate; then cp "$task_out/index_width_oracle.ads" "$task_here/index_width_oracle.ads"; fi
cmp "$task_out/index_width_oracle.ads" "$task_here/index_width_oracle.ads"
printf 'Wide selectors/data:14 ordered typed observations/revision; normal compiles and exact tables/checksums/embedding verified.\n'
