#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_output=$(mktemp -d)
trap 'rm -rf -- "$task_output"' EXIT
(cd "$task_here/reference"; sha256sum --check --quiet evidence.sha256)
test "$(cat "$task_here/reference/status.tsv")" = $'compile\t0\nwidths\t0\nwidths32\t0'
for task_file in widths widths32; do
 test "$(rg -c '^Evaluation of .* returned object' "$task_here/reference/$task_file.log")" = 17
 ! rg -q 'Evaluation of .* failed|Aborting method|Could not resolve symbol' "$task_here/reference/$task_file.log"
 awk '/\[Integer\] =/{print "Integer",$NF} /\[Buffer\] Length 08 =/{for(i=1;i<=NF;i++)if($i=="0000:"){printf "Buffer";for(j=i+1;j<=i+8;j++)printf " %s",$j;print ""}}' "$task_here/reference/$task_file.log" > "$task_output/$task_file"
done
cat > "$task_output/expected64" <<'VALUES'
Integer 0000000000000000
Integer 00000000000000A5
Integer 000000000000C3A5
Integer 00000000C3E187A5
Integer D2B39475563718F1
Integer 0000000000000000
Integer 0000000000003412
Integer 0000000000000000
Integer 0000000078123456
Integer 0000000000000000
Integer 1188776655443322
Integer 0000000000000000
Integer 0000000000000000
Integer 000000000000F3FF
Integer 0000000000000000
Integer 0000000000000000
Integer 0000000000000300
VALUES
sed -e '5c\Buffer F1 18 37 56 75 94 B3 D2' -e '11c\Buffer 22 33 44 55 66 77 88 11' "$task_output/expected64" > "$task_output/expected32"
cmp "$task_output/expected64" "$task_output/widths"
cmp "$task_output/expected32" "$task_output/widths32"
cmp <(tail -c +37 "$task_here/reference/widths.aml") <(tail -c +37 "$task_here/reference/widths32.aml")
for task_file in widths.aml widths32.aml; do
 od -An -v -tu1 "$task_here/reference/$task_file" | awk '{for(i=1;i<=NF;i++){n++;sum=(sum+$i)%256}} END{exit !(n==422 && sum==0)}'
done
{
 printf 'with Firmware_Tables;\npackage Index_Width_Oracle is\n'
 for task_revision in 64 32; do
  task_file=widths.aml
  if test "$task_revision" = 32; then task_file=widths32.aml; fi
  printf '   Table%s : constant Firmware_Tables.Bytes := [\n' "$task_revision"
  od -An -v -tu1 "$task_here/reference/$task_file" | awk '{for(i=1;i<=NF;i++){if(n++)printf ",";if(n%16==1)printf "\n      ";printf "%s",$i}} END{printf "\n   ];\n"}'
 done
 printf 'end Index_Width_Oracle;\n'
} > "$task_output/index_width_oracle.ads"
cmp "$task_output/index_width_oracle.ads" "$task_here/index_width_oracle.ads"
printf '%s\n' 'Index widths: 17 typed observations per revision, embedded AML and checksums verified.'
