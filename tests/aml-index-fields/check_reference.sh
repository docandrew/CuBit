#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_output=$(mktemp -d)
trap 'rm -rf -- "$task_output"' EXIT
(cd "$task_here/reference"; sha256sum --check --quiet evidence.sha256)
test "$(cat "$task_here/reference/status.tsv")" = $'compile\t0\nruntime\t0'
sed -n 's/^  \[Integer\] = //p' "$task_here/reference/reference.log" > "$task_output/integers"
printf '%s\n' 0000000000A503A5 0000000000005A01 0000000000003504 0000000000000035 0000000000000000 0000000000000000 000000000000005A 000000000000005A 0000000000000002 > "$task_output/expected"
cmp "$task_output/expected" "$task_output/integers"
test "$(rg -c '^Evaluation of .* returned object' "$task_here/reference/reference.log")" = 9
! rg -q 'Evaluation of .* failed|Aborting method|Could not resolve symbol' "$task_here/reference/reference.log"
{
 printf 'with Firmware_Tables;\npackage Index_Field_Oracle is\n   Raw_Table : constant Firmware_Tables.Bytes := [\n'
 od -An -v -tu1 "$task_here/reference/index_field.aml" | awk '{for(i=1;i<=NF;i++){if(n++)printf ",";if(n%16==1)printf "\n      ";printf "%s",$i}} END{printf "\n   ];\nend Index_Field_Oracle;\n"}'
} > "$task_output/index_field_oracle.ads"
cmp "$task_output/index_field_oracle.ads" "$task_here/index_field_oracle.ads"
od -An -v -tu1 "$task_here/reference/index_field.aml" | awk '{for(i=1;i<=NF;i++){n++;sum=(sum+$i)%256}} END{exit !(n==351 && sum==0)}'
printf '%s\n' 'IndexField reference: nine successful observations, exact embedded AML and checksum verified.'
