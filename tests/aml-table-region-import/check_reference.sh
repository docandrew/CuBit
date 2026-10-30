#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_output=$(mktemp -d)
trap 'rm -rf -- "$task_output"' EXIT
(
 cd "$task_here/reference"
 sha256sum --check --quiet evidence.sha256
)
test "$(cat "$task_here/reference/status.tsv")" = $'compile\t0\nruntime\t0'
# acpiexec can exit zero even after AML evaluation errors. Check observations,
# not just process status. Retain the original cache-allocation diagnostics.
sed -n 's/^  \[Integer\] = //p' "$task_here/reference/reference.log" > "$task_output/integers"
printf '%s\n' 0000000000000003 0000000000000002 0000000000000003 0000000000000002 0000000000000003 0000000000000002 0000000000000003 > "$task_output/expected"
cmp "$task_output/expected" "$task_output/integers"
test "$(rg -c '^Evaluation of .* returned object' "$task_here/reference/reference.log")" = 7
! rg -q 'Evaluation of .* failed|Aborting method|Could not resolve symbol' "$task_here/reference/reference.log"
# Check the embedded service input against the exact normally compiled AML.
{
 printf 'with Firmware_Tables;\npackage Table_Region_Oracle is\n   Raw_Table : constant Firmware_Tables.Bytes := [\n'
 od -An -v -tu1 "$task_here/reference/selectors.aml" | awk '{for(i=1;i<=NF;i++){if(n++)printf ",";if(n%16==1)printf "\n      ";printf "%s",$i}} END{printf "\n   ];\nend Table_Region_Oracle;\n"}'
} > "$task_output/table_region_oracle.ads"
cmp "$task_output/table_region_oracle.ads" "$task_here/table_region_oracle.ads"
od -An -v -tu1 "$task_here/reference/selectors.aml" | awk '{for(i=1;i<=NF;i++){n++;sum=(sum+$i)%256}} END{exit !(n==241 && sum==0)}'
printf '%s\n' 'Table region import reference: exact AML, seven integer observations and checksum verified.'
