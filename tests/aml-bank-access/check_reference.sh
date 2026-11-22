#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_root=$(cd -- "$task_here/../.." && pwd)
case "${task_here##*/}" in
 aml-bank-declarations) task_package=Bank_Declaration_Oracle; task_file=bank_declaration_oracle.ads; task_extent=393;;
 aml-bank-access) task_package=Bank_Access_Oracle; task_file=bank_access_oracle.ads; task_extent=429;;
 aml-bank-module) task_package=Bank_Module_Oracle; task_file=bank_module_oracle.ads; task_extent=236;;
 *) exit 2;;
esac
task_output=$(mktemp -d)
trap 'rm -rf -- "$task_output"' EXIT
(cd "$task_here/reference"; sha256sum --check --quiet evidence.sha256)
test "$(cat "$task_here/reference/status.tsv")" = $'compile\t0\nruntime\t0'
awk -f "$task_root/tests/aml-bank-declarations/extract_observations.awk" "$task_here/reference/reference.log" > "$task_output/actual.tsv"
cmp "$task_here/reference/expected.tsv" "$task_output/actual.tsv"
{
 printf 'with Firmware_Tables;\npackage %s is\n   Raw_Table : constant Firmware_Tables.Bytes := [\n' "$task_package"
 od -An -v -tu1 "$task_here/reference/bank.aml" | awk '{for(i=1;i<=NF;i++){if(n++)printf ",";if(n%16==1)printf "\n      ";printf "%s",$i}} END{printf "\n   ];\n"}'
 printf 'end %s;\n' "$task_package"
} > "$task_output/oracle.ads"
cmp "$task_output/oracle.ads" "$task_here/$task_file"
od -An -v -tu1 "$task_here/reference/bank.aml" | awk -v extent="$task_extent" '{for(i=1;i<=NF;i++){n++;sum=(sum+$i)%256;if(n>=5&&n<=8)header+=$i*256^(n-5)}} END{exit !(n==extent&&header==n&&sum==0)}'
awk -F '\t' '$2=="Integer"{values++}$2=="Error"{errors++}END{printf "Cached ACPICA: %d Integer returns, %d expected errors; exact ordered observations, embedded AML and checksum verified.\n",values,errors}' "$task_output/actual.tsv"
if test "${task_here##*/}" = aml-bank-module; then
 printf '%s\n' 'Module lifecycle intentionally differs: ACPICA CNT0=3/bank=7; CuBit policy CNT0=1/bank=3. This checker validates retained ACPICA observations, not compatibility of those values.'
fi
