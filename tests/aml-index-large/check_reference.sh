#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
task_out=$(mktemp -d)
trap 'rm -rf -- "$task_out"' EXIT
(cd "$task_here/reference";sha256sum --check --quiet evidence.sha256)
test "$(cat "$task_here/reference/status.tsv")" = $'compile\t0\nruntime\t0'
rg -q 'Compilation successful. 0 Errors, 0 Warnings, 0 Remarks' "$task_here/reference/compile.log"
awk -f "$task_here/extract_observations.awk" "$task_here/reference/reference.log" > "$task_out/actual"
cmp "$task_here/reference/expected.tsv" "$task_out/actual"
rg -q 'Possible buffer overflow within AML Debugger buffer \(size 0x4000 needed 0x4018\)' "$task_here/reference/reference.log"
od -An -v -tu1 "$task_here/reference/bulk.aml" | awk '{for(i=1;i<=NF;i++){n++;sum=(sum+$i)%256;if(n>=5&&n<=8)header+=$i*256^(n-5)}}END{exit !(n>36&&n==header&&sum==0)}'
{ printf 'with Firmware_Tables;\npackage Index_Width_Oracle is\n   Table64 : constant Firmware_Tables.Bytes := [\n'; od -An -v -tu1 "$task_here/reference/bulk.aml" | awk '{for(i=1;i<=NF;i++){if(n++)printf ",";if(n%16==1)printf "\n      ";printf "%s",$i}}END{printf "\n   ];\n"}'; printf 'end Index_Width_Oracle;\n'; } > "$task_out/oracle.ads"
cmp "$task_out/oracle.ads" "$task_here/index_width_oracle.ads"
printf 'ACPICA complete512/4096byte returns verified;16KiB debugger result unavailable, not a conformance pass.\n'
