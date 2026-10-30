#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "$0")" && pwd)
(cd "$task_here/reference"; sha256sum --check --quiet evidence.sha256)
test "$(cat "$task_here/reference/status.tsv")" = $'compile\t0\nruntime\t0'
rg -q 'Compilation successful. 0 Errors, 0 Warnings, 0 Remarks' "$task_here/reference/compile.log"
! rg -q 'Evaluation of .* failed|Aborting method|Could not resolve symbol' "$task_here/reference/reference.log"
task_values=$(awk '/^Evaluating /{name=$2} /^  \[Integer\] = /{print name " " $NF}' "$task_here/reference/reference.log")
test "$task_values" = $'\\GETV 0000000000000000\n\\DEV0._STA 000000000000000F\n\\DEV0._INI 0000000000000000\n\\GETV 000000000000005A'
od -An -v -tu1 "$task_here/reference/startup.aml" | awk '{for(i=1;i<=NF;i++){n++;sum=(sum+$i)%256;if(n>=5&&n<=8)header+=$i*256^(n-5)}}END{exit !(n==148&&header==n&&sum==0)}'
task_out=$(mktemp)
trap 'rm -f -- "$task_out"' EXIT
{
 printf 'with Firmware_Tables;\nwith Interfaces;\npackage Bootstrap_Driver_Oracle is\n   subtype Bytes is Firmware_Tables.Bytes;\n   Expected : constant Interfaces.Unsigned_64 := 16#5A#;\n   Raw_Table : constant Bytes := [\n'
 od -An -v -tu1 "$task_here/reference/startup.aml" | awk '{for(i=1;i<=NF;i++){if(n++)printf ",";if(n%16==1)printf "\n      ";printf "%s",$i}}END{printf "\n   ];\n"}'
 printf 'end Bootstrap_Driver_Oracle;\n'
} > "$task_out"
if test "${1:-}" = --generate; then cp "$task_out" "$task_here/bootstrap_driver_oracle.ads"; fi
cmp "$task_out" "$task_here/bootstrap_driver_oracle.ads"
printf 'Startup reference:4ordered typed observations,148B extent/checksum and exact embedding verified.\n'
