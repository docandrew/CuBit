#!/usr/bin/env bash
set -euo pipefail
if [[ $# != 2 || $1 != --output-root ]]; then
  echo 'usage: prove.sh --output-root <private-directory>' >&2
  exit 2
fi
ulimit -s 65536
ulimit -c 0
ulimit -f 1024
here=$(cd -- "$(dirname -- "$0")" && pwd)
root=$(cd -- "$here/../.." && pwd)
out=$(mktemp -d -- "$2/gpe-proof-XXXXXXXX")
printf '%s\n' "$out"
files=("$root/userspace/services/acpi/events/acpi_gpe_lifecycle.ads"
       "$root/userspace/services/acpi/events/acpi_gpe_lifecycle.adb"
       "$here/gpe_flow_instance.ads" "$here/proof.gpr" "$here/prove.sh")
sha256sum -- "${files[@]}" > "$out/inputs.sha256"
cp -- "${files[@]}" "$out/"
proof_tool=$(command -v gnatprove)
readlink -f -- "$proof_tool" > "$out/tool-path.txt"
sha256sum -- "$(readlink -f -- "$proof_tool")" > "$out/tool.sha256"
"$proof_tool" --version > "$out/tool-version.txt" 2>&1
cd -- "$out"
mkdir proof
printf '%s\n' 'timeout --kill-after=5s 180s gnatprove -P proof.gpr -u gpe_flow_instance.ads --mode=prove --level=2 --timeout=5 --checks-as-errors=on -j1' > command.txt
set +e
timeout --kill-after=5s 180s "$proof_tool" -P proof.gpr -u gpe_flow_instance.ads \
  --mode=prove --level=2 --timeout=5 --checks-as-errors=on -j1 > proof/run.log 2>&1
status=$?
set -e
printf '%s\n' "$status" > status.txt
sha256sum --check inputs.sha256 > baseline-check.log
for input in "${files[@]}"; do cmp -- "$input" "$(basename -- "$input")"; done
cat proof/run.log
if [[ $status != 0 ]]; then exit "$status"; fi
test -s proof/obj/gnatprove/gnatprove.out
printf '%s\n' 'proof completed; see report and callback assumptions' > COMPLETE.txt
