#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
task_tmp=$(mktemp -d)
trap 'rm -rf "$task_tmp"' EXIT
for task_log in reference.log reference32.log; do
 awk -f "$task_here/extract_pci_routes.awk" "$task_here/reference/$task_log" > "$task_tmp/$task_log.tsv"
 cmp "$task_here/reference/pci-routes.tsv" "$task_tmp/$task_log.tsv"
 awk '
 /^Evaluating / {method=($2~/PIRQ[.]RTPK$/ || $2~/PIRQ[.]RPNM$/);counter=($2~/PIRQ[.]CNTM$/);call=($2~/PIRQ[.]RUNC$/)}
 method && /\[Object Reference\]/ {for(i=1;i<=NF;i++)if($i=="Name" && $(i+1)=="MTH0" && $(i+2)=="Method")m++}
 counter && /\[Integer\]/ {c[++n]=$NF}
 call && /\[Integer\]/ {v=$NF;k++}
 END {exit !(m==2 && n==2 && c[1]=="0000000000000000" && c[2]=="0000000000000001" && k==1 && v=="0000000000000001")}
 ' "$task_here/reference/$task_log"
done
cmp <(head -c 8 "$task_here/reference/resources.aml") <(head -c 8 "$task_here/reference/resources32.aml")
cmp <(tail -c +11 "$task_here/reference/resources.aml") <(tail -c +11 "$task_here/reference/resources32.aml")
test "$(od -An -tu1 -j8 -N1 "$task_here/reference/resources.aml" | tr -d ' ')" = 2
test "$(od -An -tu1 -j8 -N1 "$task_here/reference/resources32.aml" | tr -d ' ')" = 1
for task_table in resources.aml resources32.aml; do
 od -An -v -tu1 "$task_here/reference/$task_table" | awk '{for(i=1;i<=NF;i++)s=(s+$i)%256} END{exit(s!=0)}'
done
awk '
 /^Evaluating / {active=($2~/PIRQ[.]BADT$/);if(active){r=-1;f=0}}
 active && /^    \[/ {r++;f=0;next}
 active && /^      \[/ {if($1=="[Integer]")v[r "," f]=$NF;f++}
 END {exit !(r==17 && v["12,3"]=="0000000000000000" && v["13,0"]=="000000000001FFFF" && v["14,1"]=="0000000000000000")}
 ' "$task_here/reference/reference32.log"
printf 'Retained ACPICA PCI routes/method references/calls and 32-bit normalization verified.\n'
