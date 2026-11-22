#!/usr/bin/env bash
set -euo pipefail
task_here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
task_root=$(cd "$task_here/../.." && pwd -P)
(cd "$task_root"; jq -r 'to_entries[]|.value+"  "+.key' "$task_here/reference-manifest.json" | sha256sum --check --quiet --strict)
task_temporary=$(mktemp)
trap 'rm -f "$task_temporary"' EXIT
awk '/^Evaluating / {key=$2; sub(/^\\/,"",key); sub(/\._[CP]RS$/,"",key)} {active=0; for(i=1;i<=NF;i++){if($i=="//")break; if($i~/^[0-9A-F][0-9A-F][0-9A-F][0-9A-F]:$/){active=1;continue} if(active && $i~/^[0-9A-F][0-9A-F]$/)v[key]=v[key] $i}} END{if(v["RCQ0"]!="4BF803087900")exit 1;for(i=0;i<20;i++){name=(i==19?"CLK0":(i==18?"PIN0":(i==17?"SBL0":(i==16?"GPI0":"DVC" substr("0123456789ABCDEF",i+1,1)))));printf "%s\t%s\n",name,v[name]}}' "$task_here/reference/reference.log" > "$task_temporary"
cmp "$task_here/reference/expected.tsv" "$task_temporary"
printf 'Twenty retained normal ACPICA resource vectors verified; diagnostics remain recorded.\n'
awk '$1=="Evaluating" {active=($2~/RCQ0[.]CNT0$/); if(active)n++} active && $1=="[Integer]" {v[n]=$NF;active=0} END{exit !(n==2 && v[1]=="0000000000000000" && v[2]=="0000000000000001")}' "$task_here/reference/reference.log"
printf 'Retained normal ACPICA resource-reader counter/buffer/counter observations verified.\n'
bash "$task_here/check_pci_reference.sh"
