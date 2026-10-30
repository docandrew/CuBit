#!/usr/bin/env bash
set -euo pipefail
out=${1:?proof run directory}
for unit in acpi_battery_status hid_i2c_wire; do
 spark="$out/$unit/obj/gnatprove/$unit.spark"
 if [ "$unit" = acpi_battery_status ]; then checks=8; flow=3; goals=25; else checks=20; flow=10; goals=41; fi
 jq -e --argjson checks "$checks" --argjson flow "$flow" --argjson goals "$goals" '
  (.proof|length)==$checks and (.flow|length)==$flow
  and all(.proof[];.severity=="info" and .how_proved=="prover")
  and all(.flow[];.severity=="info") and all(.spark[];.=="all")
  and (.pragma_assume|length)==0 and (.skip_proof|length)==0
  and (.skip_flow_proof|length)==0 and .stop_reason=="STOP_REASON_NONE"
  and ([.proof[].check_tree[]|..|objects|select(has("result"))|.result] as $r
    | ($r|length)==$goals and all($r[];.=="Valid"))' "$spark" >/dev/null
 ! rg --quiet 'warning:|error:|medium:|high:|low:|not proved' "$out/$unit.log"
done
printf '%s\n' 'Two pure units: 28 proved checks /66 Valid goals +13 flow checks; no skips, Assume or unproved.'
