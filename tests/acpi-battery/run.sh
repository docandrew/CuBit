#!/usr/bin/env bash
set -euo pipefail
here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
root=$(cd "$here/../.." && pwd)
cd "$root"
verify () {
 sha256sum --check --quiet "$here/inputs.sha256"
 cmp "$here/source-files.txt" <(while IFS= read -r dir; do
  test "$(realpath "$root/$dir")" = "$root/$dir"
  find "$dir" -maxdepth 1 \( -name '*.ads' -o -name '*.adb' \) -print
 done < "$here/source-dirs.txt" | sort)
 while IFS= read -r file; do test -f "$root/$file"; test "$(realpath "$root/$file")" = "$root/$file"; done < "$here/source-files.txt"
}
verify
if [ "${1:-}" = --validate-only ]; then printf '%s\n' 'Battery inputs verified; no build or oracle.'; exit 0; fi
test "$#" = 0
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
mkdir -p "$here/results"
out=$(mktemp -d "$here/results/run-XXXXXXXX")
printf '%s\n' "$out"
for tool in gprbuild gnatbind gnatlink; do
 tool_path=$(realpath "$(command -v "$tool")")
 case "$tool_path" in /nix/store/*) ;; *) exit 1;; esac
 printf '%s\n' "$tool_path" >> "$out/tools.paths"
 sha256sum "$tool_path" >> "$out/tools.sha256"
done
oracle=/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin
mkdir "$out/reference"
for tool in iasl acpiexec; do sha256sum "$oracle/$tool" >> "$out/tools.sha256"; done
nice -n 19 timeout --kill-after=2s 5 "$oracle/iasl" -p "$out/reference/battery" "$here/battery_observer.asl" > "$out/iasl.log" 2>&1
nice -n 19 timeout --kill-after=2s 5 "$oracle/acpiexec" -dr -b 'evaluate BAT0._HID; evaluate BAT0._BST; evaluate NAM0._HID; evaluate NAM0._BST; evaluate ORCL; evaluate SETM 9 1; evaluate BAT0._HID; evaluate BAT0._BST; evaluate RCNT; evaluate BAT0._STA; evaluate SETP 1; evaluate BAT0._STA; evaluate SETP 4; evaluate BAT0._STA' "$out/reference/battery.aml" > "$out/acpiexec.log" 2>&1
grep -Fx 'Compilation successful. 0 Errors, 0 Warnings, 0 Remarks, 16 Optimizations' "$out/iasl.log"
cat "$here/oracle-values.expected" "$here/presence-values.expected" > "$out/oracle-values.expected"
awk '/^  +\[(Integer|String|Package)\]/ {print}' "$out/acpiexec.log" > "$out/oracle-values.actual"
diff -u "$out/oracle-values.expected" "$out/oracle-values.actual"
test "$(grep -c '^Evaluation of .* returned object ' "$out/acpiexec.log")" = 11
test "$(grep -c '^No object was returned from evaluation of ' "$out/acpiexec.log")" = 3
! grep -E 'failed with status|AE_[A-Z_]+|Aborting method' "$out/acpiexec.log"
# Known ACPICA20260408 exit0 shutdown diagnostic remains visible in the receipt.
grep -Fx 'ACPI Error: 4 (0x4) Outstanding cache allocations (20260408/uttrack-902)' "$out/acpiexec.log"
sha256sum "$out/reference/battery.aml" > "$out/reference.sha256"
for profile in checked release; do
 for model in status observer; do
  build="$out/$model-$profile"
  mkdir -p "$build/obj" "$build/bin"
  if [ "$model" = status ]; then
   project="$here/$profile.gpr"; variable=BATTERY_STATUS_BUILD; main=battery_status_tests
   expected='battery status checks 14237'; alis=2; args=()
  else
   project="$here/observer_$profile.gpr"; variable=BATTERY_OBSERVER_BUILD; main=battery_observer_tests
   expected='ACPI battery observer checks 330'; alis=78; args=("$out/reference/battery.aml")
  fi
  nice -n 19 timeout --kill-after=2s 180 gprbuild -p -v -j1 -P "$project" "-X$variable=$build" 2>&1 | head -c 1048576 > "$out/$model-$profile-build.log"
  nice -n 19 timeout --kill-after=2s 30 "$build/bin/$main" "${args[@]}" 2>&1 | head -c 1048576 > "$out/$model-$profile-test.log"
  grep -Fx "$expected" "$out/$model-$profile-test.log"
  test "$(find "$build/obj" -maxdepth 1 -name '*.ali' ! -name 'b__*' | wc -l)" = "$alis"
  for ali in "$build/obj/"*.ali; do
   case "$ali" in */b__*) continue;; esac
   grep -Fxq 'A -gnatwe' "$ali"
   if [ "$profile" = checked ]; then
    grep -Fxq 'A -gnata' "$ali"; grep -Fxq 'A -gnato' "$ali"; ! grep -Fxq 'A -gnatp' "$ali"
   else grep -Fxq 'A -gnatp' "$ali"; ! grep -Fxq 'A -gnata' "$ali"; fi
  done
  sha256sum "$build/bin/$main" "$build/obj/"*.ali > "$out/$model-$profile-artifacts.sha256"
 done
done
verify
printf '%s\n' 'Both profiles, ACPICA values and input/profile checks passed.'
