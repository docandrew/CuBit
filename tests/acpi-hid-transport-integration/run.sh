#!/usr/bin/env bash
# Keep source and artifact inventories independent of the invoking locale.
export LC_ALL=C
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
 while IFS= read -r file; do
  test -f "$root/$file"
  test "$(realpath "$root/$file")" = "$root/$file"
 done < "$here/source-files.txt"
}
verify
if [ "${1:-}" = --validate-only ]; then
 printf '%s\n' 'ACPI-to-HID integration inputs verified; no build or oracle.'
 exit 0
fi
test "$#" = 0
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
mkdir -p "$here/results"
if [ -n "${ACPI_HID_TEST_RESUME:-}" ]; then
 out=$(realpath -- "$ACPI_HID_TEST_RESUME")
 test "$(dirname -- "$out")" = "$(realpath -- "$here/results")"
 case "$(basename -- "$out")" in run-*) ;; *) exit 2;; esac
else
 out=$(mktemp -d "$here/results/run-XXXXXXXX")
fi
printf '%s\n' "$out"
oracle=/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin
for tool in gprbuild gnatbind gnatlink; do
 tool_path=$(realpath "$(command -v "$tool")")
 case "$tool_path" in /nix/store/*) ;; *) exit 1;; esac
 sha256sum "$tool_path" >> "$out/tools.sha256"
done
sha256sum "$oracle/iasl" "$oracle/acpiexec" >> "$out/tools.sha256"
nice -n 19 timeout --kill-after=2s 5 "$oracle/iasl" -p "$out/discovery" "$here/integration.asl" > "$out/iasl.log" 2>&1
nice -n 19 timeout --kill-after=2s 5 "$oracle/acpiexec" -dr -b 'evaluate OREG; evaluate SETM 9; evaluate OREG; evaluate SETM 10; evaluate OREG; evaluate SETM 8; evaluate TPD0._STA; evaluate SETM 11; evaluate TPD0._STA; evaluate SETM 12; evaluate TPD0._STA' "$out/discovery.aml" > "$out/acpiexec.log" 2>&1
grep -Fx 'Compilation successful. 0 Errors, 1 Warnings, 1 Remarks, 4 Optimizations' "$out/iasl.log"
grep -F 'Warning  3150' "$out/iasl.log"
grep -F 'Remark   2095' "$out/iasl.log"
awk '/^  +\[Integer\]/ {print}' "$out/acpiexec.log" > "$out/oracle-values.actual"
diff -u "$here/oracle-values.expected" "$out/oracle-values.actual"
test "$(grep -c '^Evaluation of .* returned object ' "$out/acpiexec.log")" = 6
test "$(grep -c '^No object was returned from evaluation of ' "$out/acpiexec.log")" = 5
! grep -E 'failed with status|AE_[A-Z_]+|Aborting method|ACPI Warning:' "$out/acpiexec.log"
# Preserve ACPICA's known exit0 cache-allocation shutdown diagnostic.
grep -Fx 'ACPI Error: 4 (0x4) Outstanding cache allocations (20260408/uttrack-902)' "$out/acpiexec.log"
for profile in checked release; do
 build="$out/$profile"
 mkdir -p "$build/obj" "$build/bin"
 nice -n 19 timeout --kill-after=2s 180 gprbuild -p -v -j1 -P "$here/$profile.gpr" "-XHID_I2C_INTEGRATION_BUILD=$build" 2>&1 | head -c 1048576 > "$out/$profile-build.log"
 nice -n 19 timeout --kill-after=2s 30 "$build/bin/hid_discovery_tests" "$out/discovery.aml" 2>&1 | head -c 1048576 > "$out/$profile-test.log"
 cat "$out/$profile-test.log"
 grep -Fx 'HID I2C discovery checks 428' "$out/$profile-test.log"
 find "$build/obj" -maxdepth 1 -name '*.ali' ! -name 'b__*' -printf '%f\n' | LC_ALL=C sort > "$out/$profile-local-units.actual"
 diff -u "$here/local-units.expected" "$out/$profile-local-units.actual"
 for ali in "$build/obj/"*.ali; do
  case "$ali" in */b__*) continue;; esac
  grep -Fxq 'A -gnatwe' "$ali"
  if [ "$profile" = checked ]; then
   grep -Fxq 'A -gnata' "$ali"; grep -Fxq 'A -gnato' "$ali"; ! grep -Fxq 'A -gnatp' "$ali"
  else grep -Fxq 'A -gnatp' "$ali"; ! grep -Fxq 'A -gnata' "$ali"; fi
 done
 sha256sum "$build/bin/hid_discovery_tests" "$build/obj/"*.ali > "$out/$profile-artifacts.sha256"
done
verify
printf '%s\n' 'Integration profiles passed; native hardware remains unvalidated.'
