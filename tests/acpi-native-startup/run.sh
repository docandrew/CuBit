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
  test "$(realpath "$dir")" = "$root/$dir"
  find "$dir" -maxdepth 1 \( -name '*.ads' -o -name '*.adb' \) -type f -print
 done < "$here/source-dirs.txt" | sort)
 while IFS= read -r file; do test "$(realpath "$file")" = "$root/$file"; done < "$here/source-files.txt"
 while IFS=$'\t' read -r copy original; do
  test "$(realpath "$copy")" = "$root/$copy"
  test "$(realpath "$original")" = "$root/$original"
  cmp "$copy" "$original"
 done < "$here/mirrors.tsv"
 test "$(wc -l < "$here/cases.tsv")" = 88
}
verify
if [ "${1:-}" = --validate-only ]; then
 test "$#" = 1
 printf '%s\n' 'Native ACPI owner inputs/mirrors/cases verified; no build.'
 exit 0
fi
profile=release
if [ "${1:-}" = --mode ]; then test "$#" = 2; profile=$2; else test "$#" = 0; fi
case "$profile" in checked|release) ;; *) exit 2;; esac
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
mkdir -p "$here/results"
out=$(mktemp -d "$here/results/$profile-XXXXXXXX")
mkdir -p "$out/obj" "$out/bin"
printf '%s\n' "$out"
for tool in gprbuild gnatbind gnatlink; do
 path=$(realpath "$(command -v "$tool")")
 case "$path" in /nix/store/*) ;; *) exit 1;; esac
 sha256sum "$path" >> "$out/tools.sha256"
done
nice -n 19 timeout --kill-after=2s 180 gprbuild -p -j1 -P "$here/$profile.gpr" "-XOWNER_BUILD=$out" 2>&1 | head -c 1048576 > "$out/build.log"
case_number=0
while IFS=$'\t' read -r scenario width broken; do
 case_number=$((case_number + 1))
 log="$out/case-$case_number.log"
 nice -n 19 timeout --kill-after=2s 30 "$out/bin/owner_tests" "$scenario" "$width" "$broken" 2>&1 | head -c 1048576 > "$log"
 test "$(wc -l < "$log")" = 1
 grep -Eq '^Native owner checks [0-9]+$' "$log"
 printf '%s %s %s: %s\n' "$scenario" "$width" "$broken" "$(cat "$log")" >> "$out/test.log"
done < "$here/cases.tsv"
test "$case_number" = 88
nice -n 19 timeout --kill-after=2s 30 "$out/bin/notes_tests" > "$out/notes.log" 2>&1
grep -Fx 'Module note checks 31' "$out/notes.log"
count=0
for ali in "$out/obj/"*.ali; do
 case "$ali" in */b__*) continue;; esac
 grep -Fxq 'A -gnatwe' "$ali"
 if [ "$profile" = checked ]; then
  grep -Fxq 'A -gnata' "$ali"; grep -Fxq 'A -gnato' "$ali"; ! grep -Fxq 'A -gnatp' "$ali"
 else grep -Fxq 'A -gnatp' "$ali"; ! grep -Fxq 'A -gnata' "$ali"; fi
 count=$((count + 1))
done
test "$count" = 131
printf 'Source ALIs: %s\n' "$count"
total=$(awk '{sum += $NF} END {print sum}' "$out/test.log")
test "$total" = 1892
printf 'Native owner total checks %s\n' "$total"
sha256sum "$out/bin/owner_tests" "$out/bin/notes_tests" "$out/obj/"*.ali "$out/test.log" "$out/notes.log" > "$out/artifacts.sha256"
verify
printf '%s\n' 'Hosted native owner passed88startup cases and one diagnostic cursor process; real runtime/kernel behavior unvalidated.'
