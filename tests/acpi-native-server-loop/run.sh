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
 done < "$here/source-dirs.txt" | LC_ALL=C sort)
 while IFS=$'\t' read -r copy original; do
  test "$(realpath "$copy")" = "$root/$copy"
  test "$(realpath "$original")" = "$root/$original"
  cmp "$copy" "$original"
 done < "$here/mirrors.tsv"
}
verify
if [ "${1:-}" = --validate-only ]; then
 test "$#" = 1
 printf '%s\n' 'Direct ACPI observer runner inputs and mirrors verified; no build.'
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
nice -n 19 timeout --kill-after=2s 180 gprbuild -p -j1 -P "$here/$profile.gpr" "-XOBSERVER_BUILD=$out" 2>&1 | head -c 1048576 > "$out/build.log"
nice -n 19 timeout --kill-after=2s 30 "$out/bin/observer_tests" 2>&1 | head -c 1048576 > "$out/test.log"
grep -Fx 'Observer and native runner checks 760' "$out/test.log"
count=0
for ali in "$out/obj/"*.ali; do
 case "$ali" in */b__*) continue;; esac
 grep -Fxq 'A -gnatwe' "$ali"
 if [ "$profile" = checked ]; then
  grep -Fxq 'A -gnata' "$ali"; grep -Fxq 'A -gnato' "$ali"; ! grep -Fxq 'A -gnatp' "$ali"
 else grep -Fxq 'A -gnatp' "$ali"; ! grep -Fxq 'A -gnata' "$ali"; fi
 count=$((count + 1))
done
test "$count" = 112
sha256sum "$out/bin/observer_tests" "$out/obj/"*.ali > "$out/artifacts.sha256"
verify
printf '%s\n' 'Hosted observer runner passed; kernel/native/hardware behavior unvalidated.'
