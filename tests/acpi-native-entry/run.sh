#!/usr/bin/env bash
set -euo pipefail
here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
root=$(cd "$here/../.." && pwd)
cd "$root"
verify () {
 sha256sum --check --quiet "$here/inputs.sha256"
 cmp "$here/source-files.txt" <(find tests/acpi-native-entry/fixtures/actual -maxdepth 1 \( -name '*.ads' -o -name '*.adb' \) -type f -print | LC_ALL=C sort)
 while IFS= read -r file; do test "$(realpath "$file")" = "$root/$file"; done < "$here/source-files.txt"
 while IFS=$'\t' read -r copy original; do
  test "$(realpath "$copy")" = "$root/$copy"
  if ! test -f "$original" || ! cmp -s "$copy" "$original"; then
   printf 'ACPI API mirror pending/mismatch: %s\n' "$original" >&2
   return 1
  fi
  test "$(realpath "$original")" = "$root/$original"
 done < "$here/mirrors.tsv"
}
verify
if [ "${1:-}" = --validate-only ]; then
 test "$#" = 1
 printf '%s\n' 'Native ACPI API fixtures and repository mirrors verified; no compile.'
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
printf '%s\n' "$out"
for tool in gprbuild; do
 path=$(realpath "$(command -v "$tool")")
 case "$path" in /nix/store/*) ;; *) exit 1;; esac
 sha256sum "$path" >> "$out/tools.sha256"
done
mapfile -t units < "$here/selections.txt"
test "${#units[@]}" = 13
nice -n 19 timeout --kill-after=2s 180 gprbuild -p -c -u -j1 -P "$here/$profile.gpr" "-XNATIVE_API_BUILD=$out" "${units[@]}" 2>&1 | head -c 1048576 > "$out/compile.log"
test "$(find "$out" -maxdepth 1 -name '*.ali' | wc -l)" = 12
test "$(find "$out" -maxdepth 1 -name '*.o' | wc -l)" = 0
for ali in "$out/"*.ali; do
 grep -Fxq 'A -gnatc' "$ali"; grep -Fxq 'A -gnatwe' "$ali"
 if [ "$profile" = checked ]; then
  grep -Fxq 'A -gnata' "$ali"; grep -Fxq 'A -gnato' "$ali"; ! grep -Fxq 'A -gnatp' "$ali"
 else grep -Fxq 'A -gnatp' "$ali"; ! grep -Fxq 'A -gnata' "$ali"; fi
 case "$ali" in */startup.ali) grep -Fxq 'A -gnatwR' "$ali";; *) ! grep -Fxq 'A -gnatwR' "$ali";; esac
done
sha256sum "$out/"*.ali "$out/compile.log" > "$out/artifacts.sha256"
verify
printf '%s\n' 'Native ACPI hosted semantic API check passed; no objects/link/boot/hardware claim.'
