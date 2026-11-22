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
 while IFS= read -r file; do test -f "$root/$file"; test "$(realpath "$root/$file")" = "$root/$file"; done < "$here/source-files.txt"
}
verify
if [ "${1:-}" = --validate-only ]; then printf '%s\n' 'HID-I2C startup inputs verified; no build or oracle.'; exit 0; fi
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
for profile in checked release; do
 build="$out/$profile"
 mkdir -p "$build/obj" "$build/bin"
 nice -n 19 timeout --kill-after=2s 180 gprbuild -p -v -j1 -P "$here/$profile.gpr" "-XHID_I2C_BRING_UP_BUILD=$build" 2>&1 | head -c 1048576 > "$out/$profile-build.log"
 nice -n 19 timeout --kill-after=2s 30 "$build/bin/bring_up_tests" 2>&1 | head -c 1048576 > "$out/$profile-test.log"
 grep -Fx 'HID I2C bring up checks 1992' "$out/$profile-test.log"
 test "$(find "$build/obj" -maxdepth 1 -name '*.ali' ! -name 'b__*' | wc -l)" = 4
 for ali in "$build/obj/"*.ali; do
  case "$ali" in */b__*) continue;; esac
  grep -Fxq 'A -gnatwe' "$ali"
  if [ "$profile" = checked ]; then
   grep -Fxq 'A -gnata' "$ali"; grep -Fxq 'A -gnato' "$ali"; ! grep -Fxq 'A -gnatp' "$ali"
  else grep -Fxq 'A -gnatp' "$ali"; ! grep -Fxq 'A -gnata' "$ali"; fi
 done
 sha256sum "$build/bin/bring_up_tests" "$build/obj/"*.ali > "$out/$profile-artifacts.sha256"
done
verify
printf '%s\n' 'Both profiles and input/profile checks passed.'
