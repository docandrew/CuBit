#!/usr/bin/env bash
set -euo pipefail
here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
root=$(cd "$here/../.." && pwd)
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
cd "$root"
sha256sum --check --quiet "$here/inputs.sha256"
mkdir -p "$here/results"
out=$(mktemp -d "$here/results/run-XXXXXXXX")
printf '%s\n' "$out"
for tool in gprbuild gnatbind gnatlink; do
 tool_path=$(command -v "$tool")
 realpath "$tool_path" >> "$out/tools.paths"
 sha256sum "$(realpath "$tool_path")" >> "$out/tools.sha256"
done
for profile in checked release; do
 build="$out/$profile"
 mkdir -p "$build/obj" "$build/bin"
 timeout --kill-after=2s 180 gprbuild -p -v -j1 \
   -P "$here/descriptor_$profile.gpr" "-XHID_DESCRIPTOR_BUILD=$build" \
   2>&1 | head -c 1048576 > "$out/$profile-build.log"
 timeout --kill-after=2s 30 "$build/bin/descriptor_tests" \
   2>&1 | head -c 1048576 > "$out/$profile-test.log"
 test "$(cat "$out/$profile-test.log")" = 'HID-I2C descriptor checks: 458785'
 for unit in hid_i2c_descriptors descriptor_tests; do
  ali="$build/obj/$unit.ali"
  if test "$profile" = checked; then
   rg --quiet --fixed-strings --line-regexp 'A -gnata' "$ali"
   rg --quiet --fixed-strings --line-regexp 'A -gnato' "$ali"
   ! rg --quiet --fixed-strings --line-regexp 'A -gnatp' "$ali"
  else
   rg --quiet --fixed-strings --line-regexp 'A -gnatp' "$ali"
   ! rg --quiet --fixed-strings --line-regexp 'A -gnata' "$ali"
  fi
 done
 sha256sum "$build/bin/descriptor_tests" "$build/obj/"*.ali \
   > "$out/$profile-artifacts.sha256"
 cat "$out/$profile-test.log"
done
sha256sum --check --quiet "$here/inputs.sha256"
printf '%s\n' 'Both profiles and input/profile checks passed.'
