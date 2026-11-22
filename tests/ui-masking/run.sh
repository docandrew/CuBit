#!/usr/bin/env bash
# Hosted test of CuBit.UI_Masking (masked text field stand-ins). Run inside nix develop.
set -eu
here=$(cd "$(dirname "$0")" && pwd); root=$here/../..
out=$here/build; mkdir -p "$out"; cd "$out"
gnatmake -q -gnat2022 -gnata -I"$root/userspace/lib/ui" -I"$here" \
  "$here/masking_tests.adb" -o masking_tests -D "$out"
./masking_tests | tee result.txt
grep -q "MASKING: PASS" result.txt
