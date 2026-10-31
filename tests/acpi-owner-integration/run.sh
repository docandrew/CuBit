#!/usr/bin/env bash
# Keep source and artifact inventories independent of the invoking locale.
export LC_ALL=C
set -euo pipefail
here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
python3 "$here/verify.py"
if [ "${1:-}" = --validate-only ]; then exit 0; fi
test "$#" = 0
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
mkdir -p "$here/results"
out=$(mktemp -d "$here/results/run-XXXXXXXX")
printf '%s\n' "$out"
oracle=/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin
for tool in gprbuild gnatbind gnatlink python3; do sha256sum "$(realpath "$(command -v "$tool")")" >> "$out/tools.sha256"; done
sha256sum "$oracle/iasl" "$oracle/acpiexec" >> "$out/tools.sha256"
for revision in 1 2; do
 for fixture in source hid cache observer groups lid; do
  sed "s/\"DSDT\", 2/\"DSDT\", $revision/" "$here/fixtures/$fixture.asl" > "$out/$fixture-$revision.asl"
  timeout --kill-after=2s 5 "$oracle/iasl" -p "$out/$fixture-$revision" "$out/$fixture-$revision.asl" 2>&1 | head -c 1048576 > "$out/iasl-$fixture-$revision.log"
 done
done
python3 "$here/oracles.py" "$out"
for profile in checked release; do
 mkdir -p "$out/$profile/obj" "$out/$profile/bin"
 for main in source_tests hid_discovery_tests battery_cache_tests battery_notify_tests group_tests bootstrap_driver_tests; do
  timeout --kill-after=2s 180 gprbuild -p -j1 -P "$here/${main}_${profile}.gpr" "-XOWNER_BUILD=$out/$profile" 2>&1 | head -c 1048576 > "$out/$profile-$main-build.log"
 done
 python3 "$here/run-cases.py" "$out" "$profile"
done
python3 "$here/verify.py"
find "$out" -type f ! -name artifacts.sha256 -print0 | sort -z | xargs -0 sha256sum > "$out/artifacts.sha256"
