#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=/home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
suite_dir=$(cd -- "$(dirname -- "$0")" && pwd)
repo_dir=$(cd -- "$suite_dir/../.." && pwd)
cd "$repo_dir"
sha256sum --check --quiet "$suite_dir/inputs.sha256"
task_build=$(mktemp -d "$TMPDIR/acpi-locked-nvs-XXXXXX")
printf 'Hosted Lock/NVS logs: %s\n' "$task_build"
mkdir -p "$task_build/inputs"
while read -r digest relative; do
  case "$relative" in /*|*..*|*\ *) echo "invalid input path" >&2; exit 1;; esac
  test -f "$relative" && test ! -L "$relative"
  cp --parents -- "$relative" "$task_build/inputs"
done < "$suite_dir/inputs.sha256"
(cd "$task_build/inputs" && sha256sum --check --quiet "$suite_dir/inputs.sha256")
for profile in checked release; do
  mkdir -p "$task_build/$profile"
  nice -n 19 timeout --kill-after=2s 180 gprbuild -q -p -j1 -P "$task_build/inputs/tests/acpi-locked-nvs/$profile.gpr" -XACPI_TEST_SNAPSHOT="$task_build/inputs" -XACPI_TEST_BUILD="$task_build" > "$task_build/$profile/build.log" 2>&1
  for scenario in {0..11}; do
    nice -n 19 timeout --kill-after=2s 30 "$task_build/$profile/locked_owner_tests" "$scenario" > "$task_build/$profile/case-$scenario.log" 2>&1
    tail -n 1 "$task_build/$profile/case-$scenario.log"
  done
done
sha256sum --check --quiet "$suite_dir/inputs.sha256"
printf 'Hosted Lock/NVS owner checks passed; logs: %s\n' "$task_build"
