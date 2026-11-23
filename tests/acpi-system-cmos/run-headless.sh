#!/usr/bin/env bash
# Coordinator gate after building/staging the candidate native services/image.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
[[ ${TMPDIR:-} == /home/doc/cubit-build-tmp ]] || { echo 'Export TMPDIR=/home/doc/cubit-build-tmp before Nix' >&2; exit 2; }
[[ -n ${IN_NIX_SHELL:-} ]] || { echo 'Run inside the pinned Nix shell' >&2; exit 2; }
exec 9>"$root/coordination/build.lock"
flock --exclusive --nonblock 9 || { echo 'Shared build lock busy; CMOS headless gate deferred' >&2; exit 75; }
out=$(mktemp -d "$TMPDIR/acpi-cmos-headless-XXXXXXXX")
printf 'CMOS headless receipts: %s\n' "$out"
cd "$root"
timeout --kill-after=5s 1200s bash tests/headless/run.sh --test acpi-one-service \
  --accel "${1:-kvm}" --cpus 4 --timeout 120 --serial "$out/serial.log" --keep-logs \
  >"$out/headless.log" 2>&1
python3 "$here/check-headless.py" "$out/serial.log" --receipt "$out/CMOS-RESULT.json"
