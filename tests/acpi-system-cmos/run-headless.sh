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
# Generate the read-only fixture from source under the held build lock.
iasl=$(nix build --inputs-from "$root" nixpkgs#acpica-tools --no-link --print-out-paths | head -1)/bin/iasl
# acpica-tools from the flake-pinned nixpkgs, as the GCC port takes its sources,
# not the dev shell: adding it to the shell changed the device-build toolchain
# identity (image 40h). Its binary guard stays.
printf '%s  %s\n' 1c6a21c90ff6f846e27a98dbb3106362c706f67db8ceca21ccd81ab76a7d9dcb "$iasl" | sha256sum --check --status
timeout --kill-after=2s 30s "$iasl" -p "$out/cmos-reach" "$here/cmos-reach.asl" >"$out/iasl.log" 2>&1
sha256sum "$here/cmos-reach.asl" "$out/cmos-reach.aml" >"$out/fixture.sha256"
export ACPI_CMOS_REACH_TABLE="$out/cmos-reach.aml"
headless_status=0
timeout --kill-after=5s 1200s bash tests/headless/run.sh --test acpi-one-service \
  --accel "${1:-kvm}" --cpus 4 --timeout 120 --serial "$out/serial.log" --keep-logs \
  >"$out/headless.log" 2>&1 || headless_status=$?
printf '%s\n' "$headless_status" >"$out/headless.status"
parser_status=0
python3 "$here/check-headless.py" "$out/serial.log" --receipt "$out/CMOS-RESULT.json" || parser_status=$?
# Keep both receipts; parser success never hides a headless failure.
if [ "$headless_status" -ne 0 ]; then exit "$headless_status"; fi
exit "$parser_status"
