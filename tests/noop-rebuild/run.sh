#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
# BLD-001: a rebuild with nothing changed rebuilds, re-signs and repacks
# nothing. After a full build, this runs `make -C kernel world` and
# `make -C kernel desktop-stage` (everything run-desktop does before QEMU
# starts) twice each and fails if any of them
#   - asked ccl-build anything, staged a program, signed one,
#   - remade the initrd, a development disk, an ISO or the prepared
#     desktop disk,
#   - or changed a byte or a modification time of anything in isodir or of
#     those images.
# The desktop scratch disk is reset by design on every desktop-stage; it
# must equal the prepared disk afterwards.
#
# Run from the repository root inside nix develop, holding the build lock:
#   make -C kernel world && bash tests/noop-rebuild/run.sh
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
kernel=$root/kernel
work=${TMPDIR:-/tmp}/noop-rebuild.$$
mkdir -p "$work"
trap 'rm -r -- "$work"' EXIT

# Make the run ISO and prepared desktop disk exist first: the first
# desktop-stage after a world may make them.
make -C "$kernel" desktop-stage > "$work/prime.log" 2>&1 || { tail -40 "$work/prime.log"; exit 1; }

images=(isodir/boot/initrd.img nvme_disk.img ata_disk.img cubit_kernel.iso cubit_run.iso desktop_disk-prepared.img)
snapshot() {
    (cd "$kernel" && find isodir -type f -printf '%T@ %s %p\n' | LC_ALL=C sort -k3
     for image in "${images[@]}"; do
         if [ -e "$image" ]; then stat -c '%y %s %n' "$image"; fi
     done)
}
snapshot > "$work/before.txt"
(cd "$kernel" && find isodir -type f -print0 | xargs -0 sha256sum && sha256sum "${images[@]}") | LC_ALL=C sort -k2 > "$work/before.sha256"

failures=0
check() {
    local name=$1 log=$2
    local asked ran signed staged
    asked=$(awk '/^ccl-build:/ {n++} END {print n + 0}' "$log")
    ran=$(awk '/^ccl-build:/ {s += $2} END {print s + 0}' "$log")
    signed=$(awk '/^sign_programs: / {s += $3} END {print s + 0}' "$log")
    staged=$(grep -c '^stage ' "$log" || true)
    local remade=()
    grep -q 'grub-mkrescue' "$log" && remade+=(iso)
    grep -q 'realize.py' "$log" && remade+=(initrd)
    grep -q 'build_development_disk.py' "$log" && remade+=(development-disk)
    grep -q 'prepare_desktop_disk.py' "$log" && remade+=(prepared-desktop-disk)
    printf 'noop-rebuild: %-16s %5.1f s  ccl-build asked %s (steps run %s), signed %s, staged %s, remade: %s\n' \
        "$name" "$3" "$asked" "$ran" "$signed" "$staged" "${remade[*]:-nothing}"
    if [ "$asked" != 0 ] || [ "$ran" != 0 ] || [ "$signed" != 0 ] || [ "$staged" != 0 ] || [ ${#remade[@]} != 0 ]; then
        echo "noop-rebuild: FAIL: $name rebuilt something (log: $log)" >&2
        failures=$((failures + 1))
        cp "$log" "${TMPDIR:-/tmp}/noop-rebuild-$name.log"
    fi
}
for pass in 1 2; do
    for target in world desktop-stage; do
        start=$(date +%s.%N)
        make -C "$kernel" "$target" > "$work/$target-$pass.log" 2>&1 || {
            tail -40 "$work/$target-$pass.log"; echo "noop-rebuild: FAIL: make $target failed" >&2; exit 1; }
        check "$target-$pass" "$work/$target-$pass.log" "$(echo "$(date +%s.%N) - $start" | bc)"
    done
done

snapshot > "$work/after.txt"
(cd "$kernel" && find isodir -type f -print0 | xargs -0 sha256sum && sha256sum "${images[@]}") | LC_ALL=C sort -k2 > "$work/after.sha256"
if ! diff -u "$work/before.txt" "$work/after.txt"; then
    echo "noop-rebuild: FAIL: a staged file or image changed time or size" >&2
    failures=$((failures + 1))
fi
if ! diff -u "$work/before.sha256" "$work/after.sha256"; then
    echo "noop-rebuild: FAIL: a staged file or image changed content" >&2
    failures=$((failures + 1))
fi
if ! cmp -s "$kernel/desktop_disk-prepared.img" "$kernel/desktop_disk.img"; then
    echo "noop-rebuild: FAIL: the desktop scratch disk was not reset to the prepared disk" >&2
    failures=$((failures + 1))
fi
if [ "$failures" != 0 ]; then
    echo "noop-rebuild: FAIL ($failures)"
    exit 1
fi
echo "noop-rebuild: PASS"
