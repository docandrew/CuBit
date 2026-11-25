#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
# BUG-009: one allocator per process, from the runtime, and an incremental
# build that follows a changed runtime. Checks:
#   - the font archive carries no runtime unit (no CuAlloc copy:
#     tools/check_archive_overlap.py against libgnat-user.a);
#   - the incremental path: with CuBit.Kernel_ABI's Owned_Commit_Step_Pages
#     changed (as the commit that made the kernel refuse 16 MiB commits
#     changed it), `make ui-fonts-native font-masks` links a font-masks.app
#     whose CuAlloc compares against the new commit step, and, the spec put
#     back, against the old one again. Before the fix the font archive's
#     own CuAlloc won the link and kept the step it was built with.
# Run from the repository root inside nix develop, holding the build lock,
# after make -C kernel world (it rebuilds the runtime twice, and restores
# the spec it changes on any exit).
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
spec=$root/userspace/runtime/gnat/cubit-kernel_abi.ads
app=$root/userspace/apps/font-masks/build/font-masks.app
fail() { echo "one-allocator: FAIL: $*" >&2; exit 1; }
nm=$CUBIT_BINUTILS/bin/nm
objdump=$CUBIT_BINUTILS/bin/objdump

python3 -I "$root/tools/check_archive_overlap.py" --runtime "$root/userspace/runtime/adalib/libgnat-user.a" \
    "$root/userspace/rust/build/font-native/libcubit_fonts.a" || fail "the font archive carries a runtime unit"

pages=$(sed -n 's/^ *Owned_Commit_Step_Pages : constant := \([0-9]*\);.*/\1/p' "$spec")
[ -n "$pages" ] || fail "no Owned_Commit_Step_Pages in $spec"
changed=$(( pages / 2 ))
work=$(mktemp -d "${TMPDIR:-/tmp}/one-allocator.XXXXXX")
cp "$spec" "$work/spec"
restore() {
    cmp -s "$work/spec" "$spec" || cp "$work/spec" "$spec"
    rm -r -- "$work"
}
trap restore EXIT

step_in_app() {  # the commit step CuAlloc's ensure compares against, in font-masks.app
    local symbol
    symbol=$("$nm" "$app" | awk '/ cualloc_native__heap__ensureX$/ {print $3; exit}')
    [ -n "$symbol" ] || fail "font-masks.app has no cualloc_native__heap__ensureX"
    "$objdump" -d --no-show-raw-insn --disassemble="$symbol" "$app" > "$work/ensure.txt"
    grep -c "cmp *\$0x$(printf '%x' $(( $1 * 4096 )))," "$work/ensure.txt" || true
}
rebuild() {
    make -C "$root/kernel" ui-fonts-native font-masks > "$work/make-$1.log" 2>&1 ||
        { tail -20 "$work/make-$1.log"; fail "the rebuild ($1) failed"; }
}

sed -i "s/^\( *Owned_Commit_Step_Pages : constant := \)$pages;/\1$changed;/" "$spec"
grep -q "Owned_Commit_Step_Pages : constant := $changed;" "$spec" || fail "could not change the spec"
rebuild changed
[ "$(step_in_app "$changed")" -ge 1 ] ||
    fail "after Owned_Commit_Step_Pages $pages -> $changed, font-masks.app's CuAlloc does not use the new step (a stale copy won the link)"
cp "$work/spec" "$spec"
rebuild restored
[ "$(step_in_app "$pages")" -ge 1 ] || fail "with the spec put back, font-masks.app's CuAlloc does not use the old step"
echo "one-allocator: PASS (commit step $pages -> $changed -> $pages pages followed by the linked program)"
