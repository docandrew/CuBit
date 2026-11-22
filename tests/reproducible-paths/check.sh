#!/usr/bin/env bash
# Built native code must not depend on where the checkout is (docs/ccl-build.md,
# "Identity report"): the same sources give the same bytes in any checkout, so
# CCL build can share outputs between checkouts. Run after make world.
#
# 1. No archive or component program embeds the checkout's absolute path
#    (the Rust fonts archive once embedded it through rustc's panic and debug
#    paths, and through allocator objects compiled without the native GNAT
#    configuration's -ffile-prefix-map).
# 2. The archives' Ada is compiled only by the native configuration's GNAT
#    (gnat-15.cgpr): a gprbuild run without it compiled the allocator with
#    alr's default GNAT (16.1) and mixed that object into the fonts archive,
#    and so into every toolkit program.
#
#   tests/reproducible-paths/check.sh
set -euo pipefail
repo=$(git -C "$(dirname "$0")" rev-parse --show-toplevel)
cd "$repo"
status=0
# The native configuration's GNAT (build-params.ccl's toolchain pin).
expected_gnat="GNU Ada 15.3.0"
checked=0
artifacts=(userspace/rust/build/font-native/libcubit_fonts.a
           userspace/allocator/build/native-lib/libcubit_allocator.a)
while IFS= read -r component; do
    dir=$(dirname "$component")
    for program in "$dir"/build/*.app "$dir"/build/*.svc "$dir"/build/*.drv; do
        [ -f "$program" ] && artifacts+=("$program")
    done
done < <(git ls-files '*component.ccl' | grep -v '^tests/')
for artifact in "${artifacts[@]}"; do
    if [ ! -f "$artifact" ]; then
        echo "FAIL $artifact: not built"; status=1; continue
    fi
    checked=$((checked + 1))
    if grep -qaF "$repo/" "$artifact"; then
        echo "FAIL $artifact: embeds the checkout path $repo"; status=1
    fi
done
for archive in "${artifacts[@]:0:2}"; do
    [ -f "$archive" ] || continue
    compilers=$(strings -a "$archive" | grep -o '^GNU Ada [0-9.]*' | sort -u | paste -sd, -)
    if [ "$compilers" != "$expected_gnat" ]; then
        echo "FAIL $archive: Ada compiled by $compilers, not only $expected_gnat"; status=1
    fi
done
[ "$status" -eq 0 ] && echo "PASS $checked native artifacts independent of the checkout path; Ada only by $expected_gnat"
exit $status
