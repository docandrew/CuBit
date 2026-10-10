#!/usr/bin/env bash
# Hosted Files (docs/files-app.md, tests/files-app/README.md). Run inside
# nix develop with TMPDIR outside /tmp:
#   tests/files-app/run.sh            build, scripted tests
#   tests/files-app/run.sh --bench    build, benchmarks (quiet host please)
#   tests/files-app/run.sh --prove    build, SPARK proof of the policy units
#   tests/files-app/run.sh --window [left-root [right-root]]
#                                     interactive SDL window
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
mkdir -p "$here/build/source" "${FILES_SCRATCH:=/home/doc/cubit-build-tmp/files-scratch}"
# Runtime specs the shared units name (read-only copies; never rebuilt there).
for unit in cubit.ads cubit-filesystems.ads cubit-filesystems.adb cubit-directory_paths.ads \
            cubit-directory_paths.adb cubit-memory_grants.ads cubit-memory_grants.adb \
            cubit-messages.ads cubit-messages.adb cubit-process_ids.ads cubit-process_ids.adb \
            cubit-grant_references.ads cubit-kernel_abi.ads cubit-filesystem_queues.ads \
            cubit-submission_queues.ads cubit-submission_queues.adb cubit-slot_rings.ads \
            cubit-slot_rings.adb cubit-channel_rings.ads cubit-channel_rings.adb \
            cubit-channel_contracts.ads cubit-channel_contracts.adb cubit-protocols.ads \
            cubit-directory_pages.ads cubit-directory_pages.adb cubit-failures.ads cubit-failures.adb \
            cubit-filesystem_events.ads cubit-filesystem_events.adb cubit-volume_descriptions.ads \
            cubit-volume_descriptions.adb cubit-file_access.ads cubit-file_access.adb; do
  cmp -s "$root/userspace/runtime/gnat/$unit" "$here/build/source/$unit" ||
    cp "$root/userspace/runtime/gnat/$unit" "$here/build/source/"
done
# The desktop's themes from system.ccl, through the same compiled plan the
# desktop reads (ccl-config; built by make -C kernel ccl-config).
if [ -x "$root/userspace/ccl/build/config/ccl-config" ]; then
  plan="$("$root/userspace/ccl/build/config/ccl-config" "$root/system.ccl" --dump-plan 2>/dev/null || true)"
  FILES_THEME_LIGHT="$(printf '%s\n' "$plan" | sed -n 's/^desktop\.appearance\.theme\.light=//p')"
  FILES_THEME_DARK="$(printf '%s\n' "$plan" | sed -n 's/^desktop\.appearance\.theme\.dark=//p')"
  export FILES_THEME_LIGHT FILES_THEME_DARK
fi
cd "$root/kernel"
case "${1:-}" in
  --prove)
    alr exec -- gnatprove -P "$here/files_proof.gpr" -j0 --level=2 --report=fail --checks-as-errors=on \
      -u files_listing.adb files_order.adb files_filter.adb files_viewport.adb files_marks.adb files_pages.adb files_plan.adb
    ;;
  --bench)
    alr exec -- gprbuild -p -q -P "$here/files_app.gpr" files_bench.adb
    cd "$here" && build/files_bench "${@:2}"
    ;;
  --profile)
    # gprof of the benchmark's frame work: run.sh --profile [size]
    alr exec -- gprbuild -p -q -XFILES_MODE=profile -P "$here/files_app.gpr" files_bench.adb
    cd "$here/build/profile" && ./files_bench "${2:-100000}" frames && gprof -b -p ./files_bench gmon.out | head -40
    ;;
  --window)
    alr exec -- gprbuild -p -q -P "$here/files_app.gpr" files_window.adb
    cd "$here" && build/files_window "${@:2}"
    ;;
  *)
    alr exec -- gprbuild -p -q -P "$here/files_app.gpr" files_tests.adb
    cd "$here" && build/files_tests "$@"
    ;;
esac
