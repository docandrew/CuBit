#!/usr/bin/env bash
# Hosted calculator tests (docs/calculator.md, "Tests"). Run inside nix
# develop with TMPDIR outside /tmp:
#   tests/calculator/run.sh              core differential + CCL binding + window tests
#   tests/calculator/run.sh --core [N]   the core against Python's exact integers (N random cases)
#   tests/calculator/run.sh --ccl        calc.* in a CCL session
#   tests/calculator/run.sh --view       the window, scripted
#   tests/calculator/run.sh --screens    render the screenshot set (PNG)
#   tests/calculator/run.sh --bench      the core at -O3 -gnatp, as native apps build it
#   tests/calculator/run.sh --prove      SPARK proof of the core (level 2, memory-capped)
#   tests/calculator/run.sh --eval TEXT  evaluate one expression
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
shots="${CALC_SCREENSHOTS:-/home/doc/cubit-build-tmp/app-calc/screenshots}"
mkdir -p "$here/build/source" "$shots"
# Runtime specs the toolkit names (read-only copies; never rebuilt there).
for unit in cubit.ads cubit-filesystems.ads cubit-filesystems.adb cubit-directory_paths.ads \
            cubit-directory_paths.adb cubit-memory_grants.ads cubit-memory_grants.adb \
            cubit-messages.ads cubit-messages.adb cubit-process_ids.ads cubit-process_ids.adb \
            cubit-grant_references.ads cubit-kernel_abi.ads cubit-filesystem_queues.ads \
            cubit-submission_queues.ads cubit-submission_queues.adb cubit-slot_rings.ads \
            cubit-slot_rings.adb cubit-channel_rings.ads cubit-channel_rings.adb \
            cubit-channel_contracts.ads cubit-channel_contracts.adb cubit-protocols.ads \
            cubit-directory_pages.ads cubit-directory_pages.adb cubit-failures.ads cubit-failures.adb \
            cubit-filesystem_events.ads cubit-filesystem_events.adb cubit-volume_descriptions.ads \
            cubit-volume_descriptions.adb cubit-file_access.ads cubit-file_access.adb \
            cubit-published_clock.ads cubit-published_clock.adb cubit-kernel_calls.ads \
            cubit-kernel_calls.adb; do
  cmp -s "$root/userspace/runtime/gnat/$unit" "$here/build/source/$unit" ||
    cp "$root/userspace/runtime/gnat/$unit" "$here/build/source/"
done
for unit in clock_publication.ads clock_publication.adb clock_publication-sample.ads clock_publication-sample.adb; do
  cmp -s "$root/shared/time/$unit" "$here/build/source/$unit" || cp "$root/shared/time/$unit" "$here/build/source/"
done
# The desktop's themes from system.ccl, as the app reads them.
if [ -x "$root/userspace/ccl/build/config/ccl-config" ]; then
  plan="$("$root/userspace/ccl/build/config/ccl-config" "$root/system.ccl" --dump-plan 2>/dev/null || true)"
  CALC_THEME_LIGHT="$(printf '%s\n' "$plan" | sed -n 's/^desktop\.appearance\.theme\.light=//p')"
  CALC_THEME_DARK="$(printf '%s\n' "$plan" | sed -n 's/^desktop\.appearance\.theme\.dark=//p')"
  export CALC_THEME_LIGHT CALC_THEME_DARK
fi
build() {  # project, mode, mains...
  local project=$1 mode=$2; shift 2
  (cd "$root/kernel" && alr exec -- gprbuild -p -q -j8 -XCALC_MODE="$mode" -P "$here/$project" "$@")
}
cases() {
  python3 "$here/calc_reference.py" "$here/build/cases.txt" "${1:-3000}" "${2:-2026}"
}
case "${1:-}" in
  --core)
    build calculator.gpr checked
    cases "${2:-3000}" "${3:-2026}"
    "$here/build/calc_core_tests" "$here/build/cases.txt"
    ;;
  --ccl)
    build calc_ccl.gpr checked
    "$here/build/calc_ccl_tests"
    python3 -I "$root/tests/ccl-console/check_interface_keys.py"
    ;;
  --view)
    build calc_view.gpr checked
    (cd "$here" && build/calc_view_tests)
    ;;
  --screens)
    build calc_view.gpr checked
    rm -f "$here"/build/screen-*.ppm
    (cd "$here" && build/calc_view_tests --screens)
    python3 -I "$root/tests/text-editor/ppm_to_png.py" "$here"/build/screen-*.ppm --into "$shots"
    ;;
  --bench)
    build calculator.gpr release
    "$here/build/calc_core_tests_release" --bench
    ;;
  --prove)
    # Memory-capped (gnatwhy3 once took 44 GB): systemd scope, -j2, level 2.
    cd "$root/kernel"
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
      alr exec -- gnatprove -P "$here/calc_proof.gpr" -j2 --level=2 --report=fail --checks-as-errors=on "${@:2}"
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
      alr exec -- gnatprove -P "$here/symbols_proof.gpr" -j2 --level=1 --report=fail --checks-as-errors=on
    ;;
  --eval)
    build calculator.gpr checked
    "$here/build/calc_core_tests" --eval "$2"
    ;;
  *)
    build calculator.gpr checked
    cases 3000 2026
    "$here/build/calc_core_tests" "$here/build/cases.txt"
    build calc_ccl.gpr checked
    "$here/build/calc_ccl_tests"
    python3 -I "$root/tests/ccl-console/check_interface_keys.py"
    build calc_view.gpr checked
    (cd "$here" && build/calc_view_tests)
    ;;
esac
