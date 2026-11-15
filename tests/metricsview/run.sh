#!/usr/bin/env bash
# Hosted metrics viewer tests (MET-002's app). Run inside nix develop with
# TMPDIR outside /tmp:
#   tests/metricsview/run.sh            charts core + the window, scripted
#   tests/metricsview/run.sh --core     the charts core
#   tests/metricsview/run.sh --view     the window, scripted (checked build)
#   tests/metricsview/run.sh --screens  the window's screenshots (PNG)
#   tests/metricsview/run.sh --bench    the window at -O3 -gnatp: interaction and refresh times
#   tests/metricsview/run.sh --prove    SPARK proof of the charts core (memory-capped, -j2)
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
shots="${METRICSVIEW_SCREENSHOTS:-/home/doc/cubit-build-tmp/metricsview-shots/hosted}"
mkdir -p "$here/build" "$shots"
# Runtime specs the toolkit names (read-only copies; never rebuilt there),
# as tests/calculator/run.sh copies them.
mkdir -p "$here/build/source"
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
            cubit-kernel_calls.adb cubit-metric_updates.ads cubit-metric_updates.adb; do
  cmp -s "$root/userspace/runtime/gnat/$unit" "$here/build/source/$unit" ||
    cp "$root/userspace/runtime/gnat/$unit" "$here/build/source/"
done
# The units CCL shares with hosted tools (CCL.Typed_Values reads dashboards).
for path in "$root"/userspace/ccl/runtime-shared/*.ad?; do
  unit="$(basename "$path")"
  cmp -s "$path" "$here/build/source/$unit" || cp -L "$path" "$here/build/source/"
done
for unit in clock_publication.ads clock_publication.adb clock_publication-sample.ads clock_publication-sample.adb; do
  cmp -s "$root/shared/time/$unit" "$here/build/source/$unit" || cp "$root/shared/time/$unit" "$here/build/source/"
done
if [ -x "$root/userspace/ccl/build/config/ccl-config" ]; then
  plan="$("$root/userspace/ccl/build/config/ccl-config" "$root/system.ccl" --dump-plan 2>/dev/null || true)"
  METRICSVIEW_THEME_LIGHT="$(printf '%s\n' "$plan" | sed -n 's/^desktop\.appearance\.theme\.light=//p')"
  METRICSVIEW_THEME_DARK="$(printf '%s\n' "$plan" | sed -n 's/^desktop\.appearance\.theme\.dark=//p')"
  export METRICSVIEW_THEME_LIGHT METRICSVIEW_THEME_DARK
fi
build() {  # project, mode variable, mode
  (cd "$root/kernel" && alr exec -- gprbuild -p -q -j8 -X"$2=$3" -P "$here/$1")
}
core() {
  build charts_core.gpr CHARTS_MODE checked
  "$here/build/charts_core_tests"
}
view() {
  build metricsview_view.gpr VIEW_MODE checked
  (cd "$here" && build/metricsview_view_tests "$@")
}
case "${1:-}" in
  --core) core ;;
  --view) view ;;
  --screens)
    rm -f "$here"/build/screen-*.ppm
    view --screens || echo "metricsview: tests failed (screens still converted)"
    python3 -I "$root/tests/text-editor/ppm_to_png.py" "$here"/build/screen-*.ppm --into "$shots"
    ;;
  --bench)
    build metricsview_view.gpr VIEW_MODE release
    (cd "$here" && build/metricsview_view_tests_release)
    ;;
  --prove)
    cd "$root/kernel"
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
      alr exec -- gnatprove -P "$here/charts_proof.gpr" -j2 --level=2 --report=fail --checks-as-errors=on "${@:2}"
    ;;
  *) core; view ;;
esac
