#!/usr/bin/env bash
# Hosted CuBit Text (docs/text-editor.md, tests/text-editor/README.md). Run
# inside nix develop with TMPDIR outside /tmp:
#   tests/text-editor/run.sh              build, scripted tests (core + view)
#   tests/text-editor/run.sh --core [seed]  the core's differential tests only
#   tests/text-editor/run.sh --bench      build -O3 -gnatp, benchmarks (quiet host please)
#   tests/text-editor/run.sh --prove      SPARK proof of the core units (level 2)
#   tests/text-editor/run.sh --mutate     mutation checks: each mutant must fail a test
#   tests/text-editor/run.sh --interface  rewrite userspace/apps/text/text.ccl-interface
#   tests/text-editor/run.sh --screens    render the screenshot set (PNG)
#   tests/text-editor/run.sh --window [file...]   interactive SDL window
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
shots="${TEXT_SCREENSHOTS:-/home/doc/cubit-build-tmp/text-editor/screenshots}"
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
# The desktop's themes from system.ccl, as Files reads them.
if [ -x "$root/userspace/ccl/build/config/ccl-config" ]; then
  plan="$("$root/userspace/ccl/build/config/ccl-config" "$root/system.ccl" --dump-plan 2>/dev/null || true)"
  TEXT_THEME_LIGHT="$(printf '%s\n' "$plan" | sed -n 's/^desktop\.appearance\.theme\.light=//p')"
  TEXT_THEME_DARK="$(printf '%s\n' "$plan" | sed -n 's/^desktop\.appearance\.theme\.dark=//p')"
  export TEXT_THEME_LIGHT TEXT_THEME_DARK
fi
build() {  # mode, mains...
  local mode=$1; shift
  (cd "$root/kernel" && alr exec -- gprbuild -p -q -j8 -XTEXT_MODE="$mode" -P "$here/text_editor.gpr" "$@")
}
# The regex reference corpus (Python re's answers) for the core tests.
corpus() {
  [ -s "$here/build/regex-cases.txt" ] && [ "$here/build/regex-cases.txt" -nt "$here/regex_reference.py" ] ||
    python3 -I "$here/regex_reference.py" "$here/build/regex-cases.txt" 20000
}
core_units="${PROVE_UNITS:-text_utf8.adb text_layout.adb text_pieces.adb text_cursors.adb text_marks.adb text_history.adb text_modal.adb
            text_syntax.adb text_search.adb text_commands.adb}"
case "${1:-}" in
  --core)
    corpus
    build checked text_core_tests.adb
    cd "$here" && build/text_core_tests "${@:2}"
    ;;
  --prove)
    # Memory-capped (gnatwhy3 once took 44 GB): systemd scope, -j2, level 2.
    cd "$root/kernel"
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
      alr exec -- gnatprove -P "$here/text_proof.gpr" -j2 --level=2 --report=fail --checks-as-errors=on \
        -u $core_units "${@:2}"
    ;;
  --bench)
    build release text_bench.adb
    cd "$here" && build/text_bench "${@:2}"
    ;;
  --interface)
    build checked text_core_tests.adb
    cd "$here" && build/text_core_tests --write-interface | grep -i "interface"
    ;;
  --mutate)
    bash "$here/mutate.sh"
    ;;
  --screens)
    build checked text_view_tests.adb
    cd "$here" && TEXT_SCREENSHOTS="$shots" build/text_view_tests --screens
    python3 -I "$here/ppm_to_png.py" "$here/build"/screen-*.ppm --into "$shots"
    ;;
  --window)
    build release text_window.adb
    cd "$here" && build/text_window_release "${@:2}"
    ;;
  *)
    corpus
    build checked text_core_tests.adb text_view_tests.adb
    cd "$here"
    build/text_core_tests "$@"
    build/text_view_tests
    ;;
esac
