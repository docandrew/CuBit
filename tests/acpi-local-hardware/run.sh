#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=/home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
suite_dir=$(cd -- "$(dirname -- "$0")" && pwd)
# Resolve the flake-pinned oracle when the development shell lacks it.
if ! command -v iasl >/dev/null 2>&1 || ! command -v acpiexec >/dev/null 2>&1; then
    acpi_test_tools=$(cd -- "$suite_dir/../.." && nix build --offline --no-link --print-out-paths --no-write-lock-file --impure --expr \
        '(builtins.getFlake (toString ./.)).inputs.nixpkgs.legacyPackages.${builtins.currentSystem}.acpica-tools')
    test -x "$acpi_test_tools/bin/iasl" && test -x "$acpi_test_tools/bin/acpiexec"
    export PATH="$acpi_test_tools/bin:$PATH"
fi
exec nice -n 19 "${CUBIT_TEST_NODE:-node}" "$suite_dir/run.mjs" "$@"
