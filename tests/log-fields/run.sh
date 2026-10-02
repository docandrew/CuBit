#!/usr/bin/env bash
# Structured log-field tests and SPARK proof of CuBit.Log_Records.
#   nix develop -c bash tests/log-fields/run.sh [--no-prove]
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
runtime="$here/../../userspace/runtime/gnat"
mkdir -p "$here/build/source"
for unit in cubit.ads cubit-protocols.ads cubit-log_records.ads \
    cubit-log_records.adb; do
    cp "$runtime/$unit" "$here/build/source/"
done
cd "$here/../../kernel"
alr exec -- gprbuild -q -p -P "$here/fields.gpr"
"$here/build/main"
if [[ "${1:-}" != "--no-prove" ]]; then
    alr exec -- gnatprove -P "$here/fields.gpr" -u cubit-log_records.adb \
        --level=1 --report=fail --checks-as-errors=on -j4
fi
