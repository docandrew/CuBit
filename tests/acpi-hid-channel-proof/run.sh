#!/usr/bin/env bash
set -euo pipefail
: "${TMPDIR:?export TMPDIR=/home/doc/cubit-build-tmp before Nix}"
[[ "$TMPDIR" == /home/doc/cubit-build-tmp ]]
ulimit -c 0
ulimit -s 65536
acpi_proof_here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
exec node "$acpi_proof_here/run.mjs" "$@"
