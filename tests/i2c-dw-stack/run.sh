#!/usr/bin/env bash
set -euo pipefail
test "$#" = 2 && test "$1" = --output-root
test "${TMPDIR:-}" = /home/doc/cubit-build-tmp
ulimit -s 65536
ulimit -c 0
here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
exec node "$here/run.mjs" tests "$2"
