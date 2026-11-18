#!/usr/bin/env bash
#  Compositor_Pointer_Over against the blend it replaced (IPC-009).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/test.gpr"
"$here/build/pointer_over_tests"
