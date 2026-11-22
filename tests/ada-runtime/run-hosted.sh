#!/usr/bin/env bash
# RT-008: ada-runtime-check's checks on Linux, against upstream GNAT 15's full
# runtime and the hosted CuAlloc (inside nix develop). The native run is
# headless --test ada-runtime.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
# Alire's workspace (the pinned gnat_native and gprbuild) is kernel/'s.
(cd "$here/../../kernel" && alr exec -- gprbuild -p -q -P "$here/ada_runtime_hosted.gpr")
"$here/build/hosted_main"
