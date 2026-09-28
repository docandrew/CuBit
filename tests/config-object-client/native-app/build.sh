#!/usr/bin/env bash
# Run inside Nix with coordination/build.lock held.
set -euo pipefail
cd "$(dirname "$0")/../../.."
make -C kernel ccl-manifest user_runtime
probe=tests/config-object-client/native-app
mkdir -p "$probe/build"
for mode in create reopen benchmark; do
    manifest=manifest.ccl
    if [ "$mode" = reopen ]; then manifest=reopen.ccl; fi
    userspace/ccl/build/manifest/ccl-manifest \
        userspace/ccl/catalogs/native-runtime-services.ccl "$probe/$manifest" > "$probe/build/manifest-$mode.S"
    as --64 "$probe/build/manifest-$mode.S" -o "$probe/build/manifest-$mode.o"
done
cd kernel
alr exec -- gprbuild -p -P ../tests/config-object-client/native-app/probe.gpr
alr exec -- gprbuild -p -P ../tests/config-object-client/native-app/probe.gpr -XCONFIG_OBJECT_MODE=reopen
alr exec -- gprbuild -p -P ../tests/config-object-client/native-app/probe.gpr -XCONFIG_OBJECT_MODE=benchmark
