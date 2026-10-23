#!/usr/bin/env bash
# Run inside Nix with coordination/build.lock held.
set -euo pipefail
# Resolved before any cd, while a relative $0 still names this script.
gprbuild_config=${CUBIT_NATIVE_GPRBUILD_CONFIG:-$(make -s --no-print-directory -C "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)/kernel" print-native-gprbuild-config)}
cd "$(dirname "$0")/../../.."
make -C kernel ccl-manifest user_runtime
probe=tests/config-object-client/native-app
mkdir -p "$probe/build/generated"
for mode in create reopen benchmark; do
    manifest=manifest.ccl
    if [ "$mode" = reopen ]; then manifest=reopen.ccl; fi
    userspace/ccl/build/manifest/ccl-manifest \
        userspace/ccl/catalogs/native-runtime-services.ccl "$probe/$manifest" \
        --startup-output "$probe/build/generated/startup-$mode.ads" > "$probe/build/manifest-$mode.S"
    as --64 "$probe/build/manifest-$mode.S" -o "$probe/build/manifest-$mode.o"
done
# Every mode's manifest binds the same "config" entry: one Startup package.
cp "$probe/build/generated/startup-create.ads" "$probe/build/generated/startup.ads"
rm -f "$probe"/build/generated/startup-*.ads
cd kernel
alr exec -- gprbuild $gprbuild_config -p -P ../tests/config-object-client/native-app/probe.gpr
alr exec -- gprbuild $gprbuild_config -p -P ../tests/config-object-client/native-app/probe.gpr -XCONFIG_OBJECT_MODE=reopen
alr exec -- gprbuild $gprbuild_config -p -P ../tests/config-object-client/native-app/probe.gpr -XCONFIG_OBJECT_MODE=benchmark
