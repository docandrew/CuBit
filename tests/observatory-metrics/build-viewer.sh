#!/usr/bin/env bash
# Nix and shared build lock required; uses existing runtime and manifest tool.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
app=../userspace/apps/observatory
mkdir -p "$app/build/generated"
../userspace/ccl/build/manifest/ccl-manifest \
 ../userspace/ccl/catalogs/native-runtime-services.ccl "$app/manifest.ccl" \
 --ada-output "$app/build/generated/ccl_manifest_bindings.ads" > "$app/build/manifest.S"
alr exec -- gcc -c "$app/build/manifest.S" -o "$app/build/manifest.o"
alr exec -- gprbuild -p -P "$app/observatory.gpr"
