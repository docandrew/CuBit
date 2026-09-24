#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
# Resolve the pinned upstream source through the already-locked hosted probe;
# the native lock points at this isolated generated copy instead of the registry.
upstream_manifest=$(cargo metadata --locked --format-version 1 --filter-platform x86_64-unknown-linux-gnu --manifest-path ../Cargo.toml |
    jq -er '.packages[] | select(.name == "turso_core" and .version == "0.8.0-pre.12") | .manifest_path')
upstream_dir=$(dirname "$upstream_manifest")
prepared=../target/native-turso
key=$(sha256sum turso.patch | cut -d' ' -f1)
if [ -f "$prepared/.cubit-patch" ]; then
    test "$(< "$prepared/.cubit-patch")" = "$key" || {
        echo "Native Turso patch changed; move $prepared aside and regenerate it." >&2
        exit 1
    }
else
    test ! -e "$prepared" || {
        echo "Incomplete generated Turso tree: move $prepared aside and retry." >&2
        exit 1
    }
    mkdir -p ../target
    cp -R "$upstream_dir" "$prepared"
    chmod -R u+w "$prepared"
    patch --batch --fuzz=0 -p1 -d "$prepared" < turso.patch
    printf '%s\n' "$key" > "$prepared/.cubit-patch"
fi
