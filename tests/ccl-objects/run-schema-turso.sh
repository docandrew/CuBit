#!/usr/bin/env bash
# Linux-hosted cross-language metadata + value persistence. Run inside Nix.
set -euo pipefail
cd "$(dirname "$0")/../.."
bash tests/ccl-objects/run-schema-codec.sh
fixture_dir=$(mktemp -d /tmp/cubit-schema-objects.XXXXXX)
tests/ccl-objects/build/schema-codec/schema_codec_tests --emit "$fixture_dir/input"
cargo run --manifest-path tests/config-turso/Cargo.toml --locked --release -j2 \
    --example schema_objects -- "$fixture_dir/input" "$fixture_dir/config.sqlite" "$fixture_dir/turso"
tests/ccl-objects/build/schema-codec/schema_codec_tests --check "$fixture_dir/turso"
python3 tests/ccl-objects/check-schema-database.py "$fixture_dir/config.sqlite" "$fixture_dir/input" "$fixture_dir/sqlite"
tests/ccl-objects/build/schema-codec/schema_codec_tests --check "$fixture_dir/sqlite"
echo "Hosted schema/value persistence artifacts: $fixture_dir"
