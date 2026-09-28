#!/usr/bin/env bash
# Linux-hosted Ada -> CBOR -> real Turso -> independent SQLite -> Ada.
set -euo pipefail
cd "$(dirname "$0")/../.."
bash tests/ccl-objects/run-persistence.sh
fixture_dir=$(mktemp -d /tmp/cubit-typed-objects.XXXXXX)
tests/ccl-objects/build/persistence/persistence_tests --emit "$fixture_dir/input.hex"
(cd tests/config-turso && cargo run --locked --release -j2 --example ccl_objects -- \
    "$fixture_dir/input.hex" "$fixture_dir/config.sqlite" "$fixture_dir/turso.hex")
tests/ccl-objects/build/persistence/persistence_tests --check "$fixture_dir/turso.hex"
python3 tests/ccl-objects/check-database.py "$fixture_dir/config.sqlite" \
    "$fixture_dir/input.hex" "$fixture_dir/sqlite.hex"
tests/ccl-objects/build/persistence/persistence_tests --check "$fixture_dir/sqlite.hex"
echo "Hosted typed persistence artifacts: $fixture_dir"
