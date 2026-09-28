#!/usr/bin/env bash
# Run inside Nix. Linux-hosted direct Ada/Rust calls; no live Config/startup edits.
set -euo pipefail
cd "$(dirname "$0")/../.."
(cd tests/config-turso && cargo build --locked --release -j2 --example database_bridge)
export CONFIG_TURSO_LIBRARY="$PWD/tests/config-turso/target/release/examples"
# GPR does not track the Cargo-produced archive as an Ada dependency. Force
# rebuilding/relinking so a changed Rust adapter can never leave a stale test.
(cd kernel && alr exec -- gprbuild -f -p -P ../tests/ccl-objects/durable_turso.gpr)
mkdir -p tests/ccl-objects/build/artifacts
result_dir=$(mktemp -d "$PWD/tests/ccl-objects/build/artifacts/config-publication.XXXXXX")
tests/ccl-objects/build/durable-turso/typed_store_turso "$result_dir/config.sqlite"
python3 tests/ccl-objects/check-publication.py "$result_dir/config.sqlite" machine
python3 tests/ccl-objects/check-managed-store.py "$result_dir/config.sqlite.managed"
tests/ccl-objects/build/durable-turso/native_probe_host "$result_dir/native-worker.sqlite"
python3 tests/ccl-objects/check-publication.py "$result_dir/native-worker.sqlite"
tests/ccl-objects/build/durable-turso/schema_store_turso "$result_dir/schema-channel.sqlite"
python3 tests/ccl-objects/check-schema-channel.py "$result_dir/schema-channel.sqlite"
echo "Hosted publication/recovery artifacts: $result_dir"
