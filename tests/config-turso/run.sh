#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
# Invoke through `nix develop -c bash tests/config-turso/run.sh`.
cargo test --locked --release -j 4
cargo fmt --check
python3 test-benchmark-report.py
python3 native/test-benchmark-report.py
python3 native/test-check-storage-disk.py
python3 native/test-sql-profile.py
python3 native/test-nvme-waits.py
(cd ../../kernel && alr exec -- gprbuild -p -j4 -P../tests/config-turso/cbor_crosscheck.gpr)
target/ada-cbor/crosscheck fixtures/scalar-profile.hex
mkdir -p results
cargo metadata --locked --format-version 1 --filter-platform x86_64-unknown-linux-gnu |
    python3 audit.py > results/dependencies.md
if [ "${1:-}" = "--bench" ]; then
    mkdir -p results
    benchmark_dir=$(mktemp -d "$PWD/results/run.XXXXXX")
    cargo run --locked --release -j 4 --example bench -- syscall "$benchmark_dir/syscall" |
        tee "$benchmark_dir/latency.txt"
fi
