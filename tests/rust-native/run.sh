#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
make -C kernel rust-probe ccl-test-host clock
python3 tests/rust-native/check-elf.py
(
    cd userspace/rust
    cargo --config 'source.crates-io.replace-with="cubit-vendor"' \
      --config "source.cubit-vendor.directory=\"$CUBIT_RUST_VENDOR\"" \
      test --locked --offline --target x86_64-unknown-linux-gnu -p cubit
)
bash tests/headless/run.sh --test rust-native --accel "${QEMU_ACCEL:-kvm}" --timeout 25 --keep-logs --serial /tmp/cubit-rust-native-serial.log
