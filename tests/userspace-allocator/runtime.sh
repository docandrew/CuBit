#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
root="$PWD"
(cd kernel; alr exec -- gprbuild -p -P ../userspace/allocator/allocator_runtime_host.gpr)
cd userspace/rust
RUSTFLAGS="-L native=$root/userspace/allocator/build/runtime-host-lib -l static=cubit_allocator" \
  cargo --config 'source.crates-io.replace-with="cubit-vendor"' \
    --config "source.cubit-vendor.directory=\"$CUBIT_RUST_VENDOR\"" \
    test --locked --offline --target x86_64-unknown-linux-gnu -p cubit-allocator
