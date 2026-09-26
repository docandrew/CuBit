#!/usr/bin/env bash
# Build cubitshell (Servo embedded on CuBit) as a CuBit program:
# userspace/rust/build/cubitshell.app, stripped, with its CCL manifest.
# The manifest's sections are not loaded (procmgr reads them from the
# file), so they are added after linking: changing the manifest does not
# rebuild Servo. Uses userspace/rust/build/servo-work (checkout, CARGO_HOME,
# target) unless SERVO_DIR/CARGO_HOME/CARGO_TARGET_DIR say otherwise.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
work=$repo/userspace/rust/build/servo-work
export SERVO_DIR=${SERVO_DIR:-$work/servo}
export CARGO_HOME=${CARGO_HOME:-$work/cargo-home}
export CARGO_TARGET_DIR=${CARGO_TARGET_DIR:-$work/target}
out=$repo/userspace/rust/build/cubitshell.app
build=$repo/userspace/rust/build/cubitshell-manifest
mkdir -p "$build"

# The main thread runs the embedder's event loop; give it 8 MiB. The stack
# contract is a link option, so relink (cargo does not track it).
export CUBIT_STACK_SIZE=${CUBIT_STACK_SIZE:-8388608}
rm -f "$CARGO_TARGET_DIR/x86_64-unknown-cubit/release/cubitshell"
bash "$here/servo-cargo.sh" build --release -p cubitshell --features bundled "$@"

make -C "$repo/kernel" ccl-manifest >/dev/null
"$repo/userspace/ccl/build/manifest/ccl-manifest" \
    "$repo/userspace/ccl/catalogs/native-runtime-services.ccl" \
    "$here/manifest.ccl" > "$build/manifest.S"
as --64 "$build/manifest.S" -o "$build/manifest.o"

strip -o "$out.tmp" "$CARGO_TARGET_DIR/x86_64-unknown-cubit/release/cubitshell"
for section in $(readelf -SW "$build/manifest.o" | grep -o '\.cubit\.[a-z_.]*' | sort -u); do
    objcopy --dump-section "$section=$build/$section.bin" "$build/manifest.o"
    objcopy --add-section "$section=$build/$section.bin" "$out.tmp"
done
mv "$out.tmp" "$out"
echo "cubitshell: $out ($(du -h "$out" | cut -f1))"
