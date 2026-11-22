#!/usr/bin/env bash
# Run under the Nix shell and coordination/build.lock, like build.sh/run.sh.
# Does not boot or modify the user's base disk in place.
set -euo pipefail
cd "$(dirname "$0")/../../.."
if [ "$#" != 1 ]; then
    echo "Usage: run-reboot.sh NEW_RESULTS_DIRECTORY" >&2
    exit 2
fi
output_dir=$(realpath -m "$1")
mkdir "$output_dir"
make -C kernel filesystem
bash tests/config-turso/native/build.sh --features turso
mkdir "$output_dir/probe"
cp tests/config-turso/target/native/turso-native-probe.app "$output_dir/probe/"
cp tests/config-turso/target/native/turso-native-probe.debug.app "$output_dir/probe/"
# Restore the exact tested binaries, not a rebuild against shared sources that
# another session may have changed while these guest tests were running.
restore_seed_probe() {
    result=$?
    trap - EXIT
    cp "$output_dir/probe/turso-native-probe.app" tests/config-turso/target/native/ || exit 1
    cp "$output_dir/probe/turso-native-probe.debug.app" tests/config-turso/target/native/ || exit 1
    exit "$result"
}
trap restore_seed_probe EXIT
bash tests/headless/run.sh --test turso-native --accel tcg,thread=multi \
    --timeout 40 --keep-logs --serial "$output_dir/seed.serial" \
    --turso-export "$output_dir/seed"
bash tests/config-turso/native/build.sh --features reopen
bash tests/headless/run.sh --test turso-native --accel tcg,thread=multi \
    --timeout 40 --keep-logs --serial "$output_dir/reopen.serial" \
    --disk "$output_dir/seed/disk.img" --turso-revision 2 \
    --turso-export "$output_dir/reopen"
echo "TURSO-NATIVE: two independent boots PASS; artifacts: $output_dir"
