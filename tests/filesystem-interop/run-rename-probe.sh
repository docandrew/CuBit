#!/usr/bin/env bash
# Run inside Nix after building rename_probe.gpr. Only disposable copies change.
set -euo pipefail
cd "$(dirname "$0")/../.."
probe_dir=$(mktemp -d /tmp/cubit-rename-probe.XXXXXX)
cp --reflink=auto kernel/nvme_disk.img "$probe_dir/plain.img"
cp --reflink=auto kernel/nvme_disk.img "$probe_dir/crowded.img"
truncate -s 8192 "$probe_dir/payload"
image="$probe_dir/crowded.img"
debug() { debugfs -w -R "$1" "$image" >> "$probe_dir/seed.log" 2>&1; }
# Reproduce the directory names/order installed by storage-grants, without
# needing its binaries, double-indirect payload or intentionally corrupt inode.
debug 'symlink nav-link lost+found'
for app in storage-check.app filesystem-scope-check.app; do
    debug "rm $app"
    debug "write $probe_dir/payload $app"
done
debug 'symlink file-link config.dat'
debug 'symlink long-file-link deliberately-long-link-target-that-must-not-be-interpreted-as-block-pointers-or-followed'
debug "write $probe_dir/payload linked-file"
debug 'ln linked-file linked-alias'
debug 'set_inode_field linked-file links_count 2'
for dir in scope-allowed scope-allowed/nested scope-allowed-other scope-work; do
    debug "mkdir $dir"
done
for file in scope-allowed/readme scope-allowed-other/readme; do
    debug "write $probe_dir/payload $file"
done
debug 'rm config.dat'
debug "write $probe_dir/payload config.dat"
debug 'rm double-existing'
debug "write $probe_dir/payload double-existing"
debug 'mkdir corrupt-dir'
debug 'mkdir indexed-dir'
for fixture in plain crowded; do
    echo "Hosted production-driver rename: $fixture"
    tests/filesystem-interop/build/rename-probe/rename_probe "$probe_dir/$fixture.img"
done
echo "Disposable diagnostic artifacts: $probe_dir"
