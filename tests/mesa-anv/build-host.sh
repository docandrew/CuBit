#!/usr/bin/env bash
# Linux baseline only. Use host-shell.nix; never selects a CuBit ABI.
set -euo pipefail
build_tree=${1:?configured out-of-tree build directory required}
test -f "$build_tree/build.ninja"
build_tree=$(realpath "$build_tree")
mkdir -p "$build_tree/tmp"
export TMPDIR="$build_tree/tmp"
exec ninja -C "$build_tree" -j2 src/intel/vulkan/libvulkan_intel.so
