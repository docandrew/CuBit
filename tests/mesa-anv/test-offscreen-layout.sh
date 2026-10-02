#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
source_tree=${1:?prepared Mesa source required}
host_tree=${2:-$root/tests/mesa-anv/build-host}
out=$(mktemp -d "$root/tests/mesa-anv/target/offscreen-layout.XXXXXX")
cc -std=c11 -D_GNU_SOURCE -DHAVE_ENDIAN_H -Wall -Wextra -Werror \
  -isystem "$source_tree/include" -isystem "$source_tree/src" \
  -isystem "$host_tree/src" -isystem "$host_tree/include" \
  "$root/tests/mesa-anv/offscreen-layout-test.c" -Wl,--gc-sections \
  -Wl,--start-group "$host_tree"/src/intel/isl/*.a \
  "$host_tree/src/intel/dev/libintel_dev.a" "$host_tree"/src/util/*.a \
  -Wl,--end-group -ldrm -lexpat -lzstd -lz -lm -ldl -pthread -o "$out/probe"
"$out/probe"
echo "Retained hosted oracle: $out"
