#!/usr/bin/env bash
# Host semantic regression + CuBit compile check. No target execution.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
source_tree=${1:?prepared ANV source required}
generated_tree=${2:?configured CuBit ANV build required}
host_tree=${3:-$root/tests/mesa-anv/build-host}
out=$(mktemp -d "$root/tests/mesa-anv/target/topology-test.XXXXXX")
includes=(-isystem "$source_tree/include" -isystem "$source_tree/src"
          -isystem "$generated_tree/src" -I "$root/userspace/mesa/anv")
flags=(-std=c11 -D_GNU_SOURCE -DHAVE_ENDIAN_H -Wall -Wextra -Werror)
cc "${flags[@]}" "${includes[@]}" \
  "$root/userspace/mesa/anv/cubit-topology.c" \
  "$root/tests/mesa-anv/cubit-topology-test.c" \
  -Wl,--gc-sections "$host_tree/src/intel/dev/libintel_dev.a" \
  -o "$out/host-test"
"$out/host-test"
"$root/userspace/libc/cubit-cc" "${flags[@]}" "${includes[@]}" \
  -D__cubit__ -c "$root/userspace/mesa/anv/cubit-topology.c" -o "$out/native.o"
echo "Topology tests PASS (Linux helpers + CuBit compile only): $out"
