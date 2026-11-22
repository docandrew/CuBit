#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
source_tree=${1:?prepared Mesa source required}
host_tree=${2:-$root/tests/mesa-anv/build-host}
out=$(mktemp -d "$root/tests/mesa-anv/target/vertex-fetch.XXXXXX")
cc -std=c11 -D_GNU_SOURCE -DHAVE_ENDIAN_H -Wall -Wextra -Werror \
  -Wno-override-init -isystem "$source_tree/src" \
  -isystem "$source_tree/include" -isystem "$host_tree/src/intel/genxml" \
  -isystem "$source_tree/src/intel/genxml" \
  "$root/tests/mesa-anv/vertex-fetch-test.c" -lm -o "$out/probe"
"$out/probe" | tee "$out/result.txt"
echo "Retained vertex-fetch oracle: $out"
