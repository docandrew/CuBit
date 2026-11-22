#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
out=$(mktemp -d /tmp/cubit-gallery-present.XXXXXX)
cc -std=c11 -O2 -Wall -Wextra -Werror $(pkg-config --cflags vulkan) \
   "$root/tests/mesa-anv/gallery-present-test.c" -o "$out/test"
for mode in 0 1 2 3 4 5 6 7; do "$out/test" "$mode"; done
echo "Gallery presentation wrapper HOST MOCK PASS: $out"
