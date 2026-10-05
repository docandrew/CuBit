#!/usr/bin/env bash
# Run inside the pinned Nix environment. Hosted mock transport, not GPU evidence.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
task_out=$(mktemp -d /tmp/cubit-mapping-growth.XXXXXX)
cc -std=c11 -Wall -Wextra -Werror -g -fsanitize=address,undefined \
  -DCUBIT_TEST_ALLOC_FAILURE -Wl,--wrap=calloc \
  "$root/tests/mesa-anv/mapping-lifetime-test.c" \
  "$root/userspace/mesa/anv/native_gpu_mapping.c" -o "$task_out/test"
"$task_out/test"
echo "Hosted mapping-growth evidence: $task_out"
