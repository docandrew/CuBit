#!/usr/bin/env bash
# Compile actual callbacks against an existing prepared native Mesa build.
# Only scratch header/object outputs change; hold build lock for native work.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
build=$(realpath "${1:?prepared native Mesa build directory required}")
source_tree=$(realpath "$build/../source")
result=$(mktemp -d "$root/tests/mesa-anv/target/memory-callbacks.XXXXXX")
mkdir -p "$result/src/intel/vulkan"
cp "$source_tree/src/intel/vulkan/anv_private.h" "$result/src/intel/vulkan/"
# Current preparation already includes the field. Older prepared build trees
# need the scratch-only patch; never attempt to reverse/reapply it implicitly.
if ! rg -q 'struct cubit_cpu_mapping_tracker \* +cubit_cpu_mappings;' \
     "$result/src/intel/vulkan/anv_private.h"; then
  patch --batch --fuzz=0 -d "$result" -p1 < "$root/tests/mesa-anv/cpu-mapping-state.patch"
fi
compile_command=$(jq -r '.[] | select(.file | endswith("/anv_kmd_backend.c")) | .command' "$build/compile_commands.json")
test -n "$compile_command"
compile_command=${compile_command//src\/intel\/vulkan\/libanv_common.a.p\/anv_kmd_backend.c.o/$result/adapter.o}
compile_command=${compile_command/..\/source\/src\/intel\/vulkan\/anv_kmd_backend.c/$root/userspace/mesa/anv/anv_cubit_memory.c}
compile_command=${compile_command/ c / c -I$result/src/intel/vulkan }
(cd "$build" && eval "$compile_command")
printf 'Native ANV memory callbacks compile PASS: %s\n' "$result/adapter.o"
