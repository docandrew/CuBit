#!/usr/bin/env bash
# Run in nix develop. Hosted parser/link regression, NOT native CuBit execution.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root"
out=$(mktemp -d /tmp/cubit-mesa-build-id.XXXXXX)
echo "Build-ID regression artifacts: $out"
cc -std=c11 -Wall -Wextra -Werror -fsanitize=address,undefined \
  -DCUBIT_TEST_LINKED_BUILD_ID -Iuserspace/mesa/anv \
  userspace/mesa/anv/native_build_id.c \
  userspace/mesa/anv/native_build_id_link.c \
  tests/mesa-anv/native-build-id-test.c \
  -Wl,--build-id=sha1 -Wl,-T,userspace/mesa/anv/native_build_id.ld \
  -o "$out/test"
"$out/test"
cc -c -fno-pie -ffreestanding tests/mesa-anv/native-build-id-layout.c \
  -o "$out/layout.o"
ld --build-id=sha1 -T userspace/mesa/anv/native_build_id.ld \
  -T userspace/libc/link.ld "$out/layout.o" -o "$out/layout.elf"
python3 tests/mesa-anv/check-native-build-id.py "$out/layout.elf"
