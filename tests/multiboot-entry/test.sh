#!/usr/bin/env bash
set -euo pipefail
test_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
mkdir -p "$test_dir/build"
yasm -f elf64 -I "$test_dir/../../kernel/src/" "$test_dir/gate.asm" \
    -o "$test_dir/build/entry_gate.o"
cd "$test_dir/../../kernel"
alr exec -- gprbuild -p -P "$test_dir/entry_tests.gpr" \
    -largs "$test_dir/build/entry_gate.o"
"$test_dir/build/main"
