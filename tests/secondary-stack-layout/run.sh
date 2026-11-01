#!/usr/bin/env bash
#  Hosted checks of CuBit.Secondary_Stack_Layout (the per-thread secondary
#  stack's region layout and commit plan); --prove also runs gnatprove at
#  level 2. Run in the Nix shell from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
runtime="$here/../../userspace/runtime/gnat"
mkdir -p "$here/build/source"
cp "$runtime/cubit.ads" "$runtime/cubit-secondary_stack_layout.ads" \
   "$runtime/cubit-secondary_stack_layout.adb" "$here/build/source/"
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/layout_tests.gpr"
"$here/build/main"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/layout_tests.gpr" \
        -u cubit-secondary_stack_layout.adb --level=2 -j2 --checks-as-errors=on
fi
