#!/usr/bin/env bash
set -euo pipefail
proof_timeout="${CUBIT_ALLOCATOR_PROOF_TIMEOUT:-15}"
if [[ ! "$proof_timeout" =~ ^[1-9][0-9]*$ ]]; then
  echo 'CUBIT_ALLOCATOR_PROOF_TIMEOUT must be a positive integer (seconds)' >&2
  exit 2
fi
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/userspace-allocator/slab_tests.gpr
../tests/userspace-allocator/build/slabs/slab_tests
../tests/userspace-allocator/build/slabs/extent_tests
alr exec -- gnatprove -P ../tests/userspace-allocator/slab_tests.gpr \
  -u heap_classes.adb heap_bitmap.adb heap_extents.adb slab_model.ads heap_slab_instance.ads \
  --level=2 --timeout="$proof_timeout" --report=all --checks-as-errors=on -j2
python3 ../tests/userspace-allocator/check-proof.py
