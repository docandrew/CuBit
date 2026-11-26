#!/usr/bin/env bash
# ABI-002 owned regions: the hosted model test of the kernel's run
# bookkeeping, then the SPARK proof of Owned_Reservation_Policy (level 2,
# no unproved check). Run inside nix develop; run gnatprove under a memory
# cap (systemd-run --user --scope -p MemoryMax=24G).
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -q -p -P ../tests/owned-regions/regions.gpr
../tests/owned-regions/build/region_tests
../tests/owned-regions/build/release_walk_tests
alr exec -- gnatprove -P ../tests/owned-regions/regions.gpr -u owned_reservation_policy.adb \
  --level=2 --timeout=60 --report=fail --checks-as-errors=on -j2
echo 'owned region policy: proved'
