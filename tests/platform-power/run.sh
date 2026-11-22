#!/usr/bin/env bash
# KERN-004 platform power object: hosted tests of the FADT register decoder,
# the request decoder, admission, the register plan and the PM1 values, then
# the SPARK proof of those units (level 2, no unproved check). Run inside nix
# develop; run gnatprove under a memory cap
# (systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0).
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -q -p -P ../tests/platform-power/power.gpr
../tests/platform-power/build/power_tests
alr exec -- gnatprove -P ../tests/platform-power/power.gpr \
  -u platform_power_policy.adb -u firmware_tables-power_registers.adb -u platform_power_abi.ads \
  --level=2 --timeout=60 --report=fail --checks-as-errors=on -j2
echo 'platform power: proved'
