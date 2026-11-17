#!/usr/bin/env bash
# ABI-002 slice 2 / KERN-013: hosted test of WAIT's decision against a
# reference over every Sources word with each deadline class, and of the
# readiness counter; then the SPARK proof of Wait_Policy, Wait_ABI and
# Readiness_Publication (level 2, no unproved check). Run inside nix
# develop; run gnatprove under a memory cap
# (systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0).
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -q -p -P ../tests/wait-policy/wait.gpr
../tests/wait-policy/build/wait_tests
alr exec -- gnatprove -P ../tests/wait-policy/wait.gpr \
  -u wait_policy.adb -u wait_abi.ads -u readiness_publication.ads \
  --level=2 --timeout=60 --report=fail --checks-as-errors=on -j2
echo 'wait policy: proved'
