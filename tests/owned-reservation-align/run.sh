#!/usr/bin/env bash
# RESERVE_OWNED_MEMORY's alignment argument: hosted checks of the kernel's
# Owned_Reservation_Policy rules and Find_Base's walk, then the policy's
# SPARK proof. Run inside nix develop; run gnatprove under a memory cap.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -q -p -P ../tests/owned-reservation-align/align.gpr
../tests/owned-reservation-align/build/align_tests
alr exec -- gnatprove -P ../tests/owned-reservation-align/align.gpr -u owned_reservation_policy.adb \
  --level=2 --timeout=30 --report=fail --checks-as-errors=on -j2
echo 'owned reservation policy: proved'
