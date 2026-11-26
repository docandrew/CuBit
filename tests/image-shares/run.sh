#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
# KERN-006 layer 1: the hosted test of the share rules, then the SPARK
# proof of Image_Share_Policy (level 2, no unproved check). Run inside nix
# develop; run gnatprove under a memory cap (systemd-run --user --scope
# -p MemoryMax=24G).
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -q -p -P ../tests/image-shares/shares.gpr
../tests/image-shares/build/policy_tests
alr exec -- gnatprove -P ../tests/image-shares/shares.gpr -u image_share_policy.adb \
  --level=2 --timeout=60 --report=fail --checks-as-errors=on -j2
echo 'image share policy: proved'
