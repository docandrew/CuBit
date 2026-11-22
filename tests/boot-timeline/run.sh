#!/usr/bin/env bash
# Boot timeline (BOOT-001, docs/boot-timeline.md): hosted checks of the
# kernel's bookkeeping and the reader's analysis; --prove adds the SPARK
# proof (level 2) of both. Run inside the Nix shell, e.g.
#   nix develop -c make -C kernel prove-boot-timeline
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE"
gprbuild -q -p -P boot_timeline_tests.gpr
./build/test/boot_timeline_tests
if [ "${1:-}" = "--prove" ]; then
    gnatprove -P prove.gpr -j0 --report=fail
    grep -E "^(Total|Run-time|Functional|Assertions|Termination|Data|Flow)" \
        build/prove/gnatprove/gnatprove.out || true
fi
