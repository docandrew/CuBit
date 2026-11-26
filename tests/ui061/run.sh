#!/usr/bin/env bash
# UI-061 hosted tests: the thumbnail scaler, the hover card's layout and the
# app-name rule; --prove also proves Client_Thumbnails and Desktop_Task_Cards
# (level 2). Run under nix develop from anywhere.
set -euo pipefail
cd "$(dirname "$0")"
gprbuild -q -p -P ui061.gpr -XUI061_MODE=checked
./build/checked/ui061_tests
gprbuild -q -p -P ui061.gpr -XUI061_MODE=release
./build/release/ui061_tests time
if [[ "${1:-}" == "--prove" ]]; then
  gnatprove -P ui061_proof.gpr -j2 --level=2 --checks-as-errors=on --report=fail
fi
