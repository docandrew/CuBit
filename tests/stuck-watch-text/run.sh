#!/usr/bin/env bash
# Hosted check of how the kernel's stuck-call watch rows read.
set -euo pipefail
cd "$(dirname "$0")"
gprbuild -q -P stuck_watch_text_tests.gpr
./build/main
