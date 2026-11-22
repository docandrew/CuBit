#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
# Hosted Debugger views (DBG-001), Linux-hosted against the TEST DOUBLE in
# double/: no CuBit process is read. Run inside nix develop with TMPDIR
# outside /tmp (needs userspace/rust fonts-host built):
#   tests/debugger-gui/run.sh [rounds]
# Checks layout, keys, locals tree, graph stability, and prints frame times
# (TIME: lines) at 1080p and 2160p; writes build/debugger-*.ppm.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -j4 -P "$here/debugger_gui.gpr"
cd "$here" && build/debugger_gui_tests "${1:-20}"
