#!/usr/bin/env bash
# Invoke through nix develop. Isolated artifacts; never stage or launch a service.
set -euo pipefail
cd "$(dirname "$0")/../.."
exec 9>coordination/build.lock
flock --exclusive --nonblock 9
cd kernel
alr exec -- gprbuild -p -P ../tests/aml-core/native.gpr
python3 ../tests/aml-core/native_stack_report.py
