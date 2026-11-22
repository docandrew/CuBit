#!/usr/bin/env bash
# Invoke through nix develop. Isolated artifacts; never stage or launch a service.
set -euo pipefail
# Resolved before any cd, while a relative $0 still names this script.
gprbuild_config=${CUBIT_NATIVE_GPRBUILD_CONFIG:-$(make -s --no-print-directory -C "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)/kernel" print-native-gprbuild-config)}
cd "$(dirname "$0")/../.."
exec 9>coordination/build.lock
flock --exclusive --nonblock 9
cd kernel
alr exec -- gprbuild $gprbuild_config -p -P ../tests/aml-core/native.gpr
python3 ../tests/aml-core/native_stack_report.py
