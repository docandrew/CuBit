#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/userspace-allocator/diagnostics.gpr >&2
../tests/userspace-allocator/build/diagnostics/diagnostics
