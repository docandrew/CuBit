#!/usr/bin/env bash
set -euo pipefail
if [[ -z "${IN_NIX_SHELL:-}" ]]; then
    echo 'Run with: nix develop -c bash tests/mesa-anv/test-presenter.sh' >&2
    exit 1
fi
project_root="$(cd "$(dirname "$0")/../.." && pwd)"
presenter_build="$(mktemp -d "${TMPDIR:-/tmp}/cubit-presenter.XXXXXXXX")"
cc -std=c11 -Wall -Wextra -Werror -fsanitize=address,undefined -g \
    "$project_root/tests/mesa-anv/presenter-lifetime-test.c" \
    "$project_root/userspace/mesa/anv/native_gpu_presenter.c" \
    -o "$presenter_build/presenter-test"
"$presenter_build/presenter-test"
echo "Presenter test executable retained: $presenter_build/presenter-test"
