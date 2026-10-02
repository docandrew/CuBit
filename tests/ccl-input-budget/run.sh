#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
preview="$root/userspace/ccl/build/ccl-ui-preview/ccl-ui-preview"
test_dir=$(mktemp -d "${TMPDIR:-/tmp}/cubit-input-flood.XXXXXXXX")
trap 'rm -rf "$test_dir"' EXIT
gcc -std=c11 -O2 -Wall -Wextra -Werror -shared -fPIC \
  $(pkg-config --cflags sdl2) "$root/tests/ccl-input-budget/flood.c" \
  -ldl -o "$test_dir/flood.so"
for clock in frozen elapsed; do
  (
    unset CCL_UI_PREVIEW_FRAMES CCL_UI_SCREENSHOT CUBIT_FLOOD_ELAPSED
    if [ "$clock" = elapsed ]; then export CUBIT_FLOOD_ELAPSED=1; fi
    export SDL_VIDEODRIVER=dummy SDL_RENDER_DRIVER=software
    # Apply the interposer only to the preview, never to timeout itself.
    timeout 30 env LD_PRELOAD="$test_dir/flood.so" "$preview"
  )
done
