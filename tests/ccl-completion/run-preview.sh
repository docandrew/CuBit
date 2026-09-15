#!/usr/bin/env bash
# Run inside nix develop. This uses an isolated SDL window, not a user session.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../userspace/ccl/ccl_ui_preview.gpr
mkdir -p ../tests/ccl-completion/build
alr exec -- gcc -shared -fPIC -Wall -Wextra -Werror \
  $(pkg-config --cflags sdl2) ../tests/ccl-file-dialog/workbench_events.c \
  -o ../tests/ccl-completion/build/workbench-events.so -ldl $(pkg-config --libs sdl2)
completion_captures=$(mktemp -d /tmp/cubit-completion.XXXXXX)
timeout 20 env SDL_VIDEODRIVER=dummy SDL_RENDER_DRIVER=software \
  CCL_UI_WINDOW_WIDTH=1200 CCL_UI_WINDOW_HEIGHT=700 \
  CCL_TEST_COMPLETION=1 CCL_TEST_CAPTURE="$completion_captures" \
  LD_PRELOAD="$PWD/../tests/ccl-completion/build/workbench-events.so" \
  ../userspace/ccl/build/ccl-ui-preview/ccl-ui-preview
python3 ../tests/ccl-completion/check_frames.py "$completion_captures"
python3 ../tests/ccl-completion/check_contexts.py \
  ../userspace/ccl/build/ccl-ui-preview/ccl-ui-preview \
  "$PWD/../tests/ccl-completion/build/workbench-events.so"
