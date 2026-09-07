#!/usr/bin/env bash
# Run with nix develop -c bash tests/ccl-file-dialog/run-preview.sh
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../userspace/ccl/ccl_ui_preview.gpr
alr exec -- gprbuild -p -P ../tests/ccl-file-dialog/dialog_tests.gpr
../tests/ccl-file-dialog/build/main
alr exec -- gcc -shared -fPIC -Wall -Wextra -Werror \
    $(pkg-config --cflags sdl2) ../tests/ccl-file-dialog/workbench_events.c \
    -o ../tests/ccl-file-dialog/build/workbench-events.so -ldl $(pkg-config --libs sdl2)
dialog_captures=$(mktemp -d /tmp/cubit-file-dialog.XXXXXX)
timeout 15 env SDL_VIDEODRIVER=dummy SDL_RENDER_DRIVER=software \
    CCL_UI_WINDOW_WIDTH=900 CCL_UI_WINDOW_HEIGHT=400 \
    CCL_TEST_CAPTURE="$dialog_captures" \
    LD_PRELOAD="$PWD/../tests/ccl-file-dialog/build/workbench-events.so" \
    ../userspace/ccl/build/ccl-ui-preview/ccl-ui-preview
python3 ../tests/ccl-file-dialog/check_frames.py "$dialog_captures"
