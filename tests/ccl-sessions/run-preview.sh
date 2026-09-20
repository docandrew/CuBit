#!/usr/bin/env bash
# Run inside nix develop; reuse the isolated SDL driver, not a user window.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-sessions/sessions_tests.gpr
../tests/ccl-sessions/build/main
../tests/ccl-sessions/build/host_tests
../tests/ccl-sessions/build/text_tests
../tests/ccl-sessions/build/function_tests
../tests/ccl-sessions/build/output_tests
alr exec -- gprbuild -p -P ../userspace/ccl/ccl_ui_preview.gpr
alr exec -- gcc -shared -fPIC -Wall -Wextra -Werror \
    $(pkg-config --cflags sdl2) ../tests/ccl-file-dialog/workbench_events.c \
    -o ../tests/ccl-sessions/build/workbench-events.so -ldl $(pkg-config --libs sdl2)
repl_captures=$(mktemp -d /tmp/cubit-repl.XXXXXX)
timeout 15 env SDL_VIDEODRIVER=dummy SDL_RENDER_DRIVER=software \
    CCL_UI_WINDOW_WIDTH=900 CCL_UI_WINDOW_HEIGHT=400 \
    CCL_TEST_REPL=1 CCL_TEST_CAPTURE="$repl_captures" \
    LD_PRELOAD="$PWD/../tests/ccl-sessions/build/workbench-events.so" \
    ../userspace/ccl/build/ccl-ui-preview/ccl-ui-preview
python3 ../tests/ccl-sessions/check_frames.py "$repl_captures"
ui_captures=$(mktemp -d /tmp/cubit-ccl-ui-hooks.XXXXXX)
timeout 25 env SDL_VIDEODRIVER=dummy SDL_RENDER_DRIVER=software \
    CCL_UI_WINDOW_WIDTH=1200 CCL_UI_WINDOW_HEIGHT=700 \
    CCL_TEST_UI_HOOKS=1 CCL_TEST_CAPTURE="$ui_captures" \
    LD_PRELOAD="$PWD/../tests/ccl-sessions/build/workbench-events.so" \
    ../userspace/ccl/build/ccl-ui-preview/ccl-ui-preview
python3 ../tests/ccl-sessions/check_ui_hooks.py "$ui_captures"
