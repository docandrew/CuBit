#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../userspace/ccl/ccl_ui_preview.gpr
mkdir -p ../tests/ccl-callbacks/build
alr exec -- gcc -shared -fPIC -Wall -Wextra -Werror \
  $(pkg-config --cflags sdl2) ../tests/ccl-file-dialog/workbench_events.c \
  -o ../tests/ccl-callbacks/build/workbench-events.so -ldl $(pkg-config --libs sdl2)
button_captures=$(mktemp -d /tmp/cubit-ccl-button.XXXXXX)
button_source='(define (clicked) Boolean (ui.label-text "Clicked!")) (let ((caption (ui.button-text "Try me"))) (ui.button-on-click (handler clicked)))'
timeout 40 env SDL_VIDEODRIVER=dummy SDL_RENDER_DRIVER=software \
  CCL_UI_WINDOW_WIDTH=1200 CCL_UI_WINDOW_HEIGHT=700 \
  CCL_TEST_BUTTON="$button_source" CCL_TEST_CAPTURE="$button_captures" \
  LD_PRELOAD="$PWD/../tests/ccl-callbacks/build/workbench-events.so" \
  ../userspace/ccl/build/ccl-ui-preview/ccl-ui-preview
python3 ../tests/ccl-callbacks/check_button.py "$button_captures" "$button_source"
