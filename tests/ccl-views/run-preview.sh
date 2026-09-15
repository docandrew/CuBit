#!/usr/bin/env bash
# Run inside nix develop. Exercises the actual shared Workbench, offscreen.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-views/views_tests.gpr
../tests/ccl-views/build/main
alr exec -- gprbuild -p -P ../userspace/ccl/ccl_ui_preview.gpr
alr exec -- gcc -shared -fPIC -Wall -Wextra -Werror \
  $(pkg-config --cflags sdl2) ../tests/ccl-file-dialog/workbench_events.c \
  -o ../tests/ccl-views/build/workbench-events.so -ldl $(pkg-config --libs sdl2)
syntax_captures=$(mktemp -d /tmp/cubit-syntax-views.XXXXXX)
timeout 30 env SDL_VIDEODRIVER=dummy SDL_RENDER_DRIVER=software \
  CCL_UI_WINDOW_WIDTH=1200 CCL_UI_WINDOW_HEIGHT=700 \
  CCL_TEST_SYNTAX='(let ((text (to-string 7))) (if (= (length text) 1) (concat "0" text) text))' \
  CCL_TEST_CAPTURE="$syntax_captures" \
  LD_PRELOAD="$PWD/../tests/ccl-views/build/workbench-events.so" \
  ../userspace/ccl/build/ccl-ui-preview/ccl-ui-preview
python3 ../tests/ccl-views/check_frames.py "$syntax_captures"
vm_captures=$(mktemp -d /tmp/cubit-syntax-vm.XXXXXX)
timeout 30 env SDL_VIDEODRIVER=dummy SDL_RENDER_DRIVER=software \
  CCL_UI_WINDOW_WIDTH=1200 CCL_UI_WINDOW_HEIGHT=700 \
  CCL_TEST_SYNTAX='(let ((answer (+ 20 22))) answer)' CCL_TEST_SYNTAX_VM=1 \
  CCL_TEST_CAPTURE="$vm_captures" \
  LD_PRELOAD="$PWD/../tests/ccl-views/build/workbench-events.so" \
  ../userspace/ccl/build/ccl-ui-preview/ccl-ui-preview
python3 ../tests/ccl-views/check_frames.py "$vm_captures" vm
