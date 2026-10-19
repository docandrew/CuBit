#!/usr/bin/env bash
# Workbench's menu bar (CuBit.UI.Menus), Linux-hosted: the real shared
# Workbench loop in the SDL preview, driven by menu_events.c (F10, arrows,
# Alt+letter, mnemonics, presses on a title and outside). check_frames.py
# checks the presented frames. Run inside the Nix shell:
#   nix develop -c bash tests/workbench-menus/run.sh [PNG_DIR]
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
make ui-fonts-host
preview=${CCL_UI_PREVIEW:-../userspace/ccl/build/ccl-ui-preview/ccl-ui-preview}
if [[ -z ${CCL_UI_PREVIEW:-} ]]; then
    alr exec -- gprbuild -p -P ../userspace/ccl/ccl_ui_preview.gpr
fi
mkdir -p "$here/build"
alr exec -- gcc -shared -fPIC -Wall -Wextra -Werror \
    $(pkg-config --cflags sdl2) "$here/menu_events.c" \
    -o "$here/build/menu-events.so" -ldl $(pkg-config --libs sdl2)
captures=$(mktemp -d "${TMPDIR:-/tmp}/cubit-workbench-menus.XXXXXX")
timeout 30 env SDL_VIDEODRIVER=dummy SDL_RENDER_DRIVER=software \
    CCL_UI_WINDOW_WIDTH=900 CCL_UI_WINDOW_HEIGHT=500 \
    CCL_TEST_CAPTURE="$captures" \
    LD_PRELOAD="$here/build/menu-events.so" \
    "$preview" > "$captures/preview.log" 2>&1 || true
python3 "$here/check_frames.py" "$captures" ${1:+"$1"}
