#!/usr/bin/env bash
# The GTK frontend of `make -C kernel run-desktop`, driven on a private X
# server (Xvfb) instead of a person: the headless desktop-vm test runs with
# QEMU's GTK display, and its checker (check-desktop-vm.py, CUBIT_VM_GTK=1)
# first moves and clicks the host pointer in QEMU's window and resizes the
# window (xdotool), before the VNC/QMP checks. This exercises GTK's own
# absolute-input and resize paths (ui/gtk.c), which QMP and VNC bypass.
# Linux-hosted frontend behaviour: what only a person can judge is listed in
# docs/vm-guide.md ("Manual check").
#   tests/vm-experience/gtk-check.sh [run.sh options...]
# Needs Xvfb, xdotool and xwininfo on PATH (GTK_CHECK_TOOLS: a PATH prefix).
# Run under coordination/build.lock, like any headless run.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
if [ -n "${GTK_CHECK_TOOLS:-}" ]; then PATH="$GTK_CHECK_TOOLS:$PATH"; fi
for tool in Xvfb xdotool xwininfo; do
  command -v "$tool" >/dev/null || { echo "gtk-check: missing $tool" >&2; exit 2; }
done
DISPLAY_NUMBER="${GTK_CHECK_DISPLAY:-77}"
Xvfb ":$DISPLAY_NUMBER" -screen 0 2560x1600x24 -nolisten tcp >/dev/null 2>&1 &
XVFB=$!
trap 'kill "$XVFB" 2>/dev/null || true' EXIT
export DISPLAY=":$DISPLAY_NUMBER" GDK_BACKEND=x11 CUBIT_VM_GTK=1
for _ in $(seq 1 100); do xdotool getmouselocation >/dev/null 2>&1 && break; sleep 0.1; done
bash "$ROOT/tests/headless/run.sh" --test desktop-vm --display gtk,zoom-to-fit=on "$@"
