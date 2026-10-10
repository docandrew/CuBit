#!/usr/bin/env bash
# QEMU wrapper for tests/headless/run.sh (QEMU_BIN=this script). Adds host-
# side evidence for the virtio-gpu cursor plane without changing the guest:
#   * QEMU's own virtio_gpu_update_cursor trace (what the host was told);
#   * screendumps of the guest scanout at each DISPLAY-PLANES marker;
#   * the cursor image QEMU publishes to a VNC client (RichCursor).
# Outputs go to $CUBIT_PLANES_EVIDENCE (a directory). Real QEMU is
# $CUBIT_REAL_QEMU or qemu-system-x86_64 from PATH.
set -uo pipefail
OUT="${CUBIT_PLANES_EVIDENCE:?set CUBIT_PLANES_EVIDENCE}"
REAL="${CUBIT_REAL_QEMU:-qemu-system-x86_64}"
HERE="$(cd "$(dirname "$0")" && pwd)"
if [ "${1:-}" = --version ]; then exec "$REAL" --version; fi
mkdir -p "$OUT"
SERIAL=""
prev=""
for a in "$@"; do
  if [ "$prev" = -serial ] && [[ "$a" == file:* ]]; then SERIAL="${a#file:}"; fi
  prev="$a"
done
MON="$OUT/monitor.sock"
VNC="$OUT/vnc.sock"
rm -f "$MON" "$VNC"
if [ "${CUBIT_PLANES_DESKTOP:-0}" = 1 ]; then
(
  # Desktop pointer: once Desktop presents, move the PS/2 mouse from the
  # host monitor, then capture the scanout, the trace and the VNC cursor.
  for _ in $(seq 1 1200); do
    if [ -n "$SERIAL" ] && grep -q "desktop: asynchronous presentation active" "$SERIAL" 2>/dev/null; then
      sleep 2
      for _ in 1 2 3 4 5 6; do
        printf 'mouse_move 40 25\n' | nc -U -q 1 "$MON" >/dev/null 2>&1
        sleep 0.3
      done
      sleep 1
      printf 'screendump "%s"\n' "$OUT/desktop.ppm" | nc -U -q 1 "$MON" >/dev/null 2>&1
      python3 "$HERE/vnc_cursor.py" "$VNC" "$OUT/desktop-cursor.json" >"$OUT/desktop-vnc.log" 2>&1
      cp "$OUT/qemu-trace.log" "$OUT/desktop-trace.log" 2>/dev/null
      break
    fi
    sleep 0.1
  done
) &
fi
(
  # Wait for each guest marker, then capture while the guest holds still.
  for step in shown moved swapped; do
    for _ in $(seq 1 1200); do
      if [ -n "$SERIAL" ] && grep -q "DISPLAY-PLANES: $step" "$SERIAL" 2>/dev/null; then
        sleep 0.5
        printf 'screendump "%s"\n' "$OUT/$step.ppm" | nc -U -q 1 "$MON" >/dev/null 2>&1
        python3 "$HERE/vnc_cursor.py" "$VNC" "$OUT/$step-cursor.json" >"$OUT/$step-vnc.log" 2>&1
        cp "$OUT/qemu-trace.log" "$OUT/$step-trace.log" 2>/dev/null
        break
      fi
      sleep 0.1
    done
  done
) &
exec "$REAL" "$@" \
  -monitor "unix:$MON,server,nowait" \
  -vnc "unix:$VNC" \
  -D "$OUT/qemu-trace.log" -trace enable=virtio_gpu_update_cursor
