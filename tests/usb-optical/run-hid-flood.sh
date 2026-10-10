#!/usr/bin/env bash
# USB pointer flood through qemu-xhci + usb-mouse: heavy HMP mouse_move bursts
# with left-button presses mid-flood. Prints the xHCI publication counters
# and desktop intake counters, and fails on any lost transition:
#   - xhci: overflow (retention loss) must be 0;
#   - desktop: source_gap must be the consumer-registration resync only (1),
#     source_reject 0, input_resync 0, and every injected left transition
#     must arrive (button= total).
# Flooding starts at desktop's shell activation by default, so it overlaps
# the Devices app launch and the first presentations: the desktop stalls
# there (the timing build's serial tracing makes it worse), which is the
# NUC failure mode (a consumer stall longer than retention) in miniature.
# Needs the timing desktop (stats lines): this script builds it with
# CUBIT_COMPOSITOR_TIMING=on. Run under coordination/build.lock (it stages
# services and boots the shared image). KVM required.
set -euo pipefail
root="${CUBIT_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
cycles="${HID_FLOOD_CYCLES:-40}"
burst="${HID_FLOOD_BURST:-200}"
test_dir=$(mktemp -d "${HID_FLOOD_DIR:-${TMPDIR:-/tmp}}/cubit-usb-flood.XXXXXX")
export CUBIT_USB_MONITOR="$test_dir/monitor.sock"
serial="$test_dir/serial.log"
injector=""
cleanup() {
    if [[ -n "$injector" ]]; then
        kill "$injector" 2>/dev/null || true
        wait "$injector" 2>/dev/null || true
    fi
}
trap cleanup EXIT
if [[ "${HID_FLOOD_SKIP_BUILD:-0}" != 1 ]]; then
    make -C "$root/kernel" xhci devmgr desktop display devices CUBIT_COMPOSITOR_TIMING=on
fi
(
    ready=0
    for ((attempt=0; attempt<1200; attempt++)); do
        if [[ -S "$CUBIT_USB_MONITOR" ]] &&
           grep -q "${HID_FLOOD_START:-desktop: internal shell active}" "$serial" 2>/dev/null; then
            ready=1
            break
        fi
        sleep 0.1
    done
    [[ "$ready" == 1 ]] || { echo "USB flood injector: desktop not ready" >&2; exit 1; }
    {
        # QEMU's HID model keeps 16 queued pointer states and merges motion
        # with unchanged buttons; it drops transitions only when 15 are
        # queued. Pace presses (>= 50 ms apart) so the device model itself
        # never loses one; motion bursts are unpaced.
        for ((c=0; c<cycles; c++)); do
            for ((i=0; i<burst; i++)); do
                if ((i % 2 == 0)); then printf 'mouse_move 7 3\n';
                else printf 'mouse_move -7 -3\n'; fi
            done
            printf 'mouse_button 1\n'
            sleep 0.05
            for ((i=0; i<burst; i++)); do
                if ((i % 2 == 0)); then printf 'mouse_move 5 -4\n';
                else printf 'mouse_move -5 4\n'; fi
            done
            printf 'mouse_button 0\n'
            sleep 0.05
        done
        sleep 3
    } | nc -N -U "$CUBIT_USB_MONITOR" > "$test_dir/monitor.log"
) &
injector=$!
QEMU_BIN="$root/tests/usb-optical/qemu-hid.sh" \
    bash "$root/tests/headless/run.sh" --test devices --accel kvm \
        --cpus "${HID_FLOOD_CPUS:-2}" --timeout "${HID_FLOOD_TIMEOUT:-45}" \
        --keep-logs --serial "$serial" > "$test_dir/headless.log" 2>&1
wait "$injector"
injector=""
# xhci.drv's text goes to logsvc (Boot_Log), not necessarily the serial
# console; desktop's counters below are the end-to-end evidence.
if grep -Eq 'xhci: completion routing fault|EXCEPTION|PANIC' "$serial"; then
    echo "USB flood failed (fault); logs: $test_dir" >&2
    exit 1
fi
python3 - "$serial" "$((cycles * 2))" <<'EOF'
import re, sys
serial, injected = sys.argv[1], int(sys.argv[2])
text = open(serial, errors="replace").read()
def fields(line):
    return {k: int(v) for k, v in re.findall(r"\b([a-z_]+)=(\d+)", line)}
xhci = [fields(l) for l in text.splitlines() if "xhci: stats " in l]
desk = [fields(l) for l in text.splitlines() if "desktop: stats " in l]
frames = [fields(l) for l in text.splitlines() if "desktop: frames=" in l]
last = xhci[-1] if xhci else {}
def total(rows, key):
    return sum(r.get(key, 0) for r in rows)
drop_key = "event_busy" if any("event_busy" in r for r in desk) else "event_drop"
summary = {
    "xhci_reports": last.get("reports"), "xhci_motion": last.get("motion"),
    "xhci_buttons": last.get("buttons"), "xhci_coalesced": last.get("coalesced"),
    "xhci_overflow": last.get("overflow"), "xhci_busy": last.get("busy"),
    "desktop_mouse": total(desk, "mouse"), "desktop_button": total(desk, "button"),
    "desktop_" + drop_key: total(desk, drop_key),
    "desktop_input_resync": total(desk, "input_resync"),
    "desktop_source_gap": total(desk, "source_gap"),
    "desktop_source_reject": total(desk, "source_reject"),
    "desktop_src_age_max_ms": max((r.get("src_age_max_ms", 0) for r in desk), default=None),
    "desktop_max_motion_ms": max((r.get("max_motion_ms", 0) for r in frames), default=None),
    "injected_left_transitions": injected,
}
for k, v in summary.items():
    print(f"USB-FLOOD: {k}={v}")
lost = (summary["xhci_overflow"] not in (None, 0) or
        summary["desktop_source_gap"] > 1 or summary["desktop_source_reject"] or
        summary["desktop_input_resync"] or summary["desktop_button"] < injected or
        "retention overflow" in text)
print("USB-FLOOD:", "FAIL lost or resynchronized input" if lost else "PASS no lost transitions")
sys.exit(1 if lost else 0)
EOF
echo "USB-FLOOD: logs $test_dir"
