#!/usr/bin/env bash
# Native USB HID regression for the completion dispatcher. This is NOT a
# USB-CD loading test: the normal fixture still loads applications from NVMe.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
test_dir=$(mktemp -d /tmp/cubit-usb-hid.XXXXXX)
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
make -C "$root/kernel" xhci devmgr desktop display devices
(
    ready=0
    # The headless harness refreshes kernel/initrd before opening the socket.
    # Include that build time in startup readiness, not the input deadline.
    for ((attempt=0; attempt<1200; attempt++)); do
        if [[ -S "$CUBIT_USB_MONITOR" ]] &&
           rg -q 'devices: native window ready' "$serial" 2>/dev/null; then
            ready=1
            break
        fi
        sleep 0.1
    done
    [[ "$ready" == 1 ]] || { echo "USB HID injector: desktop not ready" >&2; exit 1; }
    {
        # Small relative reports exercise the USB ring repeatedly without
        # relying on PS/2 acceleration or exact window coordinates.
        for ((i=0; i<256; i++)); do
            if ((i % 2 == 0)); then printf 'mouse_move 2 1\n';
            else printf 'mouse_move -2 -1\n'; fi
            if ((i % 32 == 0)); then printf 'mouse_button 1\n'; fi
            if ((i % 32 == 8)); then printf 'mouse_button 0\n'; fi
            sleep 0.02
        done
        printf 'mouse_button 0\n'
    } | nc -N -U "$CUBIT_USB_MONITOR" > "$test_dir/monitor.log"
) &
injector=$!
QEMU_BIN="$root/tests/usb-optical/qemu-hid.sh" \
    bash "$root/tests/headless/run.sh" --test devices --accel kvm \
        --cpus 1 --timeout 30 --keep-logs --serial "$serial" \
        > "$test_dir/headless.log" 2>&1
wait "$injector"
injector=""
rg -q 'xhci: interrupt-driven HID input enabled' "$serial"
if rg -q 'xhci: completion routing fault|EXCEPTION|PANIC' "$serial"; then
    echo "USB HID regression failed; logs: $test_dir" >&2
    exit 1
fi
# Require decoded motion AND button transitions, not just controller startup.
awk '
    /xhci: stats / {
        reports=0; motion=0; buttons=0; errors=0; short_reports=0
        for (i=1; i<=NF; i++) {
            split($i, pair, "=")
            if (pair[1]=="reports") reports=pair[2]+0
            if (pair[1]=="motion") motion=pair[2]+0
            if (pair[1]=="buttons") buttons=pair[2]+0
            if (pair[1]=="errors") errors=pair[2]+0
            if (pair[1]=="short") short_reports=pair[2]+0
        }
        if (errors != 0 || short_reports != 0) bad=1
        if (reports >= 128 && motion >= 128 && buttons >= 4) passed=1
    }
    END { exit !(passed && !bad) }
' "$serial"
echo "USB-HID: PASS native input and completion routing; logs: $test_dir"
