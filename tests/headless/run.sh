#!/usr/bin/env bash
set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
KERNEL_DIR="$ROOT_DIR/kernel"
QEMU_BIN="${QEMU_BIN:-qemu-system-x86_64}"
TIMEOUT_BIN="${TIMEOUT_BIN:-timeout}"

TEST_NAME="boot-shell-nvme"
TIMEOUT_SECONDS=25
BUILD_WORLD=0
KEEP_LOGS=0
QEMU_ACCEL=""
SERIAL_LOG=""
NET_PCAP=""
BASE_DISK=""
TEMP_DISK=""
TEMP_AUDIO=""
TEMP_STORAGE_FIXTURE=""
MONITOR_SOCKET=""
QMP_SOCKET=""
INPUT_INJECTOR_PID=""

usage() {
    cat <<'EOF'
Usage: tests/headless/run.sh [options]

Options:
  --build              Run make world before booting QEMU
  --test NAME          Test to run: boot-shell-nvme, async-ipc, bench-ipc, ccl-vm, ccl-workbench, ccl-workbench-virtio-vga, capability-security, storage-grants, audio-grants, desktop-display, input-stream, devices, files, desktop-doom, desktop-virtio-vga, virtio-gpu, or virtio-vga-primary
  --timeout SECONDS    QEMU runtime before timeout is treated as success
  --accel NAME         QEMU accelerator (for example: tcg,thread=multi)
  --disk PATH          Base ext2 disk image (default: kernel/nvme_disk.img)
  --serial PATH        Serial log path (default: /tmp/cubit-headless-*.log)
  --pcap PATH          Packet capture path (default: /tmp/cubit-headless-*.pcap)
  --keep-logs          Leave logs in place after a passing run
  -h, --help           Show this help

The suite boots the NVMe profile headlessly and checks serial output for
stable pass markers.
EOF
}

while [ "$#" -gt 0 ]; do
    case "$1" in
        --build)
            BUILD_WORLD=1
            shift
            ;;
        --test)
            if [ "$#" -lt 2 ]; then
                echo "headless: --test requires a value" >&2
                exit 2
            fi
            TEST_NAME="$2"
            shift 2
            ;;
        --timeout)
            if [ "$#" -lt 2 ]; then
                echo "headless: --timeout requires a value" >&2
                exit 2
            fi
            TIMEOUT_SECONDS="$2"
            shift 2
            ;;
        --accel)
            if [ "$#" -lt 2 ]; then
                echo "headless: --accel requires a value" >&2
                exit 2
            fi
            QEMU_ACCEL="$2"
            shift 2
            ;;
        --disk)
            if [ "$#" -lt 2 ]; then
                echo "headless: --disk requires a value" >&2
                exit 2
            fi
            BASE_DISK="$2"
            shift 2
            ;;
        --serial)
            if [ "$#" -lt 2 ]; then
                echo "headless: --serial requires a value" >&2
                exit 2
            fi
            SERIAL_LOG="$2"
            shift 2
            ;;
        --pcap)
            if [ "$#" -lt 2 ]; then
                echo "headless: --pcap requires a value" >&2
                exit 2
            fi
            NET_PCAP="$2"
            shift 2
            ;;
        --keep-logs)
            KEEP_LOGS=1
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "headless: unknown option: $1" >&2
            usage >&2
            exit 2
            ;;
    esac
done

case "$TIMEOUT_SECONDS" in
    ''|*[!0-9]*)
        echo "headless: --timeout must be an integer number of seconds" >&2
        exit 2
        ;;
esac

case "$TEST_NAME" in
    boot-shell-nvme|async-ipc|bench-ipc|ccl-vm|ccl-workbench|ccl-workbench-virtio-vga|capability-security|storage-grants|audio-grants|desktop-display|input-stream|devices|files|desktop-doom|desktop-virtio-vga|virtio-gpu|virtio-vga-primary)
        ;;
    *)
        echo "headless: unknown test: $TEST_NAME" >&2
        exit 2
        ;;
esac

if ! command -v "$QEMU_BIN" >/dev/null 2>&1; then
    echo "headless: missing $QEMU_BIN" >&2
    exit 127
fi

if ! command -v "$TIMEOUT_BIN" >/dev/null 2>&1; then
    echo "headless: missing $TIMEOUT_BIN" >&2
    exit 127
fi

if ! command -v grub-mkrescue >/dev/null 2>&1; then
    echo "headless: missing grub-mkrescue" >&2
    exit 127
fi

if [ "$TEST_NAME" != "boot-shell-nvme" ] && ! command -v debugfs >/dev/null 2>&1; then
    echo "headless: missing debugfs" >&2
    exit 127
fi

if [ -z "$SERIAL_LOG" ]; then
    SERIAL_LOG="${TMPDIR:-/tmp}/cubit-headless-${TEST_NAME}-serial.log"
fi

if [ -z "$NET_PCAP" ]; then
    NET_PCAP="${TMPDIR:-/tmp}/cubit-headless-${TEST_NAME}-net.pcap"
fi

GRUB_CFG="$KERNEL_DIR/isodir/boot/grub/grub.cfg"
GRUB_BAK="$(mktemp "${TMPDIR:-/tmp}/cubit-grub.XXXXXX")"
cp "$GRUB_CFG" "$GRUB_BAK"

cleanup() {
    if [ -n "$INPUT_INJECTOR_PID" ]; then
        kill "$INPUT_INJECTOR_PID" >/dev/null 2>&1 || true
        wait "$INPUT_INJECTOR_PID" >/dev/null 2>&1 || true
    fi
    if [ -n "$MONITOR_SOCKET" ]; then
        rm -f "$MONITOR_SOCKET"
    fi
    if [ -n "$QMP_SOCKET" ]; then
        rm -f "$QMP_SOCKET"
    fi
    cp "$GRUB_BAK" "$GRUB_CFG"
    rm -f "$GRUB_BAK"
    if [ -n "$TEMP_DISK" ]; then
        rm -f "$TEMP_DISK"
    fi
    if [ -n "$TEMP_STORAGE_FIXTURE" ]; then
        rm -f "$TEMP_STORAGE_FIXTURE"
    fi
    if [ -n "$TEMP_AUDIO" ] && [ "$KEEP_LOGS" -eq 0 ] &&
       [ "${HEADLESS_TEST_FAILED:-0}" -eq 0 ]; then
        rm -f "$TEMP_AUDIO"
    fi
    if [ "$KEEP_LOGS" -eq 0 ] && [ "${HEADLESS_TEST_FAILED:-0}" -eq 0 ]; then
        rm -f "$NET_PCAP"
    fi
}
trap cleanup EXIT INT TERM

HEADLESS_TEST_FAILED=1

if [ "$BUILD_WORLD" -eq 1 ]; then
    make -C "$KERNEL_DIR" world
fi

if [ -z "$BASE_DISK" ]; then
    BASE_DISK="$KERNEL_DIR/nvme_disk.img"
fi

if [ ! -f "$BASE_DISK" ]; then
    echo "headless: missing disk image: $BASE_DISK" >&2
    echo "headless: run make -C kernel world, pass --build, or select one with --disk" >&2
    exit 1
fi

DISK_IMAGE="$BASE_DISK"
INIT_PROFILE=""
case "$TEST_NAME" in
    async-ipc)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-async-ipc.conf"
        ;;
    bench-ipc)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-bench-ipc.conf"
        ;;
    ccl-vm)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ccl-vm.conf"
        ;;
    ccl-workbench|ccl-workbench-virtio-vga)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ccl-workbench.conf"
        ;;
    capability-security)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-capability-security.conf"
        ;;
    storage-grants)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-storage-grants.conf"
        ;;
    desktop-display|desktop-virtio-vga)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-desktop-display.conf"
        ;;
    input-stream)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-input-stream.conf"
        ;;
    devices)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-devices.conf"
        ;;
    files)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-files.conf"
        ;;
    desktop-doom)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-doom-desktop.conf"
        ;;
    virtio-gpu|virtio-vga-primary)
        if [ "$TEST_NAME" = "virtio-vga-primary" ]; then
            INIT_PROFILE="$ROOT_DIR/init.conf"
        fi
        ;;
esac

if [ -n "$INIT_PROFILE" ]; then
    TEMP_DISK="$(mktemp "${TMPDIR:-/tmp}/cubit-${TEST_NAME}-disk.XXXXXX.img")"
    cp "$BASE_DISK" "$TEMP_DISK"
    debugfs -w -R "rm init.conf" "$TEMP_DISK" >/dev/null 2>&1
    if ! debugfs -w -R "write $INIT_PROFILE init.conf" "$TEMP_DISK" >/dev/null 2>&1; then
        echo "headless: failed to install $TEST_NAME init.conf" >&2
        exit 1
    fi
    if [ "$TEST_NAME" = "desktop-doom" ]; then
        DOOM_IMAGE="$KERNEL_DIR/isodir/boot/doom.elf"
        if [ ! -f "$DOOM_IMAGE" ]; then
            echo "headless: missing current DOOM image: $DOOM_IMAGE" >&2
            exit 1
        fi
        debugfs -w -R "rm doom.elf" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $DOOM_IMAGE doom.elf" "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install current doom.elf" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "desktop-display" ] ||
       [ "$TEST_NAME" = "ccl-workbench" ] ||
       [ "$TEST_NAME" = "ccl-workbench-virtio-vga" ] ||
       [ "$TEST_NAME" = "input-stream" ] ||
       [ "$TEST_NAME" = "devices" ] ||
       [ "$TEST_NAME" = "files" ] ||
       [ "$TEST_NAME" = "desktop-virtio-vga" ] ||
       [ "$TEST_NAME" = "desktop-doom" ] ||
       [ "$TEST_NAME" = "virtio-vga-primary" ]; then
        for DESKTOP_TEST_IMAGE_NAME in display.svc desktop.svc; do
            DESKTOP_TEST_IMAGE="$KERNEL_DIR/isodir/boot/$DESKTOP_TEST_IMAGE_NAME"
            if [ ! -f "$DESKTOP_TEST_IMAGE" ]; then
                echo "headless: missing current desktop test image: $DESKTOP_TEST_IMAGE" >&2
                exit 1
            fi
            debugfs -w -R "rm $DESKTOP_TEST_IMAGE_NAME" \
              "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R \
              "write $DESKTOP_TEST_IMAGE $DESKTOP_TEST_IMAGE_NAME" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $DESKTOP_TEST_IMAGE_NAME" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "ccl-vm" ] || [ "$TEST_NAME" = "ccl-workbench" ] ||
       [ "$TEST_NAME" = "ccl-workbench-virtio-vga" ]; then
        if [ "$TEST_NAME" = "ccl-vm" ]; then
            CCL_IMAGES="ccl-vm.app ccl-test-host.svc clock.svc"
        else
            CCL_IMAGES="ccl-workbench.app clock.svc desktop.svc display.svc"
        fi
        for CCL_IMAGE_NAME in $CCL_IMAGES; do
            CCL_IMAGE="$KERNEL_DIR/isodir/boot/$CCL_IMAGE_NAME"
            if [ ! -f "$CCL_IMAGE" ]; then
                echo "headless: missing current CCL image: $CCL_IMAGE" >&2
                exit 1
            fi
            debugfs -w -R "rm $CCL_IMAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $CCL_IMAGE $CCL_IMAGE_NAME" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $CCL_IMAGE_NAME" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "devices" ]; then
        for DEVICE_TEST_IMAGE_NAME in devices.app desktop.svc; do
            DEVICE_TEST_IMAGE="$KERNEL_DIR/isodir/boot/$DEVICE_TEST_IMAGE_NAME"
            if [ ! -f "$DEVICE_TEST_IMAGE" ]; then
                echo "headless: missing current Devices test image: $DEVICE_TEST_IMAGE" >&2
                exit 1
            fi
            debugfs -w -R "rm $DEVICE_TEST_IMAGE_NAME" \
              "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R \
              "write $DEVICE_TEST_IMAGE $DEVICE_TEST_IMAGE_NAME" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $DEVICE_TEST_IMAGE_NAME" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "files" ]; then
        for FILES_TEST_IMAGE_NAME in files.app desktop.svc; do
            FILES_TEST_IMAGE="$KERNEL_DIR/isodir/boot/$FILES_TEST_IMAGE_NAME"
            if [ ! -f "$FILES_TEST_IMAGE" ]; then
                echo "headless: missing current Files test image: $FILES_TEST_IMAGE" >&2
                exit 1
            fi
            debugfs -w -R "rm $FILES_TEST_IMAGE_NAME" \
              "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R \
              "write $FILES_TEST_IMAGE $FILES_TEST_IMAGE_NAME" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $FILES_TEST_IMAGE_NAME" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "capability-security" ]; then
        CAPABILITY_TEST_IMAGE="$KERNEL_DIR/isodir/boot/capability-test.app"
        if [ ! -f "$CAPABILITY_TEST_IMAGE" ]; then
            echo "headless: missing current capability test image: $CAPABILITY_TEST_IMAGE" >&2
            exit 1
        fi
        debugfs -w -R "rm capability-test.app" \
          "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R \
          "write $CAPABILITY_TEST_IMAGE capability-test.app" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install capability-test.app" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "input-stream" ]; then
        for INPUT_TEST_IMAGE_NAME in \
          input-stress.app ccl-workbench.app clock.svc; do
            INPUT_TEST_IMAGE="$KERNEL_DIR/isodir/boot/$INPUT_TEST_IMAGE_NAME"
            if [ ! -f "$INPUT_TEST_IMAGE" ]; then
                echo "headless: missing current input test image: $INPUT_TEST_IMAGE" >&2
                exit 1
            fi
            debugfs -w -R "rm $INPUT_TEST_IMAGE_NAME" \
              "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R \
              "write $INPUT_TEST_IMAGE $INPUT_TEST_IMAGE_NAME" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $INPUT_TEST_IMAGE_NAME" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "storage-grants" ]; then
        STORAGE_TEST_IMAGE="$KERNEL_DIR/isodir/boot/storage-check.app"
        if [ ! -f "$STORAGE_TEST_IMAGE" ]; then
            echo "headless: missing current storage-check image: $STORAGE_TEST_IMAGE" >&2
            exit 1
        fi
        debugfs -w -R "rm storage-check.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R \
          "write $STORAGE_TEST_IMAGE storage-check.app" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install storage-check.app" >&2
            exit 1
        fi
        # Install an intentionally sparse file. The storage diagnostic writes
        # its first data block, exercising ext2 allocation outside group 0 as
        # well as the shared-memory transfer.
        TEMP_STORAGE_FIXTURE="$(mktemp \
          "${TMPDIR:-/tmp}/cubit-storage-sparse.XXXXXX")"
        truncate -s 8192 "$TEMP_STORAGE_FIXTURE"
        debugfs -w -R "rm config.dat" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R \
          "write $TEMP_STORAGE_FIXTURE config.dat" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install storage fixture" >&2
            exit 1
        fi
        # Create a directory that resolves normally, then corrupt the first
        # ext2 dirent's record-length field. The service must reject the page
        # without reading a variable-length name or advancing its cursor.
        debugfs -w -R "mkdir corrupt-dir" "$TEMP_DISK" >/dev/null 2>&1
        CORRUPT_DIR_BLOCK="$(debugfs -R "stat corrupt-dir" "$TEMP_DISK" 2>/dev/null | sed -n 's/.*(0):\([0-9][0-9]*\).*/\1/p' | head -n 1)"
        if [ -z "$CORRUPT_DIR_BLOCK" ]; then
            echo "headless: could not locate corrupt directory block" >&2
            exit 1
        fi
        if ! debugfs -w -R \
          "zap_block -o 4 -l 2 -p 0 $CORRUPT_DIR_BLOCK" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: could not corrupt directory fixture" >&2
            exit 1
        fi
    fi
        DISK_IMAGE="$TEMP_DISK"
fi

rm -f "$SERIAL_LOG" "$NET_PCAP"

sed -i 's/^set default=.*/set default=4/' "$GRUB_CFG"
# Focused runs often follow `make cubit_kernel` rather than `make iso`.
# Always stage that freshly built kernel and regenerate the stage-1 archive
# from the currently staged services before rebuilding the test ISO. Without
# this, a focused driver build can appear to pass while QEMU boots an older
# copy from initrd.img.
cp "$KERNEL_DIR/cubit_kernel" "$KERNEL_DIR/isodir/boot/cubit_kernel"
if ! make -C "$KERNEL_DIR" initrd >/dev/null; then
    echo "headless: failed to refresh stage-1 initrd" >&2
    exit 1
fi
if ! grub-mkrescue -o "$KERNEL_DIR/cubit_kernel.iso" "$KERNEL_DIR/isodir" >/dev/null 2>&1; then
    echo "headless: grub-mkrescue failed" >&2
    exit 1
fi
cp "$GRUB_BAK" "$GRUB_CFG"

VIDEO_ARGS="-device virtio-gpu-pci"
if [ "$TEST_NAME" = "virtio-vga-primary" ] ||
   [ "$TEST_NAME" = "ccl-workbench-virtio-vga" ] ||
   [ "$TEST_NAME" = "desktop-virtio-vga" ] ||
   [ "$TEST_NAME" = "desktop-doom" ]; then
    VIDEO_ARGS="-vga none -device virtio-vga,xres=1024,yres=768"
fi

echo "headless: running $TEST_NAME for ${TIMEOUT_SECONDS}s"

ACCEL_ARGS=()
if [ -n "$QEMU_ACCEL" ]; then
    ACCEL_ARGS=(-accel "$QEMU_ACCEL")
fi

AUDIO_ARGS=(-audiodev none,id=snd0)
if [ "$TEST_NAME" = "desktop-doom" ]; then
    TEMP_AUDIO="$(mktemp "${TMPDIR:-/tmp}/cubit-${TEST_NAME}-audio.XXXXXX.wav")"
    AUDIO_ARGS=(-audiodev "wav,id=snd0,path=$TEMP_AUDIO")
fi

MONITOR_ARGS=()
QMP_ARGS=()
if [ "$TEST_NAME" = "desktop-display" ] || [ "$TEST_NAME" = "files" ] ||
   [ "$TEST_NAME" = "desktop-doom" ]; then
    if ! command -v nc >/dev/null 2>&1; then
        echo "headless: desktop input regression requires nc" >&2
        exit 127
    fi

    MONITOR_SOCKET="${TMPDIR:-/tmp}/cubit-${TEST_NAME}-monitor-$$.sock"
    rm -f "$MONITOR_SOCKET"
    MONITOR_ARGS=(-monitor "unix:$MONITOR_SOCKET,server,nowait")
    if [ "$TEST_NAME" = "files" ]; then
        QMP_SOCKET="${TMPDIR:-/tmp}/cubit-${TEST_NAME}-qmp-$$.sock"
        rm -f "$QMP_SOCKET"
        QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    fi

    # Exercise the real QEMU i8042 -> IRQ -> ps2.drv -> authorized typed
    # publication -> desktop path. The synthetic input-stream publisher is a
    # complementary protocol stress test and cannot detect a stale/missing
    # driver publication capability.
    (
        ready=0
        for ((attempt = 0; attempt < 250; attempt++)); do
            if [ -S "$MONITOR_SOCKET" ] &&
               { [ "$TEST_NAME" != "files" ] || [ -S "$QMP_SOCKET" ]; } &&
               grep -F "ps2: consumer registered, entering event loop" \
                 "$SERIAL_LOG" >/dev/null 2>&1; then
                ready=1
                break
            fi
            sleep 0.1
        done

        if [ "$ready" -ne 1 ]; then
            echo "headless: PS/2 injector timed out waiting for desktop" >&2
            exit 1
        fi

        if [ "$TEST_NAME" = "desktop-doom" ]; then
            doom_ready=0
            for ((attempt = 0; attempt < 100; attempt++)); do
                if grep -F "I_InitGraphics: framebuffer" \
                    "$SERIAL_LOG" >/dev/null 2>&1; then
                    doom_ready=1
                    break
                fi
                sleep 0.1
            done
            if [ "$doom_ready" -ne 1 ]; then
                echo "headless: input injector timed out waiting for DOOM" >&2
                exit 1
            fi
            # Exercise the title menu/new-game route as well as attract-mode
            # rendering. Input is sent through the real PS/2 publication path.
            sleep 1
            {
                printf 'sendkey esc\n'
                sleep 0.5
                printf 'sendkey ret\n'
                sleep 0.5
                printf 'sendkey ret\n'
                sleep 0.5
                printf 'sendkey ret\n'
                sleep 2
                printf 'sendkey up 500\n'
                sleep 1
                printf 'sendkey ctrl 500\n'
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
        elif [ "$TEST_NAME" = "files" ]; then
            # Files is the first native client of the shared resizable table
            # header.  Exercise an actual captured drag through QEMU's i8042
            # device, then use F5 to prove that input delivery and the client
            # event loop remain live after release.
            files_ready=0
            for ((attempt = 0; attempt < 100; attempt++)); do
                if grep -F "files: first frame presented" \
                    "$SERIAL_LOG" >/dev/null 2>&1; then
                    files_ready=1
                    break
                fi
                sleep 0.1
            done
            if [ "$files_ready" -ne 1 ]; then
                echo "headless: input injector timed out waiting for Files" >&2
                exit 1
            fi
            {
                # QEMU's emulated relative PS/2 device applies its own host
                # scaling.  The correction lands on the Files divider at
                # client x=490 (the first column's initial trailing edge).
                printf 'mouse_move 510 96\n'
                printf 'mouse_move -77 2\n'
                sleep 0.4
                for _step in 1 2 3 4 5 6 7 8; do
                    printf 'mouse_move 10 0\n'
                    sleep 0.05
                done
                printf 'mouse_button 1\n'
                sleep 0.1
                for _step in 1 2 3 4 5 6 7 8; do
                    printf 'mouse_move 10 0\n'
                    sleep 0.05
                done
                printf 'mouse_button 0\n'
                sleep 0.2
                printf 'mouse_move 0 20\n'
                # Move from the table divider to the shared scrollbar's
                # increment arrow and click it.  This catches integration
                # failures that isolated scrollbar-state tests cannot: stale
                # control maps, incorrect client-coordinate translation, and
                # missing pressed-frame damage.
                printf 'mouse_move 272 409\n'
                sleep 0.2
                printf 'mouse_button 1\n'
                sleep 0.1
                printf 'mouse_button 0\n'
                sleep 0.2
                # The thumb is tall at the top of this short fixture. Grab it
                # below its leading edge and drag far enough to change the
                # first visible row while capture remains active.
                printf 'mouse_move 0 -380\n'
                sleep 0.2
                printf 'mouse_button 1\n'
                sleep 0.1
                for _step in 1 2 3 4 5 6 7 8; do
                    printf 'mouse_move 0 10\n'
                    sleep 0.05
                done
                printf 'mouse_button 0\n'
                sleep 0.2
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            {
                printf '{"execute":"qmp_capabilities"}\n'
                sleep 0.1
                printf '%s\n' '{"execute":"input-send-event","arguments":{"events":[{"type":"btn","data":{"down":true,"button":"wheel-down"}},{"type":"btn","data":{"down":false,"button":"wheel-down"}}]}}'
            } | nc -U -q 1 "$QMP_SOCKET" >/dev/null
            {
                # Return from the thumb at client (843,195) to the Refresh
                # button and exercise the complete retained-button lifecycle.
                # F5 below then proves the event loop remained live after the
                # mouse activation and its filesystem reload.
                # Large relative moves are split by QEMU into several PS/2
                # packets.  Pace them so the button edge cannot overtake the
                # final motion packets in the guest input stream.
                for _step in 1 2 3 4 5 6 7 8 9 10; do
                    printf 'mouse_move -79 -17\n'
                    sleep 0.05
                done
                printf 'mouse_move -3 2\n'
                sleep 0.5
                printf 'mouse_button 1\n'
                sleep 0.1
                printf 'mouse_button 0\n'
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.2
            printf 'sendkey f5\n' | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.2
            {
                # Finish by dragging the Files title bar. This exercises live
                # compositor movement while a client surface is attached and
                # catches held-button input or region-present regressions.
                printf 'mouse_move 0 -44\n'
                sleep 0.2
                printf 'mouse_button 1\n'
                sleep 0.1
                for _step in 1 2 3 4 5 6; do
                    printf 'mouse_move 10 4\n'
                    sleep 0.05
                done
                printf 'mouse_button 0\n'
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
        else
            {
                printf 'sendkey a\n'
                printf 'mouse_move 32 16\n'
                printf 'mouse_button 1\n'
                sleep 0.1
                printf 'mouse_button 0\n'
                sleep 1.2
                printf 'sendkey b\n'
                printf 'mouse_move 16 8\n'
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
        fi
    ) &
    INPUT_INJECTOR_PID=$!
fi

(
    cd "$KERNEL_DIR" || exit 1
    # shellcheck disable=SC2086
    "$TIMEOUT_BIN" "$TIMEOUT_SECONDS" "$QEMU_BIN" \
        "${ACCEL_ARGS[@]}" \
        -machine q35 \
        -cpu Broadwell \
        -smp 4 \
        -m 128M \
        -cdrom cubit_kernel.iso \
        -serial "file:$SERIAL_LOG" \
        -display none \
        "${MONITOR_ARGS[@]}" \
        "${QMP_ARGS[@]}" \
        -drive "file=$DISK_IMAGE,if=none,id=nvme0,format=raw" \
        -device nvme,serial=cubitnvme,drive=nvme0 \
        -device virtio-net-pci,netdev=net0 \
        $VIDEO_ARGS \
        -netdev user,id=net0 \
        -object "filter-dump,id=f0,netdev=net0,file=$NET_PCAP" \
        "${AUDIO_ARGS[@]}" \
        -device intel-hda \
        -device hda-output,audiodev=snd0 \
        -no-reboot
)
qemu_status=$?

injector_status=0
if [ -n "$INPUT_INJECTOR_PID" ]; then
    wait "$INPUT_INJECTOR_PID" || injector_status=$?
    INPUT_INJECTOR_PID=""
fi

if [ "$qemu_status" -ne 0 ] && [ "$qemu_status" -ne 124 ]; then
    echo "headless: QEMU exited with status $qemu_status" >&2
    exit 1
fi

if [ "$injector_status" -ne 0 ]; then
    echo "headless: QEMU input injector failed with status $injector_status" >&2
    exit 1
fi

if [ ! -s "$SERIAL_LOG" ]; then
    echo "headless: serial log was not created: $SERIAL_LOG" >&2
    exit 1
fi

case "$TEST_NAME" in
    boot-shell-nvme)
        required_markers="
devmgr: startup complete, entering service loop
procmgr: ready, entering receive loop
shell: cwd=@nvme:0/
ps2: consumer registered, entering event loop
"
        ;;
    async-ipc)
        required_markers="
ipctest-server: registered
ipctest-client: starting
TEST: PASS async-ipc
"
        ;;
    bench-ipc)
        required_markers="
bench-ipc-server: registered
bench-ipc-client: starting
BENCH: ipc sync
BENCH: ipc async
BENCH: PASS ipc
TRACE: summary begin
TRACE: event=syscall_enter
TRACE: event=schedule_run
TRACE: event=schedule_stop
TRACE: hist=syscall_tsc
TRACE: hist=run_tsc
TRACE: hist=ready_latency_tsc
TRACE: hist=lock_wait_tsc
TRACE: hist=lock_hold_tsc
TRACE: summary end
"
        ;;
    ccl-vm)
        required_markers="
ccl-vm: starting
ccl-test-host: registered
ccl-vm: bytecode PASS
ccl-vm: module PASS
ccl-vm: source PASS
ccl-test-host: import invoked
ccl-vm: import IPC PASS
clock: registered
clock: monotonic query
ccl-vm: clock source/link PASS
ccl-vm: clock IPC PASS
ccl-vm: scheduler PASS
ccl-vm: ownership PASS
ccl-vm: all tests passed
"
        ;;
    ccl-workbench)
        required_markers="
clock: registered
desktop: display backend=1 caps=1
ccl-workbench: native window ready
ccl-workbench: first frame presented
"
        ;;
    ccl-workbench-virtio-vga)
        required_markers="
clock: registered
desktop: display backend=3 caps=13
virtio-gpu: page flipping active
ccl-workbench: native window ready
ccl-workbench: first frame presented
"
        ;;
    capability-security)
        required_markers="
capability-test: getpid PASS
capability-test: no ambient filesystem PASS
capability-test: self process rights attenuated PASS
capability-test: no ambient keyboard PASS
capability-test: no ambient mouse PASS
capability-test: no ambient process management PASS
capability-test: ambient event publication denied PASS
capability-test: self mint denied PASS
capability-test: mint denial leaves slot empty PASS
capability-test: ambient spawn denied PASS
capability-test: all tests passed
"
        ;;
    storage-grants)
        required_markers="
GRANT-REFERENCE-CHECK: PASS
GRANT-RECLAMATION-CHECK: PASS
MALFORMED-DIRECTORY-CHECK: PASS
STORAGE-CHECK: PASS
"
        ;;
    audio-grants)
        required_markers="
mixer: acquired HDA period grant
"
        ;;
    desktop-display)
        required_markers="
display: gpu not primary, using linear-fb
desktop: display backend=1 caps=1
desktop: internal shell active
shell: cwd=@nvme:0/
"
        ;;
    input-stream)
        required_markers="
desktop: internal shell active
ps2: consumer registered, entering event loop
ccl-workbench: first frame presented
input-stress: publication and recovery PASS
desktop: stats ev=
source_gap=1
source_reject=0
"
        ;;
    devices)
        required_markers="
devmgr: startup complete, entering service loop
devices: starting read-only hardware inspector
devices: inventory snapshot ready
devices: native window ready
"
        ;;
    files)
        required_markers="
files: starting read-only filesystem browser
files: directory page protocol ready
files: native window ready
files: first frame presented
files: column resize complete first=
files: scrollbar scroll row=
files: scrollbar thumb drag row=
files: wheel scroll row=
files: refresh click activated
files: refresh input received
desktop: retained move path active
"
        ;;
    desktop-virtio-vga)
        required_markers="
display: backend virtio-gpu
display: gpu copy buffer attached
virtio-gpu: page flipping active
desktop: display backend=3 caps=13
desktop: internal shell active
shell: cwd=@nvme:0/
"
        ;;
    desktop-doom)
        required_markers="
display: backend virtio-gpu
desktop: internal shell active
I_InitGraphics: framebuffer
desktop: stats
display: stats
mixer: stats
mixer: HDA period IRQ active
"
        ;;
    virtio-gpu)
        required_markers="
devmgr: found virtio-gpu at PCI
devmgr: virtio-gpu setup complete
virtio-gpu: transport ready queues=2
virtio-gpu: scanout test frame presented
virtio-gpu: ready
shell: cwd=@nvme:0/
"
        ;;
    virtio-vga-primary)
        required_markers="
devmgr: found virtio-gpu at PCI
devmgr: virtio-gpu setup complete
virtio-gpu: scanout0 1024x768 enabled=1
virtio-gpu: scanout test frame presented
virtio-gpu: ready
display: backend virtio-gpu
display: gpu scanout cleared
display: gpu copy buffer attached
display: buffer attached
shell: display buffer attached
shell: cwd=@nvme:0/
"
        ;;
esac

missing=0
while IFS= read -r marker; do
    [ -z "$marker" ] && continue
    if ! grep -F "$marker" "$SERIAL_LOG" >/dev/null 2>&1; then
        echo "headless: missing serial marker: $marker" >&2
        missing=1
    fi
done <<EOF
$required_markers
EOF

if [ "$missing" -ne 0 ]; then
    echo "headless: serial log: $SERIAL_LOG" >&2
    exit 1
fi

if [ "$TEST_NAME" = "input-stream" ]; then
    # Workbench used to submit a complete 900x400 surface for every plain
    # pointer report. The stress source publishes 128 motion reports across
    # its rich editor; only semantic hover transitions and actual edits should
    # now submit application surface damage.
    INPUT_STATS_LINE="$(grep -F 'source_gap=1' "$SERIAL_LOG" | tail -n 1)"
    INPUT_PRESENT_REQUESTS="$(printf '%s\n' "$INPUT_STATS_LINE" | \
      sed -n 's/.*present_req=\([0-9][0-9]*\).*/\1/p')"
    INPUT_REQUESTS="$(printf '%s\n' "$INPUT_STATS_LINE" | \
      sed -n 's/.*input_req=\([0-9][0-9]*\).*/\1/p')"
    if [ -z "$INPUT_PRESENT_REQUESTS" ] ||
       [ "$INPUT_PRESENT_REQUESTS" -gt 20 ]; then
        echo "headless: excessive Workbench surface presents during input stress: ${INPUT_PRESENT_REQUESTS:-missing}" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    if [ -z "$INPUT_REQUESTS" ] || [ "$INPUT_REQUESTS" -gt 160 ]; then
        echo "headless: excessive Workbench input IPC during stress: ${INPUT_REQUESTS:-missing}" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "desktop-display" ]; then
    DESKTOP_INPUT_STATS="$(grep -F 'desktop: stats ' "$SERIAL_LOG")"
    KEYBOARD_REPORTS="$(printf '%s\n' "$DESKTOP_INPUT_STATS" | awk '
      { for (i = 1; i <= NF; i++) if ($i ~ /^key=/) {
          split($i, value, "="); total += value[2]
        }
      } END { print total + 0 }')"
    POINTER_REPORTS="$(printf '%s\n' "$DESKTOP_INPUT_STATS" | awk '
      { for (i = 1; i <= NF; i++) if ($i ~ /^mouse=/) {
          split($i, value, "="); total += value[2]
        }
      } END { print total + 0 }')"
    BUTTON_TRANSITIONS="$(printf '%s\n' "$DESKTOP_INPUT_STATS" | awk '
      { for (i = 1; i <= NF; i++) if ($i ~ /^button=/) {
          split($i, value, "="); total += value[2]
        }
      } END { print total + 0 }')"
    LAST_POINTER_STATS="$(printf '%s\n' "$DESKTOP_INPUT_STATS" | \
      grep -E 'mouse=[1-9][0-9]*' | tail -n 1)"
    CURSOR_X="$(printf '%s\n' "$LAST_POINTER_STATS" | \
      sed -n 's/.*cursor_x=\([0-9][0-9]*\).*/\1/p')"
    CURSOR_Y="$(printf '%s\n' "$LAST_POINTER_STATS" | \
      sed -n 's/.*cursor_y=\([0-9][0-9]*\).*/\1/p')"

    if [ "$KEYBOARD_REPORTS" -lt 2 ] ||
       [ "$POINTER_REPORTS" -lt 2 ] ||
       [ "$BUTTON_TRANSITIONS" -lt 2 ] ||
       { [ "$CURSOR_X" = "80" ] && [ "$CURSOR_Y" = "80" ]; } ||
       grep -E 'source_reject=[1-9][0-9]*' "$SERIAL_LOG" >/dev/null 2>&1;
    then
        echo "headless: QEMU PS/2 input behavior mismatch: key=$KEYBOARD_REPORTS mouse=$POINTER_REPORTS button=$BUTTON_TRANSITIONS cursor=${CURSOR_X:-missing},${CURSOR_Y:-missing}" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "files" ]; then
    if ! grep -E 'desktop: ptr hit-down [0-9]+ [0-9]+ 1$' \
        "$SERIAL_LOG" >/dev/null; then
        echo "headless: Files title-bar drag was not observed" >&2
        exit 1
    fi
    if ! grep -E 'desktop: ptr drag-up [0-9]+ [0-9]+ [0-9]+$' \
        "$SERIAL_LOG" >/dev/null; then
        echo "headless: Files title-bar drag did not complete" >&2
        exit 1
    fi
fi

if { [ "$TEST_NAME" = "desktop-virtio-vga" ] ||
     [ "$TEST_NAME" = "ccl-workbench-virtio-vga" ] ||
     [ "$TEST_NAME" = "desktop-doom" ] ||
     [ "$TEST_NAME" = "virtio-vga-primary" ]; } &&
   grep -F "CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: create failed" \
     "$SERIAL_LOG" >/dev/null 2>&1; then
    echo "headless: display attempted an unsafe received-page re-grant" >&2
    echo "headless: serial log: $SERIAL_LOG" >&2
    exit 1
fi

FAULT_SIGNATURE='panic|assert|double fault|triple fault|general protection|machine check exception|^EXCEPTION:|deadlock|TEST: FAIL'
if grep -Ei "$FAULT_SIGNATURE" "$SERIAL_LOG" >/dev/null 2>&1; then
    echo "headless: fault signature found in serial log: $SERIAL_LOG" >&2
    grep -Ein "$FAULT_SIGNATURE" "$SERIAL_LOG" >&2
    exit 1
fi

HEADLESS_TEST_FAILED=0
echo "headless: PASS $TEST_NAME"
echo "headless: serial log: $SERIAL_LOG"
