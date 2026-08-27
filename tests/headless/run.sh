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
SERIAL_LOG=""
NET_PCAP=""
TEMP_DISK=""

usage() {
    cat <<'EOF'
Usage: tests/headless/run.sh [options]

Options:
  --build              Run make world before booting QEMU
  --test NAME          Test to run: boot-shell-nvme, async-ipc, bench-ipc, ccl-vm, desktop-display, security-authority, desktop-doom, desktop-virtio-vga, virtio-gpu, or virtio-vga-primary
  --timeout SECONDS    QEMU runtime before timeout is treated as success
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
    boot-shell-nvme|async-ipc|bench-ipc|ccl-vm|desktop-display|security-authority|desktop-doom|desktop-virtio-vga|virtio-gpu|virtio-vga-primary)
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
    cp "$GRUB_BAK" "$GRUB_CFG"
    rm -f "$GRUB_BAK"
    if [ -n "$TEMP_DISK" ]; then
        rm -f "$TEMP_DISK"
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

if [ ! -f "$KERNEL_DIR/nvme_disk.img" ]; then
    echo "headless: missing kernel/nvme_disk.img; run make -C kernel world or pass --build" >&2
    exit 1
fi

DISK_IMAGE="$KERNEL_DIR/nvme_disk.img"
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
    desktop-display|desktop-virtio-vga)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-desktop-display.conf"
        ;;
    security-authority)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-security-authority.conf"
        ;;
    desktop-doom)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-doom-desktop.conf"
        ;;
    virtio-gpu|virtio-vga-primary)
        ;;
esac

if [ -n "$INIT_PROFILE" ]; then
    TEMP_DISK="$(mktemp "${TMPDIR:-/tmp}/cubit-${TEST_NAME}-disk.XXXXXX.img")"
    cp "$KERNEL_DIR/nvme_disk.img" "$TEMP_DISK"
    debugfs -w -R "rm init.conf" "$TEMP_DISK" >/dev/null 2>&1
    if ! debugfs -w -R "write $INIT_PROFILE init.conf" "$TEMP_DISK" >/dev/null 2>&1; then
        echo "headless: failed to install $TEST_NAME init.conf" >&2
        exit 1
    fi
    DISK_IMAGE="$TEMP_DISK"
fi

rm -f "$SERIAL_LOG" "$NET_PCAP"

sed -i 's/^set default=.*/set default=4/' "$GRUB_CFG"
if ! grub-mkrescue -o "$KERNEL_DIR/cubit_kernel.iso" "$KERNEL_DIR/isodir" >/dev/null 2>&1; then
    echo "headless: grub-mkrescue failed" >&2
    exit 1
fi
cp "$GRUB_BAK" "$GRUB_CFG"

VIDEO_ARGS="-device virtio-gpu-pci"
if [ "$TEST_NAME" = "virtio-vga-primary" ] ||
   [ "$TEST_NAME" = "desktop-virtio-vga" ] ||
   [ "$TEST_NAME" = "desktop-doom" ]; then
    VIDEO_ARGS="-vga none -device virtio-vga,xres=1024,yres=768"
fi

echo "headless: running $TEST_NAME for ${TIMEOUT_SECONDS}s"

(
    cd "$KERNEL_DIR" || exit 1
    # shellcheck disable=SC2086
    "$TIMEOUT_BIN" "$TIMEOUT_SECONDS" "$QEMU_BIN" \
        -machine q35 \
        -cpu Broadwell \
        -smp 4 \
        -m 128M \
        -cdrom cubit_kernel.iso \
        -serial "file:$SERIAL_LOG" \
        -display none \
        -drive "file=$DISK_IMAGE,if=none,id=nvme0,format=raw" \
        -device nvme,serial=cubitnvme,drive=nvme0 \
        -device virtio-net-pci,netdev=net0 \
        $VIDEO_ARGS \
        -netdev user,id=net0 \
        -object "filter-dump,id=f0,netdev=net0,file=$NET_PCAP" \
        -audiodev none,id=snd0 \
        -device intel-hda \
        -device hda-output,audiodev=snd0 \
        -no-reboot
)
qemu_status=$?

if [ "$qemu_status" -ne 0 ] && [ "$qemu_status" -ne 124 ]; then
    echo "headless: QEMU exited with status $qemu_status" >&2
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
ccl-vm: scheduler PASS
ccl-vm: ownership PASS
ccl-vm: all tests passed
"
        ;;
    desktop-display)
        required_markers="
display: gpu not primary, using linear-fb
desktop: display backend=1 caps=3
desktop: internal shell active
shell: cwd=@nvme:0/
"
        ;;
    security-authority)
        required_markers="
desktop: internal shell active
security-center: starting
security-center: authority provenance ready
"
        ;;
    desktop-virtio-vga)
        required_markers="
display: backend virtio-gpu
display: direct gpu backbuffer mapped
desktop: direct gpu backbuffer
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
display: gpu buffer attached
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

if grep -Ei 'panic|assert|triple fault|general protection|deadlock|TEST: FAIL' "$SERIAL_LOG" >/dev/null 2>&1; then
    echo "headless: fault signature found in serial log: $SERIAL_LOG" >&2
    grep -Ein 'panic|assert|triple fault|general protection|deadlock|TEST: FAIL' "$SERIAL_LOG" >&2
    exit 1
fi

HEADLESS_TEST_FAILED=0
echo "headless: PASS $TEST_NAME"
echo "headless: serial log: $SERIAL_LOG"
