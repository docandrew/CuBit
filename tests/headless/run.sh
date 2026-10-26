#!/usr/bin/env bash
set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
KERNEL_DIR="$ROOT_DIR/kernel"
# Files this script writes into test disks (debugfs) carry the build's
# timestamp, like the images kernel/Makefile builds, not the Nix shell's 1980.
SOURCE_DATE_EPOCH="$(make -s --no-print-directory -C "$KERNEL_DIR" print-build-epoch)" || exit 1
export SOURCE_DATE_EPOCH
# Native fixtures built by the test scripts use the same GNAT 15.3
# configuration as make (docs/ccl-build.md).
CUBIT_NATIVE_GPRBUILD_CONFIG="$(make -s --no-print-directory -C "$KERNEL_DIR" print-native-gprbuild-config)" || exit 1
export CUBIT_NATIVE_GPRBUILD_CONFIG
QEMU_BIN="${QEMU_BIN:-qemu-system-x86_64}"
# The default guest is an Intel Broadwell under TCG and KVM alike. Without an
# explicit vendor, KVM reports the host's vendor beside Broadwell's family 6,
# a CPU that does not exist: on an AMD host, getrandom's RDRAND backend then
# (correctly, for real AMD before family 17h) refuses RDRAND, and every Rust
# std service that draws randomness exits. The boot checks the vendor below.
QEMU_CPU_VENDOR=GenuineIntel
QEMU_CPU_MODEL="${QEMU_CPU_MODEL:-Broadwell,vendor=$QEMU_CPU_VENDOR}"
TIMEOUT_BIN="${TIMEOUT_BIN:-timeout}"

TEST_NAME="boot-shell-nvme"
# Per-test QEMU runtime when --timeout is not given. Desktop tests drive a
# full desktop with injected input; the others finish well inside the default.
DEFAULT_TIMEOUT_SECONDS=25
DESKTOP_TIMEOUT_SECONDS=180
NETWORK_AUTHORITY_TIMEOUT_SECONDS=100
# net-link and net-multi: boot, DHCP, and each pulled cable (tests/net-link).
NET_LINK_TIMEOUT_SECONDS=150
BENCH_LATENCY_TIMEOUT_SECONDS=300
TIMEOUT_SECONDS=""
BUILD_WORLD=0
KEEP_LOGS=0
CHECK_EXT2=0
QEMU_ACCEL=""
QEMU_CPUS=4
# Guest memory; cases that need more (servo) raise the default.
QEMU_MEMORY="${QEMU_MEMORY:-}"
QEMU_CPUS_EXPLICIT=0
QEMU_DISPLAY=none
INPUT_BACKEND=linear
VCPU_CPUS=""
BENCH_LOAD=0
LOAD_WORKERS=1
TEMP_LOAD_PROFILE=""
SERIAL_LOG=""
NET_PCAP=""
BASE_DISK=""
TEMP_DISK=""
TURSO_REVISION=1
TURSO_EXPORT=""
CONFIG_EXPORT=""
CONFIG_STORAGE_TEST=0
TEMP_AUDIO=""
TEMP_SECOND_NVME=""
TEMP_OPTICAL_TREE=""
TEMP_STORAGE_FIXTURE=""
# --test ufs: the GPT disk behind QEMU's UFS logical unit, and its devices.
TEMP_UFS_DISK=""
UFS_ARGS=()
MONITOR_SOCKET=""
QMP_SOCKET=""
# This run's QEMU UNIX socket of one KIND (qmp, monitor, vnc): unique by
# PID and short, since sun_path holds at most 107 bytes and TMPDIR may be
# a long nix-shell directory, so the test name stays out of it.
qemu_socket() {
    printf '%s/cubit-%s-%s.sock' "${TMPDIR:-/tmp}" "$1" "$$"
}
INPUT_INJECTOR_PID=""

usage() {
    cat <<'EOF'
Usage: tests/headless/run.sh [options]

Mesa native tests: --test softpipe, opengl, buffer, mesa-window, mesa-sync, mesa-native-instance or mesa-log-bridge (default RAM 512 MiB).

Options:
  --build              Run make world before booting QEMU
  --test NAME          Test to run: boot-shell-nvme, async-ipc, bench-ipc, ccl-vm, ccl-workbench, ccl-workbench-virtio-vga, ccl-workspace, ccl-console, logs, ccl-remote, capability-security, network-authority, bench-net, bench-fs, threads, futex, clock, avx, rust-std, libc, processes, control-events, binutils, gcc, gnat, servo, bench-spread, timesync, tls-probe, tls-service, wget-https, storage-grants, audio-grants, desktop-display, desktop-protocol, display-grants, display-grants-virtio-vga, input-stream, devices, managed-ui, files, text, calculator, lexicon, dictionary, clipboard, key-repeat, desktop-doom, desktop-virtio-vga, desktop-vm, virtio-gpu, virtio-vga-primary, launch-failure, fs-coherence-CASE, startup-health, firmware-snapshot, or boot-timeline (--accel kvm --timeout 60)
  --timeout SECONDS    QEMU runtime before timeout is treated as success
                       (default 25; 180 for desktop tests, see DESKTOP_TIMEOUT_SECONDS)
  --accel NAME         QEMU accelerator (for example: tcg,thread=multi)
  --cpus COUNT         Virtual CPUs, 1..4 (default: 4)
  --display MODE       QEMU frontend (default: none; gtk,zoom-to-fit=on for UI regression)
  --input-backend MODE bench-input: linear or virtio (default: linear)
  --vcpu-cpus LIST     Pin vCPU 0,1,... to distinct Linux CPUs (e.g. 2,3,4,5; KVM only)
  --load               Add one busy peer (bench-ipc / bench-audio / bench-input)
  --load-workers COUNT Add 1..4 busy peers (bench-input only)
  --disk PATH          Base ext2 disk image (default: kernel/nvme_disk.img)
  --serial PATH        Serial log path (default: /tmp/cubit-headless-*.log)
  --pcap PATH          Packet capture path (default: /tmp/cubit-headless-*.pcap)
  --keep-logs          Leave logs (and the guest's disk) in place after a run
  --turso-revision N  turso-native: expected saved revision, 1 (seed) or 2 (fresh boot)
  --turso-export DIR  turso-native: save validated disk and SQLite in a NEW directory
  --config-export DIR config-objects/reopen/benchmark: save validated disk and SQLite/WAL in a NEW directory
  --check-ext2         bench-storage: verify Linux/CuBit/Linux inode reuse, data and e2fsck
  -h, --help           Show this help

The suite boots the NVMe profile headlessly and checks serial output for
stable pass markers.
Performance fixtures: bench-ipc, bench-audio, bench-storage, bench-input, bench-scheduler.
bench-fs: tests/fs-bench (build tests/fs-bench/build-cubit.sh first).
fs-coherence-CASE: one cross-process coherence case, CASE from
  tests/fs-coherence/cases.txt (build tests/fs-coherence/build-cubit.sh first).
bench-latency: tests/sched-latency (build tests/sched-latency/build-cubit.sh
  and bench-ipc-server first); defaults to 300 s. QEMU quits at its done marker.
Logging fixture: log-authority (build logstore procmgr clock log-check first).
Observability: log-fields (build logstore log-fields-check first); metrics
  (build metricsvc metrics-check; needs procmgr metrics tag issuance).
Render denial: render-launch-policy (build procmgr devmgr devices render-launch-policy first).
Rust fixture: rust-native (build rust-probe ccl-test-host clock first).
Native Turso: turso-native-std, turso-native, config-storage; see tests/config-turso/native/README.md.
Public typed Config: config-objects, config-objects-reopen, config-objects-benchmark;
  build tests/config-object-client/native-app/build.sh first.
QEMU_CPU_MODEL overrides Broadwell,vendor=GenuineIntel; use host with KVM for faithful CPU identification.
Multi-output discovery: virtio-gpu-multi-output (QEMU 11.1 per-head modes;
build virtio-gpu first). This does not yet test a multi-monitor desktop.
Native catalog IPC: display-discovery-multi-output (build display, virtio-gpu,
and display-check first). Walks three GPU heads plus the selected boot output.
Firmware-only catalog: display-discovery-boot-only (same builds, no virtio GPU).
Two-output presentation: display-dual-output (build devmgr, virtio-gpu, display,
and display-check first). Native IPC plus pixel checks on both QEMU heads.
Native extended Desktop: desktop-dual-output (build desktop, display, devmgr,
virtio-gpu and ccl-workbench first). Checks drag, maximize, taskbar and cleanup on both heads.
The threads test defaults to one vCPU (both instances must share a CPU).
bench-spread: four CPU-bound instances; compare WORK_STEALING=0/1 kernels.
network-authority defaults to 100 s: it includes an intentional 30-second
accept deadline and a seven-second backlog expiry wait, plus traffic checks.
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
        --display)
            if [ "$#" -lt 2 ] || [ -z "$2" ]; then
                echo "headless: --display requires a value" >&2
                exit 2
            fi
            QEMU_DISPLAY="$2"
            shift 2
            ;;
        --cpus)
            if [ "$#" -lt 2 ] || ! [[ "$2" =~ ^[1-4]$ ]]; then
                echo "headless: --cpus requires 1..4" >&2
                exit 2
            fi
            QEMU_CPUS="$2"
            QEMU_CPUS_EXPLICIT=1
            shift 2
            ;;
        --input-backend)
            if [ "$#" -lt 2 ] || ! [[ "$2" =~ ^(linear|virtio)$ ]]; then
                echo "headless: --input-backend requires linear or virtio" >&2
                exit 2
            fi
            INPUT_BACKEND="$2"
            shift 2
            ;;
        --load)
            BENCH_LOAD=1
            shift
            ;;
        --load-workers)
            if [ "$#" -lt 2 ]; then
                echo "headless: --load-workers requires a count" >&2
                exit 2
            fi
            case "$2" in
                1|2|3|4) LOAD_WORKERS="$2"; BENCH_LOAD=1 ;;
                *) echo "headless: --load-workers must be 1..4" >&2; exit 2 ;;
            esac
            shift 2
            ;;
        --vcpu-cpus)
            if [ "$#" -lt 2 ] || [ -z "$2" ]; then
                echo "headless: --vcpu-cpus requires a comma-separated CPU mapping" >&2
                exit 2
            fi
            VCPU_CPUS="$2"
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
        --turso-revision)
            case "${2:-}" in
                1|2) TURSO_REVISION="$2" ;;
                *) echo "headless: --turso-revision requires 1 or 2" >&2; exit 2 ;;
            esac
            shift 2
            ;;
        --turso-export)
            if [ "$#" -lt 2 ] || [ -z "$2" ] || [ -e "$2" ] || [ -L "$2" ]; then
                echo "headless: --turso-export requires a new directory path" >&2
                exit 2
            fi
            TURSO_EXPORT="$2"
            shift 2
            ;;
        --check-ext2)
            CHECK_EXT2=1
            shift
            ;;
        --config-export)
            if [ "$#" -lt 2 ] || [ -z "$2" ] || [ -e "$2" ] || [ -L "$2" ]; then
                echo "headless: --config-export requires a new directory path" >&2
                exit 2
            fi
            CONFIG_EXPORT="$2"
            shift 2
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

if [ -z "$TIMEOUT_SECONDS" ]; then
    case "$TEST_NAME" in
        desktop-display|desktop-protocol|files|text|calculator|lexicon|dictionary|clipboard|key-repeat|input-stream|launch-failure|startup-health)
            TIMEOUT_SECONDS=$DESKTOP_TIMEOUT_SECONDS ;;
        network-authority)
            TIMEOUT_SECONDS=$NETWORK_AUTHORITY_TIMEOUT_SECONDS ;;
        net-link|net-multi)
            TIMEOUT_SECONDS=$NET_LINK_TIMEOUT_SECONDS ;;
        bench-latency)
            TIMEOUT_SECONDS=$BENCH_LATENCY_TIMEOUT_SECONDS ;;
        *)
            TIMEOUT_SECONDS=$DEFAULT_TIMEOUT_SECONDS ;;
    esac
fi

case "$TIMEOUT_SECONDS" in
    ''|*[!0-9]*)
        echo "headless: --timeout must be an integer number of seconds" >&2
        exit 2
        ;;
esac

MESA_WINDOW_ANIMATION_WAIT_SECONDS="${MESA_WINDOW_ANIMATION_WAIT_SECONDS:-15}"
if [ "$TEST_NAME" = mesa-window ] &&
   { ! [[ "$MESA_WINDOW_ANIMATION_WAIT_SECONDS" =~ ^[1-9][0-9]{0,3}$ ]] ||
     [ "$MESA_WINDOW_ANIMATION_WAIT_SECONDS" -gt 3600 ]; }; then
    echo "headless: MESA_WINDOW_ANIMATION_WAIT_SECONDS must be 1..3600" >&2
    exit 2
fi

if [ "$TEST_NAME" != "turso-native" ] &&
   { [ "$TURSO_REVISION" != 1 ] || [ -n "$TURSO_EXPORT" ]; }; then
    echo "headless: Turso revision/export options require --test turso-native" >&2
    exit 2
fi

if [ -n "$CONFIG_EXPORT" ] && [ "$TEST_NAME" != config-objects ] && [ "$TEST_NAME" != config-objects-reopen ] && [ "$TEST_NAME" != config-objects-benchmark ]; then
    echo "headless: --config-export requires config-objects, config-objects-reopen or config-objects-benchmark" >&2
    exit 2
fi

case "$TEST_NAME" in
    softpipe|opengl|buffer|mesa-window|mesa-sync|gpu-viewer|render-launch-policy|mesa-native-instance|mesa-log-bridge)
        ;;
    config-objects|config-objects-reopen|config-objects-benchmark|config-storage)
        CONFIG_STORAGE_TEST=1
        ;;
    grant-forward|grant-forward-intermediary-exit|grant-forward-owner-exit|grant-forward-desktop|config-tree|config-inspection|log-authority|log-fields|metrics|rust-native|turso-native-std|turso-native|virtio-gpu-multi-output|display-discovery-multi-output|display-discovery-boot-only|desktop-dual-output)
        ;;
    boot-shell-nvme|async-ipc|bench-ipc|bench-audio|bench-storage|bench-input|bench-scheduler|ccl-vm|ccl-workbench|ccl-workbench-virtio-vga|ccl-workspace|ccl-console|lexicon|dictionary|logs|ccl-remote|capability-security|network-authority|bench-net|bench-fs|threads|futex|clock|avx|rust-std|libc|processes|control-events|binutils|gcc|gnat|servo|bench-spread|timesync|tls-probe|tls-service|wget-https|storage-grants|audio-grants|desktop-display|desktop-protocol|display-grants|display-grants-virtio-vga|display-dual-output|input-stream|devices|managed-ui|files|text|calculator|clipboard|key-repeat|desktop-doom|desktop-virtio-vga|desktop-vm|virtio-gpu|virtio-vga-primary|bench-latency|boot-timeline|devmem-denials|process-kill|oom-reclaim|scanout-repair|launch-failure|fs-coherence-*|startup-health|generic-launch|ufs|ufs-write|net-link|net-multi|firmware-snapshot|multi-instance|program-parameters|usb-hotplug)
        ;;
    *)
        echo "headless: unknown test: $TEST_NAME" >&2
        exit 2
        ;;
esac

PIN_ARGS=()
if [ -n "$VCPU_CPUS" ]; then
    if [ "$QEMU_ACCEL" != "kvm" ]; then
        echo "headless: --vcpu-cpus requires explicit --accel kvm" >&2
        exit 2
    fi
    PIN_ARGS=(python3 "$ROOT_DIR/tests/headless/qemu_affinity.py"
              --vcpu-cpus "$VCPU_CPUS" --count "$QEMU_CPUS")
    "${PIN_ARGS[@]}" --validate-only || exit 2
    PIN_ARGS+=(--)
fi

if [ "$INPUT_BACKEND" != linear ] && [ "$TEST_NAME" != bench-input ]; then
    echo "headless: --input-backend requires bench-input" >&2
    exit 2
fi
if [ "$BENCH_LOAD" = 1 ] && [ "$TEST_NAME" != "bench-ipc" ] && [ "$TEST_NAME" != "bench-audio" ] && [ "$TEST_NAME" != "bench-input" ]; then
    echo "headless: --load only supports bench-ipc, bench-audio and bench-input" >&2
    exit 2
fi
if [ "$LOAD_WORKERS" -gt 1 ] && [ "$TEST_NAME" != "bench-input" ]; then
    echo "headless: multiple busy peers currently require bench-input" >&2
    exit 2
fi

if [ "$CHECK_EXT2" -eq 1 ] && [ "$TEST_NAME" != "bench-storage" ]; then
    echo "headless: --check-ext2 requires bench-storage (other fixtures may intentionally corrupt metadata)" >&2
    exit 2
fi

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
NETWORK_PEER_PID=""
SERVO_HTTPS_PID=""
CLOCK_OBSERVER_PID=""
TEMP_CONFIG_REJECTED=""
GRUB_BAK="$(mktemp "${TMPDIR:-/tmp}/cubit-grub.XXXXXX")"
cp "$GRUB_CFG" "$GRUB_BAK"

cleanup() {
    if [ -n "$TEMP_CONFIG_REJECTED" ]; then
        rm -f "$TEMP_CONFIG_REJECTED"
    fi
    if [ -n "$CLOCK_OBSERVER_PID" ]; then
        kill "$CLOCK_OBSERVER_PID" >/dev/null 2>&1 || true
        wait "$CLOCK_OBSERVER_PID" >/dev/null 2>&1 || true
    fi
    if [ -n "$TEMP_LOAD_PROFILE" ]; then
        rm -f "$TEMP_LOAD_PROFILE"
    fi
    if [ -n "$NETWORK_PEER_PID" ]; then
        kill "$NETWORK_PEER_PID" >/dev/null 2>&1 || true
        wait "$NETWORK_PEER_PID" >/dev/null 2>&1 || true
    fi
    if [ -n "$SERVO_HTTPS_PID" ]; then
        kill "$SERVO_HTTPS_PID" >/dev/null 2>&1 || true
        wait "$SERVO_HTTPS_PID" >/dev/null 2>&1 || true
    fi
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
        # --keep-logs keeps the guest's disk too, for inspecting what it wrote.
        if [ "$KEEP_LOGS" -eq 1 ] && [ -n "$SERIAL_LOG" ]; then
            mv -f "$TEMP_DISK" "${SERIAL_LOG%.*}-disk.img" 2>/dev/null || rm -f "$TEMP_DISK"
        else
            rm -f "$TEMP_DISK"
        fi
    fi
    if [ -n "$TEMP_STORAGE_FIXTURE" ]; then
        rm -f "$TEMP_STORAGE_FIXTURE"
    fi
    if [ -n "$TEMP_UFS_DISK" ] && [ "$TEMP_UFS_DISK" != "${CUBIT_INSTALL_DISK:-}" ]; then
        rm -f "$TEMP_UFS_DISK"
    fi
    if [ -n "$TEMP_SECOND_NVME" ]; then
        rm -f "$TEMP_SECOND_NVME"
    fi
    if [ -n "$TEMP_OPTICAL_TREE" ]; then
        rm -rf "$TEMP_OPTICAL_TREE"
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

# The gcc toolchain as installed (--prefix=/toolchain), read-only to the
# compilers, on $TEMP_DISK: the gcc port's tree (driver, cc1, specs, libgcc,
# the libc's start files, archives and headers) and binutils' as and ld, in
# one debugfs run over a command list (gcc test, console gcc demo).
install_gcc_toolchain() {
    local tree="$ROOT_DIR/userspace/ports/gcc/build/toolchain"
    local commands="$TEMP_DISK.gcc-toolchain"
    {
        echo "mkdir toolchain"
        (cd "$tree" && find . -mindepth 1 -type d | sort | sed "s|^\./|mkdir toolchain/|")
        echo "mkdir toolchain/x86_64-linux-musl"
        echo "mkdir toolchain/x86_64-linux-musl/bin"
        (cd "$tree" && find . -type f | sort |
           sed "s|^\./\(.*\)|rm toolchain/\1\nwrite $tree/\1 toolchain/\1|")
        for tool in as ld; do
            echo "rm toolchain/x86_64-linux-musl/bin/$tool"
            echo "write $KERNEL_DIR/isodir/boot/$tool.app toolchain/x86_64-linux-musl/bin/$tool"
        done
    } > "$commands"
    debugfs -w -f "$commands" "$TEMP_DISK" >/dev/null 2>&1 || return 1
    rm -f "$commands"
    local check
    for check in toolchain/bin/gcc toolchain/x86_64-linux-musl/bin/ld toolchain/lib/libc.a; do
        if ! debugfs -R "stat $check" "$TEMP_DISK" 2>/dev/null | grep -q "Size:"; then
            echo "headless: $check was not installed" >&2
            return 1
        fi
    done
}

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
    rust-native)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-rust-native.ccl"
        ;;
    config-storage)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-config-storage.ccl"
        ;;
    config-objects)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-config-objects.ccl"
        ;;
    config-objects-reopen)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-config-objects-reopen.ccl"
        ;;
    config-objects-benchmark)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-config-objects-benchmark.ccl"
        ;;
    turso-native-std|turso-native)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-turso-native.ccl"
        ;;
    config-inspection)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-config-inspection.ccl"
        ;;
    config-tree)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-config-tree.ccl"
        ;;
    log-authority)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-log-authority.ccl"
        ;;
    log-fields|metrics)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-$TEST_NAME.ccl"
        ;;
    grant-forward|grant-forward-intermediary-exit|grant-forward-owner-exit|grant-forward-desktop|gpu-viewer)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-$TEST_NAME.ccl"
        ;;
    async-ipc)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-async-ipc.ccl"
        ;;
    bench-ipc)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-bench-ipc.ccl"
        ;;
    bench-input)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-bench-input.ccl"
        ;;
    bench-scheduler)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-bench-scheduler.ccl"
        ;;
    bench-storage)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-bench-storage.ccl"
        ;;
    ccl-vm)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ccl-vm.ccl"
        ;;
    ccl-workbench|ccl-workbench-virtio-vga|ccl-workspace)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ccl-workbench.ccl"
        ;;
    ccl-console)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ccl-console.ccl"
        if [ "${CCL_CONSOLE_DEMO:-}" = install ]; then
            # SYS-001: system.disks and system.install typed at the console,
            # answered by installer.svc, onto QEMU's UFS model (4 KiB blocks,
            # as the IdeaPad's disk). The 1 GiB disk starts full of a
            # non-zero pattern, so every structure the install relies on
            # being zero must have been zeroed. The payload (esp/, root/) is
            # @nvme:0/install. The host then checks the disk image.
            # CUBIT_INSTALL_PAYLOAD names another payload directory (esp/,
            # root/, INDEX) and CUBIT_INSTALL_DISK keeps the disk at that
            # path (tests/install-reboot boots it afterwards).
            INIT_PROFILE="$ROOT_DIR/tests/headless/init-ccl-console-install.ccl"
            if [ -n "${CUBIT_INSTALL_DISK:-}" ]; then
                TEMP_UFS_DISK="$CUBIT_INSTALL_DISK"
            else
                TEMP_UFS_DISK="$(mktemp "${TMPDIR:-/tmp}/cubit-ufs-disk.XXXXXX.img")"
            fi
            python3 -I -c 'import sys
chunk = bytes([0xA5]) * (1 << 20)
with open(sys.argv[1], "wb") as f:
    for _ in range(1024):
        f.write(chunk)' "$TEMP_UFS_DISK"
            UFS_ARGS=(-drive "file=$TEMP_UFS_DISK,if=none,id=ufs0,format=raw"
                      -device ufs,serial=cubitufs,id=ufs
                      -device ufs-lu,drive=ufs0,bus=ufs,lun=0)
        fi
        # The gcc demo runs cc1, a 45 MB image procmgr reads whole.
        if [ "${CCL_CONSOLE_DEMO:-}" = gcc ]; then QEMU_MEMORY="${QEMU_MEMORY:-1G}"; fi
        ;;
    lexicon)
        # The CCL console over lexicon.svc (docs/lexicon.md); the service
        # holds the 22 MB lexicon image.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-lexicon.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
        ;;
    dictionary)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-dictionary.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
        ;;
    logs)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-logs.ccl"
        ;;
    process-kill)
        # A process the kernel kills for a fault is recorded, logged and
        # shown on the Desktop bootstrap screen.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-process-kill.ccl"
        ;;
    oom-reclaim)
        # A process killed for want of memory is reclaimed while memory is
        # still exhausted: process teardown must not allocate.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-oom-reclaim.ccl"
        ;;
    scanout-repair)
        # The kernel's stuck-call panel is drawn on the boot framebuffer and
        # erased; the desktop must cover the erased rows again.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-scanout-repair.ccl"
        ;;
    devmem-denials)
        # CAP-001 diagnostics: refused device memory is recorded, logged
        # and shown on the Desktop bootstrap screen.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-devmem-denials.ccl"
        ;;
    capability-security)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-capability-security.ccl"
        ;;
    ccl-remote)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ccl-remote.ccl"
        ;;
    network-authority)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-network-authority.ccl"
        ;;
    net-link)
        INIT_PROFILE="$ROOT_DIR/tests/net-link/init-net-link.ccl"
        ;;
    net-multi)
        INIT_PROFILE="$ROOT_DIR/tests/net-link/init-net-multi.ccl"
        ;;
    bench-net)
        # tests/net-bench: the same client as the Linux reference (linux.sh).
        INIT_PROFILE="$ROOT_DIR/tests/net-bench/init-bench-net.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
        ;;
    bench-fs)
        # tests/fs-bench: the same program as the Linux reference (linux.sh),
        # with the same memory.
        INIT_PROFILE="$ROOT_DIR/tests/fs-bench/init-bench-fs.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
        ;;
    fs-coherence-*)
        FS_COHERENCE_CASE=${TEST_NAME#fs-coherence-}
        if ! grep -qx "$FS_COHERENCE_CASE" "$ROOT_DIR/tests/fs-coherence/cases.txt"; then
            echo "headless: unknown fs-coherence case: $FS_COHERENCE_CASE" >&2
            exit 2
        fi
        INIT_PROFILE="$ROOT_DIR/tests/fs-coherence/init-$FS_COHERENCE_CASE.ccl"
        # As bench-fs: each process lends the service about 9 MiB of queue
        # arenas, and two of them over the desktop's services exhaust the
        # default 128 MiB (the guest ran out of memory, not the cases).
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
        ;;
    bench-latency)
        # tests/sched-latency: interbench-style scheduler latency.
        INIT_PROFILE="$ROOT_DIR/tests/sched-latency/init-bench-latency.ccl"
        ;;
    threads)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-threads.ccl"
        # The per-thread CPU state check needs both instances on one CPU;
        # with more CPUs, work stealing may separate them.
        if [ "$QEMU_CPUS_EXPLICIT" = 0 ]; then QEMU_CPUS=1; fi
        ;;
    futex)
        # Threads of one process on every CPU at once (default four).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-futex.ccl"
        ;;
    firmware-snapshot)
        # FIRMWARE_SNAPSHOT over q35's ACPI tables: the approved reader,
        # the same image unapproved, and a program with no reader.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-firmware-snapshot.ccl"
        ;;
    clock)
        # docs/fast-clock.md: the clock publication page. The kernel
        # publishes it only for an invariant TSC, which QEMU exposes with
        # +invtsc under KVM (TCG drops the flag: run with --accel kvm).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-clock.ccl"
        case "$QEMU_CPU_MODEL" in
            *invtsc*) ;;
            *) QEMU_CPU_MODEL="$QEMU_CPU_MODEL,+invtsc" ;;
        esac
        ;;
    avx)
        # Two processes sharing one CPU, each holding its own YMM pattern
        # across thousands of switches (kernel XSAVE isolation).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-avx.ccl"
        if [ "$QEMU_CPUS_EXPLICIT" = 0 ]; then QEMU_CPUS=1; fi
        ;;
    rust-std)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-rust-std.ccl"
        ;;
    softpipe)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-softpipe.ccl"
        # Softpipe's context caches plus resident services exceed 128 MiB.
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
        ;;
    mesa-sync)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-mesa-sync.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
        ;;
    opengl)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-opengl.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
        ;;
    buffer)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-buffer.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
        ;;
    mesa-window)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-mesa-window.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
        ;;
    libc)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-libc.ccl"
        ;;
    binutils)
        # userspace/ports/binutils: as and ld on CuBit, compared with Linux.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-binutils.ccl"
        ;;
    gcc)
        # userspace/ports/gcc, stages 2 and 3: cc1, then the gcc driver, on
        # CuBit, compared with Linux.
        # cc1 is a 45 MB image that procmgr reads whole before mapping it.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-gcc.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-1G}"
        ;;
    gnat)
        # userspace/ports/gcc, stages 4 and 5: the gcc driver with gnat1,
        # gnatbind and libgnat build and run an Ada program on CuBit,
        # compared with Linux. gnat1 is a large image procmgr reads whole.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-gnat.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-1G}"
        ;;
    control-events)
        # docs/data-plane.md: grant lifecycle events and control messages.
        INIT_PROFILE="$ROOT_DIR/tests/control-events/init.ccl"
        ;;
    processes)
        # docs/process-arguments.md: launch arguments, posix_spawn, waitpid.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-processes.ccl"
        ;;
    program-parameters)
        # docs/ccl-launch-parameters.md: typed program parameters from the
        # startup plan, checked by procmgr, decoded by the program.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-program-parameters.ccl"
        ;;
    servo)
        # Servo (docs/servo-port.md): an 85 MB program whose heap grows
        # eagerly; fonts are installed on the disk below.
        # SERVO_DESKTOP=1: launch it from the desktop's Apps menu instead.
        if [ -n "${SERVO_DESKTOP:-}" ]; then
            INIT_PROFILE="$ROOT_DIR/tests/headless/init-servo-desktop.ccl"
        else
            INIT_PROFILE="$ROOT_DIR/tests/headless/init-servo.ccl"
        fi
        QEMU_MEMORY="${QEMU_MEMORY:-4G}"
        ;;
    bench-spread)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-bench-spread.ccl"
        ;;
    timesync)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-timesync.ccl"
        ;;
    tls-probe)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-tls-probe.ccl"
        ;;
    tls-service)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-tls-service.ccl"
        ;;
    wget-https)
        # Needs internet access from the host: a manual, network-dependent
        # lane, not a deterministic regression.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-wget-https.ccl"
        ;;
    storage-grants)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-storage-grants.ccl"
        ;;
    desktop-display|desktop-virtio-vga|desktop-vm)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-desktop-display.ccl"
        # CUBIT_TEST_INIT_PROFILE (desktop-display only): another startup
        # profile, e.g. one that also starts apps without launchers on
        # this image, for photographs (tests/headless/desktop-polish-shots.py).
        if [ "$TEST_NAME" = desktop-display ] && [ -n "${CUBIT_TEST_INIT_PROFILE:-}" ]; then
            INIT_PROFILE="$CUBIT_TEST_INIT_PROFILE"
        fi
        ;;
    boot-timeline)
        # docs/boot-timeline.md: the desktop-display boot plus the reader.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-boot-timeline.ccl"
        ;;
    input-stream)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-input-stream.ccl"
        ;;
    bench-audio)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-bench-audio.ccl"
        ;;
    desktop-dual-output)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-desktop-dual.ccl"
        ;;
    desktop-protocol)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-desktop-protocol.ccl"
        ;;
    display-grants|display-grants-virtio-vga|display-dual-output|display-discovery-multi-output|display-discovery-boot-only)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-display-grants.ccl"
        ;;
    managed-ui)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-managed-ui.ccl"
        ;;
    devices)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-devices.ccl"
        ;;
    render-launch-policy)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-render-launch-policy.ccl"
        if [ "${CUBIT_RENDER_OPTIONAL_TEST:-0}" = 1 ]; then
            INIT_PROFILE="$ROOT_DIR/tests/render-startup/native/init.ccl"
        fi
        ;;
    mesa-log-bridge)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-mesa-log-bridge.ccl"
        ;;
    mesa-native-instance)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-mesa-native-instance.ccl"
        # Static ANV image plus procmgr's retained ELF buffer and services
        # exceed the tiny generic 128 MiB fixture, like the other Mesa tests.
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
        ;;
    files)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-files.ccl"
        ;;
    generic-launch)
        # HW-002: a driver devmgr has no dedicated setup for, started from
        # its manifest alone (docs/device-matching.md "Generic PCI launch").
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-generic-launch.ccl"
        ;;
    ufs)
        # tests/ufs: the UFS driver (HW-001) on QEMU's UFS model, reading a
        # GPT disk laid out like the IdeaPad's. QEMU is a first check, not
        # the hardware test: the laptop's controller is.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ufs.ccl"
        TEMP_UFS_DISK="$(mktemp "${TMPDIR:-/tmp}/cubit-ufs-disk.XXXXXX.img")"
        python3 -I "$ROOT_DIR/tests/ufs/make-disk.py" "$TEMP_UFS_DISK"
        # Read only at the host as well: the driver must never write.
        UFS_ARGS=(-drive "file=$TEMP_UFS_DISK,if=none,id=ufs0,format=raw,readonly=on"
                  -device ufs,serial=cubitufs,id=ufs
                  -device ufs-lu,drive=ufs0,bus=ufs,lun=0)
        ;;
    ufs-write)
        # HW-001 step 4: writes behind the block write grant, on QEMU's UFS
        # model (not the laptop's controller). disk-write-check, holding
        # disk-install, opens the disk through devmgr read-only (a write is
        # refused) and read-write (write, FUA write, flush, readback); the
        # host then reads the blocks back out of the image file.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ufs-write.ccl"
        TEMP_UFS_DISK="$(mktemp "${TMPDIR:-/tmp}/cubit-ufs-disk.XXXXXX.img")"
        python3 -I "$ROOT_DIR/tests/ufs/make-disk.py" "$TEMP_UFS_DISK"
        UFS_ARGS=(-drive "file=$TEMP_UFS_DISK,if=none,id=ufs0,format=raw"
                  -device ufs,serial=cubitufs,id=ufs
                  -device ufs-lu,drive=ufs0,bus=ufs,lun=0)
        ;;
    multi-instance)
        # One driver per device for devmgr's dedicated setups: two xHCI
        # controllers and two NVMe disks (tests/multi-instance).
        INIT_PROFILE="$ROOT_DIR/tests/multi-instance/init.ccl"
        ;;
    usb-hotplug)
        # Two xHCI controllers, the boot mouse on the SECOND (as on the
        # IdeaPad: 00:0d.0 TCSS empty, 00:14.0 PCH with the mouse), then
        # hotplug through the monitor (tests/usb-hotplug).
        INIT_PROFILE="$ROOT_DIR/tests/multi-instance/init.ccl"
        ;;
    startup-health)
        # CAP-003: the full desktop boots with no refusal, every service
        # registers, and the apps' records reach logstore (boot-logs prints
        # what logstore keeps); check-refusals.py fails on refusals.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-startup-health.ccl"
        ;;
    launch-failure)
        # Desktop names why an Apps-menu launch failed (docs/process-arguments.md).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-launch-failure.ccl"
        ;;
    text)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-text.ccl"
        ;;
    calculator)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-calculator.ccl"
        ;;
    clipboard)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-clipboard.ccl"
        ;;
    key-repeat)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-key-repeat.ccl"
        ;;
    desktop-doom)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-doom-desktop.ccl"
        ;;
    virtio-gpu|virtio-vga-primary)
        if [ "$TEST_NAME" = "virtio-vga-primary" ]; then
            INIT_PROFILE="$ROOT_DIR/init.ccl"
        fi
        ;;
esac

if [ "${CUBIT_DESKTOP_METRICS_TEST:-0}" = 1 ]; then
    if [ "$TEST_NAME" != ccl-workspace ] || [ -z "${CUBIT_DESKTOP_IMAGE:-}" ]; then
        echo "headless: Desktop metrics requires ccl-workspace and a metrics-enabled Desktop image" >&2
        exit 1
    fi
    INIT_PROFILE="$ROOT_DIR/tests/compositor/init-metrics-desktop.ccl"
    if [ "${CUBIT_DESKTOP_METRICS_LOAD:-0}" = 1 ]; then
        if [ "${CUBIT_DESKTOP_METRICS_FAULT:-0}" = 1 ]; then
            echo "headless: metrics load and fault fixtures are mutually exclusive" >&2
            exit 1
        fi
        INIT_PROFILE="$ROOT_DIR/tests/compositor/init-metrics-load.ccl"
    fi
    if [ "${CUBIT_DESKTOP_METRICS_FAULT:-0}" = 1 ]; then
        if [ -z "${CUBIT_METRICS_FAULT_IMAGE:-}" ]; then
            echo "headless: metrics fault mode requires its isolated test collector" >&2
            exit 1
        fi
        INIT_PROFILE="$ROOT_DIR/tests/compositor/init-metrics-fault.ccl"
    fi
fi

if [ "${CUBIT_TEST_IDLE_DPI:-0}" = 1 ]; then
    if [ "$TEST_NAME" != desktop-dual-output ] || [ "${CUBIT_TEST_MIXED_OUTPUTS:-0}" != 1 ]; then
        echo "headless: idle DPI requires desktop-dual-output and mixed outputs" >&2
        exit 1
    fi
    INIT_PROFILE="$ROOT_DIR/tests/headless/init-desktop-dpi.ccl"
    QEMU_MEMORY="${QEMU_MEMORY:-1G}"
fi

if [ "$BENCH_LOAD" = 1 ]; then
    INIT_PROFILE="$ROOT_DIR/tests/headless/init-${TEST_NAME}-load.ccl"
    if [ "$LOAD_WORKERS" -gt 1 ]; then
        TEMP_LOAD_PROFILE="$(mktemp "${TMPDIR:-/tmp}/cubit-input-load.XXXXXX.ccl")"
        awk -v count="$LOAD_WORKERS" '
            /^[[:space:]]*[(]start "bench-input-load[.]app" [(]priority 4[)][)]$/ {
                for (i=1; i<=count; i++) print;
                next
            }
            { print }
        ' "$INIT_PROFILE" > "$TEMP_LOAD_PROFILE"
        INIT_PROFILE="$TEMP_LOAD_PROFILE"
    fi
fi
# desktop-vm mirrors run-desktop (2 GiB): large GPU banks (modes up to
# 1920x1080) need at least 1 GiB (devmgr LARGE_BANK_MINIMUM_MEMORY).
if [ "$TEST_NAME" = desktop-vm ]; then
    QEMU_MEMORY="${QEMU_MEMORY:-2G}"
fi
# CUBIT_TEST_VIDEO_SIZE=WxH (desktop-display and calculator): the firmware mode, as
# WIDTHxHEIGHT; larger modes need room for Desktop's output buffers.
if { [ "$TEST_NAME" = desktop-display ] || [ "$TEST_NAME" = calculator ]; } && [ -n "${CUBIT_TEST_VIDEO_SIZE:-}" ]; then
    if ! [[ "$CUBIT_TEST_VIDEO_SIZE" =~ ^([0-9]+)x([0-9]+)$ ]]; then
        echo "headless: CUBIT_TEST_VIDEO_SIZE must be WIDTHxHEIGHT" >&2
        exit 1
    fi
    VIDEO_WIDTH=${BASH_REMATCH[1]}
    VIDEO_HEIGHT=${BASH_REMATCH[2]}
    QEMU_MEMORY="${QEMU_MEMORY:-2G}"
fi
# A desktop session (desktop.svc, its GPU buffers, fonts and workbench) needs
# more than the 128M default. Resource-constrained sizing comes later.
if [ -z "$QEMU_MEMORY" ] && [ -n "$INIT_PROFILE" ] && grep -q '"desktop.svc"' "$INIT_PROFILE"; then
    QEMU_MEMORY=512M
fi
# config-storage.svc and the Turso probe (each about 20 MiB on disk, Turso
# linked in) do not fit beside the other services in 128M: config-storage's
# heap commits fail after a few hundred KiB and the probe fails to load.
if [ "$CONFIG_STORAGE_TEST" = 1 ] || [ "$TEST_NAME" = turso-native-std ] || [ "$TEST_NAME" = turso-native ]; then
    QEMU_MEMORY="${QEMU_MEMORY:-512M}"
fi
if [ -n "$INIT_PROFILE" ]; then
    TEMP_DISK="$(mktemp "${TMPDIR:-/tmp}/cubit-${TEST_NAME}-disk.XXXXXX.img")"
    cp "$BASE_DISK" "$TEMP_DISK"
    debugfs -w -R "rm init.ccl" "$TEMP_DISK" >/dev/null 2>&1
    if ! debugfs -w -R "write $INIT_PROFILE init.ccl" "$TEMP_DISK" >/dev/null 2>&1; then
        echo "headless: failed to install $TEST_NAME init.ccl" >&2
        exit 1
    fi
    if [ "$TEST_NAME" = "rust-native" ]; then
        for app in rust-probe.app rust-probe-denied.app rust-probe-mismatch.app ccl-test-host.svc clock.svc; do
            if [ ! -f "$KERNEL_DIR/isodir/boot/$app" ]; then
                echo "headless: build rust-probe ccl-test-host clock first ($app missing)" >&2
                exit 1
            fi
            debugfs -w -R "rm $app" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$app $app" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
    fi
    if [ "$CONFIG_STORAGE_TEST" = 1 ]; then
        for STORAGE_IMAGE in "$ROOT_DIR/userspace/services/config-storage/build/config-storage.svc" \
                             "$KERNEL_DIR/isodir/boot/clock.svc"; do
            if [ ! -f "$STORAGE_IMAGE" ]; then
                echo "headless: build Config storage/clock first: $STORAGE_IMAGE" >&2
                exit 1
            fi
            STORAGE_NAME=$(basename "$STORAGE_IMAGE")
            debugfs -w -R "rm $STORAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $STORAGE_IMAGE $STORAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
        if [ "$TEST_NAME" != "config-storage" ]; then
            OBJECT_APP=config-objects-check.app
            if [ "$TEST_NAME" = config-objects-reopen ]; then OBJECT_APP=config-objects-reopen.app; fi
            if [ "$TEST_NAME" = config-objects-benchmark ]; then OBJECT_APP=config-objects-benchmark.app; fi
            OBJECT_PROBE="$ROOT_DIR/tests/config-object-client/native-app/build/$OBJECT_APP"
            if [ ! -f "$OBJECT_PROBE" ]; then
                echo "headless: build tests/config-object-client/native-app/build.sh first" >&2
                exit 1
            fi
            debugfs -w -R "rm $OBJECT_APP" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $OBJECT_PROBE $OBJECT_APP" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        fi
    fi
    if [ "$TEST_NAME" = "turso-native-std" ] || [ "$TEST_NAME" = "turso-native" ]; then
        TURSO_PROBE="$ROOT_DIR/tests/config-turso/target/native/turso-native-probe.app"
        if [ ! -f "$TURSO_PROBE" ]; then
            echo "headless: build tests/config-turso/native/build.sh first" >&2
            exit 1
        fi
        debugfs -w -R "rm turso-native-probe.app" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "write $TURSO_PROBE turso-native-probe.app" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        debugfs -w -R "rm clock.svc" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "write $KERNEL_DIR/isodir/boot/clock.svc clock.svc" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        if [ "$TEST_NAME" = "turso-native" ]; then
            # debugfs mkdir on an existing directory can leave orphaned inode/
            # block metadata even though it prints an error. Never use it as
            # mkdir -p, particularly when restaging a saved first-boot image.
            TURSO_DIRECTORY=$(debugfs -R 'stat /turso-native' "$TEMP_DISK" 2>/dev/null)
            if [ -z "$TURSO_DIRECTORY" ]; then
                if [ "$TURSO_REVISION" = 2 ]; then
                    echo "headless: fresh-boot fixture directory is missing" >&2
                    exit 1
                fi
                debugfs -w -R 'mkdir /turso-native' "$TEMP_DISK" >/dev/null 2>&1 || exit 1
            fi
            if ! debugfs -R 'stat /turso-native' "$TEMP_DISK" 2>/dev/null | grep -q 'Type: directory'; then
                echo "headless: Turso test directory unavailable" >&2
                exit 1
            fi
            if ! e2fsck -fn "$TEMP_DISK"; then
                echo "headless: Turso fixture is inconsistent before boot" >&2
                exit 1
            fi
        fi
    fi
    if [ "$TEST_NAME" = mesa-log-bridge ]; then
        for app in logstore.svc mesa-log-check.app; do
            if [ ! -f "$KERNEL_DIR/isodir/boot/$app" ]; then
                echo "headless: build logstore mesa-log-test first ($app missing)" >&2
                exit 1
            fi
            debugfs -w -R "rm $app" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$app $app" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
    fi
    if [ "$TEST_NAME" = mesa-native-instance ]; then
        MESA_INSTANCE_IMAGE="$KERNEL_DIR/isodir/boot/mesa-no-provider.app"
        if [ ! -f "$MESA_INSTANCE_IMAGE" ]; then
            echo "headless: build and stage the native Mesa no-provider fixture first" >&2
            exit 1
        fi
        debugfs -w -R 'rm mesa-no-provider.app' "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "write $MESA_INSTANCE_IMAGE mesa-no-provider.app" \
            "$TEMP_DISK" >/dev/null 2>&1 || exit 1
    fi
    if [ "$TEST_NAME" = gpu-viewer ]; then
        for app in gpu-viewer-owner.app gpu-viewer-test.app; do
            if [ ! -f "$KERNEL_DIR/isodir/boot/$app" ]; then
                echo "headless: build gpu-viewer-test first ($app missing)" >&2
                exit 1
            fi
            debugfs -w -R "rm $app" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$app $app" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
    fi
    if [[ "$TEST_NAME" == grant-forward* ]]; then
        for role in owner reader client intermediary_exit owner_exit desktop; do
            grant_image="$KERNEL_DIR/isodir/boot/grant-forward-$role.app"
            if [ ! -f "$grant_image" ]; then
                echo "headless: build grant-forward first ($grant_image missing)" >&2
                exit 1
            fi
            debugfs -w -R "rm grant-forward-$role.app" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $grant_image grant-forward-$role.app" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
    fi
    if [ "$TEST_NAME" = "async-ipc" ]; then
        for ipc_image in ipctest-server.app ipctest-client.app ipctest-departing.app; do
            if [ ! -f "$KERNEL_DIR/isodir/boot/$ipc_image" ]; then
                echo "headless: build ipctest-server ipctest-client first ($ipc_image missing)" >&2
                exit 1
            fi
            debugfs -w -R "rm $ipc_image" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$ipc_image $ipc_image" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
    fi
    if [ "$TEST_NAME" = "bench-ipc" ] || [ "$TEST_NAME" = "bench-audio" ] || [ "$TEST_NAME" = "bench-storage" ] || [ "$TEST_NAME" = "bench-input" ] || [ "$TEST_NAME" = "bench-scheduler" ]; then
        # Stage the current diagnostic, not an executable left by an earlier
        # manual build. The kernel/initrd refresh alone does not rebuild apps.
        benchmark_target="$TEST_NAME"
        if [ "$TEST_NAME" = "bench-scheduler" ]; then benchmark_target=bench-load; fi
        # bench-ipc has no make target of its own: build its two programs.
        if [ "$TEST_NAME" = "bench-ipc" ]; then benchmark_target="bench-ipc-client bench-ipc-server"; fi
        # shellcheck disable=SC2086
        if ! make -s -C "$KERNEL_DIR" $benchmark_target; then
            echo "headless: failed to build $benchmark_target" >&2
            exit 1
        fi
        if [ "$BENCH_LOAD" = 1 ] && ! make -s -C "$KERNEL_DIR" bench-load; then
            echo "headless: failed to build benchmark load" >&2
            exit 1
        fi
        BENCHMARK_IMAGES="bench-ipc-client.app bench-ipc-server.app"
        if [ "$TEST_NAME" = "bench-audio" ]; then BENCHMARK_IMAGES="bench-audio.app"; fi
        if [ "$TEST_NAME" = "bench-storage" ]; then BENCHMARK_IMAGES="bench-storage.app"; fi
        if [ "$TEST_NAME" = "bench-input" ]; then BENCHMARK_IMAGES="bench-input.app"; fi
        if [ "$TEST_NAME" = "bench-scheduler" ]; then BENCHMARK_IMAGES="bench-cpu.app"; fi
        if [ "$BENCH_LOAD" = 1 ]; then
            if [ "$TEST_NAME" = "bench-input" ]; then
                BENCHMARK_IMAGES="$BENCHMARK_IMAGES bench-input-load.app"
            else
                BENCHMARK_IMAGES="$BENCHMARK_IMAGES bench-load.app"
            fi
        fi
        for benchmark_image in $BENCHMARK_IMAGES; do
            if [ ! -f "$KERNEL_DIR/isodir/boot/$benchmark_image" ]; then
                echo "headless: build $benchmark_image first" >&2
                exit 1
            fi
            debugfs -w -R "rm $benchmark_image" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$benchmark_image $benchmark_image" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
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
        if [ "${CUBIT_DOOM_MULTIAPP:-0}" = 1 ]; then
            for app in ccl-workbench.app; do
                if [ ! -f "$KERNEL_DIR/isodir/boot/$app" ]; then
                    echo "headless: build $app before the multi-app DOOM test" >&2
                    exit 1
                fi
                debugfs -w -R "rm $app" "$TEMP_DISK" >/dev/null 2>&1
                debugfs -w -R "write $KERNEL_DIR/isodir/boot/$app $app" \
                    "$TEMP_DISK" >/dev/null 2>&1 || exit 1
            done
        fi
    fi
    # CUBIT_TEST_EXTRA_APPS="a.app b.app" (desktop-display only): more
    # staged apps on the disk, for photographs of apps not on this image.
    if [ "$TEST_NAME" = "desktop-display" ] && [ -n "${CUBIT_TEST_EXTRA_APPS:-}" ]; then
        for app in $CUBIT_TEST_EXTRA_APPS; do
            if [ ! -f "$KERNEL_DIR/isodir/boot/$app" ]; then
                echo "headless: build $app first (CUBIT_TEST_EXTRA_APPS)" >&2
                exit 1
            fi
            debugfs -w -R "rm $app" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$app $app" \
                "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
    fi
    # desktop-vm drags the workbench window (hardware pointer follows drags).
    if [ "$TEST_NAME" = "desktop-display" ] || [ "$TEST_NAME" = "desktop-dual-output" ] ||
       [ "$TEST_NAME" = "desktop-vm" ]; then
        app=ccl-workbench.app
        if [ ! -f "$KERNEL_DIR/isodir/boot/$app" ]; then
            echo "headless: build ccl-workbench before desktop window tests" >&2
            exit 1
        fi
        debugfs -w -R "rm $app" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "write $KERNEL_DIR/isodir/boot/$app $app" \
            "$TEMP_DISK" >/dev/null 2>&1 || exit 1
    fi
    if [ "$TEST_NAME" = "desktop-display" ] || [ "$TEST_NAME" = "mesa-window" ] || [ "$TEST_NAME" = "gpu-viewer" ] ||
       [ "$TEST_NAME" = "bench-input" ] ||
       [ "$TEST_NAME" = "desktop-protocol" ] ||
       [ "$TEST_NAME" = "desktop-dual-output" ] ||
       [ "$TEST_NAME" = "ccl-workbench" ] ||
       [ "$TEST_NAME" = "ccl-workbench-virtio-vga" ] ||
       [ "$TEST_NAME" = "ccl-workspace" ] ||
       [ "$TEST_NAME" = "ccl-console" ] ||
       [ "$TEST_NAME" = "lexicon" ] ||
       [ "$TEST_NAME" = "dictionary" ] ||
       [ "$TEST_NAME" = "logs" ] ||
       [ "$TEST_NAME" = "devmem-denials" ] ||
       [ "$TEST_NAME" = "process-kill" ] ||
       [ "$TEST_NAME" = "scanout-repair" ] ||
       [ "$TEST_NAME" = "input-stream" ] ||
       [ "$TEST_NAME" = "managed-ui" ] ||
       [ "$TEST_NAME" = "devices" ] ||
       [ "$TEST_NAME" = "render-launch-policy" ] ||
       [ "$TEST_NAME" = "config-tree" ] ||
       [ "$TEST_NAME" = "files" ] ||
       [ "$TEST_NAME" = "startup-health" ] ||
       [ "$TEST_NAME" = "launch-failure" ] ||
       [ "$TEST_NAME" = "text" ] ||
       [ "$TEST_NAME" = "calculator" ] ||
       [ "$TEST_NAME" = "clipboard" ] ||
       [ "$TEST_NAME" = "key-repeat" ] ||
       [ "$TEST_NAME" = "desktop-virtio-vga" ] ||
       [ "$TEST_NAME" = "desktop-vm" ] ||
       [ "$TEST_NAME" = "desktop-doom" ] ||
       [ "$TEST_NAME" = "virtio-vga-primary" ] ||
       [ "$TEST_NAME" = "usb-hotplug" ] ||
       [ "$TEST_NAME" = "boot-timeline" ]; then
        for DESKTOP_TEST_IMAGE_NAME in display.svc desktop.svc virtio-gpu.drv; do
            DESKTOP_TEST_IMAGE="$KERNEL_DIR/isodir/boot/$DESKTOP_TEST_IMAGE_NAME"
            # Explicit test-only compositor executable; never alter production staging.
            if [ "$DESKTOP_TEST_IMAGE_NAME" = desktop.svc ] && [ -n "${CUBIT_DESKTOP_IMAGE:-}" ]; then
                DESKTOP_TEST_IMAGE="$CUBIT_DESKTOP_IMAGE"
            fi
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
        # Desktop's asset packages (docs/assets.md), from the current build:
        # the wallpapers and the application icon atlas.
        debugfs -w -R "mkdir Assets" "$TEMP_DISK" >/dev/null 2>&1
        for ASSET_ITEM in cubit-wallpapers/1/cubes.qoi cubit-wallpapers/1/cubie.qoi \
          cubit-icons/4/applications.qoi cubit-lexicon/1/english.lexicon cubit-lexicon/1/LEXICON-NOTICE; do
            ASSET_PACKAGE="Assets/${ASSET_ITEM%/*}"
            debugfs -w -R "mkdir ${ASSET_PACKAGE%/*}" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "mkdir $ASSET_PACKAGE" "$TEMP_DISK" >/dev/null 2>&1
            ASSET_FILE="$KERNEL_DIR/build/Assets/$ASSET_ITEM"
            if [ ! -f "$ASSET_FILE" ]; then
                echo "headless: build wallpaper-assets and icon-assets first: $ASSET_FILE" >&2
                exit 1
            fi
            debugfs -w -R "rm Assets/$ASSET_ITEM" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $ASSET_FILE Assets/$ASSET_ITEM" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install Assets/$ASSET_ITEM" >&2
                exit 1
            fi
        done
    fi
    if [ "${CUBIT_TEST_IDLE_DPI:-0}" = 1 ]; then
        DPI_IMAGE="$ROOT_DIR/tests/compositor/build/dpi-client/dpi-client.app"
        DPI_CHECK=$(mktemp "${TMPDIR:-/tmp}/cubit-dpi-image.XXXXXXXX")
        debugfs -w -R "rm dpi-client.app" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "write $DPI_IMAGE dpi-client.app" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -R "dump /dpi-client.app $DPI_CHECK" "$TEMP_DISK" >/dev/null 2>&1
        if ! cmp -s "$DPI_IMAGE" "$DPI_CHECK"; then
            rm -f "$DPI_CHECK"
            echo "headless: DPI client image verification failed" >&2
            exit 1
        fi
        rm -f "$DPI_CHECK"
    fi
    if [ "$TEST_NAME" = "desktop-protocol" ]; then
        DESKTOP_CHECK_IMAGE="$KERNEL_DIR/isodir/boot/desktop-check.app"
        if [ ! -f "$DESKTOP_CHECK_IMAGE" ]; then
            echo "headless: build desktop-check first" >&2
            exit 1
        fi
        debugfs -w -R "rm desktop-check.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $DESKTOP_CHECK_IMAGE desktop-check.app" "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install desktop-check" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "generic-launch" ] || [ "$TEST_NAME" = "ufs" ] ||
       [ "$TEST_NAME" = "ufs-write" ] || [ "$TEST_NAME" = "multi-instance" ] ||
       [ "$TEST_NAME" = "usb-hotplug" ]; then
        # boot-logs prints the records logstore keeps (the base disk lacks it):
        # generic drivers (generic-check, ufs) and devmgr log through logstore.
        for GENERIC_IMAGE in logstore.svc boot-logs.app; do
            if [ ! -f "$KERNEL_DIR/isodir/boot/$GENERIC_IMAGE" ]; then
                echo "headless: build $GENERIC_IMAGE first (make -C kernel world boot-logs)" >&2
                exit 1
            fi
            debugfs -w -R "rm $GENERIC_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$GENERIC_IMAGE $GENERIC_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
    fi
    if [ "$TEST_NAME" = "ufs-write" ]; then
        CHECK_APP="disk-write-check"
        WRITE_CHECK_IMAGE="$KERNEL_DIR/isodir/boot/$CHECK_APP.app"
        if [ ! -f "$WRITE_CHECK_IMAGE" ]; then
            echo "headless: build $CHECK_APP first (make -C kernel $CHECK_APP)" >&2
            exit 1
        fi
        debugfs -w -R "rm $CHECK_APP.app" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "write $WRITE_CHECK_IMAGE $CHECK_APP.app" \
          "$TEMP_DISK" >/dev/null 2>&1 || exit 1
    fi
    if [ "$TEST_NAME" = "startup-health" ]; then
        # The current build of every program the profile starts (the base
        # disk may be older), and boot-logs, which the disk does not carry.
        for HEALTH_IMAGE in logstore.svc clock.svc timesync.svc tls.svc lexicon.svc shell.app \
                            files.app calculator.app boot-logs.app; do
            if [ ! -f "$KERNEL_DIR/isodir/boot/$HEALTH_IMAGE" ]; then
                echo "headless: build $HEALTH_IMAGE first (make -C kernel world boot-logs)" >&2
                exit 1
            fi
            debugfs -w -R "rm $HEALTH_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$HEALTH_IMAGE $HEALTH_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
    fi
    if [ "$TEST_NAME" = "boot-timeline" ]; then
        for TIMELINE_IMAGE in logstore.svc clock.svc boot-timeline.app; do
            debugfs -w -R "rm $TIMELINE_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$TIMELINE_IMAGE $TIMELINE_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $TIMELINE_IMAGE (make boot-timeline)" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "virtio-vga-primary" ] || [ "$TEST_NAME" = "desktop-display" ] ||
       [ "$TEST_NAME" = "boot-timeline" ]; then
        SHELL_TEST_IMAGE="$KERNEL_DIR/isodir/boot/shell.app"
        if [ ! -f "$SHELL_TEST_IMAGE" ]; then
            echo "headless: build shell first" >&2
            exit 1
        fi
        debugfs -w -R "rm shell.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $SHELL_TEST_IMAGE shell.app" "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install current shell" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "display-grants" ] || [ "$TEST_NAME" = "display-grants-virtio-vga" ] ||
       [ "$TEST_NAME" = "display-dual-output" ] ||
       [ "$TEST_NAME" = "display-discovery-multi-output" ] ||
       [ "$TEST_NAME" = "display-discovery-boot-only" ]; then
        for DISPLAY_TEST_IMAGE_NAME in display.svc display-check.app virtio-gpu.drv; do
            DISPLAY_TEST_IMAGE="$KERNEL_DIR/isodir/boot/$DISPLAY_TEST_IMAGE_NAME"
            if [ ! -f "$DISPLAY_TEST_IMAGE" ]; then
                echo "headless: build display and display-check first" >&2
                exit 1
            fi
            debugfs -w -R "rm $DISPLAY_TEST_IMAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $DISPLAY_TEST_IMAGE $DISPLAY_TEST_IMAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $DISPLAY_TEST_IMAGE_NAME" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "ccl-vm" ] || [ "$TEST_NAME" = "ccl-workbench" ] ||
       [ "$TEST_NAME" = "ccl-workbench-virtio-vga" ] ||
       [ "$TEST_NAME" = "ccl-workspace" ] || [ "$TEST_NAME" = "ccl-console" ] ||
       [ "$TEST_NAME" = "lexicon" ] || [ "$TEST_NAME" = "dictionary" ] ||
       [ "$TEST_NAME" = "logs" ]; then
        if [ "$TEST_NAME" = "ccl-vm" ]; then
            CCL_IMAGES="ccl-vm.app ccl-test-host.svc clock.svc"
        elif [ "$TEST_NAME" = "ccl-console" ]; then
            CCL_IMAGES="ccl-console.app clock.svc desktop.svc display.svc logstore.svc"
        elif [ "$TEST_NAME" = "logs" ]; then
            CCL_IMAGES="logs.app clock.svc desktop.svc display.svc"
        elif [ "$TEST_NAME" = "lexicon" ]; then
            CCL_IMAGES="ccl-console.app lexicon.svc clock.svc desktop.svc display.svc logstore.svc"
        elif [ "$TEST_NAME" = "dictionary" ]; then
            CCL_IMAGES="dictionary.app lexicon.svc clock.svc desktop.svc display.svc logstore.svc"
        else
            CCL_IMAGES="ccl-workbench.app clock.svc desktop.svc display.svc"
            if [ "${CUBIT_DESKTOP_METRICS_TEST:-0}" = 1 ]; then
                CCL_IMAGES="$CCL_IMAGES metrics.svc"
                if [ "${CUBIT_DESKTOP_METRICS_FAULT:-0}" != 1 ]; then
                    if [ "${CUBIT_DESKTOP_METRICS_LOAD:-0}" = 1 ]; then
                        CCL_IMAGES="$CCL_IMAGES desktop-metrics-load.app desktop-metrics-load-observer.app"
                    else
                    CCL_IMAGES="$CCL_IMAGES desktop-metrics-observer.app"
                    fi
                fi
            fi
            debugfs -w -R "mkdir work" "$TEMP_DISK" >/dev/null 2>&1
        fi
        if [ "$TEST_NAME" = "ccl-console" ]; then
            # A picture for (image.load "picture.qoi"): every QOI chunk kind.
            CONSOLE_PICTURE="${TMPDIR:-/tmp}/cubit-console-picture-$$.qoi"
            python3 "$ROOT_DIR/tests/ccl-console/make-picture.py" "$CONSOLE_PICTURE"
            debugfs -w -R "mkdir work" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "rm work/picture.qoi" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $CONSOLE_PICTURE work/picture.qoi" "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install the console picture" >&2
                exit 1
            fi
            rm -f "$CONSOLE_PICTURE"
            if [ "${CCL_CONSOLE_DEMO:-}" = gcc ]; then
                # gcc, typed from the console: the toolchain, and hello.c (its
                # manifest as assembly) and bad.c in @nvme:0/work.
                if ! make -C "$KERNEL_DIR" libc user_runtime ccl-manifest >/dev/null ||
                   ! bash "$ROOT_DIR/tests/binutils/build.sh" "$KERNEL_DIR/isodir/boot" >/dev/null ||
                   ! bash "$ROOT_DIR/tests/gcc/build.sh" "$KERNEL_DIR/isodir/boot" >/dev/null; then
                    echo "headless: failed to build gcc for the console demo" >&2
                    exit 1
                fi
                install_gcc_toolchain || exit 1
                for PROGRAM_INPUT in "$ROOT_DIR/tests/gcc/hello.c" \
                  "$ROOT_DIR/tests/gcc/build/hello-manifest.s" "$ROOT_DIR/tests/ccl-console/bad.c"; do
                    debugfs -w -R "rm work/$(basename "$PROGRAM_INPUT")" "$TEMP_DISK" >/dev/null 2>&1
                    debugfs -w -R "write $PROGRAM_INPUT work/$(basename "$PROGRAM_INPUT")" \
                      "$TEMP_DISK" >/dev/null 2>&1 || exit 1
                done
                for PROGRAM_OUTPUT in hello bad; do
                    debugfs -w -R "rm work/$PROGRAM_OUTPUT" "$TEMP_DISK" >/dev/null 2>&1
                done
            fi
            if [ "${CCL_CONSOLE_DEMO:-}" = install ]; then
                if [ ! -f "$KERNEL_DIR/isodir/boot/installer.svc" ]; then
                    echo "headless: build installer.svc first (make -C kernel installer)" >&2
                    exit 1
                fi
                debugfs -w -R "rm installer.svc" "$TEMP_DISK" >/dev/null 2>&1
                debugfs -w -R "write $KERNEL_DIR/isodir/boot/installer.svc installer.svc" \
                  "$TEMP_DISK" >/dev/null 2>&1 || exit 1
                INSTALL_PAYLOAD="$(mktemp -d "${TMPDIR:-/tmp}/cubit-install-payload.XXXXXX")"
                if [ -n "${CUBIT_INSTALL_PAYLOAD:-}" ]; then
                    cp -r "$CUBIT_INSTALL_PAYLOAD/." "$INSTALL_PAYLOAD/"
                else
                    python3 -I "$ROOT_DIR/tests/disk-layout/make-payload.py" \
                      "$INSTALL_PAYLOAD/esp" "$INSTALL_PAYLOAD/root"
                fi
                python3 -I "$ROOT_DIR/tests/disk-layout/debugfs-script.py" "$INSTALL_PAYLOAD" install \
                  > "$INSTALL_PAYLOAD.commands"
                debugfs -w -f "$INSTALL_PAYLOAD.commands" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
                rm -rf "$INSTALL_PAYLOAD" "$INSTALL_PAYLOAD.commands"
            fi
            if [ "${CCL_CONSOLE_DEMO:-}" = programs ]; then
                # as and ld, typed from the console (docs/ccl-launch-parameters.md):
                # built by tests/binutils/build.sh into isodir/boot.
                for PROGRAM_IMAGE in as.app ld.app; do
                    debugfs -w -R "rm $PROGRAM_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
                    debugfs -w -R "write $KERNEL_DIR/isodir/boot/$PROGRAM_IMAGE $PROGRAM_IMAGE" \
                      "$TEMP_DISK" >/dev/null 2>&1 || exit 1
                done
                for PROGRAM_INPUT in hello.s bad.s; do
                    SOURCE_DIR="$ROOT_DIR/tests/binutils"
                    [ "$PROGRAM_INPUT" = bad.s ] && SOURCE_DIR="$ROOT_DIR/tests/ccl-console"
                    debugfs -w -R "write $SOURCE_DIR/$PROGRAM_INPUT work/$PROGRAM_INPUT" \
                      "$TEMP_DISK" >/dev/null 2>&1 || exit 1
                done
            fi
        fi
        for CCL_IMAGE_NAME in $CCL_IMAGES; do
            CCL_IMAGE="$KERNEL_DIR/isodir/boot/$CCL_IMAGE_NAME"
            if [ "$CCL_IMAGE_NAME" = desktop.svc ] && [ -n "${CUBIT_DESKTOP_IMAGE:-}" ]; then
                CCL_IMAGE="$CUBIT_DESKTOP_IMAGE"
            fi
            if [ "$CCL_IMAGE_NAME" = metrics.svc ] && [ "${CUBIT_DESKTOP_METRICS_FAULT:-0}" = 1 ]; then
                CCL_IMAGE="$CUBIT_METRICS_FAULT_IMAGE"
            fi
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
    if [ "$TEST_NAME" = "devices" ] || [ "$TEST_NAME" = "managed-ui" ] || [ "$TEST_NAME" = "render-launch-policy" ]; then
        DEVICE_TEST_IMAGES="devices.app desktop.svc"
        if [ "$TEST_NAME" = "managed-ui" ]; then
            DEVICE_TEST_IMAGES="$DEVICE_TEST_IMAGES config-inspector.app boot-logs.app logstore.svc"
        fi
        if [ "$TEST_NAME" = "render-launch-policy" ]; then
            DEVICE_TEST_IMAGES="$DEVICE_TEST_IMAGES logstore.svc render-denied.app render-unavailable.app"
            if [ "${CUBIT_RENDER_OPTIONAL_TEST:-0}" = 1 ]; then
                DEVICE_TEST_IMAGES="$DEVICE_TEST_IMAGES render-software.app render-fallback.app render-occupied.app render-invalid.app"
            fi
        fi
        for DEVICE_TEST_IMAGE_NAME in $DEVICE_TEST_IMAGES; do
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
    if [ "$TEST_NAME" = "config-tree" ]; then
        debugfs -w -R "rm config-inspector.app" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "write $KERNEL_DIR/isodir/boot/config-inspector.app config-inspector.app" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
    fi
    if [ "$TEST_NAME" = "clipboard" ]; then
        for CLIPBOARD_TEST_IMAGE_NAME in files.app clipboard-check.app logs.app desktop.svc; do
            CLIPBOARD_TEST_IMAGE="$KERNEL_DIR/isodir/boot/$CLIPBOARD_TEST_IMAGE_NAME"
            if [ ! -f "$CLIPBOARD_TEST_IMAGE" ]; then
                echo "headless: missing current clipboard test image: $CLIPBOARD_TEST_IMAGE" >&2
                exit 1
            fi
            debugfs -w -R "rm $CLIPBOARD_TEST_IMAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $CLIPBOARD_TEST_IMAGE $CLIPBOARD_TEST_IMAGE_NAME" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $CLIPBOARD_TEST_IMAGE_NAME" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "key-repeat" ]; then
        KEY_REPEAT_TEST_IMAGE="$KERNEL_DIR/isodir/boot/clipboard-check.app"
        if [ ! -f "$KEY_REPEAT_TEST_IMAGE" ]; then
            echo "headless: missing current key-repeat test image: $KEY_REPEAT_TEST_IMAGE" >&2
            exit 1
        fi
        debugfs -w -R "rm clipboard-check.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $KEY_REPEAT_TEST_IMAGE clipboard-check.app" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install clipboard-check.app" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "files" ]; then
        debugfs -w -R "mkdir lost+found/nested" "$TEMP_DISK" >/dev/null 2>&1
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
    if [ "$TEST_NAME" = "launch-failure" ]; then
        # A deliberately broken program: Files without its startup table,
        # as a binary linked before CAP-002 is. procmgr must refuse it and
        # Desktop must say why.
        BROKEN_IMAGE="$(mktemp "${TMPDIR:-/tmp}/cubit-launch-failure.XXXXXX")"
        if ! objcopy --remove-section .cubit.startup \
          "$KERNEL_DIR/isodir/boot/files.app" "$BROKEN_IMAGE"; then
            rm -f "$BROKEN_IMAGE"
            echo "headless: failed to make the broken Files image" >&2
            exit 1
        fi
        debugfs -w -R "rm files.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $BROKEN_IMAGE files.app" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            rm -f "$BROKEN_IMAGE"
            echo "headless: failed to install the broken files.app" >&2
            exit 1
        fi
        rm -f "$BROKEN_IMAGE"
    fi
    if [ "$TEST_NAME" = "text" ]; then
        TEXT_TEST_IMAGE="$KERNEL_DIR/isodir/boot/text.app"
        if [ ! -f "$TEXT_TEST_IMAGE" ]; then
            echo "headless: missing current Text test image: $TEXT_TEST_IMAGE" >&2
            exit 1
        fi
        debugfs -w -R "rm text.app" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "mkdir Documents" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "rm Documents/notes.txt" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $TEXT_TEST_IMAGE text.app" "$TEMP_DISK" >/dev/null 2>&1 ||
           ! debugfs -w -R "write $ROOT_DIR/tests/headless/text-notes.txt Documents/notes.txt" \
             "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install Text and its document" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "calculator" ]; then
        CALC_TEST_IMAGE="$KERNEL_DIR/isodir/boot/calculator.app"
        if [ ! -f "$CALC_TEST_IMAGE" ]; then
            echo "headless: missing current Calculator test image: $CALC_TEST_IMAGE" >&2
            exit 1
        fi
        debugfs -w -R "rm calculator.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $CALC_TEST_IMAGE calculator.app" "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install the Calculator" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "bench-ipc" ]; then
        for BENCH_IMAGE_NAME in bench-ipc-client.app bench-ipc-server.app; do
            BENCH_IMAGE="$KERNEL_DIR/isodir/boot/$BENCH_IMAGE_NAME"
            if [ ! -f "$BENCH_IMAGE" ]; then
                echo "headless: missing current benchmark image: $BENCH_IMAGE" >&2
                exit 1
            fi
            debugfs -w -R "rm $BENCH_IMAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $BENCH_IMAGE $BENCH_IMAGE_NAME" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $BENCH_IMAGE_NAME" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "ccl-remote" ]; then
        for REMOTE_IMAGE_NAME in ccl-control.app clock.svc; do
            REMOTE_IMAGE="$KERNEL_DIR/isodir/boot/$REMOTE_IMAGE_NAME"
            debugfs -w -R "rm $REMOTE_IMAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $REMOTE_IMAGE $REMOTE_IMAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $REMOTE_IMAGE_NAME" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "bench-net" ]; then
        for BENCH_NET_IMAGE in logstore.svc net-bench.app; do
            debugfs -w -R "rm $BENCH_NET_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$BENCH_NET_IMAGE $BENCH_NET_IMAGE" \
                "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $BENCH_NET_IMAGE (tests/net-bench/build-cubit.sh)" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "bench-fs" ]; then
        for BENCH_FS_IMAGE in logstore.svc fs-bench.app; do
            debugfs -w -R "rm $BENCH_FS_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$BENCH_FS_IMAGE $BENCH_FS_IMAGE" \
                "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $BENCH_FS_IMAGE (tests/fs-bench/build-cubit.sh)" >&2
                exit 1
            fi
        done
        # Journaled (ext3, data=ordered) by default, as Linux's side
        # (tests/fs-bench/linux.sh); FS_BENCH_FS=ext2 for the unjournaled one.
        if [ "${FS_BENCH_FS:-ext3}" = "ext3" ] && \
           ! dumpe2fs -h "$TEMP_DISK" 2>/dev/null | grep -q has_journal; then
            if ! tune2fs -j "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to add a journal to the bench-fs disk" >&2
                exit 1
            fi
        fi
        # The benchmark's directory and one per round are made here, on the
        # disposable copy.
        for BENCH_FS_DIR in fs-bench fs-bench/r1 fs-bench/r2 fs-bench/r3; do
            # Never mkdir over an existing name (see the Turso case).
            if [ -z "$(debugfs -R "stat $BENCH_FS_DIR" "$TEMP_DISK" 2>/dev/null)" ]; then
                debugfs -w -R "mkdir $BENCH_FS_DIR" "$TEMP_DISK" >/dev/null 2>&1
            fi
            if ! debugfs -R "stat $BENCH_FS_DIR" "$TEMP_DISK" 2>/dev/null | grep -q 'Type: directory'; then
                echo "headless: failed to make $BENCH_FS_DIR" >&2
                exit 1
            fi
        done
    fi
    case "$TEST_NAME" in
        fs-coherence-*)
            for FS_COHERENCE_IMAGE in logstore.svc "$TEST_NAME.app"; do
                debugfs -w -R "rm $FS_COHERENCE_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
                if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$FS_COHERENCE_IMAGE $FS_COHERENCE_IMAGE" \
                    "$TEMP_DISK" >/dev/null 2>&1; then
                    echo "headless: failed to install $FS_COHERENCE_IMAGE (tests/fs-coherence/build-cubit.sh)" >&2
                    exit 1
                fi
            done
            # Journaled, as bench-fs; its directory is made on the copy.
            if ! dumpe2fs -h "$TEMP_DISK" 2>/dev/null | grep -q has_journal && \
               ! tune2fs -j "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to add a journal to the fs-coherence disk" >&2
                exit 1
            fi
            if [ -z "$(debugfs -R "stat fs-coherence" "$TEMP_DISK" 2>/dev/null)" ]; then
                debugfs -w -R "mkdir fs-coherence" "$TEMP_DISK" >/dev/null 2>&1
            fi
            if ! debugfs -R "stat fs-coherence" "$TEMP_DISK" 2>/dev/null | grep -q 'Type: directory'; then
                echo "headless: failed to make fs-coherence" >&2
                exit 1
            fi
            ;;
    esac
    if [ "$TEST_NAME" = "bench-latency" ]; then
        for BENCH_LATENCY_IMAGE in logstore.svc bench-ipc-server.app sched-latency.app; do
            debugfs -w -R "rm $BENCH_LATENCY_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$BENCH_LATENCY_IMAGE $BENCH_LATENCY_IMAGE" \
                "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $BENCH_LATENCY_IMAGE (tests/sched-latency/build-cubit.sh)" >&2
                exit 1
            fi
        done
        # The io load's directory (never mkdir over an existing name).
        if [ -z "$(debugfs -R "stat sched-latency" "$TEMP_DISK" 2>/dev/null)" ]; then
            debugfs -w -R "mkdir sched-latency" "$TEMP_DISK" >/dev/null 2>&1
        fi
        if ! debugfs -R "stat sched-latency" "$TEMP_DISK" 2>/dev/null | grep -q 'Type: directory'; then
            echo "headless: failed to make sched-latency" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "net-link" ] || [ "$TEST_NAME" = "net-multi" ]; then
        NET_LINK_IMAGE="$TEST_NAME-check.app"
        debugfs -w -R "rm $NET_LINK_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$NET_LINK_IMAGE $NET_LINK_IMAGE" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install $NET_LINK_IMAGE" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "network-authority" ]; then
        NETWORK_TEST_IMAGE="$KERNEL_DIR/isodir/boot/network-check.app"
        debugfs -w -R "rm network-check.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $NETWORK_TEST_IMAGE network-check.app" "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install network-check.app" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "threads" ] || [ "$TEST_NAME" = "bench-spread" ] ||
       [ "$TEST_NAME" = "futex" ] || [ "$TEST_NAME" = "rust-std" ] ||
       [ "$TEST_NAME" = "libc" ]; then
        for THREADS_IMAGE in logstore.svc thread-check.app bench-spread.app futex-check.app \
          rust-std-hello.app libc-check.app cxx-check.app; do
            debugfs -w -R "rm $THREADS_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$THREADS_IMAGE $THREADS_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $THREADS_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "firmware-snapshot" ]; then
        # firmware-denied.app is firmware-check.app under another name: the
        # same declaration, without the startup plan's approval.
        for FIRMWARE_PAIR in logstore.svc:logstore.svc firmware-check.app:firmware-check.app \
                             firmware-check.app:firmware-denied.app \
                             firmware-undeclared.app:firmware-undeclared.app \
                             ../../../userspace/apps/acpi-standin/build/acpi-standin.app:acpi-standin.app; do
            FIRMWARE_SOURCE="$KERNEL_DIR/isodir/boot/${FIRMWARE_PAIR%%:*}"
            FIRMWARE_TARGET="${FIRMWARE_PAIR##*:}"
            if [ ! -f "$FIRMWARE_SOURCE" ]; then
                echo "headless: build ${FIRMWARE_PAIR%%:*} first (make -C kernel firmware-check firmware-undeclared acpi-standin)" >&2
                exit 1
            fi
            debugfs -w -R "rm $FIRMWARE_TARGET" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $FIRMWARE_SOURCE $FIRMWARE_TARGET" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $FIRMWARE_TARGET" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "clock" ]; then
        for CLOCK_IMAGE in logstore.svc clock-check.app; do
            debugfs -w -R "rm $CLOCK_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$CLOCK_IMAGE $CLOCK_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $CLOCK_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "scanout-repair" ]; then
        for STALL_IMAGE in logstore.svc stall-check.app; do
            debugfs -w -R "rm $STALL_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$STALL_IMAGE $STALL_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $STALL_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "process-kill" ]; then
        for KILL_IMAGE in logstore.svc clock.svc fault-check.app; do
            debugfs -w -R "rm $KILL_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$KILL_IMAGE $KILL_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $KILL_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "oom-reclaim" ]; then
        debugfs -w -R "rm oom-check.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/oom-check.app oom-check.app" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install oom-check.app" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "devmem-denials" ]; then
        for DEVMEM_IMAGE in logstore.svc clock.svc devmem-check.app; do
            debugfs -w -R "rm $DEVMEM_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$DEVMEM_IMAGE $DEVMEM_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $DEVMEM_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "avx" ]; then
        for AVX_IMAGE in logstore.svc avx-check.app; do
            debugfs -w -R "rm $AVX_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$AVX_IMAGE $AVX_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $AVX_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "control-events" ]; then
        if ! make -C "$KERNEL_DIR" user_runtime ccl-manifest >/dev/null ||
           ! bash "$ROOT_DIR/tests/control-events/build.sh" "$KERNEL_DIR/isodir/boot" >/dev/null; then
            echo "headless: failed to build the control-events test programs" >&2
            exit 1
        fi
        for CONTROL_IMAGE in logstore.svc control-check.app control-child.app control-producer.app; do
            debugfs -w -R "rm $CONTROL_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$CONTROL_IMAGE $CONTROL_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $CONTROL_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "processes" ]; then
        # The children link the current libc (crt1 builds argv from the
        # launch block); stage-1 services, procmgr included, come from initrd.
        if ! make -C "$KERNEL_DIR" libc >/dev/null ||
           ! bash "$ROOT_DIR/tests/process-spawn/build.sh" "$KERNEL_DIR/isodir/boot" >/dev/null; then
            echo "headless: failed to build the processes test programs" >&2
            exit 1
        fi
        # delegate-check hands args-check this place, which only it holds.
        printf 'delegated note\n' > "$(dirname "$SERIAL_LOG")/delegated-note.txt"
        debugfs -w -R "mkdir delegated" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "write $(dirname "$SERIAL_LOG")/delegated-note.txt delegated/note.txt" \
          "$TEMP_DISK" >/dev/null 2>&1
        DEEP_PLACE=delegated/a-directory-name-longer-than-the-old-sixty-four-byte-scope-limit
        debugfs -w -R "mkdir $DEEP_PLACE" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "write $(dirname "$SERIAL_LOG")/delegated-note.txt $DEEP_PLACE/note.txt" \
          "$TEMP_DISK" >/dev/null 2>&1
        for PROCESS_IMAGE in logstore.svc args-check.app spawn-check.app \
          ada-args-check.app rust-args-check.app greedy-check.app delegate-check.app; do
            debugfs -w -R "rm $PROCESS_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$PROCESS_IMAGE $PROCESS_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $PROCESS_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "program-parameters" ]; then
        # The programs (one declaring typed parameters, one declaring none),
        # then the image build's check of the plan's arguments against
        # their .cubit.parameters, which must refuse exactly the plan's
        # three deliberate mistakes, with their lines and columns; the plan
        # is installed anyway so procmgr's refusals are tested too.
        if ! make -C "$KERNEL_DIR" ccl-config program-parameters-test >/dev/null; then
            echo "headless: failed to build the program-parameters test programs" >&2
            exit 1
        fi
        PARAMETER_CHECK="$(dirname "$SERIAL_LOG")/program-parameters-image-check.txt"
        if "$ROOT_DIR/userspace/ccl/build/config/ccl-config" "$INIT_PROFILE" \
             --programs "$KERNEL_DIR/isodir/boot" 2> "$PARAMETER_CHECK"; then
            echo "headless: the image build accepted the plan's bad arguments" >&2
            exit 1
        fi
        for EXPECTED in \
          "line 13, column 10: parameters-check.app requires its parameters, a Check_Parameters" \
          "line 16, column 16: parameters-check.app: not a Check_Parameters: Value is outside the range of its type" \
          "line 19, column 16: parameters-plain.app declares no parameters, so it takes no arguments"; do
            if ! grep -qF "$EXPECTED" "$PARAMETER_CHECK"; then
                echo "headless: image check did not report: $EXPECTED" >&2
                cat "$PARAMETER_CHECK" >&2
                exit 1
            fi
        done
        if [ "$(wc -l < "$PARAMETER_CHECK")" -ne 3 ]; then
            echo "headless: image check reported other refusals:" >&2
            cat "$PARAMETER_CHECK" >&2
            exit 1
        fi
        echo "headless: image build check refused the plan's three bad entries"
        for PARAMETER_IMAGE in parameters-check.app parameters-plain.app; do
            debugfs -w -R "rm $PARAMETER_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$PARAMETER_IMAGE $PARAMETER_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $PARAMETER_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "binutils" ]; then
        # The tools, the launcher, the comparer, and the inputs and Linux's
        # outputs in @nvme:0/work, which the launcher holds and delegates
        # file by file through as's and ld's typed parameters.
        if ! make -C "$KERNEL_DIR" libc user_runtime ccl-manifest >/dev/null ||
           ! bash "$ROOT_DIR/tests/binutils/build.sh" "$KERNEL_DIR/isodir/boot" >/dev/null; then
            echo "headless: failed to build the binutils test" >&2
            exit 1
        fi
        for BINUTILS_IMAGE in logstore.svc as.app ld.app binutils-check.app binutils-compare.app; do
            debugfs -w -R "rm $BINUTILS_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$BINUTILS_IMAGE $BINUTILS_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
        debugfs -w -R "mkdir work" "$TEMP_DISK" >/dev/null 2>&1
        for BINUTILS_FILE in "$ROOT_DIR/tests/binutils/hello.s" \
          "$ROOT_DIR/tests/binutils/build/expected-hello.o" \
          "$ROOT_DIR/tests/binutils/build/expected-hello.elf"; do
            debugfs -w -R "write $BINUTILS_FILE work/$(basename "$BINUTILS_FILE")" \
              "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
    fi
    if [ "$TEST_NAME" = "gcc" ]; then
        # The launcher, as.app and the comparer (from the binutils test) at
        # the root; the toolchain; hello.c, its manifest and Linux's outputs
        # in @nvme:0/work.
        if ! make -C "$KERNEL_DIR" libc user_runtime ccl-manifest >/dev/null ||
           ! bash "$ROOT_DIR/tests/binutils/build.sh" "$KERNEL_DIR/isodir/boot" >/dev/null ||
           ! bash "$ROOT_DIR/tests/gcc/build.sh" "$KERNEL_DIR/isodir/boot" >/dev/null; then
            echo "headless: failed to build the gcc test" >&2
            exit 1
        fi
        for GCC_IMAGE in logstore.svc as.app gcc-check.app binutils-compare.app archive-walk.app; do
            debugfs -w -R "rm $GCC_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$GCC_IMAGE $GCC_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
        # The toolchain as installed (--prefix=/toolchain): install_gcc_toolchain.
        install_gcc_toolchain || exit 1
        GCC_COMMANDS="$TEMP_DISK.gcc-work"
        {
            echo "mkdir work"
            for GCC_OUTPUT in hello.s hello.o hello-driver.o hello bad bad.i bad.s; do
                echo "rm work/$GCC_OUTPUT"
            done
            for GCC_FILE in "$ROOT_DIR/tests/gcc/hello.c" \
              "$ROOT_DIR/tests/gcc/build/hello-manifest.s" \
              "$ROOT_DIR/tests/gcc/build/expected-hello.s" \
              "$ROOT_DIR/tests/gcc/build/expected-hello.o" \
              "$ROOT_DIR/tests/gcc/build/expected-hello-driver.o" \
              "$ROOT_DIR/tests/gcc/build/expected-hello" \
              "$ROOT_DIR/tests/ccl-console/bad.c"; do
                echo "rm work/$(basename "$GCC_FILE")"
                echo "write $GCC_FILE work/$(basename "$GCC_FILE")"
            done
        } > "$GCC_COMMANDS"
        debugfs -w -f "$GCC_COMMANDS" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        rm -f "$GCC_COMMANDS"
        for GCC_CHECK in work/expected-hello; do
            if ! debugfs -R "stat $GCC_CHECK" "$TEMP_DISK" 2>/dev/null | grep -q "Size:"; then
                echo "headless: $GCC_CHECK was not installed" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "gnat" ]; then
        # The launcher and the comparer (from the binutils test) at the
        # root; the toolchain; the Ada sources, the program's manifest and
        # Linux's outputs in @nvme:0/work.
        if ! make -C "$KERNEL_DIR" libc user_runtime ccl-manifest >/dev/null ||
           ! bash "$ROOT_DIR/tests/binutils/build.sh" "$KERNEL_DIR/isodir/boot" >/dev/null ||
           ! bash "$ROOT_DIR/tests/gnat/build.sh" "$KERNEL_DIR/isodir/boot" >/dev/null; then
            echo "headless: failed to build the gnat test" >&2
            exit 1
        fi
        for GNAT_IMAGE in logstore.svc gnat-check.app binutils-compare.app; do
            debugfs -w -R "rm $GNAT_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$GNAT_IMAGE $GNAT_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
        install_gcc_toolchain || exit 1
        GNAT_COMMANDS="$TEMP_DISK.gnat-work"
        {
            echo "mkdir work"
            for GNAT_OUTPUT in hello.o hello.ali "b~hello.adb" "b~hello.ads" "b~hello.o" \
              "b~hello.ali" hello transcript.txt bad.ali bad.o; do
                echo "rm \"work/$GNAT_OUTPUT\""
            done
            for GNAT_FILE in "$ROOT_DIR/tests/gnat/hello.adb" "$ROOT_DIR/tests/gnat/bad.adb" \
              "$ROOT_DIR/tests/gnat/build/hello-manifest.s" \
              "$ROOT_DIR/tests/gnat/build/expected-hello.o" \
              "$ROOT_DIR/tests/gnat/build/expected-b~hello.o" \
              "$ROOT_DIR/tests/gnat/build/expected-hello" \
              "$ROOT_DIR/tests/gnat/build/expected-transcript.txt"; do
                echo "rm \"work/$(basename "$GNAT_FILE")\""
                echo "write \"$GNAT_FILE\" \"work/$(basename "$GNAT_FILE")\""
            done
        } > "$GNAT_COMMANDS"
        debugfs -w -f "$GNAT_COMMANDS" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        rm -f "$GNAT_COMMANDS"
        for GNAT_CHECK in toolchain/libexec/gcc/x86_64-linux-musl/15.3.0/gnat1 \
          toolchain/lib/gcc/x86_64-linux-musl/15.3.0/adalib/libgnat.a \
          work/expected-transcript.txt "work/expected-b~hello.o"; do
            if ! debugfs -R "stat \"$GNAT_CHECK\"" "$TEMP_DISK" 2>/dev/null | grep -q "Size:"; then
                echo "headless: $GNAT_CHECK was not installed" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "libc" ]; then
        for PROTECTION_IMAGE in guard-fault.app readonly-fault.app; do
            debugfs -w -R "rm $PROTECTION_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$PROTECTION_IMAGE $PROTECTION_IMAGE" \
                "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
    fi
    if [ "$TEST_NAME" = "softpipe" ] || [ "$TEST_NAME" = "opengl" ] || [ "$TEST_NAME" = "buffer" ] || [ "$TEST_NAME" = "mesa-window" ] || [ "$TEST_NAME" = "mesa-sync" ]; then
        MESA_APP="native-$TEST_NAME.app"
        if [ "$TEST_NAME" = "mesa-sync" ]; then
            MESA_IMAGE="${MESA_SYNC_IMAGE:?build-native-sync.py output required}"
        elif [ "$TEST_NAME" = "softpipe" ]; then
            MESA_IMAGE="${SOFTPIPE_IMAGE:-$ROOT_DIR/tests/mesa-software/target/native-softpipe-cubit/$MESA_APP}"
        elif [ "$TEST_NAME" = "opengl" ]; then
            MESA_IMAGE="${OPENGL_IMAGE:-$ROOT_DIR/tests/mesa-software/target/native-softpipe-cubit/$MESA_APP}"
        elif [ "$TEST_NAME" = "mesa-window" ]; then
            MESA_IMAGE="${MESA_WINDOW_IMAGE:-$ROOT_DIR/tests/mesa-software/target/native-softpipe-cubit/$MESA_APP}"
        else
            MESA_IMAGE="${BUFFER_IMAGE:-$ROOT_DIR/tests/mesa-software/target/native-softpipe-cubit/$MESA_APP}"
        fi
        if [ ! -f "$MESA_IMAGE" ]; then
            echo "headless: build tests/mesa-software/build-native-$([ "$TEST_NAME" = softpipe ] && echo softpipe || echo opengl).sh first" >&2
            exit 1
        fi
        debugfs -w -R "rm $MESA_APP" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $MESA_IMAGE $MESA_APP" "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install $MESA_APP" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "servo" ]; then
        # Servo (85 MB) is past what 1 KiB ext2 blocks reach without
        # triple-indirect blocks (~64 MiB), which CuBit's ext2 does not
        # read: rebuild the copy with the same files, 4 KiB blocks, 512 MiB.
        # debugfs may report status0 without replacing an existing file.
        # Verify each controlled guest file by dumping its installed bytes.
        servo_install() {
            python3 "$ROOT_DIR/tests/servo/install_fixture.py" "$TEMP_DISK" "$1" "$2"
        }
        SERVO_STAGE="$(mktemp -d "${TMPDIR:-/tmp}/cubit-servo-stage.XXXXXX")"
        debugfs -R "rdump / $SERVO_STAGE" "$TEMP_DISK" >/dev/null 2>&1
        rm -rf "$SERVO_STAGE/lost+found"
        rm -f "$TEMP_DISK"
        if ! mke2fs -q -t ext2 -b 4096 -F -d "$SERVO_STAGE" "$TEMP_DISK" 512M; then
            rm -rf "$SERVO_STAGE"
            echo "headless: failed to rebuild the servo disk" >&2
            exit 1
        fi
        rm -rf "$SERVO_STAGE"
        for SERVO_IMAGE in logstore.svc clock.svc display.svc metrics.svc desktop.svc cubitshell.app; do
            debugfs -w -R "rm $SERVO_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! servo_install "$KERNEL_DIR/isodir/boot/$SERVO_IMAGE" "$SERVO_IMAGE"; then
                echo "headless: failed to install $SERVO_IMAGE" >&2
                exit 1
            fi
        done
        debugfs -w -R "mkdir fonts" "$TEMP_DISK" >/dev/null 2>&1
        SERVO_FONT_DIR="$(dirname "${IBM_PLEX_SANS_FONT:?run inside nix develop}")"
        for SERVO_FONT in IBMPlexSans-Regular.ttf IBMPlexSans-Bold.ttf \
          IBMPlexSerif-Regular.ttf IBMPlexMono-Regular.ttf; do
            if ! servo_install "$SERVO_FONT_DIR/$SERVO_FONT" "fonts/$SERVO_FONT"; then
                echo "headless: failed to install fonts/$SERVO_FONT" >&2
                exit 1
            fi
        done
        # The pages cubitshell loads: a data: page, then one the host serves
        # over HTTP (tests/servo/http_server.py) through netstack.
        SERVO_PAGES="$(mktemp "${TMPDIR:-/tmp}/cubit-servo-pages.XXXXXX")"
        cat > "$SERVO_PAGES" <<'SERVO_PAGES_EOF'
data:text/html,<body style='background:%23fff'><h1 style='color:%23135'>Servo on CuBit</h1><div style='width:200px;height:100px;background:%23e33'></div></body>
http://10.0.2.2:18470/servo-test.html
https://tls-test.cubit.internal:18460/
SERVO_PAGES_EOF
        # Opt-in extra pages, e.g. public sites (needs the host's internet):
        # SERVO_EXTRA_PAGES="https://example.com/ https://www.wikipedia.org/"
        for SERVO_EXTRA_PAGE in ${SERVO_EXTRA_PAGES:-}; do
            echo "$SERVO_EXTRA_PAGE" >> "$SERVO_PAGES"
        done
        debugfs -w -R "mkdir servo" "$TEMP_DISK" >/dev/null 2>&1
        if ! servo_install "$SERVO_PAGES" "servo/pages"; then
            rm -f "$SERVO_PAGES"
            echo "headless: failed to install servo/pages" >&2
            exit 1
        fi
        rm -f "$SERVO_PAGES"
        # HTTPS: the trust store is the development disk's root bundle plus
        # the test CA; the fixture's name maps to the host (the certificate
        # is still checked against that name).
        bash "$ROOT_DIR/tests/tls/make-pki.sh"
        SERVO_ROOTS="$(mktemp "${TMPDIR:-/tmp}/cubit-servo-roots.XXXXXX")"
        debugfs -R "dump tls/roots.der $SERVO_ROOTS" "$TEMP_DISK" >/dev/null 2>&1 || true
        cat "$ROOT_DIR/tests/tls/build/pki/ca.der" >> "$SERVO_ROOTS"
        debugfs -w -R "mkdir tls" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "rm tls/roots.der" "$TEMP_DISK" >/dev/null 2>&1
        SERVO_HOSTS="$(mktemp "${TMPDIR:-/tmp}/cubit-servo-hosts.XXXXXX")"
        echo "10.0.2.2 tls-test.cubit.internal" > "$SERVO_HOSTS"
        if ! servo_install "$SERVO_ROOTS" "tls/roots.der" ||
           ! servo_install "$SERVO_HOSTS" "servo/hosts"; then
            rm -f "$SERVO_HOSTS" "$SERVO_ROOTS"
            echo "headless: failed to install the servo trust store or hosts" >&2
            exit 1
        fi
        rm -f "$SERVO_HOSTS" "$SERVO_ROOTS"
        # Keep the historical batch page/ink oracle only in test sessions;
        # ordinary desktop launches enter the interactive loop immediately.
        SERVO_FLAG="$(mktemp "${TMPDIR:-/tmp}/cubit-servo-flag.XXXXXX")"
        if ! servo_install "$SERVO_FLAG" "servo/batch-test"; then
            echo "headless: failed to install Servo batch-test flag" >&2
            exit 1
        fi
        if [ "${SERVO_BROWSER_CHECK:-0}" = 1 ]; then
            if ! servo_install "$SERVO_FLAG" "servo/browser-check"; then
                echo "headless: failed to install Servo browser-check flag" >&2
                exit 1
            fi
        fi
        rm -f "$SERVO_FLAG"
        # Opt-in: dump each frame to the serial log (tests/servo/frame_from_log.py).
        if [ -n "${SERVO_DUMP_FRAMES:-}" ]; then
            SERVO_FLAG="$(mktemp "${TMPDIR:-/tmp}/cubit-servo-flag.XXXXXX")"
            servo_install "$SERVO_FLAG" "servo/dump-frames"
            rm -f "$SERVO_FLAG"
        fi
    fi
    if [ "$TEST_NAME" = "timesync" ]; then
        # timesync-test.svc is timesync.svc with only its capability section
        # replaced (loopback fixture ports); it is installed under the
        # production name that init-timesync.ccl starts.
        for TIMESYNC_IMAGE in logstore.svc clock.svc timesync-test.svc:timesync.svc \
          timesync-check.app; do
            TIMESYNC_SOURCE="${TIMESYNC_IMAGE%%:*}"
            TIMESYNC_TARGET="${TIMESYNC_IMAGE##*:}"
            debugfs -w -R "rm $TIMESYNC_TARGET" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$TIMESYNC_SOURCE $TIMESYNC_TARGET" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $TIMESYNC_TARGET" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "wget-https" ]; then
        for WGET_IMAGE in logstore.svc clock.svc timesync.svc tls.svc wget.app; do
            debugfs -w -R "rm $WGET_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$WGET_IMAGE $WGET_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $WGET_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "tls-service" ]; then
        # The test root is the service's trust store for this run.
        debugfs -w -R "mkdir tls" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "rm tls/roots.der" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $ROOT_DIR/tests/tls/build/pki/ca.der tls/roots.der" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install the test trust store" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "tls-probe" ] || [ "$TEST_NAME" = "tls-service" ]; then
        for TLS_IMAGE in logstore.svc clock.svc tls-probe.app tls.svc tls-check.app; do
            debugfs -w -R "rm $TLS_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$TLS_IMAGE $TLS_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $TLS_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "capability-security" ] || [ "$TEST_NAME" = "network-authority" ]; then
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
    if [ "$TEST_NAME" = "capability-security" ]; then
        for fixture in bad-phdr.app bad-segment.app bad-stack.app overlap.app; do
            fixture_path="$ROOT_DIR/tests/process-construction/build/$fixture"
            if [ ! -f "$fixture_path" ]; then
                echo "headless: build capability-test first ($fixture_path missing)" >&2
                exit 1
            fi
            debugfs -w -R "rm $fixture" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $fixture_path $fixture" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
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
    if [ "$TEST_NAME" = "config-inspection" ]; then
      TEMP_CONFIG_REJECTED="$TEMP_DISK.config-rejected.app"
      python3 "$ROOT_DIR/tests/config-inspection/make-rejected-image.py" \
        "$KERNEL_DIR/isodir/boot/config-denied.app" "$TEMP_CONFIG_REJECTED"
      debugfs -w -R "rm config-rejected.app" "$TEMP_DISK" >/dev/null 2>&1
      debugfs -w -R "write $TEMP_CONFIG_REJECTED config-rejected.app" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
      for CONFIG_IMAGE_NAME in config-check.app config-denied.app; do
        if [ ! -f "$KERNEL_DIR/isodir/boot/$CONFIG_IMAGE_NAME" ]; then
            echo "headless: build config and config-check first" >&2
            exit 1
        fi
        debugfs -w -R "rm $CONFIG_IMAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "write $KERNEL_DIR/isodir/boot/$CONFIG_IMAGE_NAME $CONFIG_IMAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
      done
      # An obsolete disk overlay must not override the declarative boot seed.
      debugfs -w -R "rm config.dat" "$TEMP_DISK" >/dev/null 2>&1
      debugfs -w -R "write $ROOT_DIR/tests/config-inspection/ignored-config.dat config.dat" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
    fi
    if [ "$TEST_NAME" = "log-authority" ]; then
        for LOG_IMAGE_NAME in logstore.svc clock.svc log-retire.app log-check.app; do
            LOG_IMAGE="$KERNEL_DIR/isodir/boot/$LOG_IMAGE_NAME"
            if [ ! -f "$LOG_IMAGE" ]; then
                echo "headless: build logstore clock log-check first" >&2
                exit 1
            fi
            debugfs -w -R "rm $LOG_IMAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $LOG_IMAGE $LOG_IMAGE_NAME" \
                "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $LOG_IMAGE_NAME" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "log-fields" ] || [ "$TEST_NAME" = "metrics" ]; then
        if [ "$TEST_NAME" = "log-fields" ]; then
            OBSERVABILITY_IMAGES="logstore.svc log-fields-check.app"
        else
            OBSERVABILITY_IMAGES="logstore.svc metrics.svc metrics-check.app"
        fi
        for OBSERVABILITY_IMAGE_NAME in $OBSERVABILITY_IMAGES; do
            OBSERVABILITY_IMAGE="$KERNEL_DIR/isodir/boot/$OBSERVABILITY_IMAGE_NAME"
            if [ ! -f "$OBSERVABILITY_IMAGE" ]; then
                echo "headless: missing $OBSERVABILITY_IMAGE_NAME (see --help)" >&2
                exit 1
            fi
            debugfs -w -R "rm $OBSERVABILITY_IMAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $OBSERVABILITY_IMAGE $OBSERVABILITY_IMAGE_NAME" \
                "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $OBSERVABILITY_IMAGE_NAME" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "storage-grants" ]; then
        debugfs -w -R "symlink nav-link lost+found" "$TEMP_DISK" >/dev/null 2>&1
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
        SCOPE_TEST_IMAGE="$KERNEL_DIR/isodir/boot/filesystem-scope-check.app"
        if [ ! -f "$SCOPE_TEST_IMAGE" ]; then
            echo "headless: build storage-check before running storage-grants" >&2
            exit 1
        fi
        debugfs -w -R "rm filesystem-scope-check.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R \
          "write $SCOPE_TEST_IMAGE filesystem-scope-check.app" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install filesystem scope check" >&2
            exit 1
        fi
        # Install an intentionally sparse file. The storage diagnostic writes
        # its first data block, exercising ext2 allocation outside group 0 as
        # well as the shared-memory transfer.
        TEMP_STORAGE_FIXTURE="$(mktemp \
          "${TMPDIR:-/tmp}/cubit-storage-sparse.XXXXXX")"
        truncate -s 8192 "$TEMP_STORAGE_FIXTURE"
        # Genuine Linux-created short/long symlinks and a shared regular inode.
        # debugfs ln does not update i_links_count; set it explicitly.
        debugfs -w -R "symlink file-link config.dat" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "symlink long-file-link deliberately-long-link-target-that-must-not-be-interpreted-as-block-pointers-or-followed" \
          "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "write $TEMP_STORAGE_FIXTURE linked-file" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "ln linked-file linked-alias" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "set_inode_field linked-file links_count 2" "$TEMP_DISK" >/dev/null 2>&1
        for SCOPE_DIR in scope-allowed scope-allowed/nested scope-allowed-other scope-work; do
            debugfs -w -R "mkdir $SCOPE_DIR" "$TEMP_DISK" >/dev/null 2>&1
        done
        for SCOPE_FILE in scope-allowed/readme scope-allowed-other/readme; do
            if ! debugfs -w -R "write $TEMP_STORAGE_FIXTURE $SCOPE_FILE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install scoped file fixture" >&2
                exit 1
            fi
        done
        # CuBit authority, not ext2 uid/gid/mode, decides app access. Keep the
        # regular-file type nibble and clear all legacy permission bits.
        debugfs -w -R "set_inode_field scope-allowed/readme mode 0100000" \
          "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "rm config.dat" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R \
          "write $TEMP_STORAGE_FIXTURE config.dat" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install storage fixture" >&2
            exit 1
        fi
        # Seed a separate real Linux double-indirect mapping for in-place I/O.
        if ! python3 "$ROOT_DIR/tests/filesystem-interop/seed-double.py" \
          "$TEMP_DISK" "$TEMP_STORAGE_FIXTURE"; then
            echo "headless: failed to prepare double-indirect overwrite fixture" >&2
            exit 1
        fi
        # Create a directory that resolves normally, then corrupt the first
        # ext2 dirent's record-length field. The service must reject the page
        # without reading a variable-length name or advancing its cursor.
        debugfs -w -R "mkdir corrupt-dir" "$TEMP_DISK" >/dev/null 2>&1
        # An htree-flagged directory is writable (the flag is cleared before
        # a change); a directory with a layout CuBit does not write (extents)
        # must be refused unchanged. Flagged fixtures, not real trees.
        debugfs -w -R "mkdir indexed-dir" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "set_inode_field indexed-dir flags 0x1000" \
          "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "mkdir extents-dir" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "set_inode_field extents-dir flags 0x80000" \
          "$TEMP_DISK" >/dev/null 2>&1
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
if { [ "$TEST_NAME" = desktop-display ] || [ "$TEST_NAME" = calculator ]; } && [ -n "${CUBIT_TEST_VIDEO_SIZE:-}" ]; then
    # The firmware mode Desktop starts at (VIDEO_ARGS below sizes the VGA).
    sed -i "s/set gfxpayload=1024x768x32/set gfxpayload=${VIDEO_WIDTH}x${VIDEO_HEIGHT}x32/" "$GRUB_CFG"
fi
# Focused runs must not silently exercise a kernel predating source edits.
# Compile/check the kernel, stage it, and regenerate the stage-1 archive
# from the currently staged services before rebuilding the test ISO. Without
# this, a focused driver build can appear to pass while QEMU boots an older
# copy from initrd.img.
if ! make -C "$KERNEL_DIR" cubit_kernel >/dev/null; then
    echo "headless: kernel build failed" >&2
    exit 1
fi
cp "$KERNEL_DIR/cubit_kernel" "$KERNEL_DIR/isodir/boot/cubit_kernel"
if ! make -C "$KERNEL_DIR" initrd >/dev/null; then
    echo "headless: failed to refresh stage-1 initrd" >&2
    exit 1
fi
if [ "$TEST_NAME" = "storage-grants" ]; then
    if ! make -C "$KERNEL_DIR" laptop_live_rw.img >/dev/null ||
       ! python3 "$ROOT_DIR/userspace/ccl/tools/ccl-image/realize.py" \
          "$ROOT_DIR/tests/headless/storage-initrd.ccl" \
          --output "$KERNEL_DIR/isodir/boot/initrd.img" >/dev/null; then
        echo "headless: failed to prepare volatile block-storage fixture" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "tls-service" ]; then
    # Development initrd whose tls.hosts maps the fixture name to the host.
    if ! python3 "$ROOT_DIR/userspace/ccl/tools/ccl-image/realize.py" \
          "$ROOT_DIR/tests/tls/tls-initrd.ccl" \
          --output "$KERNEL_DIR/isodir/boot/initrd.img" >/dev/null; then
        echo "headless: failed to prepare tls-service settings fixture" >&2
        exit 1
    fi
fi
if [ "$CONFIG_STORAGE_TEST" = 1 ]; then
    if ! python3 "$ROOT_DIR/userspace/ccl/tools/ccl-image/realize.py" \
          "$ROOT_DIR/tests/config-turso/native/storage-initrd.ccl" \
          --output "$KERNEL_DIR/isodir/boot/initrd.img" >/dev/null; then
        echo "headless: failed to prepare Config storage settings fixture" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "timesync" ]; then
    # Development initrd whose settings point time.servers at the fixture.
    if ! python3 "$ROOT_DIR/userspace/ccl/tools/ccl-image/realize.py" \
          "$ROOT_DIR/tests/timesync/timesync-initrd.ccl" \
          --output "$KERNEL_DIR/isodir/boot/initrd.img" >/dev/null; then
        echo "headless: failed to prepare timesync settings fixture" >&2
        exit 1
    fi
fi
if ! grub-mkrescue -o "$KERNEL_DIR/cubit_kernel.iso" "$KERNEL_DIR/isodir" >/dev/null 2>&1; then
    echo "headless: grub-mkrescue failed" >&2
    exit 1
fi
cp "$GRUB_BAK" "$GRUB_CFG"

VIDEO_ARGS=(-device virtio-gpu-pci)
# CUBIT_TEST_VIDEO_SIZE=WxH: desktop-display at another firmware mode, the
# way a machine without modesetting runs (GOP / VBE linear framebuffer).
# The secondary virtio-gpu-pci never drives the desktop (display: "gpu not
# primary, using linear-fb"), so the size goes to GRUB's gfxpayload (set
# above) on the primary std VGA, with video memory for the mode. SeaBIOS
# VBE lists 1920x1080, 2560x1440, 2560x1600 and 3840x2160 among others.
if { [ "$TEST_NAME" = desktop-display ] || [ "$TEST_NAME" = calculator ]; } && [ -n "${CUBIT_TEST_VIDEO_SIZE:-}" ]; then
    VGA_MEMORY_MB=16
    while [ $((VGA_MEMORY_MB * 1024 * 1024)) -lt $((VIDEO_WIDTH * VIDEO_HEIGHT * 4)) ]; do
        VGA_MEMORY_MB=$((VGA_MEMORY_MB * 2))
    done
    VIDEO_ARGS=(-vga none -device "VGA,vgamem_mb=$VGA_MEMORY_MB" -device virtio-gpu-pci)
fi
if [ "$TEST_NAME" = bench-input ] && [ "$INPUT_BACKEND" = virtio ]; then
    VIDEO_ARGS=(-vga none -device virtio-vga,xres=1024,yres=768)
fi
if [ "$TEST_NAME" = "display-discovery-boot-only" ]; then
    VIDEO_ARGS=()
fi
if [ "$TEST_NAME" = "virtio-vga-primary" ] ||
   [ "$TEST_NAME" = "config-tree" ] ||
   [ "$TEST_NAME" = "display-grants-virtio-vga" ] ||
   [ "$TEST_NAME" = "ccl-workspace" ] || [ "$TEST_NAME" = "ccl-console" ] ||
   [ "$TEST_NAME" = "logs" ] ||
   [ "$TEST_NAME" = "ccl-workbench-virtio-vga" ] ||
   [ "$TEST_NAME" = "desktop-virtio-vga" ] ||
   [ "$TEST_NAME" = "desktop-doom" ]; then
    VIDEO_ARGS=(-vga none -device virtio-vga,xres=1024,yres=768)
fi
if [ "$TEST_NAME" = "desktop-vm" ]; then
    # run-desktop's devices: virtio-vga plus a virtio tablet (absolute
    # pointer). VNC carries host resizes (SetDesktopSize, as a resized GTK
    # window does); QEMU's cursor trace shows the host cursor plane.
    VM_VNC_SOCKET="$(qemu_socket vnc)"
    VM_TRACE_LOG="${SERIAL_LOG}.cursor-trace.log"
    rm -f "$VM_VNC_SOCKET" "$VM_TRACE_LOG"
    VIDEO_ARGS=(-vga none -device virtio-vga,xres=1024,yres=768
                -device virtio-tablet-pci
                -vnc "unix:$VM_VNC_SOCKET"
                -D "$VM_TRACE_LOG" -trace enable=virtio_gpu_update_cursor)
fi
if [ "$TEST_NAME" = "virtio-gpu-multi-output" ] ||
   [ "$TEST_NAME" = "display-discovery-multi-output" ]; then
    # QEMU 11.1 supports per-head initial modes. These are native mode hints;
    # CuBit owns UI scale, desktop placement and content rotation separately.
    # The current guest presents only on head zero: this tests discovery.
    VIDEO_ARGS=(-device '{"driver":"virtio-gpu-pci","id":"multi-gpu","max_outputs":3,"outputs":[{"name":"CuBit Main","xres":1024,"yres":768},{"name":"CuBit Wide","xres":1920,"yres":1080},{"name":"CuBit Tall","xres":1080,"yres":1920}]}')
fi

if [ "$TEST_NAME" = "display-dual-output" ] || [ "$TEST_NAME" = "desktop-dual-output" ]; then
    VIDEO_ARGS=(-vga none -device '{"driver":"virtio-vga","id":"multi-gpu","max_outputs":2,"outputs":[{"name":"CuBit Main","xres":1024,"yres":768},{"name":"CuBit Side","xres":1024,"yres":768}]}')
    if [ "$TEST_NAME" = "desktop-dual-output" ] && [ "${CUBIT_TEST_MIXED_OUTPUTS:-0}" = 1 ]; then
        VIDEO_ARGS=(-vga none -device '{"driver":"virtio-vga","id":"multi-gpu","max_outputs":2,"outputs":[{"name":"CuBit Main","xres":1024,"yres":768},{"name":"CuBit Wide","xres":1280,"yres":720}]}')
    fi
    if [ "$TEST_NAME" = "desktop-dual-output" ] && [ "${CUBIT_TEST_BOOT_HANDOFF:-0}" = 1 ]; then
        VIDEO_ARGS=(-vga none -device '{"driver":"virtio-vga","id":"multi-gpu","max_outputs":2,"outputs":[{"name":"CuBit Wide","xres":1280,"yres":720},{"name":"CuBit Side","xres":1024,"yres":768}]}')
    fi
fi

echo "headless: running $TEST_NAME for ${TIMEOUT_SECONDS}s"
echo "headless: cpus=$QEMU_CPUS accel=${QEMU_ACCEL:-qemu-default} busy_peer=$BENCH_LOAD"
echo "headless: display=$QEMU_DISPLAY"
if [ "$BENCH_LOAD" = 1 ]; then echo "headless: load_workers=$LOAD_WORKERS"; fi
"$QEMU_BIN" --version | head -1

ACCEL_ARGS=()
if [ -n "$QEMU_ACCEL" ]; then
    ACCEL_ARGS=(-accel "$QEMU_ACCEL")
fi

AUDIO_ARGS=(-audiodev none,id=snd0)
# Devices only some tests add: QEMU's edu device binds the generic launch's
# fixture driver (HW-002); without it the driver stays unbound.
TEST_DEVICE_ARGS=()
if [ "$TEST_NAME" = "generic-launch" ]; then
    TEST_DEVICE_ARGS=(-device edu)
fi
# Two xHCI controllers, a USB keyboard on one and a USB optical drive on
# the other (a disc with the apps/ tree @cd:0 mounts, holding one file),
# and a second NVMe disk with an empty ext2 volume: each gets its own
# driver instance.
if [ "$TEST_NAME" = "multi-instance" ]; then
    TEMP_SECOND_NVME="$(mktemp "$(dirname "$SERIAL_LOG")/cubit-${TEST_NAME}-nvme1.XXXXXX.img")"
    if ! mke2fs -q -t ext2 -F "$TEMP_SECOND_NVME" 16M; then
        echo "headless: failed to create the second NVMe disk" >&2
        exit 1
    fi
    TEMP_OPTICAL_TREE="$(mktemp -d "$(dirname "$SERIAL_LOG")/cubit-${TEST_NAME}-disc.XXXXXX")"
    mkdir -p "$TEMP_OPTICAL_TREE/tree/apps"
    echo "multi-instance optical disc" > "$TEMP_OPTICAL_TREE/tree/apps/readme.txt"
    if ! xorriso -as mkisofs -quiet -V CUBITMULTI -o "$TEMP_OPTICAL_TREE/disc.iso" \
      "$TEMP_OPTICAL_TREE/tree"; then
        echo "headless: failed to create the optical disc image" >&2
        exit 1
    fi
    TEST_DEVICE_ARGS=(
        -device qemu-xhci,id=xhcikbd
        -device usb-kbd,bus=xhcikbd.0,port=1
        -device qemu-xhci,id=xhcicd
        -drive "file=$TEMP_OPTICAL_TREE/disc.iso,if=none,id=usbcd,format=raw,media=cdrom,readonly=on"
        -device usb-bot,id=usbbot,bus=xhcicd.0,port=1
        -device scsi-cd,bus=usbbot.0,lun=0,drive=usbcd
        -drive "file=$TEMP_SECOND_NVME,if=none,id=nvme1,format=raw"
        -device nvme,serial=cubitnvme1,drive=nvme1)
fi
# usb-hotplug: two controllers, the boot mouse on the second; the driver
# script hot-plugs devices through the HMP monitor (tests/usb-hotplug).
if [ "$TEST_NAME" = "usb-hotplug" ]; then
    TEST_DEVICE_ARGS=(
        -device qemu-xhci,id=xhcia
        -device qemu-xhci,id=xhcib
        -device usb-mouse,bus=xhcib.0,port=1,id=bootmouse)
fi
if [ "$TEST_NAME" = "desktop-doom" ] || [ "$TEST_NAME" = "bench-audio" ]; then
    TEMP_AUDIO="$(mktemp "$(dirname "$SERIAL_LOG")/cubit-${TEST_NAME}-audio.XXXXXX.wav")"
    AUDIO_ARGS=(-audiodev "wav,id=snd0,path=$TEMP_AUDIO")
fi

MONITOR_ARGS=()
QMP_ARGS=()
if [ "$TEST_NAME" = "managed-ui" ]; then
    MONITOR_SOCKET="$(qemu_socket monitor)"
    rm -f "$MONITOR_SOCKET"
    MONITOR_ARGS=(-monitor "unix:$MONITOR_SOCKET,server,nowait")
fi
if [ "$TEST_NAME" = "desktop-display" ] || [ "$TEST_NAME" = "files" ] || [ "$TEST_NAME" = "launch-failure" ] || [ "$TEST_NAME" = "text" ] || [ "$TEST_NAME" = "calculator" ] || [ "$TEST_NAME" = "clipboard" ] || [ "$TEST_NAME" = "mesa-window" ] || [ "$TEST_NAME" = "gpu-viewer" ] ||
   [ "$TEST_NAME" = "desktop-protocol" ] ||
   [ "$TEST_NAME" = "ccl-workspace" ] || [ "$TEST_NAME" = "ccl-console" ] ||
   [ "$TEST_NAME" = "lexicon" ] || [ "$TEST_NAME" = "dictionary" ] ||
   [ "$TEST_NAME" = "logs" ] || [ "$TEST_NAME" = "scanout-repair" ] ||
   [ "$TEST_NAME" = "desktop-doom" ] || [ "$TEST_NAME" = "boot-timeline" ] ||
   { [ "$TEST_NAME" = "servo" ] && [ -n "${SERVO_DESKTOP:-}" ]; }; then
    if ! command -v nc >/dev/null 2>&1; then
        echo "headless: desktop input regression requires nc" >&2
        exit 127
    fi

    MONITOR_SOCKET="$(qemu_socket monitor)"
    rm -f "$MONITOR_SOCKET"
    MONITOR_ARGS=(-monitor "unix:$MONITOR_SOCKET,server,nowait")
    if [ "$TEST_NAME" = "files" ]; then
        QMP_SOCKET="$(qemu_socket qmp)"
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

        if [ "$TEST_NAME" = "boot-timeline" ]; then
            # One key as soon as the PS/2 driver serves its consumer: the
            # timeline's First_Input_Accepted is when Desktop takes it.
            printf 'sendkey shift\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            exit 0
        fi
        if [ "$TEST_NAME" = "scanout-repair" ]; then
            # The kernel erases its stuck-call panel one report after
            # stall-check's call ends; then one pointer move makes Desktop
            # present, and Display must copy the whole frame.
            for ((attempt = 0; attempt < 400; attempt++)); do
                grep -aF "stall-check: done" "$SERIAL_LOG" >/dev/null 2>&1 && break
                sleep 0.1
            done
            if ! grep -aF "stall-check: done" "$SERIAL_LOG" >/dev/null 2>&1; then
                echo "headless: stall-check did not finish" >&2
                exit 1
            fi
            printf 'mouse_move 8 8\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            sleep 1
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-repaired.ppm" |
                nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            sleep 0.5
            python3 "$ROOT_DIR/tests/scanout-repair/check_band.py" \
                "${SERIAL_LOG%.log}-repaired.ppm" || exit 1
            exit 0
        fi
        if [ "$TEST_NAME" = "gpu-viewer" ]; then
            attached=0
            for ((attempt = 0; attempt < 250; attempt++)); do
                if grep -aF "gpu-viewer: completed target attached" "$SERIAL_LOG" >/dev/null 2>&1; then
                    attached=1
                    break
                fi
                sleep 0.1
            done
            if [ "$attached" != 1 ]; then
                echo "headless: GPU viewer did not attach" >&2
                exit 1
            fi
            sleep 1
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-viewer.ppm" |
                nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            python3 "$ROOT_DIR/tests/gpu-viewer/check_pixels.py" \
                "${SERIAL_LOG%.log}-viewer.ppm" || exit 1
            printf 'sendkey esc\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            exit 0
        fi
        if [ "$TEST_NAME" = "mesa-window" ]; then
            for ((attempt = 0; attempt < 200; attempt++)); do
                grep -aF "MESA-WINDOW: attached immutable Mesa buffer" "$SERIAL_LOG" >/dev/null 2>&1 && break
                sleep 0.1
            done
            # Present acknowledges scheduling, not scanout. Wait for the exact
            # final pixels instead of assuming a fixed TCG rendering duration.
            # This is a bounded correctness wait, never a latency measurement.
            case "${MESA_WINDOW_SCENE:-quads}" in
                quads) pixel_checker="$ROOT_DIR/tests/mesa-software/check-window.py" ;;
                *) exit 2 ;;
            esac
            pixel_deadline=$((SECONDS + 20))
            while :; do
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-mesa.ppm" |
                    nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
                if python3 "$pixel_checker" "${SERIAL_LOG%.log}-mesa.ppm" \
                    >"${SERIAL_LOG%.log}-mesa-pixels.log" 2>&1; then
                    break
                fi
                if ((SECONDS >= pixel_deadline)); then
                    cat "${SERIAL_LOG%.log}-mesa-pixels.log" >&2
                    exit 1
                fi
                sleep 0.1
            done
            if [ "${MESA_WINDOW_ANIMATION:-0}" = 1 ]; then
                printf 'sendkey spc\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
                for ((attempt = 0; attempt < MESA_WINDOW_ANIMATION_WAIT_SECONDS * 10; attempt++)); do
                    grep -aF "MESA-WINDOW: PASS animated cycle" "$SERIAL_LOG" >/dev/null 2>&1 && break
                    sleep 0.1
                done
                printf 'sendkey spc\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
                sleep 0.5
            fi
            printf 'sendkey esc\n' |
                nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            if [ "${CUBIT_TEST_TARGET_RETIREMENT:-0}" = 1 ]; then
                for ((attempt = 0; attempt < 100; attempt++)); do
                    grep -aF 'MESA-WINDOW: Escape; exiting' "$SERIAL_LOG" >/dev/null 2>&1 && break
                    sleep 0.1
                done
                # Once the client is gone, Q exits the otherwise idle Desktop
                # and exercises its real imported-target retirement boundary.
                printf 'sendkey q\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            fi
            exit 0
        fi
        if [ "$TEST_NAME" = "launch-failure" ]; then
            # Launch Files from Apps: Tools is the fifth category row
            # (System, Development, Web, Games, Tools), Files its first
            # entry. Photograph the notice (the Deskbar's toast), dismiss it
            # with Escape, then photograph it in the Deskbar panel's history
            # (Super+N) and close the panel.
            for ((attempt = 0; attempt < 300; attempt++)); do
                grep -F "desktop: active outputs=" "$SERIAL_LOG" >/dev/null 2>&1 && break
                sleep 0.1
            done
            sleep 2
            {
                printf 'sendkey meta_l\n'
                sleep 0.5
                for key in down down down down right ret; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.3
                done
            } | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            for ((attempt = 0; attempt < 300; attempt++)); do
                grep -aF "desktop: launch failed:" "$SERIAL_LOG" >/dev/null 2>&1 && break
                sleep 0.1
            done
            sleep 1
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-notice.ppm" |
                nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            sleep 1
            printf 'sendkey esc\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            sleep 0.5
            printf 'sendkey meta_l-n\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            sleep 1
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-history.ppm" |
                nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            sleep 1
            printf 'sendkey esc\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            exit 0
        fi
        if [ "$TEST_NAME" = "servo" ] && [ -n "${SERVO_DESKTOP:-}" ]; then
            # Launch Servo (Penny) from the Apps menu once the desktop is up:
            # the Web category is the third row (system.ccl: System,
            # Development, Web, Games, Tools), its first entry. Then
            # photograph the window after the first page renders.
            for ((attempt = 0; attempt < 300; attempt++)); do
                grep -F "desktop: active outputs=" "$SERIAL_LOG" >/dev/null 2>&1 && break
                sleep 0.1
            done
            sleep 2
            {
                printf 'sendkey meta_l\n'
                sleep 0.5
                for key in down down right ret; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.3
                done
            } | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            for ((attempt = 0; attempt < 1200; attempt++)); do
                grep -aF "CUBITSHELL: PASS" "$SERIAL_LOG" >/dev/null 2>&1 && break
                if [ "${SERVO_BROWSER_CHECK:-0}" = 1 ] &&
                   grep -aE 'USER-MEMORY-FAULT:|CUBITSHELL: FAIL|CUBITSHELL: panic' "$SERIAL_LOG" >/dev/null 2>&1; then
                    break
                fi
                sleep 0.1
            done
            if [ "${SERVO_BROWSER_CHECK:-0}" = 1 ]; then
                python3 "$ROOT_DIR/tests/servo/browser_input.py" "$MONITOR_SOCKET" "$SERIAL_LOG" "${SERIAL_LOG%.log}-browser.ppm"
            fi
            sleep 3
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-servo.ppm" |
                nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            sleep 1
        fi
        if [ "$TEST_NAME" = "desktop-protocol" ]; then
            protocol_ready=0
            for ((attempt = 0; attempt < 150; attempt++)); do
                if grep -F "DESKTOP-PROTOCOL-CHECK: PASS" "$SERIAL_LOG" >/dev/null 2>&1; then
                    protocol_ready=1
                    break
                fi
                sleep 0.1
            done
            if [ "$protocol_ready" -ne 1 ]; then
                echo "headless: desktop grant adversary did not complete" >&2
                exit 1
            fi
            # Reaping currently runs on scene composition, not a death event.
            # Open/close Apps after the adversary exits to exercise that path.
            {
                sleep 0.3
                printf 'sendkey meta_l\n'
                sleep 0.3
                printf 'sendkey esc\n'
            } | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
        elif [ "$TEST_NAME" = "desktop-doom" ]; then
            move_pointer() {
                local dx=$1 dy=$2 sx sy
                while [ "$dx" -ne 0 ] || [ "$dy" -ne 0 ]; do
                    sx=$dx; sy=$dy
                    if [ "$sx" -gt 80 ]; then sx=80; fi
                    if [ "$sx" -lt -80 ]; then sx=-80; fi
                    if [ "$sy" -gt 80 ]; then sy=80; fi
                    if [ "$sy" -lt -80 ]; then sy=-80; fi
                    printf 'mouse_move %s %s\n' "$sx" "$sy"
                    dx=$((dx-sx)); dy=$((dy-sy))
                    sleep 0.05
                done
                sleep 0.3
            }
            if [ "${CUBIT_DOOM_MULTIAPP:-0}" = 1 ]; then
                {
                    # Workbench: Development (second row), first entry.
                    printf 'sendkey meta_l\n'
                    sleep 0.3
                    printf 'sendkey down\n'
                    sleep 0.2
                    printf 'sendkey right\n'
                    sleep 0.2
                    printf 'sendkey ret\n'
                    sleep 4
                    printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-workbench.ppm"
                    # Close the native client before reusing its desktop slot.
                    move_pointer 910 14
                    sleep 0.3
                    printf 'mouse_button 1\n'
                    sleep 0.1
                    printf 'mouse_button 0\n'
                    sleep 1
                    move_pointer -910 -14
                } | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            fi
            # Launch through Apps: boot-time spawning bypasses compositor
            # bookkeeping and cannot cover interactive-launch regressions.
            # DOOM is the first entry of Games, the fourth category row
            # (system.ccl: System, Development, Web, Games, Tools).
            {
                printf 'sendkey meta_l\n'
                sleep 0.3
                for doom_entry_step in 1 2 3; do
                    printf 'sendkey down\n'
                    sleep 0.2
                done
                printf 'sendkey right\n'
                sleep 0.2
                printf 'sendkey ret\n'
            } | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            doom_ready=0
            for ((attempt = 0; attempt < 100; attempt++)); do
                if grep -F "DOOM: Running inside desktop surface" \
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
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-game.ppm"
                printf 'sendkey meta_l\n'
                sleep 0.5
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-apps.ppm"
                printf 'sendkey esc\n'
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
        elif [ "$TEST_NAME" = "ccl-workspace" ]; then
            workbench_ready=0
            for ((attempt = 0; attempt < 150; attempt++)); do
                if grep -F "ccl-workbench: first frame presented" "$SERIAL_LOG" >/dev/null; then
                    workbench_ready=1
                    break
                fi
                sleep 0.1
            done
            if [ "$workbench_ready" -ne 1 ]; then
                echo "headless: Workbench input target did not appear" >&2
                exit 1
            fi
            {
                # Default live-clock source: wake twice on a deadline without
                # input, then stop via real keyboard input during a timed wait.
                printf 'sendkey f7\n'
                sleep 2.5
                if [ "$KEEP_LOGS" -eq 1 ]; then
                    printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-live-label.ppm"
                fi
                printf 'sendkey f7\n'
                sleep 0.3
                printf 'sendkey ctrl-a\n'
                sleep 0.2
                printf 'sendkey backspace\n'
                sleep 0.2
                printf 'sendkey 4\n'
                sleep 0.2
                printf 'sendkey 1\n'
                sleep 0.2
                printf 'sendkey ctrl-s\n'
                sleep 0.5
                printf 'sendkey ret\n'
                sleep 0.7
                printf 'sendkey ctrl-a\n'
                sleep 0.2
                printf 'sendkey backspace\n'
                sleep 0.2
                printf 'sendkey 4\n'
                sleep 0.2
                printf 'sendkey 2\n'
                sleep 0.2
                # Open now prompts Save/Discard/Cancel. Cancel must preserve
                # the edits before we explicitly save revision 2.
                printf 'sendkey ctrl-o\n'
                sleep 0.4
                printf 'sendkey esc\n'
                sleep 0.2
                printf 'sendkey ctrl-s\n'
                sleep 0.5
                printf 'sendkey ret\n'
                sleep 0.7
                printf 'sendkey ctrl-o\n'
                sleep 0.5
                printf 'sendkey down\n'
                sleep 0.2
                printf 'sendkey ret\n'
                sleep 0.5
                # Save the loaded source under a human-chosen name.
                printf 'sendkey ctrl-s\n'
                sleep 0.5
                for key in c l o c k dot c c l; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.15
                done
                printf 'sendkey ret\n'
                sleep 0.6
                # Choosing an existing file in Save must not overwrite it.
                printf 'sendkey ctrl-s\n'
                sleep 0.5
                printf 'sendkey up\n'
                sleep 0.2
                printf 'sendkey ret\n'
                sleep 0.4
                printf 'sendkey esc\n'
                sleep 0.2
                printf 'sendkey f5\n'
                sleep 0.5
                printf 'sendkey f6\n'
                sleep 0.3
                # BASIC in the REPL: 40 + 2 (the session reads either dialect).
                for key in 4 0 spc shift-equal spc 2; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.15
                done
                printf 'sendkey ret\n'
                sleep 0.3
                # The REPL log viewer: (>= (length (logs.recent "desktop")) 0)
                # proves observer authority, logstore's source filter and a
                # typed List<LogEntry> crossing into the session.
                for key in shift-9 shift-dot equal spc shift-9 l e n g t h spc shift-9 \
                           l o g s dot r e c e n t spc shift-apostrophe d e s k t o p \
                           shift-apostrophe shift-0 shift-0 spc 0 shift-0; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.15
                done
                printf 'sendkey ret\n'
                sleep 0.6
                printf 'sendkey up\n'
                sleep 0.15
                printf 'sendkey down\n'
                sleep 0.15
                printf 'sendkey f6\n'
                sleep 0.3
                # Exercise composed punctuation through the real desktop text
                # path, then verify the exact saved bytes (including quotes).
                printf 'sendkey ctrl-a\n'
                sleep 0.2
                printf 'sendkey backspace\n'
                sleep 0.2
                for key in shift-apostrophe h e l l o shift-1 shift-1 shift-apostrophe; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.15
                done
                printf 'sendkey ctrl-s\n'
                sleep 0.5
                for key in q u o t e d dot c c l; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.15
                done
                printf 'sendkey ret\n'
                sleep 0.7
                printf 'sendkey f6\n'
                sleep 0.3
                for key in shift-apostrophe h e l l o shift-1 shift-1 shift-apostrophe; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.15
                done
                printf 'sendkey ret\n'
                if [ "${CUBIT_DESKTOP_METRICS_LOAD:-0}" = 1 ]; then
                    metrics_baseline_ready=0
                    for ((attempt = 0; attempt < 600; attempt++)); do
                        if grep -F 'TEST: metrics-load baseline frames=' "$SERIAL_LOG" >/dev/null; then
                            metrics_baseline_ready=1
                            break
                        fi
                        sleep 0.1
                    done
                    if [ "$metrics_baseline_ready" -ne 1 ]; then
                        echo "headless: metrics load observer did not establish baseline" >&2
                        exit 1
                    fi
                    # Explicit post-load Desktop redraws. A static desktop
                    # must not render continuously just to satisfy telemetry.
                    for ((repaint = 0; repaint < 3; repaint++)); do
                        printf 'sendkey meta_l\n'
                        sleep 0.5
                        printf 'sendkey esc\n'
                        sleep 0.5
                    done
                fi
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
        elif [ "$TEST_NAME" = "logs" ]; then
            # The Logs app: let boot records arrive, then a live frame, a
            # Warning-and-above frame (4) and a search for logstore.
            for ((attempt = 0; attempt < 150; attempt++)); do
                grep -F "logs: window ready" "$SERIAL_LOG" >/dev/null && break
                sleep 0.1
            done
            {
                sleep 4
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-logs-live.ppm"
                sleep 0.5
                printf 'sendkey 4\n'
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-logs-warnings.ppm"
                sleep 0.5
                printf 'sendkey 1\n'
                sleep 0.3
                for key in slash l o g s t o r e; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.3
                done
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-logs-search.ppm"
                sleep 0.5
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
        elif [ "$TEST_NAME" = "ccl-console" ]; then
            console_ready=0
            for ((attempt = 0; attempt < 150; attempt++)); do
                if grep -F "ccl-console: first frame presented" "$SERIAL_LOG" >/dev/null; then
                    console_ready=1
                    break
                fi
                sleep 0.1
            done
            if [ "$console_ready" -ne 1 ]; then
                echo "headless: console input target did not appear" >&2
                exit 1
            fi
            type_keys() {
                for key in "$@"; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.3
                done
            }
            if [ "${CCL_CONSOLE_DEMO:-}" = gcc ]; then
            # gcc from the console (CCL_CONSOLE_DEMO=gcc): build hello (cc1, as
            # and ld, started by the driver), watch the run, see it end; then a
            # compile error, cc1's diagnostic forwarded by the driver.
            send_text() {
                python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" "$1"
            }
            # Ask for a run's outcome until it is done (TCG: minutes, not seconds).
            wait_done() {
                local name=$1 before tries=0
                while [ $tries -lt 60 ]; do
                    before=$(grep -ac "Task<Run_Outcome>: Done" "$SERIAL_LOG" 2>/dev/null || true)
                    send_text "(gcc.outcome $name)"
                    sleep 5
                    if [ "$(grep -ac "Task<Run_Outcome>: Done" "$SERIAL_LOG" 2>/dev/null || true)" -gt "$before" ]; then
                        return 0
                    fi
                    tries=$((tries + 1))
                done
                return 1
            }
            sleep 0.5
            send_text '(define hello (gcc.run (Gcc_Parameters output => (Output_File "@nvme:0/work/hello") sources => [(Input_File "@nvme:0/work/hello.c") (Input_File "@nvme:0/work/hello-manifest.s")] work => (Output_Directory "@nvme:0/work") toolchain => (Input_Directory "@nvme:0/toolchain") optimize => true)))'
            sleep 4
            send_text 'hello'
            sleep 2
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-gcc-running.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            wait_done hello || echo "headless: gcc demo: hello did not finish" >&2
            sleep 1
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-gcc.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            send_text '(define bad (gcc.run (Gcc_Parameters output => (Output_File "@nvme:0/work/bad") sources => [(Input_File "@nvme:0/work/bad.c")] work => (Output_Directory "@nvme:0/work") toolchain => (Input_Directory "@nvme:0/toolchain"))))'
            sleep 4
            wait_done bad || echo "headless: gcc demo: bad did not finish" >&2
            send_text '(gcc.outlets bad)'
            sleep 2
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-gcc-error.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.5
            elif [ "${CCL_CONSOLE_DEMO:-}" = programs ]; then
            # Typed programs (CCL_CONSOLE_DEMO=programs): as on a bad source,
            # its unix.stderr port and its exit; then ld on a good object.
            send_text() {
                python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" "$1"
            }
            sleep 0.5
            send_text '(define bad (as.run (As_Parameters output => (Output_File "@nvme:0/work/bad.o") source => (Input_File "@nvme:0/work/bad.s"))))'
            sleep 3
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-programs-as.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            send_text '(define good (as.run (As_Parameters output => (Output_File "@nvme:0/work/hello.o") source => (Input_File "@nvme:0/work/hello.s"))))'
            sleep 3
            send_text '(define linked (ld.run (Ld_Parameters output => (Output_File "@nvme:0/work/hello") inputs => [(Input_File "@nvme:0/work/hello.o")] static => true)))'
            sleep 3
            send_text '(ld.outlets linked)'
            sleep 2
            # Looking at a run's outcome never waits: these have ended.
            send_text '(as.outcome bad)'
            sleep 1
            send_text '(ld.outcome linked)'
            sleep 1
            send_text '(ld.run (Ld_Parameters output => (Input_File "@nvme:0/work/x") inputs => []))'
            sleep 2
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-programs.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.5
            elif [ "${CCL_CONSOLE_DEMO:-}" = ps ]; then
            # What runs on the guest (CCL_CONSOLE_DEMO=ps): :ps, then the
            # largest by memory.
            {
                sleep 0.5
                type_keys shift-semicolon p s
                printf 'sendkey ret\n'
                sleep 2
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-ps.ppm"
                sleep 0.5
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            elif [ "${CCL_CONSOLE_DEMO:-}" = errors ]; then
            # Refusals on the guest (CCL_CONSOLE_DEMO=errors): a place outside the
            # console's filesystem scope, a mistyped service, a bad name.
            {
                sleep 0.5
                type_keys shift-semicolon c d
                printf 'sendkey ret\n'
                sleep 1
                # (fs.list (Place "@nvme:0" ""))
                type_keys shift-9 f s dot l i s t spc shift-9 shift-p l a c e spc \
                          shift-apostrophe shift-2 n v m e shift-semicolon 0 shift-apostrophe spc \
                          shift-apostrophe shift-apostrophe shift-0 shift-0
                printf 'sendkey ret\n'
                sleep 1.5
                # (logs.recent "netstak")
                type_keys shift-9 l o g s dot r e c e n t spc shift-apostrophe n e t s t a k \
                          shift-apostrophe shift-0
                printf 'sendkey ret\n'
                sleep 1.5
                # (fs.enter (Child here "../etc"))
                type_keys shift-9 f s dot e n t e r spc shift-9 shift-c h i l d spc h e r e spc \
                          shift-apostrophe dot dot slash e t c shift-apostrophe shift-0 shift-0
                printf 'sendkey ret\n'
                sleep 2
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-errors.ppm"
                sleep 0.5
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            elif [ "${CCL_CONSOLE_DEMO:-}" = clipboard ]; then
            # Typed copy and paste (CCL_CONSOLE_DEMO=clipboard,
            # docs/clipboard.md): copy a result as an Integer, paste its
            # literal back, evaluate it again; then the manager (Super+V).
            {
                sleep 0.5
                type_keys shift-9 shift-equal spc 4 0 spc 2 shift-0
                printf 'sendkey ret\n'
                sleep 1.5
                printf 'sendkey ctrl-c\n'
                sleep 0.8
                printf 'sendkey ctrl-v\n'
                sleep 0.8
                printf 'sendkey ret\n'
                sleep 1.5
                printf 'sendkey meta_l-v\n'
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-clipboard.ppm"
                sleep 0.5
                printf 'sendkey esc\n'
                sleep 0.5
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            elif [ "${CCL_CONSOLE_DEMO:-}" = logs ]; then
            # What logstore keeps (CCL_CONSOLE_DEMO=logs): read it, lower it
            # to Debug through log-control, read it again.
            {
                sleep 0.5
                # (logs.minimum)
                type_keys shift-9 l o g s dot m i n i m u m shift-0
                printf 'sendkey ret\n'
                sleep 1
                # (logs.set-minimum Severity.Debug)
                type_keys shift-9 l o g s dot s e t minus m i n i m u m spc \
                          shift-s e v e r i t y dot shift-d e b u g shift-0
                printf 'sendkey ret\n'
                sleep 1
                type_keys shift-9 l o g s dot m i n i m u m shift-0
                printf 'sendkey ret\n'
                sleep 1.5
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-logs.ppm"
                sleep 0.5
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            elif [ "${CCL_CONSOLE_DEMO:-}" = install ]; then
            # SYS-001 from the console (CCL_CONSOLE_DEMO=install): list the
            # disks, refuse an install whose confirmation names another
            # disk, then install to the UFS disk (it takes a while). QEMU's
            # UFS model reports no serial number: its identity is the model
            # and an empty serial, which the confirmation must name.
            # system.install answers with a task at once: each (wait ...)
            # entry finishes when its install does, and (system.progress),
            # typed while the install runs, is answered meanwhile.
            for ((attempt = 0; attempt < 300; attempt++)); do
                grep -qa "installer: registered" "$SERIAL_LOG" &&
                  grep -qa "ufs: bring-up complete" "$SERIAL_LOG" && break
                sleep 0.2
            done
            sleep 0.5
            for entry in '(system.disks)' \
                '(wait (system.install (Install_Request disk => 0 confirm => (Disk_Identity "QEMU UFS" "not-this-disk" 262144 4096) inode_ratio => 262144 esp_mib => 260 source => "@nvme:0/install")))' \
                '(wait (system.install (Install_Request disk => 0 confirm => (Disk_Identity "QEMU UFS" "" 262144 4096) inode_ratio => 262144 esp_mib => 260 source => "@nvme:0/install")))' \
                '(system.progress)'; do
                # (system.progress) follows the install at once, typed fast.
                case "$entry" in
                    '(system.progress)')
                        python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" "$entry" 0.005 ;;
                    *'"QEMU UFS" ""'*)
                        python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" "$entry" 0.05 ;;
                    *)
                        python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" "$entry" 0.05
                        sleep 2 ;;
                esac
            done
            for ((attempt = 0; attempt < 600; attempt++)); do
                # Both installs' tasks completed at the console: the
                # refusal, then the install.
                [ "$(grep -ac 'REPL completed: Install_Report:' "$SERIAL_LOG")" -ge 2 ] && break
                sleep 0.2
            done
            sleep 1
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-install.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.5
            elif [ "${CCL_CONSOLE_DEMO:-}" = calc ]; then
            # The calculator core from CCL (CCL_CONSOLE_DEMO=calc,
            # docs/calculator.md): calc.evaluate gives the app's exact answer
            # as a typed Calc_Number; a failure is typed too. (calc ..) is
            # the command form.
            sleep 0.5
            for entry in '(calc.evaluate "2/3 + 1/6")' '(field (calc.evaluate "2^64") numerator)' \
                         '(field (calc.evaluate "sqrt(2)") exact)' '(calc.evaluate "1/0")' \
                         '(field (calc "3/4+1/8") decimal)' '(field (calc "sqrt(2)") bound)' \
                         '(field (calc.evaluate-with (Calc_Request "sin(30)" (Calc_Options angle => Calc_Angle.Degrees))) decimal)' \
                         '(calc.evaluate-with (Calc_Request "0xFF + 1" (Calc_Options mode => Calc_Mode.Programmer word => Calc_Word.Bits_8)))'; do
                python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" "$entry" 0.08
                sleep 1
            done
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-calc.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.5
            elif [ -n "${CCL_CONSOLE_DEMO:-}" ]; then
            # A short demo session (CCL_CONSOLE_DEMO=1): places, a live stream,
            # then the whole transcript in BASIC. Screendumps only; no markers.
            {
                sleep 0.5
                type_keys shift-semicolon c d
                printf 'sendkey ret\n'
                sleep 1
                type_keys shift-semicolon l s
                printf 'sendkey ret\n'
                sleep 1.5
                # (define t (timer.every 100))
                type_keys shift-9 d e f i n e spc t spc shift-9 t i m e r dot e v e r y spc 1 0 0 \
                          shift-0 shift-0
                printf 'sendkey ret\n'
                sleep 1
                # (image.plot (each (fn ((x Integer)) (% x 1000)) (window 60 t)))
                type_keys shift-9 i m a g e dot p l o t spc shift-9 e a c h spc shift-9 f n spc \
                          shift-9 shift-9 x spc shift-i n t e g e r shift-0 shift-0 spc \
                          shift-9 shift-5 spc x spc 1 0 0 0 shift-0 shift-0 spc \
                          shift-9 w i n d o w spc 6 0 spc t shift-0 shift-0 shift-0
                printf 'sendkey ret\n'
                sleep 1
                type_keys shift-semicolon w a t c h
                printf 'sendkey ret\n'
                sleep 4
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-lisp.ppm"
                sleep 0.5
                printf 'sendkey f8\n'
                sleep 2
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-basic.ppm"
                sleep 0.5
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            else
            {
                sleep 0.5
                # An open form continues on a new line: (+ 20 <Enter> 22) <Enter>
                type_keys shift-9 shift-equal spc 2 0
                printf 'sendkey ret\n'
                sleep 0.3
                type_keys 2 2 shift-0
                printf 'sendkey ret\n'
                sleep 0.5
                # Completion: (so <Tab> accepts "sort ", then (list 3 1 2))
                type_keys shift-9 s o
                printf 'sendkey tab\n'
                sleep 0.3
                type_keys shift-9 l i s t spc 3 spc 1 spc 2 shift-0 shift-0
                printf 'sendkey ret\n'
                sleep 0.5
                # A service's logs, typed: (>= (length (logs.recent "desktop")) 0)
                type_keys shift-9 shift-dot equal spc shift-9 l e n g t h spc shift-9 \
                          l o g s dot r e c e n t spc shift-apostrophe d e s k t o p \
                          shift-apostrophe shift-0 shift-0 spc 0 shift-0
                printf 'sendkey ret\n'
                sleep 0.6
                # A service's logs as a table: (logs.recent "clock")
                type_keys shift-9 l o g s dot r e c e n t spc shift-apostrophe c l o c k \
                          shift-apostrophe shift-0
                printf 'sendkey ret\n'
                sleep 0.8
                # A picture file from the workspace: (image.load "picture.qoi")
                type_keys shift-9 i m a g e dot l o a d spc shift-apostrophe p i c t u r e dot q o i \
                          shift-apostrophe shift-0
                printf 'sendkey ret\n'
                sleep 0.8
                # Data as a picture: (image.plot (list 3 1 4 1 5 9 2 6))
                type_keys shift-9 i m a g e dot p l o t spc shift-9 l i s t spc 3 spc 1 spc 4 \
                          spc 1 spc 5 spc 9 spc 2 spc 6 shift-0 shift-0
                printf 'sendkey ret\n'
                sleep 0.8
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-console.ppm"
                sleep 0.5
                # Streams (docs/ccl-streams.md): a timer, a window of it, and a
                # live plot that redraws as ticks arrive.
                # (define t (timer.every 100))
                type_keys shift-9 d e f i n e spc t spc shift-9 t i m e r dot e v e r y spc 1 0 0 \
                          shift-0 shift-0
                printf 'sendkey ret\n'
                sleep 0.8
                # (window 6 t)
                type_keys shift-9 w i n d o w spc 6 spc t shift-0
                printf 'sendkey ret\n'
                sleep 0.8
                # (image.plot (each (fn ((x Integer)) (% x 1000)) (window 60 t)))
                type_keys shift-9 i m a g e dot p l o t spc shift-9 e a c h spc shift-9 f n spc \
                          shift-9 shift-9 x spc shift-i n t e g e r shift-0 shift-0 spc \
                          shift-9 shift-5 spc x spc 1 0 0 0 shift-0 shift-0 spc \
                          shift-9 w i n d o w spc 6 0 spc t shift-0 shift-0 shift-0
                printf 'sendkey ret\n'
                sleep 0.8
                # :watch makes the newest cell live.
                type_keys shift-semicolon w a t c h
                printf 'sendkey ret\n'
                sleep 3
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-console-streams.ppm"
                sleep 0.5
                # The console programs itself (console.*, interfaces/console.schema).
                type_keys shift-semicolon u n w a t c h
                printf 'sendkey ret\n'
                sleep 0.5
                # (console.title "Live from CCL")
                type_keys shift-9 c o n s o l e dot t i t l e spc shift-apostrophe shift-l i v e spc \
                          f r o m spc shift-c shift-c shift-l shift-apostrophe shift-0
                printf 'sendkey ret\n'
                sleep 0.6
                # (console.stats)
                type_keys shift-9 c o n s o l e dot s t a t s shift-0
                printf 'sendkey ret\n'
                sleep 0.6
                # (console.notation Notation.Basic): the transcript reads in BASIC.
                type_keys shift-9 c o n s o l e dot n o t a t i o n spc shift-n o t a t i o n dot \
                          shift-b a s i c shift-0
                printf 'sendkey ret\n'
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-console-basic.ppm"
                sleep 0.5
                # console.theme(Theme.Daylight), typed in BASIC.
                type_keys c o n s o l e dot t h e m e shift-9 shift-t h e m e dot shift-d a y l i g h t shift-0
                printf 'sendkey ret\n'
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-console-daylight.ppm"
                sleep 0.5
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            fi
        elif [ "$TEST_NAME" = "clipboard" ]; then
            # Desktop clipboard (docs/clipboard.md) by keyboard only: copy in
            # clipboard-check, paste into Files' filter, a paste-once secret,
            # the manager (Super+V), then typed copies from Files (names,
            # paths) and Logs (a LogEntry) pasted into other apps. Each step
            # waits for its marker.
            clip_wait() {
                local attempt
                for ((attempt = 0; attempt < 150; attempt++)); do
                    grep -aF "$1" "$SERIAL_LOG" >/dev/null 2>&1 && return 0
                    sleep 0.1
                done
                echo "headless: clipboard step did not report: $1" >&2
                exit 1
            }
            clip_keys() {
                local key
                for key in "$@"; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.15
                done | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            }
            clip_shot() {
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-$1.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            }
            clip_count() {
                grep -acF "$1" "$SERIAL_LOG" 2>/dev/null || true
            }
            clip_wait_count() {
                local attempt
                for ((attempt = 0; attempt < 150; attempt++)); do
                    [ "$(clip_count "$1")" -ge "$2" ] && return 0
                    sleep 0.1
                done
                echo "headless: clipboard step did not report $2 times: $1" >&2
                exit 1
            }
            # Bring a window to the keyboard: Desktop names the window keys
            # go to ("desktop: keys to TITLE"); Shift alone is a harmless probe.
            clip_focus() {
                local probe routed
                for ((probe = 0; probe < 4; probe++)); do
                    clip_keys shift
                    sleep 0.3
                    routed="$(grep -a "desktop: keys to " "$SERIAL_LOG" | tail -1 | tr -d '\r')"
                    [ "$routed" = "desktop: keys to $1" ] && return 0
                    clip_keys alt-tab
                    sleep 0.4
                done
                echo "headless: $1 never took the keyboard" >&2
                exit 1
            }
            clip_wait "clipboard-check: ready"
            clip_wait "files: pane 1 listed @nvme:0/ "
            clip_wait "logs: window ready"
            clip_wait "clipboard-check: ungestured paste denied"
            sleep 0.5
            clip_shot start
            clip_focus "Clipboard check"
            # 1. Copy "lost" from the ordinary field ...
            clip_keys l o s t ctrl-a ctrl-c
            clip_wait "clipboard-check: copy field=name outcome=DONE"
            clip_wait "desktop: clipboard offer entry= 1 kind=ordinary text= 4"
            # ... and paste it into Files' filter in the other process.
            clip_focus Files
            clip_shot files-focused
            clip_keys ctrl-v
            clip_wait "files: pasted 4 characters into the filter"
            clip_keys ret
            clip_wait "files: pane 1 listed @nvme:0/lost+found "
            # 2. A secret: copied from the masked field, pasted once.
            clip_focus "Clipboard check"
            clip_keys tab h u n t e r 2 ctrl-a ctrl-c
            clip_wait "clipboard-check: copy field=secret outcome=DONE"
            clip_wait "desktop: clipboard offer entry= 2 kind=sensitive"
            sleep 0.3
            clip_shot clipboard-check
            clip_keys tab ctrl-a ctrl-v
            clip_wait "clipboard-check: paste field=name outcome=DONE length= 7"
            clip_wait "desktop: clipboard paste entry= 2 kind=sensitive consumed"
            # The second paste cannot see the secret: the newest remaining
            # entry ("lost") comes back instead.
            clip_keys ctrl-a ctrl-v
            clip_wait "clipboard-check: paste field=name outcome=DONE length= 4"
            clip_wait "desktop: clipboard paste entry= 1 kind=ordinary"
            # 3. The manager: Super+V, a screenshot, Escape.
            clip_keys meta_l-v
            clip_wait "desktop: clipboard manager open entries= 2"
            sleep 0.5
            clip_shot manager
            clip_keys esc
            sleep 0.3
            # 4. Files copies the entry under the cursor as a typed
            # List-String: back up to the root (the cursor lands on
            # lost+found), Ctrl+C for its name, Ctrl+Shift+C for its path ...
            clip_focus Files
            clip_keys esc backspace
            clip_wait_count "files: pane 1 listed @nvme:0/ entries=" 2
            clip_keys ctrl-c
            clip_wait "files: copy 1 names DONE"
            clip_wait "desktop: clipboard offer entry= 3 kind=ordinary text= 10 typed=yes"
            clip_keys ctrl-shift-c
            clip_wait "files: copy 1 paths DONE"
            clip_wait "desktop: clipboard offer entry= 4 kind=ordinary text= 18 typed=yes"
            # ... and the path pastes into clipboard-check, another process.
            clip_focus "Clipboard check"
            clip_keys ctrl-a ctrl-v
            clip_wait "clipboard-check: paste field=name outcome=DONE length= 18"
            clip_wait "desktop: clipboard paste entry= 4 kind=ordinary"
            # 5. Logs copies its first record as a typed LogEntry, pastes it
            # into its own search, and clipboard-check pastes it too.
            clip_focus Logs
            clip_keys home ctrl-c
            clip_wait "logs: copy record DONE"
            clip_wait "desktop: clipboard offer entry= 5 kind=ordinary"
            clip_keys ctrl-v
            clip_wait "logs: search chord DONE"
            clip_wait "desktop: clipboard paste entry= 5 kind=ordinary"
            clip_focus "Clipboard check"
            clip_keys ctrl-a ctrl-v
            clip_wait_count "desktop: clipboard paste entry= 5 kind=ordinary" 2
            # 6. The manager lists every app's copy.
            clip_keys meta_l-v
            clip_wait "desktop: clipboard manager open entries= 5"
            sleep 0.5
            clip_shot manager-apps
            clip_keys esc
            sleep 0.3
            clip_keys esc
            clip_wait "clipboard-check: closed"
        elif [ "$TEST_NAME" = "files" ]; then
            # Files (docs/files-app.md) by keyboard only: no pixel
            # coordinates, so layout changes cannot break it. Each step
            # waits for the marker of the previous one.
            files_wait() {
                local attempt
                for ((attempt = 0; attempt < 150; attempt++)); do
                    grep -aF "$1" "$SERIAL_LOG" >/dev/null 2>&1 && return 0
                    sleep 0.1
                done
                echo "headless: Files did not report: $1" >&2
                exit 1
            }
            files_wait_count() {
                local attempt
                for ((attempt = 0; attempt < 150; attempt++)); do
                    [ "$(grep -acF "$1" "$SERIAL_LOG" 2>/dev/null)" -ge "$2" ] && return 0
                    sleep 0.1
                done
                echo "headless: Files did not report $2 times: $1" >&2
                exit 1
            }
            files_keys() {
                local key
                for key in "$@"; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.15
                done | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            }
            files_wait "files: first frame presented"
            files_wait "files: pane 1 listed @nvme:0/ "
            # Screenshots for review: Files, then the Apps menu with its
            # first category open (the taskbar and its logo show in both).
            sleep 0.5
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-files.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            files_keys meta_l
            sleep 0.4
            files_keys right
            sleep 0.4
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-apps.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            files_keys esc esc
            sleep 0.3
            # The quick filter narrows to lost+found; Enter opens it.
            files_keys l o s t ret
            files_wait "files: pane 1 listed @nvme:0/lost+found "
            # Below a root, row 1 is ".."; row 2 is the fixture's folder.
            files_keys home down ret
            files_wait "files: pane 1 listed @nvme:0/lost+found/nested "
            files_keys backspace
            files_wait_count "files: pane 1 listed @nvme:0/lost+found " 2
            files_keys backspace
            files_wait_count "files: pane 1 listed @nvme:0/ " 2
            # Refresh (Ctrl+R) relists the same folder.
            files_keys ctrl-r
            files_wait_count "files: pane 1 listed @nvme:0/ " 3
            # The other pane, then a folder there: the manifest grants
            # read only, so the service must refuse and Files must say so.
            files_keys tab
            files_wait "files: active pane 2"
            files_keys f7 x ret
            files_wait "files: status "
            sleep 0.5
            # F10 quits through the view; the window closes cleanly.
            files_keys f10
            files_wait "files: closed"
        elif [ "$TEST_NAME" = "lexicon" ] || [ "$TEST_NAME" = "dictionary" ]; then
            # The lexicon (docs/lexicon.md): its CCL commands in the console
            # (lexicon), or the Dictionary app by keyboard (dictionary).
            lexicon_wait() {
                local attempt
                for ((attempt = 0; attempt < 300; attempt++)); do
                    grep -aF "$1" "$SERIAL_LOG" >/dev/null 2>&1 && return 0
                    sleep 0.1
                done
                echo "headless: did not see: $1" >&2
                exit 1
            }
            lexicon_keys() {
                local key
                for key in "$@"; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.15
                done | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            }
            lexicon_shot() {
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-$1.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
                sleep 0.5
            }
            lexicon_wait "lexicon: image loaded"
            if [ "$TEST_NAME" = "lexicon" ]; then
                lexicon_wait "ccl-console: first frame presented"
                sleep 0.5
                for entry in '(dict "cataract")' '(synonym "wharf")' '(thesaurus "accipitrine")' \
                             '(spell-check "Teh quick brown fox jumpd over the dog")' \
                             '(lexicon.known "receive")' '(dict "recieve")'; do
                    python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" "$entry"
                    sleep 1
                done
                lexicon_shot lexicon-console
                python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" '(thesaurus "wharf")'
                lexicon_shot lexicon-thesaurus
            else
                lexicon_wait "dictionary: window ready"
                sleep 1
                lexicon_shot dictionary-welcome
                lexicon_keys c a t a r a c t ret
                lexicon_shot dictionary-cataract
                lexicon_keys esc w h a r f ret
                lexicon_shot dictionary-wharf
                lexicon_keys tab
                lexicon_shot dictionary-thesaurus
                lexicon_keys esc r e c i e v e
                lexicon_shot dictionary-not-found
            fi
        elif [ "$TEST_NAME" = "text" ]; then
            # CuBit Text (docs/text-editor.md) by keyboard only: open the
            # granted document, type a line at its end, save (sibling, flush,
            # rename), quit. The saved bytes are checked on the disk after.
            text_wait() {
                local attempt
                for ((attempt = 0; attempt < 300; attempt++)); do
                    grep -aF "$1" "$SERIAL_LOG" >/dev/null 2>&1 && return 0
                    sleep 0.1
                done
                echo "headless: Text did not report: $1" >&2
                exit 1
            }
            text_keys() {
                local key
                for key in "$@"; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.15
                done | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            }
            text_wait "text: first frame presented"
            text_wait "text: opened @nvme:0/Documents/notes.txt lines="
            sleep 0.5
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-text.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            text_keys ctrl-end ret h e l l o spc c u b i t
            sleep 0.5
            text_keys ctrl-s
            text_wait "text: saved @nvme:0/Documents/notes.txt"
            sleep 0.5
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-text-saved.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            # Frames into rotating buffers: after these moves exactly one
            # current-line band may show (text-frame-check.py).
            text_keys up down up down
            sleep 0.8
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-text-moves.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            # Ctrl+S was the last chord: two plain clicks on different lines
            # must leave one cursor (a click carries its own modifiers).
            {
                printf 'mouse_move 150 120\n'
                sleep 0.3
                printf 'mouse_button 1\n'
                sleep 0.1
                printf 'mouse_button 0\n'
                sleep 0.7
                printf 'mouse_move 40 30\n'
                sleep 0.3
                printf 'mouse_button 1\n'
                sleep 0.1
                printf 'mouse_button 0\n'
                sleep 0.5
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            text_wait "text: press ctrl=0 alt=0 cursors=1"
            sleep 0.5
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-text-clicks.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            text_keys ctrl-q
            text_wait "text: closed"
        elif [ "$TEST_NAME" = "calculator" ]; then
            # CuBit Calculator (docs/calculator.md) by keyboard: exact
            # results, ans, an approximation, a big integer, a failed Enter
            # that keeps its entry; a live preview; Ctrl+Q.
            calc_wait() {
                local attempt
                for ((attempt = 0; attempt < 300; attempt++)); do
                    grep -aF "$1" "$SERIAL_LOG" >/dev/null 2>&1 && return 0
                    sleep 0.1
                done
                echo "headless: the Calculator did not report: $1" >&2
                exit 1
            }
            calc_enter() {
                python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" "$1" 0.08
            }
            calc_wait "calculator: first frame presented"
            sleep 0.5
            calc_enter "2/3 + 1/6"
            calc_wait "calculator: evaluated 2/3 + 1/6 = 5/6 entries=1"
            calc_enter "2^100"
            calc_wait "calculator: evaluated 2^100 = 1267650600228229401496703205376 entries=2"
            calc_enter "ans / 2^99"
            calc_wait "calculator: evaluated ans / 2^99 = 2 entries=3"
            calc_enter "sqrt(2)"
            calc_wait "entries=4"
            calc_enter "100!"
            calc_wait "entries=5"
            calc_enter "1/7"
            calc_wait "calculator: evaluated 1/7 = 1/7 entries=6"
            calc_enter "1/(3-3)"
            sleep 0.8
            printf 'sendkey esc\n' | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.3
            # A live preview, not entered: (1+sqrt(5))/2
            for key in shift-9 1 shift-equal s q r t shift-9 5 shift-0 shift-0 slash 2; do
                printf 'sendkey %s\n' "$key"
                sleep 0.08
            done | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 1
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-calculator.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.5
            calc_keys() {
                for key in "$@"; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.08
                done | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            }
            # Scientific (Ctrl+2): an approximation with its stated bound;
            # pi live, not entered.
            calc_keys esc ctrl-2
            calc_wait "calculator: mode SCIENTIFIC"
            calc_enter "sin(1)"
            calc_wait "calculator: evaluated sin(1) = 0.8414709848078965066"
            calc_keys p i
            sleep 1
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-calculator-scientific.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.5
            # Programmer (Ctrl+3): a 64-bit signed word; ~0 live.
            calc_keys esc ctrl-3
            calc_wait "calculator: mode PROGRAMMER"
            calc_enter "0xFF + 1"
            calc_wait "calculator: evaluated 0xFF + 1 = 256"
            calc_enter "1 << 63"
            calc_wait "calculator: evaluated 1 << 63 = -9223372036854775808"
            calc_keys shift-grave_accent 0 x f 0
            sleep 1
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-calculator-programmer.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.5
            calc_keys esc ctrl-1
            calc_wait "calculator: mode STANDARD"
            printf 'sendkey ctrl-q\n' | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            calc_wait "calculator: closed"
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
                if [ "$TEST_NAME" = "desktop-display" ]; then
                    sleep 0.2
                    # Workbench: Development (second row), first entry.
                    printf 'sendkey meta_l\n'
                    sleep 0.2
                    printf 'sendkey down\n'
                    sleep 0.2
                    printf 'sendkey right\n'
                    sleep 0.2
                    printf 'sendkey ret\n'
                    sleep 2
                    # Cursor (128,104) -> Workbench caption (148,94).
                    printf 'mouse_move 20 -10\n'
                    sleep 0.2
                    # Move the caption to y=0 first: all three subsequent
                    # clicks stay on the caption even after maximizing.
                    printf 'mouse_button 1\n'
                    sleep 0.1
                    printf 'mouse_move 0 -82\n'
                    sleep 0.2
                    printf 'mouse_button 0\n'
                    sleep 0.6
                    # Three clicks must produce only one maximize.
                    for _click in 1 2 3; do
                        printf 'mouse_button 1\n'
                        sleep 0.06
                        printf 'mouse_button 0\n'
                        sleep 0.06
                    done
                    # Expire the third press before the restoration pair.
                    sleep 0.6
                    for _click in 1 2; do
                        printf 'mouse_button 1\n'
                        sleep 0.06
                        printf 'mouse_button 0\n'
                        sleep 0.06
                    done
                fi
            } | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
        fi
    ) &
    INPUT_INJECTOR_PID=$!
fi

if { [ "$TEST_NAME" = "display-dual-output" ] || [ "$TEST_NAME" = "desktop-dual-output" ]; } &&
   [ "${CUBIT_GPU_TEST_MODE:-production}" != clear-failure ]; then
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    if [ "$TEST_NAME" = "desktop-dual-output" ]; then
        observer=check-dual-desktop.py
        if [ "${CUBIT_TEST_IDLE_DPI:-0}" = 1 ]; then
            observer=../compositor/check-idle-dpi.py
        fi
    else
        observer=check-dual-display.py
    fi
    python3 "$ROOT_DIR/tests/headless/$observer" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" &
    INPUT_INJECTOR_PID=$!
fi

if [ "$TEST_NAME" = "usb-hotplug" ]; then
    MONITOR_SOCKET="$(qemu_socket monitor)"
    rm -f "$MONITOR_SOCKET"
    MONITOR_ARGS=(-monitor "unix:$MONITOR_SOCKET,server,nowait")
    python3 "$ROOT_DIR/tests/usb-hotplug/drive.py" \
        "$SERIAL_LOG" "$MONITOR_SOCKET" "$TIMEOUT_SECONDS" &
    INPUT_INJECTOR_PID=$!
fi

if [ "$TEST_NAME" = "desktop-vm" ]; then
    # Absolute pointer and dynamic resolution, driven from the host; the
    # checker quits QEMU when done (tests/vm-experience/check-desktop-vm.py).
    QMP_SOCKET="$(qemu_socket qmp)"
    rm -f "$QMP_SOCKET"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/vm-experience/check-desktop-vm.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "$VM_VNC_SOCKET" "$VM_TRACE_LOG" &
    INPUT_INJECTOR_PID=$!
fi

if [ "$TEST_NAME" = "key-repeat" ]; then
    # Keys held through QEMU's PS/2 keyboard (QMP); the checker reads the
    # text field's length and quits QEMU (tests/key-repeat/check-headless.py).
    QMP_SOCKET="$(qemu_socket qmp)"
    rm -f "$QMP_SOCKET"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/key-repeat/check-headless.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" &
    INPUT_INJECTOR_PID=$!
fi

if [ "$TEST_NAME" = "gcc" ]; then
    # Quit QEMU once gcc-check reports (TCG runs of the compilers are slow;
    # the timeout is only the bound).
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "GCC-CHECK: PASS" "GCC-CHECK: FAIL" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "gnat" ]; then
    # Quit QEMU once gnat-check reports (the timeout is only the bound).
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "GNAT-CHECK: PASS" "GNAT-CHECK: FAIL" &
    INPUT_INJECTOR_PID=$!
fi
case "$TEST_NAME" in
    bench-fs|fs-coherence-*)
        # Quit at the program's last line, not the timeout.
        QMP_SOCKET="$(qemu_socket qmp)"
        QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
        if [ "$TEST_NAME" = "bench-fs" ]; then
            FS_TEST_ENDS=("fs-bench: done" "fs-bench: FAIL")
        else
            FS_TEST_ENDS=("fs-coherence: end")
        fi
        python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
            "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "${FS_TEST_ENDS[@]}" &
        INPUT_INJECTOR_PID=$!
        ;;
esac
if [ "$TEST_NAME" = "control-events" ]; then
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "CONTROL-CHECK: PASS" "CONTROL-CHECK: FAIL" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "bench-latency" ]; then
    # Quit QEMU once the benchmark prints its final marker.
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/sched-latency/stop-on-done.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" &
    INPUT_INJECTOR_PID=$!
fi
NETDEV_CONFIG="user,id=net0"
if [ "$TEST_NAME" = "config-tree" ]; then
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/check-config-tree.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "ccl-remote" ]; then
    NETDEV_CONFIG="user,id=net0,hostfwd=tcp:127.0.0.1:18445-10.0.2.15:8080"
fi
if [ "$TEST_NAME" = "network-authority" ]; then
    NETDEV_CONFIG="user,id=net0,hostfwd=tcp:127.0.0.1:18444-10.0.2.15:8080"
    python3 "$ROOT_DIR/tests/network-authority/peer.py" "$SERIAL_LOG" &
    NETWORK_PEER_PID=$!
fi
# The NICs' PCI slots, pinned: netmgr names an interface by its location
# (docs/net-link.md), and the system profiles configure pci-00-10-0.
NET0_PCI_SLOT=0x10
NET1_PCI_SLOT=0x11
# net-link / net-multi (tests/net-link): the fixture pulls virtual cables
# over QMP (set_link) and quits QEMU when its checks are done.
SECOND_NET_ARGS=()
if [ "$TEST_NAME" = "net-link" ] || [ "$TEST_NAME" = "net-multi" ]; then
    QMP_SOCKET="$(qemu_socket qmp)"
    rm -f "$QMP_SOCKET"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    if [ "$TEST_NAME" = "net-link" ]; then
        python3 "$ROOT_DIR/tests/net-link/fixture.py" \
            "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" &
    else
        # A second NIC on its own user network (10.0.3.0/24, DHCP from
        # QEMU): pci-00-11-0, which has no setting (netmgr's default, DHCP).
        NET_PCAP1="${NET_PCAP%.pcap}-net1.pcap"
        SECOND_NET_ARGS=(-device "virtio-net-pci,netdev=net1,addr=$NET1_PCI_SLOT" -netdev user,id=net1,net=10.0.3.0/24
                         -object "filter-dump,id=f1,netdev=net1,file=$NET_PCAP1")
        python3 "$ROOT_DIR/tests/net-link/multi_fixture.py" \
            "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "$NET_PCAP" "$NET_PCAP1" &
    fi
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "bench-net" ]; then
    # The serve workload connects in to the guest's listener on port 8080.
    NETDEV_CONFIG="user,id=net0,hostfwd=tcp:127.0.0.1:18486-10.0.2.15:8080"
    python3 "$ROOT_DIR/tests/net-bench/server.py" "$TIMEOUT_SECONDS" &
    NETWORK_PEER_PID=$!
fi
if [ "$TEST_NAME" = "timesync" ]; then
    python3 "$ROOT_DIR/tests/timesync/fixture.py" "$SERIAL_LOG" &
    NETWORK_PEER_PID=$!
fi
if [ "$TEST_NAME" = "tls-probe" ] || [ "$TEST_NAME" = "tls-service" ]; then
    python3 "$ROOT_DIR/tests/tls/server.py" "$SERIAL_LOG" "$TEST_NAME" &
    NETWORK_PEER_PID=$!
fi
if [ "$TEST_NAME" = "servo" ]; then
    python3 "$ROOT_DIR/tests/servo/http_server.py" "$TIMEOUT_SECONDS" &
    NETWORK_PEER_PID=$!
    python3 "$ROOT_DIR/tests/tls/https_server.py" "$SERIAL_LOG" "$TIMEOUT_SECONDS" &
    SERVO_HTTPS_PID=$!
fi

if [ "$TEST_NAME" = "bench-scheduler" ]; then
    python3 "$ROOT_DIR/tests/performance/clock_reference.py" "$SERIAL_LOG" \
        --timeout "$((TIMEOUT_SECONDS + 5))" > "${SERIAL_LOG}.host-clock.json" &
    CLOCK_OBSERVER_PID=$!
fi

if [ "$CHECK_EXT2" -eq 1 ]; then
    if ! python3 "$ROOT_DIR/tests/filesystem-interop/check-native.py" before \
      "$TEMP_DISK" "${SERIAL_LOG}.ext2.json"; then
        echo "headless: Ext2 interoperability precheck failed" >&2
        exit 1
    fi
fi

# The benchmark runs without a packet capture, as its Linux reference does.
PCAP_ARGS=(-object "filter-dump,id=f0,netdev=net0,file=$NET_PCAP")
if [ "$TEST_NAME" = "bench-net" ] && [ -z "${BENCH_NET_PCAP:-}" ]; then PCAP_ARGS=(); fi   # BENCH_NET_PCAP=1 keeps the capture
(
    cd "$KERNEL_DIR" || exit 1
    # shellcheck disable=SC2086
    "$TIMEOUT_BIN" "$TIMEOUT_SECONDS" "${PIN_ARGS[@]}" "$QEMU_BIN" \
        "${ACCEL_ARGS[@]}" \
        -machine q35 \
        -cpu "$QEMU_CPU_MODEL" \
        -smp "$QEMU_CPUS" \
        -m "${QEMU_MEMORY:-128M}" \
        -cdrom cubit_kernel.iso \
        -serial "file:$SERIAL_LOG" \
        -display "$QEMU_DISPLAY" \
        "${MONITOR_ARGS[@]}" \
        "${QMP_ARGS[@]}" \
        -drive "file=$DISK_IMAGE,if=none,id=nvme0,format=raw" \
        -device nvme,serial=cubitnvme,drive=nvme0 \
        "${UFS_ARGS[@]}" \
        -device "virtio-net-pci,netdev=net0,addr=$NET0_PCI_SLOT${NET_DEVICE_OPTIONS:-}" \
        "${VIDEO_ARGS[@]}" \
        -netdev "$NETDEV_CONFIG" \
        "${PCAP_ARGS[@]}" \
        "${SECOND_NET_ARGS[@]}" \
        "${AUDIO_ARGS[@]}" \
        "${TEST_DEVICE_ARGS[@]}" \
        -device intel-hda \
        -device hda-output,audiodev=snd0 \
        -no-reboot
)
qemu_status=$?

if [ -n "$CLOCK_OBSERVER_PID" ]; then
    if ! wait "$CLOCK_OBSERVER_PID"; then
        CLOCK_OBSERVER_PID=""
        echo "headless: guest/host clock check failed: ${SERIAL_LOG}.host-clock.json" >&2
        exit 1
    fi
    CLOCK_OBSERVER_PID=""
    echo "headless: host clock reference: ${SERIAL_LOG}.host-clock.json"
fi

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
    rust-native)
        required_markers="
ccl-test-host: registered
clock: registered
procmgr: pkg id=com.cubit.rust-probe
procmgr: pkg id=com.cubit.rust-probe-denied
rust-probe: Hello from Rust! (IPC)
TEST: PASS rust-allocator peer 1
TEST: PASS rust-allocator peer 2
RUST-THREADS: PASS
TEST: PASS rust-clock-authorized
TEST: PASS rust-clock-denied
TEST: PASS rust-heap-rollback
TEST: PASS rust-native
startup: com.cubit.rust-probe: its startup table has no \"clock\"; stopping (CAP-003 startup check)
"
        ;;
    config-storage|config-objects|config-objects-reopen|config-objects-benchmark)
        required_markers="
clock: registered
procmgr: pkg id=com.cubit.config-storage
Config: storage worker attached
CONFIG-STORAGE: ready
"
        if [ "$TEST_NAME" = "config-objects" ]; then
            required_markers="$required_markers
TEST: PASS config-objects-scope-denied
TEST: PASS config-objects-compiled-vm
TEST: PASS config-objects-resource-vm
TEST: PASS config-objects-receiver-vm
TEST: PASS config-objects-discovered-type
TEST: PASS config-objects-source-host
TEST: PASS config-objects-async-vm
TEST: PASS config-objects-equivalent-schema
TEST: PASS config-objects-nested
TEST: PASS config-objects-read-outcome
TEST: PASS config-objects-native
"
        elif [ "$TEST_NAME" = config-objects-reopen ]; then
            required_markers="$required_markers
TEST: PASS config-objects-reopen
TEST: PASS config-objects-compiled-vm-reopen
TEST: PASS config-objects-discovered-type-reopen
TEST: PASS config-objects-source-host-reopen
TEST: PASS config-objects-async-vm-reopen
TEST: PASS config-objects-nested-reopen
TEST: PASS config-objects-read-outcome-reopen
"
        elif [ "$TEST_NAME" = config-objects-benchmark ]; then
            required_markers="$required_markers
TEST: PASS config-objects-benchmark
"
        fi
        ;;
    turso-native-std|turso-native)
        required_markers="
clock: registered
procmgr: pkg id=com.cubit.turso-native-probe
TURSO-NATIVE: std probe PASS
"
        if [ "$TEST_NAME" = "turso-native" ]; then
            required_markers="$required_markers
TURSO-NATIVE: volatile SQL transaction PASS
TURSO-NATIVE: typed Config CBOR/revision/reopen PASS (volatile)
TURSO-NATIVE: shared File workload PASS (volatile)
TURSO-NATIVE: shared File workload PASS (filesystem)
TURSO-NATIVE: threaded grant-backed File callbacks PASS
TURSO-NATIVE: database and WAL exclusion PASS
TURSO-NATIVE: typed Config CBOR/revision/reopen PASS (filesystem)
TURSO-NATIVE: Ada typed worker publication PASS (filesystem)
STORAGE: filesystem grant retired
"
            if [ "$TURSO_REVISION" = 2 ]; then
                required_markers="$required_markers
TURSO-NATIVE: fresh boot restored revision 1 PASS
TURSO-NATIVE: fresh boot committed revision 2 PASS
"
            fi
        fi
        ;;
    config-inspection)
        required_markers="
procmgr: config scope installation failed
procmgr: init spawn failed: config-rejected.app
TEST: PASS config-inspection
TEST: PASS config-denied
TEST: PASS config-backend-nomination-denied
"
        ;;
    config-tree)
        required_markers="
config-inspector: snapshot ready
config-inspector: native window ready
"
        ;;
    log-authority)
        required_markers="
logstore: authorized typed diagnostics ready
TEST: PASS log-unapproved
TEST: PASS log-burst
TEST: PASS log-disconnect
TEST: log collector exiting with acquired grant
TEST: PASS log-collector-death
TEST: PASS log-authority
"
        ;;
    log-fields)
        required_markers="
logstore: authorized typed diagnostics ready
TEST: PASS log-fields
"
        ;;
    metrics)
        required_markers="
metricsvc: typed metrics ready
TEST: PASS metrics
"
        ;;
    ccl-remote)
        required_markers="ccl-control: native listener ready"
        ;;
    network-authority)
        required_markers="
devmgr: virtio-net using MSI-X
virtio-net: MSI-X RX/TX/config vectors ready
TEST: PASS network-unapproved
TEST: PASS network-authority
network-check: async outbound connects PASS
network-check: async outbound round trip PASS
network-check: async completion identity PASS
network-check: listener closes with an offer outstanding PASS
network-check: arrival names the offered buffer PASS
network-check: open beyond declared connections refused PASS
network-check: buffer in use cannot open a second channel PASS
netstack: IPv6 link-local
netstack: IPv6 address
netstack: IPv6 echo reply from
netstack: IPv4 echo reply from
netstack: released the scopes of exited process
capability-test: retired PID submit rejected PASS
capability-test: authorityless capability submit rejected PASS
capability-test: kernel-only message flags refused PASS
capability-test: protocol flags delivered unchanged PASS
capability-test: authorityless capability call rejected PASS
capability-test: receive into a kernel address refused PASS
capability-test: the refused message was kept (kernel) PASS
capability-test: call from a kernel address refused PASS
capability-test: completions into a kernel address refused PASS
capability-test: receive into a read-only address refused PASS
capability-test: the refused message was kept (read-only) PASS
capability-test: call from a read-only address refused PASS
capability-test: completions into a read-only address refused PASS
capability-test: receive into a unmapped address refused PASS
capability-test: the refused message was kept (unmapped) PASS
capability-test: call from a unmapped address refused PASS
capability-test: completions into a unmapped address refused PASS
capability-test: write from a kernel address prints nothing PASS
capability-test: all tests passed
"
        if ! grep -qF 'TEST: PASS network-authority' "$SERIAL_LOG"; then
            echo "headless: guest network authority checks did not finish" >&2
            exit 1
        fi
        if ! wait "$NETWORK_PEER_PID"; then
            echo "headless: network peer failed" >&2
            exit 1
        fi
        NETWORK_PEER_PID=""
        ;;
    net-link)
        required_markers="
net-link-check: udp send while down is unreachable PASS
net-link-check: udp works after the link is back PASS
net-link-check: tcp stream survived the flap and kept transferring in order PASS
TEST: PASS net-link
"
        ;;
    net-multi)
        required_markers="
net-multi-check: stream kept flowing in order until the fixture ended it PASS
net-multi-check: a default route was always usable for the probes PASS
TEST: PASS net-multi
"
        ;;
    bench-spread)
        required_markers="
BENCH: spread elapsed_ms
"
        if [ "$(grep -caF 'BENCH: spread elapsed_ms' "$SERIAL_LOG")" -ne 4 ]; then
            echo "headless: not all four bench-spread instances finished" >&2
            exit 1
        fi
        ;;
    bench-net)
        required_markers="
net-bench: start
net-bench: done
"
        ;;
    ufs)
        required_markers="
ufs: stage identify ok pci=1b36:0013
ufs: stage enable ok
ufs: stage link-up ok
ufs: stage lists ok
ufs: stage nop ok
ufs: stage device-init ok
ufs: stage device-desc ok spec=
ufs: stage geometry ok
ufs: stage lun ok lun=0 blocks=16384 bs=4096
ufs: stage unit-ready ok
ufs: stage inquiry ok
ufs: stage capacity ok blocks=16384 bs=4096 size=64M
ufs: stage partitions ok gpt n=4 p1=esp:8192K p2=msr:4096K p3=basic:43M p4=recovery:8172K
ufs: bring-up complete (13/13), serving lun 0, writes by grant
boot-logs: ufs: bring-up complete
"
        if grep -qa 'ufs: stage [a-z-]* fail' "$SERIAL_LOG"; then
            echo "headless: a UFS bring-up stage failed" >&2
            exit 1
        fi
        ;;
    ufs-write)
        required_markers="
ufs: bring-up complete (13/13), serving lun 0, writes by grant
devmgr: disk open refused: no disk-install authority
disk-write-check: device-manager endpoint cannot open a disk PASS
disk-write-check: no disk 1 PASS
disk-write-check: disk 0 opened read-only PASS
disk-write-check: read-only describe PASS
disk-write-check: read block 0 (protective MBR) PASS
ufs: write refused: no write grant count=1
disk-write-check: write without grant refused PASS
disk-write-check: refused write left the block unchanged PASS
disk-write-check: disk 0 opened read-write PASS
disk-write-check: read-write describe (flush, FUA) PASS
disk-write-check: write 4 blocks PASS
disk-write-check: FUA write PASS
disk-write-check: flush PASS
disk-write-check: readback PASS
TEST: PASS ufs-write
"
        if grep -qa 'disk-write-check: .* FAIL\|ufs: stage [a-z-]* fail' "$SERIAL_LOG"; then
            echo "headless: ufs-write reported a failure" >&2
            exit 1
        fi
        ;;
    bench-fs)
        required_markers="
fs-bench: start
fs-bench: done
"
        if grep -qa 'fs-bench: .*FAIL' "$SERIAL_LOG"; then
            echo "headless: fs-bench reported a failure" >&2
            exit 1
        fi
        ;;
    fs-coherence-*)
        required_markers="
fs-coherence: start ${TEST_NAME#fs-coherence-}
fs-coherence: ${TEST_NAME#fs-coherence-} PASS
fs-coherence: end
"
        if grep -qa 'fs-coherence: .* FAIL' "$SERIAL_LOG"; then
            echo "headless: $TEST_NAME reported a failure" >&2
            exit 1
        fi
        ;;
    bench-latency)
        required_markers="
bench-ipc-server: registered
sched-latency: start
sched-latency: peer ready
sched-latency: workload=wake load=none
sched-latency: workload=audio load=io
sched-latency: done
"
        if grep -qa 'sched-latency: FAIL\|sched-latency: .*unavailable' "$SERIAL_LOG"; then
            echo "headless: sched-latency reported a failure" >&2
            exit 1
        fi
        ;;
    mesa-sync)
        required_markers="
MESA-SYNC-NATIVE: starting
MESA-SYNC-NATIVE: PASS timeline threads deadlines loss
"
        ;;
    softpipe)
        required_markers="
SOFTPIPE-NATIVE: starting
SOFTPIPE-NATIVE: PASS 1024 pixels
SOFTPIPE-NATIVE: PASS triangle 992 pixels
"
        if grep -qF 'SOFTPIPE-NATIVE: FAIL' "$SERIAL_LOG"; then
            echo "headless: native softpipe reported failure" >&2
            exit 1
        fi
        ;;
    opengl)
        required_markers="
OPENGL-NATIVE: starting
OPENGL-NATIVE: PASS clear 1024 pixels
OPENGL-NATIVE: PASS triangle 992 pixels
OPENGL-NATIVE: PASS depth 4096 pixels
OPENGL-NATIVE: PASS context lifecycle
"
        if grep -qF 'OPENGL-NATIVE: FAIL' "$SERIAL_LOG"; then
            echo "headless: native OpenGL reported failure" >&2
            exit 1
        fi
        ;;
    buffer)
        required_markers="
OPENGL-NATIVE: PASS caller buffer 1152 pixels
"
        if grep -qF 'OPENGL-NATIVE: FAIL' "$SERIAL_LOG"; then
            echo "headless: native caller buffer reported failure" >&2
            exit 1
        fi
        ;;
    mesa-window)
        required_markers="
MESA-WINDOW: starting
MESA-WINDOW: GL drawable rendered
MESA-WINDOW: PASS 9 frames with retired-buffer reuse
MESA-WINDOW: attached immutable Mesa buffer
MESA-WINDOW: Escape; exiting
"
        if [ "${CUBIT_TEST_PHYSICAL_CLIENT:-0}" = 1 ]; then
            required_markers+="
desktop: physical output client drawing active
"
            if grep -qF 'desktop: software rendering (' "$SERIAL_LOG"; then
                echo "headless: required physical-output Mesa path fell back to CPU" >&2
                exit 1
            fi
        fi
        if [ "${MESA_WINDOW_ANIMATION:-0}" = 1 ]; then
            required_markers+="
MESA-WINDOW: animation resumed
MESA-WINDOW: PASS animated cycle with retired-buffer reuse
MESA-WINDOW: animation paused
"
        fi
        if [ "${CUBIT_TEST_TARGET_RETIREMENT:-0}" = 1 ]; then
            required_markers+="
desktop: renderer targets retired
desktop: output readers retired= 0
desktop: pixel teardown charged= 0
"
        fi
        if grep -qF 'MESA-WINDOW: FAIL' "$SERIAL_LOG"; then
            echo "headless: Mesa window reported failure" >&2
            exit 1
        fi
        case "${MESA_WINDOW_SCENE:-quads}" in
            quads) python3 "$ROOT_DIR/tests/mesa-software/check-window.py" "${SERIAL_LOG%.log}-mesa.ppm" || exit 1 ;;
            *) echo 'headless: unsupported MESA_WINDOW_SCENE' >&2; exit 2 ;;
        esac
        ;;
    libc)
        required_markers="
libc-check: hello from musl on CuBit
libc-check: malloc large and small PASS
libc-check: pthread mutex across 8 threads PASS
libc-check: __thread variables per thread and join values PASS
libc-check: main thread's __thread initial value PASS
libc-check: usleep and clock_gettime PASS
libc-check: clock_gettime monotonic over 100000 reads PASS
libc-check: printf floats PASS
libc-check: main thread stack bounds PASS
libc-check: open and read (DER begins with a SEQUENCE) PASS
libc-check: lseek, pread and end of file PASS
libc-check: mmap a file PASS
libc-check: opendir and readdir PASS
libc-check: a file outside the scope is denied PASS
libc-check: create and write a file PASS
libc-check: fstat: type, size, links, inode PASS
libc-check: a write sets mtime and ctime to the wall clock PASS
libc-check: stat by name agrees with fstat PASS
libc-check: the directory's mtime follows its entries PASS
libc-check: readlink: no symbolic links (EINVAL), missing names ENOENT PASS
libc-check: realpath PASS
libc-check: access: W_OK follows the write scope PASS
libc-check: ftruncate and truncate shrink and zero-fill PASS
libc-check: pipe for select PASS
libc-check: select: empty pipe writable, not readable PASS
libc-check: select: readable after a write PASS
libc-check: remove the file PASS
libc-check: getcwd starts at / PASS
libc-check: chdir and getcwd PASS
libc-check: a relative name starts from the working directory PASS
libc-check: dot and dot-dot components PASS
libc-check: chdir refuses files and missing names and stays put PASS
libc-check: mkdirat, openat, fstatat, faccessat, renameat and unlinkat PASS
libc-check: chdir to a directory it may not read is refused PASS
libc-check: fchdir PASS
libc-check: openat with a non-directory or closed descriptor PASS
libc-check: dot-dot stops at the root PASS
LIBC: PASS
cxx-check: hello from C++ on CuBit
cxx-check: exceptions unwind and run destructors PASS
cxx-check: std::thread, mutex, condition_variable PASS
cxx-check: thread_local std::string PASS
cxx-check: streams and map PASS
CXX: PASS
"
        python3 "$ROOT_DIR/userspace/libc/tests/check-protection-faults.py" "$SERIAL_LOG" || exit 1
        ;;
    gcc)
        required_markers="
gcc-check: cc1 compiles hello.c PASS
gcc-check: cc1 output matches Linux PASS
gcc-check: as assembles it PASS
gcc-check: as output matches Linux PASS
gcc-check: gcc -c compiles and assembles hello.c PASS
gcc-check: gcc -c output matches Linux PASS
gcc-check: archive-walk reads libc.a PASS
gcc-check: a child's stderr arrives through the lent ring PASS
gcc-check: gcc links hello PASS
gcc-check: gcc link output matches Linux PASS
gcc-check: hello runs and exits with 42 PASS
gcc-check: gcc reports a compile error and exits with 1 PASS
gcc-check: cc1's diagnostic arrives through the driver PASS
GCC-CHECK: PASS
"
        if grep -qF 'GCC-CHECK: FAIL' "$SERIAL_LOG"; then
            echo "headless: gcc test reported a failure" >&2
            exit 1
        fi
        ;;
    gnat)
        required_markers="
gnat-check: gcc -c compiles hello.adb (gnat1, as) PASS
gnat-check: hello.o matches Linux PASS
gnat-check: gnatbind binds hello.ali PASS
gnat-check: gcc -c compiles the binder unit PASS
gnat-check: b~hello.o matches Linux PASS
gnat-check: gcc links hello (ld, libgnat) PASS
gnat-check: hello matches Linux PASS
gnat-check: hello runs and exits with 7 PASS
gnat-check: hello's standard output arrives PASS
gnat-check: hello's transcript matches Linux PASS
gnat-check: gcc reports an Ada compile error and exits with 1 PASS
gnat-check: gnat1's diagnostic arrives through the driver PASS
GNAT-CHECK: PASS
"
        if grep -qF 'GNAT-CHECK: FAIL' "$SERIAL_LOG"; then
            echo "headless: gnat test reported a failure" >&2
            exit 1
        fi
        ;;
    binutils)
        required_markers="
binutils-check: procmgr describes as.app PASS
binutils-check: procmgr describes ld.app PASS
binutils-check: as assembles hello.s PASS
binutils-check: as output matches Linux PASS
binutils-check: ld links hello.o PASS
binutils-check: ld output matches Linux PASS
binutils-check: ld without an output is refused (Missing_Parameter) PASS
binutils-check: an output outside the launcher's places is refused (Not_Granted) PASS
binutils-check: as with raw argv and no places fails PASS
BINUTILS-CHECK: PASS
"
        if grep -qF 'BINUTILS-CHECK: FAIL' "$SERIAL_LOG"; then
            echo "headless: binutils test reported a failure" >&2
            exit 1
        fi
        ;;
    control-events)
        required_markers="
control-producer: ready
control-check: the child starts with a lent ring PASS
control-child: a flooded producer still freed every reader slot
control-child: forged grant-returned event refused
control-child: forged grant-revoked event refused
control-child: forged Stop control message refused
control-child: forged child exit refused
control-child: malformed contract refused
control-child: unknown connector refused
control-child: lossless open of a broadcast outlet refused
control-child: producing into someone's outlet refused
control-child: every attack on the producer was refused
control-child: read the producer's outlet through a channel
control-child: closed and reopened the channel past the reader limit
control-check: the child writes into it PASS
control-check: a control message to a process it did not launch is refused PASS
control-check: the launcher revokes the ring PASS
control-check: an Interrupt to its own child is accepted PASS
control-check: a Reload to its own child is accepted PASS
control-check: a Stop to its own child is accepted PASS
control-child: the ring was revoked and returned
control-child: Interrupt, Reload and Stop all received
control-child: Stop received
control-check: the child saw the revoke (its runtime returned the ring), then the Stop PASS
control-check: the launcher is told its ring came back PASS
control-check: the launcher flooded the producer throughout, and the child still got through PASS
CONTROL-CHECK: PASS
"
        ;;
    program-parameters)
        required_markers="
procmgr: parameters parameters-check.app: (Check_Parameters count => 64 deadline => (Check_Deadline.Within 2500) verbose => false)
parameters-check: decoded count=64 deadline=Within 2500 verbose=false
procmgr: parameters parameters-check.app: (Check_Parameters count => 4096 deadline => Check_Deadline.Forever verbose => true)
parameters-check: decoded count=4096 deadline=Forever verbose=true
parameters-plain: started without arguments
procmgr: init spawn failed: parameters-check.app: it requires typed parameters and the launch gave none
procmgr: parameters refused: parameters-check.app: not a Check_Parameters: Value is outside the range of its type
procmgr: init spawn failed: parameters-check.app: its typed parameters are not of its parameter record
procmgr: parameters refused: parameters-plain.app declares none
procmgr: init spawn failed: parameters-plain.app: it declares no parameters and the launch gave some
"
        if [ "$(grep -c 'parameters-check: decoded' "$SERIAL_LOG")" -ne 2 ] ||
           [ "$(grep -c 'parameters-plain: started' "$SERIAL_LOG")" -ne 1 ]; then
            echo "headless: a refused launch ran, or an accepted one did not" >&2
            exit 1
        fi
        ;;
    processes)
        required_markers="
args-check: no launch block gives argv {cubit-program} PASS
spawn-check: waitpid without children is ECHILD PASS
args-check: argv, environment and writable strings PASS
spawn-check: argv, environment and exit status 42 reach parent PASS
spawn-check: absolute program name PASS
spawn-check: three children, exit codes 0, 255 and 300 (low 8 bits) via waitpid(-1) PASS
spawn-check: WNOHANG while the child runs PASS
spawn-check: then its exit code 5 PASS
spawn-check: faulting child reported as stopped (WIFSIGNALED) PASS
args-check: 1000 arguments PASS
args-check: working directory from the launcher PASS
spawn-check: no working directory chosen: a child starts at / PASS
spawn-check: chdir and getcwd PASS
spawn-check: after chdir, a child starts there (/tls) PASS
spawn-check: a child that may not read the working directory is refused (EACCES) PASS
spawn-check: chdir to a directory it may not read is refused, cwd unchanged PASS
spawn-check: 1000 arguments PASS
spawn-check: oversized arguments are E2BIG PASS
spawn-check: missing program is ENOENT PASS
spawn-check: a program the manifest does not name is EACCES (Not_Granted) PASS
spawn-check: a child asking for more authority than its launcher is EACCES PASS
spawn-check: fork is ENOSYS PASS
spawn-check: execve is ENOSYS PASS
spawn-check: system is ENOSYS PASS
spawn-check: popen is ENOSYS PASS
ada-args-check: Ada.Command_Line PASS
spawn-check: Ada.Command_Line arguments and Set_Exit_Status 43 PASS
rust-args-check: std::env::args and env::var PASS
spawn-check: Rust std::env::args, env::var and exit code 44 PASS
spawn-check: all children collected PASS
PROCESS-SPAWN: PASS
delegate-check: child started without delegation PASS
delegate-check: child started with the place delegated PASS
delegate-check: a place longer than 64 bytes delegated PASS
delegate-check: delegating write it does not hold is refused (Not_Granted) PASS
delegate-check: delegating a place it does not hold is refused (Not_Granted) PASS
DELEGATE-CHECK: PASS
"
        if [ "$(grep -c 'args-check: delegated read as expected PASS' "$SERIAL_LOG")" -lt 3 ]; then
            echo "headless: delegated-place children did not all read as expected" >&2
            exit 1
        fi
        if grep -qF 'PROCESS-SPAWN: FAIL' "$SERIAL_LOG" ||
           grep -qF 'DELEGATE-CHECK: FAIL' "$SERIAL_LOG" ||
           grep -qF 'args-check: delegated read FAIL' "$SERIAL_LOG"; then
            echo "headless: processes test reported a failure" >&2
            exit 1
        fi
        ;;
    servo)
        if [ -n "${SERVO_DESKTOP:-}" ]; then
            required_markers="
CUBITSHELL: desktop window
CUBITSHELL: loading page 0
CUBITSHELL: PASS
"
        else
        required_markers="
CUBITSHELL: loading page 0
CUBITSHELL: loading page 1
CUBITSHELL: loading page 2
CUBITSHELL: PASS
"
        fi
        if [ "${SERVO_BROWSER_CHECK:-0}" = 1 ]; then
            required_markers="$required_markers
ui-app: protected frame published
CUBITSHELL-BROWSER: aligned allocator PASS cycles=32
CUBITSHELL-BROWSER: frame cancel PASS cycles=2
CUBITSHELL-BROWSER: fonts PASS
CUBITSHELL: closed
CUBITSHELL-BROWSER: title CuBitBrowserTyped:abc
CUBITSHELL: back
CUBITSHELL: forward
CUBITSHELL: reload
"
        fi
        ;;
    rust-std)
        required_markers="
rust-std: hello from std on CuBit
rust-std: mutex and join across threads PASS
rust-std: condvar handoff PASS
rust-std: thread-locals are per thread PASS
rust-std: channels PASS
rust-std: hashmap with random keys PASS
rust-std: sleep and Instant PASS
rust-std: thread with a custom stack PASS
RUST-STD: PASS
"
        ;;
    avx)
        required_markers="
Process: user FP/SIMD state saved with XSAVEOPT
avx-check: all 16 YMM registers kept across 20000 yields PASS
"
        ;;
    clock)
        required_markers="
clock-check: clock publication present PASS
clock-check: 1000000 page reads are published and monotonic PASS
clock-check: 1000000 page millisecond reads are monotonic PASS
clock-check: a page read is cheaper than either system call PASS
clock-check: READ_MONOTONIC_MICROSECONDS lies between two page reads PASS
clock-check: the page is never behind GETTIME, and at most 10 ms ahead PASS
clock-check: a futex deadline from the page does not expire early PASS
clock-check: sleeping until a page microsecond does not return early PASS
TEST: PASS clock
clock-check: write to the clock page armed
"
        grep -F 'clock-check: cost ' "$SERIAL_LOG" || true
        # The write must fault at the page's address (0x5B00_0000_0000) and
        # retire the process; nothing may print after the write.
        if grep -F 'clock-check: ' "$SERIAL_LOG" | grep -qF 'FAIL' ||
           ! awk '/clock-check: write to the clock page armed/ { seen = 1 }
                  seen && /USER-MEMORY-FAULT: pid [0-9]+ address 100055558127616 kind=write-protection/ { fault = 1 }
                  fault && /reclaimProcess: stopped PID/ { found = 1 }
                  END { exit !found }' "$SERIAL_LOG"; then
            echo "headless: clock checks failed, or the page write was not refused" >&2
            exit 1
        fi
        ;;
    firmware-snapshot)
        required_markers="
firmware-check: catalog info PASS
firmware-check: snapshot ready with tables PASS
firmware-check: checksum FACP PASS
firmware-check: checksum DSDT PASS
firmware-check: FADT present PASS
firmware-check: DSDT present PASS
firmware-check: FACS excluded PASS
firmware-check: catalog totals PASS
firmware-check: unknown table refused PASS
firmware-check: copy of an unknown table refused PASS
acpi-standin: registered
acpi-standin: probe observer=yes
firmware-check: acpi endpoint carries the observer badge PASS
TEST: PASS firmware-snapshot
procmgr: firmware snapshot refused: firmware-denied.app is not approved by the startup plan
procmgr: init spawn failed: firmware-denied.app
firmware-undeclared: no handle not authorized PASS
firmware-undeclared: own endpoint not authorized PASS
firmware-undeclared: own process not authorized PASS
firmware-undeclared: forged handle word not authorized PASS
firmware-undeclared: no acpi endpoint PASS
TEST: PASS firmware-undeclared
"
        # The unapproved image never runs; no check fails.
        if grep -F 'firmware-check: ' "$SERIAL_LOG" | grep -qF ' FAIL' ||
           grep -qF 'TEST: FAIL firmware' "$SERIAL_LOG" ||
           grep -qF 'acpi-standin: probe observer=no' "$SERIAL_LOG" ||
           [ "$(grep -cF 'firmware-check: starting' "$SERIAL_LOG")" -ne 1 ]; then
            echo "headless: firmware snapshot checks failed, or the unapproved image ran" >&2
            exit 1
        fi
        ;;
    futex)
        required_markers="
futex-check: wait woken by another thread PASS
futex-check: mutex counter exact under contention PASS
futex-check: mutual exclusion held PASS
futex-check: each thread keeps its own fs base PASS
futex-check: 320 short-lived threads ran and were joined PASS
futex-check: thread quota is 128 including the main thread PASS
futex-check: exited threads reaped and quota returned PASS
TEST: PASS futex
futex-check: exiting with live threads
"
        # The process exits with threads blocked and spinning; the reaper
        # must then retire it (all its threads) without a kernel fault.
        if grep -F 'futex-check: ' "$SERIAL_LOG" | grep -qF ' FAIL' ||
           ! awk '/futex-check: exiting with live threads/ { seen = 1 }
                  seen && /reclaimProcess: stopped PID/ { found = 1 }
                  END { exit !found }' "$SERIAL_LOG"; then
            echo "headless: futex checks failed, or the process was not reaped" >&2
            exit 1
        fi
        ;;
    threads)
        required_markers="
thread-check: fs base survives context switches PASS
thread-check: user gs base cleared across context switches PASS
TEST: PASS thread-cpu-state
"
        # Both instances must pass: one passing alone proves no isolation.
        THREADS_PASSED=$(grep -cF 'TEST: PASS thread-cpu-state' "$SERIAL_LOG")
        if [ "$THREADS_PASSED" -ne 2 ] ||
           grep -F 'thread-check: ' "$SERIAL_LOG" | grep -qF ' FAIL'; then
            echo "headless: thread checks failed or did not finish in both instances" >&2
            exit 1
        fi
        ;;
    timesync)
        required_markers="
timesync-check: ordinary clock endpoint cannot adjust time PASS
timesync-check: undeclared clock-control is unusable PASS
clock: wall time set from sample
timesync-check: clock reports network-synchronized time PASS
TEST: PASS timesync
"
        if ! grep -qF 'TEST: PASS timesync' "$SERIAL_LOG"; then
            echo "headless: guest timesync checks did not finish" >&2
            exit 1
        fi
        if ! wait "$NETWORK_PEER_PID"; then
            echo "headless: timesync fixture failed" >&2
            exit 1
        fi
        NETWORK_PEER_PID=""
        ;;
    tls-probe)
        # The guest prints TEST: PASS only when every check passed, and the
        # fixture verifies each case independently. Individual check lines
        # are not required: concurrent serial output can interleave them.
        required_markers="
TEST: PASS tls-probe
"
        if ! grep -qF 'TEST: PASS tls-probe' "$SERIAL_LOG"; then
            echo "headless: guest TLS probe did not finish" >&2
            exit 1
        fi
        if ! wait "$NETWORK_PEER_PID"; then
            echo "headless: TLS fixture failed" >&2
            exit 1
        fi
        NETWORK_PEER_PID=""
        ;;
    wget-https)
        required_markers="
tls: ready
tls: channel 1 established with example.com
wget: status HTTP/1.1 200 OK
wget: done
"
        ;;
    tls-service)
        required_markers="
tls: ready
TEST: PASS tls-unapproved
TEST: PASS tls-service
"
        if ! grep -qF 'TEST: PASS tls-service' "$SERIAL_LOG"; then
            echo "headless: guest TLS service checks did not finish" >&2
            exit 1
        fi
        if ! wait "$NETWORK_PEER_PID"; then
            echo "headless: TLS fixture failed" >&2
            exit 1
        fi
        NETWORK_PEER_PID=""
        ;;
    grant-forward)
        required_markers="GRANT-FORWARD-CHECK: PASS"
        ;;
    grant-forward-intermediary-exit)
        required_markers="GRANT-FORWARD-INTERMEDIARY-EXIT: PASS"
        ;;
    grant-forward-owner-exit)
        required_markers="
GRANT-FORWARD-OWNER-ROOT-DRAIN: PASS
GRANT-FORWARD-OWNER-EXIT: PASS
"
        ;;
    gpu-viewer)
        required_markers="
gpu-viewer fixture: synthetic RAM producer ready
gpu-viewer: completed target attached; viewer copied no pixels
GPU-VIEWER-ROOT-RETIRED: PASS
"
        ;;
    grant-forward-desktop)
        required_markers="GRANT-FORWARD-DESKTOP: PASS"
        ;;
    async-ipc)
        required_markers="
ipctest-server: registered
ipctest-client: starting
ipctest-departing: exiting with saved reply
ipctest-server: dead caller reply retired and slot reused
ipctest-client: call deadlines PASS
ipctest-client: restored reply capability PASS
ipctest-client: four receive fairness paths PASS
ipctest-client: activity request wake PASS
ipctest-client: activity wait PASS
ipctest-client: activity target death wake PASS
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
IPC-RETIREMENT-CHECK: PASS
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
    bench-scheduler)
        required_markers="
CPU-CONTROL: START
CPU-CONTROL: COMPLETE
ACCOUNTING:
"
        ;;
    ccl-vm)
        required_markers="
ccl-vm: starting
ccl-test-host: registered
ccl-vm: bytecode PASS
ccl-vm: module PASS
ccl-vm: source PASS
ccl-vm: enum source PASS
ccl-vm: enum isolation PASS
ccl-vm: variant bytecode PASS
ccl-test-host: import invoked
ccl-vm: import IPC PASS
clock: registered
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
desktop: active outputs= 1 primary= 0
ccl-workbench: native window ready
ccl-workbench: first frame presented
ui-app: protected frame published
"
        ;;
    ccl-workbench-virtio-vga)
        required_markers="
clock: registered
display: backend virtio-gpu
desktop: active outputs= 1 primary= 0
virtio-gpu: page flipping active
ccl-workbench: native window ready
ccl-workbench: first frame presented
ui-app: protected frame published
"
        ;;
    scanout-repair)
        required_markers="
stall-check: holding a call for
STUCK-CALL: stall-check.app
stall-check: call ended
stall-check: done
display: kernel diagnostics erased the scanout; copying a whole frame
"
        ;;
    process-kill)
        required_markers="
fault-check: writing an unmapped address
process killed: fault-check.app
kernel: process killed #
name=fault-check.app
procmgr: process com.cubit.fault-check
DESKTOP-BOOTSTRAP-NOTE: kernel: process killed #
"
        ;;
    oom-reclaim)
        required_markers="
oom-check: holding
oom-check: memory exhausted after
process killed: oom-check.app out-of-memory
Process.reclaimProcess: stopped PID
procmgr: process com.cubit.oom-check
"
        ;;
    devmem-denials)
        required_markers="
devmem-check: kernel-image and bus mappings refused
device-memory denied: devmem-check.app MAP_DEVICE
devmem-check.app (identity
kernel: physical map:
DESKTOP-BOOTSTRAP-NOTE: kernel: device-memory denied #
DESKTOP-BOOTSTRAP: logs as of t=
"
        ;;
    logs)
        required_markers="
clock: registered
desktop: active outputs= 1 primary= 0
logs: window ready
ui-app: protected frame published
"
        ;;
    ccl-console)
        required_markers="
clock: registered
display: backend virtio-gpu
desktop: active outputs= 1 primary= 0
ccl-console: native window ready
ccl-console: first frame presented
ui-app: protected frame published
"
        # A named demo (CCL_CONSOLE_DEMO) types its own entries instead of
        # the default ones, so only its own results are required.
        if [ "${CCL_CONSOLE_DEMO:-}" = gcc ]; then
            required_markers="$required_markers
ccl-console: REPL completed: Task<Run_Outcome>: Done (Run_Outcome.Finished (Unix_Exit 0))
ccl-console: REPL completed: Task<Run_Outcome>: Done (Run_Outcome.Finished (Unix_Exit 1))
"
        elif [ "${CCL_CONSOLE_DEMO:-}" = programs ]; then
            required_markers="$required_markers
ccl-console: REPL completed: List<Outlet_State>: [(Outlet_State \"unix.stdout\" \"Stream<String>\"
ccl-console: REPL completed: Expression does not type-check: field output takes Output_File, not Input_File
ccl-console: REPL completed: Task<Run_Outcome>: Done (Run_Outcome.Finished (Unix_Exit 1))
ccl-console: REPL completed: Task<Run_Outcome>: Done (Run_Outcome.Finished (Unix_Exit 0))
"
        elif [ "${CCL_CONSOLE_DEMO:-}" = install ]; then
            required_markers="$required_markers
ufs: bring-up complete (13/13), serving lun 0, writes by grant
installer: registered
ccl-console: REPL completed: List<Disk>: [(Disk 0 (Disk_Identity \"QEMU UFS\" \"\" 262144 4096))]
installer: install refused: confirmation names
ccl-console: REPL completed: Install_Report: (Install_Report Install_Outcome.Wrong_Disk Identity_Proof.Serial_Number 0 0)
installer: installing to disk 0
installer: status answered during the install:
ccl-console: REPL completed: Install_Progress: (Install_Progress Install_State.
installer: install done: installed
ccl-console: REPL completed: Install_Report: (Install_Report Install_Outcome.Installed Identity_Proof.Model_Geometry
installer: disk 0 reports no serial: identified by model and geometry
"
        elif [ "${CCL_CONSOLE_DEMO:-}" = calc ]; then
            required_markers="$required_markers
ccl-console: REPL completed: Calc_Number: (Calc_Number true \"5\" \"6\" \"0.8(3)\" Calc_Form.Repeating \"\" \"\" \"\" \"\" false)
ccl-console: REPL completed: String: 18446744073709551616
ccl-console: REPL completed: Boolean: false
ccl-console: REPL completed: calc.evaluate rejected its argument: division by zero (column 2)
ccl-console: REPL completed: String: 0.875
ccl-console: REPL completed: String: 2e-21
ccl-console: REPL completed: String: 0.5
ccl-console: REPL completed: Calc_Number: (Calc_Number true \"0\" \"1\" \"0\" Calc_Form.Integer_Form \"\" \"00\" \"000\" \"00000000\" true)
"
        elif [ "${CCL_CONSOLE_DEMO:-}" = clipboard ]; then
            required_markers="$required_markers
ccl-console: REPL completed: Integer: 42
ccl-console: copy typed Integer DONE
desktop: clipboard offer entry= 1 kind=ordinary text= 2 typed=yes
desktop: clipboard paste entry= 1 kind=ordinary typed
ccl-console: pasted typed Integer
desktop: clipboard manager open entries= 1
"
        elif [ -z "${CCL_CONSOLE_DEMO:-}" ]; then
            required_markers="$required_markers
ccl-console: REPL completed: Integer: 42
ccl-console: REPL completed: List<Integer>: [1, 2, 3]
ccl-console: REPL completed: Boolean: true
ccl-console: REPL completed: List<LogEntry>: [(LogEntry
ccl-console: REPL completed: Image: (Image 160 100
ccl-console: REPL completed: Image: (Image 320 120
"
        fi
        ;;
    lexicon)
        required_markers="
lexicon: registered
lexicon: image loaded
ccl-console: first frame presented
ccl-console: REPL completed: Definition: (Definition.Found (Word_Entry \"cataract\" [(Word_Sense Part_Of_Speech.Noun \"an eye disease
ccl-console: REPL completed: Synonyms: (Synonyms.Found (Synonym_List \"wharf\" [\"pier\" \"wharfage\" \"dock\"
ccl-console: REPL completed: Thesaurus_Result: (Thesaurus_Result.Found (Thesaurus_Entry \"accipitrine\" [(Synonym_Group Part_Of_Speech.Adjective
ccl-console: REPL completed: List<Misspelling>: [(Misspelling \"Teh\" 0 [\"the\"
ccl-console: REPL completed: Boolean: true
ccl-console: REPL completed: Definition: (Definition.Not_Found (Near_Spellings \"recieve\" [\"receive\"
ccl-console: REPL completed: Thesaurus_Result: (Thesaurus_Result.Found (Thesaurus_Entry \"wharf\"
"
        ;;
    dictionary)
        required_markers="
lexicon: registered
lexicon: image loaded
dictionary: starting
dictionary: window ready
ui-app: protected frame published
"
        ;;
    ccl-workspace)
        required_markers="
clock: registered
display: backend virtio-gpu
desktop: active outputs= 1 primary= 0
ccl-workbench: native window ready
ccl-workbench: first frame presented
ui-app: protected frame published
ccl-workbench: workspace saved ccl-0001.ccl
ccl-workbench: workspace saved ccl-0002.ccl
ccl-workbench: workspace opened ccl-0002.ccl
ccl-workbench: workspace saved clock.ccl
ccl-workbench: workspace saved quoted.ccl
ccl-workbench: REPL completed: Integer: 42
ccl-workbench: REPL completed: Boolean: true
ccl-workbench: live label STARTED
ccl-workbench: live label SAMPLED
ccl-workbench: live label STOPPED
"
        ;;
    capability-security)
        required_markers="
Process.Loader: invalid program-header table
Process.Loader: invalid load-segment geometry or flags
Process.Loader: PT_GNU_STACK size violates stack policy
Process.Loader: unpublished process reclaimed
procmgr: init spawn failed: bad-phdr.app
procmgr: init spawn failed: bad-segment.app
procmgr: init spawn failed: bad-stack.app
procmgr: init spawn failed: overlap.app

capability-test: heap query PASS
capability-test: heap wrapping request rejected without growth PASS
capability-test: heap oversized request rejected without growth PASS
capability-test: heap usable after rejected requests PASS
capability-test: heap admitted pages zeroed and writable PASS
capability-test: stack demand pages writable and retained PASS
capability-test: syscall registers normal return PASS
capability-test: syscall registers unknown call PASS
capability-test: syscall registers denied call PASS
capability-test: syscall registers blocking return PASS
capability-test: getpid PASS
capability-test: startup table holds only the self entries, with the declared limit PASS
capability-test: self entries are current handles PASS
capability-test: no ambient capability at any old slot number PASS
capability-test: forged future generation refused PASS
capability-test: forged slot past the table refused PASS
capability-test: all-ones word refused PASS
capability-test: call through a forged endpoint word refused PASS
capability-test: self process rights attenuated PASS
capability-test: self endpoint names this identity PASS
capability-test: self endpoint cannot be passed on PASS
capability-test: no derivation from a capability without grant PASS
capability-test: refused derivations leave the source unchanged PASS
capability-test: ambient event publication denied PASS
capability-test: raw event publication without a handle denied PASS
capability-test: an endpoint to itself does not authorize events to another process PASS
capability-test: retired PID submit rejected PASS
capability-test: retired INFO rejected PASS
capability-test: retired VIRT_TO_PHYS rejected PASS
capability-test: retired SET_SYSINFO rejected PASS
capability-test: retired SET_WELL_KNOWN rejected PASS
capability-test: system facts published PASS
capability-test: the service registry names procmgr PASS
capability-test: own statistics read PASS
capability-test: only the clock service publishes the wall clock PASS
capability-test: only devmgr publishes the boot display handoff PASS
capability-test: authorityless capability submit rejected PASS
capability-test: kernel-only message flags refused PASS
capability-test: protocol flags delivered unchanged PASS
capability-test: authorityless capability call rejected PASS
capability-test: write from a kernel address prints nothing PASS
capability-test: retired mint, delegate and restore calls rejected PASS
capability-test: mint denial leaves the old slot number empty PASS
capability-test: no offer taken without a call to answer PASS
capability-test: no offer taken from itself PASS
capability-test: no capability given without a request PASS
capability-test: ambient spawn denied PASS
capability-test: spawn through the self process denied PASS
capability-test: raw spawn without a handle denied PASS
capability-test: the self entries survive the limit PASS
capability-test: close a saved reply PASS
capability-test: a closed handle is refused (Stale) PASS
capability-test: a closed reply cannot answer PASS
capability-test: closing frees a place under the limit PASS
capability-test: a stale handle is refused after its slot is reused PASS
capability-test: the new entry was untouched by the stale handle PASS
capability-test: close the remaining saved replies PASS
capability-test: close the self process entry PASS
capability-test: a closed process handle is refused (Stale) PASS
capability-test: probe queued to itself (kernel) PASS
capability-test: receive into a kernel address refused PASS
capability-test: call from a kernel address refused PASS
capability-test: completions into a kernel address refused PASS
capability-test: probe queued to itself (read-only) PASS
capability-test: receive into a read-only address refused PASS
capability-test: call from a read-only address refused PASS
capability-test: completions into a read-only address refused PASS
capability-test: probe queued to itself (unmapped) PASS
capability-test: receive into a unmapped address refused PASS
capability-test: call from a unmapped address refused PASS
capability-test: completions into a unmapped address refused PASS
capability-test: handle limit reached (Limit_Reached) PASS
capability-test: the refused message was kept (kernel) PASS
capability-test: the refused message was kept (read-only) PASS
capability-test: the refused message was kept (unmapped) PASS
capability-test: all tests passed
"
        ;;
    storage-grants)
        required_markers="
GRANT-REFERENCE-CHECK: PASS
GRANT-RECLAMATION-CHECK: PASS
OWNED-MEMORY-CHECK: PASS
OWNED-GRANT-RETENTION-CHECK: PASS
OWNED-EXIT-CHECK: leaving two allocations
Process.reclaimProcess: owned regions retired 2
RAM-VOLUME-CHECK: PASS
VOLUME-ISOLATION-CHECK: PASS
ramdisk: volatile block device ready
STORAGE-FLUSH-CHECK: PASS
POSITIONED-IO-CHECK: PASS
FILE-COHERENCE-CHECK: PASS
FILE-RESIZE-CHECK: PASS
FILE-DOUBLE-RESIZE-CHECK: PASS
FILE-TRIPLE-RESIZE-CHECK: PASS
FILE-EXCLUSIVE-CHECK: PASS
FILE-DOUBLE-OVERWRITE-CHECK: PASS
MALFORMED-DIRECTORY-CHECK: PASS
RENAME-CHECK: PASS
DIRECTORY-NAVIGATION-CHECK: PASS
FILESYSTEM-SCOPE-CHECK: PASS
QUEUE-WAKE-CHECK: PASS
QUEUE-RENAME-CHECK: PASS
QUEUE-DIRECTORY-CHECK: PASS
QUEUE-EVENTS-CHECK: PASS
QUEUE-SCOPES-CHECK: PASS
QUEUE-VOLUME-CHECK: PASS
QUEUE-COPY-CHECK: PASS
STORAGE-CHECK: PASS
"
        ;;
    bench-storage)
        required_markers="
STORAGE-BENCH: COMPLETE
TIMING: fs-open-existing count= 512
TIMING: fs-read-4k-sequential-warm count= 512
TIMING: fs-read-4k-random-warm count= 512
TIMING: fs-read-at-4k-random-warm count= 512
TIMING: fs-write-4k-overwrite count= 512
TIMING: fs-write-4k-overwrite-flush count= 512
"
        ;;
    audio-grants)
        required_markers="
mixer: acquired HDA period grant
"
        ;;
    bench-audio)
        required_markers="
AUDIO-BENCH: COMPLETE
TIMING: audio-mix-and-copy
TIMING: audio-driver-publication-to-mixer
mixer: HDA period IRQ active
"
        ;;
    desktop-display)
        required_markers="
display: gpu not primary, using linear-fb
desktop: wallpaper loaded @nvme:0/Assets/cubit-wallpapers/1/cubes.qoi
desktop: icons loaded @nvme:0/Assets/cubit-icons/4/applications.qoi
desktop: active outputs= 1 primary= 0
desktop: internal shell active
shell: cwd=@nvme:0/
ccl-workbench: native window ready
"
        ;;
    boot-timeline)
        required_markers="
BT-HEADER tsc-hz=
BT-PATH FIRST FRAME PRESENTED: power-on -> goal along spawns and long waits
BT-BUDGET first-frame=
BOOT-TIMELINE: COMPLETE
"
        grep -aE '^(BT-|BOOT-TIMELINE)' "$SERIAL_LOG" || true
        ;;
    desktop-dual-output)
        required_markers="
desktop: active outputs= 2 primary= 0
desktop: internal shell active
desktop: asynchronous frame released
ccl-workbench: native window ready
"
        if [ "${CUBIT_TEST_IDLE_DPI:-0}" = 1 ]; then
            required_markers="
desktop: active outputs= 2 primary= 0
desktop: native output scene rendering active
DPI-CLIENT: ready
ui-app: protected frame published
DPI-CLIENT: closed
"
        fi
        ;;
    desktop-protocol)
        required_markers="
desktop: internal shell active
DESKTOP-DENSITY-TEXT-CHECK: PASS scales=5 faces=2
DESKTOP-FRAME-PAIR-CHECK: PASS frames=12 resize=1
DESKTOP-INPUT-BATCH-CHECK: PASS batches=2 events=12 fallback=1
DESKTOP-PROTOCOL-CHECK: PASS
desktop: dead client buffer acquisition released
"
        ;;
    display-discovery-boot-only)
        required_markers="
display: backend linear-fb
display: output catalog ready
display-check: output BOOT_FRAMEBUFFER 0 SELECTED_FOR_DESKTOP advertised 1024 x 768
DISPLAY-GRANTS-CHECK: PASS
"
        ;;
    display-discovery-multi-output)
        required_markers="
display: output catalog ready
display-check: output BOOT_FRAMEBUFFER 0 SELECTED_FOR_DESKTOP advertised 1024 x 768
display-check: output VIRTIO_GPU 0 BACKEND_READY advertised 1024 x 768
display-check: output VIRTIO_GPU 1 BACKEND_READY advertised 1920 x 1080
display-check: active VIRTIO_GPU 1 1024 x 768
display-check: output VIRTIO_GPU 2 DETECTED_ONLY advertised 1080 x 1920
DISPLAY-GRANTS-CHECK: PASS
"
        ;;
    display-grants)
        required_markers="
display: boot output registered
display: gpu not primary, using linear-fb
DISPLAY-GRANTS-CHECK: PASS
DISPLAY-POOL-CHECK: PASS
DISPLAY-PLANES-CHECK: PASS composited
"
        ;;
    display-dual-output)
        required_markers="
display: backend virtio-gpu
display: second output ready
DISPLAY-CONCURRENT-CHECK: PASS
DISPLAY-DUAL-CHECK: PASS
DISPLAY-GRANTS-CHECK: PASS
"
        ;;
    display-grants-virtio-vga)
        required_markers="
display: boot output registered
display: backend virtio-gpu
display: gpu copy buffer attached
display: output 0 hardware cursor planes 1
DISPLAY-PLANES-CHECK: PASS hardware
DISPLAY-GRANTS-CHECK: PASS
"
        ;;
    bench-input)
        required_markers="
INPUT-BENCH: START
INPUT-BENCH: scenario=CONTINUOUS delivered= 2048
INPUT-BENCH: scenario=PACED delivered= 2048
INPUT-BENCH: scenario=REPAINTING delivered= 2048
INPUT-BENCH: COMPLETE
BENCH: PASS input integrity
"
        ;;
    input-stream)
        required_markers="
desktop: internal shell active
ps2: consumer registered, entering event loop
ccl-workbench: first frame presented
input-stress: publication and recovery PASS
desktop: stats ev=
input-stress: resync reports
"
        ;;
    managed-ui)
        required_markers="
devices: inventory snapshot ready
devices: native window ready
config-inspector: snapshot ready
config-inspector: native window ready
boot-logs: window ready
ui-app: protected frame published
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
    render-launch-policy)
        required_markers="
procmgr: init spawn failed: render-denied.app
procmgr: init spawn failed: render-unavailable.app
procmgr: render admission submitted; child suspended
procmgr: failed launch child stop requested
procmgr: render admission denied; child not resumed
devices: inventory snapshot ready
devices: native window ready
"
        ;;
    mesa-log-bridge)
        required_markers="
MESA-LOG round 1 delivered and retired
MESA-LOG round 2 delivered and retired
TEST: PASS mesa-log-bridge native delivery (NO GPU)
"
        ;;
    mesa-native-instance)
        required_markers="
MESA-NATIVE: instance 0 create=0
MESA-NATIVE: instance 0 enumerate=-3 count=0
MESA-NATIVE: instance 0 destroyed
MESA-NATIVE: instance 1 create=0
MESA-NATIVE: instance 1 enumerate=-3 count=0
MESA-NATIVE: instance 1 destroyed
TEST: PASS native Mesa lifecycle without provider (NO GPU)
"
        ;;
    clipboard)
        required_markers="
clipboard-check: ready
clipboard-check: ungestured paste denied
desktop: clipboard paste denied reason=
desktop: clipboard offer entry= 1 kind=ordinary text= 4
files: pasted 4 characters into the filter
files: pane 1 listed @nvme:0/lost+found entries=
desktop: clipboard offer entry= 2 kind=sensitive
desktop: clipboard paste entry= 2 kind=sensitive consumed
clipboard-check: paste field=name outcome=DONE length= 7
clipboard-check: paste field=name outcome=DONE length= 4
desktop: clipboard manager open entries= 2
desktop: keys to Logs
files: copy 1 names DONE
desktop: clipboard offer entry= 3 kind=ordinary text= 10 typed=yes
files: copy 1 paths DONE
desktop: clipboard offer entry= 4 kind=ordinary text= 18 typed=yes
clipboard-check: paste field=name outcome=DONE length= 18
desktop: clipboard paste entry= 4 kind=ordinary
logs: copy record DONE
desktop: clipboard offer entry= 5 kind=ordinary
logs: search chord DONE
desktop: clipboard manager open entries= 5
clipboard-check: closed
"
        ;;
    key-repeat)
        required_markers="
clipboard-check: ready
clipboard-check: copy field=name outcome=
clipboard-check: closed
"
        ;;
    text)
        required_markers="
text: starting
text: filesystem queue ready
text: native window ready
text: first frame presented
text: opened @nvme:0/Documents/notes.txt lines=1 bytes=27
text: saved @nvme:0/Documents/notes.txt bytes=39
text: closed
"
        ;;
    calculator)
        required_markers="
calculator: starting
calculator: window ready
calculator: first frame presented
calculator: evaluated 2/3 + 1/6 = 5/6 entries=1
calculator: evaluated 2^100 = 1267650600228229401496703205376 entries=2
calculator: evaluated ans / 2^99 = 2 entries=3
calculator: evaluated 100! = 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000 entries=5
calculator: evaluated 1/7 = 1/7 entries=6
calculator: mode SCIENTIFIC
calculator: mode PROGRAMMER
calculator: evaluated 0xFF + 1 = 256 entries=8
calculator: mode STANDARD
calculator: closed
"
        ;;
    usb-hotplug)
        # tests/usb-hotplug/check.py checks both instances and hotplug.
        required_markers="
devmgr: startup complete, entering service loop
"
        ;;
    multi-instance)
        # tests/multi-instance/check.py checks each instance by location.
        required_markers="
devmgr: startup complete, entering service loop
boot-logs: window ready
"
        ;;
    generic-launch)
        # The driver gets its window, DMA and MSI from its manifest, its
        # lines reach logstore (boot-logs prints them), and the device's
        # interrupt arrives after OP_READY. QEMU's edu device has MSI (one
        # vector) and no MSI-X.
        required_markers="
devmgr: match 00:
1234:11E8 class 00.FF.00 msi=1/1 msix=0 -> generic-check.drv (PCI_IDs 1234:11E8..11E8)
devmgr: generic-check.drv launched for
generic-check: stage identify ok
generic-check: stage map ok
generic-check: stage registers ok
generic-check: stage dma ok
devmgr: generic driver ready for
generic-check: stage irq ok
generic-check: bring-up complete
boot-logs: generic-check: bring-up complete
"
        ;;
    startup-health)
        # Every service registers; Files' and the Calculator's "started"
        # records and Desktop's records reach logstore (their publishers are
        # bound: CAP-003 R1/R2); Desktop reads its configured waitboost
        # (its Config scope: live bug 2); netmgr reads its declared net.
        # scope (no wildcard: live bug 4).
        required_markers="
devmgr: config server started
devmgr: filesystem server started
devmgr: procmgr started
devmgr: netstack started
devmgr: netmgr started
logstore: authorized typed diagnostics ready
clock: registered
display: ready
netmgr: pci-00-10-0 static settings applied
netmgr: signaled ready
desktop: internal shell active
files: first frame presented
calculator: window ready
boot-logs: window ready
boot-logs: files: started
boot-logs: calculator: started
boot-logs: desktop: gpu waitboost on (configured)
"
        ;;
    launch-failure)
        required_markers="
procmgr: launch of files.app refused: unreadable capability manifest
desktop: launch rejected token=
desktop: launch failed: Files could not start. Reason: it has no startup table (rebuild it).
desktop: notification posted id=1 source=Files urgency=normal
desktop: notification toast dismissed
"
        ;;
    files)
        required_markers="
files: starting
files: filesystem queue ready
files: native window ready
files: first frame presented
ui-app: protected frame published
files: pane 1 listed @nvme:0/ entries=
files: pane 2 listed @nvme:0/ entries=
files: pane 1 listed @nvme:0/lost+found entries=
files: pane 1 listed @nvme:0/lost+found/nested entries=
files: active pane 2
files: closed
"
        ;;
    desktop-virtio-vga)
        required_markers="
display: backend virtio-gpu
display: output 0 hardware cursor planes 1
desktop: pointer composited (host-pointer cursor planes need absolute input)
virtio-gpu: page flipping active
desktop: active outputs= 1 primary= 0
desktop: internal shell active
shell: cwd=@nvme:0/
"
        ;;
    desktop-vm)
        required_markers="
display: backend virtio-gpu
display: output 0 hardware cursor planes 1
devmgr: virtio-input driver started (MSI-X)
virtio-input: tablet ready
virtio-input: absolute pointer reports published
desktop: absolute pointer source on output 0
desktop: pointer absolute (host-pointer cursor planes eligible: yes)
virtio-gpu: display change head 0 preferred 1280x800
display: output 0 host prefers 1280x800
desktop: output 0 host mode 1280x800
virtio-gpu: head 0 mode 1280x800
display: output 0 mode 1280x800
desktop: output 0 re-laid out 1280x800
virtio-gpu: head 0 mode 800x600
desktop: output 0 re-laid out 800x600
desktop: output 0 re-laid out 1024x768
desktop: internal shell active
"
        ;;
    desktop-doom)
        required_markers="
display: backend virtio-gpu
desktop: internal shell active
DOOM: Running inside desktop surface
desktop: stats
display: stats
mixer: stats
mixer: HDA period IRQ active
"
        ;;
    virtio-gpu-multi-output)
        required_markers="
devmgr: virtio-gpu setup complete
virtio-gpu: scanout0 1024x768 enabled=1
virtio-gpu: scanout1 1920x1080 enabled=1
virtio-gpu: scanout2 1080x1920 enabled=1
virtio-gpu: scanout test frame presented
virtio-gpu: ready
shell: cwd=@nvme:0/
"
        ;;
    virtio-gpu)
        required_markers="
vendor=1AF4 device=1050 class=0380 prog-if=00 virtio-gpu
devmgr: virtio-gpu setup complete
virtio-gpu: transport ready queues=2
virtio-gpu: scanout test frame presented
virtio-gpu: ready
shell: cwd=@nvme:0/
"
        ;;
    virtio-vga-primary)
        required_markers="
vendor=1AF4 device=1050 class=0300 prog-if=00 virtio-gpu
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

if [ "$BENCH_LOAD" = 1 ]; then
    required_markers="$required_markers
BENCH-LOAD: START
BENCH-LOAD: COMPLETE"
fi
if [ "$TEST_NAME" = "desktop-doom" ] && [ "${CUBIT_DOOM_MULTIAPP:-0}" = 1 ]; then
    required_markers="$required_markers
ccl-workbench: native window ready"
fi
case "$TEST_NAME" in
    display-grants|display-grants-virtio-vga|display-dual-output|display-discovery-multi-output|display-discovery-boot-only)
        required_markers="$required_markers
DISPLAY-DISCOVERY-CHECK: PASS
DISPLAY-ASYNC-CHECK: PASS" ;;
    input-stream|bench-input|desktop-protocol|desktop-doom|desktop-display|desktop-virtio-vga|desktop-vm)
        required_markers="$required_markers
desktop: asynchronous presentation active
desktop: asynchronous frame released" ;;
esac
if [ "${CUBIT_GPU_TEST_MODE:-production}" = delayed ] && [ "$TEST_NAME" = display-dual-output ]; then
    required_markers="$required_markers
DISPLAY-STALLED-OUTPUT-CHECK: PASS"
fi
if [ "${CUBIT_GPU_TEST_MODE:-production}" = clear-failure ] && [ "$TEST_NAME" = display-dual-output ]; then
    required_markers="
display: backend virtio-gpu
display: second output ready
virtio-gpu: injected clear rejection head 0
virtio-gpu: injected clear rejection head 1
DISPLAY-DISCOVERY-CHECK: PASS
DISPLAY-BACKEND-FAILURE-CHECK: PASS
DISPLAY-GRANTS-CHECK: PASS"
fi
if [ "${CUBIT_DISPLAY_TEST_MODE:-production}" = output-rebind ]; then
    required_markers="$required_markers
display: test output generation advanced
DISPLAY-OUTPUT-REBIND-CHECK: PASS"
fi
if [ "${CUBIT_DISPLAY_TEST_MODE:-production}" = delayed ] && [ "$TEST_NAME" = input-stream ]; then
    # Input must be dispatched between the delayed reader's two fingerprints,
    # for the SAME frame, not merely while a submission was queued somewhere.
    if ! awk '
        /display-test: hold / { held[$NF] = NR }
        /desktop: input during frame / { input[$NF] = NR }
        /display-test: stable / {
            stable++;
            if (held[$NF] && input[$NF] > held[$NF] && input[$NF] < NR) progressed++;
        }
        END { exit !(stable >= 2 && progressed >= 1) }
    ' "$SERIAL_LOG"; then
        echo "headless: no input progress during a stable delayed frame loan" >&2
        exit 1
    fi
    echo "headless: input progressed during delayed read; transfer remained stable"
fi
# A model that names its vendor must boot with that vendor (see QEMU_CPU_VENDOR).
case "$QEMU_CPU_MODEL" in
    *vendor=*)
        expected_vendor="${QEMU_CPU_MODEL#*vendor=}"
        required_markers="$required_markers
CPU Vendor: ${expected_vendor%%,*}"
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

if [ "$TEST_NAME" = "ufs-write" ]; then
    # What the guest wrote reached the medium: the image file holds it.
    if ! python3 -I "$ROOT_DIR/tests/ufs/check-write.py" "$TEMP_UFS_DISK"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "usb-hotplug" ]; then
    if ! python3 "$ROOT_DIR/tests/usb-hotplug/check.py" "$SERIAL_LOG"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "ccl-console" ] && [ "${CCL_CONSOLE_DEMO:-}" = install ]; then
    # Independent tools read the installed disk: GPT, FAT32 and ext2, and
    # every payload file byte for byte (the payload is deterministic). The
    # offsets are Disk_Layout's for a 260 MiB ESP: 1 MiB, 260 MiB, 261 MiB.
    CHECK_PAYLOAD="$(mktemp -d "${TMPDIR:-/tmp}/cubit-install-check.XXXXXX")"
    if [ -n "${CUBIT_INSTALL_PAYLOAD:-}" ]; then
        cp -r "$CUBIT_INSTALL_PAYLOAD/." "$CHECK_PAYLOAD/"
        # INDEX lines naming a file elsewhere on the medium (@nvme:0, the
        # payload at install/): the installed root holds it too.
        python3 -I -c 'import shutil, sys, pathlib
payload = pathlib.Path(sys.argv[1])
for line in (payload / "INDEX").read_text().splitlines():
    if "\t" in line:
        name, where = line.split("\t")
        target = payload / name
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(payload / where.removeprefix("install/"), target)' "$CHECK_PAYLOAD"
    else
        python3 -I "$ROOT_DIR/tests/disk-layout/make-payload.py" "$CHECK_PAYLOAD/esp" "$CHECK_PAYLOAD/root"
    fi
    if ! bash "$ROOT_DIR/tests/disk-layout/check-image.sh" "$TEMP_UFS_DISK" 4096 \
         1048576 272629760 273678336 "$CHECK_PAYLOAD/esp" "$CHECK_PAYLOAD/root"; then
        rm -rf "$CHECK_PAYLOAD"
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    rm -rf "$CHECK_PAYLOAD"
fi

if [ "$TEST_NAME" = "multi-instance" ]; then
    if ! python3 "$ROOT_DIR/tests/multi-instance/check.py" "$SERIAL_LOG"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

# Refusals (CAP-003): a declaration that fell short fails these tests.
if [ "$TEST_NAME" = "startup-health" ] || [ "$TEST_NAME" = "logs" ]; then
    if ! python3 "$ROOT_DIR/tests/headless/check-refusals.py" "$SERIAL_LOG"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "desktop-doom" ]; then
    if [ "${CUBIT_DOOM_MULTIAPP:-0}" = 1 ] &&
       [ "$(grep -Ec 'desktop: ptr hit-down [0-9]+ [0-9]+ 6$' "$SERIAL_LOG")" -lt 1 ]; then
        echo "headless: multi-app fixture did not close the Workbench window" >&2
        exit 1
    fi
    if ! python3 "$ROOT_DIR/tests/headless/check-doom-frame.py" "${SERIAL_LOG%.log}"; then
        exit 1
    fi
fi

if [ "$BENCH_LOAD" = 1 ]; then
    if ! python3 "$ROOT_DIR/tests/performance/report.py" "$SERIAL_LOG" \
        --require-load --load-workers "$LOAD_WORKERS" >/dev/null; then
        echo "headless: INVALID loaded benchmark (see $SERIAL_LOG)" >&2
        exit 1
    fi
fi

if [ "${DEADLINE_TIMER_TEST:-0}" = 1 ]; then
    if ! python3 "$ROOT_DIR/tests/deadline-ownership/check_native.py" "$SERIAL_LOG" \
        --cpus "$QEMU_CPUS" >/dev/null; then
        echo "headless: invalid deadline timer probe: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "bench-input" ]; then
    if ! python3 "$ROOT_DIR/tests/performance/report.py" "$SERIAL_LOG" \
        --require-input-integrity --load-workers "$LOAD_WORKERS" >/dev/null; then
        echo "headless: INVALID input benchmark (see $SERIAL_LOG)" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "bench-scheduler" ]; then
    if ! python3 "$ROOT_DIR/tests/performance/report.py" "$SERIAL_LOG" \
        --require-compute-control --require-reference-clock >/dev/null; then
        echo "headless: invalid compute control: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "bench-ipc" ]; then
    if ! python3 "$ROOT_DIR/tests/performance/report.py" "$SERIAL_LOG" \
        --require-execution-accounting >/dev/null; then
        echo "headless: invalid native execution accounting: $SERIAL_LOG" >&2
        exit 1
    fi
    if [ "${SHADOW_SCHEDULING:-0}" = 1 ]; then
        python3 "$ROOT_DIR/tests/performance/report.py" "$SERIAL_LOG" \
            --require-shadow-budgets >/dev/null || exit 1
    fi
fi

if [ "$TEST_NAME" = "input-stream" ]; then
    # Workbench used to submit a complete 900x400 surface for every plain
    # pointer report. The stress source publishes 128 motion reports across
    # its rich editor; only semantic hover transitions and actual edits should
    # now submit application surface damage.
    # Retries explicitly request resynchronization too. Match the publisher's
    # actual count across every telemetry interval, never a literal bucket of 1.
    python3 "$ROOT_DIR/tests/headless/check-input-stream.py" "$SERIAL_LOG" || exit 1
fi

if [ "$TEST_NAME" = "desktop-display" ]; then
    # Display copies only a frame's damage: the first scene frame after the
    # bootstrap screen must cover the whole output, or bootstrap pixels stay.
    FIRST_SCENE="$(grep -a 'DESKTOP-FIRST-SCENE: output= 0 ' "$SERIAL_LOG" | head -n 1)"
    if ! printf '%s\n' "$FIRST_SCENE" |
         grep -Eq 'damage= 0 0 ([0-9]+) ([0-9]+) size= \1 \2$'; then
        echo "headless: first desktop scene frame does not cover the output: ${FIRST_SCENE:-missing}" >&2
        exit 1
    fi
    TITLE_DOUBLE_EVENTS="$(grep -F 'desktop: ptr title-double ' "$SERIAL_LOG")"
    if [ "$(printf '%s\n' "$TITLE_DOUBLE_EVENTS" | grep -c ' 1 0$')" -ne 1 ] ||
       [ "$(printf '%s\n' "$TITLE_DOUBLE_EVENTS" | grep -c ' 0 0$')" -ne 1 ]; then
        echo "headless: title double-click did not maximize/restore exactly once" >&2
        exit 1
    fi
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

if [ "$TEST_NAME" = "ccl-console" ] && [ "${CCL_CONSOLE_DEMO:-}" = clipboard ] &&
   [ "$(grep -acF 'ccl-console: REPL completed: Integer: 42' "$SERIAL_LOG")" -lt 2 ]; then
    echo "headless: the pasted literal was not evaluated back to the same value" >&2
    exit 1
fi

if [ "$TEST_NAME" = "clipboard" ]; then
    # Paste once: the secret is delivered exactly once, and no unrequested
    # paste is ever allowed.
    if [ "$(grep -acF 'desktop: clipboard paste entry= 2' "$SERIAL_LOG")" != 1 ]; then
        echo "headless: the sensitive entry was not delivered exactly once" >&2
        exit 1
    fi
    if grep -aF "clipboard-check: ungestured paste ALLOWED" "$SERIAL_LOG" >/dev/null; then
        echo "headless: a paste without a gesture was allowed" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "files" ]; then
    # The read-only grant: making a folder must have been refused.
    if ! grep -aE 'files: status Stopped at x: (not granted|the place is read-only)' \
        "$SERIAL_LOG" >/dev/null; then
        echo "headless: Files did not report the refused folder" >&2
        exit 1
    fi
    # Places are the volumes present (Queue_List_Places), and no status
    # reaches the person as a number (Files_Status).
    if grep -aE 'files: status .*status [0-9]' "$SERIAL_LOG" >/dev/null; then
        echo "headless: Files showed a raw filesystem status" >&2
        exit 1
    fi
fi

if { [ "$TEST_NAME" = "desktop-virtio-vga" ] || [ "$TEST_NAME" = "desktop-vm" ] ||
     [ "$TEST_NAME" = "ccl-workspace" ] || [ "$TEST_NAME" = "ccl-console" ] ||
     [ "$TEST_NAME" = "logs" ] ||
     [ "$TEST_NAME" = "ccl-workbench-virtio-vga" ] ||
     [ "$TEST_NAME" = "desktop-doom" ] ||
     [ "$TEST_NAME" = "virtio-vga-primary" ]; } &&
   grep -F "CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: create failed" \
     "$SERIAL_LOG" >/dev/null 2>&1; then
    echo "headless: display attempted an unsafe received-page re-grant" >&2
    echo "headless: serial log: $SERIAL_LOG" >&2
    exit 1
fi

if [ "$TEST_NAME" = "managed-ui" ]; then
    if [ "$(grep -Fc 'ui-app: protected frame published' "$SERIAL_LOG")" -ne 3 ]; then
        echo "headless: expected one protected publication from each of three applications" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "devices" ]; then
    # HW-002: devmgr binds each present device to the driver that declares
    # it (devices.index) and says so, one line per device; unmatched devices
    # say "-> unmatched" (docs/device-matching.md). Each line carries the
    # function's MSI and MSI-X capabilities: QEMU's virtio-net and NVMe
    # have MSI-X, intel-hda one MSI vector, the host bridge neither.
    for match in \
        '^devmgr: match [0-9A-F:.]+ 1AF4:1000 class 02\.00\.00 msi=(0|1/[0-9]+) msix=1/[0-9]+ -> virtio-net\.drv \(PCI_IDs 1AF4:1000\.\.1000\)' \
        '^devmgr: match [0-9A-F:.]+ 1B36:0010 class 01\.08\.02 msi=(0|1/[0-9]+) msix=1/[0-9]+ -> nvme\.drv \(PCI_Class 01\.08\.02\)' \
        '^devmgr: match [0-9A-F:.]+ 8086:2668 class 04\.03\.00 msi=1/1 msix=0 -> hda\.drv \(PCI_Class 04\.03\.00\)' \
        '^devmgr: match [0-9A-F:.]+ 8086:29C0 class 06\.00\.00 msi=0 msix=0 -> unmatched' \
        '^devmgr: match platform PS2_Controller -> ps2\.drv' \
        '^devmgr: drivers bound to [0-9]+ PCI devices; [0-9]+ matched no driver'; do
        if ! tr -d '\r' < "$SERIAL_LOG" | grep -Eq "$match"; then
            echo "headless: devmgr match line missing: $match" >&2
            exit 1
        fi
    done
    if tr -d '\r' < "$SERIAL_LOG" | grep -Eq '^devmgr: (match .* -> ambiguous|no valid devices\.index)'; then
        echo "headless: devmgr could not bind by the driver index" >&2
        exit 1
    fi
    # A prefix match would accept the old malformed id with two version-header
    # bytes appended. Require exactly the canonical identity from the loader.
    if ! tr -d '\r' < "$SERIAL_LOG" | grep -Fx 'procmgr: pkg id=com.cubit.devices' >/dev/null; then
        echo "headless: Devices package identity is malformed or missing" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "render-launch-policy" ]; then
    if [ "${CUBIT_RENDER_OPTIONAL_TEST:-0}" = 1 ]; then
        if ! python3 "$ROOT_DIR/tests/render-startup/native/check.py" "$SERIAL_LOG"; then
            echo "headless: optional render startup check failed" >&2
            exit 1
        fi
    elif [ "$(grep -Fc 'procmgr: render admission denied; child not resumed' "$SERIAL_LOG")" -ne 2 ] ||
       [ "$(grep -Fc 'procmgr: render admission submitted; child suspended' "$SERIAL_LOG")" -ne 1 ] ||
       [ "$(grep -Fc 'procmgr: failed launch child stop requested' "$SERIAL_LOG")" -ne 2 ] ||
       grep -F 'KILL: denied' "$SERIAL_LOG" >/dev/null ||
       grep -F 'procmgr: render admission complete' "$SERIAL_LOG" >/dev/null ||
       [ "$(grep -Ec 'procmgr: launch attempt com.cubit.test.render-launch-policy pid[0-9]+ withdrawn before it ran' "$SERIAL_LOG")" -ne 2 ] ||
       grep -E 'procmgr: process .* stopped \(killed or faulted\)' "$SERIAL_LOG" >/dev/null; then
        echo "headless: render launch denial count or outcome incorrect" >&2
        exit 1
    fi
fi

if [ "${CUBIT_DESKTOP_METRICS_TEST:-0}" = 1 ]; then
    if [ "${CUBIT_DESKTOP_METRICS_FAULT:-0}" = 1 ] && [ "${CUBIT_DESKTOP_METRICS_STALL:-0}" = 1 ]; then
        if ! grep -F 'TEST: metrics-stall held page unchanged checks=600' "$SERIAL_LOG" >/dev/null ||
           ! grep -F 'TEST: PASS metrics-stall resumed batches=' "$SERIAL_LOG" >/dev/null ||
           grep -F 'desktop: metrics quarantined' "$SERIAL_LOG" >/dev/null; then
            echo "headless: stalled metrics collector did not recover cleanly" >&2
            exit 1
        fi
    elif [ "${CUBIT_DESKTOP_METRICS_FAULT:-0}" = 1 ]; then
        if ! grep -F 'TEST: metrics-fault invalid count injected' "$SERIAL_LOG" >/dev/null ||
           ! grep -E 'desktop: metrics quarantined dropped=[0-9]+ invalid=1 rejected=0' "$SERIAL_LOG" >/dev/null; then
            echo "headless: malformed metrics reply did not quarantine telemetry" >&2
            exit 1
        fi
    elif [ "${CUBIT_DESKTOP_METRICS_LOAD:-0}" = 1 ]; then
        python3 "$ROOT_DIR/tests/compositor/check-metrics-load.py" "$SERIAL_LOG" || exit 1
    elif ! grep -F 'TEST: PASS desktop-metrics frames=' "$SERIAL_LOG" >/dev/null ||
         grep -F 'desktop: metrics quarantined' "$SERIAL_LOG" >/dev/null; then
        echo "headless: Desktop metric publication/observation failed" >&2
        exit 1
    fi
fi

FAULT_SIGNATURE='panic|assert|double fault|triple fault|general protection|machine check exception|^EXCEPTION:|deadlock|TEST: FAIL|BENCH: FAIL|CLOCK: FAIL|SCHED-ALARM: fallback|failed launch .*cleanup rejected'
if grep -Ei "$FAULT_SIGNATURE" "$SERIAL_LOG" >/dev/null 2>&1; then
    echo "headless: fault signature found in serial log: $SERIAL_LOG" >&2
    grep -Ein "$FAULT_SIGNATURE" "$SERIAL_LOG" >&2
    exit 1
fi

if [ "$TEST_NAME" = "ccl-workspace" ]; then
    if [ "$(grep -Fc 'ccl-workbench: live label SAMPLED' "$SERIAL_LOG")" -ne 2 ] ||
       grep -F 'ccl-workbench: live label FAULTED' "$SERIAL_LOG" >/dev/null; then
        echo "headless: live clock did not sample successfully across its timer wake" >&2
        exit 1
    fi
    first_source="$(debugfs -R 'cat work/ccl-0001.ccl' "$TEMP_DISK" 2>/dev/null)"
    second_source="$(debugfs -R 'cat work/ccl-0002.ccl' "$TEMP_DISK" 2>/dev/null)"
    named_source="$(debugfs -R 'cat work/clock.ccl' "$TEMP_DISK" 2>/dev/null)"
    quoted_source="$(debugfs -R 'cat work/quoted.ccl' "$TEMP_DISK" 2>/dev/null)"
    if [ "$first_source" != "41" ] || [ "$second_source" != "42" ] ||
       [ "$named_source" != "42" ] || [ "$quoted_source" != '"hello!!"' ]; then
        echo "headless: saved source revisions were not preserved correctly" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "capability-security" ]; then
    python3 "$ROOT_DIR/tests/process-construction/check-log.py" "$SERIAL_LOG"
fi
if [ "$TEST_NAME" = "text" ]; then
    saved_text="$(debugfs -R 'cat Documents/notes.txt' "$TEMP_DISK" 2>/dev/null)"
    expected_text="$(cat "$ROOT_DIR/tests/headless/text-notes.txt")
hello cubit"
    if [ "$saved_text" != "$expected_text" ]; then
        echo "headless: Text saved the wrong bytes: \"$saved_text\"" >&2
        exit 1
    fi
    if debugfs -R 'ls Documents' "$TEMP_DISK" 2>/dev/null | grep -F '.notes.txt.text-save' >/dev/null; then
        echo "headless: Text left its save sibling behind" >&2
        exit 1
    fi
    if ! python3 "$ROOT_DIR/tests/headless/text-frame-check.py" "${SERIAL_LOG%.log}-text-moves.ppm"; then
        echo "headless: Text showed a stale row after cursor moves" >&2
        exit 1
    fi
    if [ "$(grep -acF 'text: press ctrl=0 alt=0 cursors=1' "$SERIAL_LOG")" -lt 2 ] ||
       grep -aE 'text: (press .*cursors=[2-9]|cursors [2-9])' "$SERIAL_LOG" >/dev/null; then
        echo "headless: two plain clicks after Ctrl+S did not leave one cursor" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "calculator" ]; then
    # A failed Enter (division by zero) adds nothing: the next entry is
    # the seventh.
    if grep -aF "evaluated 1/(3-3)" "$SERIAL_LOG" >/dev/null ||
       ! grep -aF "evaluated sin(1) = 0.8414709848078965066" "$SERIAL_LOG" | grep -qF "entries=7"; then
        echo "headless: the Calculator recorded a failed evaluation" >&2
        exit 1
    fi
    if [ -n "${CALC_SCREENSHOT_DIR:-}" ]; then
        mkdir -p "$CALC_SCREENSHOT_DIR"
        python3 "$ROOT_DIR/tests/text-editor/ppm_to_png.py" "${SERIAL_LOG%.log}-calculator.ppm" \
          "${SERIAL_LOG%.log}-calculator-scientific.ppm" "${SERIAL_LOG%.log}-calculator-programmer.ppm" \
          --into "$CALC_SCREENSHOT_DIR"
    fi
fi
if [ "$CHECK_EXT2" -eq 1 ]; then
    if ! python3 "$ROOT_DIR/tests/filesystem-interop/check-native.py" after \
      "$TEMP_DISK" "${SERIAL_LOG}.ext2.json"; then
        echo "headless: Ext2 interoperability postcheck failed" >&2
        exit 1
    fi
fi
HEADLESS_TEST_FAILED=0
if [ "$CONFIG_STORAGE_TEST" = 1 ]; then
    CONFIG_ORACLE_ARGS=()
    if [ "$TEST_NAME" != "config-storage" ]; then CONFIG_ORACLE_ARGS+=(--objects); fi
    if [ "$TEST_NAME" = config-objects-benchmark ]; then
        CONFIG_ORACLE_ARGS+=(--benchmark)
        python3 "$ROOT_DIR/tests/config-object-client/native-app/report-benchmark.py" "$SERIAL_LOG" || exit 1
    fi
    if [ -n "$CONFIG_EXPORT" ]; then CONFIG_ORACLE_ARGS+=(--export-dir "$CONFIG_EXPORT"); fi
    if ! python3 "$ROOT_DIR/tests/config-turso/native/check-storage-disk.py" "$TEMP_DISK" "${CONFIG_ORACLE_ARGS[@]}"; then
        HEADLESS_TEST_FAILED=1
        echo "headless: Config storage database postcheck failed" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "turso-native" ]; then
    if ! python3 "$ROOT_DIR/tests/config-turso/native/check-disk.py" "$TEMP_DISK" \
      --revision "$TURSO_REVISION" --export-dir "$TURSO_EXPORT"; then
        HEADLESS_TEST_FAILED=1
        echo "headless: native Turso disk postcheck failed" >&2
        exit 1
    fi
fi
echo "headless: PASS $TEST_NAME"
echo "headless: serial log: $SERIAL_LOG"
if [ -n "$TEMP_AUDIO" ]; then echo "headless: audio capture: $TEMP_AUDIO"; fi
