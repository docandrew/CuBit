#!/usr/bin/env bash
set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
KERNEL_DIR="$ROOT_DIR/kernel"
# LOG-004 overflow check (CCL_CONSOLE_DEMO=kernel-log): every kernel build
# this script makes uses a 64-record kernel log ring.
if [ "${KERNEL_LOG_OVERFLOW:-0}" = 1 ]; then
    export KERNEL_LOG_RECORDS=64
fi
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
# PWR-003: boot, the button press, every stage's grace, then power-off.
SHUTDOWN_TIMEOUT_SECONDS=90
NETWORK_AUTHORITY_TIMEOUT_SECONDS=100
# net-link and net-multi: boot, DHCP, and each pulled cable (tests/net-link).
NET_LINK_TIMEOUT_SECONDS=150
# vm-linux boots the guest twice (one vCPU, then two), each bounded at 60 s.
VM_LINUX_TIMEOUT_SECONDS=240
# tsc-*: CPU 0's 10 s clock report (Time.noteTSC) must come three times.
TSC_SYNC_TIMEOUT_SECONDS=60
BENCH_LATENCY_TIMEOUT_SECONDS=300
# PERF-010 bench-build: two apps, cold, warm and edited, on CuBit.
BENCH_BUILD_TIMEOUT_SECONDS=2400
TIMEOUT_SECONDS=""
BUILD_WORLD=0
KEEP_LOGS=0
CHECK_EXT2=0
QEMU_ACCEL=""
QEMU_CPUS=4
# Guest memory; cases that need more (servo) raise the default. The
# default is 256 MiB: the idle bootstrap set takes about 81 MiB of frames
# (netstack 19.6, procmgr 14.8, filesystem 11.4, tls 10.8 at 2026-10-31),
# and 128 MiB (about 106 usable) ran tls-service out of pages (MEM-004).
QEMU_MEMORY="${QEMU_MEMORY:-}"
QEMU_CPUS_EXPLICIT=0
QEMU_DISPLAY=none
INPUT_BACKEND=linear
VCPU_CPUS=""
BENCH_LOAD=0
LOAD_WORKERS=1
# Heap loads init-bench-audio-heap.ccl starts (--load-kind heap).
HEAP_LOADS_EXPECTED=3
# --load-kind windows: its sbrk loads, and its BENCH-LOAD peers (the three
# heap loads).
SBRK_LOADS_EXPECTED=3
WINDOW_LOADS_EXPECTED=3
LOAD_KIND=cpu
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
AUDIO_KEPT_DIR=""
TEMP_SECOND_NVME=""
TEMP_OPTICAL_TREE=""
TEMP_STORAGE_FIXTURE=""
# --test ufs: the GPT disk behind QEMU's UFS logical unit, and its devices.
TEMP_UFS_DISK=""
UFS_ARGS=()
# --iommu (IOMMU-001): boot on QEMU's VT-d model (tests/headless/check-iommu.py).
IOMMU=0
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
  --test NAME          Test to run: boot-shell-nvme, boot-memory-8g, boot-memory-16g, boot-memory-32g, boot-large-module, async-ipc, bench-ipc, ccl-vm, ccl-workbench, ccl-workbench-virtio-vga, ccl-workspace, ccl-console, logs, ccl-remote, capability-security, network-authority, bench-net, bench-fs, bench-cualloc, threads, futex, secondary-stacks, ada-runtime, exit-status, clock, avx, wifi-crypto, rust-std, libc, processes, control-events, binutils, gcc, gnat, ccl-build-native, servo, bench-spread, timesync, tls-probe, tls-service, wget-https, llm-mock, agent-mock, agent-app, llm-live, storage-grants, audio-grants, desktop-display, large-manifest, desktop-protocol, display-grants, display-grants-virtio-vga, input-stream, devices, managed-ui, files, text, calculator, ui-latency, lexicon, dictionary, bible, clipboard, key-repeat, desktop-doom, desktop-virtio-vga, desktop-vm, desktop-session, desktop-session-no-invtsc, desktop-launch, display-latency, metrics-latency, metricsview, virtio-gpu, virtio-vga-primary, launch-failure, fs-coherence-CASE, startup-health, firmware-snapshot, acpi-hardware, acpi-pci-config, acpi-firmware-data, remote-screenshot, device-build, service-switch, service-switch-reboot, desktop-live-replace, console-run-staged, bench-build, nvme-timeout, nvme-stuck, tsc-sync, tsc-skew, tsc-skew-noadjust, or boot-timeline (--accel kvm --timeout 60|--test NAME          Test to run: boot-shell-nvme, boot-memory-8g, boot-memory-16g, boot-memory-32g, boot-large-module, async-ipc, bench-ipc, ccl-vm, ccl-workbench, ccl-workbench-virtio-vga, ccl-workspace, ccl-console, logs, ccl-remote, capability-security, network-authority, bench-net, bench-fs, bench-cualloc, threads, futex, secondary-stacks, ada-runtime, exit-status, clock, avx, wifi-crypto, rust-std, libc, processes, control-events, binutils, gcc, gnat, ccl-build-native, servo, bench-spread, timesync, tls-probe, tls-service, wget-https, llm-mock, agent-mock, agent-app, llm-live, net-fetch, storage-grants, audio-grants, desktop-display, large-manifest, desktop-protocol, display-grants, display-grants-virtio-vga, input-stream, devices, managed-ui, files, text, calculator, ui-latency, lexicon, dictionary, bible, clipboard, key-repeat, desktop-doom, desktop-virtio-vga, desktop-vm, desktop-session, desktop-session-no-invtsc, desktop-launch, display-latency, metrics-latency, metricsview, virtio-gpu, virtio-vga-primary, launch-failure, fs-coherence-CASE, startup-health, firmware-snapshot, acpi-hardware, acpi-pci-config, acpi-firmware-data, remote-screenshot, device-build, service-switch, service-switch-reboot, desktop-live-replace, console-run-staged, bench-build, nvme-timeout, nvme-stuck, or boot-timeline (--accel kvm --timeout 60)
  --test NAME          Test to run: boot-shell-nvme, boot-memory-8g, boot-memory-16g, boot-memory-32g, boot-large-module, async-ipc, bench-ipc, ccl-vm, ccl-workbench, ccl-workbench-virtio-vga, ccl-workspace, ccl-console, logs, ccl-remote, ccl-remote-session, capability-security, network-authority, bench-net, bench-fs, bench-cualloc, threads, futex, secondary-stacks, ada-runtime, clock, avx, wifi-crypto, rust-std, libc, processes, control-events, binutils, gcc, gnat, ccl-build-native, servo, bench-spread, timesync, tls-probe, tls-service, wget-https, storage-grants, audio-grants, desktop-display, desktop-protocol, display-grants, display-grants-virtio-vga, input-stream, devices, managed-ui, files, text, calculator, ui-latency, lexicon, dictionary, bible, clipboard, key-repeat, desktop-doom, desktop-virtio-vga, desktop-vm, virtio-gpu, virtio-vga-primary, launch-failure, fs-coherence-CASE, startup-health, firmware-snapshot, acpi-hardware, acpi-pci-config, acpi-firmware-data, or boot-timeline (--accel kvm --timeout 60)
  --timeout SECONDS    QEMU runtime before timeout is treated as success
                       (default 25; 180 for desktop tests, see DESKTOP_TIMEOUT_SECONDS)
  --accel NAME         QEMU accelerator (for example: tcg,thread=multi)
  --cpus COUNT         Virtual CPUs, 1..4 (default: 4)
  --display MODE       QEMU frontend (default: none; gtk,zoom-to-fit=on for UI regression)
  --input-backend MODE bench-input: linear or virtio (default: linear)
  --vcpu-cpus LIST     Pin vCPU 0,1,... to distinct Linux CPUs (e.g. 2,3,4,5; KVM only)
  --load               Add one busy peer (bench-ipc / bench-audio / bench-input)
  --load-workers COUNT Add 1..4 busy peers (bench-input only)
  --load-kind KIND     With --load: cpu (default, spinning peers), heap
                       (three bench-heap-load.app: each commits and releases
                       32 MiB in a loop; bench-audio only) or windows (heap
                       loads, sbrk loads and CPU peers: interrupt-window
                       preemption and migration; bench-audio only)
  --disk PATH          Base ext2 disk image (default: kernel/nvme_disk.img)
  --serial PATH        Serial log path (default: /tmp/cubit-headless-*.log)
  --pcap PATH          Packet capture path (default: /tmp/cubit-headless-*.pcap)
  --keep-logs          Leave logs (and the guest's disk) in place after a run
  --turso-revision N  turso-native: expected saved revision, 1 (seed) or 2 (fresh boot)
  --turso-export DIR  turso-native: save validated disk and SQLite in a NEW directory
  --config-export DIR config-objects/reopen/benchmark: save validated disk and SQLite/WAL in a NEW directory
  --check-ext2         bench-storage: verify Linux/CuBit/Linux inode reuse, data and e2fsck
  --iommu              Add QEMU's VT-d model (intel-iommu, intremap=on, aw-bits
                       CUBIT_IOMMU_AW_BITS, default 39 as on ADL-N; virtio-net
                       modern-only with iommu_platform=on) and check the IOMMU
                       is active with no DMA fault (generic-launch: the fault
                       probe gives exactly one typed DMA fault). Without it
                       every test checks the typed "IOMMU degraded" record.
  -h, --help           Show this help

The suite boots the NVMe profile headlessly and checks serial output for
stable pass markers.
Performance fixtures: bench-ipc, bench-audio, bench-storage, bench-input, bench-scheduler.
bench-fs: tests/fs-bench (build tests/fs-bench/build-cubit.sh first).
bench-cualloc: tests/userspace-allocator/cubit (build its build-cubit.sh first; --accel kvm).
fs-coherence-CASE: one cross-process coherence case, CASE from
  tests/fs-coherence/cases.txt (build tests/fs-coherence/build-cubit.sh first).
fat32-volume: FAT32 (FS-030) on a second NVMe disk, an MBR stick image the
  host makes and checks after the run with fsck.fat -n and mtools
  (tests/filesystem-fat/native; build its build-cubit.sh first).
usb-disk: USB mass-storage disks (SELF-003): the FAT32 suite on a USB stick
  (@usb:0), a second stick hot-plugged (@usb:1) and removed while written, a
  replacement hot-plugged as @usb:1; the host checks stick 1 with fsck.fat -n
  (tests/usb-disk; build tests/usb-disk/native/build-cubit.sh first).
usb-eject: USB sticks after boot (FS-034): plug in, write, Eject (refused
  while a file is open, then safe), pull out, plug in again (data intact), a
  second stick on the same controller, a surprise removal while written (a
  dirty stick comes back read only); the host checks the images with
  fsck.fat -n (tests/usb-eject; build tests/usb-eject/native/build-cubit.sh).
bench-latency: tests/sched-latency (build tests/sched-latency/build-cubit.sh
  and bench-ipc-server first); defaults to 300 s. QEMU quits at its done marker.
Logging fixture: log-authority (build logstore procmgr clock log-check first).
Observability: log-fields (build logstore log-fields-check first); console-route
(build logstore-test console-route-check first); late-introduction (build
logstore-test late-intro-check first); log-retention
(build logstore log-retention-check first); metrics
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
screen-settle: Files on the desktop; a held Super, then one key at a time, each on
  the screen within 0.5 s with no further input (BUG-007; tests/screen-settle/settle.py,
  --accel kvm). SCREEN_SETTLE_HOLD=sendkey holds with QEMU's sendkey hold time instead,
  which delays the next keys in QEMU's own queue.
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
        --load-kind)
            case "${2:-}" in
                cpu|heap|windows) LOAD_KIND="$2"; BENCH_LOAD=1 ;;
                *) echo "headless: --load-kind must be cpu, heap or windows" >&2; exit 2 ;;
            esac
            shift 2
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
        --iommu)
            IOMMU=1
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
        desktop-display|desktop-session|desktop-session-no-invtsc|desktop-protocol|files|text|calculator|ui-latency|lexicon|dictionary|bible|debugger-gui|debugger-gui-rs|clipboard|key-repeat|input-stream|launch-failure|startup-health|desktop-restart|desktop-reap|shutdown-console|shutdown-text|shutdown-desktop|shutdown-desktop-cancel|power-sources|graphics-report-nodriver|graphics-report-failing|graphics-report-desktop-down)
            TIMEOUT_SECONDS=$DESKTOP_TIMEOUT_SECONDS ;;
        desktop-launch)
            # Five launches and three closes; the driver quits when done.
            TIMEOUT_SECONDS=300 ;;
        shutdown|acpi-shutdown)
            TIMEOUT_SECONDS=$SHUTDOWN_TIMEOUT_SECONDS ;;
        network-authority)
            TIMEOUT_SECONDS=$NETWORK_AUTHORITY_TIMEOUT_SECONDS ;;
        net-link|net-multi)
            TIMEOUT_SECONDS=$NET_LINK_TIMEOUT_SECONDS ;;
        vm-linux)
            TIMEOUT_SECONDS=$VM_LINUX_TIMEOUT_SECONDS ;;
        tsc-sync|tsc-skew|tsc-skew-noadjust)
            TIMEOUT_SECONDS=$TSC_SYNC_TIMEOUT_SECONDS ;;
        vm-desktop)
            TIMEOUT_SECONDS=$DESKTOP_TIMEOUT_SECONDS ;;
        bench-latency)
            TIMEOUT_SECONDS=$BENCH_LATENCY_TIMEOUT_SECONDS ;;
        bench-build)
            TIMEOUT_SECONDS=$BENCH_BUILD_TIMEOUT_SECONDS ;;
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
    grant-forward|grant-forward-intermediary-exit|grant-forward-owner-exit|grant-forward-desktop|config-tree|config-inspection|log-authority|log-fields|log-retention|console-route|late-introduction|metrics|rust-native|turso-native-std|turso-native|virtio-gpu-multi-output|display-discovery-multi-output|display-discovery-boot-only|desktop-dual-output)
        ;;
    boot-shell-nvme|boot-memory-8g|boot-memory-16g|boot-memory-32g|boot-large-module|async-ipc|bench-ipc|bench-audio|bench-storage|bench-input|bench-scheduler|ccl-vm|ccl-workbench|ccl-workbench-virtio-vga|ccl-workspace|ccl-console|lexicon|dictionary|bible|debugger-gui|debugger-gui-rs|logs|metricsview|ccl-remote|capability-security|network-authority|bench-net|bench-fs|bench-cualloc|kern014-bench|threads|futex|wait|secondary-stacks|ada-runtime|exit-status|clock|avx|rust-std|libc|ladybird-js|ladybird-test262|processes|control-events|binutils|gcc|gnat|ccl-build-native|servo|bench-spread|timesync|tls-probe|tls-service|wget-https|storage-grants|audio-grants|desktop-display|desktop-protocol|display-grants|display-grants-virtio-vga|display-dual-output|input-stream|devices|managed-ui|files|text|calculator|neon-blast|ui-latency|clipboard|key-repeat|desktop-doom|desktop-virtio-vga|desktop-vm|display-latency|metrics-latency|virtio-gpu|virtio-vga-primary|bench-latency|boot-timeline|devmem-denials|process-kill|oom-reclaim|scanout-repair|launch-failure|fs-coherence-*|startup-health|generic-launch|ufs|ufs-write|net-link|net-multi|firmware-snapshot|acpi-hardware|acpi-pci-config|acpi-firmware-data|platform-pool|gpio-launch|acpi-one-service|acpi-sci|acpi-shutdown|multi-instance|fat32-volume|usb-disk|usb-eject|program-parameters|supervision|shutdown|shutdown-console|shutdown-text|shutdown-desktop|shutdown-desktop-cancel|startup-wait|large-manifest|usb-hotplug|usb-slots|wifi-crypto|trap-flag|nmi|direction-flag|alignment-flag|kernel-map-probe|mov-ss-debug|llm-mock|agent-mock|agent-app|llm-live|ccl-remote-session|cpu-times|desktop-restart|desktop-reap|font-masks|remote-screenshot|device-build|service-switch|service-switch-reboot|acpi-live-replace|desktop-live-replace|console-run-staged|bench-build|nvme-timeout|nvme-stuck|futex-share|mailbox-self|mailbox-mutual|debug-engine|debug-engine-rs|cpu-refusal|tlb-tags|spawn-rules|debug-observe|launch-gate|debug-kernel|debug-gate-disabled|debug-gate-production|debug-gate-development|virt-report|virt-report-none|vm-hello|vm-linux|vm-desktop|desktop-session|desktop-session-no-invtsc|desktop-launch|power-sources|graphics-report-nodriver|graphics-report-failing|graphics-report-desktop-down|clock-keeper|xhci-bringup|tsc-sync|tsc-skew|tsc-skew-noadjust|net-fetch|screen-settle)
        ;;
    *)
        echo "headless: unknown test: $TEST_NAME" >&2
        exit 2
        ;;
esac
# desktop-session-no-invtsc: desktop-session on run-desktop's CPU without
# the invariant TSC (QEMU_RUN_CPU before +invtsc, or a host or TCG without
# it): no clock publication, so user space and the kernel's timer waits
# both run on the millisecond word.
DESKTOP_SESSION_NO_INVTSC=0
if [ "$TEST_NAME" = "desktop-session-no-invtsc" ]; then
    TEST_NAME=desktop-session
    DESKTOP_SESSION_NO_INVTSC=1
fi
# desktop-launch (BUG-008): desktop-session, then apps launched one after
# another from the Apps menu by keyboard and by tablet clicks, and closed
# with the pointer (tests/headless/desktop-launch.py).
DESKTOP_SESSION_LAUNCH=0
if [ "$TEST_NAME" = "desktop-launch" ]; then
    TEST_NAME=desktop-session
    DESKTOP_SESSION_LAUNCH=1
fi
# screen-settle (BUG-007): the files test's boot and Files, then one key at
# a time with no further input; each result must reach the screen (QEMU's
# screendump of the scanout) within a bound (tests/screen-settle/settle.py).
SCREEN_SETTLE=0
if [ "$TEST_NAME" = "screen-settle" ]; then
    TEST_NAME=files
    SCREEN_SETTLE=1
fi

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
    if [ -n "${LARGE_MODULE:-}" ]; then
        rm -f "$LARGE_MODULE"
    fi
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
    elif [ -n "$TEMP_AUDIO" ] && [ -f "$TEMP_AUDIO" ]; then
        # Kept (--keep-logs, or a failed run) beside the serial log.
        mv -f "$TEMP_AUDIO" "$AUDIO_KEPT_DIR/" 2>/dev/null || true
    fi
    if [ "$KEEP_LOGS" -eq 0 ] && [ "${HEADLESS_TEST_FAILED:-0}" -eq 0 ]; then
        rm -f "$NET_PCAP"
    fi
    if [ -n "${SERIAL_FIFO_DIR:-}" ]; then
        rm -rf "$SERIAL_FIFO_DIR"
    fi
}
trap cleanup EXIT INT TERM

HEADLESS_TEST_FAILED=1

# The signing directory (SIGN-001): images name its CA, and the builds below
# sign with its key. Named explicitly, never a default: a run never reads or
# creates the person's own key (~/.config/cubit/signing) behind their back.
if [ -z "${CUBIT_SIGNING:-}" ]; then
    echo "headless: CUBIT_SIGNING is unset: name the signing directory to use" \
         "(make -C kernel signing-key CUBIT_SIGNING=DIR makes one)" >&2
    exit 1
fi
export CUBIT_SIGNING

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
    log-fields|log-retention|console-route|late-introduction|metrics)
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
            # path (tests/install-reboot and tests/install-generations boot
            # it afterwards); CUBIT_INSTALL_DISK_MIB sizes it (1024).
            # CUBIT_INSTALL_REINSTALL=1 installs over what that disk holds
            # (not refilled), and CUBIT_INSTALL_INTERRUPT_AT=<serial line>
            # cuts the power (QEMU quits) when the install logs that line
            # (SYS-002: an interrupted install must leave no bootable disk).
            INIT_PROFILE="$ROOT_DIR/tests/headless/init-ccl-console-install.ccl"
            INSTALL_DISK_MIB="${CUBIT_INSTALL_DISK_MIB:-1024}"
            if [ -n "${CUBIT_INSTALL_DISK:-}" ]; then
                TEMP_UFS_DISK="$CUBIT_INSTALL_DISK"
            else
                TEMP_UFS_DISK="$(mktemp "${TMPDIR:-/tmp}/cubit-ufs-disk.XXXXXX.img")"
            fi
            if [ "${CUBIT_INSTALL_REINSTALL:-0}" != 1 ]; then
                python3 -I -c 'import sys
chunk = bytes([0xA5]) * (1 << 20)
with open(sys.argv[1], "wb") as f:
    for _ in range(int(sys.argv[2])):
        f.write(chunk)' "$TEMP_UFS_DISK" "$INSTALL_DISK_MIB"
            fi
            UFS_ARGS=(-drive "file=$TEMP_UFS_DISK,if=none,id=ufs0,format=raw"
                      -device ufs,serial=cubitufs,id=ufs
                      -device ufs-lu,drive=ufs0,bus=ufs,lun=0)
        fi
        if [ "${CCL_CONSOLE_DEMO:-}" = log-exited ]; then
            # LOG-003: log-exit-probe logs and exits at boot; its records
            # must still be found by name (logs.recent) and named (boot-logs).
            INIT_PROFILE="$ROOT_DIR/tests/headless/init-ccl-console-log-exited.ccl"
        fi
        if [ "${CCL_CONSOLE_DEMO:-}" = kernel-log ]; then
            # LOG-004: crash-probe dies of an unhandled exception; its last
            # words reach logstore only through the kernel log ring.
            INIT_PROFILE="$ROOT_DIR/tests/headless/init-ccl-console-kernel-log.ccl"
        fi
        if [ "${CCL_CONSOLE_DEMO:-}" = log-burst ]; then
            # LOG-005: log-burst-probe's 1000 startup records, then 5000;
            # logs.recent returns them or typed gaps with exact counts.
            INIT_PROFILE="$ROOT_DIR/tests/headless/init-ccl-console-log-burst.ccl"
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
    bible)
        # The Bible (APP-005): the app reads its translations (about 9 MB
        # each) from the cubit-bible asset package.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-bible.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
        ;;
    logs)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-logs.ccl"
        ;;
    debugger-gui)
        # DBG-001: the Debugger attached to debug-demo (cooperative preview).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-debugger-gui.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-2G}"
        ;;
    debugger-gui-rs)
        # DBG-001: the same, attached to the Rust demo (debug-demo-rs).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-debugger-gui-rs.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-2G}"
        ;;
    metricsview)
        # MET-002's app: Desktop's display latency and its own, live from
        # metrics.svc (tests/headless/init-metricsview.ccl).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-metricsview.ccl"
        ;;
    process-kill)
        # A process the kernel kills for a fault is recorded, logged and
        # shown on the Desktop bootstrap screen.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-process-kill.ccl"
        ;;
    desktop-reap)
        # IPC-009: a client holding a window but no ring and no buffer ends
        # without goodbye; nothing redraws after it. Desktop still removes
        # its window (tests/headless/init-desktop-reap.ccl, reap-check).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-desktop-reap.ccl"
        ;;
    trap-flag)
        # SYSCALL with RFLAGS.TF set: the kernel masks TF on entry and
        # survives; the process dies for the user-mode single-step trap.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-trap-flag.ccl"
        ;;
    direction-flag)
        # A user process runs with RFLAGS.DF set while timer interrupts and
        # page faults enter the kernel: the entry stubs must clear DF before
        # any compiled kernel code (REP MOVS/STOS) runs.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-direction-flag.ccl"
        ;;
    alignment-flag)
        # A user process runs with RFLAGS.AC set while timer interrupts and
        # page faults enter the kernel: the entry stubs must clear AC, or
        # interrupt handlers run with SMAP off (SEC-026). Needs a kernel
        # built SMAP_ENTRY_PROBE=1 and a CPU model with SMAP.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-alignment-flag.ccl"
        ;;
    kernel-map-probe)
        # KERN-014: what the kernel's own map reaches. Needs a kernel built
        # KERN014_PROBE=1 (make -C kernel world KERN014_PROBE=1 after
        # removing kernel/build/libcubit.a, so build.ads is regenerated).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-kernel-map-probe.ccl"
        ;;
    mov-ss-debug)
        # DBG-001 (CVE-2018-8897 class): a data breakpoint deferred past MOV SS
        # fires on the kernel's first instruction after SYSCALL or INT3. Needs
        # a kernel built DEBUG_REGISTER_PROBE=1 (make -C kernel world
        # DEBUG_REGISTER_PROBE=1 after removing kernel/build/libcubit.a). One
        # CPU, so the armed breakpoint stays with the process that asked.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-mov-ss.ccl"
        if [ "$QEMU_CPUS_EXPLICIT" = 0 ]; then QEMU_CPUS=1; fi
        ;;
    nmi)
        # NMIs injected while a user process spins: the kernel's NMI path
        # must leave CR3 and the GS base as it found them. One CPU, so the
        # NMI lands in the spinning process and the kernel's NMI line is
        # not interleaved with other CPUs' copies of it.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-nmi.ccl"
        if [ "$QEMU_CPUS_EXPLICIT" = 0 ]; then QEMU_CPUS=1; fi
        ;;
    desktop-restart)
        # BUG-001: the desktop fails once on purpose (CUBIT_DESKTOP_IMAGE, a
        # CUBIT_DESKTOP_FAULT=exit-once build); procmgr restarts it and the
        # display leases the output to the restarted desktop.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-desktop-restart.ccl"
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
    launch-gate)
        # CAP-012: launches attenuated by the launcher's table and rights.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-launch-gate.ccl"
        ;;
    ccl-remote)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ccl-remote.ccl"
        ;;
    ccl-remote-session)
        # The LAN remote session (tests/ccl-remote/remote_session.py drives it).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ccl-remote-session.ccl"
        ;;
    remote-screenshot)
        # Remote screenshots (REMOTE-001; tests/ccl-remote/remote_screenshot.py).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-remote-screenshot.ccl"
        ;;
    service-switch|service-switch-reboot)
        # service-switch-reboot (--disk: the disk a service-switch run kept,
        # --keep-logs): its next boot, where procmgr applies the generation
        # made current for the next boot (tests/service-switch).
        # System generations (CuBit.Service_Replacement): lexicon.svc
        # (transparent) pinned to programs in the storehouse, switched and
        # rolled back through ccl-remote, and clock.svc (graceful) refused
        # (tests/service-switch/service_switch.py).
        INIT_PROFILE="$ROOT_DIR/tests/service-switch/init.ccl"
        ;;
    acpi-live-replace)
        # LIVE-001 for ACPI: the one acpi.svc (replace graceful) switched
        # through system generations over ccl-remote, its hardware grants
        # retired and granted again each time, a crash and a hang rolled
        # back, an unmet stop grace aborted with the generation kept
        # (tests/acpi-live-replace/acpi_live_replace.py). Build: make -C
        # kernel world acpi-native. As acpi-one-service: q35 with edu and
        # the regression SSDT, 2 GiB.
        INIT_PROFILE="$ROOT_DIR/tests/acpi-live-replace/init.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-2G}"
        ;;
    desktop-live-replace)
        # LIVE-002: desktop.svc (restart on-failure) (replace graceful), as
        # in the laptop plans, switched through system generations over
        # ccl-remote: Stop hands the display back and exits 0, the new
        # program presents again; a crash rolled back; rollback to the
        # image (tests/desktop-live-replace/desktop_live_replace.py).
        # 2 GiB: the app-replace survey runs every shipped app at once.
        INIT_PROFILE="$ROOT_DIR/tests/desktop-live-replace/init.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-2G}"
        ;;
    device-build|console-run-staged|bench-build)
        # bench-build (PERF-010): the same system, timing builds instead
        # (tests/device-builds/bench_build.py); KVM only, numbers otherwise
        # do not compare.
        if [ "$TEST_NAME" = bench-build ] && [ "$QEMU_ACCEL" != kvm ]; then
            echo "headless: bench-build measures; run it with --accel kvm" >&2
            exit 2
        fi
        # SELF-002: an app built ON CuBit through ccl-remote, with the
        # toolchain, Storehouse and workspace on an installed root on QEMU's
        # UFS disk, which the filesystem names @system (tests/device-builds;
        # the disk is made below, once build.sh has made its root).
        # gnat1 is a large image procmgr reads whole. console-run-staged:
        # the same machine with the CCL console, which runs the builds
        # (tests/console-run-staged).
        INIT_PROFILE="$ROOT_DIR/tests/device-builds/init.ccl"
        if [ "$TEST_NAME" = "console-run-staged" ]; then
            INIT_PROFILE="$ROOT_DIR/tests/console-run-staged/init.ccl"
        fi
        QEMU_MEMORY="${QEMU_MEMORY:-2G}"
        TEMP_UFS_DISK="$(mktemp "${TMPDIR:-/tmp}/cubit-device-build-ufs.XXXXXX.img")"
        UFS_ARGS=(-drive "file=$TEMP_UFS_DISK,if=none,id=ufs0,format=raw"
                  -device ufs,serial=cubitufs,id=ufs
                  -device ufs-lu,drive=ufs0,bus=ufs,lun=0)
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
    kern014-bench)
        # tests/kern014-bench (build it with its build.sh): KERN-014's A/B
        # benchmark of frame commit/decommit, TLB rounds, zeroing and spawn.
        INIT_PROFILE="$ROOT_DIR/tests/kern014-bench/init-kern014-bench.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-1G}"
        ;;
    bench-cualloc)
        # tests/userspace-allocator/cubit: CuAlloc through the libc malloc
        # family, single-threaded and with POSIX threads. Its medium trace
        # keeps about 128 MiB live.
        INIT_PROFILE="$ROOT_DIR/tests/userspace-allocator/cubit/init-bench-cualloc.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-1G}"
        ;;
    fat32-volume)
        # FS-030: tests/filesystem-fat/native (the stick is made below).
        INIT_PROFILE="$ROOT_DIR/tests/filesystem-fat/native/init.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
        ;;
    usb-disk)
        # SELF-003: tests/usb-disk/native (the sticks are made below).
        INIT_PROFILE="$ROOT_DIR/tests/usb-disk/native/init.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
        ;;
    usb-eject)
        # FS-034: tests/usb-eject/native (the sticks are made below).
        INIT_PROFILE="$ROOT_DIR/tests/usb-eject/native/init.ccl"
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
        # tests/sched-latency: interbench-style scheduler latency. Its
        # protect load holds 256 MiB (sched-latency.c PROTECT_BYTES).
        INIT_PROFILE="$ROOT_DIR/tests/sched-latency/init-bench-latency.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-512M}"
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
    wait)
        # ABI-002 slice 2 / KERN-013: WAIT's forms and the readiness page
        # (wait-check), and a store to the page that must fault
        # (readiness-writer).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-wait.ccl"
        ;;
    futex-share)
        # IPC-010: shared futexes across processes (wake, wait, FIFO,
        # revocation, a refused outsider, wake-plus-wait, starvation).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-futex-share.ccl"
        ;;
    mailbox-self|mailbox-mutual)
        # IDIOT-001: system calls that lock a target's mailbox, aimed at
        # the caller itself (mailbox-self) or at each other by two
        # processes at once (mailbox-mutual): refused, no halt, no deadlock.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-$TEST_NAME.ccl"
        ;;
    debug-engine)
        # DBG-001: the debugger engine attaches to debug-demo (cooperative,
        # read-only grants) and checks real frames, locals, values and graph.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-debug-engine.ccl"
        ;;
    debug-engine-rs)
        # DBG-001: the same engine attached to the Rust demo (debug-demo-rs).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-debug-engine-rs.ccl"
        ;;
    debug-gate-disabled|debug-gate-production|debug-gate-development)
        # DBG-001: the image's debugging gate, the startup plan's (policy
        # (System_Policy debugging => ...)), one test per value.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-$TEST_NAME.ccl"
        ;;
    debug-kernel)
        # DBG-001, CAP-012: the engine through the kernel (Debug_Observe
        # minted by procmgr through a launch handle and through the
        # session's debug authority); the targets have no stub.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-debug-kernel.ccl"
        ;;
    tlb-tags)
        # Address-space tags (PCIDs): tlb-check launches tlb-probe life
        # after life on reused PIDs; on every second life it revokes pages
        # a waiting thread of the life cached on its CPU (the kernel unmaps
        # them in tlb-check's address space). Two CPUs: lives often run
        # where the last one cached its translations, and the two threads
        # often on different CPUs. Only Intel CPUs (PCID and INVPCID) in a
        # TAGGED_TLB=1 kernel tag; the AMD hosts' guests and TCG pass
        # without exercising stale translations.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-tlb-tags.ccl"
        if [ "$QEMU_CPUS_EXPLICIT" = 0 ]; then QEMU_CPUS=2; fi
        ;;
    secondary-stacks)
        # secondary-stack-check (build: make -C kernel secondary-stack-check):
        # five threads' stacks at once, twice, then the main thread's grown
        # to its 256 MiB ceiling, refused and handled.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-secondary-stacks.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-1G}"
        ;;
    ada-runtime)
        # ada-runtime-check (build: make -C kernel ada-runtime-check): the
        # user GNAT runtime's tagged types, finalization and storage pools
        # (RT-008), and zero-cost exceptions (EXC-001).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ada-runtime.ccl"
        ;;
    exit-status)
        # LIVE-002: a returning main's exit status from the user runtime
        # (tests/exit-status; built below with make -C kernel
        # exit-status-test): 0 for a plain return, 1 for
        # Ada.Command_Line.Failure, 134 for an unhandled exception.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-exit-status.ccl"
        ;;
    acpi-shutdown)
        # Real acpi.svc + power.svc: copied SCI event, preparation, S5.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-acpi-shutdown.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-2G}"
        ;;
    acpi-one-service)
        # ACPI-ONE-001: the one acpi.svc on q35 (build: make -C kernel
        # acpi-native acpi-metrics-check) with the regression SSDT
        # tests/headless/acpi/acpi-one-service-pool.aml (-acpitable) and
        # QEMU's edu device (devmgr's pool test profile): its module-level
        # code reads a BAR-backed CHIPSET_SHARED pool member during table
        # load, which firmware data cannot serve. 2 GiB until MEM-004: CuAlloc's first
        # small allocation backs a whole 16 MiB arena.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-acpi-one-service.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-2G}"
        ;;
    acpi-sci)
        # SCI-DELIVERY: acpi-one-service's image and firmware, and QEMU's
        # system_powerdown twice: the PM1 power button raises the SCI (GSI
        # 9, level) on the line procmgr routed to the one acpi.svc, which
        # reads and clears PWRBTN_STS through its I/O window, logs it and
        # acknowledges (ENABLE_IRQ's owner form). The second press is sent as
        # soon as the first is logged, before or after the acknowledgement:
        # it must arrive either way. SCI_EN must be on when it arms: already
        # set, or set by its ACPI_ENABLE write to SMI_CMD (build: make -C
        # kernel acpi-native acpi-metrics-check).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-acpi-one-service.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-2G}"
        ;;
    firmware-snapshot)
        # FIRMWARE_SNAPSHOT over q35's ACPI tables: the approved reader,
        # the same image unapproved, and a program with no reader.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-firmware-snapshot.ccl"
        ;;
    acpi-hardware)
        # ACPI hardware windows (q35: FADT PM1, PM timer and GPE0, no EC;
        # firmware memory: no ACPI NVS, the FACS page in SeaBIOS's reserved
        # span at the top of RAM) and the role-35 register endpoint's
        # frontend badge.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-acpi-hardware.ccl"
        ;;
    acpi-pci-config)
        # ACPI-PCI-001: the opt-in read-only, uncached PCI segment 0 bus 0
        # window (q35's MCFG at 0xB000_0000) of the ACPI hardware grant;
        # the same image without (acpi-pci-config approve-declared) gets
        # the grant without it (build: make -C kernel acpi-pci-check).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-acpi-pci-config.ccl"
        ;;
    platform-pool)
        # ACPI-POOL-001: the platform-register pool (devmgr's test profile:
        # q35 plus QEMU's edu device, whose BAR0 stands in for MCHBAR
        # (Device_BAR) and a sideband window (Device_BAR) with one GPIO
        # community (Chipset_Shared)), mappings through it, their
        # revocation when edu's fixture driver binds, and the shared
        # community kept (build: make -C kernel platform-pool-check).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-platform-pool.ccl"
        ;;
    gpio-launch)
        # docs/device-matching.md "ACPI launch": devmgr admits a GPIO
        # controller's spans (the GPIO bootstrap admission), then launches
        # the driver whose manifest matches its _HID with exactly those
        # spans (build: make -C kernel world gpio-bootstrap-check). The
        # profile: edu plus the CBTEST/GPIOBOOT firmware identity below.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-gpio-launch.ccl"
        ;;
    acpi-firmware-data)
        # Table-named firmware data: devmgr's narrowed, uncached windows
        # over the kernel's Firmware_Data_Tag roots, for a program approved
        # with (acpi-firmware-data approve-declared); the same image without
        # it is refused (build: make -C kernel acpi-data-check).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-acpi-firmware-data.ccl"
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
    clock-keeper)
        # The system's time without CPU 0 (Time.clockTick, Timekeeping_Turns):
        # a probe kernel keeps CPU 0's timer from its clock work on
        # clock-keeper-check's request; deadlines and msTicks must go on.
        # Run with CLOCK_STALL_PROBE=1 in the environment (this script's own
        # kernel rebuild for the init profile reads it), after
        # make -C kernel world CLOCK_STALL_PROBE=1 clock-keeper-check. Other
        # CPUs keep time only with one clock (+invtsc, KVM).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-clock-keeper.ccl"
        case "$QEMU_CPU_MODEL" in
            *invtsc*) ;;
            *) QEMU_CPU_MODEL="$QEMU_CPU_MODEL,+invtsc" ;;
        esac
        ;;
    cpu-times)
        # The per-CPU times publication (KERN-002). The check sleeps and
        # spins by the clock publication, which needs +invtsc (KVM).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-cpu-times.ccl"
        case "$QEMU_CPU_MODEL" in
            *invtsc*) ;;
            *) QEMU_CPU_MODEL="$QEMU_CPU_MODEL,+invtsc" ;;
        esac
        ;;
    virt-report)
        # VMM-001 slice 0: the boot report of hardware virtualization. On
        # an AMD host with kvm_amd nested=1, -cpu host,+svm gives the guest
        # SVM with nested paging; the kernel reads CPUID and VM_CR only.
        if [ "$QEMU_ACCEL" != kvm ]; then
            echo "headless: virt-report needs --accel kvm (nested SVM)" >&2
            exit 2
        fi
        QEMU_CPU_MODEL="host,+svm"
        ;;
    vm-desktop)
        # VMM-001 slice 1: vmm on the desktop, the guest's serial console in
        # its window; screenshots into CUBIT_VM_SHOTS (default
        # /home/doc/cubit-build-tmp/vmm/shots); nested SVM.
        if [ "$QEMU_ACCEL" != kvm ]; then
            echo "headless: vm-desktop needs --accel kvm (nested SVM)" >&2
            exit 2
        fi
        QEMU_CPU_MODEL="host,+svm"
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-vm-desktop.ccl"
        # CUBIT_VM_LOAD=1: Neon Blast plays beside the vmm while both guest
        # vCPUs spin for CUBIT_VM_LOAD_SECONDS (vm-desktop-shots.py), to see
        # whether a busy guest delays input, frames or audio periods.
        if [ "${CUBIT_VM_LOAD:-0}" = 1 ]; then
            INIT_PROFILE="$ROOT_DIR/tests/headless/init-vm-desktop-load.ccl"
        fi
        # The guest's 128 MiB, beside the desktop.
        QEMU_MEMORY="${QEMU_MEMORY:-1G}"
        ;;
    vm-linux)
        # VMM-001 slice 1: the pinned Linux guest to its shell prompt
        # (userspace/apps/vm-linux-check, the monitor core); nested SVM.
        if [ "$QEMU_ACCEL" != kvm ]; then
            echo "headless: vm-linux needs --accel kvm (nested SVM)" >&2
            exit 2
        fi
        QEMU_CPU_MODEL="host,+svm"
        # The guest's 128 MiB is the monitor's own memory.
        QEMU_MEMORY="${QEMU_MEMORY:-1G}"
        ;;
    vm-hello)
        # VMM-001 slice 1: the VM mechanism (kernel/src/vm_objects.ads),
        # checked from procmgr's place by a tiny monitor; needs nested SVM
        # (kvm_amd nested=1, -cpu host,+svm under KVM).
        if [ "$QEMU_ACCEL" != kvm ]; then
            echo "headless: vm-hello needs --accel kvm (nested SVM)" >&2
            exit 2
        fi
        QEMU_CPU_MODEL="host,+svm"
        ;;
    virt-report-none)
        # The same report on a CPU with neither VMX nor SVM.
        QEMU_CPU_MODEL="$QEMU_CPU_MODEL,-vmx,-svm"
        ;;
    tsc-sync|tsc-skew)
        # One clock on every CPU (TSC_Sync): each secondary CPU's
        # TSC_ADJUST is set to CPU 0's and a warp check runs against CPU 0
        # (4 CPUs, +invtsc so the clock page is shared, +tsc-adjust; KVM).
        # tsc-skew needs a kernel built TSC_SKEW_PROBE=1 (make -C kernel
        # world TSC_SKEW_PROBE=1): CPU 1 moves its counter 2**30 ticks ahead
        # first, which also moves its TSC_ADJUST; the step must restore one
        # clock and the check agree.
        if [ "$QEMU_ACCEL" != kvm ]; then
            echo "headless: $TEST_NAME needs --accel kvm (the vCPUs' TSCs)" >&2
            exit 2
        fi
        QEMU_CPU_MODEL="$QEMU_CPU_MODEL,+invtsc,+tsc-adjust"
        ;;
    tsc-skew-noadjust)
        # The same probe kernel on a CPU without TSC_ADJUST: nothing can
        # move CPU 1 back, the check sees it ahead, and the clock becomes
        # CPU 0's alone; the system still starts.
        if [ "$QEMU_ACCEL" != kvm ]; then
            echo "headless: $TEST_NAME needs --accel kvm (the vCPUs' TSCs)" >&2
            exit 2
        fi
        QEMU_CPU_MODEL="$QEMU_CPU_MODEL,+invtsc,-tsc-adjust"
        ;;
    cpu-refusal)
        # A CPU without x86-64-v3 (Westmere: no AVX) is refused at boot with
        # one line naming what it lacks, before any program starts.
        # No vendor= suffix: the kernel halts before it reports the vendor.
        QEMU_CPU_MODEL="Westmere"
        ;;
    font-masks)
        # The native font engine's masks, hashed: every printable glyph of
        # Sans and Sans_Semibold at densities 1, 3/2 and 2, four phases each.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-font-masks.ccl"
        ;;
    avx)
        # Two processes sharing one CPU, each holding its own YMM pattern
        # across thousands of switches (kernel XSAVE isolation).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-avx.ccl"
        if [ "$QEMU_CPUS_EXPLICIT" = 0 ]; then QEMU_CPUS=1; fi
        ;;
    wifi-crypto)
        # rtw89's WiFi_Crypto known-answer self-test, natively, without the
        # device (make wifi-crypto-check: the crates are fetched by ccl-build).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-wifi-crypto.ccl"
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
    ladybird-test262)
        # WEB-006 phase a: Ladybird's test262-runner (JIT compiled out),
        # built unmodified as a Linux/musl program, over all of test262.
        # The suite needs a larger disk (TEST262_DISK_BYTES) and a while.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ladybird-test262.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-2G}"
        ;;
    ladybird-js)
        # WEB-006 phase a: Ladybird's js (LibJS, JIT compiled out), built
        # unmodified as a Linux/musl program, runs tests/ladybird/check.js.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ladybird-js.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-1G}"
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
    ccl-build-native)
        # SELF-002: the native ccl-build.app builds ps2.drv on CuBit with the
        # GCC port's toolchain; an interrupted build recovers, the program
        # matches Linux's byte for byte, and a rebuild runs no step
        # (tests/ccl-build-native). The CCL front end runs on a task with a
        # large stack, gnat1 is a large image procmgr reads whole.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ccl-build-native.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-2G}"
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
    supervision)
        # Start_Child (SUP-001): a supervisor starts a child with typed
        # parameters through procmgr, gets its exit event and typed
        # refusals, then ends and procmgr reaps its running child.
        INIT_PROFILE="$ROOT_DIR/tests/supervision/init.ccl"
        ;;
    shutdown)
        # PWR-003 slice 1: QEMU's power button (system_powerdown) through a
        # TEST STAND-IN for acpi.svc's button watch, to power.svc, whose
        # stop procmgr runs in reverse startup order with each entry's
        # grace (one entry obeys Stop, one hangs and is forced), the
        # filesystem commits and detaches (the journaled disk ends without
        # needs_recovery), devmgr stops its drivers, and the stand-in
        # writes S5 into PM1a_CNT: QEMU powers off by itself.
        INIT_PROFILE="$ROOT_DIR/tests/shutdown/init.ccl"
        ;;
    shutdown-console)
        # PWR-003 slice 1: (power-off) typed at the CCL console, the
        # console's own requester badge on power.svc; the same stop, with
        # the Desktop session, ending in S5 through the stand-in.
        INIT_PROFILE="$ROOT_DIR/tests/shutdown/init-console.ccl"
        ;;
    graphics-report-nodriver|graphics-report-failing)
        # GPU-014: the renderer report through the console's
        # (graphics.report). nodriver: only QEMU's std VGA, which no GPU
        # driver binds. failing: run with CUBIT_GPU_TEST_MODE=init-failure,
        # so the boot image's virtio-gpu.drv is the build that stops at
        # start-up (make -C kernel virtio-gpu initrd afterwards puts the
        # production driver back).
        INIT_PROFILE="$ROOT_DIR/tests/graphics-report/init.ccl"
        ;;
    graphics-report-desktop-down)
        # GPU-014: Desktop (a CUBIT_DESKTOP_IMAGE built
        # CUBIT_DESKTOP_FAULT=exit-once) exits after its first frame and is
        # not restarted, as on the T580 (BUG-009); the report must still
        # answer, with Desktop's last entry stale (graphics-report-check).
        INIT_PROFILE="$ROOT_DIR/tests/graphics-report/init-desktop-down.ccl"
        ;;
    power-sources)
        # PWR-004 slice 1: the battery and AC adapter through power.svc to
        # Desktop's Battery item and the console's (power.sources). QEMU has
        # no ACPI battery: a TEST STAND-IN serves acpi.svc's AC and battery
        # pages with acpi.svc's own codecs (tests/power-sources/standin).
        INIT_PROFILE="$ROOT_DIR/tests/power-sources/init.ccl"
        ;;
    shutdown-desktop|shutdown-desktop-cancel)
        # PWR-003: Shut Down... in Desktop's Apps menu, by keyboard, then the
        # confirmation: Enter shuts down (S5 through the stand-in); Escape
        # cancels and nothing is sent.
        INIT_PROFILE="$ROOT_DIR/tests/shutdown/init-desktop.ccl"
        ;;
    shutdown-text)
        # PWR-003: apps close safely. Text, with unsaved edits, gets
        # procmgr's Stop as a System_Shutdown close (CuBit.UI.App), writes a
        # recovery copy to its recovery place, never over its file, and
        # exits within its grace; then S5 through the stand-in.
        INIT_PROFILE="$ROOT_DIR/tests/shutdown/init-text.ccl"
        ;;
    startup-wait)
        # procmgr's startup waits (CAP-003): a client waits, parked, for a
        # provider that is starting; typed refusals when it is not.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-startup-wait.ccl"
        ;;
    large-manifest)
        # The manifest compiler's storage sized by its input: a program whose
        # typed manifest is well past the old CCL limits loads and launches.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-large-manifest.ccl"
        ;;
    servo)
        # Servo (docs/servo-port.md): an 85 MB program whose heap grows
        # eagerly. Its fonts are linked in: the disk has none, like the
        # laptop's live USB.
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
    agent-app)
        # AGENT-002 step 3: agent.app from the startup plan. The test types
        # the fake key (sk-ant-test-0000) into its masked entry, asks a
        # Diagnose mission, and the host's mock Messages API scripts one
        # tool round (logs_recent, processes_list) and an answer.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-agent-app.ccl"
        LLM_MOCK_STATE="$(mktemp -d "${TMPDIR:-/tmp}/cubit-llm-mock.XXXXXX")"
        ;;
    llm-mock|agent-mock)
        # AGENT-001 slice 1: (llm.ask ...) in the console through llm.svc
        # (its test variant) to the host's mock Messages API
        # (tests/llm/mock_server.py), the key from secrets.svc. No internet,
        # no real key: the test types sk-ant-test-0000. agent-mock (AGENT-002
        # step 2): (agent.ask ...) with the Diagnose mission, the mock
        # scripting the model's tool calls.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-llm-mock.ccl"
        LLM_MOCK_STATE="$(mktemp -d "${TMPDIR:-/tmp}/cubit-llm-mock.XXXXXX")"
        ;;
    net-fetch)
        # SELF-003 steps 1 and 2: (net.fetch ...), (archive.unpack ...) and
        # (git.clone ...) in the console, through tls.svc to the host's
        # HTTPS fixture (tests/net-fetch/server.py, git's smart HTTP by the
        # host's git upload-pack), into @system/work. No internet.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-net-fetch.ccl"
        NET_FETCH_STATE="$(mktemp -d "${TMPDIR:-/tmp}/cubit-net-fetch.XXXXXX")"
        NET_FETCH_MANY_ENTRIES=1000
        ;;
    llm-live)
        # Manual lane, like wget-https: the real api.anthropic.com. Needs the
        # host's internet and the user's key, typed into the console (with
        # --display gtk) or read by the script from LLM_LIVE_KEY_FILE.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-llm-live.ccl"
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
    desktop-session)
        # make -C kernel run-desktop's own boot (the README's quick start):
        # its startup profile (DESKTOP_INIT), its disk (prepare-desktop-disk,
        # below), its devices, memory and CPU model (below).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-desktop-session.ccl"
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
    usb-slots)
        # LIM-001: more USB devices than the old fixed 8 device slots; the
        # slot count comes from HCSPARAMS1 MaxSlots (tests/usb-slots).
        INIT_PROFILE="$ROOT_DIR/tests/multi-instance/init.ccl"
        ;;
    xhci-bringup)
        # The IdeaPad's boot stall: two xHCI controllers, devmgr never waits
        # on their USB bring-up, and each finishes within a bound
        # (tests/xhci-bringup).
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
    neon-blast)
        # Neon Blast (APP-008): the game on the desktop, its soundtrack
        # through mixer.svc and QEMU's HDA into a WAV (tests/neon-blast).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-neon-blast.ccl"
        # CUBIT_NEON_BURN=1: eight CPU-hungry programs beside the game (SCHED-001,
        # app audio threads); its frames drop, its music must not underrun.
        if [ "${CUBIT_NEON_BURN:-0}" = 1 ]; then
            INIT_PROFILE="$ROOT_DIR/tests/headless/init-neon-blast-burn.ccl"
        fi
        ;;
    display-latency)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-display-latency.ccl"
        # CUBIT_DISPLAY_LATENCY_ANIMATION=1: a window animating every refresh
        # beside the Calculator (frame-ticker.app, as Teapots on the laptop).
        if [ "${CUBIT_DISPLAY_LATENCY_ANIMATION:-0}" = 1 ]; then
            INIT_PROFILE="$ROOT_DIR/tests/headless/init-display-latency-animated.ccl"
        fi
        # SCHED-001 input urgency: CUBIT_LATENCY_LOAD=N CPU-bound peers
        # (bench-input-load.app) beside the Calculator, and
        # CUBIT_LATENCY_INPUT=usb types through QEMU's usb-kbd (xhci.drv)
        # instead of the i8042 (ps2.drv). tests/input-urgency/check_latency.py
        # then holds the driver-to-app stages to the input limit.
        if [ "${CUBIT_LATENCY_LOAD:-0}" -gt 0 ]; then
            TEMP_LATENCY_PROFILE="$(mktemp "${TMPDIR:-/tmp}/cubit-latency-load.XXXXXX.ccl")"
            awk -v count="$CUBIT_LATENCY_LOAD" '
                /^\)/ { for (i = 1; i <= count; i++)
                           print "  (start \"bench-input-load.app\" (trace approve-declared))" }
                { print }' "$INIT_PROFILE" > "$TEMP_LATENCY_PROFILE"
            INIT_PROFILE="$TEMP_LATENCY_PROFILE"
        fi
        ;;
    metrics-latency)
        # MET-002: display-latency with metrics.svc and ccl-remote; the
        # host reads the series back (tests/metrics-latency/metrics_session.py).
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-metrics-latency.ccl"
        ;;
    ui-latency)
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-ui-latency.ccl"
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
    virtio-gpu|virtio-vga-primary|audio-grants|boot-shell-nvme|boot-memory-8g|boot-memory-16g|boot-memory-32g|boot-large-module|nvme-timeout|nvme-stuck)
        # The shipped plan with logstore's test console approved: the
        # launchers' and drivers' lines (devmgr, procmgr, ps2, nvme, mixer,
        # hda, virtio-gpu) reach serial only through its mirror.
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-default.ccl"
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

if [ "$LOAD_KIND" != cpu ] && [ "$TEST_NAME" != bench-audio ]; then
    echo "headless: --load-kind $LOAD_KIND only supports bench-audio" >&2
    exit 2
fi
if [ "$BENCH_LOAD" = 1 ]; then
    INIT_PROFILE="$ROOT_DIR/tests/headless/init-${TEST_NAME}-load.ccl"
    if [ "$LOAD_KIND" = heap ]; then
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-${TEST_NAME}-heap.ccl"
    elif [ "$LOAD_KIND" = windows ]; then
        INIT_PROFILE="$ROOT_DIR/tests/headless/init-${TEST_NAME}-windows.ccl"
        QEMU_MEMORY="${QEMU_MEMORY:-1G}"
        # The source rule the load below exercises at run time: no per-CPU
        # overlay where a window may move the thread.
        python3 -I "$ROOT_DIR/tests/interrupt-windows/check_overlays.py" "$KERNEL_DIR/src" || exit 1
    fi
    if [ "$LOAD_WORKERS" -gt 1 ]; then
        TEMP_LOAD_PROFILE="$(mktemp "${TMPDIR:-/tmp}/cubit-input-load.XXXXXX.ccl")"
        awk -v count="$LOAD_WORKERS" '
            /^[[:space:]]*[(]start "bench-input-load[.]app"[ )]/ {
                for (i=1; i<=count; i++) print;
                next
            }
            { print }
        ' "$INIT_PROFILE" > "$TEMP_LOAD_PROFILE"
        INIT_PROFILE="$TEMP_LOAD_PROFILE"
    fi
fi
# BUG-002: the early boot allocator sizes itself from the memory map. The
# boot-shell-nvme boot at 8, 16 and 32 GiB (buddy metadata grows with the top of
# RAM), and with a large boot module that GRUB loads into low memory, reserved
# exactly and, consumed by no one, returned to the buddy allocator at module
# setup (tests/boot-reclaim/check.py measures it).
case "$TEST_NAME" in
    boot-memory-8g) QEMU_MEMORY="${QEMU_MEMORY:-8G}" ;;
    boot-memory-16g) QEMU_MEMORY="${QEMU_MEMORY:-16G}" ;;
    # SeaBIOS leaves the 7 MiB test initrd low, so 8 and 16 GiB fit even the
    # old 64 MiB window; 32 GiB of buddy metadata did not.
    boot-memory-32g) QEMU_MEMORY="${QEMU_MEMORY:-32G}" ;;
    boot-large-module) QEMU_MEMORY="${QEMU_MEMORY:-2G}" ;;
esac
# Size of the synthetic boot-large-module payload: the ~340 MiB live-rw.ext2.
LARGE_MODULE_MIB=340
LARGE_MODULE="$KERNEL_DIR/isodir/boot/large-module.img"
# desktop-vm mirrors run-desktop (2 GiB): large GPU banks (modes up to
# 1920x1080) need at least 1 GiB (devmgr LARGE_BANK_MINIMUM_MEMORY).
if [ "$TEST_NAME" = desktop-vm ] || [ "$TEST_NAME" = desktop-session ]; then
    QEMU_MEMORY="${QEMU_MEMORY:-2G}"
fi
# CUBIT_TEST_VIDEO_SIZE=WxH (desktop-display, calculator, remote-screenshot, neon-blast): the firmware mode, as
# WIDTHxHEIGHT; larger modes need room for Desktop's output buffers.
if { [ "$TEST_NAME" = desktop-display ] || [ "$TEST_NAME" = calculator ] || [ "$TEST_NAME" = remote-screenshot ] || [ "$TEST_NAME" = neon-blast ]; } && [ -n "${CUBIT_TEST_VIDEO_SIZE:-}" ]; then
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
# A plan procmgr would refuse starts nothing (not even logstore), and every
# driver then waits on devmgr for an introduction that never comes: the
# boot hangs with no message (usb-eject's plans kept the removed
# (priority N), 2026-11-25). Refuse it here, with the compiler's reason.
if [ -n "$INIT_PROFILE" ] && [ -x "$ROOT_DIR/userspace/ccl/build/config/ccl-config" ]; then
    if ! PLAN_CHECK="$("$ROOT_DIR/userspace/ccl/build/config/ccl-config" "$INIT_PROFILE" 2>&1 >/dev/null)"; then
        echo "headless: $INIT_PROFILE does not compile: $PLAN_CHECK" >&2
        exit 1
    fi
fi
if [ -n "$INIT_PROFILE" ]; then
    TEMP_DISK="$(mktemp "${TMPDIR:-/tmp}/cubit-${TEST_NAME}-disk.XXXXXX.img")"
    cp "$BASE_DISK" "$TEMP_DISK"
    debugfs -w -R "rm init.ccl" "$TEMP_DISK" >/dev/null 2>&1
    if ! debugfs -w -R "write $INIT_PROFILE init.ccl" "$TEMP_DISK" >/dev/null 2>&1; then
        echo "headless: failed to install $TEST_NAME init.ccl" >&2
        exit 1
    fi
    if [ "$TEST_NAME" = "desktop-session" ]; then
        # The disk run-desktop boots: its own rule (prepare-desktop-disk
        # overlays the staged desktop programs, assets and samples on the
        # base disk and writes DESKTOP_INIT as init.ccl), into this scratch
        # copy. Only logstore's test variant (below) differs.
        if ! make -s --no-print-directory -C "$KERNEL_DIR" prepare-desktop-disk \
          DESKTOP_BASE_DISK="$BASE_DISK" DESKTOP_SCRATCH_DISK="$TEMP_DISK" \
          DESKTOP_INIT="$INIT_PROFILE" >/dev/null; then
            echo "headless: prepare-desktop-disk failed for $TEST_NAME" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "desktop-live-replace" ]; then
        # The app-replace survey puts every shipped app in the workspace and
        # keeps each as a generation: room beyond the base disk's.
        truncate -s 2G "$TEMP_DISK"
        if ! "$(command -v resize2fs || echo /usr/sbin/resize2fs)" -f "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to grow the $TEST_NAME disk" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "metricsview" ]; then
        # MET-002: a saved dashboard that does not check (no time), which
        # Open must refuse with the checker's message.
        broken_dashboard="$(mktemp "${TMPDIR:-/tmp}/cubit-metricsview-broken.XXXXXX.ccl")"
        printf '(Dashboard title => "Broken" panels => (list-of Panel))\n' > "$broken_dashboard"
        debugfs -w -R "mkdir Documents" "$TEMP_DISK" >/dev/null 2>&1 || true
        debugfs -w -R "mkdir Documents/Dashboards" "$TEMP_DISK" >/dev/null 2>&1 || true
        if ! debugfs -w -R "write $broken_dashboard Documents/Dashboards/broken.ccl" "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to seed the broken dashboard" >&2
            exit 1
        fi
        rm -f "$broken_dashboard"
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
            elif [ "$LOAD_KIND" = heap ]; then
                BENCHMARK_IMAGES="$BENCHMARK_IMAGES bench-heap-load.app"
            elif [ "$LOAD_KIND" = windows ]; then
                BENCHMARK_IMAGES="$BENCHMARK_IMAGES bench-heap-load.app bench-sbrk-load.app"
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
    if [ "$TEST_NAME" = "neon-blast" ] || { [ "$TEST_NAME" = "vm-desktop" ] && [ "${CUBIT_VM_LOAD:-0}" = 1 ]; }; then
        # An image-only program (like Teapots): staged onto this run's disk.
        if [ ! -f "$KERNEL_DIR/isodir/boot/neon-blast.app" ]; then
            echo "headless: build neon-blast first (make -C kernel neon-blast)" >&2
            exit 1
        fi
        debugfs -w -R "rm neon-blast.app" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "write $KERNEL_DIR/isodir/boot/neon-blast.app neon-blast.app" \
            "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        if [ "$TEST_NAME" = "neon-blast" ] && [ "${CUBIT_NEON_BURN:-0}" = 1 ]; then
            if [ ! -f "$KERNEL_DIR/isodir/boot/bench-burn.app" ]; then
                echo "headless: build bench-burn first (make -C kernel bench-load)" >&2
                exit 1
            fi
            debugfs -w -R "rm bench-burn.app" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/bench-burn.app bench-burn.app" \
                "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        fi
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
       [ "$TEST_NAME" = "llm-mock" ] || [ "$TEST_NAME" = "agent-mock" ] || [ "$TEST_NAME" = "agent-app" ] || [ "$TEST_NAME" = "llm-live" ] ||
       [ "$TEST_NAME" = "net-fetch" ] ||
       [ "$TEST_NAME" = "lexicon" ] ||
       [ "$TEST_NAME" = "dictionary" ] ||
       [ "$TEST_NAME" = "bible" ] ||
       [ "$TEST_NAME" = "debugger-gui" ] || [ "$TEST_NAME" = "debugger-gui-rs" ] ||
       [ "$TEST_NAME" = "logs" ] ||
       [ "$TEST_NAME" = "devmem-denials" ] ||
       [ "$TEST_NAME" = "process-kill" ] ||
       [ "$TEST_NAME" = "desktop-restart" ] ||
       [ "$TEST_NAME" = "desktop-reap" ] ||
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
       [ "$TEST_NAME" = "calculator" ] || [ "$TEST_NAME" = "vm-desktop" ] ||
       [ "$TEST_NAME" = "display-latency" ] || [ "$TEST_NAME" = "metrics-latency" ] ||
       [ "$TEST_NAME" = "ui-latency" ] ||
       [ "$TEST_NAME" = "clipboard" ] ||
       [ "$TEST_NAME" = "key-repeat" ] ||
       [ "$TEST_NAME" = "desktop-virtio-vga" ] ||
       [ "$TEST_NAME" = "desktop-vm" ] ||
       [ "$TEST_NAME" = "desktop-doom" ] ||
       [ "$TEST_NAME" = "virtio-vga-primary" ] ||
       [ "$TEST_NAME" = "usb-hotplug" ] ||
       [ "$TEST_NAME" = "remote-screenshot" ] ||
       [ "$TEST_NAME" = "device-build" ] || [ "$TEST_NAME" = "console-run-staged" ] ||
       [ "$TEST_NAME" = "bench-build" ] ||
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
        # the whole wallpaper package (its catalog and whatever pictures it
        # names) and the application icon atlas.
        debugfs -w -R "mkdir Assets" "$TEMP_DISK" >/dev/null 2>&1
        WALLPAPER_PACKAGE=cubit-wallpapers/2
        if [ ! -f "$KERNEL_DIR/build/Assets/$WALLPAPER_PACKAGE/catalog.ccl" ]; then
            echo "headless: build wallpaper-assets first: $KERNEL_DIR/build/Assets/$WALLPAPER_PACKAGE" >&2
            exit 1
        fi
        WALLPAPER_ITEMS=$(cd "$KERNEL_DIR/build/Assets" && find "$WALLPAPER_PACKAGE" -maxdepth 1 -type f | sort)
        # The Bible's package only where the Bible runs (25 MB).
        BIBLE_ITEMS=""
        if [ "$TEST_NAME" = "bible" ]; then
            BIBLE_ITEMS="cubit-bible/1/kjv.bible cubit-bible/1/bsb.bible cubit-bible/1/web.bible cubit-bible/1/vul.bible cubit-bible/1/BIBLE-NOTICE"
        fi
        for ASSET_ITEM in $WALLPAPER_ITEMS \
          cubit-icons/5/applications.qoi cubit-lexicon/1/english.lexicon cubit-lexicon/1/LEXICON-NOTICE \
          $BIBLE_ITEMS; do
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
       [ "$TEST_NAME" = "usb-hotplug" ] || [ "$TEST_NAME" = "usb-slots" ] ||
       [ "$TEST_NAME" = "xhci-bringup" ]; then
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
       [ "$TEST_NAME" = "llm-mock" ] || [ "$TEST_NAME" = "agent-mock" ] || [ "$TEST_NAME" = "agent-app" ] || [ "$TEST_NAME" = "llm-live" ] ||
       [ "$TEST_NAME" = "net-fetch" ] ||
       [ "$TEST_NAME" = "lexicon" ] || [ "$TEST_NAME" = "dictionary" ] || [ "$TEST_NAME" = "bible" ] ||
       [ "$TEST_NAME" = "logs" ] || [ "$TEST_NAME" = "metricsview" ] || [ "$TEST_NAME" = "debugger-gui" ] || [ "$TEST_NAME" = "debugger-gui-rs" ]; then
        if [ "$TEST_NAME" = "ccl-vm" ]; then
            CCL_IMAGES="ccl-vm.app ccl-test-host.svc clock.svc"
        elif [ "$TEST_NAME" = "ccl-console" ]; then
            CCL_IMAGES="ccl-console.app clock.svc desktop.svc display.svc logstore.svc"
            if [ "${CCL_CONSOLE_DEMO:-}" = log-exited ]; then
                CCL_IMAGES="$CCL_IMAGES boot-logs.app log-exit-probe.app"
            fi
            if [ "${CCL_CONSOLE_DEMO:-}" = kernel-log ]; then
                CCL_IMAGES="$CCL_IMAGES crash-probe.app fault-check.app stack-probe.app"
            fi
            if [ "${CCL_CONSOLE_DEMO:-}" = log-burst ]; then
                CCL_IMAGES="$CCL_IMAGES log-burst-probe.app"
            fi
        elif [ "$TEST_NAME" = "llm-mock" ] || [ "$TEST_NAME" = "agent-mock" ] || [ "$TEST_NAME" = "agent-app" ]; then
            # llm-test.svc is llm.svc with only its tls-scope replaced (the
            # mock's name and port); it is installed under the production
            # name the startup plan starts. No other image carries it.
            CCL_IMAGES="ccl-console.app clock.svc desktop.svc display.svc logstore.svc tls.svc secrets.svc llm-test.svc:llm.svc"
            if [ "$TEST_NAME" = "agent-app" ]; then
                CCL_IMAGES="agent.app clock.svc desktop.svc display.svc logstore.svc tls.svc secrets.svc llm-test.svc:llm.svc"
            fi
        elif [ "$TEST_NAME" = "net-fetch" ]; then
            # ccl-console-fetch-test.app is ccl-console.app with only its tls
            # scope replaced (the fixture's name and port); it is installed
            # under the production name the startup plan starts.
            CCL_IMAGES="ccl-console-fetch-test.app:ccl-console.app clock.svc desktop.svc display.svc logstore.svc tls.svc"
            debugfs -w -R "mkdir work" "$TEMP_DISK" >/dev/null 2>&1
            # BUG-006: work/many holds more entries than one fs.list result
            # and than one directory-queue request (16 pages of 50), so the
            # listing pages across requests and must be refused with its
            # whole count, never listed cut short.
            many_commands="$TEMP_DISK.many"
            many_empty="$TEMP_DISK.empty"
            : > "$many_empty"
            {
                echo "mkdir work/many"
                for ((entry = 0; entry < NET_FETCH_MANY_ENTRIES; entry++)); do
                    printf 'write %s work/many/entry-%04d\n' "$many_empty" "$entry"
                done
            } > "$many_commands"
            debugfs -w -f "$many_commands" "$TEMP_DISK" >/dev/null 2>&1
            rm -f "$many_commands" "$many_empty"
            if [ "$(debugfs -R "ls work/many" "$TEMP_DISK" 2>/dev/null | grep -o 'entry-[0-9]*' | sort -u | wc -l)" \
                 -ne "$NET_FETCH_MANY_ENTRIES" ]; then
                echo "headless: failed to seed work/many" >&2
                exit 1
            fi
        elif [ "$TEST_NAME" = "llm-live" ]; then
            CCL_IMAGES="ccl-console.app clock.svc desktop.svc display.svc logstore.svc timesync.svc tls.svc secrets.svc llm.svc"
        elif [ "$TEST_NAME" = "logs" ]; then
            CCL_IMAGES="logs.app clock.svc desktop.svc display.svc"
        elif [ "$TEST_NAME" = "metricsview" ]; then
            CCL_IMAGES="metricsview.app metrics.svc clock.svc desktop.svc display.svc logstore.svc"
        elif [ "$TEST_NAME" = "lexicon" ]; then
            CCL_IMAGES="ccl-console.app lexicon.svc clock.svc desktop.svc display.svc logstore.svc"
        elif [ "$TEST_NAME" = "dictionary" ]; then
            CCL_IMAGES="dictionary.app lexicon.svc clock.svc desktop.svc display.svc logstore.svc"
        elif [ "$TEST_NAME" = "bible" ]; then
            CCL_IMAGES="bible.app clock.svc desktop.svc display.svc logstore.svc"
        elif [ "$TEST_NAME" = "debugger-gui" ]; then
            CCL_IMAGES="debugger.app debug-demo.app clock.svc desktop.svc display.svc logstore.svc"
        elif [ "$TEST_NAME" = "debugger-gui-rs" ]; then
            CCL_IMAGES="debugger.app debug-demo-rs.app clock.svc desktop.svc display.svc logstore.svc"
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
            if [ "${CCL_CONSOLE_DEMO:-}" = debug ]; then
                # debug.* (DBG-001): clock.svc's build-id debug file in the
                # @system/Debug layout, for the console's engine.
                DEBUG_ELF="$KERNEL_DIR/isodir/boot/clock.svc"
                DEBUG_ID=$(readelf -nW "$DEBUG_ELF" | sed -n 's/.*Build ID: \([0-9a-f]*\).*/\1/p')
                DEBUG_FILE="${TEMP_DISK%.img}-clock.debug"
                DEBUG_SPLIT="$KERNEL_DIR/debug/.build-id/${DEBUG_ID:0:2}/${DEBUG_ID:2}.debug"
                if [ -n "$DEBUG_ID" ] && [ -f "$DEBUG_SPLIT" ]; then
                    DEBUG_COPY=(objcopy --decompress-debug-sections "$DEBUG_SPLIT")
                else
                    DEBUG_COPY=(objcopy --only-keep-debug "$DEBUG_ELF")
                fi
                if [ -z "$DEBUG_ID" ] || ! "${DEBUG_COPY[@]}" "$DEBUG_FILE"; then
                    echo "headless: ccl-console debug demo: no build-id debug file for clock.svc" >&2
                    exit 1
                fi
                for DEBUG_DIR in Debug Debug/.build-id "Debug/.build-id/${DEBUG_ID:0:2}"; do
                    debugfs -w -R "mkdir $DEBUG_DIR" "$TEMP_DISK" >/dev/null 2>&1
                done
                debugfs -w -R "write $DEBUG_FILE Debug/.build-id/${DEBUG_ID:0:2}/${DEBUG_ID:2}.debug" \
                  "$TEMP_DISK" >/dev/null 2>&1
                rm -f "$DEBUG_FILE"
            fi
        fi
        for CCL_IMAGE_ENTRY in $CCL_IMAGES; do
            # SOURCE:TARGET installs a variant under another name.
            CCL_IMAGE_NAME="${CCL_IMAGE_ENTRY##*:}"
            CCL_IMAGE="$KERNEL_DIR/isodir/boot/${CCL_IMAGE_ENTRY%%:*}"
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
    if [ "$TEST_NAME" = "ui-latency" ]; then
        PROBE_TEST_IMAGE="$KERNEL_DIR/isodir/boot/ui-latency-probe.app"
        if [ ! -f "$PROBE_TEST_IMAGE" ]; then
            echo "headless: build the probe first (make -C kernel ui-latency-probe)" >&2
            exit 1
        fi
        debugfs -w -R "rm ui-latency-probe.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $PROBE_TEST_IMAGE ui-latency-probe.app" "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install the latency probe" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "display-latency" ] && [ "${CUBIT_LATENCY_LOAD:-0}" -gt 0 ]; then
        if ! make -s -C "$KERNEL_DIR" bench-load >/dev/null; then
            echo "headless: failed to build bench-load" >&2
            exit 1
        fi
        debugfs -w -R "rm bench-input-load.app" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "write $KERNEL_DIR/isodir/boot/bench-input-load.app bench-input-load.app" \
          "$TEMP_DISK" >/dev/null 2>&1 || exit 1
    fi
    if [ "$TEST_NAME" = "calculator" ] || [ "$TEST_NAME" = "display-latency" ] ||
       [ "$TEST_NAME" = "metrics-latency" ]; then
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
    if [ "$TEST_NAME" = "vm-desktop" ]; then
        if ! make -C "$KERNEL_DIR" vmm >/dev/null; then
            echo "headless: failed to build vmm.app" >&2
            exit 1
        fi
        debugfs -w -R "rm vmm.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/vmm.app vmm.app" "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install vmm.app" >&2
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
    if [ "$TEST_NAME" = "display-latency" ] && [ "${CUBIT_DISPLAY_LATENCY_ANIMATION:-0}" = 1 ]; then
        debugfs -w -R "rm frame-ticker.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/frame-ticker.app frame-ticker.app" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install frame-ticker.app" >&2
            exit 1
        fi
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
    if [ "$TEST_NAME" = "metrics-latency" ]; then
        # The series store (MET-002), the remote endpoint that reads it, and
        # the outlet fixture with and without its manifest's declaration.
        for METRICS_IMAGE_NAME in metrics.svc ccl-remote.app clock.svc logstore.svc \
                                  metrics-declared.app metrics-undeclared.app; do
            debugfs -w -R "rm $METRICS_IMAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$METRICS_IMAGE_NAME $METRICS_IMAGE_NAME" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $METRICS_IMAGE_NAME (make -C kernel metricsvc ccl-remote;" \
                     "tests/metrics-latency/outlet-fixture/build.sh)" >&2
                exit 1
            fi
        done
        debugfs -w -R "mkdir work" "$TEMP_DISK" >/dev/null 2>&1
    fi
    if [ "$TEST_NAME" = "ccl-remote-session" ] || [ "$TEST_NAME" = "remote-screenshot" ]; then
        # The endpoint, its work root and the device agent's message in
        # mail/to-host (the test takes it from the host side).
        for REMOTE_IMAGE_NAME in ccl-remote.app clock.svc logstore.svc; do
            debugfs -w -R "rm $REMOTE_IMAGE_NAME" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$REMOTE_IMAGE_NAME $REMOTE_IMAGE_NAME" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $REMOTE_IMAGE_NAME (make -C kernel ccl-remote)" >&2
                exit 1
            fi
        done
        REMOTE_MAIL="$TEMP_DISK.greeting.txt"
        printf 'hello host, from the device'"'"'s agent\n' > "$REMOTE_MAIL"
        REMOTE_COMMANDS="$TEMP_DISK.ccl-remote-session"
        {
            echo "mkdir work"
            echo "mkdir work/mail"
            echo "mkdir work/mail/to-host"
            echo "rm work/mail/to-host/greeting.txt"
            echo "write $REMOTE_MAIL work/mail/to-host/greeting.txt"
        } > "$REMOTE_COMMANDS"
        debugfs -w -f "$REMOTE_COMMANDS" "$TEMP_DISK" >/dev/null 2>&1
        rm -f "$REMOTE_COMMANDS" "$REMOTE_MAIL"
        if ! debugfs -R "stat \"work/mail/to-host/greeting.txt\"" "$TEMP_DISK" 2>/dev/null | grep -q "Size:"; then
            echo "headless: the remote session's mailbox was not seeded" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "device-build" ] || [ "$TEST_NAME" = "console-run-staged" ] || [ "$TEST_NAME" = "bench-build" ]; then
        # The programs on the development disk; the toolchain, Storehouse
        # and workspace on the UFS disk's root (tests/device-builds/build.sh).
        DEVICE_BUILD_IMAGES="ccl-remote.app clock.svc logstore.svc ccl-build.app"
        DEVICE_BUILD_TARGETS="libc user_runtime ccl-manifest ccl-build ccl-remote"
        if [ "$TEST_NAME" = "console-run-staged" ]; then
            DEVICE_BUILD_IMAGES="$DEVICE_BUILD_IMAGES ccl-console.app"
            DEVICE_BUILD_TARGETS="$DEVICE_BUILD_TARGETS ccl-console"
        fi
        # shellcheck disable=SC2086
        if ! make -C "$KERNEL_DIR" $DEVICE_BUILD_TARGETS >/dev/null ||
           ! bash "$ROOT_DIR/tests/device-builds/build.sh" "$KERNEL_DIR/isodir/boot" ||
           ! python3 -I "$ROOT_DIR/tests/device-builds/make-disk.py" "$TEMP_UFS_DISK" \
               "$ROOT_DIR/tests/device-builds/build/root.ext2"; then
            echo "headless: failed to build the device-build test" >&2
            exit 1
        fi
        for DEVICE_BUILD_IMAGE in $DEVICE_BUILD_IMAGES; do
            debugfs -w -R "rm $DEVICE_BUILD_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$DEVICE_BUILD_IMAGE $DEVICE_BUILD_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $DEVICE_BUILD_IMAGE" >&2
                exit 1
            fi
        done
        if debugfs -R "stat toolchain/bin/gcc" "$TEMP_DISK" 2>/dev/null | grep -q "Size:"; then
            echo "headless: the development disk carries a toolchain; device-build needs the RAM root's" >&2
            exit 1
        fi
        # PERF-010: the same builds by Linux's ccl-build on this host first
        # (the hosted reference), before QEMU competes for the CPUs.
        if [ "$TEST_NAME" = "bench-build" ] && [ "${BENCH_BUILD_LINUX:-1}" = 1 ] &&
           ! python3 "$ROOT_DIR/tests/device-builds/bench_linux.py" "$ROOT_DIR/tests/device-builds/build"; then
            echo "headless: bench-build: the Linux reference builds failed" >&2
            exit 1
        fi
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
    if [ "$TEST_NAME" = "kern014-bench" ]; then
        for K_IMAGE in logstore.svc clock.svc kern014-bench.app kern014-child.app; do
            debugfs -w -R "rm $K_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$K_IMAGE $K_IMAGE" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
    fi
    if [ "$TEST_NAME" = "bench-cualloc" ]; then
        for BENCH_CUALLOC_IMAGE in logstore.svc clock.svc cualloc-bench.app; do
            debugfs -w -R "rm $BENCH_CUALLOC_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$BENCH_CUALLOC_IMAGE $BENCH_CUALLOC_IMAGE" \
                "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $BENCH_CUALLOC_IMAGE (tests/userspace-allocator/cubit/build-cubit.sh)" >&2
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
    if [ "$TEST_NAME" = "fat32-volume" ] || [ "$TEST_NAME" = "usb-disk" ] || [ "$TEST_NAME" = "usb-eject" ]; then
        case "$TEST_NAME" in
            usb-disk) FAT_PROGRAM=usb-disk.app ;;
            usb-eject) FAT_PROGRAM=usb-eject.app ;;
            *) FAT_PROGRAM=fat-volume.app ;;
        esac
        for FAT_IMAGE in logstore.svc "$FAT_PROGRAM"; do
            debugfs -w -R "rm $FAT_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$FAT_IMAGE $FAT_IMAGE" \
                "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $FAT_IMAGE (tests/filesystem-fat/native or tests/usb-disk/native build-cubit.sh)" >&2
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
            if [ "$TEST_NAME" = "fs-coherence-hard-links" ]; then
                # One file, two names (FS-031): CuBit has no link operation
                # (Linux's run of the case makes them with link()). The
                # disk must be clean before, so that the check after means
                # something.
                HARD_LINKS="$TEMP_DISK.hard-links"
                head -c 3000 /dev/zero | tr '\0' H > "$HARD_LINKS"
                printf '%s\n' "write $HARD_LINKS fs-coherence/hard-links-a" \
                    "ln fs-coherence/hard-links-a fs-coherence/hard-links-b" \
                    "sif fs-coherence/hard-links-a links_count 2" > "$HARD_LINKS.commands"
                debugfs -w -f "$HARD_LINKS.commands" "$TEMP_DISK" >/dev/null 2>&1
                rm -f "$HARD_LINKS" "$HARD_LINKS.commands"
                if ! debugfs -R "stat fs-coherence/hard-links-b" "$TEMP_DISK" 2>/dev/null |
                       grep -q 'Links: 2'; then
                    echo "headless: failed to make the two names of fs-coherence/hard-links-a" >&2
                    exit 1
                fi
                if ! e2fsck -fn "$TEMP_DISK"; then
                    echo "headless: the fs-coherence-hard-links disk is inconsistent before boot" >&2
                    exit 1
                fi
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
    if [ "$TEST_NAME" = "wait" ]; then
        for WAIT_IMAGE in logstore.svc wait-check.app readiness-writer.app; do
            debugfs -w -R "rm $WAIT_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$WAIT_IMAGE $WAIT_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $WAIT_IMAGE (make -C kernel wait-check readiness-writer)" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "mailbox-self" ] || [ "$TEST_NAME" = "mailbox-mutual" ]; then
        if ! make -s -C "$KERNEL_DIR" bench-load >/dev/null; then
            echo "headless: failed to build bench-load" >&2
            exit 1
        fi
        for LOCK_IMAGE in logstore.svc bench-lock-self.app bench-lock-mutual.app; do
            debugfs -w -R "rm $LOCK_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$LOCK_IMAGE $LOCK_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $LOCK_IMAGE (make -C kernel bench-load)" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "futex-share" ]; then
        for SHARE_IMAGE in logstore.svc futex-share-server.app futex-share-client.app \
          futex-share-outsider.app; do
            debugfs -w -R "rm $SHARE_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$SHARE_IMAGE $SHARE_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $SHARE_IMAGE (make -C kernel futex-share)" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "debug-engine" ] || [ "$TEST_NAME" = "debugger-gui" ]; then
        # The demo, the engine's check, and the demo's build-id debug file
        # and sources in the @system/Debug layout (docs/image-debug-info.md).
        # debugger-gui (the Debugger app's test) attaches to the same demo.
        for DEBUG_IMAGE in logstore.svc debug-demo.app debug-engine-check.app; do
            debugfs -w -R "rm $DEBUG_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$DEBUG_IMAGE $DEBUG_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $DEBUG_IMAGE (make -C kernel debug-demo debug-engine-check)" >&2
                exit 1
            fi
        done
        DEBUG_DEMO="$KERNEL_DIR/isodir/boot/debug-demo.app"
        DEBUG_ID=$(readelf -nW "$DEBUG_DEMO" | sed -n 's/.*Build ID: \([0-9a-f]*\).*/\1/p')
        DEBUG_FILE="${TEMP_DISK%.img}.debug"
        # The sign step (SIGN-001, tools/sign_programs.py) strips the staged
        # program and keeps its debug information on the host by build-id,
        # zlib-compressed; the engine reads uncompressed sections only.
        DEBUG_SPLIT="$KERNEL_DIR/debug/.build-id/${DEBUG_ID:0:2}/${DEBUG_ID:2}.debug"
        if [ -n "$DEBUG_ID" ] && [ -f "$DEBUG_SPLIT" ]; then
            DEBUG_COPY=(objcopy --decompress-debug-sections "$DEBUG_SPLIT")
        else
            DEBUG_COPY=(objcopy --only-keep-debug "$DEBUG_DEMO")
        fi
        if [ -z "$DEBUG_ID" ] || ! "${DEBUG_COPY[@]}" "$DEBUG_FILE"; then
            echo "headless: debug-engine: no build-id debug file for debug-demo" >&2
            exit 1
        fi
        for DEBUG_DIR in Debug Debug/.build-id "Debug/.build-id/${DEBUG_ID:0:2}" Debug/Source \
          "Debug/Source/$DEBUG_ID"; do
            debugfs -w -R "mkdir $DEBUG_DIR" "$TEMP_DISK" >/dev/null 2>&1
        done
        debugfs -w -R "write $DEBUG_FILE Debug/.build-id/${DEBUG_ID:0:2}/${DEBUG_ID:2}.debug" \
          "$TEMP_DISK" >/dev/null 2>&1
        rm -f "$DEBUG_FILE"
        for DEBUG_SOURCE in "$ROOT_DIR"/userspace/apps/debug-demo/*.ad[sb] \
          "$ROOT_DIR"/userspace/lib/debug/stub/*.ad[sb]; do
            debugfs -w -R "write $DEBUG_SOURCE Debug/Source/$DEBUG_ID/${DEBUG_SOURCE##*/}" \
              "$TEMP_DISK" >/dev/null 2>&1
        done
        if [ "$TEST_NAME" = "debugger-gui" ]; then
            # Attach to process...: clock.svc's debug file too (no stub).
            CLOCK_ELF="$KERNEL_DIR/isodir/boot/clock.svc"
            CLOCK_ID=$(readelf -nW "$CLOCK_ELF" | sed -n 's/.*Build ID: \([0-9a-f]*\).*/\1/p')
            CLOCK_FILE="${TEMP_DISK%.img}-clock.debug"
            CLOCK_SPLIT="$KERNEL_DIR/debug/.build-id/${CLOCK_ID:0:2}/${CLOCK_ID:2}.debug"
            if [ -n "$CLOCK_ID" ] && [ -f "$CLOCK_SPLIT" ]; then
                CLOCK_COPY=(objcopy --decompress-debug-sections "$CLOCK_SPLIT")
            else
                CLOCK_COPY=(objcopy --only-keep-debug "$CLOCK_ELF")
            fi
            if [ -n "$CLOCK_ID" ] && "${CLOCK_COPY[@]}" "$CLOCK_FILE"; then
                debugfs -w -R "mkdir Debug/.build-id/${CLOCK_ID:0:2}" "$TEMP_DISK" >/dev/null 2>&1
                debugfs -w -R "write $CLOCK_FILE Debug/.build-id/${CLOCK_ID:0:2}/${CLOCK_ID:2}.debug" \
                  "$TEMP_DISK" >/dev/null 2>&1
                # Its sources, and the runtime units its waiting thread sits in.
                debugfs -w -R "mkdir Debug/Source/$CLOCK_ID" "$TEMP_DISK" >/dev/null 2>&1
                for CLOCK_SOURCE in "$ROOT_DIR"/userspace/services/clock/*.ad[sb] \
                  "$ROOT_DIR"/userspace/runtime/gnat/cubit-kernel_calls.adb \
                  "$ROOT_DIR"/userspace/runtime/gnat/cubit-waits.adb \
                  "$ROOT_DIR"/userspace/runtime/gnat/cubit-messages.adb; do
                    debugfs -w -R "write $CLOCK_SOURCE Debug/Source/$CLOCK_ID/${CLOCK_SOURCE##*/}" \
                      "$TEMP_DISK" >/dev/null 2>&1
                done
            fi
            rm -f "$CLOCK_FILE"
        fi
    fi
    case "$TEST_NAME" in debug-gate-*)
        if ! make -C "$KERNEL_DIR" debug-kernel >/dev/null; then
            echo "headless: make -C kernel debug-kernel failed" >&2
            exit 1
        fi
        for DEBUG_IMAGE in debug-plain.app debug-gate-check.app; do
            debugfs -w -R "rm $DEBUG_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$DEBUG_IMAGE $DEBUG_IMAGE" "$TEMP_DISK" \
              >/dev/null 2>&1 || exit 1
        done
        ;;
    esac
    if [ "$TEST_NAME" = "debug-kernel" ]; then
        # The plain target (no stub), the engine's kernel-path check, and
        # the build-id debug files of debug-plain and clock.svc in the
        # @system/Debug layout (docs/image-debug-info.md).
        if ! make -C "$KERNEL_DIR" debug-kernel >/dev/null; then
            echo "headless: make -C kernel debug-kernel failed" >&2
            exit 1
        fi
        for DEBUG_IMAGE in debug-plain.app debug-kernel-check.app; do
            debugfs -w -R "rm $DEBUG_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$DEBUG_IMAGE $DEBUG_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $DEBUG_IMAGE (make -C kernel debug-kernel)" >&2
                exit 1
            fi
        done
        debugfs -w -R "mkdir Debug" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "mkdir Debug/.build-id" "$TEMP_DISK" >/dev/null 2>&1
        for DEBUG_PROGRAM in debug-plain.app clock.svc; do
            DEBUG_ELF="$KERNEL_DIR/isodir/boot/$DEBUG_PROGRAM"
            DEBUG_ID=$(readelf -nW "$DEBUG_ELF" | sed -n 's/.*Build ID: \([0-9a-f]*\).*/\1/p')
            DEBUG_FILE="${TEMP_DISK%.img}-$DEBUG_PROGRAM.debug"
            # The sign step keeps each program's debug information on the
            # host by build-id, zlib-compressed (SIGN-001).
            DEBUG_SPLIT="$KERNEL_DIR/debug/.build-id/${DEBUG_ID:0:2}/${DEBUG_ID:2}.debug"
            if [ -n "$DEBUG_ID" ] && [ -f "$DEBUG_SPLIT" ]; then
                DEBUG_COPY=(objcopy --decompress-debug-sections "$DEBUG_SPLIT")
            else
                DEBUG_COPY=(objcopy --only-keep-debug "$DEBUG_ELF")
            fi
            if [ -z "$DEBUG_ID" ] || ! "${DEBUG_COPY[@]}" "$DEBUG_FILE"; then
                echo "headless: debug-kernel: no build-id debug file for $DEBUG_PROGRAM" >&2
                exit 1
            fi
            debugfs -w -R "mkdir Debug/.build-id/${DEBUG_ID:0:2}" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $DEBUG_FILE Debug/.build-id/${DEBUG_ID:0:2}/${DEBUG_ID:2}.debug" \
              "$TEMP_DISK" >/dev/null 2>&1
            rm -f "$DEBUG_FILE"
        done
    fi
    if [ "$TEST_NAME" = "debug-engine-rs" ] || [ "$TEST_NAME" = "debugger-gui-rs" ]; then
        # The Rust demo, the engine's Rust check, and the demo's build-id
        # debug file and sources in the @system/Debug layout.
        for DEBUG_IMAGE in logstore.svc debug-demo-rs.app debug-engine-rs-check.app; do
            debugfs -w -R "rm $DEBUG_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$DEBUG_IMAGE $DEBUG_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $DEBUG_IMAGE (make -C kernel debug-demo-rs debug-engine-rs-check)" >&2
                exit 1
            fi
        done
        DEBUG_DEMO="$KERNEL_DIR/isodir/boot/debug-demo-rs.app"
        DEBUG_ID=$(readelf -nW "$DEBUG_DEMO" | sed -n 's/.*Build ID: \([0-9a-f]*\).*/\1/p')
        DEBUG_FILE="${TEMP_DISK%.img}.debug"
        # The sign step (SIGN-001, tools/sign_programs.py) strips the staged
        # program and keeps its debug information on the host by build-id,
        # zlib-compressed; the engine reads uncompressed sections only.
        DEBUG_SPLIT="$KERNEL_DIR/debug/.build-id/${DEBUG_ID:0:2}/${DEBUG_ID:2}.debug"
        if [ -n "$DEBUG_ID" ] && [ -f "$DEBUG_SPLIT" ]; then
            DEBUG_COPY=(objcopy --decompress-debug-sections "$DEBUG_SPLIT")
        else
            DEBUG_COPY=(objcopy --only-keep-debug "$DEBUG_DEMO")
        fi
        if [ -z "$DEBUG_ID" ] || ! "${DEBUG_COPY[@]}" "$DEBUG_FILE"; then
            echo "headless: debug-engine-rs: no build-id debug file for debug-demo-rs" >&2
            exit 1
        fi
        for DEBUG_DIR in Debug Debug/.build-id "Debug/.build-id/${DEBUG_ID:0:2}" Debug/Source \
          "Debug/Source/$DEBUG_ID"; do
            debugfs -w -R "mkdir $DEBUG_DIR" "$TEMP_DISK" >/dev/null 2>&1
        done
        debugfs -w -R "write $DEBUG_FILE Debug/.build-id/${DEBUG_ID:0:2}/${DEBUG_ID:2}.debug" \
          "$TEMP_DISK" >/dev/null 2>&1
        rm -f "$DEBUG_FILE"
        for DEBUG_SOURCE in "$ROOT_DIR"/userspace/apps/debug-demo-rs/src/main.rs \
          "$ROOT_DIR"/userspace/lib/debug/stub-rs/src/lib.rs; do
            debugfs -w -R "write $DEBUG_SOURCE Debug/Source/$DEBUG_ID/${DEBUG_SOURCE##*/}" \
              "$TEMP_DISK" >/dev/null 2>&1
        done
    fi
    if [ "$TEST_NAME" = "tlb-tags" ]; then
        for TLB_IMAGE in logstore.svc tlb-check.app tlb-probe.app; do
            debugfs -w -R "rm $TLB_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$TLB_IMAGE $TLB_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $TLB_IMAGE (make -C kernel tlb-tags)" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "secondary-stacks" ]; then
        for STACKS_IMAGE in logstore.svc secondary-stack-check.app; do
            debugfs -w -R "rm $STACKS_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$STACKS_IMAGE $STACKS_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $STACKS_IMAGE (make -C kernel secondary-stack-check)" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "ada-runtime" ]; then
        for RUNTIME_IMAGE in logstore.svc ada-runtime-check.app; do
            debugfs -w -R "rm $RUNTIME_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$RUNTIME_IMAGE $RUNTIME_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $RUNTIME_IMAGE (make -C kernel ada-runtime-check)" >&2
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
    if [ "$TEST_NAME" = "acpi-hardware" ]; then
        # acpi-hardware-denied.app is acpi-hardware-check.app under another
        # name: the same declaration, without (acpi-hardware approve-declared).
        for HARDWARE_PAIR in logstore.svc:logstore.svc \
                             acpi-hardware-check.app:acpi-hardware-check.app \
                             acpi-hardware-check.app:acpi-hardware-denied.app; do
            HARDWARE_SOURCE="$KERNEL_DIR/isodir/boot/${HARDWARE_PAIR%%:*}"
            HARDWARE_TARGET="${HARDWARE_PAIR##*:}"
            if [ ! -f "$HARDWARE_SOURCE" ]; then
                echo "headless: build ${HARDWARE_PAIR%%:*} first (make -C kernel acpi-hardware-check)" >&2
                exit 1
            fi
            debugfs -w -R "rm $HARDWARE_TARGET" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $HARDWARE_SOURCE $HARDWARE_TARGET" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $HARDWARE_TARGET" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "acpi-pci-config" ]; then
        # acpi-pci-unapproved.app is acpi-pci-check.app under another name:
        # the same declaration, approved for the hardware grant but not for
        # PCI configuration.
        for PCI_PAIR in logstore.svc:logstore.svc \
                        acpi-pci-check.app:acpi-pci-check.app \
                        acpi-pci-check.app:acpi-pci-unapproved.app; do
            PCI_SOURCE="$KERNEL_DIR/isodir/boot/${PCI_PAIR%%:*}"
            PCI_TARGET="${PCI_PAIR##*:}"
            if [ ! -f "$PCI_SOURCE" ]; then
                echo "headless: build ${PCI_PAIR%%:*} first (make -C kernel acpi-pci-check)" >&2
                exit 1
            fi
            debugfs -w -R "rm $PCI_TARGET" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $PCI_SOURCE $PCI_TARGET" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $PCI_TARGET" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "gpio-launch" ]; then
        # hid-launch-check.drv is in the bootstrap image (devmgr's driver
        # index binds PNP0C50 to it); procmgr, which starts it through
        # Start_Child, reads programs from the disk.
        for GPIO_IMAGE in logstore.svc gpio-bootstrap-check.app hid-launch-check.drv; do
            if [ ! -f "$KERNEL_DIR/isodir/boot/$GPIO_IMAGE" ]; then
                echo "headless: build $GPIO_IMAGE first (make -C kernel world gpio-bootstrap-check)" >&2
                exit 1
            fi
            debugfs -w -R "rm $GPIO_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$GPIO_IMAGE $GPIO_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $GPIO_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "platform-pool" ]; then
        for POOL_IMAGE in logstore.svc platform-pool-check.app; do
            if [ ! -f "$KERNEL_DIR/isodir/boot/$POOL_IMAGE" ]; then
                echo "headless: build $POOL_IMAGE first (make -C kernel world platform-pool-check)" >&2
                exit 1
            fi
            debugfs -w -R "rm $POOL_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$POOL_IMAGE $POOL_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $POOL_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "acpi-firmware-data" ]; then
        # acpi-data-unapproved.app is acpi-data-check.app under another name:
        # the same declaration, approved for the hardware grant but not for
        # firmware data.
        for DATA_PAIR in logstore.svc:logstore.svc \
                         acpi-data-check.app:acpi-data-check.app \
                         acpi-data-check.app:acpi-data-unapproved.app; do
            DATA_SOURCE="$KERNEL_DIR/isodir/boot/${DATA_PAIR%%:*}"
            DATA_TARGET="${DATA_PAIR##*:}"
            if [ ! -f "$DATA_SOURCE" ]; then
                echo "headless: build ${DATA_PAIR%%:*} first (make -C kernel acpi-data-check)" >&2
                exit 1
            fi
            debugfs -w -R "rm $DATA_TARGET" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $DATA_SOURCE $DATA_TARGET" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $DATA_TARGET" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "acpi-one-service" ] || [ "$TEST_NAME" = "acpi-sci" ] || [ "$TEST_NAME" = "acpi-shutdown" ]; then
        ACPI_FILES="acpi-metrics-check.app:acpi-metrics-check.app ../../../userspace/services/acpi/native/build/acpi.svc:acpi.svc"
        for ACPI_PAIR in logstore.svc:logstore.svc $ACPI_FILES; do
            ACPI_SOURCE="$KERNEL_DIR/isodir/boot/${ACPI_PAIR%%:*}"
            ACPI_TARGET="${ACPI_PAIR##*:}"
            if [ ! -f "$ACPI_SOURCE" ]; then
                echo "headless: build ${ACPI_PAIR%%:*} first (make -C kernel acpi-native acpi-metrics-check)" >&2
                exit 1
            fi
            debugfs -w -R "rm $ACPI_TARGET" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $ACPI_SOURCE $ACPI_TARGET" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $ACPI_TARGET" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "acpi-live-replace" ]; then
        # The one acpi.svc and the remote session, as built now.
        for SWITCH_PAIR in logstore.svc:logstore.svc clock.svc:clock.svc ccl-remote.app:ccl-remote.app \
            ../../../userspace/services/acpi/native/build/acpi.svc:acpi.svc; do
            SWITCH_SOURCE="$KERNEL_DIR/isodir/boot/${SWITCH_PAIR%%:*}"
            SWITCH_TARGET="${SWITCH_PAIR##*:}"
            if [ ! -f "$SWITCH_SOURCE" ]; then
                echo "headless: build ${SWITCH_PAIR%%:*} first (make -C kernel world acpi-native)" >&2
                exit 1
            fi
            debugfs -w -R "rm $SWITCH_TARGET" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $SWITCH_SOURCE $SWITCH_TARGET" "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $SWITCH_TARGET" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "desktop-live-replace" ]; then
        # Desktop and the remote session, as built now.
        for SWITCH_PAIR in logstore.svc:logstore.svc clock.svc:clock.svc ccl-remote.app:ccl-remote.app \
            display.svc:display.svc desktop.svc:desktop.svc timesync.svc:timesync.svc \
            metrics.svc:metrics.svc lexicon.svc:lexicon.svc; do
            SWITCH_SOURCE="$KERNEL_DIR/isodir/boot/${SWITCH_PAIR%%:*}"
            SWITCH_TARGET="${SWITCH_PAIR##*:}"
            if [ ! -f "$SWITCH_SOURCE" ]; then
                echo "headless: build ${SWITCH_PAIR%%:*} first (make -C kernel world)" >&2
                exit 1
            fi
            debugfs -w -R "rm $SWITCH_TARGET" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $SWITCH_SOURCE $SWITCH_TARGET" "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $SWITCH_TARGET" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "service-switch" ]; then
        # The services switched and the remote session, as built now.
        for SWITCH_PAIR in logstore.svc:logstore.svc clock.svc:clock.svc ccl-remote.app:ccl-remote.app \
            lexicon.svc:lexicon.svc; do
            SWITCH_SOURCE="$KERNEL_DIR/isodir/boot/${SWITCH_PAIR%%:*}"
            SWITCH_TARGET="${SWITCH_PAIR##*:}"
            if [ ! -f "$SWITCH_SOURCE" ]; then
                echo "headless: build ${SWITCH_PAIR%%:*} first (make -C kernel world)" >&2
                exit 1
            fi
            debugfs -w -R "rm $SWITCH_TARGET" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $SWITCH_SOURCE $SWITCH_TARGET" "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $SWITCH_TARGET" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "cpu-times" ]; then
        for CPU_TIMES_IMAGE in logstore.svc cpu-times-check.app; do
            debugfs -w -R "rm $CPU_TIMES_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$CPU_TIMES_IMAGE $CPU_TIMES_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $CPU_TIMES_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "clock-keeper" ]; then
        for CLOCK_IMAGE in logstore.svc clock-keeper-check.app; do
            debugfs -w -R "rm $CLOCK_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$CLOCK_IMAGE $CLOCK_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $CLOCK_IMAGE (make -C kernel clock-keeper-check)" >&2
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
    if [ "$TEST_NAME" = "process-kill" ] || [ "$TEST_NAME" = "trap-flag" ] || [ "$TEST_NAME" = "nmi" ] ||
       [ "$TEST_NAME" = "direction-flag" ] || [ "$TEST_NAME" = "alignment-flag" ] ||
       [ "$TEST_NAME" = "kernel-map-probe" ] || [ "$TEST_NAME" = "mov-ss-debug" ]; then
        for KILL_IMAGE in logstore.svc clock.svc fault-check.app trap-flag-check.app nmi-check.app \
                          direction-flag-check.app alignment-flag-check.app kernel-map-probe.app \
                          mov-ss-check.app; do
            debugfs -w -R "rm $KILL_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$KILL_IMAGE $KILL_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $KILL_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "desktop-reap" ]; then
        debugfs -w -R "rm reap-check.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/reap-check.app reap-check.app" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install reap-check.app (make -C kernel reap-check)" >&2
            exit 1
        fi
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
    if [ "$TEST_NAME" = "wifi-crypto" ]; then
        for WIFI_CRYPTO_IMAGE in logstore.svc wifi-crypto-check.app; do
            debugfs -w -R "rm $WIFI_CRYPTO_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$WIFI_CRYPTO_IMAGE $WIFI_CRYPTO_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $WIFI_CRYPTO_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "font-masks" ]; then
        for FONT_IMAGE in logstore.svc font-masks.app; do
            debugfs -w -R "rm $FONT_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$FONT_IMAGE $FONT_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $FONT_IMAGE" >&2
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
    if [ "$TEST_NAME" = "large-manifest" ]; then
        # The program with the large manifest (tests/large-manifest-probe).
        if ! make -C "$KERNEL_DIR" large-manifest-probe >/dev/null; then
            echo "headless: failed to build large-manifest-probe" >&2
            exit 1
        fi
        for LARGE_IMAGE in logstore.svc large-manifest-probe.app; do
            debugfs -w -R "rm $LARGE_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$LARGE_IMAGE $LARGE_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $LARGE_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "startup-wait" ]; then
        # The late provider and its client (tests/startup-wait).
        if ! make -C "$KERNEL_DIR" startup-wait-test >/dev/null; then
            echo "headless: failed to build the startup-wait test programs" >&2
            exit 1
        fi
        for WAIT_IMAGE in logstore.svc startup-wait-provider.app startup-wait-client.app; do
            debugfs -w -R "rm $WAIT_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$WAIT_IMAGE $WAIT_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $WAIT_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "exit-status" ]; then
        if ! make -C "$KERNEL_DIR" exit-status-test >/dev/null; then
            echo "headless: failed to build exit-status-check (make -C kernel exit-status-test)" >&2
            exit 1
        fi
        debugfs -w -R "rm exit-status-check.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/exit-status-check.app exit-status-check.app" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install exit-status-check.app" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "graphics-report-nodriver" ] || [ "$TEST_NAME" = "graphics-report-failing" ] ||
       [ "$TEST_NAME" = "graphics-report-desktop-down" ]; then
        # GPU-014: the current console, Desktop and check on the test disk;
        # CUBIT_DESKTOP_IMAGE (a test build) stands in for desktop.svc.
        if [ "$TEST_NAME" = "graphics-report-failing" ] &&
           [ "${CUBIT_GPU_TEST_MODE:-production}" != init-failure ]; then
            echo "headless: graphics-report-failing needs CUBIT_GPU_TEST_MODE=init-failure" >&2
            exit 1
        fi
        if [ "$TEST_NAME" = "graphics-report-desktop-down" ] && [ -z "${CUBIT_DESKTOP_IMAGE:-}" ]; then
            echo "headless: graphics-report-desktop-down needs CUBIT_DESKTOP_IMAGE (CUBIT_DESKTOP_FAULT=exit-once)" >&2
            exit 1
        fi
        for GRAPHICS_IMAGE in ccl-console.app desktop.svc display.svc clock.svc graphics-report-check.app; do
            GRAPHICS_SOURCE="$KERNEL_DIR/isodir/boot/$GRAPHICS_IMAGE"
            if [ "$GRAPHICS_IMAGE" = desktop.svc ] && [ -n "${CUBIT_DESKTOP_IMAGE:-}" ]; then
                GRAPHICS_SOURCE="$CUBIT_DESKTOP_IMAGE"
            fi
            debugfs -w -R "rm $GRAPHICS_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $GRAPHICS_SOURCE $GRAPHICS_IMAGE" "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $GRAPHICS_IMAGE from $GRAPHICS_SOURCE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "power-sources" ]; then
        if ! make -C "$KERNEL_DIR" power-sources-test power >/dev/null; then
            echo "headless: failed to build power.svc and the power-sources stand-in" >&2
            exit 1
        fi
        for POWER_IMAGE in power.svc power-standin.app ccl-console.app desktop.svc; do
            debugfs -w -R "rm $POWER_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$POWER_IMAGE $POWER_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $POWER_IMAGE" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "acpi-shutdown" ] || [ "$TEST_NAME" = "shutdown" ] || [ "$TEST_NAME" = "shutdown-console" ] || [ "$TEST_NAME" = "shutdown-text" ] ||
       [ "$TEST_NAME" = "shutdown-desktop" ] || [ "$TEST_NAME" = "shutdown-desktop-cancel" ]; then
        if ! make -C "$KERNEL_DIR" shutdown-test power >/dev/null; then
            echo "headless: failed to build power.svc and the shutdown test programs" >&2
            exit 1
        fi
        for SHUTDOWN_IMAGE in logstore.svc power.svc shutdown-standin.app shutdown-cooperative.app \
                              shutdown-stubborn.app; do
            debugfs -w -R "rm $SHUTDOWN_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$SHUTDOWN_IMAGE $SHUTDOWN_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $SHUTDOWN_IMAGE" >&2
                exit 1
            fi
        done
        if [ "$TEST_NAME" = "shutdown-text" ]; then
            debugfs -w -R "mkdir Documents" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "mkdir Recovery" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "mkdir Recovery/com.cubit.text" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "rm Documents/notes.txt" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "rm Recovery/com.cubit.text/recovery-1.txt" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $ROOT_DIR/tests/headless/text-notes.txt Documents/notes.txt" \
                 "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install Text's document" >&2
                exit 1
            fi
        fi
        if [ "$TEST_NAME" != "shutdown" ] && [ "$TEST_NAME" != "acpi-shutdown" ]; then
            for SHUTDOWN_IMAGE in ccl-console.app desktop.svc text.app; do
                debugfs -w -R "rm $SHUTDOWN_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
                if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$SHUTDOWN_IMAGE $SHUTDOWN_IMAGE" \
                  "$TEMP_DISK" >/dev/null 2>&1; then
                    echo "headless: failed to install $SHUTDOWN_IMAGE" >&2
                    exit 1
                fi
            done
        fi
        # Journaled (ext3), so a clean stop is visible: the filesystem sets
        # needs_recovery when it admits the volume and only its stop-time
        # detach clears it. The disk must start without it.
        if ! dumpe2fs -h "$TEMP_DISK" 2>/dev/null | grep -q has_journal && \
           ! tune2fs -j "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to add a journal to the $TEST_NAME disk" >&2
            exit 1
        fi
        if dumpe2fs -h "$TEMP_DISK" 2>/dev/null | grep -q needs_recovery; then
            echo "headless: the $TEST_NAME disk needs recovery before the run" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "supervision" ]; then
        if ! make -C "$KERNEL_DIR" supervision-test >/dev/null; then
            echo "headless: failed to build the supervision test programs" >&2
            exit 1
        fi
        for SUPERVISION_IMAGE in logstore.svc supervision-check.app supervision-child.app; do
            debugfs -w -R "rm $SUPERVISION_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/$SUPERVISION_IMAGE $SUPERVISION_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1; then
                echo "headless: failed to install $SUPERVISION_IMAGE" >&2
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
          "line 16, column 10: parameters-check.app requires its parameters, a Check_Parameters" \
          "line 19, column 16: parameters-check.app: not a Check_Parameters: Value is outside the range of its type" \
          "line 22, column 16: parameters-plain.app declares no parameters, so it takes no arguments"; do
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
    if [ "$TEST_NAME" = "ladybird-js" ]; then
        # js and the launcher at the root; check.js and ICU's data file in
        # @nvme:0/ladybird, which the launcher delegates to js read-only.
        if ! make -C "$KERNEL_DIR" libc user_runtime ccl-manifest >/dev/null ||
           ! bash "$ROOT_DIR/tests/ladybird/build.sh" "$KERNEL_DIR/isodir/boot" >/dev/null; then
            echo "headless: failed to build the ladybird-js test" >&2
            exit 1
        fi
        LADYBIRD_COMMANDS="$TEMP_DISK.ladybird"
        {
            for LADYBIRD_IMAGE in logstore.svc clock.svc ladybird-js.app ladybird-js-check.app; do
                echo "rm $LADYBIRD_IMAGE"
                echo "write $KERNEL_DIR/isodir/boot/$LADYBIRD_IMAGE $LADYBIRD_IMAGE"
            done
            echo "mkdir ladybird"
            for LADYBIRD_FILE in "$ROOT_DIR/tests/ladybird/build/disk/"*; do
                echo "rm ladybird/$(basename "$LADYBIRD_FILE")"
                echo "write $LADYBIRD_FILE ladybird/$(basename "$LADYBIRD_FILE")"
            done
        } > "$LADYBIRD_COMMANDS"
        debugfs -w -f "$LADYBIRD_COMMANDS" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        rm -f "$LADYBIRD_COMMANDS"
        for LADYBIRD_CHECK in ladybird-js.app ladybird/check.js ladybird/icudt78l.dat; do
            if ! debugfs -R "stat $LADYBIRD_CHECK" "$TEMP_DISK" 2>/dev/null | grep -q "Size:"; then
                echo "headless: $LADYBIRD_CHECK was not installed" >&2
                exit 1
            fi
        done
    fi
    if [ "$TEST_NAME" = "ladybird-test262" ]; then
        # The runner and the launcher at the root; ICU's data in
        # @nvme:0/ladybird; the staged test262 (harness, tests, list.txt) in
        # @nvme:0/test262. Its 50,000 files need more room than the base
        # disk has: the test disk is grown first.
        if ! make -C "$KERNEL_DIR" libc user_runtime ccl-manifest >/dev/null ||
           ! bash "$ROOT_DIR/tests/ladybird/build.sh" "$KERNEL_DIR/isodir/boot" >/dev/null; then
            echo "headless: failed to build the ladybird-test262 test" >&2
            exit 1
        fi
        LADYBIRD_STAGE="${LADYBIRD_STAGE:-$KERNEL_DIR/build/ladybird}"
        if [ ! -f "$KERNEL_DIR/isodir/boot/ladybird-test262.app" ] || [ ! -f "$LADYBIRD_STAGE/test262/list.txt" ]; then
            echo "headless: no staged test262-runner or test262 in $LADYBIRD_STAGE" >&2
            exit 1
        fi
        truncate -s "${TEST262_DISK_BYTES:-1610612736}" "$TEMP_DISK" &&
            e2fsck -fy "$TEMP_DISK" >/dev/null 2>&1; resize2fs "$TEMP_DISK" >/dev/null 2>&1 || {
            echo "headless: could not grow the test disk" >&2
            exit 1
        }
        LADYBIRD_COMMANDS="$TEMP_DISK.ladybird"
        {
            for LADYBIRD_IMAGE in logstore.svc clock.svc ladybird-test262.app ladybird-test262-check.app; do
                echo "rm $LADYBIRD_IMAGE"
                echo "write $KERNEL_DIR/isodir/boot/$LADYBIRD_IMAGE $LADYBIRD_IMAGE"
            done
            echo "mkdir ladybird"
            for LADYBIRD_FILE in "$LADYBIRD_STAGE/data/"icudt*l.dat; do
                echo "write $LADYBIRD_FILE ladybird/$(basename "$LADYBIRD_FILE")"
            done
            echo "mkdir test262"
            (cd "$LADYBIRD_STAGE/test262" && find harness test -type d | LC_ALL=C sort | sed 's|^|mkdir test262/|')
            (cd "$LADYBIRD_STAGE/test262" && find harness test list.txt -type f | LC_ALL=C sort |
               sed "s|^\(.*\)\$|write $LADYBIRD_STAGE/test262/\1 test262/\1|")
        } > "$LADYBIRD_COMMANDS"
        debugfs -w -f "$LADYBIRD_COMMANDS" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        rm -f "$LADYBIRD_COMMANDS"
        for LADYBIRD_CHECK in ladybird-test262.app test262/list.txt test262/harness/assert.js ladybird/icudt78l.dat; do
            if ! debugfs -R "stat $LADYBIRD_CHECK" "$TEMP_DISK" 2>/dev/null | grep -q "Size:"; then
                echo "headless: $LADYBIRD_CHECK was not installed" >&2
                exit 1
            fi
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
    if [ "$TEST_NAME" = "ccl-build-native" ]; then
        # The launcher, ccl-build.app and the comparer at the root; the
        # toolchain; the manifest step's tool at @nvme:0/tools; the source
        # subset in @nvme:0/cubit-src; an empty Storehouse; Linux's ps2.drv
        # in @nvme:0/work.
        CBN_DIR="$ROOT_DIR/tests/ccl-build-native/build"
        if ! make -C "$KERNEL_DIR" libc user_runtime ccl-manifest ccl-build >/dev/null ||
           ! bash "$ROOT_DIR/tests/ccl-build-native/build.sh" "$KERNEL_DIR/isodir/boot"; then
            echo "headless: failed to build the ccl-build-native test" >&2
            exit 1
        fi
        for CBN_IMAGE in logstore.svc ccl-build-check.app ccl-build.app binutils-compare.app; do
            debugfs -w -R "rm $CBN_IMAGE" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$CBN_IMAGE $CBN_IMAGE" \
              "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
        install_gcc_toolchain || exit 1
        # A tree's directories, then its files, as debugfs commands.
        cbn_install_tree() {  # cbn_install_tree SOURCE TARGET
            echo "mkdir $2"
            (cd "$1" && find . -mindepth 1 -type d | sort | sed "s|^\./|mkdir $2/|")
            (cd "$1" && find . -type f | sort | sed "s|^\./\(.*\)|write $1/\1 $2/\1|")
        }
        CBN_COMMANDS="$TEMP_DISK.ccl-build-native"
        {
            cbn_install_tree "$CBN_DIR/cubit-src" cubit-src
            cbn_install_tree "$CBN_DIR/tools" tools
            echo "mkdir storehouse"
            echo "mkdir work"
            echo "rm work/expected-ps2.drv"
            echo "write $CBN_DIR/expected-ps2.drv work/expected-ps2.drv"
        } > "$CBN_COMMANDS"
        debugfs -w -f "$CBN_COMMANDS" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        rm -f "$CBN_COMMANDS"
        for CBN_CHECK in ccl-build.app cubit-src/build-params.ccl cubit-src/userspace/runtime/adalib/libgnat-user.a \
          cubit-src/userspace/services/ps2/main.adb tools/ccl-manifest.app work/expected-ps2.drv \
          toolchain/libexec/gcc/x86_64-linux-musl/15.3.0/gnat1; do
            if ! debugfs -R "stat \"$CBN_CHECK\"" "$TEMP_DISK" 2>/dev/null | grep -q "Size:"; then
                echo "headless: $CBN_CHECK was not installed" >&2
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
        # No font directory, whatever the base image carries (Penny's fonts
        # are linked in; the laptop's disk has none).
        rm -rf "$SERVO_STAGE/fonts"
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
        # The pages cubitshell loads: a data: page, then one the host serves
        # over HTTP (tests/servo/http_server.py) through netstack, one over
        # HTTPS, and a page of text only, whose ink can only be glyphs
        # (tests/servo/check_text_glyphs.py).
        SERVO_PAGES="$(mktemp "${TMPDIR:-/tmp}/cubit-servo-pages.XXXXXX")"
        cat > "$SERVO_PAGES" <<'SERVO_PAGES_EOF'
data:text/html,<body style='background:%23fff'><h1 style='color:%23135'>Servo on CuBit</h1><div style='width:200px;height:100px;background:%23e33'></div></body>
http://10.0.2.2:18470/servo-test.html
https://tls-test.cubit.internal:18460/
data:text/html,<title>penny-text-check</title><body style='background:%23fff;color:%23000;font:32px sans-serif'>Penny draws text. 0123456789 ABCDEFGHIJKLM nopqrstuvwxyz</body>
SERVO_PAGES_EOF
        SERVO_TEXT_PAGE=$(( $(grep -n 'penny-text-check' "$SERVO_PAGES" | cut -d: -f1) - 1 ))
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
        # Opt-in: per-frame paint/present timing (PENNY-FRAME lines).
        if [ -n "${SERVO_PERF_CHECK:-}" ]; then
            SERVO_FLAG="$(mktemp "${TMPDIR:-/tmp}/cubit-servo-flag.XXXXXX")"
            servo_install "$SERVO_FLAG" "servo/perf-check"
            rm -f "$SERVO_FLAG"
        fi
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
    if [ "$TEST_NAME" = "llm-mock" ] || [ "$TEST_NAME" = "agent-mock" ] || [ "$TEST_NAME" = "agent-app" ]; then
        # The test root is tls.svc's whole trust store for this run: only
        # the mock's certificate verifies.
        if ! bash "$ROOT_DIR/tests/llm/make-pki.sh" >/dev/null; then
            echo "headless: failed to make the llm mock's certificate" >&2
            exit 1
        fi
        debugfs -w -R "mkdir tls" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "rm tls/roots.der" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $ROOT_DIR/tests/tls/build/pki/ca.der tls/roots.der" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install the llm mock's trust store" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "net-fetch" ]; then
        # The test root is tls.svc's whole trust store: only the fixture's
        # certificate verifies.
        if ! bash "$ROOT_DIR/tests/net-fetch/make-pki.sh" >/dev/null; then
            echo "headless: failed to make the fetch fixture's certificate" >&2
            exit 1
        fi
        debugfs -w -R "mkdir tls" "$TEMP_DISK" >/dev/null 2>&1
        debugfs -w -R "rm tls/roots.der" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $ROOT_DIR/tests/tls/build/pki/ca.der tls/roots.der" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install the fetch fixture's trust store" >&2
            exit 1
        fi
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
    if [ "$TEST_NAME" = "launch-gate" ]; then
        if ! make -C "$KERNEL_DIR" launch-gate >/dev/null; then
            echo "headless: make -C kernel launch-gate failed" >&2
            exit 1
        fi
        for app in capability-test launch-gate-check launch-gate-child launch-gate-greedy; do
            debugfs -w -R "rm $app.app" "$TEMP_DISK" >/dev/null 2>&1
            debugfs -w -R "write $KERNEL_DIR/isodir/boot/$app.app $app.app" "$TEMP_DISK" >/dev/null 2>&1 || exit 1
        done
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
    if [ "$TEST_NAME" = "log-fields" ] || [ "$TEST_NAME" = "log-retention" ] || [ "$TEST_NAME" = "metrics" ]; then
        if [ "$TEST_NAME" = "log-fields" ]; then
            OBSERVABILITY_IMAGES="logstore.svc log-fields-check.app"
        elif [ "$TEST_NAME" = "log-retention" ]; then
            OBSERVABILITY_IMAGES="logstore.svc log-flood.app log-retention-check.app"
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
    if [ "$TEST_NAME" = "late-introduction" ]; then
        if [ ! -f "$KERNEL_DIR/isodir/boot/late-intro-check.app" ]; then
            echo "headless: build late-intro-check first (late-intro-check.app missing)" >&2
            exit 1
        fi
        debugfs -w -R "rm late-intro-check.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/late-intro-check.app late-intro-check.app" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install late-intro-check.app" >&2
            exit 1
        fi
    fi
    if [ "$TEST_NAME" = "console-route" ]; then
        if [ ! -f "$KERNEL_DIR/isodir/boot/console-route-check.app" ]; then
            echo "headless: build console-route-check first (console-route-check.app missing)" >&2
            exit 1
        fi
        debugfs -w -R "rm console-route-check.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/console-route-check.app console-route-check.app" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install console-route-check.app" >&2
            exit 1
        fi
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
    # The test console (Test_Console_ABI, ABI-002): a plan whose logstore
    # entry says (test-console approve-declared) runs logstore's test variant
    # (make logstore-test), which mirrors the console text programs publish
    # onto the serial console as "<source>: <text>". Last, so no test's own
    # copy of logstore.svc replaces it.
    # LOGSTORE_IMAGE: the logstore the disk carries (tests that pin or copy
    # it, service-switch, must start from the same program and manifest).
    LOGSTORE_IMAGE="$KERNEL_DIR/isodir/boot/logstore.svc"
    if grep -Eq '"logstore\.svc".*\(test-console approve-declared\)' "$INIT_PROFILE"; then
        LOGSTORE_IMAGE="$KERNEL_DIR/isodir/boot/logstore-test.svc"
        if [ ! -f "$KERNEL_DIR/isodir/boot/logstore-test.svc" ]; then
            echo "headless: build logstore-test first (logstore-test.svc missing)" >&2
            exit 1
        fi
        debugfs -w -R "rm logstore.svc" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/logstore-test.svc logstore.svc" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install logstore-test.svc" >&2
            exit 1
        fi
    fi
    # The same for ccl-remote (ABI-002): a plan whose ccl-remote entry
    # approves the test console runs its test variant (make ccl-remote-test),
    # which shows the pairing code there (display => Test_Console).
    if grep -Eq '"ccl-remote\.app".*\(test-console approve-declared\)' "$INIT_PROFILE"; then
        if [ ! -f "$KERNEL_DIR/isodir/boot/ccl-remote-test.app" ]; then
            echo "headless: build ccl-remote-test first (ccl-remote-test.app missing)" >&2
            exit 1
        fi
        debugfs -w -R "rm ccl-remote.app" "$TEMP_DISK" >/dev/null 2>&1
        if ! debugfs -w -R "write $KERNEL_DIR/isodir/boot/ccl-remote-test.app ccl-remote.app" \
          "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: failed to install ccl-remote-test.app" >&2
            exit 1
        fi
    fi
        DISK_IMAGE="$TEMP_DISK"
fi

rm -f "$SERIAL_LOG" "$NET_PCAP"

sed -i 's/^set default=.*/set default=4/' "$GRUB_CFG"
if { [ "$TEST_NAME" = desktop-display ] || [ "$TEST_NAME" = calculator ] || [ "$TEST_NAME" = remote-screenshot ] || [ "$TEST_NAME" = neon-blast ]; } && [ -n "${CUBIT_TEST_VIDEO_SIZE:-}" ]; then
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
          --import "cubit-signing-ca=$CUBIT_SIGNING/builder-ca.der" \
          --output "$KERNEL_DIR/isodir/boot/initrd.img" >/dev/null; then
        echo "headless: failed to prepare volatile block-storage fixture" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "device-build" ] || [ "$TEST_NAME" = "console-run-staged" ] || [ "$TEST_NAME" = "bench-build" ]; then
    # The development bootstrap set with storage.system-volume = ufs:0.
    if ! python3 "$ROOT_DIR/userspace/ccl/tools/ccl-image/realize.py" \
          "$ROOT_DIR/tests/device-builds/initrd.ccl" \
          --import "cubit-signing-ca=$CUBIT_SIGNING/builder-ca.der" \
          --output "$KERNEL_DIR/isodir/boot/initrd.img" >/dev/null; then
        echo "headless: failed to prepare the device-build initrd" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "tls-service" ]; then
    # Development initrd whose tls.hosts maps the fixture name to the host.
    if ! python3 "$ROOT_DIR/userspace/ccl/tools/ccl-image/realize.py" \
          "$ROOT_DIR/tests/tls/tls-initrd.ccl" \
          --import "cubit-signing-ca=$CUBIT_SIGNING/builder-ca.der" \
          --output "$KERNEL_DIR/isodir/boot/initrd.img" >/dev/null; then
        echo "headless: failed to prepare tls-service settings fixture" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "llm-mock" ] || [ "$TEST_NAME" = "agent-mock" ] || [ "$TEST_NAME" = "agent-app" ] ||
   [ "$TEST_NAME" = "net-fetch" ]; then
    # Development initrd whose tls.hosts maps the mock's (and the fetch
    # fixture's) name to the host.
    if ! python3 "$ROOT_DIR/userspace/ccl/tools/ccl-image/realize.py" \
          "$ROOT_DIR/tests/llm/llm-initrd.ccl" \
          --import "cubit-signing-ca=$CUBIT_SIGNING/builder-ca.der" \
          --output "$KERNEL_DIR/isodir/boot/initrd.img" >/dev/null; then
        echo "headless: failed to prepare the llm-mock settings fixture" >&2
        exit 1
    fi
fi
if [ "$CONFIG_STORAGE_TEST" = 1 ]; then
    if ! python3 "$ROOT_DIR/userspace/ccl/tools/ccl-image/realize.py" \
          "$ROOT_DIR/tests/config-turso/native/storage-initrd.ccl" \
          --import "cubit-signing-ca=$CUBIT_SIGNING/builder-ca.der" \
          --output "$KERNEL_DIR/isodir/boot/initrd.img" >/dev/null; then
        echo "headless: failed to prepare Config storage settings fixture" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "timesync" ]; then
    # Development initrd whose settings point time.servers at the fixture.
    if ! python3 "$ROOT_DIR/userspace/ccl/tools/ccl-image/realize.py" \
          "$ROOT_DIR/tests/timesync/timesync-initrd.ccl" \
          --import "cubit-signing-ca=$CUBIT_SIGNING/builder-ca.der" \
          --output "$KERNEL_DIR/isodir/boot/initrd.img" >/dev/null; then
        echo "headless: failed to prepare timesync settings fixture" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "debug-observe" ]; then
    # The Debug_Observe check (DBG-001) in procmgr's place: it holds the
    # process root and checks the kernel's debug sessions on a target of its
    # own making (kernel/src/debug_sessions.ads).
    if ! make -C "$KERNEL_DIR" debug-observe-check >/dev/null ||
       ! python3 "$ROOT_DIR/tests/spawn-rules/swap-procmgr.py" \
          "$KERNEL_DIR/isodir/boot/initrd.img" \
          "$KERNEL_DIR/isodir/boot/debug-observe-check.app" >/dev/null; then
        echo "headless: failed to put the Debug_Observe check in procmgr's place" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "vm-linux" ]; then
    # The Linux guest check (VMM-001) in procmgr's place.
    if ! make -C "$KERNEL_DIR" vm-linux-check >/dev/null ||
       ! python3 "$ROOT_DIR/tests/spawn-rules/swap-procmgr.py" \
          "$KERNEL_DIR/isodir/boot/initrd.img" \
          "$KERNEL_DIR/isodir/boot/vm-linux-check.app" >/dev/null; then
        echo "headless: failed to put the Linux guest check in procmgr's place" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "vm-hello" ]; then
    # The VM mechanism check (VMM-001) in procmgr's place: it holds the VM
    # creation authority devmgr gives procmgr.
    if ! make -C "$KERNEL_DIR" vm-hello-check >/dev/null ||
       ! python3 "$ROOT_DIR/tests/spawn-rules/swap-procmgr.py" \
          "$KERNEL_DIR/isodir/boot/initrd.img" \
          "$KERNEL_DIR/isodir/boot/vm-hello-check.app" >/dev/null; then
        echo "headless: failed to put the VM check in procmgr's place" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "spawn-rules" ]; then
    # The SPAWN rules check (CAP-012) in procmgr's place: it holds the
    # handles devmgr gives procmgr and checks the kernel's SPAWN rules
    # (kernel/src/spawn_admission.ads) with an image of its own making.
    if ! make -C "$KERNEL_DIR" spawn-rules-check >/dev/null ||
       ! python3 "$ROOT_DIR/tests/spawn-rules/swap-procmgr.py" \
          "$KERNEL_DIR/isodir/boot/initrd.img" \
          "$KERNEL_DIR/isodir/boot/spawn-rules-check.app" >/dev/null; then
        echo "headless: failed to put the SPAWN rules check in procmgr's place" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "boot-large-module" ]; then
    # Listed before init.img in the default (NVMe shell) entry, so GRUB loads
    # it first, right above the kernel, and init.img lands above it.
    truncate -s "${LARGE_MODULE_MIB}M" "$LARGE_MODULE"
    awk '/^menuentry / { entry++ }
         entry == 5 && /module \/boot\/initrd.img init.img/ {
             print "    module /boot/large-module.img large-module.img"
         }
         { print }' "$GRUB_BAK" > "$GRUB_CFG.large"
    sed 's/^set default=.*/set default=4/' "$GRUB_CFG.large" > "$GRUB_CFG"
    rm -f "$GRUB_CFG.large"
    if ! grep -q 'large-module.img' "$GRUB_CFG"; then
        echo "headless: could not add the large boot module" >&2
        exit 1
    fi
fi
if ! grub-mkrescue -o "$KERNEL_DIR/cubit_kernel.iso" "$KERNEL_DIR/isodir" >/dev/null 2>&1; then
    echo "headless: grub-mkrescue failed" >&2
    exit 1
fi
rm -f "$LARGE_MODULE"
cp "$GRUB_BAK" "$GRUB_CFG"

VIDEO_ARGS=(-device virtio-gpu-pci)
# CUBIT_TEST_VIDEO_SIZE=WxH: desktop-display at another firmware mode, the
# way a machine without modesetting runs (GOP / VBE linear framebuffer).
# The secondary virtio-gpu-pci never drives the desktop (display: "gpu not
# primary, using linear-fb"), so the size goes to GRUB's gfxpayload (set
# above) on the primary std VGA, with video memory for the mode. SeaBIOS
# VBE lists 1920x1080, 2560x1440, 2560x1600 and 3840x2160 among others.
if { [ "$TEST_NAME" = desktop-display ] || [ "$TEST_NAME" = calculator ] || [ "$TEST_NAME" = remote-screenshot ] || [ "$TEST_NAME" = neon-blast ]; } && [ -n "${CUBIT_TEST_VIDEO_SIZE:-}" ]; then
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
# GPU-014 graphics-report-nodriver: only the std VGA (no virtio-gpu-pci).
if [ "$TEST_NAME" = "graphics-report-nodriver" ]; then
    VIDEO_ARGS=()
fi
if [ "$TEST_NAME" = "virtio-vga-primary" ] ||
   [ "$TEST_NAME" = "config-tree" ] ||
   [ "$TEST_NAME" = "display-grants-virtio-vga" ] ||
   [ "$TEST_NAME" = "ccl-workspace" ] || [ "$TEST_NAME" = "ccl-console" ] ||
   [ "$TEST_NAME" = "shutdown-console" ] || [ "$TEST_NAME" = "power-sources" ] ||
   [ "$TEST_NAME" = "llm-mock" ] || [ "$TEST_NAME" = "agent-mock" ] || [ "$TEST_NAME" = "agent-app" ] || [ "$TEST_NAME" = "llm-live" ] ||
       [ "$TEST_NAME" = "net-fetch" ] ||
   [ "$TEST_NAME" = "logs" ] || [ "$TEST_NAME" = "metricsview" ] ||
   [ "$TEST_NAME" = "ccl-workbench-virtio-vga" ] ||
   [ "$TEST_NAME" = "desktop-virtio-vga" ] || [ "$TEST_NAME" = "display-latency" ] ||
   [ "$TEST_NAME" = "metrics-latency" ] ||
   [ "$TEST_NAME" = "desktop-doom" ]; then
    VIDEO_ARGS=(-vga none -device virtio-vga,xres=1024,yres=768)
fi
# DISP-015: display-latency on the std VGA linear framebuffer (display.svc
# copies each frame into the firmware scanout; no refresh grid is known)
# with DISPLAY_LATENCY_VIDEO=vga, instead of the virtio-gpu primary.
if [ "$TEST_NAME" = "display-latency" ] && [ "${DISPLAY_LATENCY_VIDEO:-virtio}" = vga ]; then
    VIDEO_ARGS=(-device virtio-gpu-pci)
fi
# UI-039 (CCL_CONSOLE_DEMO=large): a 1080p output, where a new window opens
# taller than the 1280x720 the CCL apps used to be capped at.
if [ "$TEST_NAME" = "ccl-console" ] && [ "${CCL_CONSOLE_DEMO:-}" = large ]; then
    VIDEO_ARGS=(-vga none -device virtio-vga,xres=1920,yres=1080)
    #  Desktop's 1080p output buffers do not fit the 512M desktop default.
    QEMU_MEMORY=2G
fi
# DBG-001: the Debugger's screenshots at 1080p, or with
# DEBUGGER_SHOT_SIZE=1440p or 4k.
if { [ "$TEST_NAME" = "debugger-gui" ] || [ "$TEST_NAME" = "debugger-gui-rs" ]; }; then
    if [ "${DEBUGGER_SHOT_SIZE:-1080p}" = 4k ]; then
        VIDEO_ARGS=(-vga none -device virtio-vga,xres=3840,yres=2160)
    elif [ "${DEBUGGER_SHOT_SIZE:-1080p}" = 1440p ]; then
        # Today above 1080p virtio-gpu.drv refuses the EDID mode (its
        # scanout budget) and the display falls back to 1024x768.
        VIDEO_ARGS=(-vga none -device virtio-vga,xres=2560,yres=1440)
    else
        VIDEO_ARGS=(-vga none -device virtio-vga,xres=1920,yres=1080)
    fi
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
if [ "$TEST_NAME" = "desktop-session" ]; then
    # run-desktop's devices (QEMU_DESKTOP_DEVICE, QEMU_POINTER_DEVICE).
    VIDEO_ARGS=(-vga none -device virtio-vga,xres=1024,yres=768 -device virtio-tablet-pci)
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
# Under KVM, expose the host's invariant TSC as real hardware does (QEMU
# masks it unless asked, even with -cpu host). Without it the kernel does not
# share the clock publication page, every clock read becomes a syscall, and
# per-frame and per-message syscall counts are inflated. TCG drops the flag.
# desktop-session boots run-desktop's own CPU model (QEMU_RUN_CPU), as is.
if [ "$TEST_NAME" = "desktop-session" ]; then
    QEMU_CPU_MODEL="$(make -s --no-print-directory -C "$KERNEL_DIR" print-run-cpu)" || exit 1
    if [ "$DESKTOP_SESSION_NO_INVTSC" = 1 ]; then
        QEMU_CPU_MODEL="${QEMU_CPU_MODEL//,+invtsc/}"
    fi
    echo "headless: desktop-session cpu=$QEMU_CPU_MODEL"
elif [ "$QEMU_ACCEL" = "kvm" ]; then
    case "$QEMU_CPU_MODEL" in
        *invtsc*) ;;
        *) QEMU_CPU_MODEL="$QEMU_CPU_MODEL,+invtsc" ;;
    esac
fi

AUDIO_ARGS=(-audiodev none,id=snd0)
# Devices only some tests add: QEMU's edu device binds the generic launch's
# fixture driver (HW-002); without it the driver stays unbound.
TEST_DEVICE_ARGS=()
if [ "$TEST_NAME" = "generic-launch" ] || [ "$TEST_NAME" = "platform-pool" ]; then
    TEST_DEVICE_ARGS=(-device edu)
fi
# display-latency typing through QEMU's USB keyboard (xhci.drv): the host's
# key presses go to the keyboard added last.
if [ "$TEST_NAME" = "display-latency" ] && [ "${CUBIT_LATENCY_INPUT:-ps2}" = usb ]; then
    TEST_DEVICE_ARGS=(-device qemu-xhci,id=xhcikbd -device usb-kbd,bus=xhcikbd.0,port=1)
fi
# gpio-launch's HID offers name their I2C controller by PCI location: edu
# stands in for it at 00:15.0 (the IdeaPad's touchpad controller's slot),
# and a second edu at 00:15.1 for the IdeaPad's other I2C controller, so
# the HID driver's "i2c" binding must reach the driver of the offer's
# controller, not merely one serving the role (SUP-001).
if [ "$TEST_NAME" = "gpio-launch" ]; then
    TEST_DEVICE_ARGS=(-device edu,addr=0x15.0,multifunction=on -device edu,addr=0x15.1)
fi
# QEMU's PCI test device has no CuBit driver: its 1 MiB 64-bit prefetchable
# BAR 2 (membar, the size acpi-data-check expects) is a driverless BAR whose
# tail devmgr must refuse as firmware data (sized at enumeration).
if [ "$TEST_NAME" = "acpi-firmware-data" ]; then
    TEST_DEVICE_ARGS=(-device pci-testdev,membar=1M)
fi
# ACPI-ONE-001's regression table: an extra SSDT whose module-level code
# reads a pool-backed SystemMemory register (tests/headless/acpi), in the
# edu test profile's CHIPSET_SHARED member (devmgr declares it with edu).
if [ "$TEST_NAME" = "acpi-one-service" ] || [ "$TEST_NAME" = "acpi-sci" ] || [ "$TEST_NAME" = "acpi-live-replace" ] || [ "$TEST_NAME" = "acpi-shutdown" ]; then
    TEST_DEVICE_ARGS=(-device edu -acpitable "file=$ROOT_DIR/tests/headless/acpi/acpi-one-service-pool.aml")
fi
# Opt-in ordinary consumer refusal regression on the real shutdown profile.
# Compile the supplied SSDT with the pinned IASL first; every run owns its
# temporary QEMU disk as usual. No alternate ACPI service or power stand-in.
# CMOS gate: a separately generated read-only SystemCMOS module-load field.
if [ -n "${ACPI_CMOS_REACH_TABLE:-}" ]; then
    if [ "$TEST_NAME" != "acpi-one-service" ] || [ ! -f "$ACPI_CMOS_REACH_TABLE" ]; then
        echo "headless: ACPI_CMOS_REACH_TABLE requires acpi-one-service and a compiled fixture" >&2
        exit 2
    fi
    TEST_DEVICE_ARGS+=(-acpitable "file=$ACPI_CMOS_REACH_TABLE")
fi

if [ -n "${ACPI_SHUTDOWN_REFUSAL_TABLE:-}" ]; then
    if [ "$TEST_NAME" != "acpi-shutdown" ] || [ ! -f "$ACPI_SHUTDOWN_REFUSAL_TABLE" ]; then
        echo "headless: ACPI_SHUTDOWN_REFUSAL_TABLE requires acpi-shutdown and a compiled fixture" >&2
        exit 1
    fi
    TEST_DEVICE_ARGS+=(-acpitable "file=$ACPI_SHUTDOWN_REFUSAL_TABLE")
fi
# fat32-volume: a FAT32 stick (an MBR, one type-0C partition) as a second
# NVMe disk, @nvme:1 (tests/filesystem-fat/native/check-image.py).
if [ "$TEST_NAME" = "fat32-volume" ]; then
    TEMP_SECOND_NVME="$(mktemp "$(dirname "$SERIAL_LOG")/cubit-${TEST_NAME}-nvme1.XXXXXX.img")"
    if ! python3 -I "$ROOT_DIR/tests/filesystem-fat/native/check-image.py" make "$TEMP_SECOND_NVME"; then
        echo "headless: failed to make the FAT32 stick" >&2
        exit 1
    fi
    TEST_DEVICE_ARGS=(
        -drive "file=$TEMP_SECOND_NVME,if=none,id=nvme1,format=raw"
        -device nvme,serial=cubitfat1,drive=nvme1)
fi
# usb-disk: two xHCI controllers: stick 1 an MBR FAT32 stick (@usb:0,
# check-image.py) with a live medium (an ISO 9660 stick, @cd:0) behind it on
# the same controller; stick 2 a bare FAT32 volume hot-plugged on the second
# controller once @usb:0 is mounted (so it is @usb:1), then removed during
# the run; stick 3 hot-plugged in its place (@usb:1 again: FS-034). Sticks 2
# and 3 are drives only; tests/usb-disk/drive.py plugs them.
if [ "$TEST_NAME" = "usb-disk" ]; then
    USB_STICK_DIR="$(mktemp -d "$(dirname "$SERIAL_LOG")/cubit-${TEST_NAME}-sticks.XXXXXX")"
    if ! python3 -I "$ROOT_DIR/tests/filesystem-fat/native/check-image.py" make "$USB_STICK_DIR/stick1.img"; then
        echo "headless: failed to make USB stick 1" >&2
        exit 1
    fi
    # The boot medium as a dd'ed live stick: an ISO 9660 image on a USB
    # disk, on stick 1's controller behind it (port 2): @cd:0, read only.
    mkdir -p "$USB_STICK_DIR/live/apps"
    echo "usb-disk live medium" > "$USB_STICK_DIR/live/apps/readme.txt"
    if ! xorriso -as mkisofs -quiet -V CUBITLIVE -o "$USB_STICK_DIR/live.iso" "$USB_STICK_DIR/live"; then
        echo "headless: failed to make the live medium" >&2
        exit 1
    fi
    for USB_STICK in stick2 stick3; do
        truncate -s 40M "$USB_STICK_DIR/$USB_STICK.img"
        if ! PATH="$PATH:/usr/sbin" mkfs.fat -F 32 -S 512 -s 1 -n "${USB_STICK^^}" \
            "$USB_STICK_DIR/$USB_STICK.img" >/dev/null; then
            echo "headless: failed to make USB $USB_STICK" >&2
            exit 1
        fi
    done
    TEST_DEVICE_ARGS=(
        -device qemu-xhci,id=xhcia
        -device qemu-xhci,id=xhcib
        -drive "file=$USB_STICK_DIR/stick1.img,if=none,id=stick1,format=raw"
        -device usb-storage,id=usbstick1,bus=xhcia.0,port=1,drive=stick1
        -drive "file=$USB_STICK_DIR/live.iso,if=none,id=livestick,format=raw"
        -device usb-storage,id=usblive,bus=xhcia.0,port=2,drive=livestick
        -drive "file=$USB_STICK_DIR/stick2.img,if=none,id=stick2,format=raw"
        -drive "file=$USB_STICK_DIR/stick3.img,if=none,id=stick3,format=raw"
        # SeaBIOS tries USB disks before the CD: boot the CD.
        -boot order=d)
fi
# usb-eject: one xHCI controller, empty at boot; sticks A and B (bare FAT32
# volumes) are plugged and pulled by tests/usb-eject/drive.py on the
# guest's markers (blockdev-add and device_add, device_del).
if [ "$TEST_NAME" = "usb-eject" ]; then
    USB_STICK_DIR="$(mktemp -d "$(dirname "$SERIAL_LOG")/cubit-${TEST_NAME}-sticks.XXXXXX")"
    for USB_STICK in sticka stickb; do
        truncate -s 40M "$USB_STICK_DIR/$USB_STICK.img"
        if ! PATH="$PATH:/usr/sbin" mkfs.fat -F 32 -S 512 -s 1 -n "${USB_STICK^^}" \
            "$USB_STICK_DIR/$USB_STICK.img" >/dev/null; then
            echo "headless: failed to make USB $USB_STICK" >&2
            exit 1
        fi
    done
    TEST_DEVICE_ARGS=(-device qemu-xhci,id=xhcia)
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
# usb-slots: one controller with eight USB hubs (one on the root port, seven
# on its ports 1-7) and a boot mouse on the first hub's port 8: the mouse is
# the ninth device slot (tests/usb-slots).
if [ "$TEST_NAME" = "usb-slots" ]; then
    TEST_DEVICE_ARGS=(-device qemu-xhci,id=xhcis
        -device usb-hub,bus=xhcis.0,port=1)
    for HUB_PORT in 1 2 3 4 5 6 7; do
        TEST_DEVICE_ARGS+=(-device "usb-hub,bus=xhcis.0,port=1.$HUB_PORT")
    done
    TEST_DEVICE_ARGS+=(-device usb-mouse,bus=xhcis.0,port=1.8)
fi
# xhci-bringup: the IdeaPad's shape. The first controller has nothing
# attached (00:0d.0); the second a boot mouse, a boot keyboard and a USB
# storage disk (direct access, like the live stick) holding an ISO9660
# image with an apps/ tree (tests/xhci-bringup).
if [ "$TEST_NAME" = "xhci-bringup" ]; then
    TEMP_OPTICAL_TREE="$(mktemp -d "$(dirname "$SERIAL_LOG")/cubit-${TEST_NAME}-stick.XXXXXX")"
    mkdir -p "$TEMP_OPTICAL_TREE/tree/apps"
    echo "xhci-bringup stick" > "$TEMP_OPTICAL_TREE/tree/apps/readme.txt"
    if ! xorriso -as mkisofs -quiet -V CUBITSTICK -o "$TEMP_OPTICAL_TREE/stick.iso" \
      "$TEMP_OPTICAL_TREE/tree"; then
        echo "headless: failed to create the USB stick image" >&2
        exit 1
    fi
    TEST_DEVICE_ARGS=(
        -device qemu-xhci,id=xhciempty
        -device qemu-xhci,id=xhcistick
        -device usb-mouse,bus=xhcistick.0,port=1
        -device usb-kbd,bus=xhcistick.0,port=2
        -drive "file=$TEMP_OPTICAL_TREE/stick.iso,if=none,id=usbstick,format=raw,readonly=on"
        -device usb-storage,bus=xhcistick.0,port=3,drive=usbstick)
fi
# usb-hotplug: two controllers, the boot mouse on the second; the driver
# script hot-plugs devices through the HMP monitor (tests/usb-hotplug).
if [ "$TEST_NAME" = "usb-hotplug" ]; then
    TEST_DEVICE_ARGS=(
        -device qemu-xhci,id=xhcia
        -device qemu-xhci,id=xhcib
        -device usb-mouse,bus=xhcib.0,port=1,id=bootmouse)
fi
if [ "$TEST_NAME" = "desktop-doom" ] || [ "$TEST_NAME" = "bench-audio" ] || [ "$TEST_NAME" = "neon-blast" ]; then
    # The WAV is captured in RAM and moved beside the serial log at the end.
    # QEMU writes it from its main loop, the thread that also runs the HDA
    # model's DMA timer: a write that waits on the host file system (ext4
    # commits every 5 s; measured stalls of 10-30 ms on Chungus) holds back
    # the guest's period interrupts as long, and the mixer then sees missed
    # periods that no guest scheduling can prevent. CUBIT_AUDIO_CAPTURE_DIR
    # overrides the directory.
    AUDIO_CAPTURE_DIR="${CUBIT_AUDIO_CAPTURE_DIR:-${XDG_RUNTIME_DIR:-/dev/shm}}"
    if [ ! -d "$AUDIO_CAPTURE_DIR" ] || [ ! -w "$AUDIO_CAPTURE_DIR" ]; then
        AUDIO_CAPTURE_DIR=/dev/shm
    fi
    AUDIO_KEPT_DIR="$(dirname "$SERIAL_LOG")"
    TEMP_AUDIO="$(mktemp "$AUDIO_CAPTURE_DIR/cubit-${TEST_NAME}-audio.XXXXXX.wav")"
    AUDIO_ARGS=(-audiodev "wav,id=snd0,path=$TEMP_AUDIO")
fi

MONITOR_ARGS=()
QMP_ARGS=()
if [ "$TEST_NAME" = "managed-ui" ] || [ "$TEST_NAME" = "remote-screenshot" ]; then
    MONITOR_SOCKET="$(qemu_socket monitor)"
    rm -f "$MONITOR_SOCKET"
    MONITOR_ARGS=(-monitor "unix:$MONITOR_SOCKET,server,nowait")
fi
if [ "$TEST_NAME" = "desktop-display" ] || [ "$TEST_NAME" = "files" ] || [ "$TEST_NAME" = "launch-failure" ] || [ "$TEST_NAME" = "text" ] || [ "$TEST_NAME" = "calculator" ] || [ "$TEST_NAME" = "ui-latency" ] || [ "$TEST_NAME" = "display-latency" ] || [ "$TEST_NAME" = "metrics-latency" ] || [ "$TEST_NAME" = "clipboard" ] || [ "$TEST_NAME" = "mesa-window" ] || [ "$TEST_NAME" = "gpu-viewer" ] ||
   [ "$TEST_NAME" = "desktop-protocol" ] ||
   [ "$TEST_NAME" = "ccl-workspace" ] || [ "$TEST_NAME" = "ccl-console" ] ||
   [ "$TEST_NAME" = "llm-mock" ] || [ "$TEST_NAME" = "agent-mock" ] || [ "$TEST_NAME" = "agent-app" ] || [ "$TEST_NAME" = "llm-live" ] ||
       [ "$TEST_NAME" = "net-fetch" ] ||
   [ "$TEST_NAME" = "lexicon" ] || [ "$TEST_NAME" = "dictionary" ] || [ "$TEST_NAME" = "bible" ] ||
   [ "$TEST_NAME" = "debugger-gui" ] || [ "$TEST_NAME" = "debugger-gui-rs" ] ||
   [ "$TEST_NAME" = "logs" ] || [ "$TEST_NAME" = "scanout-repair" ] || [ "$TEST_NAME" = "metricsview" ] ||
   [ "$TEST_NAME" = "desktop-doom" ] || [ "$TEST_NAME" = "boot-timeline" ] ||
   [ "$TEST_NAME" = "neon-blast" ] ||
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
               grep -F "ps2: input source rings open, entering event loop" \
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

        if [ "$TEST_NAME" = "neon-blast" ]; then
            # Play by keyboard and mouse through QEMU's PS/2 devices, take
            # screenshots, then watch the attract demo (tests/neon-blast).
            python3 -I "$ROOT_DIR/tests/neon-blast/headless_drive.py" "$MONITOR_SOCKET" \
              "$SERIAL_LOG" "${NEON_BLAST_SHOTS:-${SERIAL_LOG%.log}-neon}" "$TEMP_AUDIO"
            exit $?
        fi
        if [ "$TEST_NAME" = "boot-timeline" ]; then
            # One key as soon as the PS/2 driver serves its consumer: the
            # timeline's First_Input_Accepted is when Desktop takes it.
            printf 'sendkey shift\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            exit 0
        fi
        if [ "$TEST_NAME" = "display-latency" ] || [ "$TEST_NAME" = "metrics-latency" ]; then
            # LAT-002: presses at swept refresh phases into the focused
            # Calculator, then two of Desktop's report intervals.
            LATENCY_READY="calculator: first frame presented"
            if [ "${CUBIT_DISPLAY_LATENCY_ANIMATION:-0}" = 1 ]; then
                LATENCY_READY="frame-ticker: first frame presented"
            fi
            for ((attempt = 0; attempt < 600; attempt++)); do
                grep -aF "$LATENCY_READY" "$SERIAL_LOG" >/dev/null 2>&1 && break
                sleep 0.1
            done
            sleep 1
            python3 -I "$ROOT_DIR/tests/headless/display-latency-keys.py" "$MONITOR_SOCKET" \
              "${DISPLAY_LATENCY_PRESSES:-96}"
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
                # typed List<LogItem> crossing into the session.
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
        elif [ "$TEST_NAME" = "metricsview" ]; then
            # MET-002's app: the live dashboard, then keys into it (each a
            # Desktop input-to-ready sample for the app's own frames and
            # an input-to-rendered sample it publishes), a zoom, a range,
            # back to live; frames along the way.
            for ((attempt = 0; attempt < 300; attempt++)); do
                grep -aF "metricsview: window ready" "$SERIAL_LOG" >/dev/null && break
                sleep 0.1
            done
            {
                sleep 3
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-metricsview-start.ppm"
                for ((press = 0; press < 40; press++)); do
                    printf 'sendkey %s\n' "$( [ $((press % 2)) -eq 0 ] && echo equal || echo minus )"
                    sleep 0.15
                done
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-metricsview-live.ppm"
                printf 'sendkey 1\n'
                sleep 0.5
                for ((press = 0; press < 20; press++)); do
                    printf 'sendkey left\n'
                    sleep 0.1
                done
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-metricsview-paused.ppm"
                printf 'sendkey l\n'
                for ((press = 0; press < 30; press++)); do
                    printf 'sendkey equal\n'
                    sleep 0.12
                    printf 'sendkey minus\n'
                    sleep 0.12
                done
                sleep 3
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-metricsview-end.ppm"
                sleep 0.5
                # The editor (input_cost_edit_us): select, move and size
                # panels back and forth, then one move and one widening that
                # stay, the CCL overlay, the keys, then done (which saves).
                printf 'sendkey e\n'
                sleep 0.3
                for ((round = 0; round < 8; round++)); do
                    for key in tab right left shift-right shift-left; do
                        printf 'sendkey %s\n' "$key"
                        sleep 0.15
                    done
                done
                printf 'sendkey tab\n'
                sleep 0.3
                printf 'sendkey right\n'
                sleep 0.3
                printf 'sendkey shift-right\n'
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-metricsview-edit.ppm"
                printf 'sendkey c\n'
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-metricsview-source.ppm"
                printf 'sendkey c\n'
                sleep 0.3
                printf 'sendkey f1\n'
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-metricsview-keys.ppm"
                printf 'sendkey esc\n'
                sleep 0.3
                printf 'sendkey esc\n'
                sleep 2
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-metricsview-saved.ppm"
                # The menu bar: File by F10 (a screenshot), then rounds of
                # open, move, close (input_to_render_menu_us).
                printf 'sendkey f10\n'
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-metricsview-menu-file.ppm"
                printf 'sendkey esc\n'
                sleep 0.3
                for ((round = 0; round < 10; round++)); do
                    for key in f10 down down right down left esc; do
                        printf 'sendkey %s\n' "$key"
                        sleep 0.15
                    done
                done
                # A panel's source picker (Enter on the selected panel while
                # editing): its list, a series picked, the next view, undo
                # and redo (input_to_render_picker_us).
                printf 'sendkey e\n'
                sleep 0.3
                printf 'sendkey ret\n'
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-metricsview-picker.ppm"
                printf 'sendkey ret\n'
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-metricsview-picker-list.ppm"
                for key in down down ret tab down; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.3
                done
                for ((round = 0; round < 6; round++)); do
                    printf 'sendkey ctrl-z\n'
                    sleep 0.2
                    printf 'sendkey ctrl-y\n'
                    sleep 0.2
                done
                printf 'sendkey esc\n'
                sleep 0.3
                # A new panel (A): first free place, its picker open; a
                # series, then a number view; done (saves).
                printf 'sendkey a\n'
                sleep 0.5
                for key in ret down down down ret tab ret down down down down down down down down ret; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.2
                done
                sleep 0.5
                printf 'sendkey esc\n'
                sleep 0.3
                printf 'sendkey esc\n'
                sleep 3
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-metricsview-added.ppm"
                # Save As, a new dashboard, then Open of the copy (the same
                # dashboard), then Open of a file that does not check.
                printf 'sendkey ctrl-shift-s\n'
                sleep 1.5
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-metricsview-save-as.ppm"
                for key in h e a d l e s s minus c o p y ret; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.15
                done
                sleep 1.5
                printf 'sendkey ctrl-n\n'
                sleep 1.5
                printf 'sendkey esc\n'
                sleep 0.3
                printf 'sendkey esc\n'
                sleep 0.5
                printf 'sendkey ctrl-o\n'
                sleep 1.5
                for key in tab ctrl-a h e a d l e s s minus c o p y dot c c l ret; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.15
                done
                sleep 2
                printf 'sendkey ctrl-o\n'
                sleep 1.5
                for key in tab ctrl-a b r o k e n dot c c l ret; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.15
                done
                sleep 1.5
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-metricsview-refused.ppm"
                printf 'sendkey esc\n'
                sleep 3
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
        elif [ "$TEST_NAME" = "agent-app" ]; then
            # agent.app (AGENT-002 step 3): its masked entry takes the fake
            # key into secrets.svc, then one Diagnose mission runs against
            # the mock (one tool round, then an answer).
            agent_ready=0
            for ((attempt = 0; attempt < 300; attempt++)); do
                if grep -aF "agent-app: first frame presented" "$SERIAL_LOG" >/dev/null &&
                   grep -aF "llm: ready for" "$SERIAL_LOG" >/dev/null; then
                    agent_ready=1
                    break
                fi
                sleep 0.1
            done
            if [ "$agent_ready" -ne 1 ]; then
                echo "headless: agent.app or llm.svc did not start" >&2
                exit 1
            fi
            send_text() {
                python3 -I "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" "$1" 0.03
            }
            wait_marker() {
                local tries=0
                while [ $tries -lt "$2" ]; do
                    grep -aF "$1" "$SERIAL_LOG" >/dev/null && return 0
                    sleep 0.1
                    tries=$((tries + 1))
                done
                echo "headless: no '$1' in time" >&2
                return 1
            }
            sleep 1
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-agent-key-entry.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            # No key yet: the app opens on its masked key entry.
            send_text 'sk-ant-test-0000'
            wait_marker "secrets: stored anthropic-api-key" 100 || exit 1
            wait_marker "agent-app: API key stored in secrets.svc" 50 || exit 1
            sleep 0.5
            send_text 'agent-app-diagnose'
            wait_marker "agent-app: mission 1 started" 100 || exit 1
            wait_marker "agent-app: mission 1 ended" 900 || exit 1
            sleep 1
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-agent-app.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            touch "$LLM_MOCK_STATE/stop"
            sleep 1
        elif [ "$TEST_NAME" = "llm-mock" ] || [ "$TEST_NAME" = "agent-mock" ] || [ "$TEST_NAME" = "llm-live" ]; then
            # The console types into secrets.svc and llm.svc (AGENT-001).
            llm_ready=0
            for ((attempt = 0; attempt < 300; attempt++)); do
                if grep -aF "ccl-console: first frame presented" "$SERIAL_LOG" >/dev/null &&
                   grep -aF "llm: ready for" "$SERIAL_LOG" >/dev/null; then
                    llm_ready=1
                    break
                fi
                sleep 0.1
            done
            if [ "$llm_ready" -ne 1 ]; then
                echo "headless: the console or llm.svc did not start" >&2
                exit 1
            fi
            send_text() {
                python3 -I "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" "$1" 0.03
            }
            # Wait up to $2 tenths of a second for a serial marker.
            wait_marker() {
                local tries=0
                while [ $tries -lt "$2" ]; do
                    grep -aF "$1" "$SERIAL_LOG" >/dev/null && return 0
                    sleep 0.1
                    tries=$((tries + 1))
                done
                echo "headless: no '$1' in time" >&2
                return 1
            }
            sleep 0.5
            send_text '(secrets.status (Secret "anthropic-api-key"))'
            sleep 1
            send_text '(secrets.set (Secret "anthropic-api-key"))'
            sleep 1
            if [ "$TEST_NAME" = "llm-mock" ] || [ "$TEST_NAME" = "agent-mock" ]; then
                # The fake key, typed into the non-echoing entry.
                send_text 'sk-ant-test-0000'
            elif [ -n "${LLM_LIVE_KEY_FILE:-}" ]; then
                send_text "$(tr -d '\n' < "$LLM_LIVE_KEY_FILE")"
            else
                echo "headless: type the API key into the console window and press Enter" >&2
                wait_marker "secrets: stored anthropic-api-key" 1200 || exit 1
            fi
            wait_marker "secrets: stored anthropic-api-key" 100 || exit 1
            send_text '(secrets.status (Secret "anthropic-api-key"))'
            sleep 1
            if [ "$TEST_NAME" = "llm-live" ]; then
                send_text '(define r (llm.ask (Llm_Request prompt => "hello" model => Llm_Model.Default max_tokens => 256 deadline_ms => 60000 cost_ceiling => (Llm_Ceiling.Nano_Usd 100000000) system => "" mission => "llm-live")))'
                sleep 2
                send_text '(wait (llm.reply r))'
                wait_marker "Llm_Reply" 900
                send_text '(window 64 (llm.text r))'
                sleep 2
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-llm-live.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
                exit 0
            fi
            if [ "$TEST_NAME" = "agent-mock" ]; then
                # A file in the console's workspace for the mission to read.
                send_text '(define (marker) String "AGENT-FILE-MARKER-42")'
                sleep 1
                send_text ':save notes.ccl'
                wait_marker "workspace saved notes.ccl" 100 || exit 1
                # The mission's place is the console's workspace root.
                send_text '(fs.home)'
                wait_marker "REPL completed: Place:" 100 || exit 1
                agent_root="$(grep -a "REPL completed: Place:" "$SERIAL_LOG" | tail -1 | sed -n 's/.*Place: (Place "\([^"]*\)".*/\1/p')"
                if [ -z "$agent_root" ]; then
                    echo "headless: no workspace root for the mission" >&2
                    exit 1
                fi
                send_text '(define s (agent.ask (Agent_Request mission => Mission.Diagnose prompt => "agent-diagnose" model => Agent_Model.Profile budget => (Agent_Budget tokens => 200000 cost_nano_usd => 1000000000 wall_ms => 300000 tool_calls => 10 request_max_tokens => 4096 request_deadline_ms => 60000) places => ["'"$agent_root"'"] session => "diag")))'
                sleep 2
                send_text '(wait (agent.reply s))'
                wait_marker "Agent_Stop.Completed" 900 || exit 1
                send_text '(window 64 (agent.transcript s))'
                sleep 2
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-agent-transcript.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
                # The model asks for two calls; the mission allows one.
                send_text '(define b (agent.ask (Agent_Request mission => Mission.Diagnose prompt => "agent-budget" model => Agent_Model.Profile budget => (Agent_Budget tokens => 200000 cost_nano_usd => 1000000000 wall_ms => 300000 tool_calls => 1 request_max_tokens => 4096 request_deadline_ms => 60000) places => ["'"$agent_root"'"] session => "budget")))'
                sleep 2
                send_text '(wait (agent.reply b))'
                wait_marker "Agent_Stop.Tool_Call_Budget" 600 || exit 1
                send_text '(logs.recent "llm")'
                sleep 2
                touch "$LLM_MOCK_STATE/stop"
                sleep 1
                exit_agent_steps=1
            fi
            if [ "${exit_agent_steps:-0}" -ne 1 ]; then
            send_text '(define r (llm.ask (Llm_Request prompt => "hello" model => Llm_Model.Default max_tokens => 1024 deadline_ms => 60000 cost_ceiling => (Llm_Ceiling.Nano_Usd 100000000) system => "Be brief." mission => "llm-mock")))'
            for ((attempt = 0; attempt < 300; attempt++)); do
                [ -f "$LLM_MOCK_STATE/part1" ] && break
                sleep 0.1
            done
            sleep 1.5
            # The first part only: the mock holds the rest.
            send_text '(window 64 (llm.text r))'
            sleep 2
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-llm-partial.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            touch "$LLM_MOCK_STATE/release"
            send_text '(wait (llm.reply r))'
            wait_marker "Llm_Stop.End_Turn" 300
            send_text '(window 64 (llm.text r))'
            sleep 1.5
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-llm-answer.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            # A ceiling below what 1000 output tokens may cost: a typed stop.
            send_text '(define b (llm.ask (Llm_Request prompt => "budget" model => Llm_Model.Default max_tokens => 1000 deadline_ms => 60000 cost_ceiling => (Llm_Ceiling.Nano_Usd 1000) system => "" mission => "llm-mock")))'
            sleep 1
            send_text '(wait (llm.reply b))'
            wait_marker "Llm_Stop.Cost_Budget" 300
            # A deadline the mock outlasts.
            send_text '(define s (llm.ask (Llm_Request prompt => "slow" model => (Llm_Model.Named "claude-opus-5-5") max_tokens => 64 deadline_ms => 3000 cost_ceiling => Llm_Ceiling.Unlimited system => "" mission => "llm-mock")))'
            sleep 1
            send_text '(wait (llm.reply s))'
            wait_marker "Llm_Stop.Deadline" 300
            send_text '(logs.recent "llm")'
            sleep 2
            send_text '(logs.recent "secrets")'
            sleep 2
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-llm-logs.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            touch "$LLM_MOCK_STATE/stop"
            sleep 1
            fi
        elif [ "$TEST_NAME" = "net-fetch" ]; then
            # SELF-003 step 1: the console fetches from the HTTPS fixture
            # (tests/net-fetch/server.py) into @system/work and unpacks.
            fetch_ready=0
            for ((attempt = 0; attempt < 300; attempt++)); do
                if grep -aF "ccl-console: first frame presented" "$SERIAL_LOG" >/dev/null &&
                   [ -f "$NET_FETCH_STATE/ready" ]; then
                    fetch_ready=1
                    break
                fi
                sleep 0.1
            done
            if [ "$fetch_ready" -ne 1 ]; then
                echo "headless: the console or the fetch fixture did not start" >&2
                exit 1
            fi
            send_text() {
                python3 -I "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" "$1" 0.03
            }
            # Wait up to $3 tenths of a second for $2 lines with marker $1.
            wait_count() {
                local tries=0
                while [ $tries -lt "$3" ]; do
                    [ "$(grep -acF "$1" "$SERIAL_LOG")" -ge "$2" ] && return 0
                    sleep 0.1
                    tries=$((tries + 1))
                done
                echo "headless: fewer than $2 '$1' in time" >&2
                return 1
            }
            pkg_hash="$(python3 -I -c 'import json, sys; print(json.load(open(sys.argv[1]))["/files/pkg.tar"])' \
                "$NET_FETCH_STATE/hashes.json")"
            zero_hash="$(printf '0%.0s' $(seq 64))"
            base='https://fetch-test.cubit.internal:18496'
            limits='max_bytes => 1000000 max_redirects => 2 deadline_ms => 60000'
            sleep 0.5
            send_text '(define w (Place root => "@system" path => "work"))'
            sleep 1
            # 1. Pinned, Content-Length: stored.
            send_text "(define pkgf (net.fetch (Fetch_Request url => \"$base/files/pkg.tar\" into => w name => \"pkg.tar\" pin => (Fetch_Pin.Sha256 \"$pkg_hash\") existing => Fetch_Existing.Replace $limits)))"
            send_text '(wait (net.reply pkgf))'
            wait_count "Fetch_Outcome.Stored" 1 300 || exit 1
            # 2. Unpacked into work/pkg (the archive's own pkg/ inside it); a
            # file four directories down reads back.
            send_text '(archive.unpack (Unpack_Request from => w name => "pkg.tar" into => w directory => "pkg" format => Archive_Format.Tar specials => Archive_Specials.Refuse max_entries => 100 max_bytes => 100000))'
            wait_count "Unpack_Outcome.Unpacked" 1 300 || exit 1
            send_text '(fs.read (Excerpt child => (Child place => (Place root => "@system" path => "work/pkg/pkg/sub/deep") name => "x.txt") offset => 0 length => 100))'
            wait_count "NET-FETCH-DEEP-FILE" 1 100 || exit 1
            # 3. A pin that does not match: refused, nothing kept.
            send_text "(define badf (net.fetch (Fetch_Request url => \"$base/files/hello.txt\" into => w name => \"bad.txt\" pin => (Fetch_Pin.Sha256 \"$zero_hash\") existing => Fetch_Existing.Refuse $limits)))"
            send_text '(wait (net.reply badf))'
            wait_count "Fetch_Outcome.Digest_Mismatch" 1 300 || exit 1
            # 4. A redirect to a chunked body, unpinned.
            send_text "(define bigf (net.fetch (Fetch_Request url => \"$base/redirect/big\" into => w name => \"big.bin\" pin => Fetch_Pin.Unpinned existing => Fetch_Existing.Refuse $limits)))"
            send_text '(wait (net.reply bigf))'
            wait_count "Fetch_Outcome.Stored" 2 300 || exit 1
            # 5. A place the console may not write: refused before any
            # connection (the fixture must never see /files/never).
            send_text "(net.fetch (Fetch_Request url => \"$base/files/never\" into => (Place root => \"@system\" path => \"Debug\") name => \"x\" pin => Fetch_Pin.Unpinned existing => Fetch_Existing.Refuse $limits))"
            wait_count "outside this program's writable places" 1 100 || exit 1
            # 6. An archive that escapes: fetched, then refused by unpack.
            send_text "(define evilf (net.fetch (Fetch_Request url => \"$base/files/evil.tar\" into => w name => \"evil.tar\" pin => Fetch_Pin.Unpinned existing => Fetch_Existing.Refuse $limits)))"
            send_text '(wait (net.reply evilf))'
            wait_count "Fetch_Outcome.Stored" 3 300 || exit 1
            send_text '(archive.unpack (Unpack_Request from => w name => "evil.tar" into => w directory => "evil" format => Archive_Format.Tar specials => Archive_Specials.Refuse max_entries => 100 max_bytes => 100000))'
            wait_count "Unpack_Outcome.Refused" 1 300 || exit 1
            # 7. A redirect loop past max_redirects, and a 404.
            send_text "(define loopf (net.fetch (Fetch_Request url => \"$base/redirect/loop\" into => w name => \"loop\" pin => Fetch_Pin.Unpinned existing => Fetch_Existing.Refuse $limits)))"
            send_text '(wait (net.reply loopf))'
            wait_count "Fetch_Outcome.Too_Many_Redirects" 1 300 || exit 1
            send_text "(define missf (net.fetch (Fetch_Request url => \"$base/missing\" into => w name => \"missing\" pin => Fetch_Pin.Unpinned existing => Fetch_Existing.Refuse $limits)))"
            send_text '(wait (net.reply missf))'
            wait_count "Fetch_Outcome.Http_Status" 1 300 || exit 1
            # What the place holds now (checked after the run).
            # 8. git.clone (SELF-003 step 2): a SHA-1 repository whose
            # advertisement has the "# service=" preamble, the same one
            # through a redirect, a SHA-256 repository (when the host's git
            # makes one), a repository that is not there, and a target that
            # exists. The clones are judged on the host after the run.
            git_limits='max_refs => 100 max_objects => 10000 max_bytes => 10000000 max_redirects => 2 deadline_ms => 120000'
            send_text "(define c1 (git.clone (Clone_Request url => \"$base/git/sha1.git\" into => w directory => \"clone1\" $git_limits)))"
            send_text '(wait (git.reply c1))'
            wait_count "Git_Outcome.Cloned" 1 600 || exit 1
            send_text "(define c2 (git.clone (Clone_Request url => \"$base/git/moved.git\" into => w directory => \"clone2\" $git_limits)))"
            send_text '(wait (git.reply c2))'
            wait_count "Git_Outcome.Cloned" 2 600 || exit 1
            git_clones=2
            if python3 -I -c 'import json, sys; sys.exit(0 if "sha256.git" in json.load(open(sys.argv[1])) else 1)' \
                "$NET_FETCH_STATE/repos.json"; then
                send_text "(define c3 (git.clone (Clone_Request url => \"$base/git/sha256.git\" into => w directory => \"clone256\" $git_limits)))"
                send_text '(wait (git.reply c3))'
                wait_count "Git_Outcome.Cloned" 3 600 || exit 1
                git_clones=3
            fi
            send_text "(define c4 (git.clone (Clone_Request url => \"$base/git/missing.git\" into => w directory => \"nothing\" $git_limits)))"
            send_text '(wait (git.reply c4))'
            wait_count "Git_Outcome.Http_Status" 1 300 || exit 1
            send_text "(define c5 (git.clone (Clone_Request url => \"$base/git/sha1.git\" into => w directory => \"clone1\" $git_limits)))"
            send_text '(wait (git.reply c5))'
            wait_count "Git_Outcome.Exists" 1 300 || exit 1
            printf '%s\n' "$git_clones" > "$NET_FETCH_STATE/clones"
            # 9. BUG-006: a directory larger than one result is refused with
            # its whole count.
            send_text '(fs.list (Place root => "@system" path => "work/many"))'
            wait_count "holds $NET_FETCH_MANY_ENTRIES entries; one result holds at most" 1 300 || exit 1
            send_text '(fs.list w)'
            wait_count "REPL completed: List<File_Metadata>" 1 100 || exit 1
            sleep 1
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-net-fetch.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            touch "$NET_FETCH_STATE/stop"
            sleep 1
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
            elif [ "${CCL_CONSOLE_DEMO:-}" = debug ]; then
            # The debugger engine from the console (CCL_CONSOLE_DEMO=debug,
            # DBG-001): what an agent types. clock.svc has no stub: attach
            # through the kernel (Debug_Observe from procmgr's debug-session
            # authority), its threads, its main thread's frames and the
            # innermost frame's locals; procmgr itself is refused.
            send_text() {
                python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" "$1"
            }
            sleep 0.5
            send_text '(debug.attach (Attach_Query "clock.svc" 2000))'
            sleep 3
            send_text '(debug.threads 1)'
            sleep 2
            send_text '(debug.frames (Frame_Query 1 1 2000))'
            sleep 2
            send_text '(debug.locals (Locals_Query 1 1 0))'
            sleep 2
            send_text '(debug.attach (Attach_Query "procmgr.svc" 2000))'
            sleep 2
            send_text '(debug.detach 1)'
            sleep 2
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-debug.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.5
            elif [ "${CCL_CONSOLE_DEMO:-}" = wifi ]; then
            # What the radios heard (CCL_CONSOLE_DEMO=wifi, NET-WIFI-001 W1):
            # QEMU has no RTL8852BE, so wifi.scan answers through netstack
            # with no wireless interface (an empty list), or says why not.
            send_text() {
                python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" "$1"
            }
            sleep 0.5
            send_text '(wifi.scan)'
            sleep 2
            send_text '(length (wifi.scan))'
            sleep 2
            # The connection (W3/W4) through wifi.svc: no radio, so the
            # status says so and a join is refused; the passphrase goes
            # through the console's secret entry to wifi.svc, which stores
            # it as a Secret setting the console's config-all cannot read.
            send_text '(wifi.status)'
            sleep 2
            send_text '(wifi.join "CuBitTest")'
            sleep 2
            send_text '(wifi.passphrase "CuBitTest")'
            sleep 2
            send_text 'correct horse battery'
            sleep 2
            send_text '(config.get "wifi.secret.437542697454657374")'
            sleep 2
            # Logs by program name, not only by catalog role.
            send_text '(logs.recent "wifi")'
            sleep 2
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-wifi.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
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
            elif [ "${CCL_CONSOLE_DEMO:-}" = large ]; then
            # UI-039 (CCL_CONSOLE_DEMO=large): the window's size is checked
            # after the run; show it.
            sleep 1
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-large.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.5
            elif [ "${CCL_CONSOLE_DEMO:-}" = keys ]; then
            # UI-039 (CCL_CONSOLE_DEMO=keys): real keys, i8042 -> ps2.drv ->
            # Desktop -> the console, as on the laptop where the console
            # ended on the first key after a click (image 26). A click into
            # the window, one key alone, a pause (the console must still be
            # there), then an entry it must evaluate and show.
            printf 'mouse_button 1\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            sleep 0.1
            printf 'mouse_button 0\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            sleep 0.5
            printf 'sendkey 1\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            sleep 1.5
            printf 'sendkey backspace\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            sleep 0.5
            {
                type_keys shift-9 shift-equal spc 1 spc 2 shift-0
                printf 'sendkey ret\n'
                sleep 1
            } | nc -U "$MONITOR_SOCKET" >/dev/null 2>&1
            sleep 1
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-keys.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.5
            elif [ "${CCL_CONSOLE_DEMO:-}" = log-exited ]; then
            # LOG-003 (CCL_CONSOLE_DEMO=log-exited): once boot-logs has seen
            # log-exit-probe exit, its records by name at the console.
            for ((attempt = 0; attempt < 300; attempt++)); do
                grep -qa "boot-logs: log-exit-probe exited" "$SERIAL_LOG" && break
                sleep 0.2
            done
            sleep 0.5
            python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" \
              '(logs.recent "log-exit-probe")' 0.08
            sleep 2
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-log-exited.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.5
            elif [ "${CCL_CONSOLE_DEMO:-}" = log-burst ]; then
            # LOG-005 (CCL_CONSOLE_DEMO=log-burst): after each of the probe's
            # phases, its records by name at the console. The probe waits
            # 8 s between the phases for the first.
            for phase in 1 2; do
                for ((attempt = 0; attempt < 600; attempt++)); do
                    grep -qa "log-burst-probe: phase $phase done" "$SERIAL_LOG" && break
                    grep -qa "log-burst-probe: FAIL" "$SERIAL_LOG" && break
                    sleep 0.2
                done
                python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" \
                  '(logs.recent "log-burst-probe")' 0.08
                for ((attempt = 0; attempt < 100; attempt++)); do
                    [ "$(grep -ac "REPL completed: List<LogItem>" "$SERIAL_LOG")" -ge "$phase" ] && break
                    sleep 0.2
                done
            done
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-log-burst.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.5
            elif [ "${CCL_CONSOLE_DEMO:-}" = kernel-log ]; then
            # LOG-004 (CCL_CONSOLE_DEMO=kernel-log): once procmgr has seen
            # the three probes end and logstore has caught up with the
            # kernel log, each probe's records by its name, then the oldest
            # kernel records logstore holds (from before it started).
            for ((attempt = 0; attempt < 300; attempt++)); do
                grep -qa "procmgr: process com.cubit.crash-probe pid" "$SERIAL_LOG" &&
                  grep -qa "procmgr: process com.cubit.fault-check pid" "$SERIAL_LOG" &&
                  grep -qa "procmgr: process com.cubit.stack-probe pid" "$SERIAL_LOG" &&
                  grep -qa "logstore: kernel log caught up" "$SERIAL_LOG" && break
                sleep 0.2
            done
            # logstore drains the ring at least every 200 ms during boot.
            sleep 1.5
            for probe in crash-probe fault-check stack-probe; do
                python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" \
                  "(logs.recent \"$probe\")" 0.08
                sleep 2
            done
            python3 "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" \
              '(first 4 (logs.recent "kernel"))' 0.08
            sleep 2
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-demo-kernel-log.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 0.5
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
            INSTALL_BLOCKS=$((INSTALL_DISK_MIB * 256))
            for entry in '(system.disks)' \
                "(wait (system.install (Install_Request disk => 0 confirm => (Disk_Identity \"QEMU UFS\" \"not-this-disk\" $INSTALL_BLOCKS 4096) inode_ratio => 262144 esp_mib => 260 source => \"@nvme:0/install\")))" \
                "(wait (system.install (Install_Request disk => 0 confirm => (Disk_Identity \"QEMU UFS\" \"\" $INSTALL_BLOCKS 4096) inode_ratio => 262144 esp_mib => 260 source => \"@nvme:0/install\")))" \
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
            for ((attempt = 0; attempt < 3000; attempt++)); do
                # Both installs' tasks completed at the console: the
                # refusal, then the install.
                [ "$(grep -ac 'REPL completed: Install_Report:' "$SERIAL_LOG")" -ge 2 ] && break
                if [ -n "${CUBIT_INSTALL_INTERRUPT_AT:-}" ] &&
                   grep -qaF "$CUBIT_INSTALL_INTERRUPT_AT" "$SERIAL_LOG"; then
                    echo "headless: install interrupted at: $CUBIT_INSTALL_INTERRUPT_AT (power cut)"
                    printf 'quit\n' | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
                    break
                fi
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
                local probe routed look
                for ((probe = 0; probe < 4; probe++)); do
                    clip_keys shift
                    # Desktop's line reaches serial through logstore's mirror
                    # ("desktop.svc: desktop: keys to ..."), a little later.
                    for ((look = 0; look < 5; look++)); do
                        sleep 0.2
                        routed="$(grep -a "desktop: keys to " "$SERIAL_LOG" | tail -1 | tr -d '\r')"
                        [[ "$routed" == *"desktop: keys to $1" ]] && return 0
                    done
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
            if [ "$SCREEN_SETTLE" -eq 1 ]; then
                settle_status=0
                python3 -I "$ROOT_DIR/tests/screen-settle/settle.py" "$MONITOR_SOCKET" \
                  "$QMP_SOCKET" "${SERIAL_LOG%.log}" || settle_status=$?
                files_keys f10
                files_wait "files: closed"
                exit "$settle_status"
            fi
            # UI-051/052: held modifiers reveal their keys after the hold
            # delay (500 ms): Ctrl shows the key bar's Ctrl layer, Alt the
            # toolbar's keycaps, Super the Desktop's hints. sendkey's hold
            # time keeps the key down while the screen is captured.
            # (nc -q 1 itself lingers a second after sending.)
            # The key is pressed and released through QMP (qmp-key.py):
            # QEMU's sendkey hold time would queue another hold of delay
            # behind the release, and every later key would reach the
            # guest that late (BUG-007: each screenshot one step behind).
            files_hold_shot() {
                python3 -I "$ROOT_DIR/tests/headless/qmp-key.py" "$QMP_SOCKET" down "$1"
                sleep 2.0
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-$2.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
                sleep 2.5
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-$2-later.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
                python3 -I "$ROOT_DIR/tests/headless/qmp-key.py" "$QMP_SOCKET" up "$1"
                sleep 0.5
            }
            # The order is FILES_HOLD_ORDER's (default: Super, Alt, Ctrl).
            for files_hold in ${FILES_HOLD_ORDER:-meta_l alt ctrl}; do
                case "$files_hold" in
                    meta_l) files_hold_shot meta_l hold-super
                            files_wait "desktop: super hints shown"
                            # Released after the hold, Super opens the Apps menu.
                            files_keys esc
                            sleep 0.5 ;;
                    alt) files_hold_shot alt hold-alt
                         files_wait "files: revealed alt" ;;
                    ctrl) files_hold_shot ctrl hold-ctrl
                          files_wait "files: revealed ctrl" ;;
                esac
            done
            # A lag probe: one Down moves the cursor one row; the screen two
            # seconds later must show it (a frame left unshown would not).
            files_keys down
            sleep 2.0
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-probe-down.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            files_keys up
            sleep 0.5
            # The context menu's keycaps (Shift+F10).
            files_keys shift-f10
            sleep 0.5
            printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-menu.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            files_keys esc
            sleep 0.3
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
        elif { [ "$TEST_NAME" = "debugger-gui" ] || [ "$TEST_NAME" = "debugger-gui-rs" ]; }; then
            # DBG-001: the Debugger attached to debug-demo through the engine
            # (cooperative attach, preview). Shots: as attached; after F5
            # (run to the next checkpoint: changed values marked); with the
            # locals tree opened by keyboard. Every value is read from
            # debug-demo; the checks below match the log against its source.
            dbg_wait() {
                local attempt
                for ((attempt = 0; attempt < 600; attempt++)); do
                    grep -aF "$1" "$SERIAL_LOG" >/dev/null 2>&1 && return 0
                    sleep 0.1
                done
                echo "headless: the Debugger did not report: $1" >&2
                exit 1
            }
            dbg_keys() {
                local key
                for key in "$@"; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.2
                done | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            }
            dbg_shot() {
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-debugger-$1.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
                sleep 0.5
            }
            dbg_wait "debugger: window ready"
            dbg_wait "debugger: first full frame"
            sleep 2
            dbg_shot attached
            dbg_keys f5
            dbg_wait "debugger: continue"
            dbg_shot continued
            # Source -> Locals, open the first two values.
            dbg_keys tab right down right
            dbg_shot locals
            dbg_keys f5
            sleep 0.3
            dbg_shot changed
            # Live (L): the target runs; the views re-read it ten a second.
            dbg_keys l
            dbg_wait "debugger: live on"
            sleep 1.5
            dbg_shot live
            dbg_keys l
            dbg_wait "debugger: live off"
            # The data-structure graph focused (Tab from Locals), filling
            # the window (F) and fitted, then panned down to the sensor tree.
            dbg_keys tab f
            dbg_shot graph
            dbg_keys down down down down down
            dbg_shot graph-tree
            if [ "$TEST_NAME" = "debugger-gui" ]; then
                # Attach to process... (A): what runs, in the Threads pane;
                # "clo" picks clock.svc, which has no stub, and Enter
                # attaches through the kernel (Debug_Observe from procmgr).
                dbg_keys f a
                dbg_wait "debugger: attach to process: "
                dbg_keys c l o
                dbg_wait "debugger: pick clock.svc"
                dbg_shot pick
                dbg_keys ret
                dbg_wait "debugger: attached to clock.svc through the kernel"
                dbg_wait "debugger: shows clock pid"
                dbg_shot attach-clock
            fi
            if [ "$TEST_NAME" = "debugger-gui-rs" ]; then
                # The Rust demo's heap structures live in its threads'
                # frames. The jobs thread's run_jobs: F back to the panes,
                # Tab to the call stack, "r" (type-ahead) selects run_jobs;
                # in Locals "q" and Right open queue (Vec<Job>), "o" and
                # Right owners (HashMap), then "q" and G graph the queue.
                dbg_keys f tab tab tab r
                dbg_wait "run_jobs (selected)"
                dbg_keys tab tab q right o right q g
                dbg_shot jobs
                # The sensors thread ("s" in Threads) at run_sensors: G on
                # tree (Box nodes) and on history (Arc-shared readings),
                # then the graph full-size (Tab, F).
                dbg_keys shift-tab shift-tab shift-tab s
                dbg_wait "debugger: shows debug-demo-rs pid"
                dbg_keys tab r
                dbg_wait "run_sensors (selected)"
                dbg_keys tab tab t g h g tab f
                dbg_shot sensors
            fi
        elif [ "$TEST_NAME" = "bible" ]; then
            # The Bible (APP-005) by keyboard: it opens at John 1; a typed
            # reference opens John 3; words search; Tab is the next
            # translation, F4 the translation list; Ctrl+Page Down the next
            # chapter. Each passage shown
            # is a serial marker ("bible: showing John 3 (KJV)").
            bible_wait() {
                local attempt
                for ((attempt = 0; attempt < 300; attempt++)); do
                    grep -aF "$1" "$SERIAL_LOG" >/dev/null 2>&1 && return 0
                    sleep 0.1
                done
                echo "headless: the Bible did not report: $1" >&2
                exit 1
            }
            bible_keys() {
                local key
                for key in "$@"; do
                    printf 'sendkey %s\n' "$key"
                    sleep 0.15
                done | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            }
            bible_shot() {
                sleep 1
                printf 'screendump "%s"\n' "${SERIAL_LOG%.log}-$1.ppm" | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
                sleep 0.5
            }
            bible_wait "bible: window ready"
            bible_wait "bible: showing John 1 (KJV, Literata)"
            bible_shot bible-john-1
            bible_keys j o h n spc 3 ret
            bible_wait "bible: showing John 3 (KJV, Literata)"
            bible_shot bible-john-3-literata
            # F8: the next reading face, live (each shown once).
            bible_keys f8
            bible_wait "bible: showing John 3 (KJV, Source Serif 4)"
            bible_shot bible-john-3-source-serif-4
            bible_keys f8
            bible_wait "bible: showing John 3 (KJV, EB Garamond)"
            bible_shot bible-john-3-eb-garamond
            bible_keys f8
            bible_wait "bible: showing John 3 (KJV, Literata)"
            bible_keys f a i t h
            bible_shot bible-search-faith
            bible_keys esc tab
            bible_wait "bible: showing John 3 (BSB, Literata)"
            bible_keys ctrl-pgdn
            bible_wait "bible: showing John 4 (BSB, Literata)"
            bible_shot bible-john-4-bsb
            bible_keys p s spc 2 3 ret
            bible_wait "bible: showing Psalm 23 (BSB, Literata)"
            bible_shot bible-psalm-23-bsb
            # F4: the translation list; Down twice and Enter choose the
            # Vulgate, which numbers the psalm as the Septuagint does.
            bible_keys f4
            bible_shot bible-translations-open
            bible_keys down down ret
            bible_wait "bible: showing Psalm 22 (23) (VUL, Literata)"
            bible_shot bible-psalm-22-vulgate
            bible_keys j o h n spc 3 ret
            bible_wait "bible: showing John 3 (VUL, Literata)"
            bible_shot bible-john-3-vulgate
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
        elif [ "$TEST_NAME" = "ui-latency" ]; then
            # UI latency probe: 600 busy frames, then UI_LATENCY_KEYS key
            # presses UI_LATENCY_GAP seconds apart (each an idle wake), Esc.
            probe_wait() {
                local attempt
                for ((attempt = 0; attempt < 600; attempt++)); do
                    grep -aF "$1" "$SERIAL_LOG" >/dev/null 2>&1 && return 0
                    sleep 0.1
                done
                echo "headless: the latency probe did not report: $1" >&2
                exit 1
            }
            probe_wait "UI-PROBE: keys ready"
            sleep 1.5
            for ((probe_key = 0; probe_key < ${UI_LATENCY_KEYS:-40}; probe_key++)); do
                printf 'sendkey a\n' | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
                sleep "${UI_LATENCY_GAP:-0.15}"
            done
            sleep 1.2
            printf 'sendkey esc\n' | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            probe_wait "UI-PROBE: keys keys="
            probe_wait "UI-PROBE: closed"
            sleep 1.5
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

if [ "$TEST_NAME" = "nmi" ]; then
    # Inject NMIs through the HMP monitor once nmi-check is spinning.
    MONITOR_SOCKET="$(qemu_socket monitor)"
    rm -f "$MONITOR_SOCKET"
    MONITOR_ARGS=(-monitor "unix:$MONITOR_SOCKET,server,nowait")
    NMI_INJECTIONS=8
    (
        deadline=$((SECONDS + TIMEOUT_SECONDS))
        until grep -aqF 'nmi-check: spinning' "$SERIAL_LOG" 2>/dev/null; do
            [ "$SECONDS" -ge "$deadline" ] && exit 0
            sleep 0.2
        done
        for _ in $(seq "$NMI_INJECTIONS"); do
            if printf 'nmi\n' | nc -N -U "$MONITOR_SOCKET" >>"${SERIAL_LOG%.log}-nmi-inject.log" 2>&1; then
                echo "headless: nmi injected" >>"${SERIAL_LOG%.log}-nmi-inject.log"
            else
                echo "headless: nmi injection failed" >>"${SERIAL_LOG%.log}-nmi-inject.log"
            fi
            sleep 0.3
        done
    ) &
    INPUT_INJECTOR_PID=$!
fi

if [ "$TEST_NAME" = "nvme-timeout" ] || [ "$TEST_NAME" = "nvme-stuck" ]; then
    # BUG-003: once nvme.drv serves, QMP throttles the drive's reads so one
    # outlives the driver's 30 s deadline (tests/nvme-timeout/fixture.py).
    QMP_SOCKET="$(qemu_socket qmp)"
    rm -f "$QMP_SOCKET"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    NVME_FIXTURE_RESULT="${SERIAL_LOG%.log}-nvme-fixture.txt"
    python3 -I "$ROOT_DIR/tests/nvme-timeout/fixture.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" \
        "$( [ "$TEST_NAME" = "nvme-stuck" ] && echo stuck || echo recover )" \
        "$NVME_FIXTURE_RESULT" &
    INPUT_INJECTOR_PID=$!
fi

if [ "$TEST_NAME" = "usb-disk" ]; then
    # Unplug and hot-plug on the guest's markers, then quit (drive.py).
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    USB_DISK_DRIVE_RESULT="$USB_STICK_DIR/drive.result"
    python3 "$ROOT_DIR/tests/usb-disk/drive.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "$USB_DISK_DRIVE_RESULT" &
    INPUT_INJECTOR_PID=$!
fi

if [ "$TEST_NAME" = "desktop-session" ] && [ "$DESKTOP_SESSION_LAUNCH" = 1 ]; then
    # Launch and close apps through the Desktop, then quit.
    QMP_SOCKET="$(qemu_socket qmp)"
    rm -f "$QMP_SOCKET"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    DESKTOP_LAUNCH_RESULT="${SERIAL_LOG%.log}-desktop-launch.txt"
    rm -f "$DESKTOP_LAUNCH_RESULT"
    python3 -I "$ROOT_DIR/tests/headless/desktop-launch.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "$DESKTOP_LAUNCH_RESULT" &
    INPUT_INJECTOR_PID=$!
fi

if [ "$TEST_NAME" = "usb-eject" ]; then
    # Plug and pull the sticks on the guest's markers, then quit (drive.py).
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    USB_EJECT_DRIVE_RESULT="$USB_STICK_DIR/drive.result"
    python3 "$ROOT_DIR/tests/usb-eject/drive.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "$USB_EJECT_DRIVE_RESULT" "$USB_STICK_DIR" &
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

# acpi-hardware and acpi-sci: QEMU's system_powerdown raises PWRBTN_STS,
# which q35 signals on the SCI (GSI 9, level). One press per trigger line,
# each sent once that line is on the serial log, in order:
# * acpi-hardware: the stand-in enables the power button event and says
#   "SCI armed" (first press), then, with the line held masked, "SCI second
#   press wanted" (the press that must not be lost before the acknowledgement);
# * acpi-sci: the one acpi.svc says "SCI armed"; the second press follows its
#   log of the first.
if [ "$TEST_NAME" = "acpi-hardware" ] || [ "$TEST_NAME" = "acpi-sci" ]; then
    if ! command -v nc >/dev/null 2>&1; then
        echo "headless: the SCI injection requires nc" >&2
        exit 127
    fi
    if [ "$TEST_NAME" = "acpi-hardware" ]; then
        SCI_TRIGGERS=("acpi-hardware-check: SCI armed" "acpi-hardware-check: SCI second press wanted")
    else
        SCI_TRIGGERS=("acpi: SCI armed" "acpi: SCI 1 fixed=[PWRBTN ]")
    fi
    MONITOR_SOCKET="$(qemu_socket monitor)"
    rm -f "$MONITOR_SOCKET"
    MONITOR_ARGS=(-monitor "unix:$MONITOR_SOCKET,server,nowait")
    (
        for trigger in "${SCI_TRIGGERS[@]}"; do
            sent=0
            for ((attempt = 0; attempt < TIMEOUT_SECONDS * 10; attempt++)); do
                if [ -S "$MONITOR_SOCKET" ] &&
                   grep -aqF "$trigger" "$SERIAL_LOG" 2>/dev/null; then
                    printf 'system_powerdown\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
                    sent=1
                    break
                fi
                sleep 0.1
            done
            if [ "$sent" -ne 1 ]; then
                echo "headless: SCI injector timed out waiting for: $trigger" >&2
                exit 1
            fi
        done
        exit 0
    ) &
    INPUT_INJECTOR_PID=$!
fi

# GPU-014 graphics-report-*: once the console has a frame, (graphics.report)
# is typed at it; the listing's line is checked with the markers.
if [ "$TEST_NAME" = "graphics-report-nodriver" ] || [ "$TEST_NAME" = "graphics-report-failing" ]; then
    if ! command -v nc >/dev/null 2>&1; then
        echo "headless: $TEST_NAME requires nc" >&2
        exit 127
    fi
    MONITOR_SOCKET="$(qemu_socket monitor)"
    rm -f "$MONITOR_SOCKET"
    MONITOR_ARGS=(-monitor "unix:$MONITOR_SOCKET,server,nowait")
    (
        for ((attempt = 0; attempt < TIMEOUT_SECONDS * 10; attempt++)); do
            if grep -aqF "ccl-console: first frame presented" "$SERIAL_LOG" 2>/dev/null &&
               grep -aqF "desktop: renderer report output-0" "$SERIAL_LOG" 2>/dev/null &&
               [ -S "$MONITOR_SOCKET" ]; then
                break
            fi
            sleep 0.1
        done
        sleep 1
        python3 -I "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" '(graphics.report)' 0.05
        for ((attempt = 0; attempt < 300; attempt++)); do
            grep -aqF "ccl-console: REPL completed: List<Gfx_Entry>:" "$SERIAL_LOG" 2>/dev/null && break
            sleep 0.1
        done
        sleep 1
        printf 'quit\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
        exit 0
    ) &
    INPUT_INJECTOR_PID=$!
fi

# PWR-004 power-sources: nothing is typed until Desktop has shown the
# discharging battery AND then the charging one: both reach it by acpi.svc's
# watch alone (the stand-in's first publication, then its Notify), with no
# read asked for. Then, once the battery's sample is older than power.svc's
# reading_age_ms, (power.sources) is typed at the console: power.svc asks
# the stand-in for a demand read, so the console lists the freshly sampled
# battery, and Desktop's kept watch is answered with it. Then QEMU is quit.
if [ "$TEST_NAME" = "power-sources" ]; then
    if ! command -v nc >/dev/null 2>&1; then
        echo "headless: power-sources requires nc" >&2
        exit 127
    fi
    MONITOR_SOCKET="$(qemu_socket monitor)"
    rm -f "$MONITOR_SOCKET"
    MONITOR_ARGS=(-monitor "unix:$MONITOR_SOCKET,server,nowait")
    POWER_READY=("ccl-console: first frame presented" "power: ready"
                 "desktop: status battery shown: Battery 73%, discharging, 3 h 39 min left"
                 "desktop: status battery shown: Battery 74%, charging, 1 h 2 min to full [bolt]")
    POWER_LISTED=("ccl-console: REPL completed: List<Power_Source>:"
                  "power-standin: demand read 1: the battery sampled again")
    (
        wait_for() {
            local ready=0 attempt marker
            for ((attempt = 0; attempt < TIMEOUT_SECONDS * 10; attempt++)); do
                ready=1
                for marker in "$@"; do
                    if ! grep -aqF "$marker" "$SERIAL_LOG" 2>/dev/null; then
                        ready=0
                    fi
                done
                if [ "$ready" -eq 1 ] && [ -S "$MONITOR_SOCKET" ]; then
                    return 0
                fi
                sleep 0.1
            done
            return 1
        }
        if ! wait_for "${POWER_READY[@]}"; then
            echo "headless: power-sources: the battery item and the console did not come up" >&2
            exit 1
        fi
        # Older than reading_age_ms (2 s, tests/power-sources/init.ccl).
        sleep 3
        python3 -I "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" '(power.sources)' 0.05
        wait_for "${POWER_LISTED[@]}" || true
        # The battery alone: the whole listing is longer than one log
        # record (512 bytes), this one starts its own.
        python3 -I "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" '(at (power.sources) 2)' 0.05
        wait_for "ccl-console: REPL completed: Power_Source:" || true
        sleep 1
        printf 'quit\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
        exit 0
    ) &
    INPUT_INJECTOR_PID=$!
fi

# PWR-003 shutdown tests: one request once everything runs and power.svc
# watches. shutdown: QEMU's power button (system_powerdown) once the
# stand-in kept power.svc's watch and both fixtures run. shutdown-console:
# (power-off) typed at the console once it has a frame and power.svc is
# ready. Then QEMU must power off by itself.
if [ "$TEST_NAME" = "acpi-shutdown" ] || [ "$TEST_NAME" = "shutdown" ] || [ "$TEST_NAME" = "shutdown-console" ] || [ "$TEST_NAME" = "shutdown-text" ] ||
   [ "$TEST_NAME" = "shutdown-desktop" ] || [ "$TEST_NAME" = "shutdown-desktop-cancel" ]; then
    if ! command -v nc >/dev/null 2>&1; then
        echo "headless: the shutdown request requires nc" >&2
        exit 127
    fi
    MONITOR_SOCKET="$(qemu_socket monitor)"
    rm -f "$MONITOR_SOCKET"
    MONITOR_ARGS=(-monitor "unix:$MONITOR_SOCKET,server,nowait")
    if [ "$TEST_NAME" = "acpi-shutdown" ]; then
        SHUTDOWN_READY=("acpi: stage runtime-selection ok one-service mode=LOCAL_RUNTIME" "acpi: SCI armed" "power: ready" "shutdown-cooperative: running" "shutdown-stubborn: running")
        if [ -n "${ACPI_SHUTDOWN_REFUSAL_TABLE:-}" ]; then
            SHUTDOWN_READY+=("acpi: sample refusal family=BATTERY isolated=TRUE" "acpi: battery guard=" "acpi: AC ONLINE" "acpi: lid OPEN")
        fi
    elif [ "$TEST_NAME" = "shutdown" ]; then
        SHUTDOWN_READY=("shutdown-standin: watch kept" "shutdown-cooperative: running" "shutdown-stubborn: running")
    elif [ "$TEST_NAME" = "shutdown-text" ]; then
        SHUTDOWN_READY=("shutdown-standin: watch kept" "text: status Opened @nvme:0/Documents/notes.txt")
    elif [ "$TEST_NAME" = "shutdown-desktop" ] || [ "$TEST_NAME" = "shutdown-desktop-cancel" ]; then
        SHUTDOWN_READY=("power: ready" "desktop: launch menu entries=" "shutdown-stubborn: running")
    else
        SHUTDOWN_READY=("ccl-console: first frame presented" "power: ready" "shutdown-stubborn: running")
    fi
    (
        ready=0
        for ((attempt = 0; attempt < TIMEOUT_SECONDS * 10; attempt++)); do
            ready=1
            for marker in "${SHUTDOWN_READY[@]}"; do
                if ! grep -aqF "$marker" "$SERIAL_LOG" 2>/dev/null; then
                    ready=0
                fi
            done
            if [ "$ready" -eq 1 ] && [ -S "$MONITOR_SOCKET" ]; then
                break
            fi
            sleep 0.1
        done
        if [ "$ready" -ne 1 ]; then
            echo "headless: $TEST_NAME: the system did not come up for the request" >&2
            exit 1
        fi
        sleep 1
        if [ "$TEST_NAME" = "shutdown-text" ]; then
            # Unsaved edits: a line typed at the end, not saved.
            for key in ctrl-end ret u n s a v e d spc w o r k; do
                printf 'sendkey %s\n' "$key"
                sleep 0.15
            done | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            sleep 1
        fi
        if [ "$TEST_NAME" = "shutdown-desktop" ] || [ "$TEST_NAME" = "shutdown-desktop-cancel" ]; then
            # Super opens the Apps menu; Up wraps to Shut Down...; Enter
            # asks; Enter confirms, Escape cancels.
            for key in meta_l up ret "$([ "$TEST_NAME" = "shutdown-desktop" ] && echo ret || echo esc)"; do
                printf 'sendkey %s\n' "$key"
                sleep 0.6
            done | nc -U -q 1 "$MONITOR_SOCKET" >/dev/null
            if [ "$TEST_NAME" = "shutdown-desktop-cancel" ]; then
                # Nothing may follow: give it time, then end the run.
                sleep 4
                printf 'quit\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
            fi
        elif [ "$TEST_NAME" = "acpi-shutdown" ] || [ "$TEST_NAME" = "shutdown" ] || [ "$TEST_NAME" = "shutdown-text" ]; then
            printf 'system_powerdown\n' | nc -N -U "$MONITOR_SOCKET" >/dev/null 2>&1
        else
            python3 -I "$ROOT_DIR/tests/ccl-console/send-text.py" "$MONITOR_SOCKET" '(power-off)' 0.05
        fi
        exit 0
    ) &
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

if [ "$TEST_NAME" = "ladybird-test262" ]; then
    # Quit QEMU once the test262 launcher reports (the timeout is only the bound).
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "LADYBIRD-TEST262: DONE" "LADYBIRD-TEST262: FAIL" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "ladybird-js" ]; then
    # Quit QEMU once ladybird-js-check reports (the timeout is only the bound).
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "LADYBIRD-JS-CHECK: PASS" "LADYBIRD-JS-CHECK: FAIL" &
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
if [ "$TEST_NAME" = "ccl-build-native" ]; then
    # Quit QEMU once ccl-build-check reports (the timeout is only the bound).
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "CCL-BUILD-NATIVE: PASS" "CCL-BUILD-NATIVE: FAIL" &
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
    bench-fs|bench-cualloc|kern014-bench|fs-coherence-*|fat32-volume)
        # Quit at the program's last line, not the timeout.
        QMP_SOCKET="$(qemu_socket qmp)"
        QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
        if [ "$TEST_NAME" = "bench-fs" ]; then
            FS_TEST_ENDS=("fs-bench: done" "fs-bench: FAIL")
        elif [ "$TEST_NAME" = "bench-cualloc" ]; then
            FS_TEST_ENDS=("cualloc-bench: done" "cualloc-bench: FAIL")
        elif [ "$TEST_NAME" = "kern014-bench" ]; then
            FS_TEST_ENDS=("kern014: done")
        elif [ "$TEST_NAME" = "fat32-volume" ]; then
            FS_TEST_ENDS=("fat-volume: end")
        else
            FS_TEST_ENDS=("fs-coherence: end")
        fi
        python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
            "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "${FS_TEST_ENDS[@]}" &
        INPUT_INJECTOR_PID=$!
        ;;
esac
if [ "$TEST_NAME" = "vm-desktop" ]; then
    # Screenshots once the guest's prompt is on the serial log, then quit.
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/vm-desktop-shots.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "${CUBIT_VM_SHOTS:-/home/doc/cubit-build-tmp/vmm/shots}" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "vm-linux" ]; then
    # Quit at the check's last line (the timeout is only the bound).
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "vm-linux: done PASS" "vm-linux: done FAIL" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "vm-hello" ]; then
    # Quit at the check's last line (the timeout is only the bound).
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "vm-hello: done PASS" "vm-hello: done FAIL" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "debug-observe" ]; then
    # Quit at the check's last line (the timeout is only the bound).
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "debug-observe: done PASS" "debug-observe: done FAIL" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "mailbox-self" ]; then
    # Quit at the check's last line (the timeout is only the bound).
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "MAILBOX-SELF: done PASS" "MAILBOX-SELF: done FAIL" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "spawn-rules" ]; then
    # Quit at the check's last line (the timeout is only the bound).
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "spawn-rules: done PASS" "spawn-rules: done FAIL" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "xhci-bringup" ]; then
    # Quit once boot-logs (after procmgr, after every controller's bring-up)
    # is up; check.py reads the rest.
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "boot-logs: window ready" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "usb-slots" ]; then
    # Quit once devmgr has every controller's reply (the timeout is only
    # the bound); check.py reads the rest.
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/headless/stop-on-markers.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "devmgr: startup complete, entering service loop" &
    INPUT_INJECTOR_PID=$!
fi
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
if [ "$TEST_NAME" = "ccl-remote-session" ]; then
    # A LAN host's view: the guest's port 8445 (its startup entry's
    # Remote_Listen.LAN) forwarded from the host's loopback. The driver
    # pairs with the code the guest shows on its console, runs the
    # session and quits QEMU; its status is the test's.
    REMOTE_SESSION_PORT="${REMOTE_SESSION_PORT:-18447}"
    NETDEV_CONFIG="user,id=net0,hostfwd=tcp:127.0.0.1:$REMOTE_SESSION_PORT-10.0.2.15:8445"
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/ccl-remote/remote_session.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "$REMOTE_SESSION_PORT" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "remote-screenshot" ]; then
    # The same LAN host's view as ccl-remote-session, on a desktop: the
    # driver pairs, captures (the person's switch turned on by keyboard
    # over the HMP monitor), checks the PNGs and quits QEMU.
    # REMOTE_SCREENSHOT_OUT: where the PNGs are kept (default: a temp dir).
    REMOTE_SESSION_PORT="${REMOTE_SESSION_PORT:-18448}"
    NETDEV_CONFIG="user,id=net0,hostfwd=tcp:127.0.0.1:$REMOTE_SESSION_PORT-10.0.2.15:8445"
    python3 "$ROOT_DIR/tests/ccl-remote/remote_screenshot.py" \
        "$SERIAL_LOG" "$MONITOR_SOCKET" "$TIMEOUT_SECONDS" "$REMOTE_SESSION_PORT" \
        ${REMOTE_SCREENSHOT_OUT:+"$REMOTE_SCREENSHOT_OUT"} &
    INPUT_INJECTOR_PID=$!
fi
METRICS_DRIVER_PID=""
if [ "$TEST_NAME" = "metrics-latency" ]; then
    # MET-002: the guest's port 8445 from the host's loopback, as
    # ccl-remote-session; the driver pairs, waits for the presses, reads the
    # series back through CCL, checks them against Desktop's report lines
    # and quits QEMU.
    REMOTE_SESSION_PORT="${REMOTE_SESSION_PORT:-18453}"
    NETDEV_CONFIG="user,id=net0,hostfwd=tcp:127.0.0.1:$REMOTE_SESSION_PORT-10.0.2.15:8445"
    python3 "$ROOT_DIR/tests/metrics-latency/metrics_session.py" \
        "$SERIAL_LOG" "$MONITOR_SOCKET" "$TIMEOUT_SECONDS" "$REMOTE_SESSION_PORT" \
        "${DISPLAY_LATENCY_PRESSES:-96}" &
    METRICS_DRIVER_PID=$!
fi
if [ "$TEST_NAME" = "service-switch" ]; then
    # As device-build: the guest's port 8445 from the host's loopback; the
    # driver pairs, turns the Desktop's switch on by a click over QMP, and
    # switches lexicon.svc and clock.svc through system generations.
    SERVICE_SWITCH_PORT="${SERVICE_SWITCH_PORT:-18449}"
    NETDEV_CONFIG="user,id=net0,hostfwd=tcp:127.0.0.1:$SERVICE_SWITCH_PORT-10.0.2.15:8445"
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/service-switch/service_switch.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "$SERVICE_SWITCH_PORT" \
        "$KERNEL_DIR/isodir/boot/lexicon.svc" "$KERNEL_DIR/isodir/boot/clock.svc" \
        "${LOGSTORE_IMAGE:-$KERNEL_DIR/isodir/boot/logstore.svc}" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "acpi-live-replace" ]; then
    # As service-switch: the guest's port 8445 from the host's loopback.
    ACPI_LIVE_REPLACE_PORT="${ACPI_LIVE_REPLACE_PORT:-18451}"
    NETDEV_CONFIG="user,id=net0,hostfwd=tcp:127.0.0.1:$ACPI_LIVE_REPLACE_PORT-10.0.2.15:8445"
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/acpi-live-replace/acpi_live_replace.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "$ACPI_LIVE_REPLACE_PORT" \
        "$ROOT_DIR/userspace/services/acpi/native/build/acpi.svc" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "desktop-live-replace" ]; then
    # As service-switch: the guest's port 8445 from the host's loopback.
    DESKTOP_LIVE_REPLACE_PORT="${DESKTOP_LIVE_REPLACE_PORT:-18452}"
    NETDEV_CONFIG="user,id=net0,hostfwd=tcp:127.0.0.1:$DESKTOP_LIVE_REPLACE_PORT-10.0.2.15:8445"
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/desktop-live-replace/desktop_live_replace.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "$DESKTOP_LIVE_REPLACE_PORT" \
        "$KERNEL_DIR/isodir/boot/desktop.svc" "$KERNEL_DIR/isodir/boot/timesync.svc" \
        "$KERNEL_DIR/isodir/boot" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "service-switch-reboot" ]; then
    # The next boot of the kept disk: checked from the serial log only
    # (SERVICE_SWITCH_STATE names what the first boot made current).
    SERVICE_SWITCH_PORT="${SERVICE_SWITCH_PORT:-18449}"
    NETDEV_CONFIG="user,id=net0,hostfwd=tcp:127.0.0.1:$SERVICE_SWITCH_PORT-10.0.2.15:8445"
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/service-switch/service_switch.py" --after-reboot \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "${LOGSTORE_IMAGE:-$KERNEL_DIR/isodir/boot/logstore.svc}" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "device-build" ]; then
    # As ccl-remote-session: the guest's port 8445 from the host's loopback;
    # the driver pairs, builds on CuBit, swaps the running app for its new
    # build (the Desktop's switch turned on by a pointer click injected
    # over QMP's human monitor) and quits QEMU.
    DEVICE_BUILD_PORT="${DEVICE_BUILD_PORT:-18448}"
    NETDEV_CONFIG="user,id=net0,hostfwd=tcp:127.0.0.1:$DEVICE_BUILD_PORT-10.0.2.15:8445"
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/device-builds/device_build.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "$DEVICE_BUILD_PORT" \
        "$ROOT_DIR/tests/device-builds/build" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "console-run-staged" ]; then
    # As device-build: the guest's port 8445 from the host's loopback; the
    # driver pairs, builds hello on CuBit, types into the CCL console over
    # QMP (the Desktop's switch turned on by a click) and quits QEMU.
    CONSOLE_RUN_PORT="${CONSOLE_RUN_PORT:-18450}"
    NETDEV_CONFIG="user,id=net0,hostfwd=tcp:127.0.0.1:$CONSOLE_RUN_PORT-10.0.2.15:8445"
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/console-run-staged/console_run.py" \
        "$SERIAL_LOG" "$QMP_SOCKET" "$TIMEOUT_SECONDS" "$CONSOLE_RUN_PORT" \
        "$ROOT_DIR/tests/device-builds/build" &
    INPUT_INJECTOR_PID=$!
fi
if [ "$TEST_NAME" = "bench-build" ]; then
    # PERF-010: pairs as device-build does, builds hello and
    # secondary-stack-check cold, warm and edited, prints the breakdown
    # against the Linux reference (bench_linux.py, above) and quits QEMU.
    # BENCH_BUILD_OUT: the results (default tests/device-builds/build/bench-cubit.json).
    DEVICE_BUILD_PORT="${DEVICE_BUILD_PORT:-18448}"
    NETDEV_CONFIG="user,id=net0,hostfwd=tcp:127.0.0.1:$DEVICE_BUILD_PORT-10.0.2.15:8445"
    QMP_SOCKET="$(qemu_socket qmp)"
    QMP_ARGS=(-qmp "unix:$QMP_SOCKET,server=on,wait=off")
    python3 "$ROOT_DIR/tests/device-builds/bench_build.py" \
        --serial "$SERIAL_LOG" --qmp "$QMP_SOCKET" --timeout "$TIMEOUT_SECONDS" \
        --port "$DEVICE_BUILD_PORT" \
        --linux "$ROOT_DIR/tests/device-builds/build/bench-linux.json" \
        ${BENCH_BUILD_OUT:+--out "$BENCH_BUILD_OUT"} \
        "$ROOT_DIR/tests/device-builds/build" &
    INPUT_INJECTOR_PID=$!
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
if [ "$TEST_NAME" = "llm-mock" ] || [ "$TEST_NAME" = "agent-mock" ] || [ "$TEST_NAME" = "agent-app" ]; then
    python3 -I "$ROOT_DIR/tests/llm/mock_server.py" "$LLM_MOCK_STATE" "$TIMEOUT_SECONDS" \
        "$(case "$TEST_NAME" in agent-mock) echo agent ;; agent-app) echo agent-app ;; *) echo llm ;; esac)" \
        > "${SERIAL_LOG}.llm-mock.log" 2>&1 &
    NETWORK_PEER_PID=$!
fi
if [ "$TEST_NAME" = "net-fetch" ]; then
    python3 -I "$ROOT_DIR/tests/net-fetch/server.py" "$NET_FETCH_STATE" "$TIMEOUT_SECONDS" \
        > "${SERIAL_LOG}.net-fetch.log" 2>&1 &
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

# IOMMU-001: QEMU's VT-d model. Interrupt remapping is offered (intremap,
# which needs the split irqchip) but CuBit does not enable it yet; virtio
# devices reach memory through the IOMMU only as modern devices that
# negotiate VIRTIO_F_ACCESS_PLATFORM.
MACHINE_ARGS=(-machine q35)
if [ "$IOMMU" = 1 ]; then
    MACHINE_ARGS=(-machine q35,kernel-irqchip=split
                  -device "intel-iommu,intremap=on,aw-bits=${CUBIT_IOMMU_AW_BITS:-39}")
    NET_DEVICE_OPTIONS="${NET_DEVICE_OPTIONS:-},disable-legacy=on,iommu_platform=on"
    # The fault probe's DMA round trip needs its block below edu's 28-bit
    # DMA mask (above it edu clamps addresses), so all of RAM below 256 MiB.
    if [ "$TEST_NAME" = "generic-launch" ]; then
        QEMU_MEMORY=256M
    fi
fi

# The gpio-launch profile's firmware identity: QEMU writes it into every
# ACPI table's header, so devmgr's quirk table (Platform_Quirks.
# QEMU_Q35_Test_GPIO_Bootstrap) knows this q35 + edu run from platform-pool's.
if [ "$TEST_NAME" = "gpio-launch" ]; then
    MACHINE_ARGS[1]="${MACHINE_ARGS[1]},x-oem-id=CBTEST,x-oem-table-id=GPIOBOOT"
fi

# The audio tests' serial log comes through a FIFO and a relay
# (tests/headless/serial_relay.py), so QEMU never waits on the host file
# system while it holds the lock its main loop and the HDA model's DMA
# timer need; every other test writes the log directly. The FIFO lives in
# RAM beside the WAV: even a FIFO's write updates its inode's times, which
# on ext4 waits out a journal commit as a file write does.
SERIAL_ARGS=(-serial "file:$SERIAL_LOG")
SERIAL_RELAY_PID=""
SERIAL_FIFO_DIR=""
if [ -n "$TEMP_AUDIO" ]; then
    SERIAL_FIFO_DIR="$(mktemp -d "$AUDIO_CAPTURE_DIR/cubit-serial-fifo.XXXXXX")"
    mkfifo "$SERIAL_FIFO_DIR/serial.in" "$SERIAL_FIFO_DIR/serial.out"
    python3 -I "$ROOT_DIR/tests/headless/serial_relay.py" \
        "$SERIAL_FIFO_DIR/serial.out" "$SERIAL_LOG" &
    SERIAL_RELAY_PID=$!
    SERIAL_ARGS=(-serial "pipe:$SERIAL_FIFO_DIR/serial")
fi
# The benchmark runs without a packet capture, as its Linux reference does.
PCAP_ARGS=(-object "filter-dump,id=f0,netdev=net0,file=$NET_PCAP")
if [ "$TEST_NAME" = "bench-net" ] && [ -z "${BENCH_NET_PCAP:-}" ]; then PCAP_ARGS=(); fi   # BENCH_NET_PCAP=1 keeps the capture
(
    cd "$KERNEL_DIR" || exit 1
    # shellcheck disable=SC2086
    "$TIMEOUT_BIN" "$TIMEOUT_SECONDS" "${PIN_ARGS[@]}" "$QEMU_BIN" \
        "${ACCEL_ARGS[@]}" \
        "${MACHINE_ARGS[@]}" \
        -cpu "$QEMU_CPU_MODEL" \
        -smp "$QEMU_CPUS" \
        -m "${QEMU_MEMORY:-256M}" \
        -cdrom cubit_kernel.iso \
        "${SERIAL_ARGS[@]}" \
        -display "$QEMU_DISPLAY" \
        "${MONITOR_ARGS[@]}" \
        "${QMP_ARGS[@]}" \
        -drive "file=$DISK_IMAGE,if=none,id=nvme0,format=raw${NVME_DRIVE_OPTIONS:-}" \
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
if [ -n "$SERIAL_RELAY_PID" ]; then
    # QEMU has closed its end. A relay still waiting for QEMU to open the
    # FIFO (QEMU failed to start) is released by an empty writer.
    if kill -0 "$SERIAL_RELAY_PID" 2>/dev/null; then
        timeout 2 bash -c ': > "$1"' _ "$SERIAL_FIFO_DIR/serial.out" 2>/dev/null
    fi
    wait "$SERIAL_RELAY_PID"
    SERIAL_RELAY_PID=""
    rm -rf "$SERIAL_FIFO_DIR"
fi

if [ -n "$CLOCK_OBSERVER_PID" ]; then
    if ! wait "$CLOCK_OBSERVER_PID"; then
        CLOCK_OBSERVER_PID=""
        echo "headless: guest/host clock check failed: ${SERIAL_LOG}.host-clock.json" >&2
        exit 1
    fi
    CLOCK_OBSERVER_PID=""
    echo "headless: host clock reference: ${SERIAL_LOG}.host-clock.json"
fi

metrics_driver_status=0
if [ -n "$METRICS_DRIVER_PID" ]; then
    wait "$METRICS_DRIVER_PID" || metrics_driver_status=$?
    METRICS_DRIVER_PID=""
fi
if [ "$metrics_driver_status" -ne 0 ]; then
    echo "headless: metrics-latency: the host driver reported failures" >&2
    echo "headless: serial log: $SERIAL_LOG" >&2
    exit 1
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
    boot-shell-nvme|nvme-timeout|boot-memory-8g|boot-memory-16g|boot-memory-32g|boot-large-module)
        # The shipped plan starts no desktop, so ps2.drv waits for one
        # before it opens its input rings: devmgr's line says it is ready.
        required_markers="
devmgr: startup complete, entering service loop
procmgr: ready, entering receive loop
shell: cwd=@nvme:0/
devmgr: PS/2 driver started
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
exited code 78 (stopped by its CAP-003 startup check)
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
TEST: PASS log-launched
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
    console-route)
        # Only logstore's mirror prints these (under the program's name);
        # the exactly-once and no-unprefixed checks follow below.
        required_markers="
logstore: logstore: authorized typed diagnostics ready
console-route-check.app: CONSOLE-ROUTE: routed
console-route-check.app: CONSOLE-ROUTE: in three pieces
console-route-check.app: CONSOLE-ROUTE: bell?tab	end
console-route-check.app: CONSOLE-ROUTE: logged once
console-route-check.app: CONSOLE-ROUTE: after the announcement, announced=true
console-route-check.app: CONSOLE-ROUTE: after its own publisher, accepted=true
console-route-check.app: CONSOLE-ROUTE: lost= 0
"
        ;;
    late-introduction)
        # Started before logstore: its lines queue, procmgr introduces
        # logstore when it starts, and they reach the console through
        # logstore's mirror. Asked for an entry it was not started
        # without, procmgr refuses (nothing undeclared is introduced).
        required_markers="
procmgr: late-intro-check.app: optional service \"logstore\" not running; started without it; introduced when it starts
logstore: logstore: authorized typed diagnostics ready
late-intro-check.app: LATE-INTRO: queued before logstore
late-intro-check.app: LATE-INTRO: undeclared NOT_DECLARED
asked to be introduced for an entry it was not started without (refused)
introduced late to role
late-intro-check.app: LATE-INTRO: attached
late-intro-check.app: LATE-INTRO: lost= 0
late-intro-check.app: LATE-INTRO: exiting
"
        ;;
    log-retention)
        required_markers="
logstore: authorized typed diagnostics ready
TEST: PASS log-retention-quiet-source
TEST: PASS log-retention-full-history
TEST: PASS log-retention
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
    bench-build)
        required_markers="
devmgr: disk 0 attached as @ufs:0 (writable)
FS Server: @system is @ufs:0
ccl-remote: ready
ccl-remote: a host paired
ccl-remote: host build userspace/apps/hello/component.ccl
"
        ;;
    device-build)
        required_markers="
devmgr: disk 0 attached as @ufs:0 (writable)
FS Server: @system is @ufs:0
ccl-remote: ready
ccl-remote: a host paired
ccl-remote: host build userspace/apps/swap-probe/component.ccl
procmgr: staged app launches allowed by the Desktop switch
swap-probe: build 1 running
swap-probe: build 1 stopping
swap-probe: build 2 running
ccl-remote: host app rollback swap-probe
ccl-remote: host build userspace/apps/hello/component.ccl
hello: hello from an app built on CuBit
"
        ;;
    console-run-staged)
        required_markers="
devmgr: disk 0 attached as @ufs:0 (writable)
FS Server: @system is @ufs:0
ccl-remote: a host paired
ccl-console: first frame presented
ccl-remote: host build userspace/apps/hello/component.ccl
procmgr: staged app launches allowed by the Desktop switch
procmgr: staged program @system/work/remote/out/userspace/apps/hello/build/hello.app
hello: hello from an app built on CuBit
hello: rebuilt on CuBit, run again from the console
ccl-launcher: @system/work/generations/hello/1.app refused:
"
        ;;
    service-switch)
        required_markers="
lexicon: registered
ccl-remote: ready
ccl-remote: a host paired
procmgr: switch refused: SWITCH_OFF
procmgr: staged app launches allowed by the Desktop switch
procmgr: switch refused: MANIFEST_CHANGED
procmgr: switch lexicon.svc switched; it runs @system/storehouse/executables/
procmgr: switch lexicon.svc rolled back (the program ended inside its health window)
procmgr: switch clock.svc switched; it runs @system/storehouse/executables/
procmgr: switch lexicon.svc switched; it runs lexicon.svc pid
procmgr: logstore.svc -> @system/storehouse/executables/
 admitted for the next boot
"
        ;;
    acpi-live-replace)
        required_markers="
acpi: stage registration ok STARTING
ccl-remote: a host paired
procmgr: staged app launches allowed by the Desktop switch
procmgr: switch acpi.svc switched; it runs @system/storehouse/executables/
acpi: stage serve ok STOP_REQUESTED
acpi: stage retire ok QUIESCENT
procmgr: switch acpi.svc rolled back (the program ended inside its health window)
procmgr: switch acpi.svc rolled back (the program did not register its service in time)
procmgr: switch acpi.svc switched; it runs acpi.svc pid
"
        ;;
    desktop-live-replace)
        required_markers="
ccl-remote: a host paired
procmgr: staged app launches allowed by the Desktop switch
procmgr: switch desktop.svc switched; it runs @system/storehouse/executables/
desktop: stop requested; releasing the display
desktop: stopped; display released, exiting 0
procmgr: switch desktop.svc rolled back (the program ended inside its health window)
procmgr: switch desktop.svc switched; it runs desktop.svc pid
"
        ;;
    service-switch-reboot)
        # SERVICE_SWITCH_EXPECT=drift: the pinned object was damaged.
        if [ "${SERVICE_SWITCH_EXPECT:-}" = drift ]; then
            required_markers="
procmgr: generation drift: logstore.svc
lexicon: registered
"
        else
            required_markers="
procmgr: generation pin applied: logstore.svc
lexicon: registered
"
        fi
        ;;
    ccl-remote-session)
        required_markers="
ccl-remote: DEVELOPMENT PLAINTEXT on the LAN, port 8445; grants: diagnose, files, builds, mail
ccl-remote: listening on 10.0.2.15:8445
ccl-remote: ready
ccl-remote: a host paired
"
        ;;
    remote-screenshot)
        required_markers="
ccl-remote: DEVELOPMENT PLAINTEXT on the LAN, port 8445; grants: diagnose, capture
ccl-remote: ready
ccl-remote: a host paired
desktop: screen capture allowed for the remote session
desktop: screen captured for process
ccl-remote: host capture output 0
"
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
net-link-check: the idle stream's bytes arrive PASS
net-link-check: a stream read empty lets its reader sleep PASS
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
netstack: busy-poll windows=
"
        # netstack's busy-poll window polls memory: at most one kernel call
        # per ten turns (each turn used to make two, a request poll and
        # GETTIME: about 107,000 a second while ccl-remote streamed).
        WINDOW_LINE=$(grep -aoE 'netstack: busy-poll windows= [0-9]+ turns= [0-9]+ kernel calls= [0-9]+' \
                      "$SERIAL_LOG" | tail -1)
        WINDOW_TURNS=$(echo "$WINDOW_LINE" | sed -E 's/.*turns= ([0-9]+).*/\1/')
        WINDOW_CALLS=$(echo "$WINDOW_LINE" | sed -E 's/.*calls= ([0-9]+)$/\1/')
        if [ -n "$WINDOW_LINE" ] && [ $((WINDOW_CALLS * 10)) -gt "$WINDOW_TURNS" ]; then
            echo "headless: bench-net: netstack's busy-poll window made $WINDOW_CALLS kernel calls in $WINDOW_TURNS turns (at most one in ten)" >&2
            exit 1
        fi
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
    bench-cualloc)
        required_markers="
cualloc-bench: start
cualloc-bench: done
"
        if grep -qa 'cualloc-bench: .*FAIL' "$SERIAL_LOG"; then
            echo "headless: cualloc-bench reported a failure" >&2
            exit 1
        fi
        grep -a 'cualloc-bench: ' "$SERIAL_LOG" || true
        ;;
    kern014-bench)
        required_markers="
kern014: start
kern014: done
"
        if grep -qa 'kern014: FAIL' "$SERIAL_LOG"; then
            echo "headless: kern014-bench reported a failure" >&2
            exit 1
        fi
        grep -a 'kern014: ' "$SERIAL_LOG" || true
        ;;
    fat32-volume|usb-disk)
        required_markers="
fat-volume: start
fat-volume: PASS
fat-volume: end
"
        if [ "$TEST_NAME" = "usb-disk" ]; then
            required_markers="$required_markers
fat-volume: unplug
fat-volume: removal seen
usb disk ready slot=
optical LUN=0
fat-volume: live medium read only ok
disk gone (detached)
devmgr: @usb:0 attached
devmgr: @usb:1 attached
devmgr: @usb:1 gone
devmgr: @usb:1 detached
"
        fi
        if grep -qa 'fat-volume: FAIL' "$SERIAL_LOG"; then
            echo "headless: fat32-volume reported a failure" >&2
            exit 1
        fi
        ;;
    usb-eject)
        required_markers="
usb-eject: start
devmgr: @usb:0 attached
usb-eject: A written
usb-eject: eject refused open=1
FS Server: @usb:0 ejected
usb-eject: A safe to remove
devmgr: @usb:0 detached
usb-eject: A data intact
devmgr: @usb:1 attached
usb-eject: two sticks ok
usb-eject: removal seen
@usb:0 was removed without Eject
usb-eject: kept handle refused
usb-eject: dirty A read only
usb-eject: both ejected
usb-eject: PASS
usb-eject: end
"
        if grep -qa 'usb-eject: FAIL' "$SERIAL_LOG"; then
            echo "headless: usb-eject reported a failure" >&2
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
sched-latency: workload=audio load=commit
sched-latency: workload=audio load=protect
sched-latency: done
"
        if grep -qa 'sched-latency: FAIL\|sched-latency: .*unavailable' "$SERIAL_LOG"; then
            echo "headless: sched-latency reported a failure" >&2
            exit 1
        fi
        # A growing heap must not stall the others (bounded kernel steps).
        if ! python3 -I "$ROOT_DIR/tests/sched-latency/check-commit-load.py" "$SERIAL_LOG"; then
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
libc-check: an unimplemented system call is ENOSYS PASS
libc-check.app: cubit-libc: unimplemented system call 999
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
libc-check: mmap NORESERVE reserves address space PASS
libc-check: MAP_FIXED commits inside a reservation PASS
libc-check: committed pages are zero PASS
libc-check: MAP_FIXED PROT_NONE decommits PASS
libc-check: a recommitted range is fresh zero pages PASS
libc-check: MAP_FIXED over committed pages replaces only its range PASS
libc-check: munmap trims the head and tail of a reservation PASS
libc-check: the kept middle survives the trim PASS
libc-check: munmap of committed pages inside a reservation PASS
libc-check: munmap of an unaligned range is refused PASS
libc-check: MAP_FIXED outside a reservation is refused PASS
libc-check: the whole reservation is released PASS
libc-check: a second reservation, one page committed PASS
libc-check: mprotect opens a reservation in place PASS
libc-check: the second reservation is released PASS
libc-check: opendir and readdir PASS
libc-check: a file outside the scope is denied PASS
libc-check: create and write a file PASS
libc-check: fstat: type, size, links, inode PASS
libc-check: fsync on a disk that persists PASS
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
    ladybird-test262)
        # The totals, and with LADYBIRD_TEST262_REFERENCE (the same build's
        # libjs-test262 per-file results on Linux) the pass counts within
        # 0.1% of the tests.
        required_markers="
LADYBIRD-TEST262: DONE
"
        python3 "$ROOT_DIR/tests/ladybird/test262/compare.py" "$SERIAL_LOG" \
            ${LADYBIRD_TEST262_REFERENCE:+"$LADYBIRD_TEST262_REFERENCE"} || exit 1
        ;;
    ladybird-js)
        required_markers="
ladybird-js-check: js unix.stdout: \"ladybird-js: arithmetic and strings PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: allocation and collection PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: ArrayBuffer and typed arrays PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: resizable ArrayBuffer PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: RegExp PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: RegExp Unicode properties PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: JSON PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: BigInt PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: Intl.NumberFormat PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: Intl.DateTimeFormat PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: Intl.Collator PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: String.prototype.normalize PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: Date PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: Math PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: promise jobs PASS\"
ladybird-js-check: js unix.stdout: \"ladybird-js: async functions PASS\"
ladybird-js-check: js runs check.js and exits with 0 PASS
ladybird-js-check: js wrote its lines through the lent rings PASS
ladybird-js-check: every check in check.js passes PASS
LADYBIRD-JS-CHECK: PASS
"
        if grep -qF 'LADYBIRD-JS-CHECK: FAIL' "$SERIAL_LOG"; then
            echo "headless: ladybird-js test reported a failure" >&2
            exit 1
        fi
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
    ccl-build-native)
        required_markers="
ccl-build-check: ccl-build starts (first build, from an empty store) PASS
ccl-build-check: ccl-build is ended while it builds (interrupted by its process identity) PASS
ccl-build-check: the next build completes (exit code 0) PASS
ccl-build-check: it reports its steps PASS
ccl-build-check: ps2.drv is byte for byte the Linux build's PASS
ccl-build-check: ccl-build verify finds no damage PASS
ccl-build-check: a build after it completes (exit code 0) PASS
ccl-build-check: it runs no step: every step is answered by the cache PASS
ccl-build-check: its ps2.drv is still the Linux build's PASS
CCL-BUILD-NATIVE: PASS
"
        if grep -qF 'CCL-BUILD-NATIVE: FAIL' "$SERIAL_LOG"; then
            echo "headless: ccl-build-native reported a failure" >&2
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
    large-manifest)
        required_markers="
procmgr: init launched: large-manifest-probe.app
large-manifest-probe: started with its large manifest PASS
"
        ;;
    startup-wait)
        # In plan order: no provider; one that ends first; one that misses
        # its ready deadline; one that registers late, after 2.5 s.
        required_markers="
procmgr: launch of startup-wait-client.app refused: required service \"provider\" (role 36) NO_PROVIDER: no started program provides it (CAP-003)
startup-wait-provider: ends in 1500 ms without registering
procmgr: launch of startup-wait-client.app waits for service \"provider\" (role 36) to register (CAP-003)
startup-wait-provider: ending unregistered
procmgr: launch of startup-wait-client.app refused: required service \"provider\" (role 36) PROVIDER_EXITED: its provider ended before it registered (CAP-003)
startup-wait-provider: ends in 4000 ms without registering
procmgr: launch of startup-wait-client.app refused: required service \"provider\" (role 36) DEADLINE_PASSED: its provider did not register by its deadline (CAP-003)
startup-wait-provider: registers in 2500 ms
startup-wait-provider: registered
procmgr: init launched: startup-wait-client.app
startup-wait-client: started
startup-wait-client: provider answered PASS
"
        if [ "$(grep -c 'startup-wait-client: started' "$SERIAL_LOG")" -ne 1 ] ||
           [ "$(grep -c 'procmgr: init spawn failed: startup-wait-client.app: a service it requires is not running' "$SERIAL_LOG")" -ne 3 ]; then
            echo "headless: a refused client ran, or the late provider's client did not" >&2
            exit 1
        fi
        ;;
    exit-status)
        # Each ending's PID (the program's own line) must exit with its
        # code (procmgr's line for that PID).
        required_markers="
exit-status-check: pid
"
        for ENDING in Returns:0 Sets_Failure:1 Raises:134; do
            if ! awk -v ending="${ENDING%%:*}" -v code="${ENDING##*:}" '
                /exit-status-check: pid [0-9]+ ends / {
                    for (i = 1; i <= NF; i++) if ($i == "pid") p = $(i + 1)
                    if ($NF == ending) want = p
                }
                /procmgr: process com.cubit.exit-status-check pid/ && want != "" {
                    line = $0; sub(/.* pid ?/, "", line); split(line, f, " ")
                    if (f[1] == want) { got = (line ~ ("exited code " code "($|;)")) ; seen = 1 }
                }
                END { exit !(seen && got) }' "$SERIAL_LOG"; then
                echo "headless: exit-status-check ${ENDING%%:*} did not exit with code ${ENDING##*:}" >&2
                grep -aE 'exit-status-check' "$SERIAL_LOG" >&2
                exit 1
            fi
            echo "headless: exit-status-check ${ENDING%%:*} exited with code ${ENDING##*:}"
        done
        ;;
    supervision)
        required_markers="
procmgr: parameters supervision-child.app: (Child_Parameters label => Child_Label.First count => 7 mode => (Child_Mode.Exit_With 3))
procmgr: start_child supervision-child.app pid
 supervised by com.cubit.test.supervision-check pid
 (restart temporary, stop grace 1000 ms)
supervision-child: decoded label=First count=7 mode=Exit_With 3
supervision-check: a child started with typed parameters PASS
supervision-check: the child's exit reached its supervisor: pid
 exited code 3 PASS
procmgr: supervised child com.cubit.test.supervision-child pid
supervision-check: a parameter of the wrong type is refused (LAUNCH_REFUSED, parameters rejected) PASS
procmgr: parameters refused: supervision-child.app: not a Child_Parameters
supervision-check: a program its may_launch does not name is NOT_GRANTED PASS
supervision-check: a device-class start from a program is CLASS_NOT_GRANTED PASS
supervision-check: badge bindings from a program are BINDINGS_NOT_GRANTED PASS
supervision-check: an ended sibling as a dependency is DEPENDENCY_UNKNOWN PASS
supervision-check: a held start from a program is SINK_NOT_GRANTED PASS
supervision-check: a sink asked by a program is SINK_NOT_GRANTED PASS
supervision-child: decoded label=Lingering count=1 mode=Linger
supervision-child: lingering until its supervisor ends
supervision-check: a lingering child started PASS
supervision-check: the lingering child still runs PASS
supervision-check: releasing a child that was not held is NOT_HELD PASS
supervision-check: releasing a process it does not supervise is NOT_SUPERVISED PASS
supervision-check: all checks passed; ending with a child running (procmgr reaps it)
procmgr: process com.cubit.test.supervision-check pid
 exited code 0
procmgr: supervisor com.cubit.test.supervision-check pid
 ended; reaping its child com.cubit.test.supervision-child pid
 reaped (its supervisor pid
"
        if grep -aF 'supervision-check: ' "$SERIAL_LOG" | grep -qF ' FAIL' ||
           [ "$(grep -acF 'supervision-child: decoded' "$SERIAL_LOG")" -ne 2 ] ||
           [ "$(grep -acF ' ended; reaping its child com.cubit.test.supervision-child' "$SERIAL_LOG")" -ne 1 ] ||
           [ "$(grep -ac 'procmgr: start_child from com.cubit.test.supervision-check pid.* refused: ' "$SERIAL_LOG")" -ne 7 ] ||
           [ "$(grep -ac 'procmgr: release of pid.* refused: ' "$SERIAL_LOG")" -ne 2 ]; then
            echo "headless: supervision: a check failed, a refused child ran, or the child was not reaped once" >&2
            exit 1
        fi
        # The lingering child ends only after its supervisor did (reaped),
        # and it was stopped, not exited.
        if ! awk '/supervision-check: all checks passed/ && !done { done = NR }
                  / ended; reaping its child com.cubit.test.supervision-child/ && !reap { reap = NR }
                  /procmgr: process com.cubit.test.supervision-child pid.* stopped \(killed or faulted\)/ && !stop { stop = NR }
                  END { exit !(done && reap && stop && done < reap && reap < stop) }' "$SERIAL_LOG"; then
            echo "headless: supervision: the running child was not stopped after its supervisor ended" >&2
            exit 1
        fi
        ;;
    graphics-report-nodriver)
        # devmgr's own entry: the std VGA (1234:1111) has no GPU driver;
        # Desktop composes on the CPU by its default policy; the console
        # reads both back typed.
        required_markers="
devmgr: renderer report gpu 1234:1111 cpu software no-driver-bound
desktop: renderer report output-0 software policy detail 1 (posted)
ccl-console: REPL completed: List<Gfx_Entry>:
Gfx_Why.No_Driver_Bound
(Gfx_Why.Policy Gfx_Policy.Composition_CPU)
"
        ;;
    graphics-report-desktop-down)
        required_markers="
devmgr: renderer report gpu
desktop: renderer report output-0 software policy detail 1 (posted)
GRAPHICS-CHECK: PASS the report answers with Desktop gone
"
        ;;
    graphics-report-failing)
        # virtio-gpu (init-failure build) stops at start-up (before its own
        # lines reach logstore): devmgr's entry says Driver_Failed at Init,
        # and the console reads it back.
        required_markers="
devmgr: renderer report gpu 1af4:1050 virtio-gpu software driver-failed
ccl-console: REPL completed: List<Gfx_Entry>:
(Gfx_Why.Driver_Failed Gfx_Stage.Init)
"
        ;;
    power-sources)
        # The order the logstore mirror gives program lines is not theirs:
        # each line is checked on its own, and the Desktop states by count.
        # Identities: \_SB_.ADP1 and \_SB_.PCI0.LPCB.EC0_.BAT0 (the stand-in's).
        required_markers="
power-standin: registered (STAND-IN for acpi.svc's power sources)
power: acpi.svc is not running yet: its power sources are watched when procmgr introduces it
power: introduced to acpi.svc's observer endpoint
power: acpi.svc does not take the power-source watch yet (UNAVAILABLE)
power-standin: ready: the first publication (an adapter offline, a battery discharging)
power: acpi.svc serves its power sources (watch generation
power: sources generation 2: 2 sources, 1 batteries, DISCHARGING, runs on BATTERY, charge 730/1000, FRESH (watched,
power: source ADP1 AC_ADAPTER OFFLINE FRESH
power: source BAT0 INTERNAL_BATTERY DISCHARGING charge 730/1000 left TO_EMPTY 13140 s energy 36500 mWh of 50000 mWh rate 10000 mW low CLEAR FRESH
power-standin: the adapter is plugged in (Notify ADP1 and BAT0, 16#80#)
power: sources generation 3: 2 sources, 1 batteries, CHARGING, runs on MAINS, charge 740/1000, FRESH (watched,
power: source BAT0 INTERNAL_BATTERY CHARGING charge 740/1000 left TO_FULL 3744 s energy 37000 mWh of 50000 mWh rate 12500 mW low CLEAR FRESH
desktop: status battery shown: Battery 73%, discharging, 3 h 39 min left
desktop: status battery shown: Battery 74%, charging, 1 h 2 min to full [bolt]
power-standin: demand read 1: the battery sampled again (_BST), 37100 mWh
power: source BAT0 INTERNAL_BATTERY CHARGING charge 742/1000 left TO_FULL 3715 s energy 37100 mWh of 50000 mWh rate 12500 mW low CLEAR FRESH
ccl-console: REPL completed: List<Power_Source>: [(Power_Source \"ADP1\" Power_Kind.AC_Adapter Power_State.Online
ccl-console: REPL completed: Power_Source: (Power_Source \"BAT0\" Power_Kind.Internal_Battery Power_State.Charging Power_Freshness.Fresh (Power_Quantity.Known 742) (Power_Runtime.To_Full 3715) (Power_Quantity.Known 37100) (Power_Quantity.Known 50000) (Power_Quantity.Known 57000) (Power_Quantity.Known 12500) (Power_Quantity.Known 12100) (Power_Conditions Power_Flag.Clear Power_Flag.Unknown
"
        # Shown before anything was read is fine (none yet); hidden after a
        # battery was shown is not.
        if ! awk '/desktop: status battery shown/ { shown = 1 }
                  /desktop: status battery hidden/ && shown { bad = 1 }
                  END { exit bad }' "$SERIAL_LOG"; then
            echo "headless: power-sources: Desktop hid the Battery item while a battery existed" >&2
            exit 1
        fi
        # No polling: no page was read between the first reading and the
        # Notify, five seconds later (the stand-in measures it).
        if ! grep -aoE "no page was read for [0-9]+ ms before it" "$SERIAL_LOG" |
             awk '{ quiet = $6 } END { exit !(quiet >= 4000) }'; then
            echo "headless: power-sources: pages were read before the Notify without a reason (polling?)" >&2
            exit 1
        fi
        # The charging state reached Desktop before anything asked for a
        # read: the injector types (power.sources) only after Desktop's
        # charging line (POWER_READY), and nothing else asks.
        ;;
    shutdown-desktop-cancel)
        required_markers="
power: ready
desktop: shut down: confirmation shown
desktop: shut down: cancelled; nothing sent
"
        if grep -aqF "power: POWER_OFF requested" "$SERIAL_LOG" ||
           grep -aqF "procmgr: shutdown requested" "$SERIAL_LOG"; then
            echo "headless: shutdown-desktop-cancel: a cancelled confirmation still asked for a shutdown" >&2
            exit 1
        fi
        ;;
    acpi-shutdown)
        required_markers=""
        bash "$ROOT_DIR/tests/headless/check-acpi-shutdown.sh" "$SERIAL_LOG" "$qemu_status" || exit 1
        if [ -n "${ACPI_SHUTDOWN_REFUSAL_TABLE:-}" ]; then
            python3 -I "$ROOT_DIR/tests/headless/check-acpi-consumer-refusal.py" "$SERIAL_LOG" || exit 1
            # PWR-004: power.svc's watch is served by the actual acpi.svc and
            # the fixture's adapter reaches it, named by its namespace path.
            for marker in "power: acpi.svc serves its power sources (watch generation" \
                          "power: source AC00 AC_ADAPTER ONLINE FRESH"; do
                if ! grep -aqF "$marker" "$SERIAL_LOG"; then
                    echo "headless: acpi-shutdown: power.svc did not get acpi.svc's power sources: $marker" >&2
                    exit 1
                fi
            done
        fi
        # No driver forced, none slow, nothing after the drivers' report
        # before it; S5 may cut the console's tail (LOG-009).
        if ! python3 -I "$ROOT_DIR/tests/driver-stop/check_driver_stops.py" --lossy-tail --services \
               --fixture com.cubit.test.shutdown-stubborn --fixture com.cubit.test.shutdown-cooperative "$SERIAL_LOG"; then
            echo "headless: acpi-shutdown: a driver missed its Stop, or the drivers' report was not before S5" >&2
            exit 1
        fi
        if dumpe2fs -h "$TEMP_DISK" 2>/dev/null | grep -q needs_recovery ||
           ! e2fsck -fn "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: acpi-shutdown: filesystem did not detach cleanly" >&2
            exit 1
        fi
        ;;
    shutdown|shutdown-console|shutdown-text|shutdown-desktop)
        if [ "$TEST_NAME" = "shutdown-desktop" ]; then
            required_markers="
power: ready
desktop: shut down: confirmation shown
desktop: shut down: power.svc answered ACCEPTED
power: POWER_OFF requested by DESKTOP
procmgr: shutdown: stopping com.cubit.desktop pid
display: Stop: output 0:
"
            SHUTDOWN_ORDER="com.cubit.test.shutdown-stubborn com.cubit.desktop com.cubit.display"
        elif [ "$TEST_NAME" = "shutdown-text" ]; then
            required_markers="
power: POWER_OFF requested by POWER_BUTTON
procmgr: shutdown: stopping com.cubit.text pid
 (service, grace 3000 ms)
ui-app: Stop received: closing (System_Shutdown)
text: recovery copy of @nvme:0/Documents/notes.txt -> @system/Recovery/com.cubit.text/recovery-1.txt written
text: shutdown: 1 recovery copies; exiting
procmgr: shutdown: com.cubit.text pid
 STOPPED_CLEAN after
procmgr: shutdown: stopping com.cubit.desktop pid
"
            SHUTDOWN_ORDER="com.cubit.text com.cubit.desktop com.cubit.display"
        elif [ "$TEST_NAME" = "shutdown" ]; then
            required_markers="
shutdown-standin: registered (STAND-IN for acpi.svc;
power: ready
shutdown-standin: power button pressed (count 1)
power: power button pressed (FIXED_EVENT, count 1)
power: POWER_OFF requested by POWER_BUTTON
procmgr: shutdown: stopping com.cubit.test.shutdown-cooperative pid
 (service, grace 2000 ms)
shutdown-cooperative: Stop received; quiescing
shutdown-cooperative: quiesced; exiting 0
"
            SHUTDOWN_ORDER="com.cubit.test.shutdown-stubborn com.cubit.test.shutdown-cooperative"
        else
            required_markers="
power: ready
power: POWER_OFF requested by CONSOLE
procmgr: shutdown: stopping com.cubit.ccl-console pid
procmgr: shutdown: stopping com.cubit.desktop pid
procmgr: shutdown: stopping com.cubit.display pid
display: session lent scanout: the compositor composes into the scanout buffers
display: Stop: output 0: 0 loans revoked; returned to the GPU driver 3 of 3 lent buffers and 3 of 3 scanout mappings
"
            SHUTDOWN_ORDER="com.cubit.ccl-console com.cubit.test.shutdown-stubborn com.cubit.desktop com.cubit.display"
        fi
        required_markers="$required_markers
procmgr: shutdown requested by com.cubit.power pid
: power off, apps first;
procmgr: shutdown: apps stopped in
power: procmgr stopped the apps
power: asking acpi.svc to prepare to sleep (S5)
shutdown-standin: Prepare_To_Sleep S5 answered PREPARED
power: prepare to sleep PREPARED
procmgr: shutdown: keeping com.cubit.power pid
 (the requester)
procmgr: shutdown: keeping com.cubit.acpi pid
 (Power_Tail: the firmware step)
procmgr: shutdown: keeping com.cubit.logstore pid
 (logstore: the final lines)
logstore: storage stopping:
procmgr: shutdown: logstore detached its storage:
procmgr: shutdown: the filesystem commits (deadline 10000 ms)
FS: stop: nvme:0 committed and detached (journal empty, needs_recovery clear)
procmgr: shutdown: storage COMMITTED
devmgr: shutdown: procmgr asks to stop the drivers: ACCEPTED,
procmgr: shutdown: devmgr stops its drivers (
devmgr: shutdown: drivers stopped
procmgr: shutdown: stopped in
power: procmgr stopped the system:
power: asking acpi.svc to enter S5
shutdown-standin: entering S5 (SLP_TYP 0
platform power: entering S5 (SLP_TYPa 0, SLP_TYPb 0)
 other CPUs parked
"
        if [ "$TEST_NAME" != "shutdown-text" ]; then
            required_markers="$required_markers
procmgr: shutdown: stopping com.cubit.test.shutdown-stubborn pid
 (service, grace 500 ms)
shutdown-stubborn: Stop received; ignoring it
did not end within its grace of 500 ms; ending it
procmgr: shutdown: com.cubit.test.shutdown-stubborn pid
 FORCED after
"
        fi
        # QEMU must have ended by itself (ACPI S5), not at the timeout.
        if [ "$qemu_status" -ne 0 ]; then
            echo "headless: $TEST_NAME: QEMU did not power off (status $qemu_status)" >&2
            exit 1
        fi
        if grep -aqF "power: halted" "$SERIAL_LOG" || grep -aqF "STILL_RUNNING" "$SERIAL_LOG"; then
            echo "headless: $TEST_NAME: the machine halted instead of powering off" >&2
            exit 1
        fi
        # Reverse startup order: each entry's Stop after the one started
        # after it was decided; storage after every service; drivers after
        # storage; S5 last. LOG-009 (KERN-004): the kernel's own S5 line
        # comes after every line queued before the call (power.svc's stop
        # report and request, the stand-in's line), then the parking: the
        # kernel drained its console ring before it wrote PM1.
        if ! awk -v order="$SHUTDOWN_ORDER" '
              BEGIN { n = split(order, names, " ") }
              { for (i = 1; i <= n; i++)
                  if (!at[i] && index($0, "procmgr: shutdown: stopping " names[i] " pid")) at[i] = NR }
              # the narration by procmgr of the commit. Lines of routed programs
              # reach serial through the logstore mirror in drain order;
              # across programs they keep the order of the steps only
              # because each side flushes its lines into logstore before it
              # hands the step on (ABI-002): filesystem.svc before it
              # answers the commit, devmgr before it answers procmgr,
              # procmgr before it calls them and before it answers power.svc.
              /procmgr: shutdown: storage / && !fs { fs = NR }
              /FS: stop: storage / && !fs_own { fs_own = NR }
              /logstore: storage stopping:/ && !detach { detach = NR }
              /procmgr: shutdown: stopping com.cubit.logstore pid/ { stopped_logs = NR }
              /procmgr: shutdown: stopped in/ && !final { final = NR }
              /shutdown-standin: Prepare_To_Sleep S5 answered/ && !prep { prep = NR }
              /devmgr: shutdown: procmgr asks to stop the drivers/ && !dev { dev = NR }
              /shutdown-standin: entering S5/ && !s5 { s5 = NR }
              /power: procmgr stopped the system:/ && !report { report = NR }
              /power: asking acpi.svc to enter S5/ && !ask { ask = NR }
              /platform power: entering S5/ && !kernel { kernel = NR }
              / other CPUs parked/ && !parked { parked = NR }
              END {
                for (i = 1; i <= n; i++) if (!at[i] || (i > 1 && at[i] < at[i - 1])) exit 1
                # logstore: never stopped; it detached its storage after
                # the last service and before the commit, and took
                # procmgr final lines after the drivers stopped.
                if (stopped_logs || !detach || detach < at[n] || detach > fs || !final || final < dev) exit 1
                # Flushed at each hand-over: the line of the filesystem
                # before procmgr hears the outcome; the final line of procmgr
                # before power.svc reports the stop.
                if (!fs_own || fs_own > fs || !report || final > report) exit 1
                exit !(prep && prep < at[1] && fs > at[n] && dev > fs && s5 > dev &&
                       report > dev && ask > report && kernel > ask && kernel > s5 &&
                       parked > kernel)
              }' "$SERIAL_LOG"; then
            echo "headless: $TEST_NAME: not prepare-to-sleep, then reverse startup order, logstore's storage detached, storage, drivers, procmgr's final lines, S5 (with the whole console tail before the kernel's S5 line)" >&2
            exit 1
        fi
        # The filesystem detached the journaled volume: no recovery needed,
        # and the disk is consistent as Linux reads it.
        if dumpe2fs -h "$TEMP_DISK" 2>/dev/null | grep -q needs_recovery; then
            echo "headless: $TEST_NAME: the disk still needs recovery (not committed and detached)" >&2
            exit 1
        fi
        if ! e2fsck -fn "$TEMP_DISK" >/dev/null 2>&1; then
            echo "headless: $TEST_NAME: e2fsck found the disk inconsistent" >&2
            exit 1
        fi
        if [ "$TEST_NAME" = "shutdown-text" ]; then
            # The recovery copy holds the unsaved line; the user's file
            # does not (nothing was saved over it).
            if ! debugfs -R "cat Recovery/com.cubit.text/recovery-1.txt" "$TEMP_DISK" 2>/dev/null |
                   grep -qF "unsaved work"; then
                echo "headless: shutdown-text: the recovery copy does not hold the unsaved edit" >&2
                exit 1
            fi
            if debugfs -R "cat Documents/notes.txt" "$TEMP_DISK" 2>/dev/null | grep -qF "unsaved work" ||
               ! debugfs -R "cat Documents/notes.txt" "$TEMP_DISK" 2>/dev/null | cmp -s - "$ROOT_DIR/tests/headless/text-notes.txt"; then
                echo "headless: shutdown-text: the user's file changed" >&2
                exit 1
            fi
        fi
        # Every driver devmgr started and every unit procmgr stopped (but
        # the two fixtures) ended on its Stop, quickly, and devmgr's report
        # came before S5 (PWR-003; the stand-in drains the console).
        # Not judged yet: the CCL console's window loop does not read its
        # Stop (forced at its grace; PWR-003 follow-up for its owner).
        STOP_EXEMPT=(--fixture com.cubit.test.shutdown-stubborn --fixture com.cubit.test.shutdown-cooperative)
        if [ "$TEST_NAME" = "shutdown-console" ]; then
            STOP_EXEMPT+=(--fixture com.cubit.ccl-console)
        fi
        if ! python3 -I "$ROOT_DIR/tests/driver-stop/check_driver_stops.py" --services "${STOP_EXEMPT[@]}" "$SERIAL_LOG"; then
            echo "headless: $TEST_NAME: a driver missed its Stop, or the drivers' report was not before S5" >&2
            exit 1
        fi
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
    wifi-crypto)
        required_markers="
wifi-crypto-check: pbkdf2-sha1-pmk=ok aes-cmac=ok xmd-sha256=ok PASS
"
        ;;
    font-masks)
        required_markers="
font-masks: glyphs=570 failed=0 all=
"
        ;;
    launch-gate)
        # CAP-012 (docs/authority-model.md 7.5): Launch-into-S
        # (userspace/apps/launch-gate-check).
        required_markers="
launch-gate: procmgr's endpoint alone starts nothing (OP_SPAWN is retired) PASS
launch-gate: a launch without a Launch badge is refused PASS
launch-gate: with the badge, a child starts into the user session PASS
launch-gate-child: the clock given PASS
launch-gate-child: clock control and the framebuffer withheld PASS
launch-gate: a required request the session withholds is refused by name (clock-control) PASS
launch-gate: the launch hands back a launch handle with Observe, Stop, Attach, Introduce, Debug PASS
launch-gate: the requester's own control message to the child is refused (procmgr is its parent) PASS
launch-gate: Observe through the handle: the child runs PASS
launch-gate: a launch handle reaches no other procmgr operation (OP_LAUNCH refused) PASS
launch-gate: a handle operation without a launch handle is refused PASS
launch-gate: the narrowed handle may not debug (Right_Missing) PASS
launch-gate: Debug through the handle hands over Debug_Observe on the child PASS
launch-gate: the debug session lists the child's threads PASS
launch-gate: procmgr's exit notice for the child arrives PASS
launch-gate: after it ends, Observe gives its outcome (exited, code 0) PASS
launch-gate: the handle dies with the child: Debug answers Child_Ended PASS
launch-gate: revoking reaches the handle narrowed from it PASS
launch-gate: the session's debug authority observes clock.svc, a system service PASS
launch-gate: procmgr itself is never debuggable PASS
launch-gate: launching left the requester's handle table as it was (
launch-gate: checks done
"
        ;;
    debug-gate-disabled)
        # DBG-001: Disabled: no debug-session authority, no debug session.
        required_markers="
procmgr: system policy: debugging Disabled: debugging is disabled for this image
debug-gate: debug-session withheld
debug-gate: handle Debug answers DEBUGGING_DISABLED
debug-gate: done
"
        ;;
    debug-gate-production)
        # DBG-001: Production refuses as Disabled until the owner's approval
        # path exists, and says so.
        required_markers="
procmgr: system policy: debugging Production: production debugging needs owner approval (not built yet)
debug-gate: debug-session withheld
debug-gate: handle Debug answers APPROVAL_NEEDED
debug-gate: done
"
        ;;
    debug-gate-development)
        # DBG-001: Development behaves as built.
        required_markers="
procmgr: system policy: debugging Development: Debug_Observe within the session's debug envelope
debug-gate: debug-session granted; session debug answers DONE
debug-gate: handle Debug answers DONE
debug-gate: done
"
        ;;
    debug-kernel)
        # DBG-001, CAP-012: the engine through the kernel, no stub
        # (userspace/apps/debug-kernel-check, debug-plain).
        required_markers="
debug-kernel: debug-plain (no stub) starts into the user session with a launch handle PASS
debug-kernel: the launch handle's Debug right gets Debug_Observe from procmgr PASS
debug-kernel: attach to debug-plain through the kernel PASS
debug-kernel: the build-id comes from the target's own memory PASS
debug-kernel: the thread waits Sleeping or on a futex PASS
debug-kernel: Plain_Work.Pause_Between is on the stack in plain_work.adb, unwound from the kernel's registers PASS
debug-kernel: its caller Plain_Work.Run is on the stack too PASS
debug-kernel: local Remaining of Plain_Work.Pause_Between has a value PASS
debug-kernel: Totals read live through a view PASS
debug-kernel: memory at Totals reads through the kernel PASS
debug-kernel: the session's debug authority gets Debug_Observe on clock.svc PASS
debug-kernel: clock.svc's frames, unwound with its debug information PASS
debug-kernel: ALL PASS
"
        if grep -a 'debug-kernel: ' "$SERIAL_LOG" | grep -qF ' FAIL'; then
            echo "headless: debug-kernel checks failed" >&2
            exit 1
        fi
        ;;
    debug-observe)
        # The kernel's debug sessions (DBG-001, Debug_Sessions), checked from
        # procmgr's place (userspace/apps/debug-observe-check).
        required_markers="
debug-observe: the target spawns PASS
debug-observe: the target resumes PASS
debug-observe: the target's Control mints Debug_Observe PASS
debug-observe: Describe names the kind and the target's identity PASS
debug-observe: the session's slice lies in the view aperture PASS
debug-observe: Threads: one thread, in a futex wait with no deadline PASS
debug-observe: Registers: the general block is written PASS
debug-observe: Registers: a blocked thread's callee-saved registers are exact PASS
debug-observe: Registers: RIP follows the SYSCALL, and its arguments are kept PASS
debug-observe: Registers: a system-call frame, with the user selectors PASS
debug-observe: Registers: the XSAVE image, with the reset MXCSR PASS
debug-observe: Registers: a stale thread generation is refused PASS
debug-observe: Map_View maps the target's code page PASS
debug-observe: the view shows the target's bytes PASS
debug-observe: a second view on one page is Occupied PASS
debug-observe: the kernel refuses to read the aperture PASS
debug-observe: the kernel refuses to write the aperture PASS
debug-observe: Unmap_View removes the view PASS
debug-observe: the page maps again after Unmap PASS
debug-observe: Map_View refuses a page the target does not map PASS
debug-observe: Map_View refuses another session's slice PASS
debug-observe: Map_View refuses a kernel-half target PASS
debug-observe: the target's Control mints PAUSE PASS
debug-observe: PAUSE Threads answers Not_Yet PASS
debug-observe: PAUSE Describe answers PASS
debug-observe: PAUSE detaches PASS
debug-observe: PAUSE is revoked after Detach PASS
debug-observe: the target's Control mints MODIFY PASS
debug-observe: MODIFY Threads answers Not_Yet PASS
debug-observe: MODIFY detaches PASS
debug-observe: the target's Control mints INSTRUMENT PASS
debug-observe: INSTRUMENT Threads answers Not_Yet PASS
debug-observe: INSTRUMENT detaches PASS
debug-observe: a Control without Grant does not mint PASS
debug-observe: a stale or absent identity mints nothing PASS
debug-observe: Revoke_Debug through the Control PASS
debug-observe: a revoked session answers Revoked PASS
debug-observe: Revoke removed the views (the slice maps again) PASS
debug-observe: the target is stopped PASS
debug-observe: a stopped target's session ends PASS
debug-observe: done PASS
"
        ;;
    spawn-rules)
        # The kernel's SPAWN rules (CAP-012, Spawn_Admission), checked from
        # procmgr's place (userspace/apps/spawn-rules-check).
        required_markers="
spawn-rules: the process root spawns, with control, endpoint and CSPACE PASS
spawn-rules: a child's Control does not spawn (rule 1) PASS
spawn-rules: a Read and Execute copy of the root is derived PASS
spawn-rules: a spawn authority without Grant spawns PASS
spawn-rules: without Grant: no endpoint and no CSPACE (rule 2) PASS
spawn-rules: without Write: the child's Control cannot stop it (rule 2) PASS
spawn-rules: without Grant: another process is not named parent (rule 3) PASS
spawn-rules: with Grant over every process: another parent is named PASS
spawn-rules: done PASS
"
        ;;
    vm-desktop)
        required_markers="
vmm guest: CuBit guest: Linux is up under the CuBit hypervisor (guest serial console)
vmm guest: CuBit guest: 2 CPUs online
vmm guest: CuBit guest: shell ready
desktop: keys to Guest serial console
vmm guest: typed-42
vmm: key to echo
vmm guest: interrupted-5
vm-desktop-shots: key to echo
vm-desktop-shots: done PASS
"
        grep -a 'vm-desktop-shots: key to echo\|vmm guest: typed-\|vmm guest: interrupted-' "$SERIAL_LOG" || true
        if grep -aq 'vmm guest: .*sleep 100' "$SERIAL_LOG" &&
           ! grep -aq 'vmm guest: .*\^C' "$SERIAL_LOG"; then
            echo "headless: vm-desktop: no ^C after the typed Ctrl+C" >&2
            exit 1
        fi
        # CUBIT_VM_LOAD=1: a busy guest stalled neither audio, the game's
        # frames, the desktop nor keys to the guest (check-vm-load.py).
        if [ "${CUBIT_VM_LOAD:-0}" = 1 ] &&
           ! python3 -I "$ROOT_DIR/tests/headless/check-vm-load.py" "$SERIAL_LOG"; then
            exit 1
        fi
        ;;
    vm-linux)
        required_markers="
guest: CuBit guest: Linux is up under the CuBit hypervisor (guest serial console)
guest: # uname -a
guest: CuBit guest: shell ready
guest: CuBit guest: spec_store_bypass: Mitigation: Speculative Store Bypass disabled via prctl
guest: CuBit guest: spectre_v2: Mitigation:
vm-linux: [1 vCPU] the guest reached its shell prompt PASS
vm-linux: [2 vCPU] the guest reached its shell prompt PASS
vm-linux: [2 vCPU] the guest started its AP with INIT and SIPI PASS
vm-linux: [2 vCPU] the guest reports 2 CPUs online PASS
vm-linux: [2 vCPU] INIT, SIPI and the AP's mode switch left the nested tables unchanged, its tag flushed PASS
guest: typed-42
guest: interrupted-5
vm-linux: [1 vCPU] keys typed one at a time reach the shell and each is echoed: typed-42 PASS
vm-linux: [1 vCPU] Ctrl+C ends a foreground sleep 100
vm-linux: [1 vCPU] a command longer than the receive FIFO arrives whole: interrupted-5 PASS
vm-linux: [2 vCPU] keys typed one at a time reach the shell and each is echoed: typed-42 PASS
vm-linux: [2 vCPU] Ctrl+C ends a foreground sleep 100
vm-linux: [2 vCPU] a command longer than the receive FIFO arrives whole: interrupted-5 PASS
vm-linux: done PASS
"
        if grep -a 'vm-linux: \[[12] vCPU\] Ctrl+C ends' "$SERIAL_LOG" | grep -qv ' PASS'; then
            echo "headless: vm-linux: Ctrl+C did not bring the prompt back" >&2
            exit 1
        fi
        grep -a 'vm-linux: \[\|guest: Linux\|guest: # \|guest: model name\|guest: CuBit\|guest: typed-\|guest: interrupted-' "$SERIAL_LOG" | grep -v ' exit [0-9]' | head -80 || true
        # The guest mitigates with the speculation controls the kernel
        # switches for it (docs/hypervisor.md 9.2): IBPB everywhere, and on
        # a host with AutoIBRS (QEMU's -cpu host passes it on) AutoIBRS, not
        # retpolines.
        if ! grep -aF 'guest: CuBit guest: spectre_v2:' "$SERIAL_LOG" | grep -qF 'IBPB: conditional'; then
            echo "headless: vm-linux: the guest's spectre_v2 line has no IBPB" >&2
            exit 1
        fi
        if grep -qw auto_ibrs /proc/cpuinfo &&
           ! grep -aF 'guest: CuBit guest: spectre_v2:' "$SERIAL_LOG" | grep -qF 'Enhanced / Automatic IBRS'; then
            echo "headless: vm-linux: the host has AutoIBRS but the guest does not use it" >&2
            exit 1
        fi
        ;;
    vm-hello)
        required_markers="
vm: SVM enabled (EFER.SVME, host save area); guests run as kernel objects
vm-hello: Create_VM gives a VM handle PASS
vm-hello: Add_RAM pins and maps 16 pages PASS
vm-hello: an overlapping Add_RAM is refused PASS
vm-hello: Add_RAM of memory the monitor does not own is refused PASS
vm-hello: Create_VCPU gives a vCPU handle PASS
vm-hello: guest serial: Hello from a CuBit guest
vm-hello: the guest's port output reaches the monitor PASS
vm-hello: HLT stays in the kernel until the deadline PASS
vm-hello: Add_RAM after the first run is refused PASS
vm-hello: a store outside guest RAM is a nested fault at its address 0000000000020000 PASS
vm-hello: VMMCALL exits to the monitor PASS
vm-hello: a guest spinning with interrupts off returns at the deadline PASS
vm-hello: an architectural reset leaves every nested-table leaf unchanged PASS
vm-hello: the guest's tag is flushed before it runs again PASS
vm-hello: a CPUID answered through Set_State advances past the instruction PASS
vm-hello: a SPEC_CTRL write of a reserved bit is #GP in the guest (the kernel decides) PASS
vm-hello: a SPEC_CTRL write the host allows is kept and its later writes pass through PASS
vm-hello: the host's SPEC_CTRL is back after the exit PASS
vm-hello: the guest reads back its SPEC_CTRL after exits and re-entries PASS
vm-hello: the host's SPEC_CTRL is unchanged after the guest ran with a different value PASS
vm-hello: the host's SPEC_CTRL is unchanged after the timed exits PASS
vm-hello: with writes passed through, the CPU refuses a reserved bit (#GP) and the value stays PASS
vm-hello: Set_State refuses a SPEC_CTRL bit the host lacks PASS
vm-hello: after a state round trip the guest runs on PASS
vm-hello: a guest shutdown (int3 with an empty IDT) stops the vCPU and returns Shutdown PASS
vm-hello: an external interrupt below 32 is refused PASS
vm-hello: #GP without its error code is refused PASS
vm-hello: an external interrupt 48 is accepted PASS
vm-hello: a second pending event is Busy PASS
vm-hello: Destroy PASS
vm-hello: the vCPU handle is stale after Destroy PASS
vm-hello: the VM handle is stale after Destroy PASS
vm-hello: a second VM, left for the reaper PASS
vm-hello: done PASS
"
        if grep -a 'vm-hello: ' "$SERIAL_LOG" | grep -qF 'FAIL'; then
            echo "headless: vm-hello checks failed" >&2
            exit 1
        fi
        ;;
    virt-report)
        required_markers="
virt: svm usable
"
        grep -F 'virt: ' "$SERIAL_LOG" || true
        if ! grep -qE '^.*virt: svm revision=[0-9]+ asids=[0-9]+ npt=yes nrip=yes' "$SERIAL_LOG"; then
            echo "headless: virt-report: no SVM feature line with npt and nrip" >&2
            exit 1
        fi
        ;;
    virt-report-none)
        required_markers="
virt: none: the CPU reports neither VMX nor SVM
"
        ;;
    tsc-sync|tsc-skew|tsc-skew-noadjust)
        grep -aE 'TSC(-PROBE)?: ' "$SERIAL_LOG" || true
        tsc_failed=0
        case "$TEST_NAME" in
            tsc-sync)
                required_markers="
TSC: CPU 0 TSC_ADJUST 0
TSC: CPU 1 agrees with CPU 0: max warp 0 ticks
TSC: CPU 2 agrees with CPU 0: max warp 0 ticks
TSC: CPU 3 agrees with CPU 0: max warp 0 ticks
TSC: 3 of 3 secondary CPUs agree with CPU 0; the clock is shared
Process.Loader: Loaded module procmgr.svc
"
                if grep -aqF 'TSC-PROBE: ' "$SERIAL_LOG"; then
                    echo "headless: tsc-sync: a TSC_SKEW_PROBE kernel (rebuild without it)" >&2
                    tsc_failed=1
                fi
                if grep -aqE 'TSC: CPU [0-9]+ TSC_ADJUST -?[0-9]+ ->' "$SERIAL_LOG"; then
                    echo "headless: tsc-sync: a TSC_ADJUST was rewritten on synchronized vCPUs" >&2
                    tsc_failed=1
                fi
                ;;
            tsc-skew)
                required_markers="
TSC: CPU 0 TSC_ADJUST 0
TSC-PROBE: CPU 1's counter moved 1073741824 ticks ahead: written
TSC: CPU 1 agrees with CPU 0: max warp 0 ticks
TSC: CPU 2 agrees with CPU 0: max warp 0 ticks
TSC: CPU 3 agrees with CPU 0: max warp 0 ticks
TSC: 3 of 3 secondary CPUs agree with CPU 0; the clock is shared
Process.Loader: Loaded module procmgr.svc
"
                # The probe moved CPU 1's TSC_ADJUST with its counter; the
                # step put it back to CPU 0's.
                if ! grep -aqE 'TSC: CPU 1 TSC_ADJUST [0-9]+ -> 0 \(CPU 0.s\)' "$SERIAL_LOG"; then
                    echo "headless: tsc-skew: CPU 1's TSC_ADJUST was not set back to CPU 0's" >&2
                    tsc_failed=1
                fi
                ;;
            tsc-skew-noadjust)
                required_markers="
TSC: no TSC_ADJUST; each CPU's counter is checked against CPU 0's
TSC-PROBE: CPU 1's counter moved 1073741824 ticks ahead: written
TSC: CPU 2 agrees with CPU 0: max warp 0 ticks
TSC: CPU 3 agrees with CPU 0: max warp 0 ticks
Process.Loader: Loaded module procmgr.svc
"
                # CPU 1 is ahead: CPU 0 sees the warp, about the probe's
                # 2**30 ticks (less the time between the probe's read and
                # its write, and a turn of the lock).
                if ! grep -aqE '^.*TSC: CPUs disagree by 10[0-9]{8} ticks after TSC_ADJUST; the clock is CPU 0.s alone' "$SERIAL_LOG"; then
                    echo "headless: tsc-skew-noadjust: no disagreement of about 2**30 ticks" >&2
                    tsc_failed=1
                fi
                if ! grep -aqE 'TSC: 2 of 3 secondary CPUs agree with CPU 0, 1 disagree by up to [0-9]+ ticks; the clock is CPU 0.s alone' "$SERIAL_LOG"; then
                    echo "headless: tsc-skew-noadjust: the clock was not withdrawn to CPU 0" >&2
                    tsc_failed=1
                fi
                ;;
        esac
        # The system's time goes on: CPU 0's 10 s report of each CPU's clock
        # against msTicks (Time.noteTSC) came at least three times, and no
        # CPU's counter fell outside the scheduling epoch.
        tsc_reports=$(grep -acF 'TSC: CPU 0 msTicks minus its own clock' "$SERIAL_LOG")
        if [ "$tsc_reports" -lt 3 ]; then
            echo "headless: $TEST_NAME: msTicks did not reach 30 s ($tsc_reports reports)" >&2
            tsc_failed=1
        fi
        if grep -aqF 'CLOCK: FAIL' "$SERIAL_LOG"; then
            echo "headless: $TEST_NAME: a CLOCK: FAIL line" >&2
            tsc_failed=1
        fi
        if [ "$TEST_NAME" != tsc-skew-noadjust ] && grep -aqF 'the clock is CPU 0' "$SERIAL_LOG"; then
            echo "headless: $TEST_NAME: the shared clock was withdrawn" >&2
            tsc_failed=1
        fi
        if [ "$tsc_failed" -ne 0 ]; then
            echo "headless: serial log: $SERIAL_LOG" >&2
            exit 1
        fi
        ;;
    cpu-refusal)
        required_markers="
CuBit needs an x86-64-v3 CPU; this one lacks: AVX2 FMA BMI1 BMI2 MOVBE F16C LZCNT OSXSAVE AVX XCR0.AVX. Halted.
"
        ;;
    avx)
        required_markers="
Process: user FP/SIMD state saved with XSAVEOPT
avx-check: all 16 YMM registers kept across 20000 yields PASS
"
        ;;
    clock-keeper)
        # Each check ran during its own probe stall of CPU 0's clock work;
        # the kernel says when another CPU took over the time and when CPU 0
        # came back (Time.clockTick).
        required_markers="
CLOCK-PROBE: CPU 0 runs no timer work for 800 ms
clock-keeper-check: clock publication present PASS
clock-keeper-check: a receive deadline ends on time while CPU 0 runs no timer work PASS
clock-keeper-check: a futex deadline ends on time while CPU 0 runs no timer work PASS
clock-keeper-check: msTicks advances while CPU 0 runs no timer work PASS
TEST: PASS clock-keeper
CLOCK: CPU 0 ran no timer work for
CLOCK: CPU 0 runs its timer work again after
"
        grep -aE 'clock-keeper-check: (receive|futex) deadline|clock-keeper-check: msTicks advanced|CLOCK(-PROBE)?: ' \
          "$SERIAL_LOG" || true
        if grep -aF 'clock-keeper-check: ' "$SERIAL_LOG" | grep -qF ' FAIL'; then
            echo "headless: clock-keeper: a deadline or msTicks stopped with CPU 0's timer work" >&2
            exit 1
        fi
        # Another CPU kept time throughout: the time never jumped (case B).
        if grep -aqF "CLOCK: the system's time jumped" "$SERIAL_LOG"; then
            echo "headless: clock-keeper: the time jumped with CPU 0 silent" >&2
            exit 1
        fi
        ;;
    clock)
        required_markers="
clock-check: clock publication present PASS
clock-check: 1000000 page reads are published and monotonic PASS
clock-check: 1000000 page millisecond reads are monotonic PASS
clock-check: page microseconds and milliseconds agree PASS
clock-check: the page is never behind the kernel's msTicks, and at most 10 ms ahead PASS
clock-check: a futex deadline from the page does not expire early PASS
clock-check: sleeping until a page microsecond does not return early PASS
TEST: PASS clock
clock-check: write to the clock page armed
"
        grep -F 'clock-check: cost ' "$SERIAL_LOG" || true
        # The write must fault at the page's address (0x5B00_0000_0000) and
        # retire the process; nothing may print after the write. Only the
        # kernel's own lines (the fault, then the reclaim) are in the
        # guest's order: clock-check's "armed" reaches serial through
        # logstore's mirror (ABI-002), later, so it is required present,
        # not before the fault.
        if grep -F 'clock-check: ' "$SERIAL_LOG" | grep -qF 'FAIL' ||
           ! grep -qF 'clock-check: write to the clock page armed' "$SERIAL_LOG" ||
           ! awk '/USER-MEMORY-FAULT: pid [0-9]+ address 100055558127616 kind=write-protection/ { fault = 1 }
                  fault && /reclaimProcess: stopped PID/ { found = 1 }
                  END { exit !found }' "$SERIAL_LOG"; then
            echo "headless: clock checks failed, or the page write was not refused" >&2
            exit 1
        fi
        ;;
    cpu-times)
        required_markers="
cpu-times-check: CPU times publication granted and present PASS
cpu-times-check: the counter rate is the clock publication's PASS
cpu-times-check: free memory is published and within RAM PASS
cpu-times-check: every started CPU is published PASS
cpu-times-check: every CPU's counters advance PASS
cpu-times-check: an idle machine is mostly idle PASS
cpu-times-check: a spinning thread drives one CPU's worth of busy time PASS
cpu-times-check: aggregate load shows the spinner PASS
TEST: PASS cpu-times
cpu-times-check: write to the CPU times page armed
"
        grep -F 'cpu-times-check: ' "$SERIAL_LOG" | grep -E ' (idle|spin) |cost ' || true
        # The page is read-only: the write faults at its address
        # (0x5B00_0000_5000) and retires the process; nothing prints after.
        if grep -F 'cpu-times-check: ' "$SERIAL_LOG" | grep -qF 'FAIL' ||
           ! awk '/cpu-times-check: write to the CPU times page armed/ { seen = 1 }
                  seen && /USER-MEMORY-FAULT: pid [0-9]+ address 100055558148096 kind=write-protection/ { fault = 1 }
                  fault && /reclaimProcess: stopped PID/ { found = 1 }
                  END { exit !found }' "$SERIAL_LOG"; then
            echo "headless: CPU times checks failed, or the page write was not refused" >&2
            exit 1
        fi
        ;;
    acpi-one-service)
        # One process holds the hardware and runs AML: Hardware, the memory
        # bridge (pool cache + PCI broker) and Fields come up inside module
        # startup from the copied FADT; module AML then completes for BOTH
        # tables, including the SSDT's pool-backed read, which the former
        # pair refused (STARTUP_REFUSED; laptop image 11's MBAR).
        # No acpi-backend.svc and no role-35 endpoint exist.
        # Events: q35 declares two general-purpose event methods, so the
        # event stage serves CONSUMERS; the EC scan must find no EC with no
        # node errors (image 27: unrelated _STA failures were fatal).
        expected_acpi_tables=8
        expected_acpi_modules=2
        if [ -n "${ACPI_CMOS_REACH_TABLE:-}" ]; then
            expected_acpi_tables=9
            expected_acpi_modules=3
        fi
        required_markers="
acpi: stage service-start ok one-service
acpi: stage hardware-config ok memory=
pool=1 pci=1
acpi: stage snapshot ok tables= $expected_acpi_tables bytes=
acpi: stage aml-startup ok FINISHED
acpi: stage hardware ok registers=
acpi: stage memory-bridge ok pool=1 cache=1 pci=1
completed= $expected_acpi_modules charged=
acpi: stage sleep-registers ok 
acpi: stage finish-startup ok setup=CLOSED
acpi: stage runtime ok LOCAL_IDLE
acpi: stage ec-scan status=NO_EC absent= 0 pruned= 0 node-errors= 0 relevance-links= 0 ecdt=FALSE
acpi: stage events ok CONSUMERS ec=NO_EC general= 2
acpi: stage registration ok STARTING
acpi: bring-up reached registration (4/4) hardware=LOCAL
acpi-metrics-check: metrics reply well formed PASS
acpi-metrics-check: phase complete PASS
acpi-metrics-check: tables captured= $expected_acpi_tables installed= $expected_acpi_tables
acpi-metrics-check: every expected table installed PASS
acpi-metrics-check: revision stable across reads PASS
TEST: PASS acpi-metrics
"
        # q35 still has no provisioned _REG regions (ACPI_Unprovisioned_Regions):
        # the device walk stops at the first OperationRegion and HID/power
        # discovery wait on it, exactly as before the merge. Any other failed
        # stage, a refused module (the pair's MBAR-style refusal), recovery,
        # heap exhaustion, exit or a backend/role-35 trace fails.
        if grep -aE '^acpi: stage [a-z-]+ fail' "$SERIAL_LOG" |
           grep -avE '^acpi: stage (devices fail DEVICES_INCOMPLETE regions=REGIONS_UNAVAILABLE errors= 0 fuel= 0|hid-discovery fail ( 1 search= 0 query= 0 |NOT_READY search=NOT_READY query=NOT_ATTEMPTED )|power-discovery fail ( 1 phase= 0 fuel= 0 |NOT_READY phase=LID_SAMPLING fuel= 0 ))' ||
           grep -aqE 'STARTUP_REFUSED|module-field-context|recovery=TRUE|Ada heap: cannot allocate|com\.cubit\.acpi(-backend)? pid[0-9]+ exited|acpi-metrics-check: .* FAIL' "$SERIAL_LOG"; then
            echo "headless: one-service acpi.svc bring-up or metrics check failed" >&2
            exit 1
        fi
        ;;
    acpi-sci)
        # The SCI reaches the one acpi.svc and is acknowledged: two presses,
        # two SCIs, each logged, nothing lost and no storm.
        required_markers="
devmgr: ACPI SCI is line 41, GSI 9 level/active-high (MADT override)
procmgr: ACPI SCI line 41 (GSI 9, level/active-high, MADT override) routed to acpi.svc
acpi: bring-up reached registration (4/4) hardware=LOCAL
acpi: SCI armed line=41 pm1a=0x600 gpe0=0x620+8
 acpi_mode=ON acpi_mode_switch=
 fixed=[POWER_BUTTON]
acpi: SCI 1 fixed=[PWRBTN ] gpe=[]
acpi: SCI 2 fixed=[PWRBTN ] gpe=[]
acpi: power-button presses=
"
        # The event domain (queue 24) publishes one copied power-button fact
        # per delivered press, once the service is quiet again.
        presses=$(grep -acE '^acpi: power-button presses= *[0-9]+' "$SERIAL_LOG")
        interrupts=$(grep -acE '^acpi: SCI [0-9]+ fixed=' "$SERIAL_LOG")
        if [ "$presses" -ne 2 ] || [ "$interrupts" -ne 2 ] ||
           grep -aqE 'acpi: SCI unavailable|acpi: SCI acknowledgement refused|ACPI-mode switch failed|ACPI SCI refused|ENABLE_IRQ: refused|has no consumer' "$SERIAL_LOG" ||
           grep -aqE 'com\.cubit\.acpi pid[0-9]+ exited|Ada heap: cannot allocate' "$SERIAL_LOG"; then
            echo "headless: acpi.svc SCI delivery failed: presses=$presses interrupts=$interrupts" >&2
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
    acpi-hardware)
        required_markers="
acpi-hardware-check: grant has I/O windows PASS
acpi-hardware-check: grant has memory windows PASS
acpi-hardware-check: windows sorted and disjoint PASS
acpi-hardware-check: window 1 handle covers exactly its range PASS
acpi-hardware-check: window 1 read-write without grant PASS
acpi-hardware-check: window 1 refuses the next port PASS
acpi-hardware-check: FADT read PASS
acpi-hardware-check: q35 has no ECDT PASS
acpi-hardware-check: windows are exactly the FADT fixed blocks PASS
acpi-hardware-check: memory window 1 handle covers exactly its pages PASS
acpi-hardware-check: memory window 1 is firmware memory PASS
acpi-hardware-check: memory window 1 kind and cache mode from its kernel tag PASS
acpi-hardware-check: FACS window is write-back (an NVS run, or the FACS page alone) PASS
acpi-hardware-check: memory window 1 read-write without grant PASS
acpi-hardware-check: memory window 1 cannot be copied on PASS
acpi-hardware-check: memory window 1 refuses the next page PASS
acpi-hardware-check: memory window 1 refuses the page before PASS
acpi-hardware-check: FACS page inside a memory window PASS
acpi-hardware-check: FACS page maps through its window PASS
acpi-hardware-check: FACS signature read through its window PASS
claimed, closed to device memory
procmgr: ACPI memory window 1 installed for acpi-hardware-check.app
acpi-hardware-check: PM timer inside a window PASS
acpi-hardware-check: PM timer advances through its window PASS
devmgr: ACPI SCI is line 41, GSI 9 level/active-high (MADT override)
procmgr: ACPI SCI line 41 (GSI 9, level/active-high, MADT override) routed to acpi-hardware-check.app
acpi-hardware-check: SCI line granted PASS
acpi-hardware-check: SCI resolves from this program's own FADT and MADT PASS
acpi-hardware-check: q35 SCI is the MADT override's GSI 9, level, active high PASS
acpi-hardware-check: SCI handle names exactly the resolved line PASS
acpi-hardware-check: SCI handle is read-only without grant PASS
acpi-hardware-check: acknowledging no line handle is refused PASS
acpi-hardware-check: acknowledging another line with the SCI handle is refused PASS
acpi-hardware-check: PM1 event block inside a window PASS
acpi-hardware-check: power button event enabled through the PM1 window PASS
acpi-hardware-check: SCI armed
acpi-hardware-check: SCI delivered for the power button PASS
acpi-hardware-check: PWRBTN_STS set PASS
acpi-hardware-check: SCI held masked until acknowledged (no storm) PASS
acpi-hardware-check: first acknowledgement accepted after one delivery PASS
acpi-hardware-check: uncleared SCI delivered again after acknowledgement (nothing lost) PASS
acpi-hardware-check: PWRBTN_STS cleared PASS
acpi-hardware-check: SCI second press wanted
acpi-hardware-check: second press latched in PWRBTN_STS PASS
acpi-hardware-check: second press not delivered while the line is held masked PASS
acpi-hardware-check: acknowledged with the second press pending, two deliveries so far PASS
acpi-hardware-check: second SCI delivered at the acknowledgement (not lost) PASS
acpi-hardware-check: acknowledged after clearing, three deliveries in all PASS
acpi-hardware-check: no SCI after clearing and acknowledgement PASS
acpi-hardware-check: the line stayed quiet: still three deliveries PASS
acpi-hardware-check: SCI done
TEST: PASS acpi-hardware
procmgr: ACPI hardware refused: acpi-hardware-denied.app is not approved by the startup plan
procmgr: init spawn failed: acpi-hardware-denied.app
"
        # The unapproved image never runs; no check fails; no port I/O
        # outside a window other than the deliberate next-port probes.
        if grep -aF 'acpi-hardware-check: ' "$SERIAL_LOG" | grep -qF ' FAIL' ||
           grep -aqF 'TEST: FAIL acpi-' "$SERIAL_LOG" ||
           grep -aqF 'still open to device memory' "$SERIAL_LOG" ||
           grep -aqF 'installed for acpi-hardware-denied.app' "$SERIAL_LOG" ||
           grep -aqF 'routed to acpi-hardware-denied.app' "$SERIAL_LOG" ||
           grep -aqF 'ACPI SCI refused' "$SERIAL_LOG" ||
           grep -aqF 'ENABLE_IRQ: refused' "$SERIAL_LOG" ||
           [ "$(grep -acF 'acpi-hardware-check: starting' "$SERIAL_LOG")" -ne 1 ]; then
            echo "headless: ACPI hardware checks failed, or the unapproved image ran" >&2
            exit 1
        fi
        # The PCI configuration window is opt-in: no entry here approves it.
        if grep -aqF '(PCI configuration, read-only) installed' "$SERIAL_LOG" ||
           grep -aqF 'ACPI PCI configuration approved' "$SERIAL_LOG"; then
            echo "headless: a PCI configuration window reached an unapproved program" >&2
            exit 1
        fi
        ;;
    acpi-pci-config)
        required_markers="
procmgr: ACPI memory window 1 installed for acpi-pci-unapproved.app
acpi-pci-check: no PCI configuration startup pregrant PASS
acpi-pci-check: unapproved broker request refused without a capability PASS
acpi-pci-check: unapproved reply cleanup complete PASS
acpi-pci-check: bus 0 acquired from the PCI broker PASS
acpi-pci-check: windows sorted and disjoint PASS
acpi-pci-check: the rest of the grant is unchanged (I/O and write-back firmware windows) PASS
acpi-pci-check: handle covers exactly bus 0 (1 MiB) PASS
acpi-pci-check: window is the MCFG's segment 0 bus 0 slice PASS
acpi-pci-check: kind and cache mode from its kernel tag (PCI_Config, uncached) PASS
acpi-pci-check: read-only without grant PASS
acpi-pci-check: cannot be copied on PASS
acpi-pci-check: writable mapping refused PASS
acpi-pci-check: bus 1 (the page past the window) refused PASS
acpi-pci-check: the page before the window refused PASS
acpi-pci-check: host bridge function 00:00.0 maps read-only PASS
acpi-pci-check: host bridge vendor ID read through the window PASS
TEST: PASS acpi-pci-config
acpi-pci-check: attempting forbidden write at 0000600000000000
"
        # One run of each image; nothing fails; the unapproved image never
        # gets the window; devmgr closed the slice to device memory; the
        # forbidden write is a write-protection fault of acpi-pci-check.app
        # at the mapped page; the host bridge IDs read through the window
        # are the ones devmgr enumerated at 00:00.0 through 0xCF8/0xCFC.
        if grep -aF 'acpi-pci-check: ' "$SERIAL_LOG" | grep -qF ' FAIL' ||
           grep -aqF 'TEST: FAIL acpi-pci-config' "$SERIAL_LOG" ||
           grep -aqF 'still open to device memory' "$SERIAL_LOG" ||
           grep -aqF '(PCI configuration, read-only) installed for acpi-pci-unapproved.app' "$SERIAL_LOG" ||
           grep -aqF 'ACPI PCI configuration approved for acpi-pci-unapproved.app' "$SERIAL_LOG" ||
           [ "$(grep -acF 'acpi-pci-check: starting' "$SERIAL_LOG")" -ne 2 ] ||
           [ "$(grep -acF 'TEST: PASS acpi-pci-config' "$SERIAL_LOG")" -ne 1 ] ||
           [ "$(grep -acF 'acpi-pci-check: unapproved broker request refused without a capability PASS' "$SERIAL_LOG")" -ne 1 ] ||
           [ "$(grep -acF 'acpi-pci-check: no PCI configuration startup pregrant PASS' "$SERIAL_LOG")" -ne 2 ]; then
            echo "headless: ACPI PCI configuration checks failed, or the unapproved image got the window" >&2
            exit 1
        fi
        if ! python3 -I "$ROOT_DIR/tests/acpi-headless-checks/check-launch.py" pci-config "$SERIAL_LOG"
        then
            echo "headless: PCI configuration window read/fault check failed" >&2
            exit 1
        fi
        ;;
    acpi-firmware-data)
        required_markers="
(firmware data)
devmgr: roots:
 BAR2 64-bit prefetchable
devmgr: PCI memory claimed for devices:
procmgr: ACPI firmware data approved for acpi-data-check.app
acpi-data-check: no firmware-data endpoint
acpi-data-check: unapproved request refused (NOT_APPROVED)
devmgr: firmware data refused: no acpi-firmware-data approval
acpi-data-check: a request through device-manager refused (not the badge) PASS
acpi-data-check: the reply names the envelope PASS
acpi-data-check: the grant is carried PASS
acpi-data-check: a reserved page granted as firmware data PASS
acpi-data-check: the handle covers exactly the page PASS
acpi-data-check: its kernel tag is firmware data (uncached) PASS
acpi-data-check: read-write without grant PASS
acpi-data-check: cannot be copied on PASS
acpi-data-check: the page past the envelope refused PASS
acpi-data-check: maps read-write (uncached) PASS
acpi-data-check: reads the page PASS
acpi-data-check: the same request id is answered the same PASS
devmgr: firmware data request
acpi-data-check: an ECAM page of bus 0 refused (Overlaps_PCI_Config) PASS
acpi-data-check: an ECAM page of bus 1 refused (Overlaps_PCI_Config) PASS
acpi-data-check: a RAM page refused (Not_Firmware_Class) PASS
acpi-data-check: PCI broker endpoint available for BAR probes PASS
acpi-data-check: bus 0 acquired from the PCI broker for BAR probes PASS
acpi-data-check: PCI bus window maps read-only for BAR probes PASS
acpi-data-check: a device BAR page refused (Overlaps_Device) PASS
acpi-data-check: a driverless BAR's tail refused (Overlaps_Device) PASS
acpi-data-check: the FACS page refused (Overlaps_Control) PASS
acpi-data-check: a request without pages refused (Malformed) PASS
TEST: PASS acpi-firmware-data
"
        # One run of each image; nothing fails; the unapproved image never
        # gets the endpoint; the granted page was claimed for ACPI.
        if grep -aF 'acpi-data-check: ' "$SERIAL_LOG" | grep -qF ' FAIL' ||
           grep -aqF 'TEST: FAIL acpi-firmware-data' "$SERIAL_LOG" ||
           grep -aqF 'ACPI firmware data approved for acpi-data-unapproved.app' "$SERIAL_LOG" ||
           grep -aqF 'CAPACITY' "$SERIAL_LOG" ||
           [ "$(grep -acF 'acpi-data-check: starting' "$SERIAL_LOG")" -ne 2 ] ||
           [ "$(grep -acF 'TEST: PASS acpi-firmware-data' "$SERIAL_LOG")" -ne 1 ] ||
           [ "$(grep -acF 'acpi-data-check: no firmware-data endpoint' "$SERIAL_LOG")" -ne 1 ]; then
            echo "headless: ACPI firmware-data checks failed, or the unapproved image got the endpoint" >&2
            exit 1
        fi
        ;;
    gpio-launch)
        # devmgr admits the test controller's two spans (P2SB discovery ran
        # on q35 and did not find), answers before any launch, queues one
        # launch, binds gpio-launch-check.drv by its ACPI_ID, gives window 1
        # = span 1 (clamped to the 64 KiB span) and window 2 = span 0 (one
        # page, read-only) and routes the observed parent line (GSI 10,
        # level/active-low) to its "parent" binding; the driver sees exactly
        # that, acknowledges the line, maps both, and the kernel refuses
        # every page outside them.
        required_markers="
devmgr: sideband: P2SB outcome=
devmgr: platform pool: room for 4 GPIO bootstrap spans
gpio-bootstrap-check: the bootstrap payload is lent read-only PASS
devmgr: platform pool member 0x20010000 +0x10000 CHIPSET_SHARED owner=GPIO bootstrap CBIT1057 span 1
devmgr: platform pool member 0x20000000 +0x10000 CHIPSET_SHARED owner=GPIO bootstrap CBIT1057 span 2
gpio-bootstrap-check: devmgr admits the test controller's two spans PASS
devmgr: acpi device CBIT1057 \_SB_.GPI0 admitted with 2 spans (epoch 1); its driver launch waits for consumer initialization to complete
gpio-bootstrap-check: the same request id gets the same answer PASS
gpio-bootstrap-check: a second admission is ALREADY_ADMITTED PASS
logstore: authorized typed diagnostics ready
gpio-bootstrap-check: a four-word frame is MALFORMED PASS
gpio-bootstrap-check: a stray fourth word is MALFORMED PASS
gpio-bootstrap-check: reserved tag bits are MALFORMED PASS
gpio-bootstrap-check: a frame accepting a capability is MALFORMED PASS
 MALFORMED epoch 0 status 1 COMPLETED; 0 driver launches queued
gpio-bootstrap-check: COMPLETED for epoch 0 is MALFORMED PASS
devmgr: consumer initialization complete: request 10 from
 MALFORMED epoch 1 status 153; 0 driver launches queued
gpio-bootstrap-check: a completion with an unknown status is MALFORMED PASS
 RECEIVED epoch 2 status 3 INCOMPLETE; 0 driver launches queued
gpio-bootstrap-check: INCOMPLETE is RECEIVED and launches nothing PASS
 CONFLICTING epoch 2 status 1 COMPLETED (declared INCOMPLETE); 0 driver launches queued
gpio-bootstrap-check: COMPLETED after INCOMPLETE for one epoch is CONFLICTING PASS
 RECEIVED epoch 3 status 1 COMPLETED; 0 driver launches queued
gpio-bootstrap-check: COMPLETED for an epoch nothing was observed in is RECEIVED, releases nothing PASS
gpio-bootstrap-check: sending consumer initialization complete
devmgr: acpi device CBIT1057 \_SB_.GPI0: its driver launch is queued (observed in epoch 1)
 RECEIVED epoch 1 status 1 COMPLETED; 1 driver launches queued
gpio-bootstrap-check: consumer initialization COMPLETED is RECEIVED and reports one launch released PASS
gpio-bootstrap-check: the same completion again gets the same receipt and the same released count PASS
gpio-bootstrap-check: a four-word offer frame is MALFORMED PASS
gpio-bootstrap-check: a stray fourth offer word is MALFORMED PASS
gpio-bootstrap-check: reserved offer tag bits are MALFORMED PASS
gpio-bootstrap-check: an offer frame accepting a capability is MALFORMED PASS
gpio-bootstrap-check: offer request id 0 is MALFORMED PASS
gpio-bootstrap-check: offer version 2 is MALFORMED at byte 0 PASS
gpio-bootstrap-check: a wrong length field is MALFORMED at byte 4 PASS
gpio-bootstrap-check: another compatible ID is MALFORMED at byte 160 PASS
gpio-bootstrap-check: an offer in epoch 0 is MALFORMED at byte 8 PASS
gpio-bootstrap-check: an offer whose grant is not the sender's is COPY_FAILED PASS
devmgr: hid offer: request 31 from
 UNKNOWN_CONTROLLER detail 0 epoch 1 device \_SB_.PCI0.I2C1.TPD0 i2c \_SB_.PCI0.I2C1 pci unresolved gpio CBIT1057 \_SB_.GPI0 pin 76
gpio-bootstrap-check: an unresolved _ADR is UNKNOWN_CONTROLLER PASS
gpio-bootstrap-check: an absent PCI function is UNKNOWN_CONTROLLER PASS
 CONTROLLER_MISMATCH detail 0 epoch 1 device \_SB_.PCI0.I2C1.TPD0
gpio-bootstrap-check: a function no driver binds is CONTROLLER_MISMATCH PASS
gpio-bootstrap-check: another GPIO controller is GPIO_NOT_ADMITTED PASS
gpio-bootstrap-check: an epoch with no admitted GPIO controller is GPIO_NOT_ADMITTED PASS
gpio-bootstrap-check: offering the touchpad
 QUEUED detail 0 epoch 1 device \_SB_.PCI0.I2C1.TPD0 i2c \_SB_.PCI0.I2C1 pci 0x0:0x0:0x150000 (00:15.0
i2c-badge 0x4932101500061A80
gpio-bootstrap-check: the touchpad offer is QUEUED at position 1 PASS
gpio-bootstrap-check: the same offer again gets the same answer PASS
gpio-bootstrap-check: a second offer of the touchpad is DUPLICATE_DEVICE PASS
gpio-bootstrap-check: a recorded request id keeps its answer (never decided twice) PASS
gpio-bootstrap-check: admitted, released and offered; devmgr launches the driver
devmgr: match acpi CBIT1057 \_SB_.GPI0 -> gpio-launch-check.drv (ACPI_ID CBIT1057)
devmgr: gpio-launch-check.drv window 1 = span 2 0x20000000 +0x10000 read-write
devmgr: gpio-launch-check.drv window 2 = span 1 0x20010000 +0x1000 read-only
devmgr: gpio-launch-check.drv parent line routed GSI 10 vector 42 level/active-low shared
devmgr: gpio-launch-check.drv launched for acpi CBIT1057 \_SB_.GPI0 with 2 of 2 admitted spans (ACPI launch)
gpio-launch-check: stage identify ok hid=CBIT1057 path=\_SB_.GPI0 uid-type= 1 uid=0x0000000000000000 epoch=0x0000000000000001 windows= 2 window 1=0x0000000020000000+0x0000000000010000 window 2=0x0000000020010000+0x0000000000001000
gpio-launch-check: stage parent-line ok binding=TRUE present=TRUE gsi= 10 vector= 42 ack=TRUE
gpio-launch-check: stage map ok
gpio-launch-check: stage bounded ok past-second refused=TRUE past-first refused=TRUE other-span refused=TRUE
gpio-launch-check: bring-up complete
devmgr: acpi driver ready for CBIT1057 \_SB_.GPI0
devmgr: hid offer \_SB_.PCI0.I2C1.TPD0 (request 30): the GPIO driver and the I2C controller's driver (generic-check.drv at 00:15.0
i2c-target badge 0x4932101500061A80 gpio-input badge 0x475010000001004C
devmgr: hid offer \_SB_.PCI0.I2C1.TPD0 (request 30): starting hid-launch-check.drv held through procmgr's Start_Child (class PNP0C50, fact descriptor_register = 32, i2c on pid
procmgr: parameters hid-launch-check.drv: (HID_Parameters descriptor_register => 32 x => (HID_Axis numerator => 1 denominator => 1 direction => HID_Direction.As_Reported)
procmgr: held PID
procmgr: start_child hid-launch-check.drv pid
 supervised by devmgr pid
 (restart temporary, stop grace 5000 ms, class PNP0C50: 1 facts, held for its supervisor)
devmgr: hid offer \_SB_.PCI0.I2C1.TPD0 (request 30): hid-launch-check.drv started pid
 (Start_Child, held), supervised by devmgr
gpio-launch-check: sink admission ok status=COMPLETED badge=0x475010000001004C owner=
gpio-launch-check: event through the sink sent
devmgr: hid offer \_SB_.PCI0.I2C1.TPD0 (request 30): GPIO input 0x475010000001004C admitted at the GPIO driver
 released by devmgr
devmgr: hid offer \_SB_.PCI0.I2C1.TPD0 (request 30): hid driver pid
 released by procmgr; it runs with its GPIO input live
hid-launch-check: parameters descriptor_register=0x0000000000000020 x=1/1 As_Reported y=1/1 As_Reported confidence=Require_Confident buttons=Primary_Button,Secondary_Button,Middle_Button contact_limit=5 binding_budget=65536 timing=10000/1000/250/1000/1
hid-launch-check: i2c badge 0x4932101500061A80, gpio badge 0x475010000001004C
generic-check: probed at 00:15.0 badge=0x4932101500061A80
hid-launch-check: i2c reaches the driver at 00:15.0 with badge 0x4932101500061A80
hid-launch-check: gpio event through the admitted sink from the GPIO driver, badge 0x475010000001004C
devmgr: hid driver ready for \_SB_.PCI0.I2C1.TPD0
devmgr: child hid-launch-check (
) exited code 0
procmgr: supervised child com.cubit.test.hid-launch-check pid
"
        if grep -aF 'gpio-bootstrap-check: ' "$SERIAL_LOG" | grep -qF ' FAIL' ||
           grep -aqF 'TEST: FAIL gpio-launch' "$SERIAL_LOG" ||
           grep -aF 'gpio-launch-check: stage' "$SERIAL_LOG" | grep -qF ' fail ' ||
           [ "$(grep -acF 'devmgr: gpio-launch-check.drv launched for acpi' "$SERIAL_LOG")" -ne 1 ] ||
           [ "$(grep -acF 'MALFORMED frame' "$SERIAL_LOG")" -ne 4 ] ||
           [ "$(grep -acF ' driver launches queued' "$SERIAL_LOG")" -ne 10 ] ||
           [ "$(grep -acF 'devmgr: hid offer: request' "$SERIAL_LOG")" -ne 17 ] ||
           [ "$(grep -acF ' QUEUED detail 0 epoch 1' "$SERIAL_LOG")" -ne 1 ] ||
           [ "$(grep -acF 'devmgr: hid offer \_SB_.PCI0.I2C1.TPD0 (request 30): the GPIO driver and' "$SERIAL_LOG")" -ne 1 ] ||
           [ "$(grep -acF 'procmgr: start_child hid-launch-check.drv pid' "$SERIAL_LOG")" -ne 1 ] ||
           [ "$(grep -acF 'hid-launch-check: parameters ' "$SERIAL_LOG")" -ne 1 ] ||
           [ "$(grep -acF 'gpio-launch-check: sink admission ok' "$SERIAL_LOG")" -ne 1 ] ||
           [ "$(grep -acF 'devmgr: generic-check.drv launched for' "$SERIAL_LOG")" -ne 2 ] ||
           grep -aqF 'generic-check: probed at 00:15.1' "$SERIAL_LOG" ||
           grep -aqF 'gpio-launch-check: sink admission fail' "$SERIAL_LOG" ||
           grep -aqF 'hid-launch-check: no gpio event' "$SERIAL_LOG" ||
           grep -aqF 'procmgr: release of pid' "$SERIAL_LOG" ||
           grep -aqF 'procmgr: start_child from devmgr' "$SERIAL_LOG"; then
            echo "headless: gpio-launch: a check failed, or the driver was not launched exactly once" >&2
            exit 1
        fi
        # No launch before Consumer_Initialization_Complete, although
        # logstore (the generic launch's precondition) ran before it.
        if ! awk '/logstore: authorized typed diagnostics ready/ && !ready { ready = NR }
                  /gpio-bootstrap-check: sending consumer initialization complete/ && !sent { sent = NR }
                  /launch is queued \(observed in epoch/ && !queued { queued = NR }
                  /devmgr: match acpi CBIT1057/ && !matched { matched = NR }
                  /devmgr: gpio-launch-check.drv launched for acpi/ && !launched { launched = NR }
                  END { exit !(ready && sent && queued && matched && launched &&
                               ready < sent && sent < queued && queued < matched && matched < launched) }' \
             "$SERIAL_LOG"; then
            echo "headless: gpio-launch: the driver started before consumer initialization completed" >&2
            exit 1
        fi
        # The HID offer is answered QUEUED before anything for it happens;
        # its deferred step runs only after the GPIO driver registered.
        if ! awk '/ QUEUED detail 0 epoch 1 device/ && !queued { queued = NR }
                  /devmgr: acpi driver ready for CBIT1057/ && !ready { ready = NR }
                  /devmgr: hid offer .*TPD0 \(request 30\): the GPIO driver and/ && !step { step = NR }
                  END { exit !(queued && ready && step && queued < step && ready < step) }' \
             "$SERIAL_LOG"; then
            echo "headless: gpio-launch: the HID offer's launch step ran before the GPIO driver registered" >&2
            exit 1
        fi
        # Verify the held child's sink ownership before its first output.
        # ABI-002 routes procmgr's narration through logstore; the release
        # line can reach serial after the resumed child's direct output.
        if ! python3 -I "$ROOT_DIR/tests/acpi-headless-checks/check-launch.py" gpio-launch "$SERIAL_LOG"; then
            echo "headless: gpio-launch: held child/sink admission evidence failed" >&2
            exit 1
        fi
        ;;
    platform-pool)
        # devmgr builds the pool from its own discovery (the test profile's
        # register block), declaring its sizes first; the stand-in maps it,
        # plain loads and stores work, its mapping records grow, the
        # driver's later bind revokes the device-BAR mappings (kernel log),
        # the same request is then refused CLAIMED naming the driver, the
        # shared community stays mapped and admitted (kernel log "shared"),
        # and a touch of the revoked mapping faults.
        required_markers="
platform-pool: declared members=
devmgr: platform pool declared:
devmgr: platform pool member
DEVICE_BAR owner=test profile: QEMU edu BAR0 for MCHBAR
CHIPSET_SHARED owner=test profile: edu BAR0 for a GPIO community
DEVICE_BAR owner=test profile: edu BAR0 for the sideband window
platform-pool: member phys=
platform-pool-check: devmgr grants the pool PASS
platform-pool-check: the bootstrap payload is lent read-only PASS
platform-pool-check: gpio bootstrap: a wrong version is MALFORMED at its byte PASS
platform-pool-check: gpio bootstrap: no P2SB discovery here: NO_DISCOVERY_FAILURE PASS
platform-pool-check: gpio bootstrap: the same request id gets the same answer PASS
devmgr: gpio bootstrap: request 2 from
platform-pool-check: the pool capability is carried PASS
platform-pool-check: its kernel tag is the platform pool PASS
platform-pool-check: read-write without grant PASS
platform-pool-check: cannot be copied on PASS
platform-pool-check: bus 0 acquired from the PCI broker PASS
platform-pool-check: edu found PASS
platform-pool-check: edu's register page maps through the pool PASS
platform-pool: mapped phys=
platform-pool-check: a plain load reads the register PASS
platform-pool-check: a plain store reaches the register PASS
platform-pool-check: two pages at MCHBAR+0x7000 map (image-10 MBAR witness) PASS
platform-pool-check: a request across the MCHBAR block's end refused (OUTSIDE_POOL) PASS
platform-pool-check: a GPIO community page maps through the pool PASS
platform-pool-check: a sideband window page between members maps PASS
platform-pool-check: 300 mappings of one community page: the kernel's records grow PASS
platform-pool-check: a request across the community's start refused (OUTSIDE_POOL) PASS
platform-pool-check: the page past the member refused (OUTSIDE_POOL) PASS
platform-pool-check: a RAM page refused (NOT_COVERED) PASS
platform-pool-check: PCI configuration of bus 1 refused (NOT_COVERED) PASS
devmgr: generic-check.drv launched for
platform-pool: revoked phys=
platform-pool: shared phys=
kind=CHIPSET_SHARED
platform-pool: refused phys=
why=CLAIMED claimant=
platform-pool-check: a bound driver's BAR is refused, naming both claimants (CLAIMED) PASS
platform-pool-check: the shared community stays mapped after the bind PASS
platform-pool-check: the community under the bound driver is still admitted (shared) PASS
shared-with=
TEST: PASS platform-pool
platform-pool-check: touching the revoked mapping
 address 105553116266496 kind=unmapped
"
        if grep -aF 'platform-pool-check: ' "$SERIAL_LOG" | grep -qF ' FAIL' ||
           grep -aqF 'TEST: FAIL platform-pool' "$SERIAL_LOG" ||
           grep -aqF 'platform-pool-check: revoked mapping still readable' "$SERIAL_LOG" ||
           grep -aqF 'why=CAPACITY' "$SERIAL_LOG" ||
           grep -aqF 'MAPPING_CAPACITY' "$SERIAL_LOG" ||
           [ "$(grep -acF 'platform-pool: shared phys=' "$SERIAL_LOG")" -ne 1 ] ||
           [ "$(grep -acF 'TEST: PASS platform-pool' "$SERIAL_LOG")" -ne 1 ]; then
            echo "headless: platform-pool checks failed, or the revoked mapping stayed readable" >&2
            exit 1
        fi
        ;;
    mailbox-self)
        required_markers="
MAILBOX-SELF: RESUME of itself refused PASS
MAILBOX-SELF: INSTALL_LAUNCH_ARGUMENTS on itself refused PASS
MAILBOX-SELF: ENABLE_IRQ to itself refused PASS
MAILBOX-SELF: done PASS
"
        ;;
    mailbox-mutual)
        required_markers="
MAILBOX-MUTUAL: done PASS
"
        ;;
    futex-share)
        required_markers="
futex-share: two pages granted PASS
futex-share: server acquired them PASS
futex-share: unaligned shared word refused PASS
futex-share: shared word past the grant refused PASS
futex-share: hand-off with a refused wake does not wait PASS
futex-share: ping-pong of 20000 rounds, handoff, no wake lost PASS
futex-share: ping-pong of 20000 rounds, separate, no wake lost PASS
futex-share: eight waiters woken oldest first PASS
futex-share: owner's waiter woken as revoked PASS
futex-share: grantee's waiter woken as revoked PASS
futex-share: wait on an ended grant refused PASS
futex-share: wake on an ended grant refused PASS
futex-share: bystanders keep their share beside a ping-pong PASS
futex-share: ALL PASS
futex-share-outsider: wake without the grant refused PASS
futex-share-outsider: wait without the grant refused PASS
futex-share: a peer wake through a grant without the notify flag refused PASS
futex-share: a peer wake ends an activity wait PASS
futex-share: 2000 notified grants taken and given back PASS
futex-share: an idle activity loop sleeps after grant-return notices PASS
futex-share-outsider: peer wake without being a party refused PASS
futex-share-outsider: ALL PASS
msr-safe: an absent MSR faulted and was recovered
"
        if grep -aE 'futex-share(-outsider)?: ' "$SERIAL_LOG" | grep -qF ' FAIL' ||
           grep -aqF 'futex-share-server: pong FAIL' "$SERIAL_LOG" ||
           grep -aqF 'futex-share-server: peer wake FAIL' "$SERIAL_LOG"; then
            echo "headless: futex-share checks failed" >&2
            exit 1
        fi
        ;;
    debug-engine)
        required_markers="
debug-engine: attach to debug-demo PASS
debug-engine: attach is cooperative PASS
debug-engine: two observed threads PASS
debug-engine: pause: both threads parked at a checkpoint PASS
debug-engine: frame 0 is the stub's checkpoint PASS
debug-engine: Demo_World.Advance is on the stack in demo_world.adb PASS
debug-engine: local Step of Demo_World.Advance has a value PASS
debug-engine: Demo_World.Insert is on the stack in demo_world.adb PASS
debug-engine: local Depth of Demo_World.Insert has a value PASS
debug-engine: The_World read live PASS
debug-engine: graph holds the job list PASS
debug-engine: graph holds the sensor tree PASS
debug-engine: source demo_world.adb from @system/Debug/Source PASS
debug-engine: The_World.Generation moved while running PASS
debug-engine: Generation is marked changed since the last read PASS
debug-engine: a shared page cannot be acquired for writing PASS
debug-engine: a shared page can be acquired for reading PASS
debug-engine: ALL PASS
"
        if grep -a 'debug-engine: ' "$SERIAL_LOG" | grep -qF ' FAIL'; then
            echo "headless: debug-engine checks failed" >&2
            exit 1
        fi
        ;;
    debug-engine-rs)
        required_markers="
debug-engine-rs: attach to debug-demo-rs PASS
debug-engine-rs: the target is the Rust demo PASS
debug-engine-rs: two observed Rust threads, jobs and sensors PASS
debug-engine-rs: pause: both Rust threads parked at a checkpoint PASS
debug-engine-rs: frame 0 is the Rust stub's checkpoint PASS
debug-engine-rs: debug_demo_rs::advance is on the stack in main.rs PASS
debug-engine-rs: queue is a Vec<Job> of its jobs PASS
debug-engine-rs: graph of queue: the Vec and its Job nodes PASS
debug-engine-rs: debug_demo_rs::insert is on the stack in main.rs PASS
debug-engine-rs: tree is Some(Box<Sensor>) (a niche enum) PASS
debug-engine-rs: graph holds the sensor tree PASS
debug-engine-rs: the Rust stub's pages cannot be acquired for writing PASS
debug-engine-rs: debug_demo_rs::GENERATION moved while running PASS
debug-engine-rs: ALL PASS
"
        if grep -a 'debug-engine-rs: ' "$SERIAL_LOG" | grep -qF ' FAIL'; then
            echo "headless: debug-engine-rs checks failed" >&2
            exit 1
        fi
        ;;
    tlb-tags)
        required_markers="
Address-space tags (PCID)
tlb-check: every life checked its fresh pages PASS
tlb-check: every revoke life faulted on its revoked grant PASS
with Buffer at the same address PASS
tlb-check: ALL PASS
process killed: tlb-probe.app
"
        if grep -aF 'tlb-check: ' "$SERIAL_LOG" | grep -qF ' FAIL' ||
           grep -aqF 'tlb-probe: FAIL' "$SERIAL_LOG" ||
           [ "$(grep -acF 'process killed: tlb-probe.app' "$SERIAL_LOG")" -ne 6 ]; then
            echo "headless: tlb-tags checks failed" >&2
            exit 1
        fi
        ;;
    wait)
        required_markers="
wait-check: Now on requests, empty PASS
wait-check: Now on events, empty PASS
wait-check: Now on messages, empty PASS
wait-check: Now on completions, empty PASS
wait-check: Now on readiness, nothing ready PASS
wait-check: Now on a futex word holding another value PASS
wait-check: Now on a futex word holding the value PASS
wait-check: Now on an offered capability, no call PASS
wait-check: a messages deadline expires PASS
wait-check: a readiness deadline expires PASS
wait-check: the timer sleeps until its instant PASS
wait-check: requests counter moved by one PASS
wait-check: Now on requests takes it PASS
wait-check: completions counter moved by one PASS
wait-check: Now on completions takes it PASS
wait-check: events counter moved by one PASS
wait-check: Now on events takes it, with its publisher PASS
wait-check: a new request is pending PASS
wait-check: taking every counted request needs no empty poll PASS
wait-check: Ready_Word: the word no longer holds the value PASS
wait-check: a futex wake of the wake word wakes it PASS
wait-check: the next Ready_Word wait takes the wake PASS
wait-check: a wake is taken once PASS
wait-check: an event poll takes the wake as the wake notice PASS
wait-check: a sent wake notice is refused PASS
wait-check: ALL PASS
readiness-writer: page readable PASS
readiness-writer: storing to the page
"
        # The store to the read-only page (its only store) must end the
        # writer with a write-protection fault, and it never prints "wrote".
        # Serial lines interleave, so match the kernel's one-line record.
        if grep -aF 'wait-check: ' "$SERIAL_LOG" | grep -qF ' FAIL' ||
           grep -aqF 'readiness-writer: wrote' "$SERIAL_LOG" ||
           ! grep -aqE 'name=readiness-writer pid=0x[0-9A-F]+ kind=write-protection' "$SERIAL_LOG"; then
            echo "headless: wait checks failed, or the readiness page was writable" >&2
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
futex-check: a joined thread's quota slot is free at once PASS
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
    ada-runtime)
        # Every check line must be a PASS, and the program must end on its
        # own (no last-chance handler, no fault).
        required_markers="
ada-runtime-check: dispatching through Shape'Class (heap objects) PASS
ada-runtime-check: interface membership from a class-wide object PASS
ada-runtime-check: Unchecked_Deallocation through access-to-interface PASS
ada-runtime-check: External_Tag and Internal_Tag (registered library-level tags) PASS
ada-runtime-check: nested tagged types dispatch to primitives reading the enclosing frame PASS
ada-runtime-check: nested tagged type External_Tag round trip PASS
ada-runtime-check: library-level controlled object initialized at elaboration PASS
ada-runtime-check: controlled record and array components
ada-runtime-check: collection finalizes live objects when its access type ends
ada-runtime-check: controlled object in a user pool finalized before Deallocate PASS
ada-runtime-check: Deallocate receives exact address, size and alignment ( 5 of 5) PASS
ada-runtime-check: pragma Default_Storage_Pool (CuBit.CuAlloc_Pool.Pool) in a package spec PASS
ada-runtime-check: exception raised three calls down reaches the caller's handler PASS
ada-runtime-check: Exception_Name (EXCEPTION_CHECKS.CUSTOM_ERROR) PASS
ada-runtime-check: index check raises Constraint_Error, caught by the caller PASS
ada-runtime-check: overflow check raises Constraint_Error PASS
ada-runtime-check: controlled objects finalized while the exception propagates (I0F2F1F0) PASS
ada-runtime-check: raise; keeps the occurrence; Save_Occurrence copies it PASS
ada-runtime-check: Finalize raising: the collection finishes, then Program_Error PASS
ada-runtime-check: three threads raise and handle at once, each sees its own occurrence ( 200 200 200 of 200) PASS
TEST: PASS ada-runtime
"
        if grep -aF 'ada-runtime-check: ' "$SERIAL_LOG" | grep -qF ' FAIL' ||
           grep -aqE 'User-space exception|Page fault|PAGE FAULT' "$SERIAL_LOG"; then
            echo "headless: ada-runtime checks failed, or the program ended abnormally" >&2
            exit 1
        fi
        ;;
    secondary-stacks)
        required_markers="
secondary-stack-check: main thread: 20 MiB primary, 9 MiB secondary, intact PASS
secondary-stack-check: main thread: a result past the ceiling refused (CAPACITY CEILING_REACHED), handled, stack usable PASS
secondary-stack-check: main thread: grew again on the same chunks after the release PASS
secondary-stack-check: main thread's secondary stack is outside its primary stack PASS
secondary-stack-check: extra threads started with their own areas PASS
secondary-stack-check: main thread alongside the extra threads: 20 MiB primary, 9 MiB secondary, intact PASS
secondary-stack-check: the main thread kept its stack PASS
secondary-stack-check: extra thread 1: a result past the ceiling refused (CAPACITY CEILING_REACHED), handled, stack usable PASS
secondary-stack-check: extra thread 4: grew again on the same chunks after the release PASS
secondary-stack-check: every thread has its own secondary stack PASS
secondary-stack-check: a second batch of threads, intact PASS
secondary-stack-check: ended threads gave back their secondary stacks (a second batch needs no more memory) PASS
secondary-stack-check: exhaustion caught as Storage_Error with a CAPACITY reason PASS
secondary-stack-check: the ceiling refused exactly the piece that would pass it PASS
secondary-stack-check: everything grown before the refusal is intact PASS
secondary-stack-check: released to the mark after the refusal PASS
secondary-stack-check: after the exhaustion, the program continues: results intact PASS
TEST: PASS secondary-stacks
secondary-stack-check: a result past the ceiling, unhandled
User-space exception STORAGE_ERROR: secondary stack exhausted: CAPACITY CEILING_REACHED demand=
"
        # The last request ends the program through the last-chance handler
        # (Storage_Error with the runtime's CAPACITY message, which names
        # the demand and the 256 MiB ceiling) and exit code 134, the typed
        # Unhandled_Exception outcome, after the PASS line: no corruption,
        # no fault, no "never refused".
        if grep -aF 'secondary-stack-check: ' "$SERIAL_LOG" | grep -qF ' FAIL' ||
           ! awk '/secondary-stack-check: a result past the ceiling, unhandled/ { seen = 1; next }
                  seen && /User-space exception STORAGE_ERROR: secondary stack exhausted: CAPACITY CEILING_REACHED demand=[0-9]+ ceiling=268435456/ { report = 1; next }
                  report && /procmgr: process com\.cubit\.secondary-stack-check pid[0-9 ]+ exited code 134/ { found = 1 }
                  END { exit !found }' "$SERIAL_LOG" ||
           grep -aE 'killed|Page fault|PAGE FAULT' "$SERIAL_LOG" | grep -qi 'secondary-stack-check'; then
            echo "headless: secondary-stack checks failed, or the unhandled exhaustion did not end the program cleanly" >&2
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
    llm-mock)
        required_markers="
secrets: ready
llm: ready for llm-mock.cubit.internal:18490
ccl-console: first frame presented
secrets: stored anthropic-api-key for process
secrets: anthropic-api-key used by role 37
tls: channel 1 established with llm-mock.cubit.internal
ccl-console: REPL completed: Secret_State: (Secret_State \"anthropic-api-key\" true \"llm\")
mission \"llm-mock\" model claude-opus-5-5 stop end_turn status 200 input 25 output 12 cache_read 0 cache_write 0 cost_nano_usd 340000
Llm_Stop.End_Turn
END-OF-ANSWER
stop cost_budget
Llm_Stop.Cost_Budget
stop deadline
Llm_Stop.Deadline
"
        llm_failed=0
        # The fake key reaches the mock only: never the serial console,
        # which also carries every logstore record (CuBit.Log echoes) and
        # the console's transcript lines.
        if grep -aF "sk-ant-test-0000" "$SERIAL_LOG" >/dev/null; then
            echo "headless: the API key appeared in the serial log" >&2
            llm_failed=1
        fi
        if grep -aF "MUST-NOT-ARRIVE" "$SERIAL_LOG" >/dev/null; then
            echo "headless: text after a cost stop reached the console" >&2
            llm_failed=1
        fi
        # Streaming: the first look at the text had part one only.
        first_text="$(grep -a "REPL completed: Stream<String>\|REPL completed: List<String>" "$SERIAL_LOG" | grep -aF "Hello from the mock" | head -1)"
        if [ -z "$first_text" ] || printf '%s' "$first_text" | grep -qF "END-OF-ANSWER"; then
            echo "headless: the text did not stream (first view: $first_text)" >&2
            llm_failed=1
        fi
        # The usage reached logstore: (logs.recent "llm") shows it.
        if ! grep -a "REPL completed: List<LogItem>" "$SERIAL_LOG" | grep -aF "stop end_turn" >/dev/null; then
            echo "headless: (logs.recent \"llm\") did not show the exchange" >&2
            llm_failed=1
        fi
        if ! wait "$NETWORK_PEER_PID"; then
            echo "headless: the llm mock failed: ${SERIAL_LOG}.llm-mock.log" >&2
            llm_failed=1
        fi
        NETWORK_PEER_PID=""
        rm -rf "$LLM_MOCK_STATE"
        if [ "$llm_failed" -ne 0 ]; then
            exit 1
        fi
        ;;
    net-fetch)
        required_markers="
ccl-console: first frame presented
tls: channel 1 established with fetch-test.cubit.internal
Fetch_Outcome.Stored
Unpack_Outcome.Unpacked
NET-FETCH-DEEP-FILE
Fetch_Outcome.Digest_Mismatch
outside this program's writable places
Unpack_Outcome.Refused
Unsafe_Path
../evil.txt
Fetch_Outcome.Too_Many_Redirects
Fetch_Outcome.Http_Status
Git_Outcome.Cloned
Git_Outcome.Http_Status
Git_Outcome.Exists
one result holds at most
"
        fetch_failed=0
        fetch_hash() {
            python3 -I -c 'import json, sys; print(json.load(open(sys.argv[1]))[sys.argv[2]])' \
                "$NET_FETCH_STATE/hashes.json" "$1"
        }
        # Each stored fetch's reply carries the body's SHA-256.
        for fetched in /files/pkg.tar /files/big.bin /files/evil.tar; do
            if ! grep -a "Fetch_Outcome.Stored" "$SERIAL_LOG" | grep -aF "$(fetch_hash "$fetched")" >/dev/null; then
                echo "headless: no stored fetch carries the SHA-256 of $fetched" >&2
                fetch_failed=1
            fi
        done
        # The place, as the console listed it last (the reply whole: it is
        # longer than one console line): the kept files and clones, and
        # nothing of the refused fetches, the refused unpack or the refused
        # clones.
        fetch_listing="$(python3 -I "$ROOT_DIR/tests/headless/console-reply.py" "$SERIAL_LOG" \
            "REPL completed: List<File_Metadata>")"
        kept_clones="clone1 clone2"
        if [ "$(cat "$NET_FETCH_STATE/clones" 2>/dev/null)" = 3 ]; then
            kept_clones="$kept_clones clone256"
        fi
        for kept in pkg.tar big.bin evil.tar pkg many $kept_clones; do
            if ! printf '%s' "$fetch_listing" | grep -aF "\"$kept\"" >/dev/null; then
                echo "headless: @system/work lacks $kept" >&2
                fetch_failed=1
            fi
        done
        for leftover in bad.txt .bad.txt.partial .big.bin.partial loop .loop.partial missing .missing.partial \
                        evil .evil.unpacking .pkg.unpacking evil.txt \
                        nothing .nothing.cloning .clone1.cloning .clone2.cloning .clone256.cloning; do
            if printf '%s' "$fetch_listing" | grep -aF "\"$leftover\"" >/dev/null; then
                echo "headless: @system/work still holds $leftover" >&2
                fetch_failed=1
            fi
        done
        # The bytes on the guest's disk, read back on the host.
        fetch_dump="$(mktemp -d "${TMPDIR:-/tmp}/cubit-net-fetch-dump.XXXXXX")"
        for fetched in pkg.tar big.bin; do
            debugfs -R "dump work/$fetched $fetch_dump/$fetched" "$TEMP_DISK" >/dev/null 2>&1
            if [ "$(sha256sum "$fetch_dump/$fetched" 2>/dev/null | cut -d' ' -f1)" != "$(fetch_hash "/files/$fetched")" ]; then
                echo "headless: work/$fetched on the disk is not the served body" >&2
                fetch_failed=1
            fi
        done
        debugfs -R "dump work/pkg/pkg/sub/deep/x.txt $fetch_dump/x.txt" "$TEMP_DISK" >/dev/null 2>&1
        if [ "$(cat "$fetch_dump/x.txt" 2>/dev/null)" != "NET-FETCH-DEEP-FILE" ]; then
            echo "headless: work/pkg/pkg/sub/deep/x.txt was not unpacked" >&2
            fetch_failed=1
        fi
        for escaped in work/evil.txt evil.txt; do
            if debugfs -R "stat $escaped" "$TEMP_DISK" 2>/dev/null | grep -q '^Inode:'; then
                echo "headless: the archive escaped its directory: $escaped exists" >&2
                fetch_failed=1
            fi
        done
        # Each clone, read back from the disk, judged by the host's git
        # against the repository served: fsck --strict, the commits, refs
        # and tag, status, every file.
        git_base='https://fetch-test.cubit.internal:18496/git'
        clone_check() {
            debugfs -R "rdump work/$1 $fetch_dump" "$TEMP_DISK" >/dev/null 2>&1
            if ! python3 -I "$ROOT_DIR/tests/net-fetch/check_clone.py" "$fetch_dump/$1" \
                "$(python3 -I -c 'import json, sys; print(json.load(open(sys.argv[1]))[sys.argv[2]])' \
                   "$NET_FETCH_STATE/repos.json" "$2")" "$3" "$4" >&2; then
                echo "headless: the clone work/$1 is not the repository $2" >&2
                fetch_failed=1
            fi
        }
        clone_check clone1 sha1.git sha1 "$git_base/sha1.git"
        clone_check clone2 sha1.git sha1 "$git_base/moved.git"
        if [ "$(cat "$NET_FETCH_STATE/clones" 2>/dev/null)" = 3 ]; then
            clone_check clone256 sha256.git sha256 "$git_base/sha256.git"
            if ! grep -a "Git_Outcome.Cloned" "$SERIAL_LOG" | grep -aF "Git_Format.Sha256" >/dev/null; then
                echo "headless: no clone reported the SHA-256 object format" >&2
                fetch_failed=1
            fi
        fi
        rm -rf "$fetch_dump"
        # The fixture saw each expected request, well formed, and nothing
        # else (the fetch into a place the console may not write never
        # connected).
        if ! wait "$NETWORK_PEER_PID"; then
            echo "headless: the fetch fixture's checks failed: ${SERIAL_LOG}.net-fetch.log" >&2
            fetch_failed=1
        fi
        NETWORK_PEER_PID=""
        rm -rf "$NET_FETCH_STATE"
        if [ "$fetch_failed" -ne 0 ]; then
            exit 1
        fi
        ;;
    agent-app)
        required_markers="
agent-app: first frame presented
secrets: stored anthropic-api-key
agent-app: API key stored in secrets.svc
secrets: anthropic-api-key used by role 37
tls: channel 1 established with llm-mock.cubit.internal
agent-app: mission 1 started
mission \"agent.Diagnose session 1\" model claude-opus-5-5 stop tool_use
mission \"agent.Diagnose session 1\" model claude-opus-5-5 stop end_turn
tool logs_recent grant logs.recent outcome ok
tool processes_list grant proc.list outcome ok
stop completed turns 2 tool_calls 2
agent-app: mission 1 ended
"
        agent_failed=0
        # The fake key reaches the mock only: never the serial log.
        if grep -aF "sk-ant-test-0000" "$SERIAL_LOG" >/dev/null; then
            echo "headless: the API key appeared in the serial log" >&2
            agent_failed=1
        fi
        if ! wait "$NETWORK_PEER_PID"; then
            echo "headless: the agent-app mock's checks failed: ${SERIAL_LOG}.llm-mock.log" >&2
            agent_failed=1
        fi
        NETWORK_PEER_PID=""
        rm -rf "$LLM_MOCK_STATE"
        if [ "$agent_failed" -ne 0 ]; then
            exit 1
        fi
        ;;
    agent-mock)
        required_markers="
secrets: anthropic-api-key used by role 37
tls: channel 1 established with llm-mock.cubit.internal
workspace saved notes.ccl
mission \"agent.Diagnose session 1\" model claude-opus-5-5 stop tool_use
mission \"agent.Diagnose session 1\" model claude-opus-5-5 stop end_turn
Agent_Stop.Completed
Agent_Stop.Tool_Call_Budget
[tool logs_recent]
[tool files_write]
[tool files_read]
not_granted: refused: files_write is not a tool of this Diagnose mission
END-OF-DIAGNOSIS
tool logs_recent grant logs.recent outcome ok
tool files_write grant none outcome not_granted
tool logs_recent grant logs.recent outcome malformed
tool files_read grant fs.read outcome ok
stop tool_call_budget turns 1 tool_calls 0
stop completed turns 3 tool_calls 5
"
        agent_failed=0
        # The fake key reaches the mock only.
        if grep -aF "sk-ant-test-0000" "$SERIAL_LOG" >/dev/null; then
            echo "headless: the API key appeared in the serial log" >&2
            agent_failed=1
        fi
        # Refused calls never ran: the budget turn's calls have no cards.
        if grep -aF "toolu_budget" "$SERIAL_LOG" | grep -aF "outcome ok" >/dev/null; then
            echo "headless: a call over the tool budget ran" >&2
            agent_failed=1
        fi
        if ! wait "$NETWORK_PEER_PID"; then
            echo "headless: the agent mock's checks failed: ${SERIAL_LOG}.llm-mock.log" >&2
            agent_failed=1
        fi
        NETWORK_PEER_PID=""
        rm -rf "$LLM_MOCK_STATE"
        if [ "$agent_failed" -ne 0 ]; then
            exit 1
        fi
        ;;
    llm-live)
        required_markers="
llm: ready for api.anthropic.com:443
tls: channel 1 established with api.anthropic.com
Llm_Stop.End_Turn
"
        if grep -aF "sk-ant-" "$SERIAL_LOG" >/dev/null; then
            echo "headless: something like an API key appeared in the serial log" >&2
            exit 1
        fi
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
ipctest-client: displaced reply ended PASS
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
BENCH: PASS kernel-object call
TIMING: kernel-object-call
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
    desktop-reap)
        required_markers="
reap-check: surface
procmgr: process com.cubit.reap-check
ended: its process ended
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
    nmi)
        required_markers="
nmi-check: spinning
NMI on CPU
nmi-check: round 20
nmi-check: done
"
        # The process that was running when the NMIs landed is never killed.
        if grep -aqF 'name=nmi-check' "$SERIAL_LOG" ||
           grep -aqF 'DOUBLE FAULT' "$SERIAL_LOG" ||
           grep -aqF 'KERNEL PANIC' "$SERIAL_LOG"; then
            echo "headless: nmi: a process or the kernel died after an NMI" >&2
            exit 1
        fi
        ;;
    direction-flag)
        required_markers="
direction-flag-check: running with DF set
direction-flag-check: round 8
direction-flag-check: rounds done; faulting with DF set
kind=exception vector=6
com.cubit.direction-flag-check
"
        # Before the fix the kernel ran its interrupt handlers with the
        # process's DF: building the stop record for the UD2 zeroed a return
        # address, and the kernel recursed on its own faults until the
        # machine reset (no stop record, often no message at all).
        if grep -aqF 'DOUBLE FAULT' "$SERIAL_LOG" ||
           grep -aqF 'direction-flag-check: FAIL' "$SERIAL_LOG" ||
           grep -aqF 'NXE violation in kernel' "$SERIAL_LOG" ||
           grep -aqF 'KERNEL PANIC' "$SERIAL_LOG" ||
           grep -aqF 'kernel exception' "$SERIAL_LOG"; then
            echo "headless: direction-flag: the kernel faulted with the process's DF set" >&2
            exit 1
        fi
        ;;
    alignment-flag)
        required_markers="
SMAP enabled
SMAP probe: armed
alignment-flag-check: running with AC set
SMAP probe: user memory faulted in an interrupt handler (kernel AC=0)
alignment-flag-check: round 8
alignment-flag-check: done
"
        # Before the fix interrupt handlers ran with the process's AC, so
        # under SMAP a kernel read of user memory went through.
        if grep -aqF 'SMAP probe: an interrupt handler read user memory' "$SERIAL_LOG" ||
           grep -aqF 'alignment-flag-check: FAIL' "$SERIAL_LOG" ||
           grep -aqF 'KERNEL PANIC' "$SERIAL_LOG" ||
           grep -aqF 'kernel exception' "$SERIAL_LOG"; then
            echo "headless: alignment-flag: an interrupt handler ran with the process's AC set" >&2
            exit 1
        fi
        ;;
    kernel-map-probe)
        required_markers="
KERN014 probe: armed
KERN014-PROBE: freed frame holds allocator links: no
kernel-map-probe: marker 0x4b45524e30313421
KERN014-PROBE: user write stored
kernel-map-probe: writable page holds the store
kernel-map-probe: read-only page unchanged
kernel-map-probe: done
"
        # Since KERN-014 slice 4 a process's pages come from regions the
        # kernel's linear map no longer covers: the probe's read through it
        # faults ("unmapped"). Before, it read the marker. Since slice 1
        # the buddy allocator's free-list links live out of the blocks: a
        # freed frame holds none ("yes" before).
        KERNEL_MAP_READ=$(grep -a -o 'KERN014-PROBE: process page through the kernel map: [a-z]*[ 0-9a-fx]*' "$SERIAL_LOG" | head -1)
        echo "headless: kernel-map-probe: ${KERNEL_MAP_READ:-no probe report}"
        # Since slice 2 the kernel stores into a process's memory through its
        # own mapping: refused into a read-only page, a reserved page with no
        # frame and the kernel half; the first two fault and are recovered.
        KERNEL_MAP_REFUSED=$(grep -a -c 'KERN014-PROBE: user write refused' "$SERIAL_LOG")
        KERNEL_MAP_FAULTS=$(grep -a -o 'KERN014-PROBE: user-access faults recovered [0-9]*' "$SERIAL_LOG" | grep -o '[0-9]*$' | head -1)
        echo "headless: kernel-map-probe: stores refused ${KERNEL_MAP_REFUSED}, faults recovered ${KERNEL_MAP_FAULTS:-none}"
        if [ "$KERNEL_MAP_REFUSED" != 3 ] || [ "${KERNEL_MAP_FAULTS:-0}" -lt 2 ] ||
           grep -aqF "KERN-014: user access refused: caller's tables not loaded" "$SERIAL_LOG" ||
           [ "$KERNEL_MAP_READ" != "KERN014-PROBE: process page through the kernel map: unmapped" ] ||
           grep -aqF 'kernel-map-probe: FAIL' "$SERIAL_LOG" ||
           grep -aqF 'KERNEL PANIC' "$SERIAL_LOG" ||
           grep -aqF 'kernel exception' "$SERIAL_LOG"; then
            echo "headless: kernel-map-probe: unexpected kernel-map reach" >&2
            exit 1
        fi
        ;;
    mov-ss-debug)
        required_markers="
DBGK-PROBE: armed
mov-ss-check: MOV SS from a watched word, then SYSCALL
mov-ss-check: SYSCALL variant survived, kernel #DB taken
mov-ss-check: MOV SS from a watched word, then INT3
kind=exception vector=3
com.cubit.mov-ss-check
"
        # The kernel took each deferred #DB on its own stack with its own GS
        # base: no double fault (the user stack under SMAP), no kernel
        # exception or panic, and the INT3 reached user mode as a #BP.
        if grep -aqF 'DOUBLE FAULT' "$SERIAL_LOG" ||
           grep -aqF 'mov-ss-check: FAIL' "$SERIAL_LOG" ||
           grep -aqF 'kernel exception' "$SERIAL_LOG" ||
           grep -aqF 'KERNEL PANIC' "$SERIAL_LOG"; then
            echo "headless: mov-ss-debug: kernel faulted on a deferred debug exception" >&2
            exit 1
        fi
        ;;
    trap-flag)
        required_markers="
trap-flag-check: setting TF, then SYSCALL
kind=exception vector=1
com.cubit.trap-flag-check
"
        # The kernel survived the syscall and the trap came from user mode;
        # before the fix the #DB hit in ring 0 on the user's stack pointer.
        if grep -aqF 'DOUBLE FAULT' "$SERIAL_LOG" ||
           grep -aqF 'trap-flag-check: FAIL' "$SERIAL_LOG" ||
           grep -aqF 'KERNEL PANIC' "$SERIAL_LOG"; then
            echo "headless: trap-flag: kernel faulted, or the single-step did not trap" >&2
            exit 1
        fi
        ;;
    desktop-restart)
        required_markers="
DESKTOP-TEST-FAULT: exiting after the first frame
procmgr: process com.cubit.desktop
procmgr: restarting desktop.svc in 1000 ms (restart 1)
procmgr: restarted desktop.svc pid
DESKTOP-TEST-FAULT: restarted; taking the display over
display: lease of ended owner pid
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
    metricsview)
        required_markers="
metricsvc: typed metrics ready
desktop: active outputs= 1 primary= 0
metricsview: dashboard \"Display latency\" panels=8 store=bound
metricsview: window ready
metricsview: first frame presented
metricsview: own series metricsview.input_to_render_us n=
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
ccl-console: REPL completed: List<Disk>: [(Disk 0 (Disk_Identity \"QEMU UFS\" \"\" $((${CUBIT_INSTALL_DISK_MIB:-1024} * 256)) 4096))]
installer: install refused: confirmation names
ccl-console: REPL completed: Install_Report: (Install_Report Install_Outcome.Wrong_Disk Identity_Proof.Serial_Number Install_Step.Checking 0 0)
installer: installing to disk 0
installer: status answered during the install:
ccl-console: REPL completed: Install_Progress: (Install_Progress Install_State.
installer: step: partitioning
installer: step: formatting the root
installer: step: filling the storehouse
installer: step: writing generation 1
installer: step: verifying the root
installer: step: writing the ESP
installer: step: verifying the ESP
installer: step: making the disk bootable
installer: step: flushing
installer: step: done
installer: install done: installed
ccl-console: REPL completed: Install_Report: (Install_Report Install_Outcome.Installed Identity_Proof.Model_Geometry Install_Step.Done
installer: disk 0 reports no serial: identified by model and geometry
"
            if [ -n "${CUBIT_INSTALL_INTERRUPT_AT:-}" ]; then
                # The power was cut mid-install: only how far it got.
                required_markers="
installer: installing to disk 0
$CUBIT_INSTALL_INTERRUPT_AT
"
            fi
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
        elif [ "${CCL_CONSOLE_DEMO:-}" = debug ]; then
            required_markers="$required_markers
ccl-console: REPL completed: Debug_Session: (Debug_Session 1 
kernel attach (Debug_Observe)
ccl-console: REPL completed: List<Debug_Thread>: [(Debug_Thread 1
ccl-console: REPL completed: List<Debug_Frame>: [(Debug_Frame 0
ccl-console: REPL completed: List<Debug_Binding>:
ccl-console: REPL completed: debug.attach was refused by its service: procmgr.svc is never debuggable
ccl-console: REPL completed: Boolean: true
"
        elif [ "${CCL_CONSOLE_DEMO:-}" = wifi ]; then
            required_markers="$required_markers
ccl-console: REPL completed: List<Wifi_Network>: (list-of Wifi_Network)
ccl-console: REPL completed: Integer: 0
wifi: starting
ccl-console: REPL completed: Wifi_Status: (Wifi_Status Wifi_State.No_Radio \"\" \"00:00:00:00:00:00\"
wifi: no Wi-Fi radio is attached
ccl-console: REPL completed: config.get failed
wifi: passphrase stored for \"CuBitTest\"
ccl-console: REPL completed: List<LogItem>: [(LogItem.Logged (LogEntry
"
        elif [ "${CCL_CONSOLE_DEMO:-}" = log-exited ]; then
            # LOG-003: the probe's records carry no name of their own; after
            # it exits boot-logs still names them and logs.recent finds them.
            required_markers="$required_markers
log-exit-probe: published 3 records, exiting
boot-logs: log-exit-probe: exit probe stage 1 of 3
boot-logs: log-exit-probe: exit probe stage 3 of 3
boot-logs: log-exit-probe exited (com.cubit.log-exit-probe, log-exit-probe.app)
ccl-console: REPL completed: List<LogItem>: [(LogItem.Logged (LogEntry
\"exit probe stage 1 of 3\"))
\"exit probe stage 3 of 3\")) (LogItem.Tally (LogTally
"
        elif [ "${CCL_CONSOLE_DEMO:-}" = log-burst ]; then
            # LOG-005: both phases, and both results (tests/log-burst/
            # check_serial.py checks their counts).
            required_markers="$required_markers
log-burst-probe: phase 1 done, 1000 lines
log-burst-probe: phase 2 done, 5000 lines
"
        elif [ "${CCL_CONSOLE_DEMO:-}" = kernel-log ]; then
            # LOG-004: the handler's line on serial as before; in logstore
            # the exception through crash-probe's own log channel, and the
            # summaries of the kernel's typed fault reports under
            # fault-check's and stack-probe's names, their details withheld
            # (tests/kernel-log-ring/check_serial.py checks the rest).
            required_markers="$required_markers
crash-probe: raising an exception nobody handles
User-space exception PROGRAM_ERROR: main.adb:25 explicit raise
procmgr: process com.cubit.crash-probe pid
exited code 134
fault-check: writing an unmapped address
stack-probe: recursing until the stack runs out
logstore: kernel log caught up:
\"unhandled exception PROGRAM_ERROR: main.adb:25 explicit raise; traceback 0x
\"com.cubit.fault-check: stopped by a page fault (write); details restricted\")
\"com.cubit.stack-probe: stopped by a page fault (write); details restricted\")
withheld the restricted fault detail of com.cubit.fault-check (null page)
withheld the restricted fault detail of com.cubit.stack-probe (stack overflow)
"
        elif [ "${CCL_CONSOLE_DEMO:-}" = keys ]; then
            required_markers="$required_markers
desktop: keys to CCL Console
ccl-console: REPL completed: Integer: 3
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
ccl-console: REPL completed: List<LogItem>: [(LogItem.Logged (LogEntry
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
    debugger-gui|debugger-gui-rs)
        required_markers="
debugger: attach ATTACHED
debugger: window ready
debugger: first full frame
debugger: continue
debugger: live on
debugger: live off
ui-app: protected frame published
"
        if [ "$TEST_NAME" = "debugger-gui" ]; then
            # Attach to process... (DBG-001): clock.svc, no stub, through
            # the kernel (procmgr's Debug_Observe, the session's authority).
            required_markers="$required_markers
debugger: attach to process clock.svc
procmgr: debug: Debug_Observe on com.cubit.clock
debugger: attached to clock.svc through the kernel (kernel attach (Debug_Observe))
debugger: shows clock pid
"
        fi
        ;;
    bible)
        required_markers="
bible: starting
bible: window ready
ui-app: protected frame published
bible: showing John 1 (KJV, Literata)
bible: showing John 3 (KJV, Literata)
bible: showing John 3 (KJV, Source Serif 4)
bible: showing John 3 (KJV, EB Garamond)
bible: showing John 3 (BSB, Literata)
bible: showing John 4 (BSB, Literata)
bible: showing Psalm 23 (BSB, Literata)
bible: showing Psalm 22 (23) (VUL, Literata)
bible: showing John 3 (VUL, Literata)
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
capability-test: startup table holds only the self entries and logstore, with the declared limit PASS
capability-test: self entries are current handles PASS
capability-test: no firmware-memory window without the ACPI hardware grant PASS
capability-test: firmware pages never map through a forged or foreign handle PASS
capability-test: PCI configuration never maps through a forged or foreign handle PASS
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
capability-test: retired BOOT_TIMELINE rejected PASS
capability-test: a plain process cannot list processes PASS
capability-test: no boot-timeline publication without declaring it PASS
capability-test: all tests passed
capability-test: read of the undeclared boot timeline armed
"
        ;;
    storage-grants)
        required_markers="
GRANT-REFERENCE-CHECK: PASS
GRANT-RECLAMATION-CHECK: PASS
OWNED-MEMORY-CHECK: PASS
OWNED-REGION-CHECK: PASS
OWNED-GRANT-RETENTION-CHECK: PASS
OWNED-EXIT-CHECK: leaving two allocations
Process.reclaimProcess: owned regions retired 3
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
display: session copies frames
desktop: output 0 copies frames to Display
desktop: wallpaper catalog @system/Assets/cubit-wallpapers/2/catalog.ccl loaded 1 wallpaper(s)
desktop: wallpaper loaded @system/Assets/cubit-wallpapers/2/cubes.qoi size= 2048 x 576
desktop: icons loaded @system/Assets/cubit-icons/5/applications.qoi
desktop: active outputs= 1 primary= 0
desktop: internal shell active
shell: cwd=@nvme:0/
ccl-workbench: native window ready
desktop: status network wired=shown wifi=hidden (no adapter)
desktop: status memory bar
desktop: status CPU bar
"
        ;;
    desktop-session)
        # run-desktop's session reaches its first frame on the virtio-vga.
        required_markers="
virtio-gpu: ready
display: backend virtio-gpu
procmgr: init launched: desktop.svc
desktop: active outputs= 1 primary= 0
desktop: asynchronous frame released
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
DESKTOP-INPUT-BATCH-CHECK: PASS batches=2 events=12 ring=2
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
DISPLAY-LEND-CHECK: PASS copy only (firmware framebuffer)
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
DISPLAY-LEND-CHECK: PASS three loans
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
ps2: input source rings open, entering event loop
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
    display-latency)
        required_markers="
display: backend virtio-gpu (3 buffers)
display: session lent scanout
desktop: output 0 composes into the lent scanout buffers (no GPU endpoint held)
calculator: first frame presented
desktop: latency inputs=
desktop: latency stage=input_to_ready
"
        if [ "${CUBIT_DISPLAY_LATENCY_ANIMATION:-0}" = 1 ]; then
            required_markers="
display: backend virtio-gpu (3 buffers)
display: session lent scanout
desktop: output 0 composes into the lent scanout buffers (no GPU endpoint held)
frame-ticker: no offerable self endpoint (none declared; self cannot be offered)
frame-ticker: first frame presented
frame-ticker: animating
desktop: latency inputs=
desktop: latency stage=input_to_ready
"
        fi
        ;;
    metrics-latency)
        required_markers="
metricsvc: typed metrics ready
display: backend virtio-gpu
calculator: first frame presented
desktop: latency inputs=
ccl-remote: a host paired
metrics-fixture: entry=present
metrics-fixture: entry=absent
"
        ;;
    ui-latency)
        required_markers="
UI-PROBE: first frame ms=
UI-PROBE: busy frames=
UI-PROBE: keys ready
UI-PROBE: keys keys=
UI-PROBE: closed
"
        ;;
    neon-blast)
        required_markers="
neon-blast: starting
neon-blast: audio stream open (48 kHz stereo, lent ring) after
neon-blast: first frame painted (software)
neon-blast: stats path=software
mixer: HDA period IRQ active
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
    nvme-stuck)
        # tests/nvme-timeout/check.py checks the typed failure, then that
        # later reads succeed.
        required_markers="
devmgr: startup complete, entering service loop
NVMe Driver: Ready, entering message loop.
"
        ;;
    usb-slots)
        # tests/usb-slots/check.py checks the slots and the ninth device.
        required_markers="
devmgr: startup complete, entering service loop
"
        ;;
    xhci-bringup)
        # tests/xhci-bringup/check.py checks the times and the devices.
        required_markers="
devmgr: startup complete, entering service loop
boot-logs: window ready
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
generic-check: stage dma-round-trip ok
generic-check: stage dma-unmapped ok
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
shell: idle system calls in 1000 ms=
"
        # The shell idles in an activity wait: it used to poll for events
        # and sleep 1 ms, about 2,000 system calls a second.
        SHELL_IDLE_CALLS=$(grep -aoE 'shell: idle system calls in 1000 ms= [0-9]+' "$SERIAL_LOG" |
                           tail -1 | grep -oE '[0-9]+$')
        if [ -n "$SHELL_IDLE_CALLS" ] && [ "$SHELL_IDLE_CALLS" -gt 50 ]; then
            echo "headless: startup-health: the idle shell made $SHELL_IDLE_CALLS system calls in 1 s (at most 50)" >&2
            exit 1
        fi
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
        if [ "$SCREEN_SETTLE" -eq 1 ]; then
            required_markers="
files: first frame presented
files: pane 1 listed @nvme:0/ entries=
files: closed
"
        fi
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
    # Input must be dispatched while the delayed reader held a frame, for
    # the SAME frame, not merely while a submission was queued somewhere:
    # Desktop names the frame its input arrived during. (Its line reaches
    # serial through logstore's mirror, ABI-002, so its position against
    # Display's lines says nothing; the frame number does.)
    if ! awk '
        /display-test: hold / { held[$NF] = 1 }
        /desktop: input during frame / { input[$NF] = 1 }
        /display-test: stable / { stable++; settled[$NF] = 1 }
        END {
            for (f in input) if (held[f] && settled[f]) progressed++;
            exit !(stable >= 2 && progressed >= 1)
        }
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
if { [ "$TEST_NAME" = "device-build" ] || [ "$TEST_NAME" = "service-switch" ] \
       || [ "$TEST_NAME" = "service-switch-reboot" ] || [ "$TEST_NAME" = "acpi-live-replace" ] \
       || [ "$TEST_NAME" = "desktop-live-replace" ] \
       || [ "$TEST_NAME" = "console-run-staged" ] || [ "$TEST_NAME" = "bench-build" ]; } && [ -n "$INPUT_INJECTOR_PID" ]; then
    # The host driver's own checks (its PASS/FAIL lines above) gate too.
    if ! wait "$INPUT_INJECTOR_PID"; then
        INPUT_INJECTOR_PID=""
        echo "headless: $TEST_NAME: the host driver reported failures" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    INPUT_INJECTOR_PID=""
fi
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

if [ "$TEST_NAME" = "mailbox-self" ] || [ "$TEST_NAME" = "mailbox-mutual" ]; then
    # The source rule the run exercises: no own-table lookup under another
    # process's mailbox lock (checked after the run, so a kernel without
    # the rule still shows what it does).
    python3 -I "$ROOT_DIR/tests/mailbox-locks/check_own_lookups.py" "$KERNEL_DIR/src" || exit 1
fi
if [ "$TEST_NAME" = "mailbox-mutual" ]; then
    # Both copies finished their rounds, every RESUME refused (IDIOT-001).
    MUTUAL_DONE="$(grep -ac 'MAILBOX-MUTUAL: done PASS' "$SERIAL_LOG")"
    if [ "$MUTUAL_DONE" -ne 2 ]; then
        echo "headless: $MUTUAL_DONE of 2 mutual RESUME copies finished (deadlock?)" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "console-route" ]; then
    # ABI-002: every line came through logstore's mirror, once: none was
    # written to the kernel console by the program itself (an unprefixed
    # copy), CuBit.Log's echo made no second record, and the long line went
    # on as a second record rather than being dropped.
    console_failed=0
    if grep -aq '^CONSOLE-ROUTE:' "$SERIAL_LOG"; then
        echo "headless: console-route: a line reached the console without logstore" >&2
        console_failed=1
    fi
    for once in "CONSOLE-ROUTE: routed" "CONSOLE-ROUTE: logged once" "CONSOLE-ROUTE: in three pieces"; do
        seen=$(grep -acF "$once" "$SERIAL_LOG")
        if [ "$seen" -ne 1 ]; then
            echo "headless: console-route: \"$once\" seen $seen times, not once" >&2
            console_failed=1
        fi
    done
    # 600 L's: 492 complete the first record's 512 bytes, 108 continue.
    if ! grep -aqE ": CONSOLE-ROUTE: long L{492}[[:space:]]*$" "$SERIAL_LOG" ||
       ! grep -aqE "^[^ ]+: L{108}[[:space:]]*$" "$SERIAL_LOG"; then
        echo "headless: console-route: the long line was not split into 512 + 108 bytes" >&2
        console_failed=1
    fi
    if [ "$console_failed" -ne 0 ]; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    echo "headless: console-route: every line once, through logstore's mirror"
fi

if [ "$TEST_NAME" = "servo" ] && [ -z "${SERVO_DESKTOP:-}" ]; then
    # A page of text only must reach the frame as glyphs: no fonts (or no
    # glyph rasterization) leaves it blank.
    if ! python3 "$ROOT_DIR/tests/servo/check_text_glyphs.py" "$SERIAL_LOG" "$SERVO_TEXT_PAGE"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "ui-latency" ] && [ "$QEMU_ACCEL" = "kvm" ]; then
    # LAT-002: a key delivered to the probe is published within 1 ms in
    # every report period. The window used to hear its answer only at the
    # end of Desktop's turn, after the frame was composed (about 2 ms).
    slow=$(grep -a "latency stage=delivery_to_publish" "$SERIAL_LOG" |
           grep -oE "max=[0-9]+" | awk -F= '$2 > 1000' | wc -l)
    if [ "$slow" -ne 0 ]; then
        echo "headless: ui-latency: $slow report periods with delivery_to_publish max over 1000 us" >&2
        grep -a "latency stage=delivery_to_publish" "$SERIAL_LOG" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    echo "headless: every key published within 1 ms of its delivery"
    # Idle glyph warming: once Desktop says the cache is warm, no text batch
    # waits for a glyph to be rasterized (a new window's title did, ~1.4 ms
    # for 13 glyphs).
    if ! grep -aq "desktop: glyph cache warmed" "$SERIAL_LOG"; then
        echo "headless: ui-latency: the glyph cache was never warmed" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    # Unanchored: Desktop's lines reach serial behind their source's name
    # (logstore's mirror, ABI-002).
    cold=$(awk '/desktop: glyph cache warmed/ {w=1} w && /DESKTOP-TEXT-SLOW:/ {n++} END {print n+0}' "$SERIAL_LOG")
    if [ "$cold" -ne 0 ]; then
        echo "headless: ui-latency: $cold slow text batches after the glyph cache was warmed" >&2
        grep -a "DESKTOP-TEXT-SLOW" "$SERIAL_LOG" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    echo "headless: no text waited for rasterization after warming"
fi

if [ "$TEST_NAME" = "cpu-refusal" ]; then
    # Refused before userspace: no program was started.
    if grep -aEq "^(procmgr\.svc: )?procmgr: " "$SERIAL_LOG"; then
        echo "headless: cpu-refusal: programs started on a CPU without x86-64-v3" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    echo "headless: a CPU without x86-64-v3 is refused before any program starts"
fi

if [ "$TEST_NAME" = "font-masks" ]; then
    # The native masks are byte for byte those of the hosted engine's golden
    # (userspace/rust/fonts/tests/masks.golden, its masks_match_golden test):
    # every glyph's hash and advance, in order.
    if ! diff <(grep -a "^font-masks: face=" "$SERIAL_LOG" | sed 's/^font-masks: //') \
              "$ROOT_DIR/userspace/rust/fonts/tests/masks.golden" >&2; then
        echo "headless: font-masks: native masks differ from userspace/rust/fonts/tests/masks.golden" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    echo "headless: every native glyph mask matches the hosted golden"
fi

if [ "$TEST_NAME" = "desktop-restart" ]; then
    # Exactly one failure, and the restarted desktop presents frames again.
    faults=$(grep -cF 'DESKTOP-TEST-FAULT: exiting after the first frame' "$SERIAL_LOG")
    released=$(grep -cF 'desktop: asynchronous frame released' "$SERIAL_LOG")
    if [ "$faults" -ne 1 ] || [ "$released" -lt 2 ] ||
       ! awk '/display: lease of ended owner pid/ { leased = NR }
              /desktop: asynchronous frame released/ && leased { shown = 1 }
              END { exit !shown }' "$SERIAL_LOG"; then
        echo "headless: desktop restart: faults=$faults frames released=$released" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    echo "headless: desktop restarted after its failure and presents again"
    # IPC-009: the input drivers' source rings end with the failed Desktop
    # and open again to the restarted one.
    if ! awk '/DESKTOP-TEST-FAULT: restarted; taking the display over/ { restarted = NR }
              /desktop: input source [0-9]+ opened/ && restarted { reopened = 1 }
              END { exit !reopened }' "$SERIAL_LOG"; then
        echo "headless: desktop restart: no input source ring opened to the restarted desktop" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    echo "headless: the input source rings opened again to the restarted desktop"
    # IPC-009: the failed Desktop's frame ring ends at Display, and the
    # restarted Desktop's frames reach Display through a new one.
    # Display's own lines, in Display's order: opened, ended, opened again
    # (Desktop's restart line reaches serial through logstore's mirror,
    # ABI-002, so its position against Display's lines is not evidence).
    if ! awk '/display: frame ring [0-9]+ opened/ { opened++; if (ended) reopened = 1 }
              /display: frame ring [0-9]+ ended/ && opened == 1 { ended = 1 }
              END { exit !(ended && reopened && opened >= 2) }' "$SERIAL_LOG"; then
        echo "headless: desktop restart: the frame ring to Display did not end and open again" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    echo "headless: the frame ring to Display ended and opened again with the restarted desktop"
fi

if [ "$TEST_NAME" = "ccl-console" ] && [ "${CCL_CONSOLE_DEMO:-}" = large ]; then
    # UI-039: on 1080p the console's window is configured past 1280x720 (the
    # cap the CCL apps had); Desktop opens it at 3/4 of the work area's height.
    if ! grep -aoE "ccl-console: window size [0-9]+x[0-9]+" "$SERIAL_LOG" |
         awk -F'[ x]' '{ if ($4 > 1280 || $5 > 720) found = 1 } END { exit !found }'; then
        echo "headless: the console was never configured past 1280x720:" >&2
        grep -aE "ccl-console: window size" "$SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "ccl-console" ] && [ "${CCL_CONSOLE_DEMO:-}" = keys ]; then
    # UI-039: typing never ends the console (image 26: it exited, code 0,
    # on the first key).
    if grep -aE "procmgr: process com\.cubit\.ccl-console pid[0-9]+ exited" "$SERIAL_LOG"; then
        echo "headless: the console ended while it was typed into" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "ccl-console" ] && [ "${CCL_CONSOLE_DEMO:-}" = kernel-log ]; then
    # LOG-004: kernel records from before logstore started, an exact
    # account of the ring (taken + dropped = numbered), and with
    # KERNEL_LOG_OVERFLOW=1 (a kernel built with KERNEL_LOG_RECORDS=64) the
    # dropped marker with the same counts.
    if ! python3 -I "$ROOT_DIR/tests/kernel-log-ring/check_serial.py" "$SERIAL_LOG" \
        ${KERNEL_LOG_OVERFLOW:+--overflow}; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "ccl-console" ] && [ "${CCL_CONSOLE_DEMO:-}" = log-burst ]; then
    # LOG-005: logs.recent returned all of the probe's records or typed
    # gaps with exact counts, and logstore's tally balances.
    if ! python3 -I "$ROOT_DIR/tests/log-burst/check_serial.py" "$SERIAL_LOG"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "ccl-console" ] && [ "${CCL_CONSOLE_DEMO:-}" = wifi ]; then
    # The passphrase typed into the secret entry never reaches a log.
    if grep -qF 'correct horse' "$SERIAL_LOG"; then
        echo "headless: the passphrase reached the serial log" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "ufs-write" ]; then
    # What the guest wrote reached the medium: the image file holds it.
    if ! python3 -I "$ROOT_DIR/tests/ufs/check-write.py" "$TEMP_UFS_DISK"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "nvme-timeout" ] || [ "$TEST_NAME" = "nvme-stuck" ]; then
    if ! python3 -I "$ROOT_DIR/tests/nvme-timeout/check.py" "$SERIAL_LOG" \
      "$NVME_FIXTURE_RESULT" "$( [ "$TEST_NAME" = "nvme-stuck" ] && echo stuck || echo recover )"; then
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
if [ "$TEST_NAME" = "xhci-bringup" ]; then
    if ! python3 "$ROOT_DIR/tests/xhci-bringup/check.py" "$SERIAL_LOG"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "usb-slots" ]; then
    if ! python3 "$ROOT_DIR/tests/usb-slots/check.py" "$SERIAL_LOG"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "ccl-console" ] && [ "${CCL_CONSOLE_DEMO:-}" = install ] \
   && [ -z "${CUBIT_INSTALL_INTERRUPT_AT:-}" ]; then
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
    if "\t" in line and line.split("\t")[1]:
        name, where = line.split("\t")[:2]
        target = payload / name
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(payload / where.removeprefix("install/"), target)' "$CHECK_PAYLOAD"
    else
        python3 -I "$ROOT_DIR/tests/disk-layout/make-payload.py" "$CHECK_PAYLOAD/esp" "$CHECK_PAYLOAD/root"
    fi
    # What the installer adds to the root (SYS-003): every component INDEX
    # names with its SHA-256 once more by content in the storehouse (root
    # programs under executables/, mode 0755; boot files under files/), and
    # generation 1, current.
    python3 -I -c 'import os, shutil, sys, pathlib
payload = pathlib.Path(sys.argv[1])
store = payload / "root" / "storehouse"
for kind in ("executables", "files"):
    (store / kind).mkdir(parents=True, exist_ok=True)
index = payload / "INDEX"
for line in (index.read_text().splitlines() if index.exists() else []):
    fields = line.split("\t")
    digest = [f[len("sha256="):] for f in fields[1:] if f.startswith("sha256=")]
    if not digest:
        continue
    root_file = fields[0].startswith("root/")
    target = store / ("executables" if root_file else "files") / digest[0]
    target.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(payload / fields[0], target)
    os.chmod(target, 0o755 if root_file else 0o644)
system = store / "profiles" / "system"
system.mkdir(parents=True, exist_ok=True)
(system / "1").write_text("(System_Generation number => 1 previous => 0 pins => (list-of Service_Pin))\n")
(system / "current").write_text("1\n")' "$CHECK_PAYLOAD"
    if ! bash "$ROOT_DIR/tests/disk-layout/check-image.sh" "$TEMP_UFS_DISK" 4096 \
         1048576 272629760 273678336 "$CHECK_PAYLOAD/esp" "$CHECK_PAYLOAD/root"; then
        rm -rf "$CHECK_PAYLOAD"
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    rm -rf "$CHECK_PAYLOAD"
fi

# Boot memory comes back after its last reader (tests/boot-reclaim).
case "$TEST_NAME" in
    boot-shell-nvme|boot-memory-8g|boot-memory-16g|boot-memory-32g|boot-large-module|storage-grants)
        if ! python3 -I "$ROOT_DIR/tests/boot-reclaim/check.py" "$SERIAL_LOG" "$TEST_NAME"; then
            echo "headless: serial log: $SERIAL_LOG" >&2
            exit 1
        fi
        ;;
esac

# ABI-002, no lost events: every numbered line late-intro-check queued
# before the introduction and published after it reached serial, in order,
# while the program still ran (before the kernel reclaimed it).
if [ "$TEST_NAME" = "late-introduction" ]; then
    if ! awk '
          match($0, /Loaded module late-intro-check w\/ process ID [0-9]+/) && !pid {
              pid = substr($0, RSTART, RLENGTH); sub(/.* /, "", pid) }
          pid && $0 ~ ("Process.reclaimProcess: stopped PID " pid "$") && !gone { gone = NR }
          match($0, /LATE-INTRO: (before|after) [0-9]+$/) {
              n = split(substr($0, RSTART, RLENGTH), w, " ")
              key = w[2] " " w[3]; if (!(key in at)) { at[key] = NR; count++ } }
          END {
              if (count != 40 || !gone) exit 1
              last = 0
              for (side = 1; side <= 2; side++)
                  for (i = 1; i <= 20; i++) {
                      key = (side == 1 ? "before" : "after") " " i
                      if (!(key in at) || at[key] < last || at[key] > gone) exit 1
                      last = at[key] }
          }' "$SERIAL_LOG"; then
        echo "headless: late-introduction: numbered lines before and after the introduction not all delivered, in order, before the program ended" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

# usb-disk: the driver script's steps, then Linux checks stick 1 as
# fat32-volume checks its NVMe stick (CUBIT_USB_DISK_KEEP=DIR keeps them).
if [ "$TEST_NAME" = "usb-disk" ]; then
    if [ -n "${CUBIT_USB_DISK_KEEP:-}" ]; then
        mkdir -p "$CUBIT_USB_DISK_KEEP" && cp "$USB_STICK_DIR"/*.img "$CUBIT_USB_DISK_KEEP/"
    fi
    if ! grep -qx 'replacement attached' "$USB_DISK_DRIVE_RESULT" 2>/dev/null; then
        echo "headless: usb-disk driver steps: $(cat "$USB_DISK_DRIVE_RESULT" 2>/dev/null)" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    if ! python3 -I "$ROOT_DIR/tests/filesystem-fat/native/check-image.py" check "$USB_STICK_DIR/stick1.img"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    rm -rf "$USB_STICK_DIR"
fi

# usb-eject: the driver script's steps (it checked stick A's dirty mark
# after the surprise removal), then Linux checks both sticks after their
# final Eject: B clean; A still dirty (CuBit mounted it read only and never
# repaired it), its files intact (CUBIT_USB_EJECT_KEEP=DIR keeps them).
if [ "$TEST_NAME" = "desktop-session" ] && [ "$DESKTOP_SESSION_LAUNCH" = 1 ]; then
    if ! grep -qx 'done' "$DESKTOP_LAUNCH_RESULT" 2>/dev/null; then
        echo "headless: desktop-launch steps:" >&2
        sed 's/^/headless:   /' "$DESKTOP_LAUNCH_RESULT" >&2 2>/dev/null || true
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    echo "headless: desktop-launch: $(grep -c '^launched' "$DESKTOP_LAUNCH_RESULT") launches, $(grep -c '^closed' "$DESKTOP_LAUNCH_RESULT") closes"
fi
if [ "$TEST_NAME" = "usb-eject" ]; then
    if [ -n "${CUBIT_USB_EJECT_KEEP:-}" ]; then
        mkdir -p "$CUBIT_USB_EJECT_KEEP" && cp "$USB_STICK_DIR"/*.img "$CUBIT_USB_EJECT_KEEP/"
    fi
    if ! grep -qx 'done' "$USB_EJECT_DRIVE_RESULT" 2>/dev/null; then
        echo "headless: usb-eject driver steps: $(cat "$USB_EJECT_DRIVE_RESULT" 2>/dev/null)" >&2
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    if ! python3 -I "$ROOT_DIR/tests/usb-eject/check-sticks.py" "$USB_STICK_DIR"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    rm -rf "$USB_STICK_DIR"
fi

# fat32-volume: Linux checks the stick CuBit wrote (CUBIT_FAT32_KEEP=PATH
# keeps a copy of it).
if [ "$TEST_NAME" = "fat32-volume" ]; then
    if [ -n "${CUBIT_FAT32_KEEP:-}" ]; then
        cp "$TEMP_SECOND_NVME" "$CUBIT_FAT32_KEEP"
    fi
    if ! python3 -I "$ROOT_DIR/tests/filesystem-fat/native/check-image.py" check "$TEMP_SECOND_NVME"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

if [ "$TEST_NAME" = "multi-instance" ]; then
    if ! python3 "$ROOT_DIR/tests/multi-instance/check.py" "$SERIAL_LOG"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

# MET-002's app: the live series, its own read back, its measurements.
if [ "$TEST_NAME" = "metricsview" ]; then
    if ! python3 -I "$ROOT_DIR/tests/metricsview/check_metricsview.py" "$SERIAL_LOG"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

# Refusals (CAP-003): a declaration that fell short fails these tests.
if [ "$TEST_NAME" = "startup-health" ] || [ "$TEST_NAME" = "logs" ] || [ "$TEST_NAME" = "metricsview" ]; then
    if ! python3 "$ROOT_DIR/tests/headless/check-refusals.py" "$SERIAL_LOG"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
fi

# IOMMU-001: DMA protection is a typed boot record, never silent. With
# --iommu the IOMMU is active and no device faults (generic-launch: its
# fault probe gives exactly one typed DMA fault); without it the record
# says DMA is unconfined. cpu-refusal's kernel halts before IOMMU setup, by
# design (CPU_Baseline), so it has no record to check.
if [ "$TEST_NAME" = "cpu-refusal" ]; then
    IOMMU_CHECK=none
elif [ "$IOMMU" = 1 ]; then
    if [ "$TEST_NAME" = "generic-launch" ]; then IOMMU_CHECK=fault; else IOMMU_CHECK=active; fi
else
    IOMMU_CHECK=degraded
fi
if [ "$IOMMU_CHECK" != none ] && ! python3 -I "$ROOT_DIR/tests/headless/check-iommu.py" "$IOMMU_CHECK" "$SERIAL_LOG"; then
    echo "headless: serial log: $SERIAL_LOG" >&2
    exit 1
fi

if [ "$TEST_NAME" = "neon-blast" ] && [ "${CUBIT_NEON_BURN:-0}" = 1 ]; then
    # Beside the burners: frames dropped, and not one underrun after the
    # music started (tests/neon-blast/check_underruns.py).
    if ! python3 -I "$ROOT_DIR/tests/neon-blast/check_underruns.py" "$SERIAL_LOG"; then
        exit 1
    fi
fi
if [ "$TEST_NAME" = "neon-blast" ]; then
    # The soundtrack QEMU's HDA wrote: present, continuous, at a sane level.
    if ! python3 -I "$ROOT_DIR/tests/neon-blast/check_wav.py" "$TEMP_AUDIO"; then
        exit 1
    fi
    # The software path's dynamic resolution (APP-008): each stats line
    # names its window and internal size. CUBIT_NEON_DYNRES=constrained
    # (with a big CUBIT_TEST_VIDEO_SIZE) also requires the resolution to
    # drop, the frames to come back inside the budget, and a raise.
    if ! python3 -I "$ROOT_DIR/tests/neon-blast/check_resolution.py" "$SERIAL_LOG" \
        "${CUBIT_NEON_DYNRES:-plain}"; then
        exit 1
    fi
fi
if [ "$LOAD_KIND" = windows ]; then
    # Every heap growth landed in its own heap (init-bench-audio-windows.ccl).
    SBRK_LOADS_PASSED="$(grep -ac 'SBRK-LOAD: PASS' "$SERIAL_LOG")"
    if [ "$SBRK_LOADS_PASSED" -ne "$SBRK_LOADS_EXPECTED" ]; then
        echo "headless: $SBRK_LOADS_PASSED of $SBRK_LOADS_EXPECTED sbrk loads passed" >&2
        grep -a 'SBRK-LOAD: COMPLETE' "$SERIAL_LOG" >&2
        exit 1
    fi
fi
if [ "$LOAD_KIND" = heap ] || [ "$LOAD_KIND" = windows ]; then
    # Every heap load (init-bench-audio-heap.ccl) ran to the end, no
    # commit or release refused: the guest really held all three heaps.
    HEAP_LOADS_CLEAN="$(grep -Eac 'BENCH-LOAD: COMPLETE heap_rounds= *[1-9][0-9]* failures= 0 ' "$SERIAL_LOG")"
    if [ "$HEAP_LOADS_CLEAN" -ne "$HEAP_LOADS_EXPECTED" ]; then
        echo "headless: $HEAP_LOADS_CLEAN of $HEAP_LOADS_EXPECTED heap loads completed without a failure" >&2
        exit 1
    fi
fi
if [ "$TEST_NAME" = "neon-blast" ] || [ "$TEST_NAME" = "bench-audio" ]; then
    # Every audio period met its deadline (tests/headless/check-audio-periods.py).
    if ! python3 -I "$ROOT_DIR/tests/headless/check-audio-periods.py" "$SERIAL_LOG"; then
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
    REPORT_LOAD_WORKERS="$LOAD_WORKERS"
    if [ "$LOAD_KIND" = heap ]; then
        REPORT_LOAD_WORKERS="$HEAP_LOADS_EXPECTED"
    elif [ "$LOAD_KIND" = windows ]; then
        REPORT_LOAD_WORKERS="$WINDOW_LOADS_EXPECTED"
    fi
    if ! python3 "$ROOT_DIR/tests/performance/report.py" "$SERIAL_LOG" \
        --require-load --load-workers "$REPORT_LOAD_WORKERS" >/dev/null; then
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
# SCHED-001 urgency: beside CPU-bound peers (--load-workers 1..4), keys still
# reach the app within 1 ms in all but INPUT_LOADED_MISSES_PERCENT of the
# samples of each scenario. Input driver, Desktop and app each run their part
# in an urgent turn; before urgency, each waited for a 1.5 ms slice.
INPUT_LOADED_MISSES_PERCENT=10
if [ "$TEST_NAME" = "bench-input" ] && [ "$BENCH_LOAD" = 1 ]; then
    if ! python3 "$ROOT_DIR/tests/performance/report.py" "$SERIAL_LOG" \
        --load-workers "$LOAD_WORKERS" \
        --max-input-misses-percent "$INPUT_LOADED_MISSES_PERCENT" >/dev/null; then
        echo "headless: loaded input missed 1 ms too often (see $SERIAL_LOG)" >&2
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

if [ "$TEST_NAME" = "files" ] && [ "$SCREEN_SETTLE" -eq 0 ]; then
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
if { [ "$TEST_NAME" = "debugger-gui" ] || [ "$TEST_NAME" = "debugger-gui-rs" ]; }; then
    # DBG-001: what the Debugger showed is the demo's real state: its
    # current line is a line of the demo's own source (debug-demo's Ada,
    # or debug-demo-rs's Rust), and it listed variables and data-structure
    # nodes read from the target.
    if [ "$TEST_NAME" = "debugger-gui-rs" ]; then
        dbg_demo=debug-demo-rs
        dbg_sources=("$ROOT_DIR"/userspace/apps/debug-demo-rs/src/*.rs)
    else
        dbg_demo=debug-demo
        dbg_sources=("$ROOT_DIR"/userspace/apps/debug-demo/*.adb)
    fi
    # The demo's: the last current line before Attach to process... moved
    # the Debugger to clock.svc (debugger-gui), checked on its own below.
    dbg_line="$(tr -d '\r' < "$SERIAL_LOG" | sed '/debugger: attach to process clock.svc/,$d' |
      sed -n 's/^.*debugger: current line: //p' | tail -n 1)"
    if [ -z "$(printf '%s' "$dbg_line" | tr -d ' ')" ] ||
       ! cat "${dbg_sources[@]}" | grep -qxF -- "$dbg_line"; then
        echo "headless: the Debugger's current line is not debug-demo source: '$dbg_line'" >&2
        exit 1
    fi
    if ! grep -aEq 'debugger: local [A-Za-z_.:]+ : .+ = .+' "$SERIAL_LOG" ||
       ! grep -aEq 'debugger: node [A-Za-z_.:<>, ]+ fields [1-9]' "$SERIAL_LOG" ||
       ! grep -aEq "debugger: shows $dbg_demo pid [0-9]+ threads [1-9]" "$SERIAL_LOG"; then
        echo "headless: the Debugger showed no variables, nodes or threads of debug-demo" >&2
        exit 1
    fi
    if grep -aF "test double" "$SERIAL_LOG" >/dev/null; then
        echo "headless: the Debugger ran the test double" >&2
        exit 1
    fi
    # Attach to process...: clock.svc (no stub) shown with real frames
    # unwound through the kernel, and a current line of its own source.
    if [ "$TEST_NAME" = "debugger-gui" ]; then
        if ! tr -d '\r' < "$SERIAL_LOG" | grep -aqE 'debugger: shows clock pid [0-9]+ threads [1-9][0-9]* frames [1-9]'; then
            echo "headless: the Debugger showed no clock.svc frames through the kernel" >&2
            exit 1
        fi
        clock_line="$(tr -d '\r' < "$SERIAL_LOG" | sed -n '/debugger: attach to process clock.svc/,$p' |
          sed -n 's/^.*debugger: current line: //p' | tail -n 1)"
        if [ -z "$(printf '%s' "$clock_line" | tr -d ' ')" ] ||
           ! cat "$ROOT_DIR"/userspace/services/clock/*.ad[sb] "$ROOT_DIR"/userspace/runtime/gnat/*.ad[sb] |
             grep -qxF -- "$clock_line"; then
            echo "headless: the Debugger's current line is not clock.svc's source: '$clock_line'" >&2
            exit 1
        fi
    fi
fi
if [ "$TEST_NAME" = "devices" ]; then
    # HW-002: devmgr binds each present device to the driver that declares
    # it (devices.index) and says so, one line per device; unmatched devices
    # say "-> unmatched" (docs/device-matching.md). Each line carries the
    # function's MSI and MSI-X capabilities: QEMU's virtio-net and NVMe
    # have MSI-X, intel-hda one MSI vector, the host bridge neither.
    # Each match starts a line or follows logstore's mirror prefix anywhere
    # in one: the kernel's own console text (Process.Loader's line, written
    # in pieces) can share the serial line with a mirrored one (k3w25).
    for match in \
        '(^|devmgr\.svc: )devmgr: match [0-9A-F:.]+ 1AF4:1000 class 02\.00\.00 msi=(0|1/[0-9]+) msix=1/[0-9]+ -> virtio-net\.drv \(PCI_IDs 1AF4:1000\.\.1000\)' \
        '(^|devmgr\.svc: )devmgr: match [0-9A-F:.]+ 1B36:0010 class 01\.08\.02 msi=(0|1/[0-9]+) msix=1/[0-9]+ -> nvme\.drv \(PCI_Class 01\.08\.02\)' \
        '(^|devmgr\.svc: )devmgr: match [0-9A-F:.]+ 8086:2668 class 04\.03\.00 msi=1/1 msix=0 -> hda\.drv \(PCI_Class 04\.03\.00\)' \
        '(^|devmgr\.svc: )devmgr: match [0-9A-F:.]+ 8086:29C0 class 06\.00\.00 msi=0 msix=0 -> unmatched' \
        '(^|devmgr\.svc: )devmgr: match platform PS2_Controller -> ps2\.drv' \
        '(^|devmgr\.svc: )devmgr: drivers bound to [0-9]+ PCI devices; [0-9]+ matched no driver'; do
        if ! tr -d '\r' < "$SERIAL_LOG" | grep -Eq "$match"; then
            echo "headless: devmgr match line missing: $match" >&2
            exit 1
        fi
    done
    if tr -d '\r' < "$SERIAL_LOG" | grep -Eq '^(devmgr\.svc: )?devmgr: (match .* -> ambiguous|no valid devices\.index)'; then
        echo "headless: devmgr could not bind by the driver index" >&2
        exit 1
    fi
    # A prefix match would accept the old malformed id with two version-header
    # bytes appended. Require exactly the canonical identity from the loader.
    if ! tr -d '\r' < "$SERIAL_LOG" | grep -Ex '(procmgr\.svc: )?procmgr: pkg id=com\.cubit\.devices' >/dev/null; then
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
# A VM guest's serial console (VMM-001: "guest: " from vm-linux-check,
# "vmm guest: " from vmm.app) is the guest's text, not this machine's: its
# kernel command line says panic=-1, and a guest may oops without the host
# failing. The monitor's own lines are checked like every other.
fault_scan() {
    case "$TEST_NAME" in
        vm-linux|vm-desktop) grep -av -e '^guest: ' -e 'vmm guest: ' "$SERIAL_LOG" ;;
        *) cat "$SERIAL_LOG" ;;
    esac
}
if fault_scan 2>/dev/null | grep -Ei "$FAULT_SIGNATURE" >/dev/null 2>&1; then
    echo "headless: fault signature found in serial log: $SERIAL_LOG" >&2
    fault_scan | grep -Ein "$FAULT_SIGNATURE" >&2
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
    # The boot timeline is a granted publication (ABI-002): capability-test
    # declares none, so its read of the page (0x5B00_0000_9000) faults as
    # unmapped and retires it.
    # The armed line is a record logstore mirrors (ABI-002): it can reach
    # serial after the kernel's own fault line. Only the kernel's two lines
    # are ordered.
    if ! awk '/capability-test: read of the undeclared boot timeline armed/ { seen = 1 }
              /USER-MEMORY-FAULT: pid [0-9]+ address 100055558164480 kind=unmapped/ { fault = 1 }
              fault && /reclaimProcess: stopped PID/ { found = 1 }
              END { exit !(seen && found) }' "$SERIAL_LOG"; then
        echo "headless: an undeclared process could read the boot timeline" >&2
        exit 1
    fi
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
if [ "$TEST_NAME" = "desktop-display" ] || [ "$TEST_NAME" = "display-latency" ]; then
    # LAT-002: timing builds compare idle damage-only frames with a full
    # repaint (Desktop's checkFullRepaint); any difference is a damage or
    # culling bug. desktop-display needs the timing build, so it must check.
    if grep -aE 'COMPOSITOR-REPAINT-CHECK: .* mismatched_px=[1-9]' "$SERIAL_LOG"; then
        echo "headless: a damage-only frame differs from a full repaint" >&2
        exit 1
    fi
    if [ "$TEST_NAME" = "desktop-display" ] && ! grep -aqF 'COMPOSITOR-REPAINT-CHECK: ' "$SERIAL_LOG"; then
        echo "headless: no full-repaint check ran (build with CUBIT_COMPOSITOR_TIMING=on)" >&2
        exit 1
    fi
    echo "headless: full-repaint checks: $(grep -acF 'COMPOSITOR-REPAINT-CHECK: ' "$SERIAL_LOG")"
fi
if [ "$TEST_NAME" = "display-latency" ] || [ "$TEST_NAME" = "metrics-latency" ]; then
    # LAT-002: the merged report; budgets printed, not enforced.
    if ! python3 -I "$ROOT_DIR/tests/headless/display-latency-summary.py" "$SERIAL_LOG"; then
        echo "headless: serial log: $SERIAL_LOG" >&2
        exit 1
    fi
    grep -aE "virtio-gpu: |display: stats " "$SERIAL_LOG" | tail -n 3 || true
    if [ "$TEST_NAME" = "display-latency" ] && [ "${CUBIT_LATENCY_LOAD:-0}" -gt 0 ]; then
        # SCHED-001: beside the CPU-bound peers, the driver-to-app stages
        # stay within the input limit (an urgent turn at each hop).
        if ! python3 -I "$ROOT_DIR/tests/input-urgency/check_latency.py" "$SERIAL_LOG" \
            "${CUBIT_LATENCY_INPUT:-ps2}"; then
            echo "headless: loaded input missed the limit (see $SERIAL_LOG)" >&2
            exit 1
        fi
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
if [ "$TEST_NAME" = "fs-coherence-hard-links" ]; then
    # The disk as Linux would mount it (journal replayed, orphans
    # completed) must be clean: every link count right, no leaked or
    # doubly freed inode or block, and both names gone (FS-031).
    e2fsck -fy -E journal_only "$TEMP_DISK" >/dev/null 2>&1
    if ! e2fsck -fn "$TEMP_DISK"; then
        echo "headless: $TEST_NAME left an inconsistent disk" >&2
        exit 1
    fi
    for HARD_LINK_NAME in hard-links-a hard-links-b; do
        if debugfs -R "stat fs-coherence/$HARD_LINK_NAME" "$TEMP_DISK" 2>/dev/null | grep -q '^Inode:'; then
            echo "headless: $TEST_NAME left fs-coherence/$HARD_LINK_NAME" >&2
            exit 1
        fi
    done
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
if [ -n "$TEMP_AUDIO" ]; then
    if [ "$KEEP_LOGS" -eq 1 ]; then
        echo "headless: audio capture: $AUDIO_KEPT_DIR/$(basename "$TEMP_AUDIO")"
    else
        echo "headless: audio capture: removed (--keep-logs keeps it)"
    fi
fi
