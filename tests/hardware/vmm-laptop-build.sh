#!/usr/bin/env bash
#  VMM-001 slice 1, the N150 run (VMX on real hardware; no test host here
#  has VMX). Builds the UEFI live image with vmm.app started by its startup
#  plan, and prints what to look for. Run on the build host from this
#  worktree, in the Nix shell environment, holding the shared build lock:
#
#    CUBIT_SPARK_CRATES=... CUBIT_SIGNING=... bash tests/hardware/vmm-laptop-build.sh OUTDIR
#
#  On the laptop (boot the image; nothing else to do):
#  1. The kernel log (log-kernel.txt) shows the N150's VMX report and
#     whether the kernel enabled VMX:
#       virt: vmx usable            (or the first unmet requirement)
#       virt: vmx ept ... / apic ... / other ... / raw ...
#       vm: VMX enabled (VMXON, EPT, VPID); guests run as kernel objects
#  2. A window titled "Guest serial console" (banner: "Linux 6.12 in a CuBit VM")
#     shows Linux booting on two vCPUs and ends at the guest's shell
#     prompt after `uname -a`, `head -5 /proc/cpuinfo` and "CuBit guest:
#     2 CPUs online". One photo of it is the acceptance evidence.
#  3. vmm.app's log lines ("vmm: ..."): "entering the guest at ...", any
#     "VMLAUNCH failed, VM-instruction error N" or "VM entry or EPT failure,
#     VMX exit reason N" (the kernel log), and "guest ended: ...".
#  4. The guest's own SPEC_CTRL (vmx_entry.asm's switching has run nowhere
#     before this): the kernel log has "vm: guest SPEC_CTRL: bits 0x...,
#     switched at entry and exit" with a non-zero mask; the console window
#     shows "CuBit guest: spec_store_bypass: Mitigation: ..." (not
#     "Vulnerable") and a spectre_v2 line with "IBPB: conditional"; the
#     boot reaches the prompt with no "VMLAUNCH failed" and no VMX exit
#     reason 34 (MSR loading) in the kernel log. Without ARCH_CAPABILITIES
#     the guest sees no eIBRS, so retpolines for spectre_v2 are expected.
#  Collect log-kernel.txt and the vmm.app log for the report.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
out=${1:?output directory}
mkdir -p "$out"
cd "$root"
make -C kernel world CUBIT_COMPOSITOR_TIMING=on > "$out/world.log" 2>&1
# Every laptop live image carries the Servo payload (usb-live-iso checks
# for it); this is a no-op when it is already built.
make -C kernel servo > "$out/servo.log" 2>&1
make -C kernel usb-live-vmm-uefi-iso > "$out/iso.log" 2>&1
cp kernel/cubit_live_uefi_vmm.img "$out/cubit-laptop-vmm.img"
(cd "$out" && sha256sum cubit-laptop-vmm.img | tee cubit-laptop-vmm.img.sha256)
(cd kernel/isodir/boot && sha256sum vmm.app) | tee "$out/vmm-app.sha256"
echo "vmm-laptop-build: image $out/cubit-laptop-vmm.img"
