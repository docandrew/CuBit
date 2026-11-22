#!/usr/bin/env bash
#  Hosted tests for EDID catalogs, mode admission and the Intel display units
#  (DISP-002..006). --prove also runs gnatprove (level 2) on the SPARK units.
#  Run in the Nix shell, from anywhere.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
src="$here/build/source"
mkdir -p "$src"
cp "$root/userspace/runtime/gnat/cubit.ads" \
   "$root"/userspace/lib/display/cubit-monitor_edid.ad? \
   "$root"/userspace/lib/display/cubit-monitor_edid-catalog.ad? \
   "$root"/userspace/services/intel-gpu/intel_gpu_display_registers.ads \
   "$root"/userspace/services/intel-gpu/intel_gpu_transcoder_timing.ads \
   "$root"/userspace/services/intel-gpu/intel_gpu_display_decode.ad? \
   "$root"/userspace/services/intel-gpu/intel_gpu_mode_admission.ad? \
   "$root"/userspace/services/intel-gpu/intel_gpu_gmbus.ad? \
   "$root"/userspace/services/intel-gpu/intel_gpu_dp_aux.ad? \
   "$root"/userspace/services/intel-gpu/intel_gpu_combo_wrpll.ad? \
   "$root"/userspace/services/intel-gpu/intel_gpu_modeset_plan.ad? \
   "$root"/userspace/services/intel-gpu/intel_gpu_hdmi_modeset.ad? \
   "$root"/userspace/services/intel-gpu/intel_gpu_port_detection.ad? \
   "$root"/userspace/services/intel-gpu/intel_gpu_port_scan.ad? \
   "$root"/userspace/services/intel-gpu/intel_gpu_gmbus_outcome.ad? \
   "$root"/userspace/services/intel-gpu/intel_gpu_hotplug_irq.ad? \
   "$root"/userspace/services/intel-gpu/intel_gpu_hotplug_debounce.ad? \
   "$root"/userspace/services/intel-gpu/intel_gpu_msi_enable.ad? \
   "$root"/userspace/services/intel-gpu/intel_gpu_pci_power.ad? \
   "$src/"
python3 "$here/edid_fixtures.py" "$src/edid_fixtures.ads"
cd "$root/kernel"
alr exec -- gprbuild -p -q -P "$here/display_modeset.gpr"
"$here/build/edid_tests"
"$here/build/intel_tests"
"$here/build/modeset_tests"
"$here/build/aux_tests"
"$here/build/detection_tests"
"$here/build/hotplug_tests"
"$here/build/msi_enable_tests"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/display_modeset.gpr" -j2 \
        -u cubit-monitor_edid-catalog.adb intel_gpu_transcoder_timing.ads \
           intel_gpu_display_decode.adb intel_gpu_mode_admission.adb gmbus_proof.ads aux_proof.ads \
           intel_gpu_combo_wrpll.adb intel_gpu_modeset_plan.adb \
           intel_gpu_port_detection.adb detection_proof.ads \
           intel_gpu_hotplug_debounce.adb hotplug_proof.ads intel_gpu_msi_enable.adb
fi
