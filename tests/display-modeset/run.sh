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
   "$src/"
python3 "$here/edid_fixtures.py" "$src/edid_fixtures.ads"
cd "$root/kernel"
alr exec -- gprbuild -p -q -P "$here/display_modeset.gpr"
"$here/build/edid_tests"
"$here/build/intel_tests"
"$here/build/modeset_tests"
"$here/build/aux_tests"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/display_modeset.gpr" -j2 \
        -u cubit-monitor_edid-catalog.adb intel_gpu_transcoder_timing.ads \
           intel_gpu_display_decode.adb intel_gpu_mode_admission.adb gmbus_proof.ads aux_proof.ads \
           intel_gpu_combo_wrpll.adb intel_gpu_modeset_plan.adb
fi
