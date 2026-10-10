#!/usr/bin/env bash
# Reproducible ANV adaptation, separate from the software Mesa source cache.
# Run inside Nix. Input is the pristine pinned Mesa tarball, never patched.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
source_tree=${1:?pristine Mesa 26.2.3 source required}
destination=${2:?new destination directory required}
test "$(<"$source_tree/VERSION")" = 26.2.3
bash "$root/tests/mesa-software/prepare-cubit-source.sh" "$source_tree" "$destination"
chmod u+w "$destination/src/vulkan/wsi" \
  "$destination/src/vulkan/wsi/wsi_common_headless.c" \
  "$destination/include/drm-uapi" "$destination/include/drm-uapi/drm_fourcc.h" \
  "$destination/src/intel/dev" \
  "$destination/src/intel/dev/intel_device_info.c" \
  "$destination/src/intel/dev/intel_device_info.h" \
  "$destination/src/intel/dev/intel_kmd.h"
for adaptation in cubit-anv.patch runtime-finalize.patch; do
  patch --batch --fuzz=0 -d "$destination" -p1 < "$root/tests/mesa-anv/$adaptation"
done
chmod u+w "$destination/src/intel/dev" "$destination/src/intel/dev/i915" \
  "$destination/src/intel/dev/i915/intel_device_info.c" \
  "$destination/src/intel/dev/i915/intel_device_info.h" \
  "$destination/src/intel/dev/intel_hwconfig.c" "$destination/src/intel/dev/meson.build" \
  "$destination/src/intel/isl" "$destination/src/intel/isl/isl_drm.c" \
  "$destination/src/intel/common" \
  "$destination/src/intel/common/intel_common.c" \
  "$destination/src/intel/common/intel_common.h" \
  "$destination/src/intel/common/intel_engine.c" \
  "$destination/src/intel/common/meson.build" \
  "$destination/src/intel/common/intel_gem.h" \
  "$destination/src/intel/common/intel_aux_map.c" \
  "$destination/src/intel/vulkan" "$destination/src/intel/vulkan/anv_perf.c" \
  "$destination/src/intel/perf" "$destination/src/intel/perf/intel_perf.c" \
  "$destination/src/intel/perf/intel_perf_mdapi.c" \
  "$destination/src/intel/perf/meson.build" \
  "$destination/src/intel/ds" "$destination/src/intel/ds/intel_driver_ds.cc" \
  "$destination/src/intel/vulkan/anv_private.h" \
  "$destination/src/intel/vulkan/anv_wsi.c" \
  "$destination/src/intel/vulkan/anv_instance.c" \
  "$destination/src/intel/vulkan/anv_physical_device.c" \
  "$destination/src/intel/vulkan/anv_formats.c" \
  "$destination/src/intel/vulkan/meson.build" \
  "$destination/src/intel/vulkan/anv_kmd_backend.c" \
  "$destination/src/intel/vulkan/anv_batch_chain.c" \
  "$destination/src/intel/vulkan/anv_device.c" \
  "$destination/src/intel/vulkan/anv_queue.c" \
  "$destination/src/intel/vulkan/anv_sparse.c" \
  "$destination/src/intel/vulkan/anv_allocator.c" \
  "$destination/src/intel/vulkan/anv_gem.c" \
  "$destination/src/intel/vulkan/anv_kmd_backend.h" \
  "$destination/src/intel/vulkan/i915" "$destination/src/intel/vulkan/xe" \
  "$destination/src/intel/vulkan/i915/anv_kmd_backend.c" \
  "$destination/src/intel/vulkan/xe/anv_kmd_backend.c" \
  "$destination/src/util/futex.h" "$destination/src/util/futex.c" \
  "$destination/src/util/build_id.c" \
  "$destination/src/vulkan/runtime" "$destination/src/vulkan/runtime/vk_image.h" \
  "$destination/src/vulkan/runtime/vk_image.c"
for adaptation in device-platform.patch device-topology.patch device-build.patch \
  isl-platform.patch engine-platform.patch common-build.patch address-header.patch \
  perf-policy.patch perf-results.patch perf-portable.patch tracing-headers.patch anv-header.patch \
  image-modifier.patch external-memory-policy.patch anv-backend-build.patch \
  futex-platform.patch batch-chain-header.patch context-backend.patch \
  unused-device-wait.patch physical-backend.patch physical-common.patch \
  physical-cleanup.patch physical-destroy.patch native-optional-services.patch \
  buffer-unmap.patch cpu-mapping-state.patch cpu-mapping-build.patch \
  instance-platform.patch memory-contract.patch native-kmd.patch budget-snapshot.patch \
  native-build-id.patch native-device-trace.patch native-state-table.patch; do
  patch --batch --fuzz=0 -d "$destination" -p1 < "$root/tests/mesa-anv/$adaptation"
done
# Keep the upstream adaptation reproducible from our owned transport sources.
# These compile into ANV, but do not select or advertise a usable backend.
for transport in anv_cubit_memory.c anv_cubit_memory.h anv_cubit_sync.c anv_cubit_sync.h \
  anv_cubit_sync_types.c anv_backend_sync.h anv_cubit_physical.c anv_cubit_physical.h \
  native_gpu_mapping.c native_gpu_mapping.h native_gpu_buffers.h native_gpu_memory.h native_gpu_query.h \
  native_gpu_queue.h native_gpu_timeline.h \
  anv_cubit_state_table.c anv_cubit_state_table.h \
  cubit-device-query.c cubit-device-query.h cubit-device-info.c cubit-device-native.c \
  cubit-topology.c cubit-topology.h cubit-memory-info.c cubit-memory-info.h; do
  cp "$root/userspace/mesa/anv/$transport" "$destination/src/intel/vulkan/$transport"
done
echo "Prepared complete CuBit ANV source: $destination (native backend still incomplete)"
