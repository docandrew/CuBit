#include "cubit-device-query.h"
#include "cubit-topology.h"
#include "intel/dev/intel_device_info.h"

bool
cubit_mesa_query_runtime_device(cubit_gpu_query_call call, void *endpoint,
                                struct intel_device_info *out)
{
   struct intel_device_info info;
   struct cubit_gpu_vm_contract vm;
   if (!out || !cubit_mesa_query_device_defaults(call, endpoint, &info) ||
       !cubit_gpu_query_vm(call, endpoint, &vm))
      return false;
   info.has_context_isolation = vm.private_context;
   info.gtt_size = vm.address_space_size;
   *out = info;
   return true;
}

bool
cubit_mesa_query_device_defaults(cubit_gpu_query_call call, void *endpoint,
                                 struct intel_device_info *out)
{
   struct cubit_gpu_device_snapshot hardware;
   if (!out || !cubit_gpu_query_device(call, endpoint, &hardware))
      return false;
   struct intel_device_info info = {0};
   if (!intel_device_info_init_runtime_defaults(hardware.device, &info) ||
       !cubit_mesa_adln_topology_masks(&info, hardware.dss_mask, hardware.eu_mask))
      return false;
   uint32_t timestamp_hz;
   if (!cubit_gpu_query_timestamp(call, endpoint, &timestamp_hz))
      return false;
   /* Replace offline platform defaults with the driver's retained observation.
    * This supplies conversion frequency, not a timestamp counter read API. */
   info.timestamp_frequency = timestamp_hz;
   info.pci_revision_id = hardware.pci_revision;
   /* Linux v6.16 i915_getparam_ioctl(I915_PARAM_REVISION) returns
    * pdev->revision, and Mesa's i915 discovery assigns it to this field.
    * This is the i915 userspace ABI value, not Linux's enum intel_step.
    * Runtime memory is still pending. */
   info.revision = hardware.pci_revision;
   /* Upstream finalization derives scratch-ID bounds from physical topology,
    * engine prefetch sizes and stepping workarounds. Do not substitute the
    * offline dense topology or reimplement these generation-specific rules.
    * This does not select a KMD or establish memory/queue support. */
   intel_device_info_finalize_runtime(&info);
   *out = info;
   return true;
}
