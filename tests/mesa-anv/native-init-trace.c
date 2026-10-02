/* Bring-up probe only. Linker wrappers call the real Mesa functions exactly
 * once, preserving arguments, result and initialization order. No GPU mocks.
 * Wrappers only intercept cross-object references; audit the linked callsites.
 */
#include "anv_private.h"
#include "intel/compiler/brw/brw_compiler.h"
#include "util/build_id.h"
#include <string.h>

extern void cubit_test_mesa_init_stage(const char *, int, int);

/* Serial bring-up probe only: common ANV can collapse several distinct
 * failures to OUT_OF_DEVICE_MEMORY. Preserve calls/results and identify the
 * boundary without changing allocation policy or retrying an operation. */
extern VkResult __real_anv_device_alloc_bo(struct anv_device *, const char *,
   uint64_t, enum anv_bo_alloc_flags, uint64_t, struct anv_bo **);
VkResult __wrap_anv_device_alloc_bo(struct anv_device *, const char *,
   uint64_t, enum anv_bo_alloc_flags, uint64_t, struct anv_bo **);
VkResult __wrap_anv_device_alloc_bo(struct anv_device *device, const char *name,
   uint64_t size, enum anv_bo_alloc_flags flags, uint64_t address, struct anv_bo **bo)
{
   const bool user = name && strcmp(name, "user") == 0;
   if (user) {
      cubit_test_mesa_init_stage("allocation:user-bytes-low", 0, (uint32_t)size);
      cubit_test_mesa_init_stage("allocation:user-bytes-high", 0, size >> 32);
      cubit_test_mesa_init_stage("allocation:user-flags", 0, flags);
      cubit_test_mesa_init_stage("allocation:aux-map-ready", 0,
         device->info && device->info->has_aux_map && device->aux_map_ctx);
   }
   VkResult result = __real_anv_device_alloc_bo(device, name, size, flags, address, bo);
   if (user)
      cubit_test_mesa_init_stage("allocation:user-bo", 1, result);
   return result;
}

extern uint32_t __real_cubit_intel_create_buffer(uint64_t, uint64_t, uint32_t *);
uint32_t __wrap_cubit_intel_create_buffer(uint64_t, uint64_t, uint32_t *);
uint32_t __wrap_cubit_intel_create_buffer(uint64_t slot, uint64_t bytes, uint32_t *handle)
{
   uint32_t status = __real_cubit_intel_create_buffer(slot, bytes, handle);
   if (status) {
      cubit_test_mesa_init_stage("allocation:backing-bytes-low", 1, (uint32_t)bytes);
      cubit_test_mesa_init_stage("allocation:backing-bytes-high", 1, bytes >> 32);
      cubit_test_mesa_init_stage("allocation:backing-status", 1, status);
   }
   return status;
}

extern uint64_t __real_anv_vma_alloc(struct anv_device *, uint64_t, uint64_t,
   enum anv_bo_alloc_flags, uint64_t, struct util_vma_heap **);
uint64_t __wrap_anv_vma_alloc(struct anv_device *, uint64_t, uint64_t,
   enum anv_bo_alloc_flags, uint64_t, struct util_vma_heap **);
uint64_t __wrap_anv_vma_alloc(struct anv_device *device, uint64_t size, uint64_t align,
   enum anv_bo_alloc_flags flags, uint64_t address, struct util_vma_heap **heap)
{
   uint64_t result = __real_anv_vma_alloc(device, size, align, flags, address, heap);
   if (!result) {
      cubit_test_mesa_init_stage("allocation:VA-bytes-low", 1, (uint32_t)size);
      cubit_test_mesa_init_stage("allocation:VA-align-low", 1, (uint32_t)align);
      cubit_test_mesa_init_stage("allocation:VA-failed-flags", 1, flags);
   }
   return result;
}

#define TRACE_VK(name) \
   extern VkResult __real_##name(struct anv_physical_device *); \
   VkResult __wrap_##name(struct anv_physical_device *); \
   VkResult __wrap_##name(struct anv_physical_device *device) { \
      cubit_test_mesa_init_stage(#name, 0, 0); \
      VkResult result = __real_##name(device); \
      cubit_test_mesa_init_stage(#name, 1, result); \
      return result; \
   }

#define TRACE_VOID(name) \
   extern void __real_##name(struct anv_physical_device *); \
   void __wrap_##name(struct anv_physical_device *); \
   void __wrap_##name(struct anv_physical_device *device) { \
      cubit_test_mesa_init_stage(#name, 0, 0); \
      __real_##name(device); \
      cubit_test_mesa_init_stage(#name, 1, 0); \
   }

TRACE_VK(anv_physical_device_init_common)
TRACE_VK(anv_cubit_init_sync_types)
TRACE_VK(anv_init_wsi)
TRACE_VOID(anv_physical_device_init_va_ranges)
TRACE_VOID(anv_physical_device_init_properties)
TRACE_VOID(anv_shader_init_uuid)

extern struct brw_compiler *__real_brw_compiler_create(
   void *, const struct intel_device_info *);
struct brw_compiler *__wrap_brw_compiler_create(void *, const struct intel_device_info *);
struct brw_compiler *__wrap_brw_compiler_create(
   void *context, const struct intel_device_info *info)
{
   cubit_test_mesa_init_stage("brw_compiler_create", 0, 0);
   struct brw_compiler *result = __real_brw_compiler_create(context, info);
   cubit_test_mesa_init_stage("brw_compiler_create", 1, result ? 0 : -1);
   return result;
}

extern void __real_isl_device_init(struct isl_device *, const struct intel_device_info *);
void __wrap_isl_device_init(struct isl_device *, const struct intel_device_info *);
void __wrap_isl_device_init(struct isl_device *device, const struct intel_device_info *info)
{
   cubit_test_mesa_init_stage("isl_device_init", 0, 0);
   __real_isl_device_init(device, info);
   cubit_test_mesa_init_stage("isl_device_init", 1, 0);
}

extern const struct build_id_note *__real_build_id_find_nhdr_for_addr(const void *);
const struct build_id_note *__wrap_build_id_find_nhdr_for_addr(const void *);
const struct build_id_note *__wrap_build_id_find_nhdr_for_addr(const void *address)
{
   cubit_test_mesa_init_stage("build_id_lookup", 0, 0);
   const struct build_id_note *result = __real_build_id_find_nhdr_for_addr(address);
   cubit_test_mesa_init_stage("build_id_lookup", 1, result ? 0 : -1);
   return result;
}
