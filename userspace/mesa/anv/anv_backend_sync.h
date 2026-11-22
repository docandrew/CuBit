#pragma once
#include "vk_device.h"
#include "vk_physical_device.h"
#include "vk_sync.h"

/* Internal ANV fences use the selected transport, never a hard-coded DRM
 * syncobj. The physical device owns this immutable, priority-ordered list. */
static inline VkResult
anv_backend_sync_create(struct vk_device *device, enum vk_sync_flags flags,
                        uint64_t initial_value, struct vk_sync **out)
{
   if (flags & ~VK_SYNC_IS_TIMELINE)
      return VK_ERROR_FEATURE_NOT_PRESENT;
   uint32_t required = VK_SYNC_FEATURE_CPU_WAIT | VK_SYNC_FEATURE_GPU_WAIT |
      (flags & VK_SYNC_IS_TIMELINE ? VK_SYNC_FEATURE_TIMELINE : VK_SYNC_FEATURE_BINARY);
   const struct vk_sync_type *const *types = device->physical->supported_sync_types;
   if (types)
      for (; *types; types++)
         if (((*types)->features & required) == required)
            return vk_sync_create(device, *types, flags, initial_value, out);
   return VK_ERROR_FEATURE_NOT_PRESENT;
}
