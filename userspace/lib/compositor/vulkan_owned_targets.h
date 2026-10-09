#ifndef CUBIT_VULKAN_OWNED_TARGETS_H
#define CUBIT_VULKAN_OWNED_TARGETS_H
#include "vulkan_owned_image.h"
#include "vulkan_targets.h"
uint32_t cubit_vulkan_owned_targets_bind(void *description,void *a,void *b,void *c,void *submission);
uint32_t cubit_vulkan_owned_targets_prepare_frame(void *,void *,uint32_t,uint32_t,uint32_t,uint32_t);
/* Private completed target and destination staging; caller holds pool readback
 * ticket and recording command outside a render pass. No wait or publication.
 * Tight BGRA rows, offset zero. Restores target layout for subsequent rendering.
 * CPU reads require this transfer's completion even on coherent memory. */
uint32_t cubit_vulkan_owned_targets_record_readback(void *,void *,void *,uint32_t);
/* Bounded same-coordinate repair regions. Offsets retain the full-image row
 * layout, but only these pixels become CPU-readable after transfer completion. */
struct cubit_vulkan_readback_region { uint32_t left,top,right,bottom; };
uint32_t cubit_vulkan_owned_targets_record_readback_regions(void *,void *,void *,uint32_t,
    uint32_t,const struct cubit_vulkan_readback_region *);
#endif
