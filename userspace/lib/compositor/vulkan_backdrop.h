#ifndef CUBIT_VULKAN_BACKDROP_H
#define CUBIT_VULKAN_BACKDROP_H
#include "vulkan_affine.h"
/* SPARK owns placement and clipping. Physical coordinates ignore desktop DPI.
 * Source is BGRA8, same device, exact source_w/source_h, sampled-read layout.
 * All draw/engine/descriptor/image leases outlive actual GPU completion.
 * Pixels outside the placed image preserve the previously painted background.
 */
struct cubit_vulkan_backdrop {
    int64_t left,top;
    uint64_t width,height;
    uint32_t clip_x,clip_y,clip_w,clip_h,source_w,source_h;
};
/* 0 recorded, 1 rejected before any command. Never completion evidence. */
uint32_t cubit_vulkan_record_backdrop(void *borrowed,
    const struct cubit_vulkan_backdrop *draw,uint32_t width,uint32_t height);
#endif
