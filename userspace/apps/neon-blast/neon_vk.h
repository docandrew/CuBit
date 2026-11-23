/* Neon Blast's Vulkan calls (neon_vk.c): pipelines, per-slot buffers and
 * one recorded pass a frame. The scene (every number drawn) comes from Ada
 * (Neon_GPU, built from Neon_Scene); this file only turns it into Vulkan
 * work. Externally serialized; one renderer per process.
 * SPDX-License-Identifier: GPL-3.0-or-later */
#pragma once
#include <stdint.h>
#include "gpu_canvas_vk.h"

/* Render targets the widget lends (CuBit.UI.GPU_Canvas slots). */
#define NEON_SLOTS 2u
/* Bytes of Neon_GPU.Globals: 8 + 64 + 32 vec4 rows (shaders/scene.glsl). */
#define NEON_GLOBALS_BYTES 1664u
/* One instance: three vec4 (Neon_GPU.Instance). */
#define NEON_INSTANCE_BYTES 48u

/* Layout of Neon_GPU.Target_View (Ada): one lent widget target. */
struct neon_target {
   uint64_t image, view, ready;   /* VkImage, VkImageView, VkSemaphore */
   uint32_t width, height, format, slot;
   uint32_t generation, reserved;
};

enum neon_status {
   NEON_OK = 0,
   NEON_FAILED = 1,
   NEON_BAD_ARGUMENT = 2,
};

/* Pipelines and per-slot storage for up to max_instances segments. */
uint32_t neon_vk_create(const struct gpu_canvas_device *device, uint32_t color_format,
                        uint32_t max_instances);
/* Record and submit one frame into target: the background, count
 * instances of glowing segments and points, the post passes; the image
 * ends in TRANSFER_SRC_OPTIMAL and target->ready is signalled. */
uint32_t neon_vk_render(const struct neon_target *target, const void *globals,
                        const void *instances, uint32_t count);
/* GPU nanoseconds of the newest finished frame (timestamp queries), or 0
 * when the queue cannot time. */
uint64_t neon_vk_gpu_ns(void);
/* Wait for submitted frames, destroy everything this file created. */
void neon_vk_destroy(void);
