/* CuBit.UI.GPU_Canvas: the widget's Vulkan calls, nothing else.
 *
 * Thin glue between the Ada widget (state, sizing, visibility, which slot is
 * where) and Vulkan. Each target slot owns a colour image the application
 * renders into, a semaphore the application signals when that rendering is
 * submitted, and a persistently mapped host-visible buffer the widget copies
 * the finished image into. No policy lives here: the Ada side decides when a
 * slot is created, presented, polled or destroyed. Externally serialized.
 */
#pragma once
#include <stdint.h>
#include <vulkan/vulkan.h>

/* Same layout as CuBit.UI.GPU_Canvas_Device.Device_View (and the native
 * Mesa_Service.Device_View it is filled from). */
struct gpu_canvas_device {
   VkInstance instance;
   VkPhysicalDevice physical;
   VkDevice device;
   VkQueue queue;
   uint32_t family;
   PFN_vkGetInstanceProcAddr instance_proc;
};

/* Targets: canvases per process times slots per canvas, both fixed by the
 * Ada side (CuBit.UI.GPU_Canvas.Canvas_Limit, GPU_Canvas_Slots.Slot_Count).
 * Every function names a target by its index in 0 .. GPU_CANVAS_TARGETS-1. */
#define GPU_CANVAS_TARGETS 8u
#define GPU_CANVAS_FORMAT VK_FORMAT_B8G8R8A8_UNORM
#define GPU_CANVAS_BYTES_PER_PIXEL 4u

enum gpu_canvas_status {
   GPU_CANVAS_OK = 0,
   GPU_CANVAS_PENDING = 1,
   GPU_CANVAS_FAILED = 2,
   GPU_CANVAS_BAD_ARGUMENT = 3,
};

/* What the application renders into (VkImage/VkImageView/VkSemaphore as
 * 64-bit handles; same layout as the Ada Target_Handles record). */
struct gpu_canvas_target {
   uint64_t image;
   uint64_t view;
   uint64_t ready;      /* VkSemaphore the application signals */
   uint32_t width;
   uint32_t height;
   uint32_t format;     /* VkFormat */
   uint32_t pitch;      /* readback row bytes */
};

/* Load entry points and create the command pool. 0 on success. */
uint32_t gpu_canvas_vk_open(const struct gpu_canvas_device *device);
/* Create slot storage for width x height pixels; fills *target. */
uint32_t gpu_canvas_vk_create(uint32_t slot, uint32_t width, uint32_t height,
                              struct gpu_canvas_target *target);
/* Submit the copy of the slot's image into its buffer, waiting on its ready
 * semaphore at the transfer stage, signalling the slot's fence. */
uint32_t gpu_canvas_vk_present(uint32_t slot);
/* GPU_CANVAS_OK when the copy finished, PENDING while it runs. */
uint32_t gpu_canvas_vk_poll(uint32_t slot);
/* Readback pixels of a finished slot (persistently mapped), or NULL. */
const void *gpu_canvas_vk_pixels(uint32_t slot);
/* Wait for the slot's outstanding copy, then free its storage. */
void gpu_canvas_vk_destroy(uint32_t slot);
/* Wait until the queue is idle (a lent target's semaphore may be signalled
 * without a present waiting on it). */
void gpu_canvas_vk_idle(void);
/* Destroy every slot, the pool and the fences. The device stays the owner's. */
void gpu_canvas_vk_close(void);
