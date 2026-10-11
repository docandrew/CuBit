#ifndef CUBIT_VULKAN_SUBMISSION_H
#define CUBIT_VULKAN_SUBMISSION_H
#include <vulkan/vulkan.h>
#include <stdint.h>
/* Fresh borrowed, externally synchronized objects from one device/queue.
 * Command pool must permit individual command-buffer resets. Fence must not
 * be shared with another submission. SPARK policy supplies call ordering.
 * All source/target/descriptor/pipeline objects remain retained until confirmed
 * completion or successful cancellation of NEVER-submitted commands. No wait,
 * allocation, fence reset after submit, or unknown-operation replay occurs.
 * Queue completion is not scanout retirement or a display latch signal. */
struct cubit_vulkan_submission {
    VkDevice device; VkQueue queue; VkCommandBuffer command; VkFence fence;
    PFN_vkResetFences reset_fences; PFN_vkResetCommandBuffer reset_command;
    PFN_vkBeginCommandBuffer begin; PFN_vkEndCommandBuffer end;
    PFN_vkQueueSubmit submit; PFN_vkGetFenceStatus status;
    PFN_vkCmdBeginRenderPass begin_scene; PFN_vkCmdEndRenderPass end_scene;
    PFN_vkCmdClearAttachments fill;
};
uint32_t cubit_vulkan_submission_init(struct cubit_vulkan_submission *fresh,
    VkDevice device,VkQueue queue,VkCommandBuffer command,VkFence fence,PFN_vkGetDeviceProcAddr proc);
/* Private immutable pass description. Device/pass/framebuffer compatibility
 * and attachment ownership are audited platform obligations. Clear data is
 * borrowed only during Begin_Scene. Exactly one inline subpass is supported. */
struct cubit_vulkan_scene { VkDevice device; VkRenderPassBeginInfo begin; };
uint32_t cubit_vulkan_submission_begin_scene(void *borrowed,void *pass,uint32_t width,uint32_t height);
uint32_t cubit_vulkan_submission_end_scene(void *borrowed);
/* Opaque RGB fill inside the active color attachment; no source image needed. */
uint32_t cubit_vulkan_submission_fill(void *borrowed,uint32_t width,uint32_t height,
    uint32_t left,uint32_t top,uint32_t right,uint32_t bottom,uint32_t rgb);
/* 0 success; Poll alone: 1 not ready; all other outcomes 2 unknown/error. */
uint32_t cubit_vulkan_submission_start(void *borrowed);
uint32_t cubit_vulkan_submission_seal(void *borrowed);
uint32_t cubit_vulkan_submission_submit(void *borrowed);
uint32_t cubit_vulkan_submission_poll(void *borrowed);
uint32_t cubit_vulkan_submission_cancel(void *borrowed);
uint32_t cubit_vulkan_submission_matches(void *borrowed,void *draw);

/* Job accounting (GPU-001 step 3 follow-up): what each submission does.
 * Recorders note their work into the submission being recorded (Desktop
 * records and submits one command buffer at a time); submit tags the job
 * for the GPU driver's per-kind execution-time line and keeps per-kind
 * counts, sizes and submit-to-seen times for Desktop's own line. Kinds are
 * CuBit.GPU_Queues.Kind_* bits. Weak: fixtures without the submission
 * module simply record nothing. */
enum cubit_vulkan_job_kind {
    CUBIT_VULKAN_JOB_UPLOAD = 1,    /* buffer to image: amount in bytes */
    CUBIT_VULKAN_JOB_RENDER = 2,    /* render pass: amount in pixels */
    CUBIT_VULKAN_JOB_READBACK = 4,  /* image to buffer: amount in bytes */
};
void cubit_vulkan_job_note(enum cubit_vulkan_job_kind kind,uint64_t amount) __attribute__((weak));
/* One draw recorded into the render pass: its scissored pixel area (each
 * draw is a quad shaded over its clip rectangle). Source names the recorder
 * (cubit_vulkan_draw_source). */
enum cubit_vulkan_draw_source {
    CUBIT_VULKAN_DRAW_AFFINE = 0,   /* client surfaces, glyphs, icons */
    CUBIT_VULKAN_DRAW_BACKDROP = 1, /* the wallpaper */
    CUBIT_VULKAN_DRAW_CHECKER = 2,
    CUBIT_VULKAN_DRAW_FILL = 3,     /* attachment clears */
    CUBIT_VULKAN_DRAW_SOURCES = 4,
};
void cubit_vulkan_job_note_draw(enum cubit_vulkan_draw_source source,uint64_t pixels) __attribute__((weak));
/* "kinds=jobs/amounts/seen_us(avg/max) ..." since the last report, then
 * reset; NUL-terminated within capacity. Returns the length. */
uint32_t cubit_vulkan_job_report(char *buffer,uint32_t capacity);
#endif
