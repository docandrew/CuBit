/* Teapot gallery: its Vulkan calls only (pipelines, buffers, recording).
 * The scene (which pot, which shading, every matrix) comes from Ada
 * (Teapot_Scene); this file turns a draw list into one submitted pass.
 * Externally serialized; one renderer per process. */
#pragma once
#include <stdint.h>
#include "gpu_canvas_vk.h"

/* Number of shading pipelines: Teapot_Scene.Shading'Pos range. */
#define TEAPOT_SHADINGS 9u
/* Render targets the widget lends (CuBit.UI.GPU_Canvas slots). */
#define TEAPOT_SLOTS 2u
/* Push constants: mat4 mvp, three rows of the model-view matrix, params. */
#define TEAPOT_PUSH_FLOATS 32u
/* Teapot_Mesh.Vertex: position, normal, uv, barycentric, pad (12 floats). */
#define TEAPOT_VERTEX_BYTES 48u

/* Layout of Teapot_Renderer.Draw (Ada). */
struct teapot_draw {
   int32_t x, y;               /* viewport origin in target pixels */
   uint32_t width, height;     /* viewport size */
   uint32_t shading;           /* 0 .. TEAPOT_SHADINGS-1 */
   uint32_t reserved[3];
   float push[TEAPOT_PUSH_FLOATS];
};

/* Layout of Teapot_Renderer.Target_View (Ada): one lent widget target. */
struct teapot_target {
   uint64_t image, view, ready;   /* VkImage, VkImageView, VkSemaphore */
   uint32_t width, height, format, slot;
   uint32_t generation, reserved;
};

enum teapot_status {
   TEAPOT_OK = 0,
   TEAPOT_FAILED = 1,
   TEAPOT_BAD_ARGUMENT = 2,
};

/* Pipelines for every shading and the vertex buffer (a plain triangle list
 * of vertex_count vertices, vertex_bytes each: Teapot_Mesh.Vertex). */
uint32_t teapot_vk_create(const struct gpu_canvas_device *device, uint32_t color_format,
                          const void *vertices, uint32_t vertex_count, uint32_t vertex_bytes);
/* Record and submit one frame into target: clear, then each draw in its
 * viewport, ending with the image in TRANSFER_SRC_OPTIMAL; signals
 * target->ready. clear is RGBA. */
uint32_t teapot_vk_render(const struct teapot_target *target, const struct teapot_draw *draws,
                          uint32_t count, const float clear[4]);
/* Wait for submitted frames, destroy everything this file created. */
void teapot_vk_destroy(void);
