/* Small C boundary probe for the ported Mesa C API, not a new renderer.
 * Must run as a CuBit ELF: successful host linking is not execution evidence.
 */
#include <stdint.h>
#include <string.h>
#include <cubit/debug.h>
#include "pipe/p_context.h"
#include "pipe/p_screen.h"
#include "pipe/p_state.h"
#include "softpipe/sp_public.h"
#include "null/null_sw_winsys.h"
#include "util/u_simple_shaders.h"
#include "util/u_draw.h"
#include "util/detect_os.h"

_Static_assert(DETECT_OS_CUBIT && !DETECT_OS_LINUX,
               "Native probe must use CuBit platform detection, not Linux");

static int report(int code, const char *message)
{
    cubit_debug_write(message, strlen(message));
    return code;
}

int main(void)
{
    report(0, "SOFTPIPE-NATIVE: starting\n");
    struct sw_winsys *ws = null_sw_create();
    struct pipe_screen *screen = ws ? softpipe_create_screen(ws) : NULL;
    if (!screen) return report(1, "SOFTPIPE-NATIVE: FAIL screen\n");
    struct pipe_context *ctx = screen->context_create(screen, NULL, 0);
    if (!ctx) return report(2, "SOFTPIPE-NATIVE: FAIL context\n");
    struct pipe_resource desc = {
        .target = PIPE_TEXTURE_2D, .format = PIPE_FORMAT_R8G8B8A8_UNORM,
        .width0 = 32, .height0 = 32, .depth0 = 1, .array_size = 1,
        .bind = PIPE_BIND_RENDER_TARGET, .usage = PIPE_USAGE_DEFAULT,
    };
    struct pipe_resource *image = screen->resource_create(screen, &desc);
    if (!image) return report(3, "SOFTPIPE-NATIVE: FAIL resource\n");
    struct pipe_surface surface = { .texture = image, .format = desc.format };
    union pipe_color_union color = { .f = {1.0f, 0.0f, 0.0f, 1.0f} };
    ctx->clear_render_target(ctx, &surface, &color, 0, 0, 32, 32, false);
    struct pipe_box box = { .width = 32, .height = 32, .depth = 1 };
    struct pipe_transfer *transfer = NULL;
    const uint8_t *pixels = ctx->texture_map(ctx, image, 0, PIPE_MAP_READ,
                                           &box, &transfer);
    if (!pixels || !transfer) return report(4, "SOFTPIPE-NATIVE: FAIL map\n");
    unsigned errors = 0;
    for (unsigned y = 0; y < 32; ++y)
        for (unsigned x = 0; x < 32; ++x) {
            const uint8_t *p = pixels + y * transfer->stride + x * 4;
            errors += p[0] != 255 || p[1] != 0 || p[2] != 0 || p[3] != 255;
        }
    ctx->texture_unmap(ctx, transfer);
    if (errors) return report(5, "SOFTPIPE-NATIVE: FAIL pixels\n");
    report(0, "SOFTPIPE-NATIVE: PASS 1024 pixels\n");

    /* NDC right triangle maps to (0,0), (32,0), (0,32). Check all pixel
     * centers except the diagonal, whose edge ownership is rasterizer-defined.
     * Use green over red so a no-op draw cannot satisfy the oracle. */
    const float vertices[3][8] = {
        {-1,-1,0,1, 0,1,0,1}, {1,-1,0,1, 0,1,0,1}, {-1,1,0,1, 0,1,0,1}
    };
    const enum tgsi_semantic semantics[] = {TGSI_SEMANTIC_POSITION, TGSI_SEMANTIC_COLOR};
    const unsigned indexes[] = {0, 0};
    void *vs = util_make_vertex_passthrough_shader(ctx, 2, semantics, indexes, false);
    void *fs = util_make_fragment_passthrough_shader(ctx, TGSI_SEMANTIC_COLOR,
                                                    TGSI_INTERPOLATE_LINEAR, false);
    if (!vs || !fs) return report(6, "SOFTPIPE-NATIVE: FAIL shaders\n");
    struct pipe_blend_state blend = {0};
    blend.rt[0].colormask = PIPE_MASK_RGBA;
    struct pipe_depth_stencil_alpha_state depth = {0};
    struct pipe_rasterizer_state raster = {0};
    raster.line_width = 1;
    raster.point_size = 1;
    struct pipe_vertex_element elements[2] = {
        {.src_offset=0, .src_format=PIPE_FORMAT_R32G32B32A32_FLOAT, .src_stride=32},
        {.src_offset=16, .src_format=PIPE_FORMAT_R32G32B32A32_FLOAT, .src_stride=32}
    };
    void *bs = ctx->create_blend_state(ctx, &blend);
    void *ds = ctx->create_depth_stencil_alpha_state(ctx, &depth);
    void *rs = ctx->create_rasterizer_state(ctx, &raster);
    void *ve = ctx->create_vertex_elements_state(ctx, 2, elements);
    if (!bs || !ds || !rs || !ve) return report(7, "SOFTPIPE-NATIVE: FAIL draw state\n");
    ctx->bind_blend_state(ctx, bs);
    ctx->bind_depth_stencil_alpha_state(ctx, ds);
    ctx->bind_rasterizer_state(ctx, rs);
    ctx->bind_vs_state(ctx, vs);
    ctx->bind_fs_state(ctx, fs);
    ctx->bind_vertex_elements_state(ctx, ve);
    struct pipe_vertex_buffer vb = {.is_user_buffer=true, .buffer.user=vertices};
    ctx->set_vertex_buffers(ctx, 1, &vb);
    struct pipe_framebuffer_state fb = {.width=32, .height=32, .nr_cbufs=1};
    fb.cbufs[0] = surface;
    ctx->set_framebuffer_state(ctx, &fb);
    struct pipe_viewport_state viewport = {.scale={16,16,1}, .translate={16,16,0}};
    ctx->set_viewport_states(ctx, 0, 1, &viewport);
    util_draw_arrays(ctx, MESA_PRIM_TRIANGLES, 0, 3);
    ctx->flush(ctx, NULL, 0);
    transfer = NULL;
    pixels = ctx->texture_map(ctx, image, 0, PIPE_MAP_READ, &box, &transfer);
    if (!pixels || !transfer) return report(8, "SOFTPIPE-NATIVE: FAIL triangle map\n");
    for (unsigned y = 0; y < 32; ++y)
        for (unsigned x = 0; x < 32; ++x) {
            if (x + y == 31) continue;
            const uint8_t *p = pixels + y * transfer->stride + x * 4;
            const int inside = x + y < 31;
            errors += p[0] != (inside ? 0 : 255) || p[1] != (inside ? 255 : 0) ||
                      p[2] != 0 || p[3] != 255;
        }
    ctx->texture_unmap(ctx, transfer);
    ctx->set_vertex_buffers(ctx, 0, NULL);
    ctx->bind_vs_state(ctx, NULL); ctx->bind_fs_state(ctx, NULL);
    ctx->bind_blend_state(ctx, NULL); ctx->bind_rasterizer_state(ctx, NULL);
    ctx->bind_depth_stencil_alpha_state(ctx, NULL); ctx->bind_vertex_elements_state(ctx, NULL);
    ctx->delete_vs_state(ctx, vs); ctx->delete_fs_state(ctx, fs);
    ctx->delete_blend_state(ctx, bs); ctx->delete_rasterizer_state(ctx, rs);
    ctx->delete_depth_stencil_alpha_state(ctx, ds); ctx->delete_vertex_elements_state(ctx, ve);
    fb.nr_cbufs = 0;
    memset(fb.cbufs, 0, sizeof fb.cbufs);
    ctx->set_framebuffer_state(ctx, &fb);
    screen->resource_destroy(screen, image);
    ctx->destroy(ctx);
    screen->destroy(screen);
    return report(errors ? 9 : 0, errors ? "SOFTPIPE-NATIVE: FAIL triangle pixels\n" :
                  "SOFTPIPE-NATIVE: PASS triangle 992 pixels\n");
}
