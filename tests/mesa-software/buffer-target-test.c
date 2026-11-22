#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <cubit/debug.h>
#include "pipe/p_context.h"
#include "pipe/p_screen.h"
#include "pipe/p_state.h"
#include "frontend/sw_winsys.h"
#include "gallium/drivers/softpipe/sp_public.h"
#include "buffer-winsys.h"

static int result(int failed)
{
    const char *text = failed ? "OPENGL-NATIVE: FAIL caller buffer\n" :
                               "OPENGL-NATIVE: PASS caller buffer 1152 pixels\n";
    cubit_debug_write(text, strlen(text));
    return failed;
}

int cubit_buffer_target_test(void)
{
    void *storage = NULL;
    if (posix_memalign(&storage, 4096, 8192)) return result(1);
    memset(storage, 0xA5, 8192);
    /* Reject undersized backing, zero dimensions and non-page-aligned base. */
    if (cubit_buffer_winsys(&(struct cubit_pixel_buffer){storage, 6143, 48, 24, 256}, 1) ||
        cubit_buffer_winsys(&(struct cubit_pixel_buffer){storage, 8192, 0, 24, 256}, 1) ||
        cubit_buffer_winsys(&(struct cubit_pixel_buffer){(char *)storage + 1, 8191, 48, 24, 256}, 1)) return result(2);
    const struct cubit_pixel_buffer aliases[2] = {
        {storage, 8192, 48, 24, 256}, {storage, 8192, 48, 24, 256},
    };
    if (cubit_buffer_winsys(aliases, 2) || cubit_buffer_winsys(aliases, 0) ||
        cubit_buffer_winsys(aliases, 3)) return result(2);
    struct sw_winsys *ws = cubit_buffer_winsys(
        &(struct cubit_pixel_buffer){storage, 8192, 48, 24, 256}, 1);
    struct pipe_screen *screen = ws ? softpipe_create_screen(ws) : NULL;
    if (!screen) return result(3);
    struct pipe_context *ctx = screen->context_create(screen, NULL, 0);
    if (!ctx) return result(4);
    struct pipe_resource desc = {0};
    desc.target = PIPE_TEXTURE_2D;
    desc.format = PIPE_FORMAT_B8G8R8A8_UNORM;
    desc.width0 = 48; desc.height0 = 24; desc.depth0 = desc.array_size = 1;
    desc.bind = PIPE_BIND_RENDER_TARGET | PIPE_BIND_DISPLAY_TARGET;
    struct pipe_resource *image = screen->resource_create(screen, &desc);
    if (!image || screen->resource_create(screen, &desc)) return result(5);
    union pipe_color_union blue = {.f = {0, 0, 1, 1}};
    struct pipe_surface surface = {.texture = image, .format = desc.format};
    ctx->clear_render_target(ctx, &surface, &blue, 0, 0, 48, 24, false);
    ctx->flush(ctx, NULL, 0);
    /* Observe the caller's original bytes, not a transfer/readback allocation. */
    const uint8_t *pixels = storage;
    int failed = 0;
    for (unsigned y = 0; y < 24; ++y) {
        for (unsigned x = 0; x < 48; ++x) {
            const uint8_t *p = pixels + y * 256 + x * 4;
            failed |= p[0] != 255 || p[1] != 0 || p[2] != 0 || p[3] != 255;
        }
        for (unsigned x = 192; x < 256; ++x) failed |= pixels[y * 256 + x] != 0xA5;
    }
    for (unsigned i = 6144; i < 8192; ++i) failed |= pixels[i] != 0xA5;
    screen->resource_destroy(screen, image);
    ctx->destroy(ctx); screen->destroy(screen);
    free(storage);
    return result(failed);
}
