#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include "util/format/u_formats.h"
#include "frontend/sw_winsys.h"
#include "pipe/p_defines.h"
#include "buffer-winsys.h"

struct sw_displaytarget {
    void *pixels;
    unsigned width, height, pitch;
    bool claimed;
};
struct buffer_winsys {
    struct sw_winsys base;
    struct sw_displaytarget targets[2];
    unsigned count;
};

static bool supported(struct sw_winsys *ws, unsigned usage, enum pipe_format format)
{
    (void)ws; (void)usage;
    return format == PIPE_FORMAT_B8G8R8A8_UNORM;
}
static struct sw_displaytarget *create(struct sw_winsys *ws, unsigned usage,
    enum pipe_format format, unsigned width, unsigned height, unsigned alignment,
    const void *private, unsigned *stride)
{
    (void)usage; (void)private;
    struct buffer_winsys *buffers = (struct buffer_winsys *)ws;
    if (!stride || !supported(ws, usage, format) || !alignment)
        return NULL;
    for (unsigned i = 0; i < buffers->count; ++i) {
        struct sw_displaytarget *target = &buffers->targets[i];
        if (!target->claimed && width == target->width && height == target->height &&
            !((uintptr_t)target->pixels % alignment) && !(target->pitch % alignment)) {
            target->claimed = true;
            *stride = target->pitch;
            return target;
        }
    }
    return NULL;
}
static void *map(struct sw_winsys *ws, struct sw_displaytarget *target, unsigned flags)
{
    (void)flags;
    struct buffer_winsys *buffers = (struct buffer_winsys *)ws;
    for (unsigned i = 0; i < buffers->count; ++i)
        if (target == &buffers->targets[i] && target->claimed) return target->pixels;
    return NULL;
}
static void unmap(struct sw_winsys *ws, struct sw_displaytarget *target)
{
    (void)ws; (void)target; /* CPU-owned coherent storage, no publication. */
}
static void destroy_target(struct sw_winsys *ws, struct sw_displaytarget *target)
{
    struct buffer_winsys *buffers = (struct buffer_winsys *)ws;
    for (unsigned i = 0; i < buffers->count; ++i)
        if (target == &buffers->targets[i]) target->claimed = false;
}
static void destroy(struct sw_winsys *ws) { free(ws); }
static struct sw_displaytarget *from_handle(struct sw_winsys *ws,
    const struct pipe_resource *resource, struct winsys_handle *handle, unsigned *stride)
{
    (void)ws; (void)resource; (void)handle; (void)stride;
    return NULL;
}
static bool get_handle(struct sw_winsys *ws, struct sw_displaytarget *target,
    struct winsys_handle *handle)
{
    (void)ws; (void)target; (void)handle;
    return false;
}

struct sw_winsys *cubit_buffer_winsys(const struct cubit_pixel_buffer *buffers,
    unsigned count)
{
    if (!buffers || count < 1 || count > 2) return NULL;
    for (unsigned i = 0; i < count; ++i) {
        const struct cubit_pixel_buffer *b = &buffers[i];
        if (!b->pixels || (uintptr_t)b->pixels % 4096 || !b->width || !b->height ||
            b->width > 65535 || b->height > 65535 || b->pitch < b->width * 4 ||
            b->pitch % 64 || (size_t)b->pitch > b->capacity / b->height ||
            b->capacity > UINTPTR_MAX - (uintptr_t)b->pixels) return NULL;
        for (unsigned j = 0; j < i; ++j)
            if ((uintptr_t)b->pixels < (uintptr_t)buffers[j].pixels + buffers[j].capacity &&
                (uintptr_t)buffers[j].pixels < (uintptr_t)b->pixels + b->capacity)
                return NULL;
    }
    struct buffer_winsys *ws = calloc(1, sizeof *ws);
    if (!ws) return NULL;
    ws->count = count;
    for (unsigned i = 0; i < count; ++i)
        ws->targets[i] = (struct sw_displaytarget){buffers[i].pixels,
            buffers[i].width, buffers[i].height, buffers[i].pitch, false};
    ws->base.destroy = destroy;
    ws->base.is_displaytarget_format_supported = supported;
    ws->base.displaytarget_create = create;
    ws->base.displaytarget_map = map;
    ws->base.displaytarget_unmap = unmap;
    ws->base.displaytarget_destroy = destroy_target;
    ws->base.displaytarget_from_handle = from_handle;
    ws->base.displaytarget_get_handle = get_handle;
    return &ws->base;
}
