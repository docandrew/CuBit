/* Mesa-to-Desktop probe: alternate two buffers. Attached pixels stay immutable
 * until successful replacement; a present acknowledgement is NOT retirement. */
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <cubit/debug.h>
#include "../../userspace/c/cubit_desktop.h"
#include "pipe/p_context.h"
#include "pipe/p_screen.h"
#include "pipe/p_state.h"
#include "gallium/drivers/softpipe/sp_public.h"
#include "state_tracker/st_context.h"
#include "glapi/glapi/glapi.h"
#include "buffer-winsys.h"

static int report(int code, const char *text)
{
    cubit_debug_write(text, strlen(text));
    return code;
}
#ifndef MESA_WINDOW_CUBE
static bool check_pixels(const void *storage, unsigned frame)
{
    static const uint8_t bgra[4][4] = {
        {0, 0, 255, 255}, {0, 255, 0, 255}, {255, 0, 0, 255}, {255, 255, 255, 255},
    };
    const uint8_t *pixels = storage;
    for (unsigned y = 0; y < 384; ++y)
        for (unsigned x = 0; x < 512; ++x)
            if (memcmp(pixels + (y * 512 + x) * 4,
                       bgra[((y / 192) * 2 + x / 256 + frame) % 4], 4)) return false;
    return true;
}
#else
#include "cube-scene.h"
static uint64_t pixel_hash(const void *storage)
{
    const uint8_t *p = storage;
    uint64_t hash = UINT64_C(14695981039346656037);
    for (unsigned i = 0; i < 512 * 384 * 4; ++i)
        hash = (hash ^ p[i]) * UINT64_C(1099511628211);
    return hash;
}
#endif

struct window_drawable {
    struct pipe_frontend_drawable base;
    struct pipe_resource *image;
    struct pipe_resource *depth;
};
static int get_param(struct pipe_frontend_screen *screen, enum st_manager_param param)
{
    (void)screen; (void)param;
    return 0;
}
static bool validate(struct st_context *st, struct pipe_frontend_drawable *drawable,
    const enum st_attachment_type *attachments, unsigned count,
    struct pipe_resource **out, struct pipe_resource **resolve)
{
    (void)st;
    if (resolve) *resolve = NULL;
    for (unsigned i = 0; i < count; ++i) out[i] = NULL;
    for (unsigned i = 0; i < count; ++i)
        if (attachments[i] != ST_ATTACHMENT_FRONT_LEFT &&
            !(attachments[i] == ST_ATTACHMENT_DEPTH_STENCIL &&
              ((struct window_drawable *)drawable)->depth)) return false;
    for (unsigned i = 0; i < count; ++i)
        pipe_resource_reference(&out[i], attachments[i] == ST_ATTACHMENT_FRONT_LEFT ?
            ((struct window_drawable *)drawable)->image : ((struct window_drawable *)drawable)->depth);
    return true;
}
static bool defer_present(struct st_context *st, struct pipe_frontend_drawable *drawable,
                          enum st_attachment_type attachment)
{
    (void)st; (void)drawable; (void)attachment;
    /* Frame is private until explicit Desktop attachment after glFinish.
     * Do not report a compositor presentation or retirement here. */
    return false;
}
#define LOAD(result, name, arguments) \
    typedef result (*name##_fn) arguments; \
    name##_fn name = (name##_fn)_mesa_glapi_get_proc_address(#name); \
    if (!name) return report(8, "MESA-WINDOW: FAIL dispatch " #name "\n")

static int call(unsigned label, uint64_t a, uint64_t b, uint64_t c, uint64_t d,
                cubit_async_message_t *message)
{
    *message = (cubit_async_message_t){0};
    message->tag.label = label; message->tag.length = 4;
    message->words[0] = a; message->words[1] = b;
    message->words[2] = c; message->words[3] = d;
    return syscall3(SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY, CAP_SLOT_DESKTOP,
                    message, CUBIT_WAIT_FOREVER) != -1 &&
           message->tag.label == label && !message->tag.reserved &&
           (!message->tag.flags || (label == 0x0821 && message->tag.flags == 1));
}

int main(void)
{
    enum { width = 512, height = 384, pitch = width * 4, bytes = pitch * height };
    void *pixels[2] = {NULL, NULL};
    struct cubit_pixel_buffer buffers[2];
    report(0, "MESA-WINDOW: starting\n");
    for (unsigned i = 0; i < 2; ++i) {
        if (posix_memalign(&pixels[i], 4096, bytes)) return report(1, "MESA-WINDOW: FAIL allocation\n");
        memset(pixels[i], 0, bytes);
        buffers[i] = (struct cubit_pixel_buffer){pixels[i], bytes, width, height, pitch};
    }
    struct sw_winsys *ws = cubit_buffer_winsys(buffers, 2);
    struct pipe_screen *screen = ws ? softpipe_create_screen(ws) : NULL;
    if (!screen) return report(2, "MESA-WINDOW: FAIL screen\n");
    struct pipe_resource desc = {0};
    desc.target = PIPE_TEXTURE_2D; desc.format = PIPE_FORMAT_B8G8R8A8_UNORM;
    desc.width0 = width; desc.height0 = height; desc.depth0 = desc.array_size = 1;
    desc.bind = PIPE_BIND_DISPLAY_TARGET | PIPE_BIND_RENDER_TARGET;
    struct pipe_resource *images[2];
    for (unsigned i = 0; i < 2; ++i) {
        images[i] = screen->resource_create(screen, &desc);
        if (!images[i]) return report(3, "MESA-WINDOW: FAIL target\n");
    }
    struct pipe_frontend_screen frontend = {.screen = screen, .get_param = get_param};
    struct st_visual visual = {.buffer_mask = ST_ATTACHMENT_FRONT_LEFT_MASK,
                               .color_format = desc.format};
    struct window_drawable drawable = {
        .base = {.stamp = 1, .ID = 1, .fscreen = &frontend, .visual = &visual,
                 .validate = validate, .flush_front = defer_present},
        .image = images[0],
    };
#ifdef MESA_WINDOW_CUBE
    desc.bind = PIPE_BIND_DEPTH_STENCIL;
    desc.format = PIPE_FORMAT_Z24_UNORM_S8_UINT;
    drawable.depth = screen->resource_create(screen, &desc);
    if (!drawable.depth) return report(13, "MESA-WINDOW: FAIL depth target\n");
    visual.buffer_mask |= ST_ATTACHMENT_DEPTH_STENCIL_MASK;
    visual.depth_stencil_format = desc.format;
    uint64_t hashes[2] = {0, 0};
#endif
    struct st_context_attribs attributes = {0};
    attributes.profile = API_OPENGL_COMPAT;
    attributes.major = 2; attributes.minor = 1; attributes.visual = visual;
    enum st_context_error error;
    struct st_context *context = st_api_create_context(&frontend, &attributes, &error, NULL);
    if (!context || !st_api_make_current(context, &drawable.base, &drawable.base))
        return report(9, "MESA-WINDOW: FAIL GL drawable\n");
    LOAD(void, glViewport, (GLint, GLint, GLsizei, GLsizei));
    LOAD(void, glColor4f, (GLfloat, GLfloat, GLfloat, GLfloat));
    LOAD(void, glBegin, (GLenum));
    LOAD(void, glVertex2f, (GLfloat, GLfloat));
    LOAD(void, glEnd, (void));
    LOAD(void, glFinish, (void));
    LOAD(GLenum, glGetError, (void));
#ifndef MESA_WINDOW_CUBE
    const union pipe_color_union colors[4] = {
        {.f = {1, 0, 0, 1}}, {.f = {0, 1, 0, 1}},
        {.f = {0, 0, 1, 1}}, {.f = {1, 1, 1, 1}},
    };
#endif
    cubit_async_message_t reply;
    if (!call(0x0800, UINT64_C(1) << 32, 0, 0, 0, &reply) ||
        reply.tag.length != 4 || !reply.words[0])
        return report(4, "MESA-WINDOW: FAIL hello\n");
    if (!call(0x0810, width + 20, height + 44, 2, 0, &reply) ||
        reply.tag.length != 4 || !reply.words[0])
        return report(5, "MESA-WINDOW: FAIL create\n");
    uint64_t id = reply.words[0];
    long grants[2];
    for (unsigned i = 0; i < 2; ++i) {
        grants[i] = syscall4(SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
                            CAP_SLOT_DESKTOP, pixels[i], bytes / 4096, 0);
        if (grants[i] < 0) return report(6, "MESA-WINDOW: FAIL grant\n");
    }
    int attached = -1;
    unsigned frame = 0, animated_frames = 0;
    bool initial_complete = false, animate = false, space_down = false;
    uint64_t serial = 0;
    for (;;) {
      if (!initial_complete || animate) {
        unsigned target = frame % 2;
        if (attached == (int)target) return report(11, "MESA-WINDOW: FAIL attached write\n");
        drawable.image = images[target];
        drawable.base.stamp++;
        if (!st_api_make_current(context, &drawable.base, &drawable.base))
            return report(9, "MESA-WINDOW: FAIL drawable replacement\n");
        glViewport(0, 0, width, height);
#ifdef MESA_WINDOW_CUBE
        if (!cubit_draw_cube(frame, width, height)) return report(14, "MESA-WINDOW: FAIL cube dispatch\n");
#else
        for (unsigned i = 0; i < 4; ++i) {
            const float left = (i % 2) ? 0 : -1;
            const float top = (i / 2) ? 0 : 1;
            const float *color = colors[(i + frame) % 4].f;
            glColor4f(color[0], color[1], color[2], 1);
            glBegin(GL_QUADS);
            glVertex2f(left, top - 1); glVertex2f(left + 1, top - 1);
            glVertex2f(left + 1, top); glVertex2f(left, top);
            glEnd();
        }
#endif
        glFinish();
        if (glGetError() != GL_NO_ERROR) return report(10, "MESA-WINDOW: FAIL GL draw\n");
#ifdef MESA_WINDOW_CUBE
        uint64_t hash = pixel_hash(pixels[target]);
        if (hash == hashes[target] ||
            (attached >= 0 && pixel_hash(pixels[attached]) != hashes[attached]))
            return report(12, "MESA-WINDOW: FAIL buffer isolation\n");
        hashes[target] = hash;
#else
        if (!check_pixels(pixels[target], frame) ||
            (attached >= 0 && !check_pixels(pixels[attached], frame - 1)))
            return report(12, "MESA-WINDOW: FAIL buffer isolation\n");
#endif
        /* Only successful replacement retires the previous CPU attachment.
         * On failure/ambiguous response, exit without reusing either buffer. */
        if (cubit_desktop_attach_buffer(id, grants[target], width, height, pitch))
            return report(6, "MESA-WINDOW: FAIL attach\n");
        attached = target;
        if (!call(0x0812, id, 0, 0, 0, &reply) || reply.tag.length != 1 || reply.words[0])
            return report(7, "MESA-WINDOW: FAIL present\n");
        if (initial_complete && animated_frames < 36 && ++animated_frames == 36)
            report(0, "MESA-WINDOW: PASS animated cycle with retired-buffer reuse\n");
        if (!initial_complete && frame == 8) {
            initial_complete = true;
    report(0, "MESA-WINDOW: GL drawable rendered\n");
    report(0, "MESA-WINDOW: PASS 9 frames with retired-buffer reuse\n");
#ifdef MESA_WINDOW_CUBE
    report(0, "MESA-WINDOW: depth-tested cube ready\n");
#endif
    report(0, "MESA-WINDOW: attached immutable Mesa buffer\n");
            report(0, "MESA-WINDOW: Space toggles animation; Escape closes\n");
        }
        /* Bound the angle and frame arithmetic for arbitrarily long runs.
         * 36 is even, preserving alternation across the wrap. */
        frame = (frame + 1) % 36;
      }
        /* Poll while rendering too. Attached storage remains immutable until
         * replacement; exit cleanup must not free an acquired Desktop buffer. */
        if (!call(0x0821, id, serial, 0, 0, &reply) ||
            !cubit_desktop_input_reply_valid(0x0821, reply.tag.label,
                reply.tag.length, reply.tag.flags, reply.tag.reserved, reply.words))
            return report(0, "MESA-WINDOW: surface unavailable; exiting\n");
        if (reply.words[0]) serial = reply.words[1];
        if (reply.words[2] == 0x39) {
            if (reply.words[0] == 2) space_down = false;
            if (reply.words[0] == 1 && !space_down) {
                space_down = true;
                animate = !animate;
                report(0, animate ? "MESA-WINDOW: animation resumed\n" :
                                   "MESA-WINDOW: animation paused\n");
            }
        }
        if (reply.words[0] == 1 && reply.words[2] == 0x01) {
            /* Escape. Destroy withdraws Desktop's attachment before Goodbye.
             * Even on an ambiguous response, process teardown retains pins. */
            call(0x0811, id, 0, 0, 0, &reply);
            call(0x0801, 0, 0, 0, 0, &reply);
            return report(0, "MESA-WINDOW: Escape; exiting\n");
        }
        usleep((!initial_complete || animate) ? 100000 : 16000);
    }
}
