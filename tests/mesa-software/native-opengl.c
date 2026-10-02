/* CuBit-native Mesa frontend probe. No window-system or Linux DRM shim. */
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <cubit/debug.h>
#include "pipe/p_screen.h"
#include "gallium/drivers/softpipe/sp_public.h"
#include "gallium/winsys/sw/null/null_sw_winsys.h"
#include "state_tracker/st_context.h"
#include "glapi/glapi/glapi.h"

static int report(int code, const char *text)
{
    cubit_debug_write(text, strlen(text));
    return code;
}

static int get_param(struct pipe_frontend_screen *screen, enum st_manager_param param)
{
    (void)screen; (void)param;
    return 0;
}

#define LOAD(result, name, arguments) \
    typedef result (*name##_fn) arguments; \
    name##_fn name = (name##_fn)_mesa_glapi_get_proc_address(#name); \
    if (!name) return report(3, "OPENGL-NATIVE: FAIL dispatch " #name "\n")

static int run_context(void)
{
    report(0, "OPENGL-NATIVE: starting\n");
    struct sw_winsys *winsys = null_sw_create();
    struct pipe_screen *screen = winsys ? softpipe_create_screen(winsys) : NULL;
    if (!screen) return report(1, "OPENGL-NATIVE: FAIL screen\n");
    struct pipe_frontend_screen frontend = { .screen = screen, .get_param = get_param };
    struct st_context_attribs attributes = {0};
    attributes.profile = API_OPENGL_COMPAT;
    attributes.major = 2; attributes.minor = 1;
    enum st_context_error error;
    struct st_context *context = st_api_create_context(&frontend, &attributes, &error, NULL);
    if (!context || !st_api_make_current(context, NULL, NULL))
        return report(2, "OPENGL-NATIVE: FAIL context\n");
    LOAD(const GLubyte *, glGetString, (GLenum));
    LOAD(GLenum, glGetError, (void));
    LOAD(void, glGenFramebuffers, (GLsizei, GLuint *));
    LOAD(void, glBindFramebuffer, (GLenum, GLuint));
    LOAD(void, glGenRenderbuffers, (GLsizei, GLuint *));
    LOAD(void, glBindRenderbuffer, (GLenum, GLuint));
    LOAD(void, glRenderbufferStorage, (GLenum, GLenum, GLsizei, GLsizei));
    LOAD(void, glFramebufferRenderbuffer, (GLenum, GLenum, GLenum, GLuint));
    LOAD(GLenum, glCheckFramebufferStatus, (GLenum));
    LOAD(void, glViewport, (GLint, GLint, GLsizei, GLsizei));
    LOAD(void, glClearColor, (GLfloat, GLfloat, GLfloat, GLfloat));
    LOAD(void, glClear, (GLbitfield));
    LOAD(void, glReadPixels, (GLint, GLint, GLsizei, GLsizei, GLenum, GLenum, void *));
    LOAD(void, glColor4f, (GLfloat, GLfloat, GLfloat, GLfloat));
    LOAD(void, glBegin, (GLenum));
    LOAD(void, glVertex2f, (GLfloat, GLfloat));
    LOAD(void, glVertex3f, (GLfloat, GLfloat, GLfloat));
    LOAD(void, glEnable, (GLenum));
    LOAD(void, glDisable, (GLenum));
    LOAD(void, glDepthFunc, (GLenum));
    LOAD(void, glDepthMask, (GLboolean));
    LOAD(void, glClearDepth, (GLdouble));
    LOAD(void, glEnd, (void));
    LOAD(void, glFinish, (void));
    LOAD(void, glDeleteFramebuffers, (GLsizei, const GLuint *));
    LOAD(void, glDeleteRenderbuffers, (GLsizei, const GLuint *));
    const char *version = (const char *)glGetString(GL_VERSION);
    if (!version) return report(4, "OPENGL-NATIVE: FAIL version\n");
    report(0, "OPENGL-NATIVE: version "); report(0, version); report(0, "\n");
    GLuint framebuffer, color;
    glGenFramebuffers(1, &framebuffer); glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);
    glGenRenderbuffers(1, &color); glBindRenderbuffer(GL_RENDERBUFFER, color);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_RGBA8, 32, 32);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, color);
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
        return report(5, "OPENGL-NATIVE: FAIL framebuffer\n");
    glViewport(0, 0, 32, 32);
    glClearColor(1, 0, 0, 1); glClear(GL_COLOR_BUFFER_BIT);
    uint8_t pixels[32 * 32 * 4];
    memset(pixels, 0xA5, sizeof pixels);
    glReadPixels(0, 0, 32, 32, GL_RGBA, GL_UNSIGNED_BYTE, pixels);
    for (unsigned i = 0; i < 1024; ++i)
        if (pixels[i*4] != 255 || pixels[i*4+1] != 0 ||
            pixels[i*4+2] != 0 || pixels[i*4+3] != 255)
            return report(6, "OPENGL-NATIVE: FAIL clear pixels\n");
    report(0, "OPENGL-NATIVE: PASS clear 1024 pixels\n");
    glColor4f(0, 1, 0, 1);
    glBegin(GL_TRIANGLES);
    glVertex2f(-1, -1); glVertex2f(1, -1); glVertex2f(-1, 1);
    glEnd(); glFinish();
    memset(pixels, 0xA5, sizeof pixels);
    glReadPixels(0, 0, 32, 32, GL_RGBA, GL_UNSIGNED_BYTE, pixels);
    if (glGetError() != GL_NO_ERROR) return report(7, "OPENGL-NATIVE: FAIL GL error\n");
    for (unsigned y = 0; y < 32; ++y)
        for (unsigned x = 0; x < 32; ++x) {
            if (x + y == 31) continue;
            const uint8_t *p = pixels + (y * 32 + x) * 4;
            const int inside = x + y < 31;
            if (p[0] != (inside ? 0 : 255) || p[1] != (inside ? 255 : 0) ||
                p[2] != 0 || p[3] != 255)
                return report(8, "OPENGL-NATIVE: FAIL triangle pixels\n");
        }
    report(0, "OPENGL-NATIVE: PASS triangle 992 pixels\n");

    GLuint depth;
    glGenRenderbuffers(1, &depth); glBindRenderbuffer(GL_RENDERBUFFER, depth);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT24, 32, 32);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, depth);
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
        return report(9, "OPENGL-NATIVE: FAIL depth framebuffer\n");
    glDepthMask(GL_TRUE); glDepthFunc(GL_LESS); glClearDepth(1);
    float depths[32 * 32];
    /* Disabled-depth control first: last draw must win. Enabled depth must
     * preserve near green regardless of order, including actual depth storage.
     * NDC z=-0.5 maps to depth0.25; far z=0.5 maps to depth0.75. */
    for (unsigned enabled = 0; enabled < 2; ++enabled) {
        if (enabled) glEnable(GL_DEPTH_TEST); else glDisable(GL_DEPTH_TEST);
        for (unsigned order = 0; order < 2; ++order) {
            glClearColor(0, 0, 1, 1);
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
            for (unsigned draw = 0; draw < 2; ++draw) {
                const int near = (draw == order);
                const float z = near ? -0.5f : 0.5f;
                glColor4f(near ? 0 : 1, near ? 1 : 0, 0, 1);
                glBegin(GL_QUADS);
                glVertex3f(-1, -1, z); glVertex3f(1, -1, z);
                glVertex3f(1, 1, z); glVertex3f(-1, 1, z);
                glEnd();
            }
            glFinish();
            memset(pixels, 0xA5, sizeof pixels);
            memset(depths, 0xA5, sizeof depths);
            glReadPixels(0, 0, 32, 32, GL_RGBA, GL_UNSIGNED_BYTE, pixels);
            glReadPixels(0, 0, 32, 32, GL_DEPTH_COMPONENT, GL_FLOAT, depths);
            if (glGetError() != GL_NO_ERROR)
                return report(10, "OPENGL-NATIVE: FAIL depth GL error\n");
            const int green = enabled || order == 1;
            const float expected_depth = enabled ? 0.25f : 1.0f;
            for (unsigned i = 0; i < 1024; ++i) {
                const uint8_t *p = pixels + i * 4;
                if (p[0] != (green ? 0 : 255) || p[1] != (green ? 255 : 0) ||
                    p[2] != 0 || p[3] != 255 ||
                    !(depths[i] >= expected_depth - 0.000001f &&
                      depths[i] <= expected_depth + 0.000001f))
                    return report(11, "OPENGL-NATIVE: FAIL depth pixels\n");
            }
        }
    }
    glDisable(GL_DEPTH_TEST);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
    glDeleteFramebuffers(1, &framebuffer); glDeleteRenderbuffers(1, &color);
    glDeleteRenderbuffers(1, &depth);
    st_api_make_current(NULL, NULL, NULL);
    st_destroy_context(context); st_screen_destroy(&frontend); screen->destroy(screen);
    return report(0, "OPENGL-NATIVE: PASS depth 4096 pixels\n");
}

#ifndef CUBIT_CONTEXT_ITERATIONS
#define CUBIT_CONTEXT_ITERATIONS 1
#endif
#if CUBIT_CONTEXT_ITERATIONS > 1
extern uint64_t cubit_lifetime_freed_bytes(void);
#endif
int main(void)
{
    for (unsigned iteration = 0; iteration < CUBIT_CONTEXT_ITERATIONS; ++iteration) {
        unsigned long before = syscall(SYS_brk, 0);
        int result = run_context();
        unsigned long after = syscall(SYS_brk, 0);
        char text[160];
        int n = snprintf(text, sizeof(text),
                         "OPENGL-NATIVE: cycle=%u result=%d heap-before=%lx heap-after=%lx growth=%lu\n",
                         iteration, result, before, after, after - before);
        if (n > 0 && (size_t)n < sizeof(text)) cubit_debug_write(text, n);
#if CUBIT_CONTEXT_ITERATIONS > 1
        n = snprintf(text, sizeof(text), "OPENGL-NATIVE: successful munmap bytes=%lu\n",
                     (unsigned long)cubit_lifetime_freed_bytes());
        if (n > 0 && (size_t)n < sizeof(text)) cubit_debug_write(text, n);
#endif
        if (result) return result;
    }
    return report(0, "OPENGL-NATIVE: PASS context lifecycle\n");
}
