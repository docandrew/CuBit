#ifndef CUBIT_COMPOSITOR_H
#define CUBIT_COMPOSITOR_H
#include <stdint.h>
/* Private Mesa FFI. No IPC identities, allocation authority or scene policy.
 * Every image is retained caller storage; the adapter never copies its pixels
 * into an upload buffer. Caller validates geometry, authority and non-aliasing.
 */
struct cubit_mesa_image { void *pixels; uint32_t width, height, pitch, writable; };
struct cubit_mesa_draw {
    uint32_t sx, sy, sw, sh, dx, dy, dw, dh;
    uint32_t clip_x, clip_y, clip_w, clip_h, over;
};
/* NULL on initialization/import failure. Images belong to exactly one context.
 * Context must outlive its imported images. All access is single-threaded. */
void *cubit_mesa_create(void);
void *cubit_mesa_import(void *context, const struct cubit_mesa_image *image);
/* 0=completed, 1=rejected before writes, 2=failed but quiescent,
 * 3=uncertain retained access: do not reuse/retire; compositor restart required.
 * Success includes source and target unmapping, not display publication. */
uint32_t cubit_mesa_draw(void *context, void *target, void *source,
                         const struct cubit_mesa_draw *draw);
uint32_t cubit_mesa_release(void *context, void *image);
void cubit_mesa_destroy(void *context);
#endif
