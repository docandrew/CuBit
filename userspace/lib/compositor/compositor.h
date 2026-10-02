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
struct cubit_mesa_affine {
    int64_t origin_x, origin_y;
    uint32_t logical_w, logical_h, numerator, denominator, rotation;
    uint32_t clip_x, clip_y, clip_w, clip_h, over;
};
struct cubit_mesa_uv { int64_t u, v; };
struct cubit_mesa_quad {
    struct cubit_mesa_uv corners[4];
    int64_t ud, vd;
    uint32_t width, height;
};
#define CUBIT_MESA_MASK_BATCH_MAX 32
struct cubit_mesa_mask_command {
    void *source;
    struct cubit_mesa_affine draw;
    struct cubit_mesa_quad quad;
    uint32_t argb;
};
uint32_t cubit_mesa_draw_mask_batch(void *context, void *target,
                                  const struct cubit_mesa_mask_command *commands,
                                  uint32_t count);
uint32_t cubit_mesa_draw_affine(void *context, void *target, void *source,
                              const struct cubit_mesa_affine *draw,
                              const struct cubit_mesa_quad *quad);
/* NULL on initialization/import failure. Images belong to exactly one context.
 * Context must outlive its imported images. All access is single-threaded. */
void *cubit_mesa_create(void);
void *cubit_mesa_import(void *context, const struct cubit_mesa_image *image);
/* A8 coverage, read-only, caller-owned: no BGRA expansion or upload copy.
 * Tint is straight-alpha 0xAARRGGBB; blending uses premultiplied coverage.
 * Mask imports use the same release/retirement contract as color images. */
void *cubit_mesa_import_mask(void *context, const struct cubit_mesa_image *image,
                           uint64_t capacity);
uint32_t cubit_mesa_draw_mask(void *context, void *target, void *mask,
                            const struct cubit_mesa_affine *draw,
                            const struct cubit_mesa_quad *quad, uint32_t argb);
/* 0=completed, 1=rejected before writes, 2=failed but quiescent,
 * 3=uncertain retained access: do not reuse/retire; compositor restart required.
 * Success includes source and target unmapping, not display publication. */
uint32_t cubit_mesa_draw(void *context, void *target, void *source,
                         const struct cubit_mesa_draw *draw);
uint32_t cubit_mesa_release(void *context, void *image);
void cubit_mesa_destroy(void *context);
#endif
