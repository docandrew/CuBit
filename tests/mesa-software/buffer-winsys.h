#ifndef CUBIT_BUFFER_WINSYS_H
#define CUBIT_BUFFER_WINSYS_H
#include <stddef.h>
struct sw_winsys;
struct cubit_pixel_buffer {
    void *pixels;
    size_t capacity;
    unsigned width, height, pitch;
};
/* Single-threaded Mesa boundary adapter for one or two caller-owned BGRA buffers.
 * Caller retains storage until the resource AND screen have been destroyed.
 * This does not lend storage to Desktop or implement presentation completion.
 * No display submission or Linux handle import is supported. */
struct sw_winsys *cubit_buffer_winsys(const struct cubit_pixel_buffer *buffers,
                                    unsigned count);
int cubit_buffer_target_test(void);
#endif
