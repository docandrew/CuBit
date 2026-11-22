#include <stdint.h>
#include <assert.h>
struct description { void *description, *a, *b, *c; uint32_t allowed; };
static uint32_t calls;
uint32_t device_targets_metadata_calls(void) { return calls; }
uint32_t cubit_vulkan_device_targets_prepare(uint32_t width, uint32_t height, struct description *out)
{
    ++calls; assert(width==32 && (height==24 || height==0));
    *out=(struct description){(void *)(uintptr_t)44,(void *)(uintptr_t)1,
        (void *)(uintptr_t)2,(void *)(uintptr_t)3,1};
    /* Deliberately dirty failure output: Ada must not pass it to owners. */
    return height==0 ? 1 : 0;
}
