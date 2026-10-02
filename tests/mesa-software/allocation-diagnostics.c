/* Test-only linker wrappers: observe failures without changing allocator behavior. */
#include <stddef.h>
#include <stdio.h>
#include <cubit/debug.h>
void *__real_malloc(size_t);
void *__real_calloc(size_t, size_t);
void *__real_aligned_alloc(size_t, size_t);
int __real_posix_memalign(void **, size_t, size_t);
static void failed(const char *kind, size_t count, size_t size)
{
    char text[160];
    int n = snprintf(text, sizeof text, "SOFTPIPE-ALLOC: %s count=%zu size=%zu\n", kind, count, size);
    if (n > 0 && (size_t)n < sizeof text) cubit_debug_write(text, (size_t)n);
}
void *__wrap_malloc(size_t size)
{
    void *p = __real_malloc(size);
    if (!p && size) failed("malloc", 1, size);
    return p;
}
void *__wrap_calloc(size_t count, size_t size)
{
    void *p = __real_calloc(count, size);
    if (!p && count && size) failed("calloc", count, size);
    return p;
}
void *__wrap_aligned_alloc(size_t alignment, size_t size)
{
    void *p = __real_aligned_alloc(alignment, size);
    if (!p && size) failed("aligned_alloc", alignment, size);
    return p;
}
int __wrap_posix_memalign(void **p, size_t alignment, size_t size)
{
    int result = __real_posix_memalign(p, alignment, size);
    if (result) failed("posix_memalign", alignment, size);
    return result;
}
