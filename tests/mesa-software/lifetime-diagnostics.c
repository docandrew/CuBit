/* Single-threaded lifecycle probe only; preserves real munmap behavior. */
#include <stddef.h>
#include <stdint.h>
int __real_munmap(void *, size_t);
int __wrap_munmap(void *, size_t);
static uint64_t released_bytes;
uint64_t cubit_lifetime_freed_bytes(void);
uint64_t cubit_lifetime_freed_bytes(void) { return released_bytes; }
int __wrap_munmap(void *base, size_t size)
{
    int result = __real_munmap(base, size);
    if (!result) released_bytes += size;
    return result;
}
