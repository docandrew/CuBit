/* Test-only link wrappers: identify failed libc allocations without modifying Mesa. */
#include <stddef.h>
#include <stdio.h>
#include <errno.h>
#include <cubit/debug.h>
void *__real_malloc(size_t);
void *__real_calloc(size_t,size_t);
void *__real_realloc(void *,size_t);
void *__real_aligned_alloc(size_t,size_t);
int __real_posix_memalign(void **,size_t,size_t);
void *__wrap_malloc(size_t);
void *__wrap_calloc(size_t,size_t);
void *__wrap_realloc(void *,size_t);
void *__wrap_aligned_alloc(size_t,size_t);
int __wrap_posix_memalign(void **,size_t,size_t);
static void failed(const char *name,size_t size,int error)
{
    char text[160];
    int n=snprintf(text,sizeof text,"COMPOSITOR-ALLOC: %s size=%zu error=%d\n",name,size,error);
    cubit_debug_write(text,(size_t)n);
}
void *__wrap_malloc(size_t n)
{ void *p=__real_malloc(n);if(!p&&n) failed("malloc",n,errno);return p; }
void *__wrap_calloc(size_t n,size_t s)
{ void *p=__real_calloc(n,s);if(!p&&n&&s) failed("calloc",s,errno);return p; }
void *__wrap_realloc(void *old,size_t n)
{ void *p=__real_realloc(old,n);if(!p&&n) failed("realloc",n,errno);return p; }
void *__wrap_aligned_alloc(size_t a,size_t n)
{ void *p=__real_aligned_alloc(a,n);if(!p&&n) failed("aligned_alloc",n,errno);return p; }
int __wrap_posix_memalign(void **p,size_t a,size_t n)
{ int e=__real_posix_memalign(p,a,n);if(e) failed("posix_memalign",n,e);return e; }
