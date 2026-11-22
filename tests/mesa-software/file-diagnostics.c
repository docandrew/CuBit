/* Test-only observation of Mesa's optional file probes. No extra authority. */
#include <stdio.h>
#include <fcntl.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <cubit/debug.h>
FILE *__real_fopen(const char *, const char *);
int __real_open(const char *, int, ...);
static void trace(const char *path)
{
    const int saved = errno;
    const char prefix[] = "SOFTPIPE-FILE: ";
    cubit_debug_write(prefix, sizeof prefix - 1);
    cubit_debug_write(path, strlen(path));
    cubit_debug_write("\n", 1);
    errno = saved;
}
FILE *__wrap_fopen(const char *path, const char *mode)
{
    trace(path);
    return __real_fopen(path, mode);
}
int __wrap_open(const char *path, int flags, ...)
{
    trace(path);
    if ((flags & O_CREAT) || (flags & O_TMPFILE) == O_TMPFILE) {
        va_list ap;
        va_start(ap, flags);
        mode_t mode = va_arg(ap, mode_t);
        va_end(ap);
        return __real_open(path, flags, mode);
    }
    return __real_open(path, flags);
}
