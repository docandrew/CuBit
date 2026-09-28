#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

/* Port-local logging adapter. There is no stdin shell or subprocess support. */
int vasprintf(char **result, const char *format, va_list arguments)
{
    va_list copy;
    *result = NULL;
    va_copy(copy, arguments);
    int length = vsnprintf(NULL, 0, format, copy);
    va_end(copy);
    if (length < 0 || length > 65535) return -1;
    char *text = malloc((size_t)length + 1);
    if (!text) return -1;
    va_copy(copy, arguments);
    vsnprintf(text, (size_t)length + 1, format, copy);
    va_end(copy);
    *result = text;
    return length;
}
