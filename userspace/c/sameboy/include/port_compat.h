#pragma once
#include <stdio.h>
#include <time.h>
int vasprintf(char **result, const char *format, va_list arguments);
#define alloca(size) __builtin_alloca(size)
/* WorkBoy's optional peripheral functions are not exposed by this frontend
 * and are removed by section GC. Declarations allow compiling upstream. */
struct tm *localtime(const time_t *);
time_t mktime(struct tm *);
