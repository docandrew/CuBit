/*
 * CuBit OS - strings.h
 */
#ifndef _STRINGS_H
#define _STRINGS_H

#include "cubit.h"

int strcasecmp(const char *s1, const char *s2);
int strncasecmp(const char *s1, const char *s2, size_t n);
char *strcasestr(const char *haystack, const char *needle);

#endif /* _STRINGS_H */
