/*
 * CuBit OS - String/Memory Utility Functions
 * Copyright (C) 2026 Jon Andrew
 *
 * Freestanding implementations of standard string.h and string-like functions.
 */
#include "cubit.h"

void *memcpy(void *dest, const void *src, size_t n)
{
    unsigned char *d = (unsigned char *)dest;
    const unsigned char *s = (const unsigned char *)src;

    if (n == 0 || d == s) {
        return dest;
    }

    if ((((uintptr_t)d ^ (uintptr_t)s) & 7UL) == 0) {
        while (n > 0 && ((uintptr_t)d & 7UL) != 0) {
            *d++ = *s++;
            n--;
        }

        while (n >= 8) {
            *(uint64_t *)d = *(const uint64_t *)s;
            d += 8;
            s += 8;
            n -= 8;
        }
    }

    while (n > 0) {
        *d++ = *s++;
        n--;
    }

    return dest;
}

void *memmove(void *dest, const void *src, size_t n)
{
    unsigned char *d = (unsigned char *)dest;
    const unsigned char *s = (const unsigned char *)src;

    if (n == 0 || d == s) {
        return dest;
    }

    if (d < s) {
        return memcpy(dest, src, n);
    } else if (d > s) {
        d += n;
        s += n;

        if ((((uintptr_t)d ^ (uintptr_t)s) & 7UL) == 0) {
            while (n > 0 && ((uintptr_t)d & 7UL) != 0) {
                *--d = *--s;
                n--;
            }

            while (n >= 8) {
                d -= 8;
                s -= 8;
                *(uint64_t *)d = *(const uint64_t *)s;
                n -= 8;
            }
        }

        while (n > 0) {
            *--d = *--s;
            n--;
        }
    }

    return dest;
}

void *memset(void *s, int c, size_t n)
{
    unsigned char *p = (unsigned char *)s;
    unsigned char byte = (unsigned char)c;
    uint64_t word = byte;

    word |= word << 8;
    word |= word << 16;
    word |= word << 32;

    while (n > 0 && ((uintptr_t)p & 7UL) != 0) {
        *p++ = byte;
        n--;
    }

    while (n >= 8) {
        *(uint64_t *)p = word;
        p += 8;
        n -= 8;
    }

    while (n > 0) {
        *p++ = byte;
        n--;
    }

    return s;
}

int memcmp(const void *s1, const void *s2, size_t n)
{
    const unsigned char *a = (const unsigned char *)s1;
    const unsigned char *b = (const unsigned char *)s2;
    for (size_t i = 0; i < n; i++) {
        if (a[i] != b[i]) {
            return a[i] - b[i];
        }
    }
    return 0;
}

size_t strlen(const char *s)
{
    size_t len = 0;
    while (s[len]) len++;
    return len;
}

char *strcpy(char *dest, const char *src)
{
    char *d = dest;
    while ((*d++ = *src++) != '\0');
    return dest;
}

char *strncpy(char *dest, const char *src, size_t n)
{
    size_t i;
    for (i = 0; i < n && src[i] != '\0'; i++) {
        dest[i] = src[i];
    }
    for (; i < n; i++) {
        dest[i] = '\0';
    }
    return dest;
}

int strcmp(const char *s1, const char *s2)
{
    while (*s1 && *s1 == *s2) {
        s1++;
        s2++;
    }
    return (unsigned char)*s1 - (unsigned char)*s2;
}

int strncmp(const char *s1, const char *s2, size_t n)
{
    for (size_t i = 0; i < n; i++) {
        if (s1[i] != s2[i]) {
            return (unsigned char)s1[i] - (unsigned char)s2[i];
        }
        if (s1[i] == '\0') return 0;
    }
    return 0;
}

char *strcat(char *dest, const char *src)
{
    char *d = dest;
    while (*d) d++;
    while ((*d++ = *src++) != '\0');
    return dest;
}

char *strchr(const char *s, int c)
{
    while (*s) {
        if (*s == (char)c) return (char *)s;
        s++;
    }
    if (c == '\0') return (char *)s;
    return NULL;
}

char *strrchr(const char *s, int c)
{
    const char *last = NULL;
    while (*s) {
        if (*s == (char)c) last = s;
        s++;
    }
    if (c == '\0') return (char *)s;
    return (char *)last;
}

char *strstr(const char *haystack, const char *needle)
{
    if (!*needle) return (char *)haystack;
    for (; *haystack; haystack++) {
        const char *h = haystack;
        const char *n = needle;
        while (*h && *n && *h == *n) {
            h++;
            n++;
        }
        if (!*n) return (char *)haystack;
    }
    return NULL;
}

long strtol(const char *nptr, char **endptr, int base)
{
    const char *s = nptr;
    long result = 0;
    int negative = 0;

    /* Skip whitespace */
    while (*s == ' ' || *s == '\t' || *s == '\n') s++;

    /* Sign */
    if (*s == '-') { negative = 1; s++; }
    else if (*s == '+') { s++; }

    /* Auto-detect base */
    if (base == 0) {
        if (*s == '0') {
            s++;
            if (*s == 'x' || *s == 'X') { base = 16; s++; }
            else { base = 8; }
        } else {
            base = 10;
        }
    } else if (base == 16 && *s == '0' && (s[1] == 'x' || s[1] == 'X')) {
        s += 2;
    }

    while (*s) {
        int digit;
        if (*s >= '0' && *s <= '9') digit = *s - '0';
        else if (*s >= 'a' && *s <= 'z') digit = *s - 'a' + 10;
        else if (*s >= 'A' && *s <= 'Z') digit = *s - 'A' + 10;
        else break;

        if (digit >= base) break;
        result = result * base + digit;
        s++;
    }

    if (endptr) *endptr = (char *)s;
    return negative ? -result : result;
}

unsigned long strtoul(const char *nptr, char **endptr, int base)
{
    return (unsigned long)strtol(nptr, endptr, base);
}

int atoi(const char *nptr)
{
    return (int)strtol(nptr, NULL, 10);
}

long atol(const char *nptr)
{
    return strtol(nptr, NULL, 10);
}

int abs(int x)
{
    return x < 0 ? -x : x;
}
