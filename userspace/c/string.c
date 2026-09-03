/*
 * CuBit OS - String/Memory Utility Functions
 * Copyright (C) 2026 Jon Andrew
 *
 * Freestanding implementations of standard string.h and string-like functions.
 */
#include "cubit.h"
#include <errno.h>
#include <limits.h>

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

/*
 * C-only programs need a freestanding fallback.  Mixed Ada/C programs use
 * the equivalent strong implementation in the Ada runtime when it is linked.
 * This is a general-purpose comparison, not a constant-time secret compare.
 */
__attribute__((weak)) int memcmp(const void *s1, const void *s2, size_t n)
{
    const unsigned char *a = (const unsigned char *)s1;
    const unsigned char *b = (const unsigned char *)s2;

    typedef uint64_t unaligned_word
        __attribute__((aligned(1), may_alias));

    while (n >= 8 * sizeof(unaligned_word)) {
#pragma GCC unroll 8
        for (size_t i = 0; i < 8; i++) {
            const uint64_t difference =
                ((const unaligned_word *)a)[i] ^
                ((const unaligned_word *)b)[i];

            if (difference != 0) {
                const size_t byte = (size_t)__builtin_ctzll(difference) >> 3;
                const size_t offset = i * sizeof(unaligned_word) + byte;
                return a[offset] - b[offset];
            }
        }

        a += 8 * sizeof(unaligned_word);
        b += 8 * sizeof(unaligned_word);
        n -= 8 * sizeof(unaligned_word);
    }

    while (n >= sizeof(unaligned_word)) {
        const unaligned_word aw = *(const unaligned_word *)a;
        const unaligned_word bw = *(const unaligned_word *)b;
        const uint64_t difference = aw ^ bw;

        if (difference != 0) {
            const size_t byte = (size_t)__builtin_ctzll(difference) >> 3;
            return a[byte] - b[byte];
        }

        a += sizeof(unaligned_word);
        b += sizeof(unaligned_word);
        n -= sizeof(unaligned_word);
    }

    while (n > 0) {
        if (*a != *b) {
            return *a - *b;
        }
        a++;
        b++;
        n--;
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

size_t strspn(const char *s, const char *accept)
{
    size_t count = 0;
    while (s[count] != '\0') {
        const char *a = accept;
        while (*a != '\0' && *a != s[count]) a++;
        if (*a == '\0') break;
        count++;
    }
    return count;
}

size_t strcspn(const char *s, const char *reject)
{
    size_t count = 0;
    while (s[count] != '\0') {
        const char *r = reject;
        while (*r != '\0' && *r != s[count]) r++;
        if (*r != '\0') break;
        count++;
    }
    return count;
}

char *strpbrk(const char *s, const char *accept)
{
    while (*s != '\0') {
        const char *a = accept;
        while (*a != '\0') {
            if (*a++ == *s) return (char *)s;
        }
        s++;
    }
    return NULL;
}

char *strtok(char *restrict s, const char *restrict delim)
{
    static char *next;
    char *token;

    if (s == NULL) s = next;
    if (s == NULL) return NULL;

    s += strspn(s, delim);
    if (*s == '\0') {
        next = NULL;
        return NULL;
    }

    token = s;
    s += strcspn(s, delim);
    if (*s == '\0') {
        next = NULL;
    } else {
        *s = '\0';
        next = s + 1;
    }
    return token;
}

char *strndup(const char *s, size_t n)
{
    size_t length = 0;
    char *copy;
    while (length < n && s[length] != '\0') length++;
    copy = malloc(length + 1);
    if (copy == NULL) return NULL;
    memcpy(copy, s, length);
    copy[length] = '\0';
    return copy;
}

long strtol(const char *nptr, char **endptr, int base)
{
    const char *s = nptr;
    unsigned long result = 0;
    unsigned long limit;
    int negative = 0;
    int any = 0;

    /* Skip whitespace */
    while (*s == ' ' || *s == '\t' || *s == '\n') s++;

    /* Sign */
    if (*s == '-') { negative = 1; s++; }
    else if (*s == '+') { s++; }

    if (base != 0 && (base < 2 || base > 36)) {
        errno = EINVAL;
        if (endptr) *endptr = (char *)nptr;
        return 0;
    }

    /* Auto-detect base without consuming a lone leading zero. */
    if (base == 0) {
        if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
            base = 16;
            s += 2;
        } else if (s[0] == '0') {
            base = 8;
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
        limit = negative ? (unsigned long)LONG_MAX + 1UL : (unsigned long)LONG_MAX;
        if (result > (limit - (unsigned long)digit) / (unsigned long)base) {
            errno = ERANGE;
            while (1) {
                char next = s[1];
                int next_digit;
                if (next >= '0' && next <= '9') next_digit = next - '0';
                else if (next >= 'a' && next <= 'z') next_digit = next - 'a' + 10;
                else if (next >= 'A' && next <= 'Z') next_digit = next - 'A' + 10;
                else break;
                if (next_digit >= base) break;
                s++;
            }
            s++;
            if (endptr) *endptr = (char *)s;
            return negative ? LONG_MIN : LONG_MAX;
        }
        result = result * (unsigned long)base + (unsigned long)digit;
        any = 1;
        s++;
    }

    if (endptr) *endptr = (char *)(any ? s : nptr);
    if (negative && result == (unsigned long)LONG_MAX + 1UL) return LONG_MIN;
    return negative ? -(long)result : (long)result;
}

unsigned long strtoul(const char *nptr, char **endptr, int base)
{
    return (unsigned long)strtol(nptr, endptr, base);
}

long long strtoll(const char *nptr, char **endptr, int base)
{
    /* CuBit's x86-64 C ABI gives long and long long the same value range. */
    return (long long)strtol(nptr, endptr, base);
}

unsigned long long strtoull(const char *nptr, char **endptr, int base)
{
    return (unsigned long long)strtoul(nptr, endptr, base);
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
