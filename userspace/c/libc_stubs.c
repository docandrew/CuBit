/*
 * CuBit OS - C Library Stubs for DOOM
 * Copyright (C) 2026 Jon Andrew
 *
 * Freestanding implementations of standard C library functions that DOOM
 * needs but that aren't in our minimal runtime. Includes printf family,
 * ctype, strdup, math stubs, etc.
 */
#include "cubit.h"
#include <stdio.h>
#include <string.h>

/*---------------------------------------------------------------------------
 * Internal: number-to-string conversion for printf
 *---------------------------------------------------------------------------*/
static int _emit(char *buf, size_t pos, size_t limit, char c)
{
    if (buf && pos < limit)
        buf[pos] = c;
    return 1;
}

static int _emit_str(char *buf, size_t pos, size_t limit, const char *s)
{
    int count = 0;
    while (*s) {
        count += _emit(buf, pos + count, limit, *s);
        s++;
    }
    return count;
}

/* Convert unsigned 64-bit to string with given base.
 * min_digits: minimum number of digits (from precision, e.g. %.3d → 3) */
static int _utoa(char *buf, size_t pos, size_t limit, unsigned long val,
                 int base, int uppercase, int width, char pad, int is_neg,
                 int min_digits)
{
    char tmp[24];
    int i = 0;
    const char *digits = uppercase ? "0123456789ABCDEF" : "0123456789abcdef";

    if (val == 0) {
        tmp[i++] = '0';
    } else {
        while (val > 0) {
            tmp[i++] = digits[val % base];
            val /= base;
        }
    }

    /* Zero-pad digit count to reach min_digits (from precision) */
    int num_digits = i < min_digits ? min_digits : i;

    int count = 0;
    int total_width = num_digits + (is_neg ? 1 : 0);

    /* Pad with spaces if padding char is space and we need width */
    if (pad == ' ') {
        for (int j = total_width; j < width; j++)
            count += _emit(buf, pos + count, limit, ' ');
    }

    if (is_neg)
        count += _emit(buf, pos + count, limit, '-');

    /* Pad with zeros for width (e.g. %05d) */
    if (pad == '0') {
        for (int j = total_width; j < width; j++)
            count += _emit(buf, pos + count, limit, '0');
    }

    /* Pad with zeros for precision (e.g. %.5d) */
    for (int j = i; j < min_digits; j++)
        count += _emit(buf, pos + count, limit, '0');

    /* Digits in reverse */
    while (i > 0)
        count += _emit(buf, pos + count, limit, tmp[--i]);

    return count;
}

/*---------------------------------------------------------------------------
 * vsnprintf - Core formatting engine
 *---------------------------------------------------------------------------*/
int vsnprintf(char *str, size_t size, const char *fmt, va_list ap)
{
    size_t pos = 0;
    size_t limit = (size > 0) ? size - 1 : 0;

    while (*fmt) {
        if (*fmt != '%') {
            pos += _emit(str, pos, limit, *fmt);
            fmt++;
            continue;
        }

        fmt++; /* skip '%' */

        /* Flags */
        char pad = ' ';
        int left_justify = 0;
        int plus_sign = 0;

        for (;;) {
            if (*fmt == '0') { pad = '0'; fmt++; }
            else if (*fmt == '-') { left_justify = 1; fmt++; }
            else if (*fmt == '+') { plus_sign = 1; fmt++; }
            else if (*fmt == ' ') { fmt++; }
            else if (*fmt == '#') { fmt++; }
            else break;
        }
        if (left_justify) pad = ' ';

        /* Width */
        int width = 0;
        if (*fmt == '*') {
            width = va_arg(ap, int);
            fmt++;
        } else {
            while (*fmt >= '0' && *fmt <= '9') {
                width = width * 10 + (*fmt - '0');
                fmt++;
            }
        }

        /* Precision */
        int has_precision = 0;
        int precision = -1;
        if (*fmt == '.') {
            has_precision = 1;
            fmt++;
            if (*fmt == '*') {
                precision = va_arg(ap, int);
                fmt++;
            } else {
                precision = 0;
                while (*fmt >= '0' && *fmt <= '9') {
                    precision = precision * 10 + (*fmt - '0');
                    fmt++;
                }
            }
        }

        /* Length modifier */
        int is_long = 0;
        int is_short = 0;
        int is_size_t = 0;
        if (*fmt == 'l') {
            is_long = 1; fmt++;
            if (*fmt == 'l') { fmt++; } /* ll = long long, same as long on LP64 */
        } else if (*fmt == 'h') {
            is_short = 1; fmt++;
            if (*fmt == 'h') { fmt++; }
        } else if (*fmt == 'z') {
            is_size_t = 1; fmt++;
        }

        /* Conversion */
        switch (*fmt) {
        case '%':
            pos += _emit(str, pos, limit, '%');
            break;

        case 'c':
            pos += _emit(str, pos, limit, (char)va_arg(ap, int));
            break;

        case 's': {
            const char *s = va_arg(ap, const char *);
            if (!s) s = "(null)";
            int slen = 0;
            const char *sp = s;
            while (*sp) { slen++; sp++; }
            if (has_precision && precision >= 0 && precision < slen)
                slen = precision;
            /* Pad left */
            if (!left_justify) {
                for (int j = slen; j < width; j++)
                    pos += _emit(str, pos, limit, ' ');
            }
            for (int j = 0; j < slen; j++)
                pos += _emit(str, pos, limit, s[j]);
            /* Pad right */
            if (left_justify) {
                for (int j = slen; j < width; j++)
                    pos += _emit(str, pos, limit, ' ');
            }
            break;
        }

        case 'd':
        case 'i': {
            long val;
            if (is_long || is_size_t)
                val = va_arg(ap, long);
            else
                val = va_arg(ap, int);
            int neg = 0;
            unsigned long uval;
            if (val < 0) { neg = 1; uval = -val; }
            else { uval = val; }
            int md = has_precision ? precision : 0;
            pos += _utoa(str, pos, limit, uval, 10, 0, width, pad, neg, md);
            break;
        }

        case 'u': {
            unsigned long val;
            if (is_long || is_size_t)
                val = va_arg(ap, unsigned long);
            else
                val = (unsigned int)va_arg(ap, unsigned int);
            int md = has_precision ? precision : 0;
            pos += _utoa(str, pos, limit, val, 10, 0, width, pad, 0, md);
            break;
        }

        case 'x': {
            unsigned long val;
            if (is_long || is_size_t)
                val = va_arg(ap, unsigned long);
            else
                val = (unsigned int)va_arg(ap, unsigned int);
            int md = has_precision ? precision : 0;
            pos += _utoa(str, pos, limit, val, 16, 0, width, pad, 0, md);
            break;
        }

        case 'X': {
            unsigned long val;
            if (is_long || is_size_t)
                val = va_arg(ap, unsigned long);
            else
                val = (unsigned int)va_arg(ap, unsigned int);
            int md = has_precision ? precision : 0;
            pos += _utoa(str, pos, limit, val, 16, 1, width, pad, 0, md);
            break;
        }

        case 'o': {
            unsigned long val;
            if (is_long || is_size_t)
                val = va_arg(ap, unsigned long);
            else
                val = (unsigned int)va_arg(ap, unsigned int);
            int md = has_precision ? precision : 0;
            pos += _utoa(str, pos, limit, val, 8, 0, width, pad, 0, md);
            break;
        }

        case 'p': {
            void *ptr = va_arg(ap, void *);
            pos += _emit_str(str, pos, limit, "0x");
            pos += _utoa(str, pos, limit, (unsigned long)ptr, 16, 0, 0, '0', 0, 0);
            break;
        }

        case 'n':
            /* Ignore %n for security */
            break;

        default:
            /* Unknown format: just emit the character */
            pos += _emit(str, pos, limit, '%');
            pos += _emit(str, pos, limit, *fmt);
            break;
        }

        if (*fmt) fmt++;
    }

    if (str && size > 0)
        str[pos < limit ? pos : limit] = '\0';

    return (int)pos;
}

/*---------------------------------------------------------------------------
 * snprintf / sprintf / printf / fprintf / vfprintf
 *---------------------------------------------------------------------------*/
int snprintf(char *str, size_t size, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int ret = vsnprintf(str, size, fmt, ap);
    va_end(ap);
    return ret;
}

int sprintf(char *str, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    /* Use a very large limit */
    int ret = vsnprintf(str, 65536, fmt, ap);
    va_end(ap);
    return ret;
}

int printf(const char *fmt, ...)
{
    char buf[1024];
    va_list ap;
    va_start(ap, fmt);
    int ret = vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    if (ret > 0) {
        int len = ret < (int)sizeof(buf) ? ret : (int)sizeof(buf) - 1;
        cubit_write(STDOUT, buf, len);
    }
    return ret;
}

int vfprintf(FILE *stream, const char *fmt, va_list ap)
{
    char buf[1024];
    (void)stream; /* treat all output as stdout for now */
    int ret = vsnprintf(buf, sizeof(buf), fmt, ap);
    if (ret > 0) {
        int len = ret < (int)sizeof(buf) ? ret : (int)sizeof(buf) - 1;
        cubit_write(STDOUT, buf, len);
    }
    return ret;
}

int fprintf(FILE *stream, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int ret = vfprintf(stream, fmt, ap);
    va_end(ap);
    return ret;
}

int fputs(const char *s, FILE *stream)
{
    (void)stream;
    size_t len = strlen(s);
    cubit_write(STDOUT, s, len);
    return 0;
}

int puts(const char *s)
{
    size_t len = strlen(s);
    cubit_write(STDOUT, s, len);
    cubit_write(STDOUT, "\n", 1);
    return 0;
}

int putchar(int c)
{
    char ch = (char)c;
    cubit_write(STDOUT, &ch, 1);
    return c;
}

int fflush(FILE *stream)
{
    (void)stream;
    return 0;
}

int feof(FILE *stream)
{
    (void)stream;
    return 0;
}

int ferror(FILE *stream)
{
    (void)stream;
    return 0;
}

char *fgets(char *s, int size, FILE *stream)
{
    (void)stream;
    if (size <= 0) return NULL;
    s[0] = '\0';
    return NULL; /* No interactive input support yet */
}

/*---------------------------------------------------------------------------
 * fwrite - Write data to a file (stub: writes to stdout for debug)
 *---------------------------------------------------------------------------*/
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream)
{
    (void)stream;
    size_t total = size * nmemb;
    if (total > 0)
        cubit_write(STDOUT, ptr, total);
    return nmemb;
}

int remove(const char *pathname)
{
    (void)pathname;
    return -1;
}

int rename(const char *oldpath, const char *newpath)
{
    (void)oldpath; (void)newpath;
    return -1;
}

/*---------------------------------------------------------------------------
 * sscanf - Minimal implementation for DOOM
 * Supports: %d, %i, %x, %s, %c, and literal char matching.
 *---------------------------------------------------------------------------*/
int sscanf(const char *str, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int count = 0;
    const char *s = str;

    while (*fmt && *s) {
        if (*fmt == '%') {
            fmt++;
            /* Width (ignored for now) */
            while (*fmt >= '0' && *fmt <= '9') fmt++;

            switch (*fmt) {
            case 'd':
            case 'i': {
                int *out = va_arg(ap, int *);
                long val = 0;
                int neg = 0;
                while (*s == ' ') s++;
                if (*s == '-') { neg = 1; s++; }
                else if (*s == '+') s++;
                if (!(*s >= '0' && *s <= '9')) goto done;
                while (*s >= '0' && *s <= '9') {
                    val = val * 10 + (*s - '0');
                    s++;
                }
                *out = neg ? -(int)val : (int)val;
                count++;
                break;
            }
            case 'x':
            case 'X': {
                unsigned int *out = va_arg(ap, unsigned int *);
                unsigned long val = 0;
                while (*s == ' ') s++;
                if (*s == '0' && (s[1] == 'x' || s[1] == 'X')) s += 2;
                int digits = 0;
                while (1) {
                    int d;
                    if (*s >= '0' && *s <= '9') d = *s - '0';
                    else if (*s >= 'a' && *s <= 'f') d = *s - 'a' + 10;
                    else if (*s >= 'A' && *s <= 'F') d = *s - 'A' + 10;
                    else break;
                    val = val * 16 + d;
                    s++;
                    digits++;
                }
                if (!digits) goto done;
                *out = (unsigned int)val;
                count++;
                break;
            }
            case 's': {
                char *out = va_arg(ap, char *);
                while (*s == ' ') s++;
                while (*s && *s != ' ' && *s != '\t' && *s != '\n') {
                    *out++ = *s++;
                }
                *out = '\0';
                count++;
                break;
            }
            case 'c': {
                char *out = va_arg(ap, char *);
                *out = *s++;
                count++;
                break;
            }
            default:
                goto done;
            }
            fmt++;
        } else if (*fmt == ' ') {
            while (*s == ' ' || *s == '\t') s++;
            fmt++;
        } else {
            if (*fmt != *s) goto done;
            fmt++;
            s++;
        }
    }

done:
    va_end(ap);
    return count;
}

/*---------------------------------------------------------------------------
 * stderr / stdout / stdin pseudo-streams
 *---------------------------------------------------------------------------*/
FILE *stderr = (FILE *)2;
FILE *stdout = (FILE *)1;
FILE *stdin  = (FILE *)0;

/*---------------------------------------------------------------------------
 * errno
 *---------------------------------------------------------------------------*/
int errno = 0;

/*---------------------------------------------------------------------------
 * strdup / strcasecmp / strncasecmp / strncat / strerror / perror
 *---------------------------------------------------------------------------*/
char *strdup(const char *s)
{
    size_t len = strlen(s) + 1;
    char *d = malloc(len);
    if (d) memcpy(d, s, len);
    return d;
}

char *strncat(char *dest, const char *src, size_t n)
{
    char *d = dest;
    while (*d) d++;
    while (n-- > 0 && *src) *d++ = *src++;
    *d = '\0';
    return dest;
}

int strcasecmp(const char *s1, const char *s2)
{
    while (*s1 && *s2) {
        int c1 = (*s1 >= 'A' && *s1 <= 'Z') ? *s1 + 32 : *s1;
        int c2 = (*s2 >= 'A' && *s2 <= 'Z') ? *s2 + 32 : *s2;
        if (c1 != c2) return c1 - c2;
        s1++;
        s2++;
    }
    int c1 = (*s1 >= 'A' && *s1 <= 'Z') ? *s1 + 32 : *s1;
    int c2 = (*s2 >= 'A' && *s2 <= 'Z') ? *s2 + 32 : *s2;
    return c1 - c2;
}

int strncasecmp(const char *s1, const char *s2, size_t n)
{
    for (size_t i = 0; i < n; i++) {
        if (!s1[i] && !s2[i]) return 0;
        int c1 = (s1[i] >= 'A' && s1[i] <= 'Z') ? s1[i] + 32 : s1[i];
        int c2 = (s2[i] >= 'A' && s2[i] <= 'Z') ? s2[i] + 32 : s2[i];
        if (c1 != c2) return c1 - c2;
        if (!c1) return 0;
    }
    return 0;
}

char *strerror(int errnum)
{
    (void)errnum;
    return "error";
}

void perror(const char *s)
{
    if (s && *s) {
        cubit_write(STDOUT, s, strlen(s));
        cubit_write(STDOUT, ": ", 2);
    }
    cubit_write(STDOUT, "error\n", 6);
}

/*---------------------------------------------------------------------------
 * ctype functions
 *---------------------------------------------------------------------------*/
int toupper(int c)
{
    if (c >= 'a' && c <= 'z') return c - 32;
    return c;
}

int tolower(int c)
{
    if (c >= 'A' && c <= 'Z') return c + 32;
    return c;
}

int isspace(int c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r' ||
           c == '\f' || c == '\v';
}

int isalpha(int c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

int isdigit(int c)
{
    return c >= '0' && c <= '9';
}

int isalnum(int c)
{
    return isalpha(c) || isdigit(c);
}

int isprint(int c)
{
    return c >= 0x20 && c < 0x7f;
}

int isupper(int c)
{
    return c >= 'A' && c <= 'Z';
}

int islower(int c)
{
    return c >= 'a' && c <= 'z';
}

int isxdigit(int c)
{
    return isdigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

/*---------------------------------------------------------------------------
 * exit / abort / atexit / system
 *---------------------------------------------------------------------------*/
typedef void (*atexit_fn)(void);
#define MAX_ATEXIT 32
static atexit_fn _atexit_fns[MAX_ATEXIT];
static int _atexit_count = 0;

void exit(int status)
{
    for (int i = _atexit_count - 1; i >= 0; i--)
        _atexit_fns[i]();
    cubit_exit(status);
    __builtin_unreachable();
}

void abort(void)
{
    cubit_exit(134); /* SIGABRT convention */
    __builtin_unreachable();
}

int atexit(void (*function)(void))
{
    if (_atexit_count >= MAX_ATEXIT) return -1;
    _atexit_fns[_atexit_count++] = function;
    return 0;
}

int system(const char *command)
{
    (void)command;
    return -1; /* No shell on CuBit */
}

/*---------------------------------------------------------------------------
 * qsort - Simple insertion sort (sufficient for DOOM's small arrays)
 *---------------------------------------------------------------------------*/
void qsort(void *base, size_t nmemb, size_t size,
            int (*compar)(const void *, const void *))
{
    char *arr = (char *)base;
    char tmp[256]; /* Enough for any DOOM struct */

    for (size_t i = 1; i < nmemb; i++) {
        memcpy(tmp, arr + i * size, size);
        size_t j = i;
        while (j > 0 && compar(arr + (j - 1) * size, tmp) > 0) {
            memcpy(arr + j * size, arr + (j - 1) * size, size);
            j--;
        }
        memcpy(arr + j * size, tmp, size);
    }
}

/*---------------------------------------------------------------------------
 * atof - Convert string to double (minimal implementation)
 *---------------------------------------------------------------------------*/
double atof(const char *nptr)
{
    double result = 0.0;
    double sign = 1.0;
    const char *s = nptr;

    while (*s == ' ' || *s == '\t') s++;
    if (*s == '-') { sign = -1.0; s++; }
    else if (*s == '+') s++;

    while (*s >= '0' && *s <= '9') {
        result = result * 10.0 + (*s - '0');
        s++;
    }

    if (*s == '.') {
        s++;
        double frac = 0.1;
        while (*s >= '0' && *s <= '9') {
            result += (*s - '0') * frac;
            frac *= 0.1;
            s++;
        }
    }

    return sign * result;
}

/*---------------------------------------------------------------------------
 * Math functions (minimal - DOOM barely uses floating point)
 *---------------------------------------------------------------------------*/

/* Taylor series approximation for sin - only needs to be rough for DOOM */
double fabs(double x)
{
    return x < 0 ? -x : x;
}

/* Reduce angle to [-pi, pi] range */
static double _reduce(double x)
{
    const double PI = 3.14159265358979323846;
    const double TWO_PI = 6.28318530717958647692;
    while (x > PI) x -= TWO_PI;
    while (x < -PI) x += TWO_PI;
    return x;
}

double sin(double x)
{
    x = _reduce(x);
    /* Taylor: x - x^3/6 + x^5/120 - x^7/5040 + x^9/362880 */
    double x2 = x * x;
    double x3 = x2 * x;
    double x5 = x3 * x2;
    double x7 = x5 * x2;
    double x9 = x7 * x2;
    return x - x3 / 6.0 + x5 / 120.0 - x7 / 5040.0 + x9 / 362880.0;
}

double cos(double x)
{
    const double HALF_PI = 1.57079632679489661923;
    return sin(x + HALF_PI);
}

double tan(double x)
{
    double c = cos(x);
    if (c == 0.0) return 1e30; /* Avoid division by zero */
    return sin(x) / c;
}
