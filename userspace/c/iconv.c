/* Bounded character conversion for the initial CuBit browser frontend. */
#include <errno.h>
#include <iconv.h>
#include <stdint.h>
#include <strings.h>

#define ICONV_UTF8_TO_CP1252 ((iconv_t)(uintptr_t)1)
#define ICONV_CP1252_TO_UTF8 ((iconv_t)(uintptr_t)2)
#define ICONV_IDENTITY       ((iconv_t)(uintptr_t)3)

static int is_utf8(const char *name)
{
    return strcasecmp(name, "UTF-8") == 0 || strcasecmp(name, "UTF8") == 0;
}

static int is_cp1252(const char *name)
{
    return strcasecmp(name, "CP1252") == 0 ||
           strcasecmp(name, "WINDOWS-1252") == 0;
}

iconv_t iconv_open(const char *to, const char *from)
{
    if (to == NULL || from == NULL) { errno = EINVAL; return (iconv_t)-1; }
    if ((is_utf8(to) && is_utf8(from)) ||
        (is_cp1252(to) && is_cp1252(from))) return ICONV_IDENTITY;
    if (is_cp1252(to) && is_utf8(from)) return ICONV_UTF8_TO_CP1252;
    if (is_utf8(to) && is_cp1252(from)) return ICONV_CP1252_TO_UTF8;
    errno = EINVAL;
    return (iconv_t)-1;
}

int iconv_close(iconv_t cd)
{
    return (cd == ICONV_IDENTITY || cd == ICONV_UTF8_TO_CP1252 ||
            cd == ICONV_CP1252_TO_UTF8) ? 0 : -1;
}

size_t iconv(iconv_t cd, char **restrict input, size_t *restrict input_left,
             char **restrict output, size_t *restrict output_left)
{
    size_t replacements = 0;

    if (input == NULL || *input == NULL) return 0;
    if (input_left == NULL || output == NULL || *output == NULL ||
        output_left == NULL) { errno = EINVAL; return (size_t)-1; }

    while (*input_left != 0) {
        unsigned char byte = (unsigned char)**input;
        if (*output_left == 0) { errno = E2BIG; return (size_t)-1; }

        if (cd == ICONV_IDENTITY || byte < 0x80) {
            **output = (char)byte;
            (*output)++; (*output_left)--; (*input)++; (*input_left)--;
        } else if (cd == ICONV_CP1252_TO_UTF8) {
            if (*output_left < 2) { errno = E2BIG; return (size_t)-1; }
            /* Latin-1-compatible subset; undefined CP1252 controls fail. */
            if (byte < 0xa0) { errno = EILSEQ; return (size_t)-1; }
            *(*output)++ = (char)(0xc0 | (byte >> 6));
            *(*output)++ = (char)(0x80 | (byte & 0x3f));
            *output_left -= 2; (*input)++; (*input_left)--;
        } else if (cd == ICONV_UTF8_TO_CP1252) {
            uint32_t cp;
            unsigned char second;
            if (*input_left < 2 || byte < 0xc2 || byte > 0xc3) {
                errno = EILSEQ; return (size_t)-1;
            }
            second = (unsigned char)(*input)[1];
            if ((second & 0xc0) != 0x80) { errno = EILSEQ; return (size_t)-1; }
            cp = ((uint32_t)(byte & 0x1f) << 6) | (second & 0x3f);
            if (cp < 0xa0 || cp > 0xff) { errno = EILSEQ; return (size_t)-1; }
            **output = (char)cp;
            (*output)++; (*output_left)--; *input += 2; *input_left -= 2;
        } else {
            errno = EINVAL; return (size_t)-1;
        }
    }
    return replacements;
}
