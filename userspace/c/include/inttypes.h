/*
 * CuBit OS - inttypes.h shim
 */
#ifndef _INTTYPES_H
#define _INTTYPES_H

#include <stdint.h>

/* Format macros for printf (LP64 model) */
#define PRId8   "d"
#define PRId16  "d"
#define PRId32  "d"
#define PRId64  "ld"
#define PRIu8   "u"
#define PRIu16  "u"
#define PRIu32  "u"
#define PRIu64  "lu"
#define PRIx8   "x"
#define PRIx16  "x"
#define PRIx32  "x"
#define PRIx64  "lx"
#define PRIX32  "X"
#define PRIX64  "lX"

#endif /* _INTTYPES_H */
