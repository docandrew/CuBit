/*
 * CuBit libc: explicit diagnostics.
 *
 * Writes to the kernel console. This is a temporary debugging channel (the
 * console is mirrored to the serial port), not a program's output: stdout
 * and stderr are the program's declared CuBit streams, and there is no
 * implicit fallback from them to the console.
 */
#ifndef _CUBIT_DEBUG_H
#define _CUBIT_DEBUG_H
#include <stddef.h>
#ifdef __cplusplus
extern "C" {
#endif
void cubit_debug_write(const char *text, size_t length);
#ifdef __cplusplus
}
#endif
#endif
