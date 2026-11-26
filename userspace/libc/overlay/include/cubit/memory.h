/*
 * SPDX-License-Identifier: GPL-3.0-or-later
 * CuBit libc: owned-memory operations musl's mmap layer does not name.
 */
#ifndef _CUBIT_MEMORY_H
#define _CUBIT_MEMORY_H
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
/* The kernel's Discard over [address, address + bytes), page-aligned
 * (MEM-007, K12; what MADV_DONTNEED needs): committed pages read zero from
 * then on and keep their access; reserved pages stay reserved. 0, or the
 * kernel's refusal word (Owned_Memory_ABI). */
uint64_t cubit_owned_discard(uint64_t address, uint64_t bytes);
#ifdef __cplusplus
}
#endif
#endif
