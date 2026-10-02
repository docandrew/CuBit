#pragma once
#include <stdint.h>
/* 0 = acquired one borrow, 1 = rejected; output cleared on rejection.
 * Capability slot stability and a writable output pointer are caller duties.
 * Kernel validates owner endpoint, reference generation, range and access. */
uint32_t cubit_intel_acquire_view(uint64_t slot, uint64_t reference,
                                uint64_t offset, uint64_t bytes,
                                uint64_t writable, uint64_t *output);
/* Return one borrow, NOT munmap, revocation or GPU backing release. */
uint32_t cubit_intel_return_view(uint64_t reference);
