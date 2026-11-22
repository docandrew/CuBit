#pragma once
#include <stdint.h>
#include "cubit_handles.h"
/* Scalar Ada FFI, not a native IPC structure overlay. Zero reports transport
 * success only; callers must validate all returned protocol words. */
uint32_t cubit_intel_query(cubit_endpoint endpoint, uint64_t selector,
                          uint64_t reply_words[4]);
uint32_t cubit_intel_budget(cubit_endpoint endpoint, uint64_t reply_words[4]);
