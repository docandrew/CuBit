/* Hosted tests only (CAP-002): endpoint handles from numbers, so mocks can
 * stand in for the kernel. Never included by a CuBit program; native tests
 * take their handles from the startup table (cubit_startup_endpoint). */
#ifndef CUBIT_TEST_HANDLES_H
#define CUBIT_TEST_HANDLES_H
#include <stdint.h>
#include "../../../userspace/c/cubit_handles.h"
/* A test endpoint handle (generation 1). */
#define TEST_ENDPOINT(n) ((cubit_endpoint){(UINT64_C(1) << 32) | (uint64_t)(n)})
#endif
