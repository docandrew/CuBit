#include "../../../userspace/mesa/anv/native_gpu_buffers.h"
#include "../../../userspace/c/cubit_handles.h"
/* A test endpoint handle (generation 1). */
#define TEST_ENDPOINT(n) ((cubit_endpoint){(UINT64_C(1) << 32) | (uint64_t)(n)})
/* The index a test endpoint was made from. */
#define TEST_INDEX(e) ((unsigned)((e).opaque & 0xffffffffu))
uint32_t test_buffer_c_bridge(void)
{
   uint64_t limit = UINT64_MAX, charged = UINT64_MAX;
   if (cubit_intel_query_accounting(TEST_ENDPOINT(63), &limit, &charged) != 0 ||
       limit != 12288 || charged != 8192)
      return 30;
   if (cubit_intel_query_accounting(CUBIT_NO_ENDPOINT, &limit, &charged) != 4 ||
       limit != 0 || charged != 0)
      return 31;
   if (cubit_intel_query_accounting(TEST_ENDPOINT(63), 0, &charged) != 4 || charged != 0 ||
       cubit_intel_query_accounting(TEST_ENDPOINT(63), &limit, &limit) != 4 || limit != 0)
      return 32;
   uint64_t retired = UINT64_MAX;
   if (cubit_intel_poll_session_retirement(TEST_ENDPOINT(63)) != 0 ||
       cubit_intel_poll_session_retirement(CUBIT_NO_ENDPOINT) != 5)
      return 22;
   if (cubit_intel_close_session(TEST_ENDPOINT(63), &retired) != 0 ||
       retired != UINT64_C(0x4750000000000001))
      return 20;
   if (cubit_intel_close_session(CUBIT_NO_ENDPOINT, &retired) != 4 || retired != 0)
      return 21;
   uint32_t handle = UINT32_MAX;
   uint32_t generation = UINT32_MAX;
   if (cubit_intel_update_binding(TEST_ENDPOINT(63), 1, 0x20000, 4096, 4096, 1, 0,
                                  &generation) != 0 || generation != 1)
      return 13;
   if (cubit_intel_update_binding(TEST_ENDPOINT(63), 1, 0x20000, 4096, 4096, 2, 0,
                                  &generation) != 4 || generation != 0)
      return 14;
   if (cubit_intel_create_buffer(TEST_ENDPOINT(63), 4096, &handle) != 0 || handle == 0)
      return 1;
   if (cubit_intel_bind_buffer(TEST_ENDPOINT(63), handle, 0x10000, 0, 4096) != 0)
      return 7;
   if (cubit_intel_unbind_buffer(CUBIT_NO_ENDPOINT, handle, 0x10000, 0, 4096) != 4 ||
       cubit_intel_unbind_buffer(TEST_ENDPOINT(62), handle, 0x10000, 0, 4096) != 1 ||
       cubit_intel_unbind_buffer(TEST_ENDPOINT(63), handle, 0x10000, 0, 4096) != 0 ||
       cubit_intel_unbind_buffer(TEST_ENDPOINT(63), handle, 0x10000, 0, 4096) != 1 ||
       cubit_intel_bind_buffer(TEST_ENDPOINT(63), handle, 0x10000, 0, 4096) != 0)
      return 23;
   if (cubit_intel_close_buffer(TEST_ENDPOINT(63), handle) != 0)
      return 2;
   if (cubit_intel_close_buffer(TEST_ENDPOINT(63), handle) != 1)
      return 3;
   if (cubit_intel_create_buffer(TEST_ENDPOINT(63), 0, &handle) != 4 || handle != 0)
      return 4;
   uint32_t mapping = UINT32_MAX;
   uint64_t reference = UINT64_MAX;
   if (cubit_intel_map_buffer(CUBIT_NO_ENDPOINT, 1, 0, 4096, 1, &mapping, &reference) != 5 ||
       mapping != 0 || reference != 0)
      return 5;
   if (cubit_intel_retire_mapping(TEST_ENDPOINT(63), 0) != 5)
      return 6;
   mapping = UINT32_MAX;
   reference = UINT64_MAX;
   if (cubit_intel_map_presentation(CUBIT_NO_ENDPOINT, 1, 0, 4096, &mapping, &reference) != 5 ||
       mapping != 0 || reference != 0)
      return 10;
   if (cubit_intel_prepare_context(CUBIT_NO_ENDPOINT) != 4 ||
       cubit_intel_prepare_context(TEST_ENDPOINT(62)) != 1 ||
       cubit_intel_prepare_context(TEST_ENDPOINT(63)) != 0)
      return 8;
   if (cubit_intel_register_context(CUBIT_NO_ENDPOINT) != 4 ||
       cubit_intel_register_context(TEST_ENDPOINT(62)) != 1 ||
       cubit_intel_register_context(TEST_ENDPOINT(63)) != 0)
      return 9;
   return 0;
}
