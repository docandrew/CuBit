#include "../../../userspace/mesa/anv/native_gpu_buffers.h"
uint32_t test_buffer_c_bridge(void)
{
   uint64_t limit = UINT64_MAX, charged = UINT64_MAX;
   if (cubit_intel_query_accounting(63, &limit, &charged) != 0 ||
       limit != 12288 || charged != 8192)
      return 30;
   if (cubit_intel_query_accounting(64, &limit, &charged) != 4 ||
       limit != 0 || charged != 0)
      return 31;
   if (cubit_intel_query_accounting(63, 0, &charged) != 4 || charged != 0 ||
       cubit_intel_query_accounting(63, &limit, &limit) != 4 || limit != 0)
      return 32;
   uint64_t retired = UINT64_MAX;
   if (cubit_intel_poll_session_retirement(63) != 0 ||
       cubit_intel_poll_session_retirement(64) != 5)
      return 22;
   if (cubit_intel_close_session(63, &retired) != 0 ||
       retired != UINT64_C(0x4750000000000001))
      return 20;
   if (cubit_intel_close_session(64, &retired) != 4 || retired != 0)
      return 21;
   uint32_t handle = UINT32_MAX;
   uint32_t generation = UINT32_MAX;
   if (cubit_intel_update_binding(63, 1, 0x20000, 4096, 4096, 1, 0,
                                  &generation) != 0 || generation != 1)
      return 13;
   if (cubit_intel_update_binding(63, 1, 0x20000, 4096, 4096, 2, 0,
                                  &generation) != 4 || generation != 0)
      return 14;
   if (cubit_intel_create_buffer(63, 4096, &handle) != 0 || handle == 0)
      return 1;
   if (cubit_intel_bind_buffer(63, handle, 0x10000, 0, 4096) != 0)
      return 7;
   if (cubit_intel_unbind_buffer(64, handle, 0x10000, 0, 4096) != 4 ||
       cubit_intel_unbind_buffer(62, handle, 0x10000, 0, 4096) != 1 ||
       cubit_intel_unbind_buffer(63, handle, 0x10000, 0, 4096) != 0 ||
       cubit_intel_unbind_buffer(63, handle, 0x10000, 0, 4096) != 1 ||
       cubit_intel_bind_buffer(63, handle, 0x10000, 0, 4096) != 0)
      return 23;
   if (cubit_intel_close_buffer(63, handle) != 0)
      return 2;
   if (cubit_intel_close_buffer(63, handle) != 1)
      return 3;
   if (cubit_intel_create_buffer(63, 0, &handle) != 4 || handle != 0)
      return 4;
   uint32_t mapping = UINT32_MAX;
   uint64_t reference = UINT64_MAX;
   if (cubit_intel_map_buffer(64, 1, 0, 4096, 1, &mapping, &reference) != 5 ||
       mapping != 0 || reference != 0)
      return 5;
   if (cubit_intel_retire_mapping(63, 0) != 5)
      return 6;
   mapping = UINT32_MAX;
   reference = UINT64_MAX;
   if (cubit_intel_map_presentation(64, 1, 0, 4096, &mapping, &reference) != 5 ||
       mapping != 0 || reference != 0)
      return 10;
   if (cubit_intel_prepare_context(64) != 4 ||
       cubit_intel_prepare_context(62) != 1 ||
       cubit_intel_prepare_context(63) != 0)
      return 8;
   if (cubit_intel_register_context(64) != 4 ||
       cubit_intel_register_context(62) != 1 ||
       cubit_intel_register_context(63) != 0)
      return 9;
   return 0;
}
