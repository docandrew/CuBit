#include "cubit-device-query.h"

/* Test glue only. Use the real Mesa-port decoder and Ada transport, not a
 * second implementation of its protocol. The app retains its manifest slot
 * throughout this sequence; there is no lookup, admission, or cap mutation. */
uint32_t
cubit_test_native_discovery(uint64_t slot)
{
   struct cubit_gpu_native_endpoint endpoint = { .slot = slot };
   struct cubit_gpu_device_snapshot device;
   struct cubit_gpu_vm_contract vm;
   struct cubit_gpu_budget_snapshot budget;
   enum cubit_gpu_memory_contract memory;
   uint32_t timestamp_hz;
   if (!cubit_gpu_query_device(cubit_gpu_native_query_call, &endpoint, &device))
      return 1;
   if (!cubit_gpu_query_timestamp(cubit_gpu_native_query_call, &endpoint,
                                &timestamp_hz))
      return 2;
   if (!cubit_gpu_query_vm(cubit_gpu_native_query_call, &endpoint, &vm))
      return 3;
   if (!cubit_gpu_query_memory(cubit_gpu_native_query_call, &endpoint, &memory))
      return 4;
   if (!cubit_gpu_query_budget(cubit_gpu_native_query_call, &endpoint, &budget))
      return 5;
   /* A valid exhausted budget is still valid discovery. Allocation separately
    * decides availability; explicit-maintenance policy is NOT coherent. */
   return 0;
}
