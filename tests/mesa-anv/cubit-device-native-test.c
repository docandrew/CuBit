#include "cubit-device-query.h"
#include "native_gpu_query.h"
#include <assert.h>
#include <string.h>
static unsigned calls;
static uint32_t status;
uint32_t cubit_intel_budget(uint64_t slot, uint64_t words[4])
{
   assert(slot == 63);
   calls++;
   words[0] = 0;
   words[1] = 33554432;
   words[2] = 4096;
   words[3] = 15;
   return status;
}
uint32_t cubit_intel_query(uint64_t slot, uint64_t selector, uint64_t words[4])
{
   assert(slot == 63 && selector <= 4);
   calls++;
   words[0] = 2; /* Well-formed UNAVAILABLE is not a transport failure. */
   words[1] = 1;
   words[2] = words[3] = 0;
   return status;
}
int main(void)
{
   struct cubit_gpu_native_endpoint endpoint = {63};
   struct cubit_gpu_query_message request = {
      .label = 0x0a20, .length = 4, .words = {1, 1, 0, 0},
   };
   struct cubit_gpu_query_message reply;
   memset(&reply, 0xa5, sizeof(reply));
   struct cubit_gpu_query_message before = reply;
   assert(!cubit_gpu_native_query_call(NULL, &request, &reply));
   assert(!cubit_gpu_native_query_call(&endpoint, NULL, &reply));
   assert(!cubit_gpu_native_query_call(&endpoint, &request, NULL));
   endpoint.slot = 64;
   assert(!cubit_gpu_native_query_call(&endpoint, &request, &reply));
   endpoint.slot = UINT64_MAX;
   assert(!cubit_gpu_native_query_call(&endpoint, &request, &reply));
   endpoint.slot = 63;
   assert(calls == 0 && memcmp(&reply, &before, sizeof(reply)) == 0);
   status = 1;
   assert(!cubit_gpu_native_query_call(&endpoint, &request, &reply));
   assert(calls == 1 && memcmp(&reply, &before, sizeof(reply)) == 0);
   status = 0;
   assert(cubit_gpu_native_query_call(&endpoint, &request, &reply));
   assert(calls == 2 && reply.label == 0x0a20 && reply.length == 4);
   assert(reply.flags == 0 && reply.reserved == 0);
   assert(reply.words[0] == 2 && reply.words[1] == 1);
   struct cubit_gpu_device_snapshot snapshot;
   memset(&snapshot, 0xa5, sizeof(snapshot));
   struct cubit_gpu_device_snapshot saved = snapshot;
   assert(!cubit_gpu_query_device(cubit_gpu_native_query_call, &endpoint, &snapshot));
   assert(calls == 3 && memcmp(&snapshot, &saved, sizeof(snapshot)) == 0);
   request.words[1] = 2;
   assert(cubit_gpu_native_query_call(&endpoint, &request, &reply));
   assert(calls == 4);
   struct cubit_gpu_budget_snapshot budget;
   assert(cubit_gpu_query_budget(cubit_gpu_native_query_call, &endpoint, &budget));
   assert(calls == 5 && budget.total == 33554432 && budget.retained == 4096);
   assert(budget.available == 33550336 && budget.max_allocation == 16777216);
   assert(budget.unused_tickets == 15);
   status = 1;
   assert(!cubit_gpu_query_budget(cubit_gpu_native_query_call, &endpoint, &budget));
   assert(calls == 6 && budget.total == 0 && budget.available == 0);
   request.words[1] = 3;
   status = 0;
   enum cubit_gpu_memory_contract memory = CUBIT_GPU_MEMORY_OWNED_WB_COHERENT;
   assert(!cubit_gpu_query_memory(cubit_gpu_native_query_call, &endpoint, &memory));
   assert(calls == 7 && memory == CUBIT_GPU_MEMORY_UNAVAILABLE);
   assert(cubit_gpu_native_query_call(&endpoint, &request, &reply));
   assert(calls == 8);
   request.words[1] = 4;
   assert(cubit_gpu_native_query_call(&endpoint, &request, &reply));
   assert(calls == 9);
   struct cubit_gpu_vm_contract vm = { .address_space_size=1, .private_context=true };
   assert(!cubit_gpu_query_vm(cubit_gpu_native_query_call, &endpoint, &vm));
   assert(calls == 10 && !vm.address_space_size && !vm.private_context);
   request.words[1] = 5;
   assert(!cubit_gpu_native_query_call(&endpoint, &request, &reply));
   assert(calls == 10);
   return 0;
}
