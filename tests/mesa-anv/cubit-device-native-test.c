#include "cubit-device-query.h"
#include "native_gpu_query.h"
#include <assert.h>
#include <string.h>
#include "../../userspace/c/cubit_handles.h"
/* A test endpoint handle (generation 1). */
#define TEST_ENDPOINT(n) ((cubit_endpoint){(UINT64_C(1) << 32) | (uint64_t)(n)})
/* The index a test endpoint was made from. */
#define TEST_INDEX(e) ((unsigned)((e).opaque & 0xffffffffu))
static unsigned calls;
static uint32_t status;
uint32_t cubit_intel_budget(cubit_endpoint endpoint, uint64_t words[4])
{
   assert(endpoint.opaque == TEST_ENDPOINT(63).opaque);
   calls++;
   words[0] = 0;
   words[1] = 33554432;
   words[2] = 4096;
   words[3] = 15;
   return status;
}
uint32_t cubit_intel_query(cubit_endpoint endpoint, uint64_t selector, uint64_t words[4])
{
   assert(endpoint.opaque == TEST_ENDPOINT(63).opaque && selector <= 4);
   calls++;
   words[0] = 2; /* Well-formed UNAVAILABLE is not a transport failure. */
   words[1] = 1;
   words[2] = words[3] = 0;
   return status;
}
int main(void)
{
   struct cubit_gpu_native_endpoint endpoint = {TEST_ENDPOINT(63)};
   struct cubit_gpu_query_message request = {
      .label = 0x0a20, .length = 4, .words = {1, 1, 0, 0},
   };
   struct cubit_gpu_query_message reply;
   memset(&reply, 0xa5, sizeof(reply));
   struct cubit_gpu_query_message before = reply;
   assert(!cubit_gpu_native_query_call(NULL, &request, &reply));
   assert(!cubit_gpu_native_query_call(&endpoint, NULL, &reply));
   assert(!cubit_gpu_native_query_call(&endpoint, &request, NULL));
   /* No handle (generation zero) is refused before any call. */
   endpoint.endpoint = CUBIT_NO_ENDPOINT;
   assert(!cubit_gpu_native_query_call(&endpoint, &request, &reply));
   endpoint.endpoint = (cubit_endpoint){UINT64_C(0xffffffff)};
   assert(!cubit_gpu_native_query_call(&endpoint, &request, &reply));
   endpoint.endpoint = TEST_ENDPOINT(63);
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
   request = (struct cubit_gpu_query_message){.label=0xa2e, .length=4,
                                             .words={1,0,0,0}};
   assert(!cubit_gpu_native_query_call(&endpoint, &request, &reply));
   assert(calls == 10); /* obsolete budget version never reaches transport */
   request.words[0] = 2;
   assert(cubit_gpu_native_query_call(&endpoint, &request, &reply));
   assert(calls == 11);
   return 0;
}
