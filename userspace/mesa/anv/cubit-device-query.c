#include "cubit-device-query.h"

bool
cubit_gpu_query_budget(cubit_gpu_query_call call, void *endpoint,
                       struct cubit_gpu_budget_snapshot *out)
{
   if (!out)
      return false;
   *out = (struct cubit_gpu_budget_snapshot){0};
   const struct cubit_gpu_query_message request = {
      .label = 0x0a2e, .length = 4, .words = {2, 0, 0, 0},
   };
   struct cubit_gpu_query_message reply = {0};
   if (!call || !call(endpoint, &request, &reply) ||
       reply.label != request.label || reply.length != 4 ||
       reply.flags || reply.reserved || reply.words[0] != 0)
      return false;
   const uint64_t total = reply.words[1], retained = reply.words[2];
   const uint64_t slots = reply.words[3], max_slice = UINT64_C(16) * 1024 * 1024;
   /* v2: owned backing and free records are independent budgets. The current
    * per-allocation ceiling remains 16 MiB, not a VRAM/system-RAM limit. */
   if (!total || total % 4096 || retained > total ||
       retained % 4096 || slots > UINT64_C(2147483647))
      return false;
   const uint64_t available = total - retained;
   *out = (struct cubit_gpu_budget_snapshot) {
      .total = total, .retained = retained, .available = available,
      .max_allocation = slots ? (available < max_slice ? available : max_slice) : 0,
      .unused_tickets = (uint32_t)slots,
   };
   return true;
}

static bool
query(cubit_gpu_query_call call, void *endpoint, uint64_t selector,
      struct cubit_gpu_query_message *reply)
{
   const struct cubit_gpu_query_message request = {
      .label = 0x0a20, .length = 4, .words = {1, selector, 0, 0},
   };
   *reply = (struct cubit_gpu_query_message){0};
   return call(endpoint, &request, reply) &&
          reply->label == request.label && reply->length == 4 &&
          reply->flags == 0 && reply->reserved == 0 &&
          reply->words[0] == 0 && reply->words[1] == 1;
}

bool
cubit_gpu_query_vm(cubit_gpu_query_call call, void *endpoint,
                   struct cubit_gpu_vm_contract *out)
{
   if (!out)
      return false;
   *out = (struct cubit_gpu_vm_contract){0};
   struct cubit_gpu_query_message reply;
   if (!call || !query(call, endpoint, 4, &reply) ||
       reply.words[2] != 48 || reply.words[3] != 1)
      return false;
   out->address_space_size = UINT64_C(1) << 48;
   out->private_context = true;
   return true;
}

bool
cubit_gpu_query_timestamp(cubit_gpu_query_call call, void *endpoint, uint32_t *hz)
{
   struct cubit_gpu_query_message reply;
   if (!call || !hz || !query(call, endpoint, 2, &reply) ||
       !reply.words[2] || reply.words[2] > UINT64_C(1025000000) || reply.words[3])
      return false;
   *hz = (uint32_t)reply.words[2];
   return true;
}

bool
cubit_gpu_query_memory(cubit_gpu_query_call call, void *endpoint,
                       enum cubit_gpu_memory_contract *out)
{
   if (!out)
      return false;
   *out = CUBIT_GPU_MEMORY_UNAVAILABLE;
   struct cubit_gpu_query_message reply;
   if (!call || !query(call, endpoint, 3, &reply) || reply.words[3] ||
       (reply.words[2] != CUBIT_GPU_MEMORY_OWNED_WB_EXPLICIT &&
        reply.words[2] != CUBIT_GPU_MEMORY_OWNED_WB_COHERENT))
      return false;
   *out = (enum cubit_gpu_memory_contract)reply.words[2];
   return true;
}

bool
cubit_gpu_query_device(cubit_gpu_query_call call, void *endpoint,
                       struct cubit_gpu_device_snapshot *out)
{
   if (!call || !out)
      return false;
   struct cubit_gpu_query_message reply;
   if (!query(call, endpoint, 0, &reply))
      return false;
   /* v1 has no public render feature bits. Reject unknown bits rather than
    * mistakenly interpreting a future service as this prototype contract. */
   if ((reply.words[2] >> 40) != 0 ||
       (reply.words[2] & UINT64_C(0xffffffff)) != UINT64_C(0x46d28086) ||
       reply.words[3] != 0)
      return false;
   struct cubit_gpu_device_snapshot result = {
      .device = 0x46d2, .pci_revision = (uint8_t)(reply.words[2] >> 32),
   };
   if (!query(call, endpoint, 1, &reply))
      return false;
   const uint64_t dss = reply.words[2], eu = reply.words[3];
   if (!dss || dss > 0x3f || !eu || eu > 0xffff ||
       (eu & 0x5555) != ((eu >> 1) & 0x5555))
      return false;
   result.dss_mask = (uint8_t)dss;
   result.eu_mask = (uint16_t)eu;
   *out = result;
   return true;
}
