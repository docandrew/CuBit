#include "cubit-device-query.h"
#include "native_gpu_query.h"

/* Ada owns the native IPC layout and capability call; only scalar words cross
 * this FFI. Zero means transport succeeded, NOT that the query status is OK. */

bool
cubit_gpu_native_query_call(void *endpoint,
                            const struct cubit_gpu_query_message *request,
                            struct cubit_gpu_query_message *reply)
{
   if (!endpoint || !request || !reply)
      return false;
   const struct cubit_gpu_native_endpoint *native = endpoint;
   const bool budget = request->label == 0x0a2e;
   if (native->slot > 63 || (!budget && request->label != 0x0a20) ||
       request->length != 4 || request->flags || request->reserved ||
       request->words[0] != (budget ? 2 : 1) || request->words[1] > (budget ? 0 : 4) ||
       request->words[2] || request->words[3])
      return false;
   struct cubit_gpu_query_message result = {.label = request->label, .length = 4};
   const uint32_t transport = budget ? cubit_intel_budget(native->slot, result.words) :
      cubit_intel_query(native->slot, request->words[1], result.words);
   if (transport != 0)
      return false;
   *reply = result;
   return true;
}
