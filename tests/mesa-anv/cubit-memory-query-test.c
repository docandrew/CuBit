#include "cubit-device-query.h"
#include <assert.h>
#include <stddef.h>
static struct cubit_gpu_query_message response;
static bool available = true;
static bool exchange(void *endpoint, const struct cubit_gpu_query_message *r,
                     struct cubit_gpu_query_message *out)
{
   (void)endpoint;
   assert(r->label == 0xa20 && r->length == 4 && !r->flags && !r->reserved);
   assert(r->words[0] == 1 && r->words[1] == 3 && !r->words[2] && !r->words[3]);
   *out = response;
   return available;
}
static void reject(void)
{
   enum cubit_gpu_memory_contract out = CUBIT_GPU_MEMORY_OWNED_WB_COHERENT;
   assert(!cubit_gpu_query_memory(exchange, NULL, &out));
   assert(out == CUBIT_GPU_MEMORY_UNAVAILABLE);
}
int main(void)
{
   const struct cubit_gpu_query_message valid = {
      .label = 0xa20, .length = 4, .words = {0, 1, 2, 0},
   };
   enum cubit_gpu_memory_contract out;
   for (unsigned policy = 1; policy <= 2; policy++) {
      response = valid; response.words[2] = policy;
      assert(cubit_gpu_query_memory(exchange, NULL, &out) && out == policy);
   }
   for (unsigned word = 0; word < 4; word++) {
      for (unsigned bit = 0; bit < 64; bit++) {
         response = valid; response.words[word] ^= UINT64_C(1) << bit;
         reject();
      }
   }
   response = valid; available = false; reject(); available = true;
   response = valid; response.label++; reject();
   response = valid; response.length--; reject();
   response = valid; response.flags++; reject();
   response = valid; response.reserved++; reject();
   out = CUBIT_GPU_MEMORY_OWNED_WB_COHERENT;
   assert(!cubit_gpu_query_memory(NULL, NULL, &out) && !out);
   assert(!cubit_gpu_query_memory(exchange, NULL, NULL));
}
