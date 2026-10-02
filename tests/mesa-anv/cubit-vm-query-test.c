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
   assert(r->words[0] == 1 && r->words[1] == 4 && !r->words[2] && !r->words[3]);
   *out = response;
   return available;
}
static void reject(void)
{
   struct cubit_gpu_vm_contract out = { .address_space_size=1, .private_context=true };
   assert(!cubit_gpu_query_vm(exchange, NULL, &out));
   assert(!out.address_space_size && !out.private_context);
}
int main(void)
{
   const struct cubit_gpu_query_message valid = {
      .label = 0xa20, .length = 4, .words = {0, 1, 48, 1},
   };
   struct cubit_gpu_vm_contract out;
   response = valid;
   assert(cubit_gpu_query_vm(exchange, NULL, &out));
   assert(out.private_context && out.address_space_size == (UINT64_C(1) << 48));
   for (unsigned word = 0; word < 4; word++)
      for (unsigned bit = 0; bit < 64; bit++) {
         response = valid; response.words[word] ^= UINT64_C(1) << bit;
         reject();
      }
   response = valid; available = false; reject(); available = true;
   response = valid; response.label++; reject();
   response = valid; response.length--; reject();
   response = valid; response.flags++; reject();
   response = valid; response.reserved++; reject();
   out = (struct cubit_gpu_vm_contract){ .address_space_size=1, .private_context=true };
   assert(!cubit_gpu_query_vm(NULL, NULL, &out));
   assert(!out.address_space_size && !out.private_context);
   assert(!cubit_gpu_query_vm(exchange, NULL, NULL));
}
