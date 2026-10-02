#include "cubit-device-query.h"
#include <assert.h>
#include <string.h>

struct fixture {
   struct cubit_gpu_query_message replies[2];
   unsigned calls, fail_at;
};
static bool exchange(void *endpoint, const struct cubit_gpu_query_message *request,
                     struct cubit_gpu_query_message *reply)
{
   struct fixture *f = endpoint;
   assert(f->calls < 2);
   assert(request->label == 0x0a20 && request->length == 4);
   assert(request->flags == 0 && request->reserved == 0);
   assert(request->words[0] == 1 && request->words[1] == f->calls);
   assert(request->words[2] == 0 && request->words[3] == 0);
   *reply = f->replies[f->calls++];
   return f->calls != f->fail_at;
}
static void reject(struct fixture f)
{
   struct cubit_gpu_device_snapshot out;
   memset(&out, 0xa5, sizeof(out));
   struct cubit_gpu_device_snapshot before = out;
   assert(!cubit_gpu_query_device(exchange, &f, &out));
   assert(memcmp(&out, &before, sizeof(out)) == 0);
}
int main(void)
{
   const struct fixture valid = {.replies = {
      {.label = 0x0a20, .length = 4, .words = {0, 1, 0x1146d28086, 0}},
      {.label = 0x0a20, .length = 4, .words = {0, 1, 1, 0xffff}},
   }};
   struct cubit_gpu_device_snapshot out;
   struct fixture f = valid;
   assert(cubit_gpu_query_device(exchange, &f, &out) && f.calls == 2);
   assert(out.device == 0x46d2 && out.pci_revision == 17);
   assert(out.dss_mask == 1 && out.eu_mask == 0xffff);
   assert(!cubit_gpu_query_device(NULL, &f, &out));
   assert(!cubit_gpu_query_device(exchange, &f, NULL));
   for (unsigned stage = 0; stage < 2; stage++) {
      f = valid; f.fail_at = stage + 1; reject(f);
      for (unsigned field = 0; field < 4; field++) {
         for (unsigned bit = 0; bit < 32; bit++) {
            f = valid;
            if (field == 0) f.replies[stage].label ^= 1u << bit;
            else if (field == 1 && bit < 8) f.replies[stage].length ^= 1u << bit;
            else if (field == 2 && bit < 8) f.replies[stage].flags ^= 1u << bit;
            else if (field == 3 && bit < 16) f.replies[stage].reserved ^= 1u << bit;
            else continue;
            reject(f);
         }
      }
      for (unsigned word = 0; word < 4; word++) {
         for (unsigned bit = 0; bit < 64; bit++) {
            f = valid; f.replies[stage].words[word] ^= UINT64_C(1) << bit;
            /* Only the documented revision and nonempty DSS bits can vary
             * individually in these two valid baseline messages. */
            bool allowed = (stage == 0 && word == 2 && bit >= 32 && bit < 40) ||
                           (stage == 1 && word == 2 && bit > 0 && bit < 6);
            if (!allowed) reject(f);
            else assert(cubit_gpu_query_device(exchange, &f, &out));
         }
      }
   }
   for (unsigned eu = 0; eu < 65536; eu++) {
      f = valid; f.replies[1].words[3] = eu;
      bool good = eu && ((eu & 0x5555) == ((eu >> 1) & 0x5555));
      assert(cubit_gpu_query_device(exchange, &f, &out) == good);
      if (good) assert(out.eu_mask == eu);
   }
   return 0;
}
