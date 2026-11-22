#include "cubit-device-query.h"
#include <assert.h>
#include <stdio.h>

static struct cubit_gpu_query_message response;
static bool transport = true;
static bool call(void *endpoint, const struct cubit_gpu_query_message *request,
                 struct cubit_gpu_query_message *reply)
{
   assert(endpoint == &response);
   assert(request->label == 0xa2e && request->length == 4);
   assert(!request->flags && !request->reserved);
   assert(request->words[0] == 2 && !request->words[1] &&
          !request->words[2] && !request->words[3]);
   *reply = response;
   return transport;
}
static void check(bool valid)
{
   struct cubit_gpu_budget_snapshot out = {1,2,3,4,5};
   assert(cubit_gpu_query_budget(call, &response, &out) == valid);
   if (!valid) {
      assert(!out.total && !out.retained && !out.available &&
             !out.max_allocation && !out.unused_tickets);
   } else {
      assert(out.total == response.words[1] && out.retained == response.words[2]);
      assert(out.available == out.total - out.retained);
      assert(out.unused_tickets == response.words[3]);
      assert(out.max_allocation <= out.available && out.max_allocation <= 16777216);
      if (!out.unused_tickets) assert(!out.max_allocation);
   }
}
int main(void)
{
   response = (struct cubit_gpu_query_message){.label=0xa2e,.length=4};
   response.words[1] = 33554432;
   for (unsigned pages=0; pages<=8192; pages++) {
      for (unsigned slots=0; slots<=16; slots++) {
         response.words[2] = (uint64_t)pages * 4096;
         response.words[3] = slots;
         check(true);
      }
   }
   response.words[2]=4096; response.words[3]=15;
   for (unsigned exponent=25; exponent<=50; exponent++) {
      response.words[1] = UINT64_C(1) << exponent;
      response.words[3] = 1000000;
      check(true);
   }
   response.words[1]=33554432; response.words[3]=15;
   const struct cubit_gpu_query_message good=response;
   response.words[3]=UINT64_C(2147483648); check(false);
   response=good; response.words[1]=0; check(false);
   response=good; response.words[2]=response.words[1]+4096; check(false);
   response=good;
   for (unsigned bit=0; bit<32; bit++) {
      response=good; response.label ^= UINT32_C(1)<<bit; check(false);
   }
   for (unsigned byte=0; byte<256; byte++) {
      response=good; response.length=byte; check(byte==4);
      response=good; response.flags=byte; check(byte==0);
   }
   for (unsigned bit=0; bit<16; bit++) {
      response=good; response.reserved=UINT16_C(1)<<bit; check(false);
   }
   for (unsigned field=0; field<4; field++) {
      response=good; response.words[field]=UINT64_MAX; check(false);
   }
   response=good; response.words[0]=4; check(false);
   response=good; response.words[2]++; check(false);
   response=good; transport=false; check(false);
   assert(!cubit_gpu_query_budget(call,&response,NULL));
   struct cubit_gpu_budget_snapshot out={1,2,3,4,5};
   assert(!cubit_gpu_query_budget(NULL,&response,&out) && !out.total && !out.max_allocation);
   puts("Mesa budget PASS: 139281 allocator combinations, malformed replies, transport failure; mock IPC");
   return 0;
}
