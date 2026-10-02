#include "cubit-device-query.h"
#include <assert.h>
#include <stddef.h>
static struct cubit_gpu_query_message response;
static bool transport = true;
static bool exchange(void *endpoint, const struct cubit_gpu_query_message *r,
                     struct cubit_gpu_query_message *out)
{
   (void)endpoint;
   assert(r->label==0xa20 && r->length==4 && !r->flags && !r->reserved);
   assert(r->words[0]==1 && r->words[1]==2 && !r->words[2] && !r->words[3]);
   *out=response; return transport;
}
static void reject(void)
{
   uint32_t hz=123;
   assert(!cubit_gpu_query_timestamp(exchange,NULL,&hz));
   assert(hz==123);
}
int main(void)
{
   const struct cubit_gpu_query_message valid={.label=0xa20,.length=4,
                                               .words={0,1,12000000,0}};
   response=valid; uint32_t hz=0;
   assert(cubit_gpu_query_timestamp(exchange,NULL,&hz) && hz==12000000);
   assert(!cubit_gpu_query_timestamp(NULL,NULL,&hz));
   assert(!cubit_gpu_query_timestamp(exchange,NULL,NULL));
   transport=false; reject(); transport=true;
   for(unsigned w=0;w<4;w++) {
      if(w==2) continue;
      for(unsigned b=0;b<64;b++) {
         response=valid; response.words[w]^=UINT64_C(1)<<b; reject();
      }
   }
   const uint64_t invalid[]={0,1025000001,UINT64_C(1)<<32,UINT64_MAX};
   for(unsigned i=0;i<4;i++) { response=valid; response.words[2]=invalid[i]; reject(); }
   response=valid; response.flags=1; reject();
   response=valid; response.reserved=1; reject();
   response=valid; response.label++; reject();
   response=valid; response.length=3; reject();
}
