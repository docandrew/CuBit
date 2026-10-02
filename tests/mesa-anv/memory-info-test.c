#include "anv_private.h"
#include "cubit-memory-info.h"
#include <assert.h>
#include <stdio.h>
#include <pthread.h>

struct reader {
   struct anv_physical_device *device;
   bool fail;
};
static bool concurrent_call(void *endpoint,
   const struct cubit_gpu_query_message *request,
   struct cubit_gpu_query_message *reply)
{
   const struct reader *r = endpoint;
   assert(request->label == 0xa2e);
   *reply = (struct cubit_gpu_query_message){
      .label=0xa2e,.length=4,.words={0,33554432,4096,15}};
   return !r->fail;
}
static void *read_budgets(void *arg)
{
   struct reader *r = arg;
   for (unsigned n=0;n<4096;n++) {
      p_atomic_add(&r->device->memory.heaps_budget->used[0], 4096);
      VkPhysicalDeviceMemoryBudgetPropertiesEXT out = {
         .sType=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT,
         .pNext=r};
      assert(cubit_mesa_memory_budget(r->device,concurrent_call,r,&out)==!r->fail);
      assert(out.sType==VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT && out.pNext==r);
      assert(out.heapUsage[0]>=4096 && out.heapUsage[0]<=4*4096);
      assert(out.heapBudget[0]>=out.heapUsage[0] && out.heapBudget[0]<=33554432);
      if(r->fail) assert(out.heapBudget[0]==out.heapUsage[0]);
      for(unsigned i=1;i<VK_MAX_MEMORY_HEAPS;i++)
         assert(!out.heapBudget[i] && !out.heapUsage[i]);
      p_atomic_add(&r->device->memory.heaps_budget->used[0], -4096);
   }
   return NULL;
}

static struct cubit_gpu_query_message response = {
   .label=0xa2e, .length=4, .words={0,33554432,4096,15},
};
static bool transport=true;
static uint64_t policy=2;
static bool call(void *endpoint, const struct cubit_gpu_query_message *request,
                 struct cubit_gpu_query_message *reply)
{
   assert(endpoint == &response);
   if (request->label == 0xa20) {
      assert(request->words[1]==3);
      *reply=(struct cubit_gpu_query_message){.label=0xa20,.length=4,
                                            .words={0,1,policy,0}};
      return transport;
   }
   assert(request->label == 0xa2e);
   *reply=response;
   return transport;
}
int main(void)
{
   struct anv_physical_device *device=calloc(1,sizeof(*device));
   assert(device);
   assert(cubit_mesa_refresh_memory_info(device,call,&response));
   assert(device->sys.size==33554432 && device->sys.available==33550336);
   assert(device->info.mem.sram.mappable.size==device->sys.size &&
          device->info.mem.sram.mappable.free==device->sys.available);
   assert(!device->memory.heap_count && !device->memory.type_count);
   assert(!device->sys.region && !device->memory.need_flush && !device->kmd_backend);
   /* All tickets consumed: byte tail exists, but none is allocatable. */
   response.words[2]=65536; response.words[3]=0;
   assert(cubit_mesa_refresh_memory_info(device,call,&response));
   assert(device->sys.size==33554432 && device->sys.available==0);
   response.words[2]=4096; response.words[3]=15;
   assert(cubit_mesa_refresh_memory_info(device,call,&response));
   transport=false;
   assert(!cubit_mesa_refresh_memory_info(device,call,&response));
   assert(device->sys.available==0 && device->sys.size==33554432);
   assert(!device->info.mem.sram.mappable.free);
   transport=true; response.words[0]=4;
   device->sys.available=123;
   assert(!cubit_mesa_refresh_memory_info(device,call,&response));
   assert(device->sys.available==0);
   response.words[0]=0; device->sys.size=16777216;
   assert(!cubit_mesa_refresh_memory_info(device,call,&response));
   assert(device->sys.size==16777216 && device->sys.available==0);
   device->sys.size=0;
   device->info.mem.sram.mappable.size=16777216;
   device->info.mem.sram.mappable.free=123;
   assert(!cubit_mesa_refresh_memory_info(device,call,&response));
   assert(!device->sys.size && !device->info.mem.sram.mappable.free &&
          device->info.mem.sram.mappable.size==16777216);
   device->sys.size=0; device->info.has_local_mem=true;
   assert(!cubit_mesa_refresh_memory_info(device,call,&response));
   assert(!device->sys.size && !device->sys.available);
   assert(!cubit_mesa_refresh_memory_info(NULL,call,&response));
   memset(device,0,sizeof(*device));
   assert(cubit_mesa_refresh_memory_info(device,call,&response));
   device->sys.region=&device->info.mem.sram.mem;
   device->memory.heap_count=1;
   device->memory.heaps[0]=(struct anv_memory_heap){
      .size=device->sys.size,.flags=VK_MEMORY_HEAP_DEVICE_LOCAL_BIT};
   for(policy=0;policy<=3;policy++) {
      if(policy==2) continue;
      assert(!cubit_mesa_init_memory_types(device,call,&response));
      assert(!device->memory.type_count);
   }
   policy=2;
   transport=false;
   assert(!cubit_mesa_init_memory_types(device,call,&response));
   transport=true;
   response.words[0]=4;
   assert(!cubit_mesa_init_memory_types(device,call,&response));
   assert(!device->sys.available && !device->info.mem.sram.mappable.free);
   response.words[0]=0;
   device->memory.heaps[0].size--;
   assert(!cubit_mesa_init_memory_types(device,call,&response));
   device->memory.heaps[0].size++;
   device->has_protected_contexts=true;
   assert(!cubit_mesa_init_memory_types(device,call,&response));
   device->has_protected_contexts=false;
   device->memory.heaps[0].is_local_mem=true;
   assert(!cubit_mesa_init_memory_types(device,call,&response));
   device->memory.heaps[0].is_local_mem=false;
   device->sys.region=NULL;
   assert(!cubit_mesa_init_memory_types(device,call,&response));
   device->sys.region=&device->info.mem.sram.mem;
   device->memory.heaps[0].flags=0;
   assert(!cubit_mesa_init_memory_types(device,call,&response));
   device->memory.heaps[0].flags=VK_MEMORY_HEAP_DEVICE_LOCAL_BIT;
   device->memory.heap_count=2;
   assert(!cubit_mesa_init_memory_types(device,call,&response));
   device->memory.heap_count=1;
   assert(cubit_mesa_init_memory_types(device,call,&response));
   assert(device->memory.type_count==1 && !device->memory.need_flush);
   assert(device->memory.types[0].propertyFlags==
     (VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT|VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|
      VK_MEMORY_PROPERTY_HOST_COHERENT_BIT|VK_MEMORY_PROPERTY_HOST_CACHED_BIT));
   assert(!device->memory.types[0].heapIndex &&
          !device->memory.types[0].compressed && !device->memory.types[0].dynamic_visible);
   assert(!cubit_mesa_init_memory_types(device,call,&response)); /* No reinit. */
   struct anv_memory_budget accounting = {0};
   device->memory.heaps_budget=&accounting;
   VkPhysicalDeviceMemoryBudgetPropertiesEXT budget = {
      .sType=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT,
      .pNext=device};
   const uint64_t usages[]={0,1,4096,33554431,33554432,UINT64_MAX};
   const uint64_t retained[]={0,4096,65536,16777216,33550336,33554432};
   for(unsigned u=0;u<ARRAY_SIZE(usages);u++) {
      p_atomic_set(&accounting.used[0],usages[u]);
      for(unsigned r=0;r<ARRAY_SIZE(retained);r++) {
         for(unsigned tickets=0;tickets<=16;tickets++) {
            response.words[2]=retained[r]; response.words[3]=tickets;
            for(unsigned fails=0;fails<2;fails++) {
               transport=!fails;
               bool valid=!fails && retained[r]>=(16-tickets)*UINT64_C(4096) &&
                  retained[r]<=(16-tickets)*UINT64_C(16777216);
               uint64_t used=MIN2(usages[u],UINT64_C(33554432));
               uint64_t avail=valid && tickets ?
                  MIN2(33554432-retained[r],33554432-used) : 0;
               assert(cubit_mesa_memory_budget(device,call,&response,&budget)==valid);
               assert(budget.heapUsage[0]==used && budget.heapBudget[0]==MAX2(UINT64_C(1),used+avail));
               assert(budget.sType==VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT && budget.pNext==device);
               for(unsigned i=1;i<VK_MAX_MEMORY_HEAPS;i++)
                  assert(!budget.heapBudget[i] && !budget.heapUsage[i]);
            }
         }
      }
   }
   p_atomic_set(&accounting.used[0],0);
   transport=true; response.words[1]=16777216;
   assert(!cubit_mesa_memory_budget(device,call,&response,&budget));
   assert(budget.heapBudget[0]==1 && !budget.heapUsage[0]);
   assert(!cubit_mesa_memory_budget(NULL,call,&response,&budget));
   assert(!budget.heapBudget[0] && !budget.heapUsage[0]);
   assert(!cubit_mesa_memory_budget(device,call,&response,NULL));
   struct anv_physical_device *before=malloc(sizeof(*before)); assert(before);
   memcpy(before,device,sizeof(*before));
   pthread_t threads[4]; struct reader readers[4];
   for(unsigned i=0;i<4;i++) {
      readers[i]=(struct reader){device,i%2};
      assert(!pthread_create(&threads[i],NULL,read_budgets,&readers[i]));
   }
   for(unsigned i=0;i<4;i++) assert(!pthread_join(threads[i],NULL));
   assert(!p_atomic_read(&accounting.used[0]));
   assert(!memcmp(before,device,sizeof(*before))); /* No shared discovery writes. */
   free(before);
   free(device);
   puts("ANV memory policy PASS: coherent UMA, explicit-only rejected, 1224 budget boundaries, 16384 concurrent snapshots (mock IPC)");
   return 0;
}
