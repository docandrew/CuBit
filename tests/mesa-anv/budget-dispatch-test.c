#include "anv_private.h"
#include <assert.h>

static unsigned snapshots, refreshes;
void anv_update_meminfo(struct anv_physical_device *d)
{ assert(d); refreshes++; }
static void snapshot(struct anv_physical_device *d,
                     VkPhysicalDeviceMemoryBudgetPropertiesEXT *out)
{
   assert(d); snapshots++;
   memset(out->heapBudget,0,sizeof(out->heapBudget));
   memset(out->heapUsage,0,sizeof(out->heapUsage));
   out->heapBudget[0]=1;
}
int main(void)
{
   struct anv_physical_device *d=calloc(1,sizeof(*d)); assert(d);
   d->vk.base.type=VK_OBJECT_TYPE_PHYSICAL_DEVICE;
   d->vk.supported_extensions.EXT_memory_budget=true;
   d->memory.heap_count=1;
   d->memory.heaps[0].size=33554432;
   d->sys.available=33554432;
   struct anv_memory_budget accounting={0};
   d->memory.heaps_budget=&accounting;
   struct anv_kmd_backend backend={.get_memory_budget=snapshot};
   d->kmd_backend=&backend;
   VkPhysicalDeviceMemoryBudgetPropertiesEXT budget={
      .sType=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT};
   VkPhysicalDeviceMemoryProperties2 props={
      .sType=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2,
      .pNext=&budget};
   anv_GetPhysicalDeviceMemoryProperties2(anv_physical_device_to_handle(d),&props);
   assert(snapshots==1 && refreshes==0 && budget.heapBudget[0]==1);
   assert(props.memoryProperties.memoryHeapCount==1 &&
          props.memoryProperties.memoryHeaps[0].size==33554432);
   assert(props.pNext==&budget &&
          budget.sType==VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT);
   d->vk.supported_extensions.EXT_memory_budget=false;
   anv_GetPhysicalDeviceMemoryProperties2(anv_physical_device_to_handle(d),&props);
   assert(snapshots==1 && refreshes==0);
   d->vk.supported_extensions.EXT_memory_budget=true;
   backend.get_memory_budget=NULL;
   anv_GetPhysicalDeviceMemoryProperties2(anv_physical_device_to_handle(d),&props);
   assert(snapshots==1 && refreshes==1 && budget.heapBudget[0]>1);
   free(d);
   return 0;
}
