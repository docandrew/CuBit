/* Hosted Vulkan-call contract test, NOT Mesa execution or GPU emulation. */
#define _DEFAULT_SOURCE
#include "native-transfer-probe.h"
#include <assert.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>

#define HANDLE(type, n) ((type)(uintptr_t)(n))
static uint32_t storage[1024], pattern;
static unsigned calls, fail_at, submits, waits, frees, fills, barriers;
static unsigned resources;
static unsigned idle_calls;
static bool coherent, mismatch, timeout_first, lost, transient_first;
static const char *missing;
static VkResult step(void) { return ++calls == fail_at ? VK_ERROR_OUT_OF_HOST_MEMORY : VK_SUCCESS; }
static void log_message(const char *format, ...) { (void)format; }
static void properties(VkPhysicalDevice p, VkPhysicalDeviceMemoryProperties *out)
{
   *out = (VkPhysicalDeviceMemoryProperties){.memoryTypeCount=1};
   out->memoryTypes[0].propertyFlags = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
      (coherent ? VK_MEMORY_PROPERTY_HOST_COHERENT_BIT : 0);
}
static VkResult create_buffer(VkDevice d, const VkBufferCreateInfo *i,
                             const VkAllocationCallbacks *a, VkBuffer *out)
{
   assert(i->size == 4096 && i->usage == VK_BUFFER_USAGE_TRANSFER_DST_BIT);
   VkResult r=step(); if (!r) { *out=HANDLE(VkBuffer,1); resources++; } return r;
}
static void destroy_buffer(VkDevice d, VkBuffer b, const VkAllocationCallbacks *a)
{ assert(b); resources--; frees++; }
static void requirements(VkDevice d, VkBuffer b, VkMemoryRequirements *out)
{ *out=(VkMemoryRequirements){.size=4096,.alignment=4096,.memoryTypeBits=1}; }
static VkResult allocate(VkDevice d, const VkMemoryAllocateInfo *i,
                         const VkAllocationCallbacks *a, VkDeviceMemory *out)
{ assert(i->allocationSize==4096 && i->memoryTypeIndex==0);
  VkResult r=step(); if (!r) { *out=HANDLE(VkDeviceMemory,2); resources++; } return r; }
static void free_memory(VkDevice d, VkDeviceMemory m, const VkAllocationCallbacks *a)
{ resources--; frees++; }
static VkResult bind(VkDevice d, VkBuffer b, VkDeviceMemory m, VkDeviceSize offset)
{ assert(offset==0); return step(); }
static VkResult map(VkDevice d, VkDeviceMemory m, VkDeviceSize offset, VkDeviceSize size,
                    VkMemoryMapFlags flags, void **out)
{ assert(size==4096 && offset==0); VkResult r=step(); if (!r) *out=storage; return r; }
static void unmap(VkDevice d, VkDeviceMemory m) { }
static VkResult create_pool(VkDevice d, const VkCommandPoolCreateInfo *i,
                           const VkAllocationCallbacks *a, VkCommandPool *out)
{ assert(i->queueFamilyIndex==0); VkResult r=step();
  if (!r) { *out=HANDLE(VkCommandPool,3); resources++; } return r; }
static void destroy_pool(VkDevice d, VkCommandPool p, const VkAllocationCallbacks *a)
{ resources--; frees++; }
static VkResult commands(VkDevice d, const VkCommandBufferAllocateInfo *i, VkCommandBuffer *out)
{ assert(i->commandBufferCount==1); VkResult r=step();
  if (!r) { *out=HANDLE(VkCommandBuffer,4); } return r; }
static VkResult begin(VkCommandBuffer c, const VkCommandBufferBeginInfo *i) { return step(); }
static VkResult end(VkCommandBuffer c) { return step(); }
static void fill(VkCommandBuffer c, VkBuffer b, VkDeviceSize offset, VkDeviceSize size, uint32_t data)
{ assert(offset==0 && size==4096); fills++; pattern=data; }
static void barrier(VkCommandBuffer c, VkPipelineStageFlags src, VkPipelineStageFlags dst,
                    VkDependencyFlags flags, uint32_t mc, const VkMemoryBarrier *m,
                    uint32_t bc, const VkBufferMemoryBarrier *b, uint32_t ic,
                    const VkImageMemoryBarrier *i)
{
   assert(src==VK_PIPELINE_STAGE_TRANSFER_BIT && dst==VK_PIPELINE_STAGE_HOST_BIT);
   assert(mc==0 && bc==1 && ic==0 && b->srcAccessMask==VK_ACCESS_TRANSFER_WRITE_BIT);
   assert(b->dstAccessMask==VK_ACCESS_HOST_READ_BIT && b->size==4096);
   assert(b->srcQueueFamilyIndex==VK_QUEUE_FAMILY_IGNORED);
   assert(b->dstQueueFamilyIndex==VK_QUEUE_FAMILY_IGNORED);
   barriers++;
}
static VkResult create_fence(VkDevice d, const VkFenceCreateInfo *i,
                            const VkAllocationCallbacks *a, VkFence *out)
{ VkResult r=step(); if (!r) { *out=HANDLE(VkFence,5); resources++; } return r; }
static void destroy_fence(VkDevice d, VkFence f, const VkAllocationCallbacks *a)
{ resources--; frees++; }
static void get_queue(VkDevice d, uint32_t family, uint32_t index, VkQueue *out)
{ assert(family==0 && index==0); *out=HANDLE(VkQueue,6); }
static VkResult submit(VkQueue q, uint32_t count, const VkSubmitInfo *s, VkFence f)
{ submits++; assert(count==1 && s->commandBufferCount==1 && fills==1 && barriers==1);
  return step(); }
static VkResult wait_fence(VkDevice d, uint32_t count, const VkFence *f, VkBool32 all, uint64_t timeout)
{
   assert(resources==4 && frees==0 && submits==1 && count==1 && all && timeout);
   waits++;
   if (timeout_first && waits==1) return VK_TIMEOUT;
   if (transient_first && waits==1) return VK_ERROR_OUT_OF_HOST_MEMORY;
   if (lost) return VK_ERROR_DEVICE_LOST;
   for (unsigned n=0; n<1024; n++) storage[n]=pattern;
   if (mismatch) storage[1023]=0;
   return VK_SUCCESS;
}
static VkResult idle(VkDevice d)
{ assert(resources==4 && frees==0 && submits==1); idle_calls++; return VK_SUCCESS; }
static PFN_vkVoidFunction device_proc(VkDevice d, const char *name)
{
   if (missing && !strcmp(missing,name)) return NULL;
#define ENTRY(n, f) if (!strcmp(name,"vk" #n)) return (PFN_vkVoidFunction)f
   ENTRY(CreateBuffer,create_buffer); ENTRY(DestroyBuffer,destroy_buffer);
   ENTRY(GetBufferMemoryRequirements,requirements); ENTRY(AllocateMemory,allocate);
   ENTRY(FreeMemory,free_memory); ENTRY(BindBufferMemory,bind); ENTRY(MapMemory,map);
   ENTRY(UnmapMemory,unmap); ENTRY(CreateCommandPool,create_pool);
   ENTRY(DestroyCommandPool,destroy_pool); ENTRY(AllocateCommandBuffers,commands);
   ENTRY(BeginCommandBuffer,begin); ENTRY(EndCommandBuffer,end); ENTRY(CmdFillBuffer,fill);
   ENTRY(CmdPipelineBarrier,barrier); ENTRY(CreateFence,create_fence);
   ENTRY(DestroyFence,destroy_fence); ENTRY(GetDeviceQueue,get_queue);
   ENTRY(QueueSubmit,submit); ENTRY(WaitForFences,wait_fence);
   ENTRY(DeviceWaitIdle,idle);
#undef ENTRY
   return NULL;
}
static PFN_vkVoidFunction instance_proc(VkInstance i, const char *name)
{
   if (!strcmp(name,"vkGetDeviceProcAddr")) return (PFN_vkVoidFunction)device_proc;
   if (!strcmp(name,"vkGetPhysicalDeviceMemoryProperties")) return (PFN_vkVoidFunction)properties;
   return NULL;
}
static VkResult run(void)
{ return mesa_transfer_probe(HANDLE(VkInstance,1), HANDLE(VkPhysicalDevice,2),
                             HANDLE(VkDevice,3), instance_proc, log_message); }
static void reset(void)
{
   assert(resources==0);
   calls=fail_at=submits=waits=frees=fills=barriers=0;
   idle_calls=0;
   coherent=true; mismatch=timeout_first=lost=transient_first=false; missing=NULL;
}
int main(void)
{
   reset(); assert(run()==VK_SUCCESS && waits==1 && resources==0);
   reset(); timeout_first=true; assert(run()==VK_SUCCESS && waits==2 && submits==1);
   reset(); transient_first=true; assert(run()==VK_SUCCESS && waits==2 && submits==1);
   reset(); lost=true; assert(run()==VK_ERROR_DEVICE_LOST && resources==0);
   reset(); mismatch=true; assert(run()==VK_ERROR_UNKNOWN && resources==0);
   reset(); coherent=false; assert(run()==VK_ERROR_FEATURE_NOT_PRESENT && submits==0);
   reset(); missing="vkCmdPipelineBarrier"; assert(run()==VK_ERROR_INITIALIZATION_FAILED && calls==0);
   for (unsigned n=1; n<=10; ++n) {
      reset(); fail_at=n; assert(run()==VK_ERROR_OUT_OF_HOST_MEMORY && resources==0 && waits==0);
      assert(idle_calls==(n==10 ? 1u : 0u));
   }
   puts("Vulkan transfer fixture PASS: ordering/readback/retention and ten failures (hosted mocks only)");
}
