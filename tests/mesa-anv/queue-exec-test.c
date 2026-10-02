/* Actual ANV adapter/types; mocked IPC, sync and common batch chaining. */
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include <assert.h>
#include <stdio.h>
static unsigned chains, flushes, submits, signals;
static unsigned phase;
static uint32_t markers[64], submit_status;
static VkResult wait_status = VK_SUCCESS, signal_status = VK_SUCCESS;
static struct anv_cmd_buffer *commands[2];
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
   int line, const char *message, ...)
{ (void)file; (void)line; (void)message; p_atomic_set(&device->_lost.lost, 1); return VK_ERROR_DEVICE_LOST; }
uint32_t cubit_intel_prepare_context(uint64_t slot) { markers[slot]=1; return 0; }
uint32_t cubit_intel_register_context(uint64_t slot) { (void)slot; return 0; }
uint32_t cubit_intel_submit_batch(uint64_t slot, uint32_t handle,
   uint64_t gpu, uint64_t offset, uint64_t bytes, uint32_t previous, uint32_t *completion)
{
   assert(handle == 17 && gpu == 0x20000 && offset == 0 && bytes == 4096);
   assert(previous == markers[slot]);
   assert(phase==3); phase=4;
   submits++; *completion=++markers[slot]; return submit_status;
}
VkResult vk_sync_wait_many(struct vk_device *device, uint32_t count,
   const struct vk_sync_wait *waits, enum vk_sync_wait_flags flags, uint64_t deadline)
{ (void)device; (void)count; (void)waits; assert(phase==0 && flags==0 && deadline==0); phase=1; return wait_status; }
VkResult vk_sync_signal_many(struct vk_device *device, uint32_t count,
   const struct vk_sync_signal *values)
{ (void)device; assert(count==1 && values && (phase==1 || phase==4 || phase==5));
  if(phase==5) assert(values->sync==(struct vk_sync *)(uintptr_t)1 && values->signal_value==0);
  phase=5; signals++; return signal_status; }
void anv_cmd_buffer_chain_command_buffers(struct anv_cmd_buffer **cmds, uint32_t count)
{ assert(cmds==commands && count==2 && phase==1); phase=2; chains++; }
void anv_cmd_buffer_clflush(struct anv_cmd_buffer **cmds, uint32_t count)
{ assert(cmds==commands && count==2 && phase==2); phase=3; flushes++; }
static struct vk_sync_signal output;
static VkResult run(struct anv_queue *queue, unsigned count)
{ phase=0; return anv_cubit_queue_exec_locked(queue,0,NULL,count,commands,1,&output,NULL,0,NULL); }
int main(void)
{
   static struct anv_device devices[4];
   static struct anv_physical_device physical;
   static struct anv_cmd_buffer cmd[2];
   struct anv_bo bos[2]={{.gem_handle=17,.offset=0x20000,.actual_size=4096},
                         {.gem_handle=18,.offset=0x30000,.actual_size=4096}};
   struct anv_batch_bo batches[2]={{.bo=&bos[0],.length=4096},{.bo=&bos[1],.length=4096}};
   physical.queue.family_count=1;
   physical.queue.families[0].engine_class=INTEL_ENGINE_CLASS_RENDER;
   float priority=0.5f;
   VkDeviceQueueCreateInfo qi={.sType=VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
      .queueCount=1,.pQueuePriorities=&priority};
   VkDeviceCreateInfo ci={.sType=VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
      .queueCreateInfoCount=1,.pQueueCreateInfos=&qi};
   struct anv_queue queue={.device=&devices[0],.family=&physical.queue.families[0]};
   physical.memory.need_flush=true;
   for(unsigned i=0;i<4;i++) {
      devices[i].physical=&physical;
      devices[i].batch_bo_pool.bo_alloc_flags=ANV_BO_ALLOC_HOST_CACHED;
      assert(anv_cubit_memory_init(&devices[i],63-i)==VK_SUCCESS);
      assert(anv_cubit_setup_context(&devices[i],&ci,1)==VK_SUCCESS);
      queue.device=&devices[i];
      assert(anv_cubit_create_engine(&devices[i],&queue,&qi)==VK_SUCCESS);
      assert(anv_cubit_prepare_submission(&devices[i])==VK_SUCCESS);
   }
   queue.device=&devices[0];
   for(unsigned i=0;i<2;i++) {
      commands[i]=&cmd[i]; cmd[i].device=&devices[0];
      list_inithead(&cmd[i].batch_bos); list_addtail(&batches[i].link,&cmd[i].batch_bos);
   }
   assert(run(&queue,2)==VK_SUCCESS);
   assert(chains==1 && flushes==1 && submits==1 && signals==1);
   assert(run(&queue,0)==VK_SUCCESS);
   assert(chains==1 && submits==1 && signals==2);
   cmd[1].companion_rcs_cmd_buffer=&cmd[0];
   assert(run(&queue,2)==VK_ERROR_FEATURE_NOT_PRESENT);
   assert(chains==1 && submits==1 && signals==2);
   cmd[1].companion_rcs_cmd_buffer=NULL;
   wait_status=VK_TIMEOUT;
   assert(run(&queue,2)==VK_ERROR_DEVICE_LOST);
   assert(chains==1 && submits==1 && signals==2);
   wait_status=VK_SUCCESS;
   assert(run(&queue,2)==VK_ERROR_DEVICE_LOST);
   queue.device=&devices[1];
   for(unsigned i=0;i<2;i++)cmd[i].device=queue.device;
   submit_status=4;
   assert(run(&queue,2)==VK_ERROR_DEVICE_LOST);
   assert(submits==2 && signals==2);
   submit_status=0; queue.device=&devices[2];
   for(unsigned i=0;i<2;i++)cmd[i].device=queue.device;
   signal_status=VK_ERROR_OUT_OF_HOST_MEMORY;
   assert(run(&queue,2)==VK_ERROR_DEVICE_LOST);
   assert(submits==3 && signals==3);
   assert(run(&queue,2)==VK_ERROR_DEVICE_LOST);
   assert(submits==3 && signals==3);
   queue.device=&devices[3]; queue.sync=(struct vk_sync *)(uintptr_t)1;
   for(unsigned i=0;i<2;i++)cmd[i].device=queue.device;
   signal_status=VK_SUCCESS;
   assert(run(&queue,2)==VK_SUCCESS);
   assert(submits==4 && signals==5);
   assert(run(&queue,0)==VK_SUCCESS);
   assert(submits==4 && signals==7);
   anv_cubit_destroy_engine(&devices[3],&queue);
   assert(run(&queue,0)==VK_ERROR_DEVICE_LOST);
   assert(submits==4 && signals==7);
   puts("ANV render callback PASS: chain/flush/submit order, empty submit, unsupported companion, nonblocking dependencies, sticky errors (mock transport)");
}
