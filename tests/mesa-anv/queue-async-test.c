/* Actual callback/types; deterministic mocked IPC and synchronization. */
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include "../../userspace/mesa/anv/native_gpu_mapping.h"
#include <assert.h>
#include <stdio.h>
static unsigned phase, submissions, notifications;
static unsigned preparations, registrations;
static unsigned health_queries, drains, closes, retirements;
uint32_t cubit_intel_memory_contract(uint64_t slot)
{ assert(slot >= 53 && slot <= 60); return 1; }
uint32_t cubit_intel_session_status(uint64_t slot)
{ assert(slot >= 53 && slot <= 60); health_queries++; return 0; }
bool cubit_cpu_tracker_drain(struct cubit_cpu_mapping_tracker *tracker)
{ assert(tracker->slot >= 53 && tracker->slot <= 60); drains++; return true; }
uint32_t cubit_intel_close_session(uint64_t slot, uint64_t *tag)
{ assert(slot >= 53 && slot <= 60); closes++; *tag=slot+1; return 0; }
uint32_t cubit_intel_poll_session_retirement(uint64_t slot)
{ assert(slot >= 53 && slot <= 60); retirements++; return 0; }
static uint32_t marker[64], transport_status;
static VkResult wait_result = VK_SUCCESS, signal_result = VK_SUCCESS;
static unsigned flushes;
void util_flush_range(void *start, size_t size)
{ assert(start && size==4096 && phase==1); flushes++; }
VkResult _vk_device_set_lost(struct vk_device *d, const char *f, int l,
                            const char *m, ...)
{ (void)f; (void)l; (void)m; p_atomic_set(&d->_lost.lost, 1); return VK_ERROR_DEVICE_LOST; }
uint32_t cubit_intel_prepare_context(uint64_t s) { assert(marker[s]==0); preparations++; marker[s]=1; return s==56 ? 4 : 0; }
uint32_t cubit_intel_register_context(uint64_t s) { registrations++; return s==55 ? 4 : 0; }
uint32_t cubit_intel_submit_batch(uint64_t s, uint32_t h, uint64_t g,
   uint64_t o, uint64_t b, uint32_t previous, uint32_t *completed)
{
   assert(phase==1 && h==17 && g==0x20000 && o==0 && b==4096);
   assert(previous==marker[s]); phase=2; submissions++;
   *completed=++marker[s]; return transport_status;
}
VkResult vk_sync_wait_many(struct vk_device *d, uint32_t n,
   const struct vk_sync_wait *w, enum vk_sync_wait_flags f, uint64_t deadline)
{ (void)d; (void)w; assert(n==0 && phase==0 && f==0 && deadline==UINT64_MAX);
  phase=1; return wait_result; }
VkResult vk_sync_signal_many(struct vk_device *d, uint32_t n,
   const struct vk_sync_signal *s)
{
   (void)d;
   if (n==0) { assert(phase==2); phase=3; return VK_SUCCESS; }
   assert(n==1 && phase>=2 && phase<=4);
   assert(s->sync==(struct vk_sync *)(uintptr_t)(phase-1));
   phase++; notifications++; return signal_result;
}
int main(void)
{
   static struct anv_device d[8];
   static struct anv_physical_device physical;
   physical.memory.need_flush=true;
   physical.va.null_initialized_heap.addr=0x20000;
   physical.va.null_initialized_heap.size=UINT64_C(8)<<30;
   struct anv_vm_bind null_bind={.address=0x20000,.size=UINT64_C(8)<<30,.op=ANV_VM_BIND};
   struct anv_sparse_submission null_submit={.binds=&null_bind,.binds_len=1,.binds_capacity=1};
   static uint32_t storage[1024];
   physical.queue.family_count=1;
   physical.queue.families[0].engine_class=INTEL_ENGINE_CLASS_RENDER;
   float priority=0.5f;
   VkDeviceQueueCreateInfo qi={.sType=VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
      .queueCount=1,.pQueuePriorities=&priority};
   VkDeviceCreateInfo ci={.sType=VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
      .queueCreateInfoCount=1,.pQueueCreateInfos=&qi};
   struct anv_queue q={.device=&d[0],.family=&physical.queue.families[0],
                       .sync=(struct vk_sync *)(uintptr_t)3};
   struct anv_bo bo={.gem_handle=17,.offset=0x20000,.size=4096,
                     .actual_size=4096,.map=storage};
   struct anv_bo *batches[]={&bo};
   struct anv_async_submit submit={.queue=&q,.bo_pool=&d[0].batch_bo_pool,
      .signal={.sync=(struct vk_sync *)(uintptr_t)2}};
   /* Borrowed fixed array; no allocation or dynarray destruction in fixture. */
   submit.batch_bos.data=batches; submit.batch_bos.size=sizeof(batches);
   const struct vk_sync_signal output={.sync=(struct vk_sync *)(uintptr_t)1};
   for(unsigned i=0;i<8;i++) {
      d[i].physical=&physical;
      d[i].batch_bo_pool.bo_alloc_flags=ANV_BO_ALLOC_HOST_CACHED;
      assert(anv_cubit_attach_session(&d[i],60-i)==VK_SUCCESS);
      assert(anv_cubit_setup_context(&d[i],&ci,1)==VK_SUCCESS);
      q.device=&d[i];
      assert(anv_cubit_create_engine(&d[i],&q,&qi)==VK_SUCCESS);
   }
   q.device=&d[0];
   assert(health_queries==8 && !preparations && !registrations && !submissions);
   assert(anv_cubit_vm_bind(&d[0],&null_submit,ANV_VM_BIND_FLAG_SIGNAL_BIND_TIMELINE)==VK_SUCCESS);
   d[0].batch_bo_pool.bo_alloc_flags|=ANV_BO_ALLOC_NULL_INITIALIZED_HEAP;
   assert(anv_cubit_queue_exec_async(&submit,0,NULL,1,&output)==VK_SUCCESS);
   assert(phase==5 && submissions==1 && notifications==3 && flushes==1);
   assert(preparations==1 && registrations==1);
   phase=0; submit.use_companion_rcs=true;
   assert(anv_cubit_queue_exec_async(&submit,0,NULL,1,&output)==VK_ERROR_FEATURE_NOT_PRESENT);
   assert(phase==0); submit.use_companion_rcs=false;
   wait_result=VK_TIMEOUT;
   assert(anv_cubit_queue_exec_async(&submit,0,NULL,1,&output)==VK_TIMEOUT);
   assert(submissions==1 && notifications==3);
   phase=0; wait_result=VK_SUCCESS; transport_status=4;
   assert(anv_cubit_queue_exec_async(&submit,0,NULL,1,&output)==VK_ERROR_DEVICE_LOST);
   assert(submissions==2 && notifications==3);
   phase=0; transport_status=0; q.device=&d[1]; signal_result=VK_ERROR_OUT_OF_HOST_MEMORY;
   assert(anv_cubit_queue_exec_async(&submit,0,NULL,1,&output)==VK_ERROR_DEVICE_LOST);
   assert(submissions==3 && notifications==4);
   phase=0;
   assert(anv_cubit_queue_exec_async(&submit,0,NULL,1,&output)==VK_ERROR_DEVICE_LOST);
   assert(submissions==3 && notifications==4);
   /* Real device initialization supplies no caller signals or debug fence. */
   q.device=&d[2]; q.sync=NULL; signal_result=VK_SUCCESS; phase=0;
   assert(anv_cubit_queue_exec_async(&submit,0,NULL,0,NULL)==VK_SUCCESS);
   assert(phase==4 && submissions==4 && notifications==5);
   phase=0; submit.batch_bos.size=0;
   assert(anv_cubit_queue_exec_async(&submit,0,NULL,1,&output)==VK_ERROR_DEVICE_LOST);
   q.device=&d[3]; submit.batch_bos.size=sizeof(batches); bo.size=8192;
   assert(anv_cubit_queue_exec_async(&submit,0,NULL,1,&output)==VK_ERROR_DEVICE_LOST);
   assert(submissions==4 && notifications==5);
   assert(preparations==3 && registrations==3);
   bo.size=4096;
   for(unsigned i=4;i<6;i++) {
      q.device=&d[i]; phase=0;
      assert(anv_cubit_queue_exec_async(&submit,0,NULL,0,NULL)==VK_ERROR_DEVICE_LOST);
      assert(anv_cubit_queue_exec_async(&submit,0,NULL,0,NULL)==VK_ERROR_DEVICE_LOST);
      assert(phase==0 && submissions==4 && notifications==5);
   }
   assert(preparations==5 && registrations==4);
   /* Loss declared by Mesa, independently of the transport tracker, must
    * prevent even initial context preparation and all later submissions. */
   q.device=&d[6]; phase=0;
   p_atomic_set(&d[6].vk._lost.lost, 1);
   assert(anv_cubit_queue_exec_async(&submit,0,NULL,0,NULL)==VK_ERROR_DEVICE_LOST);
   assert(phase==0 && preparations==5 && registrations==4);
   assert(submissions==4 && notifications==5);
   q.device=&d[7]; phase=0;
   assert(anv_cubit_vm_bind(&d[7],&null_submit,ANV_VM_BIND_FLAG_SIGNAL_BIND_TIMELINE)==VK_SUCCESS);
   assert(anv_cubit_queue_exec_async(&submit,0,NULL,0,NULL)==VK_SUCCESS);
   assert(phase==4 && submissions==5 && notifications==6);
   null_bind.op=ANV_VM_UNBIND;
   assert(anv_cubit_vm_bind(&d[7],&null_submit,ANV_VM_BIND_FLAG_SIGNAL_BIND_TIMELINE)==VK_SUCCESS);
   phase=0;
   assert(anv_cubit_queue_exec_async(&submit,0,NULL,0,NULL)==VK_ERROR_DEVICE_LOST);
   assert(phase==0 && submissions==5 && notifications==6);
   for(unsigned i=0;i<8;i++) {
      q.device=&d[i];
      anv_cubit_destroy_engine(&d[i],&q);
      assert(anv_cubit_destroy_context(&d[i]));
      anv_cubit_close_device(&d[i]);
      assert(!d[i].cubit_cpu_mappings);
   }
   assert(drains==8 && closes==8 && retirements==8);
   assert(anv_cubit_memory_poll()==0 && health_queries==8);
   puts("ANV startup callback PASS: attach/health/context/engine, completion-before-signals, null heap and session teardown, sticky failures (mock transport)");
}
