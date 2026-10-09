/* Owned-session attachment, not a broker/delegation or native IPC test. */
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include "../../userspace/mesa/anv/native_gpu_mapping.h"
#include <assert.h>
#include <stdio.h>
static unsigned queries, drains, closes, polls;
static bool fail_directory, fail_record;
void *__real_realloc(void *, size_t);
void *__real_calloc(size_t, size_t);
void *__wrap_realloc(void *, size_t);
void *__wrap_calloc(size_t, size_t);
void *__wrap_realloc(void *ptr, size_t bytes)
{ return fail_directory ? NULL : __real_realloc(ptr, bytes); }
void *__wrap_calloc(size_t count, size_t bytes)
{ return fail_record ? NULL : __real_calloc(count, bytes); }
static uint32_t health, retirement=4, close_result;
static uint32_t policy=1;
static unsigned memory_queries, creates;
static void retired(void *context)
{ p_atomic_inc((unsigned *)context); }
uint32_t cubit_intel_memory_contract(uint64_t slot)
{ assert(slot <= 63); memory_queries++; return policy; }
uint32_t cubit_intel_create_buffer(uint64_t slot, uint64_t bytes, uint32_t *handle)
{ assert(slot <= 63 && bytes==4096); creates++; *handle=17; return 0; }
uint32_t cubit_intel_session_status(uint64_t slot)
{ assert(slot <= 63); queries++; return health; }
bool cubit_cpu_tracker_drain(struct cubit_cpu_mapping_tracker *tracker)
{ assert(tracker->slot <= 63); drains++; return true; }
uint32_t cubit_intel_close_session(uint64_t slot, uint64_t *tag)
{ assert(slot <= 63); closes++; *tag = close_result ? 0 : 1; return close_result; }
uint32_t cubit_intel_poll_session_retirement(uint64_t slot)
{ assert(slot <= 63); polls++; return retirement; }
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
                             int line, const char *message, ...)
{ (void)file; (void)line; (void)message;
  p_atomic_set(&device->_lost.lost, 1); return VK_ERROR_DEVICE_LOST; }
int main(void)
{
   static struct anv_physical_device physical;
   static struct anv_device d[10];
   /* Native address binding never enables Linux relocations or any sparse
    * emulation mode, even when the incoming structure requests one. */
   for (unsigned sparse = ANV_SPARSE_TYPE_NOT_SUPPORTED;
        sparse <= ANV_SPARSE_TYPE_FAKE; sparse++) {
      physical.uses_relocs = true;
      physical.sparse_type = sparse;
      anv_cubit_init_addressing(&physical);
      assert(!physical.uses_relocs &&
             physical.sparse_type == ANV_SPARSE_TYPE_NOT_SUPPORTED);
   }
   physical.memory.need_flush=true;
   for(unsigned i=0;i<10;i++) d[i].physical=&physical;
   /* Host allocation failure precedes authority transfer. The provider still
    * owns exactly the original pin and no driver request may have occurred. */
   unsigned failed_notifications=0;
   struct anv_cubit_endpoint_pin failed_pin={&failed_notifications,retired};
   for(unsigned failure=0;failure<2;failure++) {
      fail_directory=failure==0;
      fail_record=failure==1;
      assert(anv_cubit_attach_owned_session(&d[0],63,&failed_pin)==
             VK_ERROR_OUT_OF_HOST_MEMORY);
      assert(failed_pin.context==&failed_notifications && failed_pin.retired==retired);
      assert(!failed_notifications && !d[0].cubit_cpu_mappings);
      assert(!anv_cubit_memory_slot_retained(63));
      assert(!queries && !memory_queries && !drains && !closes && !polls);
   }
   fail_directory=false; fail_record=false;
   assert(anv_cubit_attach_session(NULL,63)==VK_ERROR_INITIALIZATION_FAILED);
   assert(anv_cubit_attach_session(&d[0],64)==VK_ERROR_INITIALIZATION_FAILED);
   assert(!queries && !closes);
   assert(anv_cubit_attach_session(&d[0],63)==VK_SUCCESS);
   assert(queries==1 && d[0].cubit_cpu_mappings && !closes);
   /* Duplicate or aliased attachment never closes the original device. */
   assert(anv_cubit_attach_session(&d[0],63)==VK_ERROR_INITIALIZATION_FAILED);
   assert(anv_cubit_attach_session(&d[1],63)==VK_ERROR_INITIALIZATION_FAILED);
   assert(queries==1 && !closes);
   health=3;
   assert(anv_cubit_attach_session(&d[1],62)==VK_ERROR_DEVICE_LOST);
   assert(!d[1].cubit_cpu_mappings && queries==2 && drains==1 && closes==1 && polls==1);
   assert(anv_cubit_memory_slot_retained(62));
   health=0;
   assert(anv_cubit_attach_session(&d[2],62)==VK_ERROR_INITIALIZATION_FAILED);
   assert(closes==1 && queries==2);
   retirement=0;
   assert(anv_cubit_memory_poll()==0);
   assert(!anv_cubit_memory_slot_retained(62));
   /* Uncertain close remains quarantined and is never replayed. */
   health=4; close_result=4;
   assert(anv_cubit_attach_session(&d[3],61)==VK_ERROR_DEVICE_LOST);
   assert(!d[3].cubit_cpu_mappings && closes==2);
   assert(anv_cubit_memory_poll()==1);
   assert(anv_cubit_memory_poll()==1 && closes==2);
   assert(anv_cubit_memory_slot_retained(61));
   assert(anv_cubit_attach_session(&d[4],61)==VK_ERROR_INITIALIZATION_FAILED);
   assert(closes==2 && queries==3);
   assert(memory_queries==1); /* No memory query after failed health. */
   health=0; close_result=0;
   policy=0;
   assert(anv_cubit_attach_session(&d[5],60)==VK_ERROR_INITIALIZATION_FAILED);
   assert(!d[5].cubit_cpu_mappings && !anv_cubit_memory_slot_retained(60));
   policy=3;
   assert(anv_cubit_attach_session(&d[6],59)==VK_ERROR_INITIALIZATION_FAILED);
   physical.memory.need_flush=false; policy=1;
   assert(anv_cubit_attach_session(&d[7],58)==VK_ERROR_INITIALIZATION_FAILED);
   policy=2;
   assert(anv_cubit_attach_session(&d[8],57)==VK_SUCCESS);
   const struct intel_memory_class_instance region={0};
   const struct intel_memory_class_instance *regions[]={&region};
   physical.sys.region=&region;
   uint64_t bytes=99;
   assert(anv_cubit_gem_create(&d[8],regions,1,4096,
      ANV_BO_ALLOC_HOST_CACHED|ANV_BO_ALLOC_HOST_COHERENT,&bytes)==17);
   assert(bytes==4096 && creates==1);
   /* A coherent session must not change an earlier explicit-only session. */
   assert(!anv_cubit_gem_create(&d[0],regions,1,4096,
      ANV_BO_ALLOC_HOST_CACHED|ANV_BO_ALLOC_HOST_COHERENT,&bytes));
   assert(!bytes && creates==1);
   assert(anv_cubit_gem_create(&d[8],regions,1,4096,
      ANV_BO_ALLOC_HOST_COHERENT|ANV_BO_ALLOC_MAPPED|ANV_BO_ALLOC_INTERNAL|
      ANV_BO_ALLOC_CAPTURE,&bytes)==17); /* Actual Mesa workaround flags. */
   assert(bytes==4096 && creates==2);
   assert(anv_cubit_gem_create(&d[8],regions,1,4096,0,&bytes)==17);
   assert(bytes==4096 && creates==3); /* No CPU cache-mode request. */
   physical.memory.need_flush=true;
   assert(!anv_cubit_gem_create(&d[0],regions,1,4096,0,&bytes));
   assert(!bytes && creates==3);
   /* Common ANV selects these GPU VA heaps before native bind. Neither
    * intent authorizes a new backing region or bypasses coherence admission. */
   const enum anv_bo_alloc_flags pool_flags[] = {
      ANV_BO_ALLOC_DESCRIPTOR_POOL_FLAGS,
      ANV_BO_ALLOC_DYNAMIC_VISIBLE_POOL_FLAGS,
   };
   for (unsigned i=0; i<2; i++) {
      for (unsigned slab=0; slab<2; slab++) {
         enum anv_bo_alloc_flags flags=pool_flags[i] |
            (slab ? ANV_BO_ALLOC_SLAB_PARENT : 0);
         unsigned before=creates;
         assert(anv_cubit_gem_create(&d[8],regions,1,4096,flags,&bytes)==17);
         assert(bytes==4096 && creates==before+1);
         before=creates;
         assert(!anv_cubit_gem_create(&d[0],regions,1,4096,flags,&bytes));
         assert(!bytes && creates==before);
         assert(!anv_cubit_gem_create(&d[8],regions,1,4096,
            flags | ANV_BO_ALLOC_COMPRESSED,&bytes));
         assert(!bytes && creates==before);
      }
   }
   /* Device-address visibility selects common ANV's VA reservation policy,
    * not external sharing. Test both ordinary and slab-parent allocation. */
   for (unsigned slab=0; slab<2; slab++) {
      enum anv_bo_alloc_flags flags=ANV_BO_ALLOC_CLIENT_VISIBLE_ADDRESS |
         ANV_BO_ALLOC_NO_LOCAL_MEM | ANV_BO_ALLOC_HOST_CACHED_COHERENT |
         (slab ? ANV_BO_ALLOC_SLAB_PARENT : 0);
      unsigned before=creates;
      assert(anv_cubit_gem_create(&d[8],regions,1,4096,flags,&bytes)==17);
      assert(bytes==4096 && creates==before+1);
      before=creates;
      assert(!anv_cubit_gem_create(&d[0],regions,1,4096,flags,&bytes));
      assert(!bytes && creates==before);
      assert(!anv_cubit_gem_create(&d[8],regions,1,4096,
         flags | ANV_BO_ALLOC_EXTERNAL,&bytes));
      assert(!bytes && creates==before);
      assert(!anv_cubit_gem_create(&d[8],regions,1,4096,
         flags | ANV_BO_ALLOC_IMPORTED,&bytes));
      assert(!bytes && creates==before);
   }
   /* Pre-12.5 scratch buffers select the low VA heap in common ANV. The
    * address-width intent must not be confused with physical placement. */
   for (unsigned slab=0; slab<2; slab++) {
      enum anv_bo_alloc_flags flags=ANV_BO_ALLOC_32BIT_ADDRESS |
         ANV_BO_ALLOC_INTERNAL | (slab ? ANV_BO_ALLOC_SLAB_PARENT : 0);
      unsigned before=creates;
      assert(anv_cubit_gem_create(&d[8],regions,1,4096,flags,&bytes)==17);
      assert(bytes==4096 && creates==before+1);
      before=creates;
      assert(!anv_cubit_gem_create(&d[8],regions,1,4096,
         flags | ANV_BO_ALLOC_PROTECTED,&bytes));
      assert(!bytes && creates==before);
   }
   /* Exhaust the flag word for a coherent system-memory session without
    * AUX/null-heap capabilities. New or unsupported intents must not silently
    * acquire backing. Pool flags are covered above in actual Mesa composites. */
   const unsigned ordinary_flags = ANV_BO_ALLOC_MAPPED |
      ANV_BO_ALLOC_HOST_COHERENT | ANV_BO_ALLOC_CAPTURE |
      ANV_BO_ALLOC_FIXED_ADDRESS | ANV_BO_ALLOC_NO_LOCAL_MEM |
      ANV_BO_ALLOC_DESCRIPTOR_POOL | ANV_BO_ALLOC_HOST_CACHED |
      ANV_BO_ALLOC_DYNAMIC_VISIBLE_POOL | ANV_BO_ALLOC_INTERNAL |
      ANV_BO_ALLOC_SLAB_PARENT | ANV_BO_ALLOC_CLIENT_VISIBLE_ADDRESS |
      ANV_BO_ALLOC_32BIT_ADDRESS;
   for (unsigned bit=0; bit<32; bit++) {
      unsigned intent=UINT32_C(1)<<bit;
      unsigned before=creates;
      uint32_t handle=anv_cubit_gem_create(&d[8],regions,1,4096,
         (enum anv_bo_alloc_flags)(ANV_BO_ALLOC_HOST_CACHED | intent),&bytes);
      if (ordinary_flags & intent) {
         assert(handle==17 && bytes==4096 && creates==before+1);
      } else {
         assert(!handle && !bytes && creates==before);
      }
   }
   /* Provider pins transfer with tracker attachment, not with VK_SUCCESS.
    * Callback context is process-owned, not stored in the Vulkan wrapper. */
   unsigned notifications=0;
   struct anv_cubit_endpoint_pin pin={&notifications,retired};
   struct anv_device owned={.physical=&physical};
   assert(anv_cubit_attach_owned_session(&owned,56,NULL)==VK_ERROR_INITIALIZATION_FAILED);
   assert(anv_cubit_attach_owned_session(NULL,56,&pin)==VK_ERROR_INITIALIZATION_FAILED);
   assert(pin.context==&notifications && pin.retired==retired);
   assert(anv_cubit_attach_owned_session(&owned,63,&pin)==VK_ERROR_INITIALIZATION_FAILED);
   assert(pin.context==&notifications && pin.retired==retired && !notifications);
   retirement=4;
   assert(anv_cubit_attach_owned_session(&owned,56,&pin)==VK_SUCCESS);
   assert(!pin.context && !pin.retired && !notifications);
   assert(anv_cubit_memory_finish(&owned)==VK_ERROR_DEVICE_LOST);
   assert(!notifications && !owned.cubit_cpu_mappings);
   /* Lose every byte of the old wrapper before process cleanup. */
   memset(&owned,0,sizeof(owned));
   assert(anv_cubit_memory_poll()==2 && !notifications);
   retirement=0;
   assert(anv_cubit_memory_poll()==1 && notifications==1);
   assert(anv_cubit_memory_poll()==1 && notifications==1);
   /* Immediate failed health check still consumes the pin and confirms
    * retirement before notifying; caller must not perform a second release. */
   owned=(struct anv_device){.physical=&physical};
   pin=(struct anv_cubit_endpoint_pin){&notifications,retired};
   health=3;
   assert(anv_cubit_attach_owned_session(&owned,56,&pin)==VK_ERROR_DEVICE_LOST);
   assert(!pin.retired && !pin.context && notifications==2);
   assert(anv_cubit_memory_poll()==1 && notifications==2);
   /* Reuse a completed bookkeeping record; each pin gets one notification. */
   health=0;
   for(unsigned n=0;n<128;n++) {
      owned=(struct anv_device){.physical=&physical};
      pin=(struct anv_cubit_endpoint_pin){&notifications,retired};
      assert(anv_cubit_attach_owned_session(&owned,56,&pin)==VK_SUCCESS);
      assert(!pin.retired);
      assert(anv_cubit_memory_finish(&owned)==VK_SUCCESS);
      assert(notifications==n+3);
   }
   /* Uncertain close never notifies, even after unrelated retirement succeeds. */
   owned=(struct anv_device){.physical=&physical};
   pin=(struct anv_cubit_endpoint_pin){&notifications,retired};
   close_result=4; health=3;
   assert(anv_cubit_attach_owned_session(&owned,56,&pin)==VK_ERROR_DEVICE_LOST);
   assert(!pin.retired && notifications==130);
   assert(anv_cubit_memory_poll()==2 && notifications==130);
   assert(anv_cubit_memory_poll()==2 && notifications==130);
   puts("ANV session attach PASS: allocation-failure pin ownership, health-checked transfer, alias denial, retained failure cleanup (mock IPC)");
}
