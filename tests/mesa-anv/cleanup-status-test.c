#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_mapping.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static unsigned queries;
static unsigned captures;
static uint32_t response;
static uint32_t close_response = 3;
static unsigned closes, releases;
static const char *expected_operation;
static const char *operations[5];
void cubit_test_mesa_transport_failure(const char *, uint32_t, uint32_t);
void cubit_test_mesa_transport_failure(const char *operation, uint32_t status,
                                      uint32_t handle)
{
   assert(status==3);
   assert((!strcmp(operation,"session-health") && handle==0) ||
          (!strcmp(operation,"close-buffer") && handle==42) ||
          (!strcmp(operation,"close-cpu-view") && handle==42));
   if (expected_operation) assert(!strcmp(operation, expected_operation));
   assert(captures < sizeof(operations)/sizeof(operations[0]));
   operations[captures] = operation;
   captures++;
}
uint32_t cubit_intel_session_status(uint64_t slot)
{ assert(slot==60 || slot==59); queries++; return response; }
uint32_t cubit_intel_close_buffer(uint64_t slot, uint32_t handle)
{ assert((slot==58 || slot==57 || slot==56) && handle==42); closes++; return close_response; }
uint32_t cubit_cpu_mapping_release(struct cubit_cpu_mapping *record, bool replace)
{
   assert(record->bo_handle==42 && !replace);
   assert(record->state==CUBIT_MAP_LIVE);
   record->state=CUBIT_MAP_FAILED;
   releases++;
   return 3;
}
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
                            int line, const char *message, ...)
{
   (void)file; (void)line; (void)message;
   p_atomic_set(&device->_lost.lost, 1);
   return VK_ERROR_DEVICE_LOST;
}
int main(void)
{
   static struct anv_device device;
   assert(anv_cubit_memory_init(&device,60)==VK_SUCCESS);
   assert(anv_cubit_check_status(&device.vk)==VK_SUCCESS && queries==1);
   /* Inject the sticky state set by unsuccessful mapping/BO cleanup.
    * No GPU/transport failure is required to produce the next health=-4. */
   device.cubit_cpu_mappings->lost=true;
   assert(anv_cubit_check_status(&device.vk)==VK_ERROR_DEVICE_LOST);
   assert(queries==1);
   assert(captures==0); /* Local sticky loss is not a native health failure. */
   static struct anv_device remote;
   assert(anv_cubit_memory_init(&remote,59)==VK_SUCCESS);
   response=3;
   assert(anv_cubit_check_status(&remote.vk)==VK_ERROR_DEVICE_LOST);
   assert(queries==2 && captures==1);
   assert(anv_cubit_check_status(&remote.vk)==VK_ERROR_DEVICE_LOST);
   assert(queries==2 && captures==1);
   assert(vk_device_is_lost_no_report(&device.vk));
   assert(anv_cubit_check_status(&device.vk)==VK_ERROR_DEVICE_LOST);
   assert(queries==2 && captures==1);
   puts("Cleanup status PASS: local loss becomes device loss without a native health query; no retry (mock state)");
   static struct anv_device closing;
   struct anv_bo bo={.gem_handle=42};
   assert(anv_cubit_memory_init(&closing,58)==VK_SUCCESS);
   anv_cubit_gem_close(&closing,&bo);
   assert(captures==2 && queries==2);
   assert(vk_device_is_lost_no_report(&closing.vk));
   assert(anv_cubit_check_status(&closing.vk)==VK_ERROR_DEVICE_LOST);
   assert(captures==2 && queries==2);
   puts("BO close PASS: void cleanup captures original status then next health fails locally");
   /* CPU retirement failure must poison the device even if retiring the BO
    * name succeeds. The retained mapping is not reclaimed by name close. */
   static struct anv_device mapped;
   assert(anv_cubit_memory_init(&mapped,57)==VK_SUCCESS);
   struct cubit_cpu_mapping_tracker *tracker=mapped.cubit_cpu_mappings;
   tracker->used=1;
   struct cubit_cpu_mapping *record=cubit_cpu_tracker_records(tracker);
   *record=(struct cubit_cpu_mapping){.state=CUBIT_MAP_LIVE,.bo_handle=42};
   close_response=0;
   expected_operation="close-cpu-view";
   anv_cubit_gem_close(&mapped,&bo);
   assert(releases==1 && closes==2 && captures==3);
   assert(record->state==CUBIT_MAP_FAILED && tracker->used==1 && tracker->lost);
   assert(anv_cubit_check_status(&mapped.vk)==VK_ERROR_DEVICE_LOST);
   assert(queries==2);
   /* Both callbacks fire in causal order; the bounded first-failure recorder
    * (tested separately) must retain CPU release, not the later name failure. */
   static struct anv_device both;
   assert(anv_cubit_memory_init(&both,56)==VK_SUCCESS);
   tracker=both.cubit_cpu_mappings;
   tracker->used=1;
   record=cubit_cpu_tracker_records(tracker);
   *record=(struct cubit_cpu_mapping){.state=CUBIT_MAP_LIVE,.bo_handle=42};
   close_response=3;
   expected_operation=NULL;
   anv_cubit_gem_close(&both,&bo);
   assert(releases==2 && closes==3 && captures==5);
   assert(!strcmp(operations[3],"close-cpu-view"));
   assert(!strcmp(operations[4],"close-buffer"));
   assert(record->state==CUBIT_MAP_FAILED && tracker->lost);
   assert(anv_cubit_check_status(&both.vk)==VK_ERROR_DEVICE_LOST && queries==2);
   puts("CPU close PASS: failed views retained despite name close; health sticky without native query");
}
