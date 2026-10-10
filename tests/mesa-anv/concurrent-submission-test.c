/* Lock independence of the session queue (GPU-001 step 3), actual ANV types,
 * real vk_sync, mock queue and IPC. The driver defers a VM update or buffer
 * request while GPU work is in flight; here the mock holds one of each
 * inside the "driver" (the buffer request holding lifetime_mutex, as it
 * does across its IPC) while submission, completion, fence waits and health
 * checks on the same device carry on. Then four devices submit from four
 * threads while a GPU thread completes and VM updates run beside them. */
#include "queue-rig.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include "../../userspace/mesa/anv/native_gpu_mapping.h"
#include <pthread.h>
#include <stdatomic.h>
#include <stdio.h>

uint32_t cubit_intel_memory_contract(uint64_t slot) { (void)slot; return 1; }
uint32_t cubit_intel_session_status(uint64_t slot) { (void)slot; return 0; }
bool cubit_cpu_tracker_drain(struct cubit_cpu_mapping_tracker *tracker)
{ (void)tracker; return true; }
uint32_t cubit_intel_close_session(uint64_t slot, uint64_t *tag) { *tag = slot + 1; return 0; }
uint32_t cubit_intel_poll_session_retirement(uint64_t slot) { (void)slot; return 0; }
uint32_t cubit_intel_prepare_context(uint64_t slot) { (void)slot; return 0; }
uint32_t cubit_intel_register_context(uint64_t slot) { (void)slot; return 0; }
void anv_cmd_buffer_chain_command_buffers(struct anv_cmd_buffer **c, uint32_t n) { (void)c; (void)n; }
void anv_cmd_buffer_clflush(struct anv_cmd_buffer **c, uint32_t n) { (void)c; (void)n; }
void util_flush_range(void *start, size_t size) { (void)start; (void)size; }
uint32_t cubit_cpu_mapping_release(struct cubit_cpu_mapping *record, bool replace)
{ (void)record; (void)replace; return 0; }
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
   int line, const char *message, ...)
{
   (void)file; (void)line; (void)message;
   p_atomic_set(&device->_lost.lost, 1);
   return VK_ERROR_DEVICE_LOST;
}

/* The "driver" holds requests here until released. */
static pthread_mutex_t gate = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t gate_changed = PTHREAD_COND_INITIALIZER;
static bool hold, update_inside, close_inside;
static atomic_uint generations[64], updates;
static void held(bool *inside)
{
   pthread_mutex_lock(&gate);
   *inside = true;
   pthread_cond_broadcast(&gate_changed);
   while (hold) pthread_cond_wait(&gate_changed, &gate);
   pthread_mutex_unlock(&gate);
}
uint32_t cubit_intel_update_binding(uint64_t slot, uint32_t handle, uint64_t gpu,
   uint64_t offset, uint64_t bytes, uint32_t remove, uint32_t previous, uint32_t *generation)
{
   (void)handle; (void)gpu; (void)offset; (void)bytes; (void)remove;
   if (hold) held(&update_inside);
   assert(previous == atomic_load(&generations[slot]));
   *generation = atomic_fetch_add(&generations[slot], 1) + 1;
   atomic_fetch_add(&updates, 1);
   return 0;
}
uint32_t cubit_intel_close_buffer(uint64_t slot, uint32_t handle)
{
   (void)slot; (void)handle;
   if (hold) held(&close_inside);
   return 0;
}

static struct rig rigs[4];
static struct anv_bo vm_bo = {.gem_handle = 21, .actual_size = 8192};
static struct anv_bo closed_bo = {.gem_handle = 22, .actual_size = 4096};
static VkResult update_result, close_ran;
static void *update_thread(void *argument)
{
   update_result = anv_cubit_update_bo_binding(argument, &vm_bo, 0x80000, 0, 4096, false);
   return NULL;
}
static void *close_thread(void *argument)
{
   anv_cubit_gem_close(argument, &closed_bo);
   close_ran = VK_SUCCESS;
   return NULL;
}
static void wait_inside(bool *inside)
{
   pthread_mutex_lock(&gate);
   while (!*inside) pthread_cond_wait(&gate_changed, &gate);
   pthread_mutex_unlock(&gate);
}

#define JOBS_PER_THREAD 2000u
#define UPDATES_PER_RIG 200u
static struct vk_sync *timelines[4];
static atomic_bool submitting = true;
static void *submitter(void *argument)
{
   struct rig *r = argument;
   const unsigned index = (unsigned)(r - rigs);
   for (unsigned n = 1; n <= JOBS_PER_THREAD; n++) {
      const struct vk_sync_signal s = {.sync = timelines[index], .signal_value = n};
      assert(anv_cubit_queue_exec_locked(&r->queue, 0, NULL, 1, r->cmds, 1, &s,
                                         NULL, 0, NULL) == VK_SUCCESS);
   }
   return NULL;
}
static void *updater(void *argument)
{
   struct rig *r = argument;
   for (unsigned n = 0; n < UPDATES_PER_RIG; n++)
      assert(anv_cubit_update_bo_binding(&r->device, &vm_bo, 0x80000, 0, 4096, n & 1) == VK_SUCCESS);
   return NULL;
}
static void *gpu(void *argument)
{
   (void)argument;
   while (atomic_load(&submitting))
      for (unsigned i = 0; i < 4; i++) {
         struct native_gpu_values completed, submitted;
         assert(cubit_gpu_queue_observe(rigs[i].slot, &completed, &submitted) == CUBIT_GPU_QUEUE_OK);
         gpu_queue_mock_complete(rigs[i].slot, 0, submitted.value[0]);
      }
   return NULL;
}

int main(void)
{
   const struct vk_sync_type *const *types = rig_physical();
   unsigned checks = 0;
   for (unsigned i = 0; i < 4; i++) {
      open_rig(&rigs[i], 40 + i);
      struct vk_sync *fence = make(&rigs[i], &binary_type.sync, 0);
      assert(startup(&rigs[i], fence, 0, NULL) == VK_SUCCESS);
      timelines[i] = make(&rigs[i], types[0], VK_SYNC_IS_TIMELINE);
   }

   /* A VM update and a buffer close held inside the driver. */
   struct rig *r = &rigs[0];
   hold = true;
   pthread_t updating, closing;
   assert(!pthread_create(&updating, NULL, update_thread, &r->device));
   assert(!pthread_create(&closing, NULL, close_thread, &r->device));
   wait_inside(&update_inside);
   wait_inside(&close_inside);
   struct vk_sync *fence = make(r, &binary_type.sync, 0);
   for (unsigned frame = 0; frame < 1000; frame++) {
      assert(vk_sync_reset(&r->device.vk, fence) == VK_SUCCESS);
      const struct vk_sync_signal s = {.sync = fence};
      assert(anv_cubit_queue_exec_locked(&r->queue, 0, NULL, 1, r->cmds, 1, &s,
                                         NULL, 0, NULL) == VK_SUCCESS);
      assert(anv_cubit_check_status(&r->device.vk) == VK_SUCCESS);
      gpu_queue_mock_complete(r->slot, 0, gpu_queue_mock[r->slot].submitted.value[0]);
      assert(vk_sync_wait(&r->device.vk, fence, 0, 0, UINT64_MAX) == VK_SUCCESS);
      checks += 3;
   }
   assert(atomic_load(&updates) == 0);
   pthread_mutex_lock(&gate);
   hold = false;
   pthread_cond_broadcast(&gate_changed);
   pthread_mutex_unlock(&gate);
   assert(!pthread_join(updating, NULL) && !pthread_join(closing, NULL));
   assert(update_result == VK_SUCCESS && close_ran == VK_SUCCESS && atomic_load(&updates) == 1);
   checks += 2;

   /* Four submitters, four VM updaters and a GPU, all at once. */
   pthread_t submitters[4], updaters[4], completer;
   assert(!pthread_create(&completer, NULL, gpu, NULL));
   for (unsigned i = 0; i < 4; i++) {
      assert(!pthread_create(&submitters[i], NULL, submitter, &rigs[i]));
      assert(!pthread_create(&updaters[i], NULL, updater, &rigs[i]));
   }
   for (unsigned i = 0; i < 4; i++)
      assert(!pthread_join(submitters[i], NULL) && !pthread_join(updaters[i], NULL));
   for (unsigned i = 0; i < 4; i++) {
      assert(vk_sync_wait(&rigs[i].device.vk, timelines[i], JOBS_PER_THREAD, 0, UINT64_MAX) == VK_SUCCESS);
      checks++;
   }
   atomic_store(&submitting, false);
   assert(!pthread_join(completer, NULL));
   assert(atomic_load(&updates) == 1 + 4 * UPDATES_PER_RIG);
   for (unsigned i = 0; i < 4; i++) close_rig(&rigs[i]);
   assert(anv_cubit_memory_poll() == 0);
   printf("ANV queue concurrency PASS: %u checks; 1000 frames submitted, completed and waited "
          "while a VM update and a buffer close (holding lifetime_mutex) sat in the driver; "
          "4 x %u jobs from 4 threads beside 4 x %u VM updates (mock queue/IPC)\n",
          checks + 1, JOBS_PER_THREAD, UPDATES_PER_RIG);
   return 0;
}
