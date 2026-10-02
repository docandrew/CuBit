/* Actual Mesa adapter/types, mock transport. No hardware execution. */
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include <assert.h>
#include <pthread.h>
#include <sched.h>
#include <stdatomic.h>
#include <stdio.h>

static struct anv_device device;
static struct anv_bo bo = { .gem_handle = 17, .actual_size = 8192 };
static atomic_flag in_transport = ATOMIC_FLAG_INIT;
static pthread_barrier_t start;
static uint32_t epoch, marker = 1;
static unsigned updates, submits;
static bool fail_updates;

static void enter(void)
{
   assert(!atomic_flag_test_and_set(&in_transport));
   /* Make lock-free/too-short critical sections observable under contention. */
   for (unsigned i = 0; i < 20; i++) sched_yield();
}
static void leave(void) { atomic_flag_clear(&in_transport); }
uint32_t cubit_intel_prepare_context(uint64_t slot)
{ assert(slot == 63); return 0; }
uint32_t cubit_intel_register_context(uint64_t slot)
{ assert(slot == 63); return 0; }
uint32_t cubit_intel_update_binding(uint64_t slot, uint32_t handle,
   uint64_t gpu, uint64_t offset, uint64_t bytes, uint32_t remove,
   uint32_t previous, uint32_t *generation)
{
   enter();
   assert(slot == 63 && handle == 17 && gpu == 0x20000 &&
          offset == 4096 && bytes == 4096 && remove <= 1 && previous == epoch);
   updates++;
   *generation = fail_updates ? 0 : ++epoch;
   leave();
   return fail_updates ? 4 : 0;
}
uint32_t cubit_intel_submit_batch(uint64_t slot, uint32_t handle,
   uint64_t gpu, uint64_t offset, uint64_t bytes, uint32_t previous,
   uint32_t *completion)
{
   enter();
   assert(slot == 63 && handle == 17 && gpu == 0x20000 &&
          offset == 8 && bytes == 4096 && previous == marker);
   submits++;
   *completion = ++marker;
   leave();
   return 0;
}
VkResult _vk_device_set_lost(struct vk_device *d, const char *file,
                            int line, const char *message, ...)
{
   (void)d; (void)file; (void)line; (void)message;
   return VK_ERROR_DEVICE_LOST;
}
static void *worker(void *arg)
{
   (void)arg;
   int rc = pthread_barrier_wait(&start);
   assert(rc == 0 || rc == PTHREAD_BARRIER_SERIAL_THREAD);
   for (unsigned n = 0; n < 128; n++) {
      VkResult expected = fail_updates ? VK_ERROR_DEVICE_LOST : VK_SUCCESS;
      assert(anv_cubit_update_bo_binding(&device, &bo, 0x20000,
                                        4096, 4096, n & 1) == expected);
      assert(anv_cubit_submit_bo(&device, &bo, 0x20000, 8, 4096) == expected);
   }
   return NULL;
}
static void run(void)
{
   pthread_t threads[4];
   assert(pthread_barrier_init(&start, NULL, 4) == 0);
   for (unsigned i = 0; i < 4; i++)
      assert(pthread_create(&threads[i], NULL, worker, NULL) == 0);
   for (unsigned i = 0; i < 4; i++) assert(pthread_join(threads[i], NULL) == 0);
   assert(pthread_barrier_destroy(&start) == 0);
}
int main(void)
{
   assert(anv_cubit_memory_init(&device, 63) == VK_SUCCESS);
   assert(anv_cubit_prepare_submission(&device) == VK_SUCCESS);
   run();
   assert(updates == 512 && submits == 512 && epoch == 512 && marker == 513);
   /* One uncertain update poisons the shared lifetime before another thread
    * may submit or replay, even though the mocked transport remains callable. */
   fail_updates = true;
   run();
   assert(updates == 513 && submits == 512 && epoch == 512 && marker == 513);
   puts("ANV concurrency PASS: 4 threads, 1024 serialized operations, independent epochs, sticky shared failure; mocked IPC only");
   return 0;
}
