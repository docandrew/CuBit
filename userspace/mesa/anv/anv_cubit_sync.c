/* Mesa-facing synchronization shim; GPU completion remains owned by Ada. */
#include "anv_cubit_sync.h"
#include "vk_device.h"
#include <pthread.h>
#include <errno.h>
#include <time.h>

struct cubit_sync { struct vk_sync base; uint64_t value; };
/* One lock/condition lets wait-many examine all values atomically and avoids
 * missing a signal between checking separate objects. No cross-process state.
 * Vulkan callers must retain object lifetimes until waiters have returned. */
static pthread_mutex_t sync_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t sync_changed;
static pthread_once_t sync_once = PTHREAD_ONCE_INIT;
static bool sync_ready;
static void initialize_condition(void)
{
   pthread_condattr_t attr;
   if (pthread_condattr_init(&attr)) return;
   if (!pthread_condattr_setclock(&attr, CLOCK_MONOTONIC))
      sync_ready = pthread_cond_init(&sync_changed, &attr) == 0;
   pthread_condattr_destroy(&attr);
}
VkResult anv_cubit_sync_prepare(void)
{
   return pthread_once(&sync_once, initialize_condition) || !sync_ready ?
      VK_ERROR_INITIALIZATION_FAILED : VK_SUCCESS;
}
static VkResult initialize(struct vk_device *device, struct vk_sync *sync, uint64_t initial)
{
   (void)device;
   if (sync->flags != VK_SYNC_IS_TIMELINE)
      return VK_ERROR_FEATURE_NOT_PRESENT;
   VkResult result = anv_cubit_sync_prepare();
   if (result != VK_SUCCESS) return result;
   ((struct cubit_sync *)sync)->value = initial;
   return VK_SUCCESS;
}
static void finish(struct vk_device *device, struct vk_sync *sync)
{ (void)device; (void)sync; }
static VkResult signal_value(struct vk_device *device, struct vk_sync *sync, uint64_t value)
{
   if (vk_device_is_lost_no_report(device)) return VK_ERROR_DEVICE_LOST;
   pthread_mutex_lock(&sync_mutex);
   struct cubit_sync *object = (struct cubit_sync *)sync;
   VkResult result = value < object->value ? VK_ERROR_UNKNOWN : VK_SUCCESS;
   if (result == VK_SUCCESS) {
      object->value = value;
      if (pthread_cond_broadcast(&sync_changed)) result = VK_ERROR_UNKNOWN;
   }
   pthread_mutex_unlock(&sync_mutex);
   return result;
}
static VkResult get_value(struct vk_device *device, struct vk_sync *sync, uint64_t *value)
{
   if (vk_device_is_lost_no_report(device)) return VK_ERROR_DEVICE_LOST;
   pthread_mutex_lock(&sync_mutex);
   *value = ((struct cubit_sync *)sync)->value;
   pthread_mutex_unlock(&sync_mutex);
   return VK_SUCCESS;
}
static VkResult wait_values(struct vk_device *device, uint32_t count,
   const struct vk_sync_wait *waits, enum vk_sync_wait_flags flags, uint64_t deadline)
{
   if ((flags & ~(VK_SYNC_WAIT_ANY | VK_SYNC_WAIT_PENDING)) || (count && !waits))
      return VK_ERROR_FEATURE_NOT_PRESENT;
   for (uint32_t i = 0; i < count; i++)
      if (!waits[i].sync || waits[i].sync->type != &anv_cubit_cpu_timeline_type)
         return VK_ERROR_FEATURE_NOT_PRESENT;
   pthread_mutex_lock(&sync_mutex);
   VkResult result = VK_SUCCESS;
   for (;;) {
      if (vk_device_is_lost_no_report(device)) { result = VK_ERROR_DEVICE_LOST; break; }
      uint32_t satisfied = 0;
      for (uint32_t i = 0; i < count; i++)
         satisfied += ((struct cubit_sync *)waits[i].sync)->value >= waits[i].wait_value;
      if (satisfied == count || ((flags & VK_SYNC_WAIT_ANY) && satisfied)) break;
      struct timespec now;
      if (clock_gettime(CLOCK_MONOTONIC, &now)) { result = VK_ERROR_UNKNOWN; break; }
      uint64_t ns = (uint64_t)now.tv_sec * UINT64_C(1000000000) + now.tv_nsec;
      if (ns >= deadline) { result = VK_TIMEOUT; break; }
      /* Bounded sleep also observes device loss even if no sync is signaled.
       * The absolute Vulkan deadline is monotonic, never realtime. */
      uint64_t until = deadline - ns > UINT64_C(10000000) ? ns + UINT64_C(10000000) : deadline;
      struct timespec timeout = { .tv_sec = until / UINT64_C(1000000000),
                                 .tv_nsec = until % UINT64_C(1000000000) };
      int error = pthread_cond_timedwait(&sync_changed, &sync_mutex, &timeout);
      if (error && error != ETIMEDOUT) { result = VK_ERROR_UNKNOWN; break; }
      /* Always recheck values and clock, including spurious wakeups. */
   }
   pthread_mutex_unlock(&sync_mutex);
   return result;
}
const struct vk_sync_type anv_cubit_cpu_timeline_type = {
   .size = sizeof(struct cubit_sync),
   /* GPU_WAIT denotes the queue worker's CPU dependency wait, not a hardware
    * semaphore. Pending is conservatively completion: this synchronous backend
    * never exposes an earlier submitted-but-incomplete timeline value. */
   .features = VK_SYNC_FEATURE_TIMELINE | VK_SYNC_FEATURE_CPU_WAIT |
               VK_SYNC_FEATURE_CPU_SIGNAL | VK_SYNC_FEATURE_WAIT_ANY |
               VK_SYNC_FEATURE_GPU_WAIT | VK_SYNC_FEATURE_WAIT_PENDING,
   .init = initialize, .finish = finish, .signal = signal_value,
   .get_value = get_value, .wait_many = wait_values,
};

static VkResult move_completed(struct vk_device *device, struct vk_sync *dst,
                               struct vk_sync *src)
{
   if (vk_device_is_lost_no_report(device)) return VK_ERROR_DEVICE_LOST;
   struct vk_sync_binary *from = vk_sync_as_binary(src);
   struct vk_sync_binary *to = vk_sync_as_binary(dst);
   if (!from || !to || from == to ||
       from->timeline.type != &anv_cubit_cpu_timeline_type ||
       to->timeline.type != &anv_cubit_cpu_timeline_type)
      return VK_ERROR_FEATURE_NOT_PRESENT;
   pthread_mutex_lock(&sync_mutex);
   struct cubit_sync *source = (struct cubit_sync *)&from->timeline;
   struct cubit_sync *target = (struct cubit_sync *)&to->timeline;
   VkResult result = VK_ERROR_UNKNOWN;
   /* vk_queue waits PENDING before moving. Here pending==complete, so a
    * completed value suffices: no future producer refers to the moved event.
    * Source becomes unsignaled; its next signal cannot satisfy dst's wait.
    * Object lifetime and binary reset/move are externally synchronized. */
   if (source->value >= from->next_point && source->value != UINT64_MAX) {
      target->value = source->value;
      to->next_point = from->next_point;
      from->next_point = source->value + 1;
      result = pthread_cond_broadcast(&sync_changed) ? VK_ERROR_UNKNOWN : VK_SUCCESS;
   }
   pthread_mutex_unlock(&sync_mutex);
   return result;
}
static VkResult reset_binary(struct vk_device *device, struct vk_sync *sync)
{
   if (vk_device_is_lost_no_report(device)) return VK_ERROR_DEVICE_LOST;
   struct vk_sync_binary *binary = vk_sync_as_binary(sync);
   if (!binary || binary->timeline.type != &anv_cubit_cpu_timeline_type)
      return VK_ERROR_FEATURE_NOT_PRESENT;
   pthread_mutex_lock(&sync_mutex);
   /* Wrapping to zero would make an unsignaled binary appear completed. */
   VkResult result = VK_ERROR_UNKNOWN;
   if (binary->next_point != UINT64_MAX) {
      binary->next_point++;
      result = VK_SUCCESS;
   }
   pthread_mutex_unlock(&sync_mutex);
   return result;
}

struct vk_sync_binary_type anv_cubit_binary_sync_type(void)
{
   struct vk_sync_binary_type type = vk_sync_binary_get_type(&anv_cubit_cpu_timeline_type);
   type.sync.move = move_completed;
   type.sync.reset = reset_binary;
   /* Mesa's generic wrapper has sync-file hooks; this backend shares nothing. */
   type.sync.import_sync_file = NULL;
   type.sync.export_sync_file = NULL;
   return type;
}
