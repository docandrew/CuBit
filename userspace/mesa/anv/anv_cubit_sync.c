/* Mesa-facing synchronization: the GPU timeline (GPU-001 step 3).
 * A Vulkan timeline value the CPU knows is reached, plus the points the
 * session queue signals later, each a (context, value) on the GPU's own
 * timeline. The logic is Ada (native_gpu_timeline.ads, proved); this file
 * is the vk_sync glue: locking, resolution against the queue's status lines
 * and blocking waits. */
#include "anv_cubit_sync.h"
#include "native_gpu_queue.h"
#include "vk_device.h"
#include <pthread.h>
#include <errno.h>
#include <time.h>

struct cubit_gpu_timeline {
   struct vk_sync base;
   uint64_t slot;        /* the session queue every point belongs to */
   bool bound;           /* slot is set: the first point fixed it */
   struct native_gpu_timeline state;
};

/* A wait on CPU signals only (nothing submitted for it yet) sleeps on the
 * condition in slices this long, so it also observes device loss. While a
 * WAIT_ANY also has GPU points, its GPU sleeps are this long at most. */
#define CPU_WAIT_SLICE_NS UINT64_C(10000000)
#define MIXED_WAIT_SLICE_NS UINT64_C(1000000)
#define NANOSECONDS_PER_SECOND UINT64_C(1000000000)

/* One lock/condition for every timeline: wait-many examines all values
 * atomically, and a CPU signal or a new point cannot be missed between
 * checking separate objects. Never held across a blocking call: GPU waits
 * drop it. Lock order: a queue's lock (anv_cubit_memory.c), then this. */
static pthread_mutex_t sync_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t sync_changed;
static pthread_once_t sync_once = PTHREAD_ONCE_INIT;
static bool sync_ready;
static void initialize_condition(void)
{
   pthread_condattr_t attr;
   if (native_gpu_timeline_bytes() != NATIVE_GPU_TIMELINE_BYTES) return;
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

static struct cubit_gpu_timeline *as_timeline(struct vk_sync *sync)
{
   return sync && sync->type == &anv_cubit_gpu_timeline_type ?
      (struct cubit_gpu_timeline *)sync : NULL;
}

/* A binary semaphore or fence is Mesa's binary-on-timeline wrapper around
 * this type: its event is the inner timeline at next_point. */
static struct cubit_gpu_timeline *timeline_point(struct vk_sync *sync, uint64_t *value)
{
   struct cubit_gpu_timeline *timeline = as_timeline(sync);
   if (timeline) return timeline;
   struct vk_sync_binary *binary = sync ? vk_sync_as_binary(sync) : NULL;
   if (!binary || binary->timeline.type != &anv_cubit_gpu_timeline_type) return NULL;
   *value = binary->next_point;
   return (struct cubit_gpu_timeline *)&binary->timeline;
}

/* Under sync_mutex: take every point its context has completed. False: the
 * session failed or its queue is gone (device lost). No IPC. */
static bool resolve_locked(struct cubit_gpu_timeline *t)
{
   if (!t->bound) return true;
   struct native_gpu_values completed, submitted;
   if (cubit_gpu_queue_observe(t->slot, &completed, &submitted) != CUBIT_GPU_QUEUE_OK)
      return false;
   native_gpu_timeline_resolve(&t->state, &completed);
   return true;
}

static VkResult lost(struct vk_device *device)
{
   return vk_device_set_lost(device, "CuBit GPU session failed");
}

static VkResult initialize(struct vk_device *device, struct vk_sync *sync, uint64_t initial)
{
   (void)device;
   if (sync->flags != VK_SYNC_IS_TIMELINE)
      return VK_ERROR_FEATURE_NOT_PRESENT;
   VkResult result = anv_cubit_sync_prepare();
   if (result != VK_SUCCESS) return result;
   struct cubit_gpu_timeline *t = (struct cubit_gpu_timeline *)sync;
   t->slot = 0;
   t->bound = false;
   native_gpu_timeline_initialize(&t->state, initial);
   return VK_SUCCESS;
}
static void finish(struct vk_device *device, struct vk_sync *sync)
{ (void)device; (void)sync; }

static VkResult signal_value(struct vk_device *device, struct vk_sync *sync, uint64_t value)
{
   if (vk_device_is_lost_no_report(device)) return VK_ERROR_DEVICE_LOST;
   uint32_t ok = 0;
   pthread_mutex_lock(&sync_mutex);
   native_gpu_timeline_signal(&((struct cubit_gpu_timeline *)sync)->state, value, &ok);
   VkResult result = ok ? VK_SUCCESS : VK_ERROR_UNKNOWN;
   if (ok && pthread_cond_broadcast(&sync_changed)) result = VK_ERROR_UNKNOWN;
   pthread_mutex_unlock(&sync_mutex);
   return result;
}

static VkResult get_value(struct vk_device *device, struct vk_sync *sync, uint64_t *value)
{
   if (vk_device_is_lost_no_report(device)) return VK_ERROR_DEVICE_LOST;
   struct cubit_gpu_timeline *t = (struct cubit_gpu_timeline *)sync;
   uint64_t last;
   pthread_mutex_lock(&sync_mutex);
   bool healthy = resolve_locked(t);
   native_gpu_timeline_value(&t->state, value, &last);
   pthread_mutex_unlock(&sync_mutex);
   return healthy ? VK_SUCCESS : lost(device);
}

static uint64_t now_ns(bool *ok)
{
   struct timespec now;
   *ok = clock_gettime(CLOCK_MONOTONIC, &now) == 0;
   return *ok ? (uint64_t)now.tv_sec * NANOSECONDS_PER_SECOND + (uint64_t)now.tv_nsec : 0;
}

static VkResult wait_values(struct vk_device *device, uint32_t count,
   const struct vk_sync_wait *waits, enum vk_sync_wait_flags flags, uint64_t deadline)
{
   if ((flags & ~(VK_SYNC_WAIT_ANY | VK_SYNC_WAIT_PENDING)) || (count && !waits))
      return VK_ERROR_FEATURE_NOT_PRESENT;
   for (uint32_t i = 0; i < count; i++)
      if (!as_timeline(waits[i].sync))
         return VK_ERROR_FEATURE_NOT_PRESENT;
   const bool any = flags & VK_SYNC_WAIT_ANY, pending = flags & VK_SYNC_WAIT_PENDING;
   pthread_mutex_lock(&sync_mutex);
   VkResult result = VK_SUCCESS;
   for (;;) {
      if (vk_device_is_lost_no_report(device)) { result = VK_ERROR_DEVICE_LOST; break; }
      uint32_t satisfied = 0;
      bool healthy = true, on_gpu = false, on_cpu = false;
      uint64_t slot = 0, target = 0;
      uint32_t context = 0;
      for (uint32_t i = 0; i < count; i++) {
         struct cubit_gpu_timeline *t = as_timeline(waits[i].sync);
         healthy = healthy && resolve_locked(t);
         enum native_gpu_wait_kind kind;
         uint32_t point_context;
         uint64_t point_gpu;
         native_gpu_timeline_find(&t->state, waits[i].wait_value, &kind, &point_context, &point_gpu);
         if (kind == NATIVE_GPU_WAIT_REACHED || (pending && kind == NATIVE_GPU_WAIT_ON_GPU)) {
            satisfied++;
         } else if (kind == NATIVE_GPU_WAIT_ON_GPU) {
            /* ALL: any unmet point will do. ANY: the earliest on one context. */
            if (!on_gpu || (any && t->slot == slot && point_context == context &&
                            point_gpu < target)) {
               slot = t->slot; context = point_context; target = point_gpu;
            }
            on_gpu = true;
         } else {
            on_cpu = true;
         }
      }
      if (!healthy) { pthread_mutex_unlock(&sync_mutex); return lost(device); }
      if (satisfied == count || (any && satisfied)) break;
      bool clock_ok;
      uint64_t now = now_ns(&clock_ok);
      if (!clock_ok) { result = VK_ERROR_UNKNOWN; break; }
      if (now >= deadline) { result = VK_TIMEOUT; break; }
      if (on_gpu) {
         /* Sleep on the driver without the lock: a CPU signal or new point
          * on another object is examined after. A WAIT_ANY that could also
          * be met by the CPU sleeps in slices. */
         uint64_t remaining = deadline == UINT64_MAX ? CUBIT_GPU_NO_TIMEOUT : deadline - now;
         if (any && on_cpu && remaining > MIXED_WAIT_SLICE_NS)
            remaining = MIXED_WAIT_SLICE_NS;
         pthread_mutex_unlock(&sync_mutex);
         enum cubit_gpu_queue_status status = cubit_gpu_queue_wait(slot, context, target, remaining);
         if (status != CUBIT_GPU_QUEUE_OK && status != CUBIT_GPU_QUEUE_TIMED_OUT)
            return lost(device);
         pthread_mutex_lock(&sync_mutex);
         continue;
      }
      /* Bounded sleep also observes device loss even if no sync is signaled.
       * The absolute Vulkan deadline is monotonic, never realtime. */
      uint64_t until = deadline - now > CPU_WAIT_SLICE_NS ? now + CPU_WAIT_SLICE_NS : deadline;
      struct timespec timeout = { .tv_sec = until / NANOSECONDS_PER_SECOND,
                                  .tv_nsec = until % NANOSECONDS_PER_SECOND };
      int error = pthread_cond_timedwait(&sync_changed, &sync_mutex, &timeout);
      if (error && error != ETIMEDOUT) { result = VK_ERROR_UNKNOWN; break; }
      /* Always recheck values and clock, including spurious wakeups. */
   }
   pthread_mutex_unlock(&sync_mutex);
   return result;
}

const struct vk_sync_type anv_cubit_gpu_timeline_type = {
   .size = sizeof(struct cubit_gpu_timeline),
   /* GPU_WAIT: the session queue carries same-session points as descriptor
    * waits. WAIT_PENDING: a submitted point counts as pending, so Mesa's
    * threaded submit can order waits before signals reach the queue. */
   .features = VK_SYNC_FEATURE_TIMELINE | VK_SYNC_FEATURE_CPU_WAIT |
               VK_SYNC_FEATURE_CPU_SIGNAL | VK_SYNC_FEATURE_WAIT_ANY |
               VK_SYNC_FEATURE_GPU_WAIT | VK_SYNC_FEATURE_WAIT_PENDING,
   .init = initialize, .finish = finish, .signal = signal_value,
   .get_value = get_value, .wait_many = wait_values,
};

VkResult
anv_cubit_sync_find(struct vk_device *device, const struct vk_sync_wait *wait, uint64_t slot,
                    enum native_gpu_wait_kind *kind, uint32_t *context, uint64_t *gpu)
{
   uint64_t value = wait->wait_value;
   struct cubit_gpu_timeline *t = timeline_point(wait->sync, &value);
   if (!t) return VK_ERROR_FEATURE_NOT_PRESENT;
   pthread_mutex_lock(&sync_mutex);
   bool healthy = resolve_locked(t);
   native_gpu_timeline_find(&t->state, value, kind, context, gpu);
   /* A point from another device's queue cannot be a descriptor wait. */
   bool foreign = *kind == NATIVE_GPU_WAIT_ON_GPU && t->slot != slot;
   pthread_mutex_unlock(&sync_mutex);
   if (!healthy) return lost(device);
   return foreign ? VK_ERROR_FEATURE_NOT_PRESENT : VK_SUCCESS;
}

VkResult
anv_cubit_sync_add_point(struct vk_device *device, const struct vk_sync_signal *signal,
                         uint64_t slot, uint32_t context, uint64_t gpu)
{
   uint64_t value = signal->signal_value;
   struct cubit_gpu_timeline *t = timeline_point(signal->sync, &value);
   if (!t) return VK_ERROR_FEATURE_NOT_PRESENT;
   pthread_mutex_lock(&sync_mutex);
   VkResult result = VK_SUCCESS;
   if (t->bound && t->slot != slot) {
      result = VK_ERROR_FEATURE_NOT_PRESENT;
   } else {
      t->slot = slot;
      t->bound = true;
      /* Resolve first: every point left then belongs to a job the queue
       * still owes a record, which bounds the table. */
      enum native_gpu_add_result added = NATIVE_GPU_FULL;
      if (resolve_locked(t))
         native_gpu_timeline_add(&t->state, value, context, gpu, &added);
      if (added != NATIVE_GPU_ADDED || pthread_cond_broadcast(&sync_changed))
         result = VK_ERROR_UNKNOWN;
   }
   pthread_mutex_unlock(&sync_mutex);
   return result == VK_SUCCESS ? result : lost(device);
}

static VkResult move_payload(struct vk_device *device, struct vk_sync *dst,
                             struct vk_sync *src)
{
   if (vk_device_is_lost_no_report(device)) return VK_ERROR_DEVICE_LOST;
   struct vk_sync_binary *from = vk_sync_as_binary(src);
   struct vk_sync_binary *to = vk_sync_as_binary(dst);
   if (!from || !to || from == to ||
       from->timeline.type != &anv_cubit_gpu_timeline_type ||
       to->timeline.type != &anv_cubit_gpu_timeline_type)
      return VK_ERROR_FEATURE_NOT_PRESENT;
   struct cubit_gpu_timeline *source = (struct cubit_gpu_timeline *)&from->timeline;
   struct cubit_gpu_timeline *target = (struct cubit_gpu_timeline *)&to->timeline;
   pthread_mutex_lock(&sync_mutex);
   /* vk_queue waits PENDING before moving: the event is reached or a point
    * the queue will signal. Target takes it, points and all; source becomes
    * unsignaled above anything it had, so a later signal cannot satisfy the
    * moved wait. Object lifetime and binary reset/move are externally
    * synchronized. */
   VkResult result = VK_ERROR_UNKNOWN;
   uint64_t target_next = 0, source_next = 0;
   uint32_t ok = 0;
   if (resolve_locked(source)) {
      native_gpu_timeline_move(&target->state, &source->state, from->next_point,
                               &target_next, &source_next, &ok);
      if (ok) {
         to->next_point = target_next;
         from->next_point = source_next;
         target->slot = source->slot;
         target->bound = source->bound;
         result = pthread_cond_broadcast(&sync_changed) ? VK_ERROR_UNKNOWN : VK_SUCCESS;
      }
   }
   pthread_mutex_unlock(&sync_mutex);
   return result;
}
static VkResult reset_binary(struct vk_device *device, struct vk_sync *sync)
{
   if (vk_device_is_lost_no_report(device)) return VK_ERROR_DEVICE_LOST;
   struct vk_sync_binary *binary = vk_sync_as_binary(sync);
   if (!binary || binary->timeline.type != &anv_cubit_gpu_timeline_type)
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
   struct vk_sync_binary_type type = vk_sync_binary_get_type(&anv_cubit_gpu_timeline_type);
   type.sync.move = move_payload;
   type.sync.reset = reset_binary;
   /* Mesa's generic wrapper has sync-file hooks; this backend shares nothing. */
   type.sync.import_sync_file = NULL;
   type.sync.export_sync_file = NULL;
   return type;
}
