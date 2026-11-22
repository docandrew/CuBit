/* The GPU timeline sync type (anv_cubit_sync.c) with Mesa's actual vk_sync
 * dispatcher and binary wrapper, the proved Ada timeline logic and host
 * pthreads. The session queue is gpu-queue-mock.c: its "GPU" completes jobs
 * when the test says. No GPU, no driver, no native CuBit claim. */
#include "vk_sync.c"
#include "vk_sync_binary.c"
#include "util/os_time.c"
#include "c11/impl/time.c"
#include "../../userspace/mesa/anv/anv_cubit_sync.c"
#include "gpu-queue-mock.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef CUBIT_SYNC_NATIVE_TEST
#include <cubit/debug.h>
#include <string.h>
static void report_native(const char *message)
{ cubit_debug_write(message, strlen(message)); }
#undef assert
#define assert(condition) do { if (!(condition)) { \
   report_native("MESA-SYNC-NATIVE: FAIL " #condition "\n"); abort(); \
} } while (0)
#endif
int64_t debug_get_num_option(const char *name, int64_t fallback)
{ (void)name; return fallback; }
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
   int line, const char *message, ...)
{
   (void)file; (void)line; (void)message;
   p_atomic_set(&device->_lost.lost, 1);
   return VK_ERROR_DEVICE_LOST;
}

/* The session queue under test (a test endpoint handle). */
#include "../../userspace/c/cubit_handles.h"
#define TEST_ENDPOINT(n) ((cubit_endpoint){(UINT64_C(1) << 32) | (uint64_t)(n)})
#define SLOT TEST_ENDPOINT(5)
static struct vk_device device;
static struct vk_physical_device physical;
static struct vk_sync_binary_type binary_type;

static struct vk_sync *make(const struct vk_sync_type *type, enum vk_sync_flags flags,
                            uint64_t initial)
{
   struct vk_sync *sync = calloc(1, type->size);
   assert(sync && vk_sync_init(&device, sync, type, flags, initial) == VK_SUCCESS);
   return sync;
}
static void drop(struct vk_sync *sync) { vk_sync_finish(&device, sync); free(sync); }

/* What the queue does for a job: the descriptor, then the signal's point. */
static uint64_t job(uint32_t context)
{
   struct cubit_gpu_job j = {.operation = CUBIT_GPU_SIGNAL, .context = context,
                             .deadline_us = CUBIT_GPU_NO_DEADLINE};
   uint64_t gpu;
   assert(cubit_gpu_queue_submit(SLOT, &j, &gpu) == CUBIT_GPU_QUEUE_OK);
   return gpu;
}
static void point(struct vk_sync *sync, uint64_t value, uint32_t context, uint64_t gpu)
{
   const struct vk_sync_signal s = {.sync = sync, .signal_value = value};
   assert(anv_cubit_sync_add_point(&device, &s, SLOT, context, gpu) == VK_SUCCESS);
}
static uint64_t value_of(struct vk_sync *sync)
{
   uint64_t v;
   assert(vk_sync_get_value(&device, sync, &v) == VK_SUCCESS);
   return v;
}
static uint64_t now(void)
{
   struct timespec t;
   clock_gettime(CLOCK_MONOTONIC, &t);
   return (uint64_t)t.tv_sec * 1000000000u + t.tv_nsec;
}

struct completion { uint32_t context; uint64_t value; unsigned delay_us; };
static void *complete_later(void *argument)
{
   struct completion *c = argument;
   struct timespec delay = {.tv_nsec = (long)c->delay_us * 1000};
   nanosleep(&delay, NULL);
   gpu_queue_mock_complete(SLOT, c->context, c->value);
   return NULL;
}
static struct vk_sync *cpu_target;
static void *signal_later(void *argument)
{
   (void)argument;
   struct timespec delay = {.tv_nsec = 2000000};
   nanosleep(&delay, NULL);
   assert(vk_sync_signal(&device, cpu_target, 9) == VK_SUCCESS);
   return NULL;
}
static void *lose_later(void *argument)
{
   (void)argument;
   struct timespec delay = {.tv_nsec = 2000000};
   nanosleep(&delay, NULL);
   p_atomic_set(&device._lost.lost, 1);   /* no signal, no wake */
   return NULL;
}

int main(void)
{
#ifdef CUBIT_SYNC_NATIVE_TEST
   report_native("MESA-SYNC-NATIVE: starting\n");
#endif
   binary_type = anv_cubit_binary_sync_type();
   const struct vk_sync_type *types[] = {&anv_cubit_gpu_timeline_type, &binary_type.sync, NULL};
   physical.supported_sync_types = types;
   device.physical = &physical;
   gpu_queue_mock_reset();
   assert(cubit_gpu_queue_open(SLOT) == CUBIT_GPU_QUEUE_OK);
   unsigned checks = 0;

   /* CPU timelines, as before: signal, wait all/any/pending, threads. */
   struct vk_sync *a = make(types[0], VK_SYNC_IS_TIMELINE, 0);
   struct vk_sync *b = make(types[0], VK_SYNC_IS_TIMELINE, 0);
   struct vk_sync_wait both[] = {{.sync = a, .wait_value = 5}, {.sync = b, .wait_value = 7}};
   assert(vk_sync_wait_many(&device, 2, both, 0, 0) == VK_TIMEOUT);
   assert(vk_sync_wait_many(&device, 2, both, VK_SYNC_WAIT_PENDING, 0) == VK_TIMEOUT);
   assert(vk_sync_signal(&device, a, 5) == VK_SUCCESS);
   assert(vk_sync_wait_many(&device, 2, both, VK_SYNC_WAIT_ANY, 0) == VK_SUCCESS);
   assert(vk_sync_signal(&device, b, 6) == VK_SUCCESS);
   assert(vk_sync_signal(&device, b, 5) == VK_ERROR_UNKNOWN);   /* backwards */
   assert(value_of(b) == 6);
   checks += 7;

   /* A GPU point: pending at once, reached only when its context is. No
    * wait call happens for a zero timeout. */
   struct vk_sync *t = make(types[0], VK_SYNC_IS_TIMELINE, 0);
   uint64_t g1 = job(0), g2 = job(0);
   point(t, 3, 0, g1);
   point(t, 4, 0, g2);
   const unsigned waits_before = gpu_queue_mock_waits;
   assert(value_of(t) == 0);
   assert(vk_sync_wait(&device, t, 4, VK_SYNC_WAIT_PENDING, 0) == VK_SUCCESS);
   assert(vk_sync_wait(&device, t, 5, VK_SYNC_WAIT_PENDING, 0) == VK_TIMEOUT);
   for (unsigned poll = 0; poll < 10000; poll++)
      assert(vk_sync_wait(&device, t, 3, 0, 0) == VK_TIMEOUT);
   assert(gpu_queue_mock_waits == waits_before);
   gpu_queue_mock_complete(SLOT, 0, g1);
   assert(value_of(t) == 3);
   assert(vk_sync_wait(&device, t, 4, 0, 0) == VK_TIMEOUT);
   /* Blocking: a waiter sleeps in the queue's wait until the GPU gets there. */
   struct completion later = {0, g2, 3000};
   pthread_t thread;
   assert(!pthread_create(&thread, NULL, complete_later, &later));
   assert(vk_sync_wait(&device, t, 4, 0, UINT64_MAX) == VK_SUCCESS);
   assert(!pthread_join(thread, NULL));
   assert(gpu_queue_mock_waits > waits_before && value_of(t) == 4);
   checks += 9;

   /* Vulkan's value is the largest signalled: a later point reached on
    * another context first takes the timeline past an earlier one. */
   struct vk_sync *m = make(types[0], VK_SYNC_IS_TIMELINE, 0);
   uint64_t on0 = job(0), on1 = job(1);
   point(m, 10, 0, on0);
   point(m, 11, 1, on1);
   gpu_queue_mock_complete(SLOT, 1, on1);
   assert(value_of(m) == 11);
   gpu_queue_mock_complete(SLOT, 0, on0);
   assert(value_of(m) == 11);
   /* A point must exceed everything signalled or pending. */
   const struct vk_sync_signal stale = {.sync = m, .signal_value = 11};
   assert(anv_cubit_sync_add_point(&device, &stale, SLOT, 0, job(0)) == VK_ERROR_DEVICE_LOST);
   p_atomic_set(&device._lost.lost, 0);
   checks += 3;

   /* Timed out, deadline honoured, no hang. */
   struct vk_sync *late = make(types[0], VK_SYNC_IS_TIMELINE, 0);
   uint64_t never = job(0);
   point(late, 1, 0, never);
   uint64_t start = now();
   assert(vk_sync_wait(&device, late, 1, 0, start + 5000000) == VK_TIMEOUT);
   assert(now() >= start + 5000000);
   checks += 2;

   /* WAIT_ANY with a CPU-only wait and a GPU point: the CPU signal wins
    * while the GPU point never completes (GPU sleeps come in slices). */
   cpu_target = make(types[0], VK_SYNC_IS_TIMELINE, 0);
   struct vk_sync_wait mixed[] = {{.sync = late, .wait_value = 1},
                                  {.sync = cpu_target, .wait_value = 9}};
   assert(!pthread_create(&thread, NULL, signal_later, NULL));
   start = now();
   assert(vk_sync_wait_many(&device, 2, mixed, VK_SYNC_WAIT_ANY, start + 2000000000u) == VK_SUCCESS);
   assert(!pthread_join(thread, NULL) && now() - start < 1000000000u);
   gpu_queue_mock_complete(SLOT, 0, never);
   checks += 2;

   /* A CPU signal past pending points supersedes them. */
   struct vk_sync *cpu = make(types[0], VK_SYNC_IS_TIMELINE, 0);
   point(cpu, 2, 0, job(0));
   assert(vk_sync_signal(&device, cpu, 5) == VK_SUCCESS && value_of(cpu) == 5);
   checks++;

   /* Fences: Desktop's pattern. Reset, a job signals it, poll with a zero
    * timeout until the GPU completes it. 1000 frames, no wait calls. */
   struct vk_sync *fence = make(&binary_type.sync, 0, 0);
   const unsigned fence_waits = gpu_queue_mock_waits;
   for (unsigned frame = 0; frame < 1000; frame++) {
      assert(vk_sync_reset(&device, fence) == VK_SUCCESS);
      assert(vk_sync_wait(&device, fence, 0, 0, 0) == VK_TIMEOUT);
      const struct vk_sync_signal s = {.sync = fence};
      uint64_t gpu = job(0);
      assert(anv_cubit_sync_add_point(&device, &s, SLOT, 0, gpu) == VK_SUCCESS);
      assert(vk_sync_wait(&device, fence, 0, VK_SYNC_WAIT_PENDING, 0) == VK_SUCCESS);
      assert(vk_sync_wait(&device, fence, 0, 0, 0) == VK_TIMEOUT);
      gpu_queue_mock_complete(SLOT, 0, gpu);
      assert(vk_sync_wait(&device, fence, 0, 0, 0) == VK_SUCCESS);
   }
   assert(gpu_queue_mock_waits == fence_waits);
   checks += 5001;

   /* Binary payload move (threaded submit): the moved event keeps its point;
    * the source is unsignalled and a later signal cannot satisfy it. */
   struct vk_sync *src = make(&binary_type.sync, 0, 0), *dst = make(&binary_type.sync, 0, 0);
   assert(vk_sync_move(&device, dst, src) == VK_ERROR_UNKNOWN);   /* not pending */
   const struct vk_sync_signal on_src = {.sync = src};
   uint64_t moved = job(0);
   assert(anv_cubit_sync_add_point(&device, &on_src, SLOT, 0, moved) == VK_SUCCESS);
   assert(vk_sync_move(&device, dst, src) == VK_SUCCESS);
   assert(vk_sync_wait(&device, dst, 0, VK_SYNC_WAIT_PENDING, 0) == VK_SUCCESS);
   assert(vk_sync_wait(&device, dst, 0, 0, 0) == VK_TIMEOUT);
   assert(vk_sync_wait(&device, src, 0, VK_SYNC_WAIT_PENDING, 0) == VK_TIMEOUT);
   gpu_queue_mock_complete(SLOT, 0, moved);
   assert(vk_sync_wait(&device, dst, 0, 0, 0) == VK_SUCCESS);
   assert(vk_sync_wait(&device, src, 0, 0, 0) == VK_TIMEOUT);
   assert(vk_sync_signal(&device, src, 0) == VK_SUCCESS);
   assert(vk_sync_move(&device, dst, src) == VK_SUCCESS);
   assert(vk_sync_wait(&device, dst, 0, 0, 0) == VK_SUCCESS);
   struct vk_sync_binary *edge = vk_sync_as_binary(src);
   edge->next_point = UINT64_MAX - 1;
   assert(vk_sync_reset(&device, src) == VK_SUCCESS && edge->next_point == UINT64_MAX);
   assert(vk_sync_reset(&device, src) == VK_ERROR_UNKNOWN);
   checks += 12;

   /* At most a queue's worth of unresolved points (the queue holds 64
    * unfinished jobs); a 65th point on the same jobs fails closed. */
   struct vk_sync *full = make(types[0], VK_SYNC_IS_TIMELINE, 0);
   uint64_t first_full = 0, last_full = 0;
   for (unsigned n = 1; n <= 64; n++) {
      last_full = job(2);
      if (n == 1) first_full = last_full;
      point(full, n, 2, last_full);
   }
   const struct vk_sync_signal overflow = {.sync = full, .signal_value = 65};
   assert(anv_cubit_sync_add_point(&device, &overflow, SLOT, 2, last_full) == VK_ERROR_DEVICE_LOST);
   p_atomic_set(&device._lost.lost, 0);
   gpu_queue_mock_complete(SLOT, 2, first_full);
   assert(value_of(full) == 1);
   const struct vk_sync_signal room = {.sync = full, .signal_value = 66};
   assert(anv_cubit_sync_add_point(&device, &room, SLOT, 2, last_full) == VK_SUCCESS);
   gpu_queue_mock_complete(SLOT, 2, last_full);
   assert(value_of(full) == 66);
   checks += 4;

   /* A point from another queue cannot join this timeline. */
   assert(cubit_gpu_queue_open(TEST_ENDPOINT(6)) == CUBIT_GPU_QUEUE_OK);
   const struct vk_sync_signal foreign = {.sync = full, .signal_value = 67};
   assert(anv_cubit_sync_add_point(&device, &foreign, TEST_ENDPOINT(6), 0, 1) == VK_ERROR_DEVICE_LOST);
   p_atomic_set(&device._lost.lost, 0);
   checks++;

   /* Device loss: Mesa's flag (a bounded CPU wait sees it without a
    * signal), and the session failing (a GPU sleeper is woken). */
   struct vk_sync *idle = make(types[0], VK_SYNC_IS_TIMELINE, 0);
   assert(!pthread_create(&thread, NULL, lose_later, NULL));
   assert(vk_sync_wait(&device, idle, 1, 0, UINT64_MAX) == VK_ERROR_DEVICE_LOST);
   assert(!pthread_join(thread, NULL));
   p_atomic_set(&device._lost.lost, 0);
   struct vk_sync *hung = make(types[0], VK_SYNC_IS_TIMELINE, 0);
   point(hung, 1, 0, job(0));
   gpu_queue_mock_fail(SLOT);
   assert(vk_sync_wait(&device, hung, 1, 0, UINT64_MAX) == VK_ERROR_DEVICE_LOST);
   p_atomic_set(&device._lost.lost, 0);
   assert(vk_sync_get_value(&device, hung, &start) == VK_ERROR_DEVICE_LOST);
   checks += 3;

   struct vk_sync *all[] = {a, b, t, m, late, cpu_target, cpu, fence, src, dst, full, idle, hung};
   for (unsigned i = 0; i < sizeof(all) / sizeof(all[0]); i++) drop(all[i]);
#ifdef CUBIT_SYNC_NATIVE_TEST
   report_native("MESA-SYNC-NATIVE: PASS timeline threads deadlines loss\n");
#endif
   printf("GPU timeline sync PASS: %u checks; CPU and GPU points, max semantics, pending, "
          "zero-timeout polls without a wait call, mixed ANY, 1000 fence frames, binary move, "
          "bounded points, foreign queue, device loss (mock queue, host pthreads)\n", checks);
   return 0;
}
