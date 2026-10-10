/* The ANV adapter's session-queue submission (GPU-001 step 3) with actual
 * ANV types, Mesa's real vk_sync dispatcher and binary wrapper, the GPU
 * timeline sync type and the proved Ada timeline logic, host pthreads.
 * Mocked: device IPC, ANV batch chaining, and the queue's C ABI
 * (gpu-queue-mock.c), whose GPU completes jobs when the test says.
 * Every submit runs with waits forbidden: the mock aborts on one. */
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/anv_cubit_sync.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include "../../userspace/mesa/anv/native_gpu_mapping.h"
#include "gpu-queue-mock.h"
#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

static unsigned preparations, registrations, health_queries, drains, closes, retirements;
static unsigned chains, flushes, range_flushes;
uint32_t cubit_intel_memory_contract(uint64_t slot) { (void)slot; return 1; }
uint32_t cubit_intel_session_status(uint64_t slot) { (void)slot; health_queries++; return 0; }
bool cubit_cpu_tracker_drain(struct cubit_cpu_mapping_tracker *tracker)
{ (void)tracker; drains++; return true; }
uint32_t cubit_intel_close_session(uint64_t slot, uint64_t *tag)
{
   /* The queue closes before the session does. */
   assert(!gpu_queue_mock[slot].open);
   closes++; *tag = slot + 1; return 0;
}
uint32_t cubit_intel_poll_session_retirement(uint64_t slot) { (void)slot; retirements++; return 0; }
uint32_t cubit_intel_prepare_context(uint64_t slot) { (void)slot; preparations++; return 0; }
uint32_t cubit_intel_register_context(uint64_t slot) { (void)slot; registrations++; return 0; }
void anv_cmd_buffer_chain_command_buffers(struct anv_cmd_buffer **cmds, uint32_t count)
{ assert(cmds && count); chains++; }
void anv_cmd_buffer_clflush(struct anv_cmd_buffer **cmds, uint32_t count)
{ assert(cmds && count); flushes++; }
void util_flush_range(void *start, size_t size) { assert(start && size); range_flushes++; }
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
   int line, const char *message, ...)
{
   (void)file; (void)line; (void)message;
   p_atomic_set(&device->_lost.lost, 1);
   return VK_ERROR_DEVICE_LOST;
}

#include "queue-rig.h"
static unsigned checks;
#define CHECK(condition) do { assert(condition); checks++; } while (0)

static struct vk_sync *cpu_dependency;
static struct vk_device *dependency_device;
static void *signal_dependency(void *argument)
{
   (void)argument;
   struct timespec delay = {.tv_nsec = 2000000};
   nanosleep(&delay, NULL);
   assert(vk_sync_signal(dependency_device, cpu_dependency, 1) == VK_SUCCESS);
   return NULL;
}

int main(void)
{
   const struct vk_sync_type *const *types = rig_physical();

   /* Startup: the first internal batch prepares, registers and opens the
    * queue, then writes one descriptor and returns before the GPU runs it. */
   static struct rig r;
   open_rig(&r, 60);
   CHECK(!preparations && !gpu_queue_mock_opens && health_queries == 1);
   struct vk_sync *init_fence = make(&r, &binary_type.sync, 0);
   struct vk_sync *output = make(&r, types[0], VK_SYNC_IS_TIMELINE);
   const struct vk_sync_signal out1 = {.sync = output, .signal_value = 1};
   CHECK(startup(&r, init_fence, 1, &out1) == VK_SUCCESS);
   CHECK(preparations == 1 && registrations == 1 && gpu_queue_mock_opens == 1);
   const struct gpu_queue_mock_slot *q = &gpu_queue_mock[r.slot];
   CHECK(q->executes == 1 && q->last.operation == CUBIT_GPU_EXECUTE && q->last.handle == 17 &&
         q->last.gpu == 0x20000 && q->last.offset == 0 && q->last.bytes == 4096 &&
         q->last.context == 0 && !q->last.first.target && !q->last.second.target &&
         q->last.deadline_us == CUBIT_GPU_NO_DEADLINE && range_flushes == 1);
   const uint64_t startup_value = q->submitted.value[0];
   CHECK(!done(&r, init_fence, 0) && !done(&r, output, 1));
   CHECK(vk_sync_wait(&r.device.vk, output, 1, VK_SYNC_WAIT_PENDING, 0) == VK_SUCCESS);
   gpu_queue_mock_complete(r.slot, 0, startup_value);
   CHECK(done(&r, init_fence, 0) && done(&r, output, 1));

   /* vkQueueSubmit: two chained command buffers waiting on the startup
    * output (same context: dropped, ring order covers it), a timeline
    * semaphore and a fence. Several jobs go in flight; none waits. */
   struct vk_sync *timeline = make(&r, types[0], VK_SYNC_IS_TIMELINE);
   struct vk_sync *fence = make(&r, &binary_type.sync, 0);
   uint64_t values[8];
   for (unsigned n = 0; n < 8; n++) {
      const struct vk_sync_wait wait = {.sync = output, .wait_value = 1};
      const struct vk_sync_signal signals[] = {{.sync = timeline, .signal_value = n + 1},
                                               {.sync = fence}};
      CHECK(vk_sync_reset(&r.device.vk, fence) == VK_SUCCESS);
      CHECK(submit(&r, 2, 1, &wait, 2, signals) == VK_SUCCESS);
      CHECK(q->last.operation == CUBIT_GPU_EXECUTE && q->last.handle == 30 &&
            q->last.gpu == 0x40000 && q->last.bytes == 4096 &&
            !q->last.first.target && !q->last.second.target);
      values[n] = q->submitted.value[0];
   }
   CHECK(chains == 8 && flushes == 8 && q->executes == 9);
   uint64_t reached;
   CHECK(vk_sync_get_value(&r.device.vk, timeline, &reached) == VK_SUCCESS && reached == 0);
   gpu_queue_mock_complete(r.slot, 0, values[4]);
   CHECK(vk_sync_get_value(&r.device.vk, timeline, &reached) == VK_SUCCESS && reached == 5);
   CHECK(!done(&r, fence, 0));
   gpu_queue_mock_complete(r.slot, 0, values[7]);
   CHECK(done(&r, fence, 0) && done(&r, timeline, 8));

   /* A same-session point on another context is a descriptor wait. */
   struct vk_sync *other = make(&r, types[0], VK_SYNC_IS_TIMELINE);
   struct cubit_gpu_job side = {.operation = CUBIT_GPU_SIGNAL, .context = 1,
                                .deadline_us = CUBIT_GPU_NO_DEADLINE};
   uint64_t on_other;
   CHECK(cubit_gpu_queue_submit(r.slot, &side, &on_other) == CUBIT_GPU_QUEUE_OK);
   const struct vk_sync_signal other_point = {.sync = other, .signal_value = 4};
   CHECK(anv_cubit_sync_add_point(&r.device.vk, &other_point, r.slot, 1, on_other) == VK_SUCCESS);
   const struct vk_sync_wait cross[] = {{.sync = other, .wait_value = 4},
                                        {.sync = timeline, .wait_value = 8}};
   const struct vk_sync_signal t9 = {.sync = timeline, .signal_value = 9};
   CHECK(submit(&r, 1, 2, cross, 1, &t9) == VK_SUCCESS);
   CHECK(q->last.first.context == 1 && q->last.first.target == on_other && !q->last.second.target);

   /* No commands, no GPU wait: the signal follows the last job; with
    * nothing outstanding it is signalled at once. No descriptor either way. */
   unsigned jobs = q->jobs;
   const struct vk_sync_signal t10 = {.sync = timeline, .signal_value = 10};
   CHECK(submit(&r, 0, 0, NULL, 1, &t10) == VK_SUCCESS && q->jobs == jobs);
   CHECK(!done(&r, timeline, 10));
   gpu_queue_mock_complete(r.slot, 0, q->submitted.value[0]);
   CHECK(done(&r, timeline, 10));
   const struct vk_sync_signal t11 = {.sync = timeline, .signal_value = 11};
   CHECK(submit(&r, 0, 0, NULL, 1, &t11) == VK_SUCCESS && q->jobs == jobs && done(&r, timeline, 11));
   /* No commands but a wait on another context: a Signal barrier. */
   const struct vk_sync_wait cross_only = {.sync = other, .wait_value = 4};
   const struct vk_sync_signal t12 = {.sync = timeline, .signal_value = 12};
   CHECK(submit(&r, 0, 1, &cross_only, 1, &t12) == VK_SUCCESS);
   CHECK(q->jobs == jobs + 1 && q->last.operation == CUBIT_GPU_SIGNAL &&
         q->last.first.context == 1 && q->last.first.target == on_other && !q->last.handle);
   gpu_queue_mock_complete(r.slot, 1, on_other);
   gpu_queue_mock_complete(r.slot, 0, q->submitted.value[0]);
   CHECK(done(&r, timeline, 12));

   /* The pre-lock hook: a pending point needs no wait call; a wait whose
    * signal is not submitted yet blocks there, never in the queue. */
   const unsigned waits = gpu_queue_mock_waits;
   const struct vk_sync_wait pending = {.sync = timeline, .wait_value = 12};
   CHECK(anv_cubit_wait_dependencies(&r.device, 1, &pending, 0) == VK_SUCCESS);
   cpu_dependency = make(&r, types[0], VK_SYNC_IS_TIMELINE);
   dependency_device = &r.device.vk;
   const struct vk_sync_wait unsubmitted = {.sync = cpu_dependency, .wait_value = 1};
   CHECK(anv_cubit_wait_dependencies(&r.device, 1, &unsubmitted, 0) == VK_TIMEOUT);
   pthread_t thread;
   CHECK(!pthread_create(&thread, NULL, signal_dependency, NULL));
   CHECK(anv_cubit_wait_dependencies(&r.device, 1, &unsubmitted, UINT64_MAX) == VK_SUCCESS);
   CHECK(!pthread_join(thread, NULL) && gpu_queue_mock_waits == waits);
   const struct vk_sync_signal t13 = {.sync = timeline, .signal_value = 13};
   CHECK(submit(&r, 1, 1, &unsubmitted, 1, &t13) == VK_SUCCESS && !q->last.first.target);

   /* Health from the status lines once the queue is open: no IPC. */
   const unsigned queries = health_queries;
   CHECK(anv_cubit_check_status(&r.device.vk) == VK_SUCCESS && health_queries == queries);

   /* Device teardown: the queue closes before the session. */
   close_rig(&r);
   CHECK(gpu_queue_mock_closes == 1 && closes == 1 && retirements == 1);

   /* Sequencing error: a wait nobody will signal reaching the queue is a
    * lost device, sticky, never a wait under device->mutex. */
   static struct rig e;
   open_rig(&e, 59);
   struct vk_sync *e_fence = make(&e, &binary_type.sync, 0);
   CHECK(startup(&e, e_fence, 0, NULL) == VK_SUCCESS);
   struct vk_sync *never = make(&e, types[0], VK_SYNC_IS_TIMELINE);
   const struct vk_sync_wait missing = {.sync = never, .wait_value = 1};
   jobs = gpu_queue_mock[e.slot].jobs;
   CHECK(submit(&e, 1, 1, &missing, 0, NULL) == VK_ERROR_DEVICE_LOST);
   p_atomic_set(&e.device.vk._lost.lost, 0);
   CHECK(submit(&e, 1, 0, NULL, 0, NULL) == VK_ERROR_DEVICE_LOST && gpu_queue_mock[e.slot].jobs == jobs);
   close_rig(&e);

   /* A failed descriptor write fails the session; nothing is signalled. */
   static struct rig f;
   open_rig(&f, 58);
   struct vk_sync *f_fence = make(&f, &binary_type.sync, 0);
   struct vk_sync *f_out = make(&f, types[0], VK_SYNC_IS_TIMELINE);
   CHECK(startup(&f, f_fence, 0, NULL) == VK_SUCCESS);
   gpu_queue_mock[f.slot].inject = CUBIT_GPU_QUEUE_FAILED;
   const struct vk_sync_signal f1 = {.sync = f_out, .signal_value = 1};
   CHECK(submit(&f, 1, 0, NULL, 1, &f1) == VK_ERROR_DEVICE_LOST);
   CHECK(vk_sync_wait(&f.device.vk, f_out, 1, VK_SYNC_WAIT_PENDING, 0) == VK_ERROR_DEVICE_LOST);
   p_atomic_set(&f.device.vk._lost.lost, 0);
   CHECK(vk_sync_wait(&f.device.vk, f_out, 1, VK_SYNC_WAIT_PENDING, 0) == VK_TIMEOUT);
   close_rig(&f);

   /* The GPU hangs: a fence waiter is woken with the device lost, and the
    * status lines report it. */
   static struct rig h;
   open_rig(&h, 57);
   struct vk_sync *h_fence = make(&h, &binary_type.sync, 0);
   CHECK(startup(&h, h_fence, 0, NULL) == VK_SUCCESS);
   gpu_queue_mock_fail(h.slot);
   CHECK(vk_sync_wait(&h.device.vk, h_fence, 0, 0, UINT64_MAX) == VK_ERROR_DEVICE_LOST);
   p_atomic_set(&h.device.vk._lost.lost, 0);
   CHECK(anv_cubit_check_status(&h.device.vk) == VK_ERROR_DEVICE_LOST);
   close_rig(&h);

   /* Waits on three contexts exceed a descriptor's two. */
   static struct rig w;
   open_rig(&w, 56);
   struct vk_sync *w_fence = make(&w, &binary_type.sync, 0);
   CHECK(startup(&w, w_fence, 0, NULL) == VK_SUCCESS);
   struct vk_sync_wait three[3];
   for (uint32_t c = 1; c <= 3; c++) {
      struct vk_sync *sync = make(&w, types[0], VK_SYNC_IS_TIMELINE);
      struct cubit_gpu_job j = {.operation = CUBIT_GPU_SIGNAL, .context = c,
                                .deadline_us = CUBIT_GPU_NO_DEADLINE};
      uint64_t gpu;
      CHECK(cubit_gpu_queue_submit(w.slot, &j, &gpu) == CUBIT_GPU_QUEUE_OK);
      const struct vk_sync_signal s = {.sync = sync, .signal_value = 1};
      CHECK(anv_cubit_sync_add_point(&w.device.vk, &s, w.slot, c, gpu) == VK_SUCCESS);
      three[c - 1] = (struct vk_sync_wait){.sync = sync, .wait_value = 1};
   }
   CHECK(submit(&w, 1, 2, three, 0, NULL) == VK_SUCCESS);
   CHECK(submit(&w, 1, 3, three, 0, NULL) == VK_ERROR_DEVICE_LOST);
   close_rig(&w);

   /* Null-heap teardown forbids later GPU work. */
   static struct rig n;
   open_rig(&n, 55);
   struct anv_vm_bind null_bind = {.address = 0x20000, .size = UINT64_C(8) << 30, .op = ANV_VM_BIND};
   struct anv_sparse_submission null_submit = {.binds = &null_bind, .binds_len = 1, .binds_capacity = 1};
   CHECK(anv_cubit_vm_bind(&n.device, &null_submit, ANV_VM_BIND_FLAG_SIGNAL_BIND_TIMELINE) == VK_SUCCESS);
   n.device.batch_bo_pool.bo_alloc_flags |= ANV_BO_ALLOC_NULL_INITIALIZED_HEAP;
   struct vk_sync *n_fence = make(&n, &binary_type.sync, 0);
   CHECK(startup(&n, n_fence, 0, NULL) == VK_SUCCESS);
   null_bind.op = ANV_VM_UNBIND;
   CHECK(anv_cubit_vm_bind(&n.device, &null_submit, ANV_VM_BIND_FLAG_SIGNAL_BIND_TIMELINE) == VK_SUCCESS);
   CHECK(submit(&n, 1, 0, NULL, 0, NULL) == VK_ERROR_DEVICE_LOST);
   close_rig(&n);

   /* A queue the driver refuses to open fails device creation's first batch. */
   static struct rig o;
   open_rig(&o, 54);
   gpu_queue_mock_refuse_open = 54;
   struct vk_sync *o_fence = make(&o, &binary_type.sync, 0);
   CHECK(startup(&o, o_fence, 0, NULL) == VK_ERROR_DEVICE_LOST);
   close_rig(&o);

   CHECK(anv_cubit_memory_poll() == 0);
   printf("ANV session queue PASS: %u checks; startup prepares/opens and returns before the GPU, "
          "8 jobs in flight with no wait, same-context waits dropped, cross-context wait, empty "
          "submits aliased or barriers, pending pre-lock hook, status-line health, sticky "
          "failures, hang wakes waiters, wait overflow, null-heap teardown, refused open "
          "(mock queue/IPC, real vk_sync)\n", checks);
   return 0;
}
