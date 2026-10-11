#include "gpu-queue-mock.h"
#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

struct gpu_queue_mock_slot gpu_queue_mock[GPU_QUEUE_MOCK_SLOTS];
unsigned gpu_queue_mock_opens, gpu_queue_mock_closes, gpu_queue_mock_submits,
   gpu_queue_mock_observes, gpu_queue_mock_waits;
bool gpu_queue_mock_forbid_waits;
uint64_t gpu_queue_mock_refuse_open = UINT64_MAX;
static pthread_mutex_t mock_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t mock_changed = PTHREAD_COND_INITIALIZER;
/* As CuBit.GPU_Queues.Slots and Native_GPU_Queue's Room_Budget_Ms of 1 ms looks. */
#define GPU_QUEUE_MOCK_DEPTH 64u
#define GPU_QUEUE_MOCK_ROOM_LOOKS 2000u

static uint64_t outstanding(const struct gpu_queue_mock_slot *s)
{
   uint64_t jobs = 0;
   for (unsigned c = 0; c < NATIVE_GPU_CONTEXTS; c++)
      jobs += s->submitted.value[c] - s->completed.value[c];
   return jobs;
}

void gpu_queue_mock_reset(void)
{
   pthread_mutex_lock(&mock_mutex);
   memset(gpu_queue_mock, 0, sizeof(gpu_queue_mock));
   gpu_queue_mock_opens = gpu_queue_mock_closes = gpu_queue_mock_submits = 0;
   gpu_queue_mock_observes = gpu_queue_mock_waits = 0;
   gpu_queue_mock_forbid_waits = false;
   gpu_queue_mock_refuse_open = UINT64_MAX;
   pthread_mutex_unlock(&mock_mutex);
}

enum cubit_gpu_queue_status cubit_gpu_queue_open(uint64_t slot)
{
   assert(slot < GPU_QUEUE_MOCK_SLOTS);
   pthread_mutex_lock(&mock_mutex);
   gpu_queue_mock_opens++;
   enum cubit_gpu_queue_status status = CUBIT_GPU_QUEUE_CLOSED;
   if (slot != gpu_queue_mock_refuse_open && !gpu_queue_mock[slot].open) {
      struct gpu_queue_mock_slot *s = &gpu_queue_mock[slot];
      memset(s, 0, sizeof(*s));
      s->open = true;
      /* The context's setup segment: registration observed value 1. */
      s->submitted.value[0] = s->completed.value[0] = 1;
      status = CUBIT_GPU_QUEUE_OK;
   }
   pthread_mutex_unlock(&mock_mutex);
   return status;
}

void cubit_gpu_queue_close(uint64_t slot)
{
   assert(slot < GPU_QUEUE_MOCK_SLOTS);
   pthread_mutex_lock(&mock_mutex);
   assert(gpu_queue_mock[slot].open);
   gpu_queue_mock[slot].open = false;
   gpu_queue_mock_closes++;
   pthread_cond_broadcast(&mock_changed);
   pthread_mutex_unlock(&mock_mutex);
}

enum cubit_gpu_queue_status cubit_gpu_queue_submit(uint64_t slot,
   const struct cubit_gpu_job *job, uint64_t *signal)
{
   assert(slot < GPU_QUEUE_MOCK_SLOTS && job && signal);
   pthread_mutex_lock(&mock_mutex);
   struct gpu_queue_mock_slot *s = &gpu_queue_mock[slot];
   gpu_queue_mock_submits++;
   *signal = 0;
   enum cubit_gpu_queue_status status = CUBIT_GPU_QUEUE_OK;
   if (!s->open) status = CUBIT_GPU_QUEUE_CLOSED;
   else if (s->failed) status = CUBIT_GPU_QUEUE_FAILED;
   else if (s->inject != CUBIT_GPU_QUEUE_OK) { status = s->inject; s->inject = CUBIT_GPU_QUEUE_OK; }
   else if (job->context >= NATIVE_GPU_CONTEXTS || job->kinds > CUBIT_GPU_KINDS_MASK ||
            (job->operation != CUBIT_GPU_EXECUTE && job->operation != CUBIT_GPU_SIGNAL) ||
            (job->first.target && job->first.context == job->context) ||
            (job->second.target && job->second.context == job->context) ||
            (job->first.target && job->first.target > s->submitted.value[job->first.context]) ||
            (job->second.target && job->second.target > s->submitted.value[job->second.context]))
      status = CUBIT_GPU_QUEUE_INVALID;
   /* Backpressure, as Native_GPU_Queue: a queue holding GPU_QUEUE_MOCK_DEPTH
    * unfinished jobs takes the next only once one completes, within a
    * budget; never a wait call. */
   for (unsigned look = 0; status == CUBIT_GPU_QUEUE_OK && !s->failed &&
                           outstanding(s) >= GPU_QUEUE_MOCK_DEPTH; look++) {
      if (look == GPU_QUEUE_MOCK_ROOM_LOOKS) { s->failed = true; break; }
      struct timespec slice;
      clock_gettime(CLOCK_REALTIME, &slice);
      slice.tv_nsec += 1000000;
      if (slice.tv_nsec >= 1000000000) { slice.tv_sec++; slice.tv_nsec -= 1000000000; }
      pthread_cond_timedwait(&mock_changed, &mock_mutex, &slice);
   }
   if (status == CUBIT_GPU_QUEUE_OK && s->failed) status = CUBIT_GPU_QUEUE_FAILED;
   if (status == CUBIT_GPU_QUEUE_OK) {
      s->last = *job;
      s->jobs++;
      s->executes += job->operation == CUBIT_GPU_EXECUTE;
      s->signals += job->operation == CUBIT_GPU_SIGNAL;
      *signal = ++s->submitted.value[job->context];
   }
   pthread_mutex_unlock(&mock_mutex);
   return status;
}

enum cubit_gpu_queue_status cubit_gpu_queue_observe(uint64_t slot,
   struct native_gpu_values *completed, struct native_gpu_values *submitted)
{
   assert(slot < GPU_QUEUE_MOCK_SLOTS && completed && submitted);
   pthread_mutex_lock(&mock_mutex);
   struct gpu_queue_mock_slot *s = &gpu_queue_mock[slot];
   gpu_queue_mock_observes++;
   *completed = s->completed;
   *submitted = s->submitted;
   enum cubit_gpu_queue_status status = !s->open ? CUBIT_GPU_QUEUE_CLOSED :
      s->failed ? CUBIT_GPU_QUEUE_FAILED : CUBIT_GPU_QUEUE_OK;
   pthread_mutex_unlock(&mock_mutex);
   return status;
}

enum cubit_gpu_queue_status cubit_gpu_queue_wait(uint64_t slot, uint32_t context,
   uint64_t target, uint64_t remaining_ns)
{
   if (gpu_queue_mock_forbid_waits) {
      fprintf(stderr, "FAIL: a wait on the submit path\n");
      abort();
   }
   assert(slot < GPU_QUEUE_MOCK_SLOTS && context < NATIVE_GPU_CONTEXTS);
   struct timespec until;
   clock_gettime(CLOCK_MONOTONIC, &until);
   uint64_t ns = (uint64_t)until.tv_sec * 1000000000u + until.tv_nsec + remaining_ns;
   if (remaining_ns == CUBIT_GPU_NO_TIMEOUT) ns = UINT64_MAX;
   until.tv_sec = ns / 1000000000u; until.tv_nsec = ns % 1000000000u;
   pthread_mutex_lock(&mock_mutex);
   gpu_queue_mock_waits++;
   struct gpu_queue_mock_slot *s = &gpu_queue_mock[slot];
   enum cubit_gpu_queue_status status = CUBIT_GPU_QUEUE_OK;
   while (s->open && !s->failed && s->completed.value[context] < target) {
      /* The condition uses CLOCK_REALTIME by default: wait in short slices
       * and check the monotonic deadline ourselves. */
      struct timespec now, slice;
      clock_gettime(CLOCK_MONOTONIC, &now);
      uint64_t at = (uint64_t)now.tv_sec * 1000000000u + now.tv_nsec;
      if (at >= ns) { status = CUBIT_GPU_QUEUE_TIMED_OUT; break; }
      clock_gettime(CLOCK_REALTIME, &slice);
      slice.tv_nsec += 1000000;
      if (slice.tv_nsec >= 1000000000) { slice.tv_sec++; slice.tv_nsec -= 1000000000; }
      pthread_cond_timedwait(&mock_changed, &mock_mutex, &slice);
   }
   if (status == CUBIT_GPU_QUEUE_OK)
      status = !s->open ? CUBIT_GPU_QUEUE_CLOSED : s->failed ? CUBIT_GPU_QUEUE_FAILED :
               CUBIT_GPU_QUEUE_OK;
   pthread_mutex_unlock(&mock_mutex);
   return status;
}

void gpu_queue_mock_complete(uint64_t slot, uint32_t context, uint64_t value)
{
   pthread_mutex_lock(&mock_mutex);
   struct gpu_queue_mock_slot *s = &gpu_queue_mock[slot];
   assert(value <= s->submitted.value[context] && value >= s->completed.value[context]);
   s->completed.value[context] = value;
   pthread_cond_broadcast(&mock_changed);
   pthread_mutex_unlock(&mock_mutex);
}

void gpu_queue_mock_fail(uint64_t slot)
{
   pthread_mutex_lock(&mock_mutex);
   gpu_queue_mock[slot].failed = true;
   pthread_cond_broadcast(&mock_changed);
   pthread_mutex_unlock(&mock_mutex);
}
