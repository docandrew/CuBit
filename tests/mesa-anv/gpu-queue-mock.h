#pragma once
/* Linux-hosted stand-in for Native_GPU_Queue's C ABI (native_gpu_queue.h)
 * for tests with actual Mesa types. Each slot's "GPU" completes a context's
 * jobs only when the test says so. Every call is counted, so a test can show
 * the submit path never waits. This is not the Ada glue or the driver: those
 * meet in gpu-timeline/gpu_timeline_tests.adb (real queue service, model GPU). */
#include "../../userspace/mesa/anv/native_gpu_queue.h"
#include <stdbool.h>

#define GPU_QUEUE_MOCK_SLOTS 64u
struct gpu_queue_mock_slot {
   bool open, failed;
   struct native_gpu_values submitted, completed;
   struct cubit_gpu_job last;           /* the last descriptor written */
   unsigned jobs, executes, signals;
   enum cubit_gpu_queue_status inject;  /* answer the next submit with this, once */
};
extern struct gpu_queue_mock_slot gpu_queue_mock[GPU_QUEUE_MOCK_SLOTS];
extern unsigned gpu_queue_mock_opens, gpu_queue_mock_closes, gpu_queue_mock_submits,
   gpu_queue_mock_observes, gpu_queue_mock_waits;
/* While set, any wait aborts the test (the submit path must not wait). */
extern bool gpu_queue_mock_forbid_waits;
/* Open refuses this slot (UINT64_MAX: none). */
extern uint64_t gpu_queue_mock_refuse_open;

/* The GPU completes context's jobs up to value; wakes waiters. */
void gpu_queue_mock_complete(uint64_t slot, uint32_t context, uint64_t value);
/* The context hangs: the session fails; wakes waiters. */
void gpu_queue_mock_fail(uint64_t slot);
void gpu_queue_mock_reset(void);
