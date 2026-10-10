#pragma once
/* Mesa's GPU session queue (native_gpu_queue.ads; GPU-001 step 3). One
 * queue per render endpoint slot, opened after the context registers. Open,
 * close and submit are serialized per slot by the caller; observe and wait
 * may run on any thread. Submit makes no blocking call: it reaps records,
 * writes one descriptor and kicks only when the driver's wake word asks. */
#include <stdint.h>
#include "native_gpu_timeline.h"

enum cubit_gpu_queue_status {
   CUBIT_GPU_QUEUE_OK = 0,
   CUBIT_GPU_QUEUE_FULL = 1,
   CUBIT_GPU_QUEUE_FAILED = 2,     /* the session failed: device lost */
   CUBIT_GPU_QUEUE_CLOSED = 3,
   CUBIT_GPU_QUEUE_INVALID = 4,
   CUBIT_GPU_QUEUE_TIMED_OUT = 5,
};
/* CuBit.GPU_Queues.Opcode */
enum cubit_gpu_operation { CUBIT_GPU_EXECUTE = 1, CUBIT_GPU_SIGNAL = 2 };
/* Absolute monotonic microseconds; none spelled out. */
#define CUBIT_GPU_NO_DEADLINE UINT64_MAX
/* Vulkan's no-timeout as remaining nanoseconds (Native_GPU_Job_Rules). */
#define CUBIT_GPU_NO_TIMEOUT UINT64_MAX

struct cubit_gpu_wait { uint32_t context, reserved; uint64_t target; /* 0: none */ };
struct cubit_gpu_job {
   uint32_t operation, context;
   uint32_t handle, offset, bytes, reserved;   /* Execute: the batch within its BO */
   uint64_t gpu;                                /* raw 48-bit address of the batch */
   struct cubit_gpu_wait first, second;
   uint64_t deadline_us;
};
_Static_assert(sizeof(struct cubit_gpu_job) == 72, "Native_GPU_Queue.Job_Bytes");

enum cubit_gpu_queue_status cubit_gpu_queue_open(uint64_t slot);
void cubit_gpu_queue_close(uint64_t slot);
/* *signal: the job's value on its context (that context's next). */
enum cubit_gpu_queue_status cubit_gpu_queue_submit(uint64_t slot,
   const struct cubit_gpu_job *job, uint64_t *signal);
/* Completed values (status lines and reaped records) and the last value
 * submitted, per context. FAILED: a context faulted, hung or was lost. */
enum cubit_gpu_queue_status cubit_gpu_queue_observe(uint64_t slot,
   struct native_gpu_values *completed, struct native_gpu_values *submitted);
/* Block until context reaches target or remaining_ns pass. OK: reached. */
enum cubit_gpu_queue_status cubit_gpu_queue_wait(uint64_t slot, uint32_t context,
   uint64_t target, uint64_t remaining_ns);
