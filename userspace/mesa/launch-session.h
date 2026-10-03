/* Trusted one-shot ANV provider for a launch-supplied render session.
 * Not a capability broker, session factory, or public render admission API.
 * Storage must remain at a stable address through all deferred retirement.
 * The owner externally serializes retain/release/query/open and capability
 * operations. Only the retirement callback may run concurrently; it publishes
 * an atomic notification and performs no IPC, allocation or locking.
 */
#pragma once
#include "anv_cubit_physical.h"
#include "anv_cubit_memory.h"
#include "native_gpu_buffers.h"
#include <limits.h>

struct cubit_mesa_launch_session {
   struct cubit_gpu_native_endpoint endpoint;
   unsigned references, queries;
   bool lifetime_error;
   bool started, session_claimed, session_transferred;
   unsigned retired;
   bool finishing, close_attempted, finish_unsafe, finish_ready;
};

/* Zero-initialized once, never reset/reused, including after retirement.
 * Budget is supplied separately: all providers for the same process/GPU must
 * share its persistent accounting record, not reset it for each new device.
 */
static inline bool
cubit_mesa_launch_start(struct cubit_mesa_launch_session *p, uint64_t slot)
{
   if (!p || slot > 63 || p->started)
      return false;
   p->started = true;
   p->endpoint.slot = slot;
   return true;
}

static inline bool cubit_mesa_launch_retain(void *context)
{
   struct cubit_mesa_launch_session *p = context;
   if (!p || !p->started || p->finishing || p->lifetime_error || p->references == UINT_MAX ||
       p_atomic_read(&p->retired))
      return false;
   p->references++;
   return true;
}

static inline void cubit_mesa_launch_release(void *context)
{
   struct cubit_mesa_launch_session *p = context;
   if (!p)
      return;
   if (!p->references)
      p->lifetime_error = true;
   else
      p->references--;
}

static inline bool
cubit_mesa_launch_query(void *context,
                       const struct cubit_gpu_query_message *request,
                       struct cubit_gpu_query_message *reply)
{
   struct cubit_mesa_launch_session *p = context;
   if (!reply)
      return false;
   *reply = (struct cubit_gpu_query_message){0};
   if (!p || !request || !p->started || p->finishing || !p->references || p->lifetime_error ||
       p->queries == UINT_MAX || p_atomic_read(&p->retired))
      return false;
   p->queries++;
   if (cubit_gpu_native_query_call(&p->endpoint, request, reply))
      return true;
   *reply = (struct cubit_gpu_query_message){0};
   return false;
}

static inline void cubit_mesa_launch_retired(void *context)
{
   struct cubit_mesa_launch_session *p = context;
   p_atomic_inc(&p->retired);
}

static inline VkResult
cubit_mesa_launch_open(void *context, struct anv_device *device)
{
   struct cubit_mesa_launch_session *p = context;
   if (!p || !device || !p->started || p->finishing || !p->references || p->lifetime_error ||
       p->session_claimed || p_atomic_read(&p->retired))
      return VK_ERROR_INITIALIZATION_FAILED;
   /* Consume the attempt before calling Mesa, even if attachment fails. */
   p->session_claimed = true;
   struct anv_cubit_endpoint_pin pin = {p, cubit_mesa_launch_retired};
   VkResult result = anv_cubit_attach_owned_session(device, p->endpoint.slot, &pin);
   p->session_transferred = pin.retired == NULL;
   return result;
}

static inline struct anv_cubit_provider
cubit_mesa_launch_provider(struct cubit_mesa_launch_session *p,
                           struct anv_memory_budget *shared_budget)
{
   return (struct anv_cubit_provider){
      .context=p, .retain=cubit_mesa_launch_retain,
      .release=cubit_mesa_launch_release, .query=cubit_mesa_launch_query,
      .open_device=cubit_mesa_launch_open, .budget=shared_budget,
   };
}

enum cubit_mesa_retirement {
   CUBIT_MESA_RETIRED = 0,
   CUBIT_MESA_RETIRE_PENDING = 1,
   CUBIT_MESA_RETIRE_UNSAFE = 2,
};

/* One pass, no sleep or close replay. Individual transport calls may block;
 * this is not an asynchronous IPC or hard-latency guarantee. Call after device
 * and instance destruction, with all external image consumers retired.
 * Pending/unsafe require retaining owner storage and the endpoint capability.
 * Retired does not grant authority to recycle a slot or reclaim GPU backing.
 */
static inline enum cubit_mesa_retirement
cubit_mesa_launch_finish(struct cubit_mesa_launch_session *p)
{
   if (!p || !p->started || p->lifetime_error || p->finish_unsafe)
      return CUBIT_MESA_RETIRE_UNSAFE;
   if (p->references)
      return CUBIT_MESA_RETIRE_PENDING;
   if (p->finish_ready)
      return CUBIT_MESA_RETIRED;
   p->finishing = true;
   if (p->session_transferred) {
      const unsigned pending = anv_cubit_memory_poll();
      const unsigned retired = p_atomic_read(&p->retired);
      if (retired == 1) {
         p->finish_ready = true;
         return CUBIT_MESA_RETIRED;
      }
      if (!retired && pending)
         return CUBIT_MESA_RETIRE_PENDING;
      /* No tracker and no exact notification is not proof of retirement. */
      p->finish_unsafe = true;
      return CUBIT_MESA_RETIRE_UNSAFE;
   }
   if (!p->close_attempted) {
      p->close_attempted = true;
      uint64_t tag = 0;
      if (cubit_intel_close_session(p->endpoint.slot, &tag) != 0 || !tag) {
         p->finish_unsafe = true;
         return CUBIT_MESA_RETIRE_UNSAFE;
      }
   }
   const uint32_t result = cubit_intel_poll_session_retirement(p->endpoint.slot);
   if (result == 4)
      return CUBIT_MESA_RETIRE_PENDING;
   if (result == 0) {
      p->finish_ready = true;
      return CUBIT_MESA_RETIRED;
   }
   p->finish_unsafe = true;
   return CUBIT_MESA_RETIRE_UNSAFE;
}
