/* Actual provider implementation / Mesa types, mocked transport only.
 * Not native IPC, GPU execution, or a concurrency proof. */
#include "../../userspace/mesa/launch-session.h"
#include <assert.h>
#include <stdio.h>

static unsigned calls, opens;
static bool fail_query, transfer;
static VkResult attach_result;
static struct anv_cubit_endpoint_pin pending;
static unsigned closes, polls, tracker_polls;
static uint32_t close_status, poll_status, tracker_pending;
static uint64_t close_tag = 1;

uint32_t cubit_intel_close_session(uint64_t slot, uint64_t *tag)
{
   assert(slot == 7);
   closes++;
   *tag = close_tag;
   return close_status;
}
uint32_t cubit_intel_poll_session_retirement(uint64_t slot)
{
   assert(slot == 7);
   polls++;
   return poll_status;
}
uint32_t anv_cubit_memory_poll(void)
{
   tracker_polls++;
   return tracker_pending;
}

bool cubit_gpu_native_query_call(void *context,
   const struct cubit_gpu_query_message *request,
   struct cubit_gpu_query_message *reply)
{
   const struct cubit_gpu_native_endpoint *endpoint = context;
   assert(endpoint->slot == 7 && request);
   calls++;
   reply->words[0] = 123;
   return !fail_query;
}

VkResult anv_cubit_attach_owned_session(struct anv_device *device, uint64_t slot,
   struct anv_cubit_endpoint_pin *pin)
{
   assert(device && slot == 7 && pin->context && pin->retired);
   opens++;
   if (transfer) {
      pending = *pin;
      *pin = (struct anv_cubit_endpoint_pin){0};
   }
   return attach_result;
}

int main(void)
{
   struct anv_memory_budget budget = {0};
   struct anv_device device = {0};
   struct cubit_gpu_query_message request = {0}, reply = {0};
   for (unsigned scenario = 0; scenario < 3; scenario++) {
      struct cubit_mesa_launch_session owner = {0};
      const struct anv_cubit_provider p = cubit_mesa_launch_provider(&owner, &budget);
      assert(p.budget == &budget);
      assert(!p.retain(p.context));
      assert(!cubit_mesa_launch_start(&owner, 64));
      assert(cubit_mesa_launch_start(&owner, 7));
      assert(!cubit_mesa_launch_start(&owner, 8) && owner.endpoint.slot == 7);
      assert(!p.query(p.context, &request, &reply));
      assert(p.retain(p.context));
      fail_query = false;
      assert(p.query(p.context, &request, &reply) && reply.words[0] == 123);
      fail_query = true;
      assert(!p.query(p.context, &request, &reply) && !reply.words[0]);
      transfer = scenario != 0;
      attach_result = scenario == 2 ? VK_SUCCESS : VK_ERROR_DEVICE_LOST;
      const unsigned before = opens;
      assert(p.open_device(p.context, &device) == attach_result);
      assert(owner.session_transferred == transfer);
      assert(p.open_device(p.context, &device) == VK_ERROR_INITIALIZATION_FAILED);
      assert(opens == before + 1);
      p.release(p.context);
      assert(!owner.references && !owner.lifetime_error);
      if (transfer) {
         /* Physical provider release is NOT logical endpoint retirement. */
         assert(pending.context == &owner && p_atomic_read(&owner.retired) == 0);
         pending.retired(pending.context);
         pending = (struct anv_cubit_endpoint_pin){0};
         assert(p_atomic_read(&owner.retired) == 1);
         assert(!p.retain(p.context));
      }
      assert(!cubit_mesa_launch_start(&owner, 7));
      p.release(p.context);
      assert(owner.lifetime_error && !p.retain(p.context));
   }
   assert(calls == 6 && opens == 3);
   for (unsigned scenario = 0; scenario < 6; scenario++) {
      struct cubit_mesa_launch_session owner = {0};
      assert(cubit_mesa_launch_start(&owner, 7));
      closes = polls = 0;
      close_status = scenario == 1 ? 3 : 0;
      close_tag = scenario == 2 ? 0 : 1;
      poll_status = scenario == 3 ? 5 : 4;
      assert(cubit_mesa_launch_retain(&owner));
      assert(cubit_mesa_launch_finish(&owner) == CUBIT_MESA_RETIRE_PENDING);
      assert(!closes && !owner.finishing);
      cubit_mesa_launch_release(&owner);
      if (scenario == 4 || scenario == 5) {
         owner.session_transferred = true;
         tracker_pending = scenario == 4 ? 1 : 0;
      }
      enum cubit_mesa_retirement expected =
         scenario == 0 || scenario == 4 ? CUBIT_MESA_RETIRE_PENDING :
                                         CUBIT_MESA_RETIRE_UNSAFE;
      assert(cubit_mesa_launch_finish(&owner) == expected);
      assert(!cubit_mesa_launch_retain(&owner));
      const unsigned closed_once = closes, polled_once = polls;
      assert(cubit_mesa_launch_finish(&owner) == expected);
      assert(closes == closed_once); /* Never retry close, including failure. */
      if (expected == CUBIT_MESA_RETIRE_UNSAFE)
         assert(polls == polled_once);
      else {
         poll_status = 0;
         if (scenario == 4)
            cubit_mesa_launch_retired(&owner);
         assert(cubit_mesa_launch_finish(&owner) == CUBIT_MESA_RETIRED);
         const unsigned final_polls = polls, final_tracker = tracker_polls;
         assert(cubit_mesa_launch_finish(&owner) == CUBIT_MESA_RETIRED);
         assert(polls == final_polls && tracker_polls == final_tracker);
      }
      assert(closes == (scenario < 4 ? 1u : 0u));
   }
   puts("Launch provider PASS: untransferred failure, transferred failure, success, deferred retirement; no reuse");
   puts("Retirement pump PASS: six paths, reference gate, one-shot close, sticky uncertainty, exact notification");
}
