/* Native test glue for an already admitted, manifest-bound render endpoint.
 * No endpoint lookup, authority minting or mock replies.
 * Optional logical-device mode consumes the one fresh manifest session once;
 * it is NOT a production source of further sessions. The default device mode
 * has no application drawing, though construction may execute setup batches.
 * Explicit transfer/triangle modes additionally submit Vulkan commands.
 */
#include "anv_cubit_physical.h"
#include "anv_cubit_memory.h"
#include "anv_entrypoints.h"
#include "native_gpu_buffers.h"
#include <cubit/debug.h>
#include <stdio.h>
#include <stdarg.h>
#include <limits.h>
#include <unistd.h>
#ifdef CUBIT_TEST_TRANSFER
#include "native-transfer-probe.h"
#endif
#ifdef CUBIT_TEST_TRIANGLE
#include "native-triangle-probe.h"
#endif

uint32_t cubit_test_mesa_authorized_discovery(uint64_t slot);
void cubit_test_mesa_init_stage(const char *stage, int returned, int result);
extern uint64_t cubit_test_render_slot(void);
extern uint32_t cubit_test_run_logged(uint32_t (*callback)(void *));
extern void cubit_test_log(void *context, const char *text, uint32_t length);
static void *log_context;

struct discovery_probe {
   struct cubit_gpu_native_endpoint endpoint;
   struct anv_memory_budget budget;
   unsigned references, queries;
   bool lifetime_error;
   bool started, session_claimed, session_transferred;
   unsigned retired;
};
/* Never stack-owned: a failed device construction may retain its endpoint
 * after the instance is gone. This fixture never replaces capability slots. */
static struct discovery_probe probe;

static void report(const char *format, ...)
{
   char text[192];
   va_list args;
   va_start(args, format);
   int length = vsnprintf(text, sizeof(text), format, args);
   va_end(args);
   if (length > 0) {
      size_t used = (size_t)length < sizeof(text) ? (size_t)length : sizeof(text) - 1;
      cubit_debug_write(text, used);
      /* Typed log records disallow control characters; console keeps LF. */
      if (used && text[used - 1] == '\n')
         used--;
      if (log_context && used)
         cubit_test_log(log_context, text, (uint32_t)used);
   }
}

/* Trace real initialization boundaries, bounded independently of IPC queries.
 * Exported only by this diagnostic probe, not an application/service ABI. */
void cubit_test_mesa_init_stage(const char *stage, int returned, int result)
{
   static unsigned records;
   if (__atomic_fetch_add(&records, 1, __ATOMIC_RELAXED) < 256)
      report("MESA-INIT %s %s result=%d\n", stage,
             returned ? "returned" : "begin", result);
}

#ifdef CUBIT_TEST_PRESENT_TRIANGLE
#include "native-triangle-present.h"
#endif

static bool retain(void *context)
{
   struct discovery_probe *p = context;
   if (p->lifetime_error || p->references == UINT_MAX)
      return false;
   p->references++;
   return true;
}

static void release(void *context)
{
   struct discovery_probe *p = context;
   if (p->references == 0)
      p->lifetime_error = true;
   else
      p->references--;
}

static bool query(void *context, const struct cubit_gpu_query_message *request,
                  struct cubit_gpu_query_message *reply)
{
   struct discovery_probe *p = context;
   if (!p->references || p->lifetime_error || p->queries == UINT_MAX)
      return false;
   p->queries++;
   *reply = (struct cubit_gpu_query_message){0};
   const bool delivered = cubit_gpu_native_query_call(&p->endpoint, request, reply);
   /* Bounded bring-up evidence, not a retry or an admission override. Budget
    * refreshes may continue after discovery, so do not flood the log forever.
    * A failed transport reply is not readable evidence, even if it wrote bytes.
    */
   if (p->queries <= 16) {
      report("MESA-QUERY n=%u label=%x selector=%llu delivered=%u\n",
             p->queries, (unsigned)request->label,
             (unsigned long long)request->words[1], (unsigned)delivered);
      if (delivered) {
         report("MESA-QUERY reply label=%x len=%u flags=%u reserved=%u\n",
                (unsigned)reply->label, (unsigned)reply->length,
                (unsigned)reply->flags, (unsigned)reply->reserved);
         report("MESA-QUERY words=%llx/%llx/%llx/%llx\n",
                (unsigned long long)reply->words[0],
                (unsigned long long)reply->words[1],
                (unsigned long long)reply->words[2],
                (unsigned long long)reply->words[3]);
      }
   }
   return delivered;
}

static void session_retired(void *context)
{
   struct discovery_probe *p = context;
   p_atomic_inc(&p->retired); /* Notification only, under transport lock. */
}

static VkResult open_logical_device(void *context, struct anv_device *device)
{
#ifdef CUBIT_TEST_LOGICAL_DEVICE
   struct discovery_probe *p = context;
   /* Single-threaded fixture, one launch-supplied fresh session. Never retry
    * after either attachment failure or retirement; no authority is minted. */
   if (p->session_claimed)
      return VK_ERROR_INITIALIZATION_FAILED;
   p->session_claimed = true;
   struct anv_cubit_endpoint_pin pin = {p, session_retired};
   VkResult result = anv_cubit_attach_owned_session(device, p->endpoint.slot, &pin);
   p->session_transferred = pin.retired == NULL;
   report("MESA-DEVICE attach=%d transferred=%u\n", result,
          (unsigned)p->session_transferred);
   return result;
#else
   (void)context;
   (void)device;
   return VK_ERROR_INITIALIZATION_FAILED;
#endif
}

uint32_t cubit_test_mesa_authorized_discovery(uint64_t slot)
{
   if (slot > 63 || probe.started)
      return 1;
   probe.started = true;
   probe.endpoint.slot = slot;
   enum cubit_gpu_memory_contract policy;
   if (!cubit_gpu_query_memory(cubit_gpu_native_query_call,
                               &probe.endpoint, &policy))
      return 2;
   report("MESA-DISCOVERY memory-policy=%u\n", (unsigned)policy);
   const struct anv_cubit_provider provider = {
      .context = &probe, .retain = retain, .release = release,
      .query = query, .open_device = open_logical_device, .budget = &probe.budget,
   };
   const VkInstanceCreateInfo create = {
      .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
   };
   VkInstance handle = VK_NULL_HANDLE;
   report("MESA-DISCOVERY instance create beginning\n");
   VkResult result = anv_CreateInstance(&create, NULL, &handle);
   report("MESA-DISCOVERY create=%d\n", result);
   if (result != VK_SUCCESS)
      return 3;
   ANV_FROM_HANDLE(anv_instance, instance, handle);
   result = anv_cubit_install_discovery(instance, &provider, 1);
   report("MESA-DISCOVERY install=%d\n", result);
   uint32_t count = 0;
   PFN_vkEnumeratePhysicalDevices enumerate = (PFN_vkEnumeratePhysicalDevices)
      anv_GetInstanceProcAddr(handle, "vkEnumeratePhysicalDevices");
   report("MESA-DISCOVERY enumerate beginning\n");
   if (result == VK_SUCCESS)
      result = enumerate ? enumerate(handle, &count, NULL) :
                           VK_ERROR_INITIALIZATION_FAILED;
   report("MESA-DISCOVERY enumerate=%d count=%u queries=%u\n",
          result, count, probe.queries);
#ifdef CUBIT_TEST_LOGICAL_DEVICE
   if (result == VK_SUCCESS && count == 1 &&
       policy == CUBIT_GPU_MEMORY_OWNED_WB_COHERENT) {
      VkPhysicalDevice physical = VK_NULL_HANDLE;
      result = enumerate(handle, &count, &physical);
      PFN_vkCreateDevice create_device = (PFN_vkCreateDevice)
         anv_GetInstanceProcAddr(handle, "vkCreateDevice");
      PFN_vkDestroyDevice destroy_device = (PFN_vkDestroyDevice)
         anv_GetInstanceProcAddr(handle, "vkDestroyDevice");
      if (result == VK_SUCCESS && count == 1 && physical &&
          create_device && destroy_device) {
         const float priority = 1.0f;
         const VkDeviceQueueCreateInfo queue = {
            .sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
            .queueFamilyIndex = 0, .queueCount = 1, .pQueuePriorities = &priority,
         };
         const VkDeviceCreateInfo device_info = {
            .sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
            .queueCreateInfoCount = 1, .pQueueCreateInfos = &queue,
         };
         VkDevice device = VK_NULL_HANDLE;
         report("MESA-DEVICE create beginning (internal setup batches possible)\n");
         result = create_device(physical, &device_info, NULL, &device);
         report("MESA-DEVICE create=%d\n", result);
         if (result == VK_SUCCESS && device) {
#ifdef CUBIT_TEST_TRIANGLE
#ifndef CUBIT_TEST_TRIANGLE_CYCLES
#define CUBIT_TEST_TRIANGLE_CYCLES 1
#endif
#if CUBIT_TEST_TRIANGLE_CYCLES < 1 || CUBIT_TEST_TRIANGLE_CYCLES > 16
#error "Triangle cycle count must be between 1 and 16"
#endif
            for (unsigned cycle=0; cycle<CUBIT_TEST_TRIANGLE_CYCLES; cycle++) {
               report("MESA-TRIANGLE cycle=%u/%u beginning (same device)\n",
                      cycle+1,(unsigned)CUBIT_TEST_TRIANGLE_CYCLES);
               result = mesa_triangle_probe(handle, physical, device,
                                         anv_GetInstanceProcAddr, report,
#ifdef CUBIT_TEST_PRESENT_TRIANGLE
                                         present_completed_triangle);
            report("MESA-TRIANGLE window result=%d\n", result);
#else
                                         NULL);
            report("MESA-TRIANGLE result=%d (offscreen; NOT presented)\n", result);
#endif
               if (result!=VK_SUCCESS) {
                  report("MESA-TRIANGLE cycle=%u stopped result=%d (NO replay)\n",
                         cycle+1,result);
                  break;
               }
               /* The synchronous consumer cannot return until Desktop grants
                * are retired; probe cleanup then releases this frame's BOs.
                * Only now may the same device begin another GPU submission. */
               report("MESA-TRIANGLE cycle=%u retired and cleaned\n",cycle+1);
            }
#endif
#ifdef CUBIT_TEST_TRANSFER
            result = mesa_transfer_probe(handle, physical, device,
                                         anv_GetInstanceProcAddr, report);
            report("MESA-TRANSFER result=%d (NO application drawing)\n", result);
#endif
            report("MESA-DEVICE destroy beginning\n");
            destroy_device(device, NULL);
#ifdef CUBIT_TEST_PRESENT_TRIANGLE
            report("MESA-DEVICE destroyed (window probe finished)\n");
#elif defined(CUBIT_TEST_TRIANGLE)
            report("MESA-DEVICE destroyed (offscreen draw attempted; NOT presented)\n");
#else
            report("MESA-DEVICE destroyed (NO application drawing batch)\n");
#endif
         } else if (result == VK_SUCCESS) {
            result = VK_ERROR_INITIALIZATION_FAILED;
         }
      } else {
         result = VK_ERROR_INITIALIZATION_FAILED;
      }
   }
#endif
   report("MESA-DISCOVERY instance destroy beginning\n");
   anv_DestroyInstance(handle, NULL);
   report("MESA-DISCOVERY destroyed references=%u lifetime-error=%u\n",
          probe.references, (unsigned)probe.lifetime_error);
   if (probe.references || probe.lifetime_error || !probe.queries)
      return 4;
   /* An expected cache-policy rejection must not hide failures that occurred
    * before the factory queried the memory contract. The query counter alone
    * cannot identify that stage; policy1 is diagnostic, not a PASS verdict.
    */
   if (policy == CUBIT_GPU_MEMORY_OWNED_WB_EXPLICIT) {
      report("MESA-DISCOVERY explicit-only: Vulkan admission not established\n");
      return 5;
   }
   if (policy != CUBIT_GPU_MEMORY_OWNED_WB_COHERENT ||
       result != VK_SUCCESS || count != 1)
      return 6;
#ifdef CUBIT_TEST_LOGICAL_DEVICE
   if (!probe.session_transferred)
      return 7;
   report("MESA-DEVICE lifecycle complete; endpoint retirement checked separately\n");
#else
   report("MESA-DISCOVERY physical device enumerated (NO logical device/GPU work)\n");
#endif
   return 0;
}

static uint32_t run_probe(void *context)
{
   log_context = context;
   const uint64_t slot = cubit_test_render_slot();
   report("MESA-DISCOVERY admitted app entered\n");
   uint32_t status = cubit_test_mesa_authorized_discovery(slot);
   uint64_t retired = 0;
   uint32_t closed = 0;
   if (probe.session_transferred) {
      unsigned pending = anv_cubit_memory_poll();
      report("MESA-DEVICE pending retirement=%u\n", pending);
      /* Keep the process/pin alive. An uncertain close is quarantined by the
       * tracker and never replayed here. No cap reuse or backing-free claim. */
      while (pending) {
         usleep(100000);
         pending = anv_cubit_memory_poll();
      }
      closed = p_atomic_read(&probe.retired) == 1 ? 0 : 1;
   } else {
      closed = cubit_intel_close_session(slot, &retired);
   }
   report("MESA-DISCOVERY result=%u close=%u\n", status, closed);
   /* Closing is one-shot. Success is not proof that backing was reclaimed. */
   log_context = NULL;
   return status == 0 && closed == 0 ? 0 : 1;
}

int main(void)
{
   return (int)cubit_test_run_logged(run_probe);
}
