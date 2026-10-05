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
#include "../../userspace/mesa/device-bootstrap.h"
#include "../../userspace/mesa/launch-session.h"
#include <cubit/debug.h>
#include <stdio.h>
#include <stdarg.h>
#include <limits.h>
#include <unistd.h>
#ifdef CUBIT_TEST_TRANSFER
#include "native-transfer-probe.h"
#endif
#ifdef CUBIT_TEST_TRIANGLE
#ifdef CUBIT_TEST_TEAPOT
#include "../mesa-teapot/render.h"
#define mesa_triangle_probe mesa_teapot_probe
#define mesa_scene_probe mesa_teapot_probe_with_source
#define DRAW_LOG "MESA-TEAPOT"
#else
#include "native-triangle-probe.h"
#define mesa_scene_probe mesa_triangle_probe_with_source
#define DRAW_LOG "MESA-TRIANGLE"
#endif
#endif
#ifdef CUBIT_TEST_SCENE
#include "native-scene-consumer.h"
#endif

uint32_t cubit_test_mesa_authorized_discovery(uint64_t slot);
void cubit_test_mesa_init_stage(const char *stage, int returned, int result);
extern uint64_t cubit_test_render_slot(void);
extern uint32_t cubit_test_run_logged(uint32_t (*callback)(void *));
extern void cubit_test_log(void *context, const char *text, uint32_t length);
static void *log_context;

/* Never stack-owned: a failed device construction may retain its endpoint
 * after the instance is gone. This fixture never replaces capability slots. */
static struct cubit_mesa_launch_session probe;
static struct anv_memory_budget budget;

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

#ifdef CUBIT_TEST_SCENE
static VkResult compose_completed_source(const struct mesa_completed_image *source,
                                        mesa_completed_pixels present)
{
   return mesa_scene_compose(source,present,report);
}
#endif

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
#ifdef CUBIT_TEAPOT_GALLERY
#include "native-gallery-present.h"
#else
#include "native-triangle-present.h"
#endif
#endif

static bool query(void *context, const struct cubit_gpu_query_message *request,
                  struct cubit_gpu_query_message *reply)
{
   struct cubit_mesa_launch_session *p = context;
   const unsigned previous = p->queries;
   const bool delivered = cubit_mesa_launch_query(context, request, reply);
   /* Bounded bring-up evidence, not a retry or an admission override. Budget
    * refreshes may continue after discovery, so do not flood the log forever.
    * A failed transport reply is not readable evidence, even if it wrote bytes.
    */
   if (p->queries != previous && p->queries <= 16) {
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

static VkResult open_logical_device(void *context, struct anv_device *device)
{
#ifdef CUBIT_TEST_LOGICAL_DEVICE
   struct cubit_mesa_launch_session *p = context;
   VkResult result = cubit_mesa_launch_open(context, device);
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
   if (!cubit_mesa_launch_start(&probe, slot))
      return 1;
   enum cubit_gpu_memory_contract policy;
   if (!cubit_gpu_query_memory(cubit_gpu_native_query_call,
                               &probe.endpoint, &policy))
      return 2;
   report("MESA-DISCOVERY memory-policy=%u\n", (unsigned)policy);
   struct anv_cubit_provider provider = cubit_mesa_launch_provider(&probe, &budget);
   provider.query = query; /* Diagnostic wrapper, same ownership checks. */
   provider.open_device = open_logical_device;
#ifdef CUBIT_TEST_SCENE
   const VkApplicationInfo application={.sType=VK_STRUCTURE_TYPE_APPLICATION_INFO,
      .apiVersion=VK_API_VERSION_1_1};
#endif
   const VkInstanceCreateInfo create = {
      .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
#ifdef CUBIT_TEST_SCENE
      .pApplicationInfo=&application,
#endif
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
      if (result == VK_SUCCESS && count == 1 && physical) {
         struct cubit_mesa_device owned = {0};
         report("MESA-DEVICE create beginning (internal setup batches possible)\n");
         result = cubit_mesa_device_create(handle, physical,
            anv_GetInstanceProcAddr, 0, VK_QUEUE_GRAPHICS_BIT, &owned);
         VkDevice device = owned.device;
         report("MESA-DEVICE create=%d\n", result);
         if (result == VK_SUCCESS && device) {
#ifdef CUBIT_TEST_TRIANGLE
#ifndef CUBIT_TEST_TRIANGLE_CYCLES
#define CUBIT_TEST_TRIANGLE_CYCLES 1
#endif
/* Bounded diagnostic workload; does not enlarge driver resource limits. */
#if CUBIT_TEST_TRIANGLE_CYCLES < 1 || CUBIT_TEST_TRIANGLE_CYCLES > 1024
#error "Triangle cycle count must be between 1 and 1024"
#endif
            for (unsigned cycle=0; cycle<CUBIT_TEST_TRIANGLE_CYCLES; cycle++) {
               report(DRAW_LOG " cycle=%u/%u beginning (same device)\n",
                      cycle+1,(unsigned)CUBIT_TEST_TRIANGLE_CYCLES);
#ifdef CUBIT_TEST_SCENE
               result = mesa_scene_probe(handle,physical,device,anv_GetInstanceProcAddr,report,
#ifdef CUBIT_TEST_PRESENT_TRIANGLE
                                        present_completed_triangle,
#else
                                        NULL,
#endif
                                        compose_completed_source);
               report("MESA-SCENE window result=%d\n",result);
#else
               result = mesa_triangle_probe(handle, physical, device,
                                         anv_GetInstanceProcAddr, report,
#ifdef CUBIT_TEST_PRESENT_TRIANGLE
                                         present_completed_triangle);
            report(DRAW_LOG " window result=%d\n", result);
#else
                                         NULL);
            report(DRAW_LOG " result=%d (offscreen; NOT presented)\n", result);
#endif
#endif
#ifdef CUBIT_TEAPOT_GALLERY
               finish_gallery_window();
#endif
               if (result!=VK_SUCCESS) {
                  report(DRAW_LOG " cycle=%u stopped result=%d (NO replay)\n",
                         cycle+1,result);
                  break;
               }
               /* The synchronous consumer cannot return until Desktop grants
                * are retired; probe cleanup then releases this frame's BOs.
                * Only now may the same device begin another GPU submission. */
               report(DRAW_LOG " cycle=%u retired and cleaned\n",cycle+1);
            }
#endif
#ifdef CUBIT_TEST_TRANSFER
            result = mesa_transfer_probe(handle, physical, device,
                                         anv_GetInstanceProcAddr, report);
            report("MESA-TRANSFER result=%d (NO application drawing)\n", result);
#endif
            report("MESA-DEVICE destroy beginning\n");
            cubit_mesa_device_destroy_retired(&owned);
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
   enum cubit_mesa_retirement retirement = cubit_mesa_launch_finish(&probe);
   report("MESA-DEVICE retirement state=%u (0 retired, 1 pending, 2 unsafe)\n",
          (unsigned)retirement);
   /* The reusable pump does not sleep. This single-purpose demo retains its
    * process while cleanup is pending/unsafe; a service drives the same pump
    * from its event loop. Never exit and discard an uncertain endpoint pin. */
   while (retirement != CUBIT_MESA_RETIRED) {
      usleep(100000);
      if (retirement == CUBIT_MESA_RETIRE_PENDING) {
         enum cubit_mesa_retirement next = cubit_mesa_launch_finish(&probe);
         if (next != retirement)
            report("MESA-DEVICE retirement state=%u\n", (unsigned)next);
         retirement = next;
      }
   }
   report("MESA-DISCOVERY result=%u close=0\n", status);
   /* Retirement does not imply GPU backing reclamation or cap-slot reuse. */
   log_context = NULL;
   return status == 0 ? 0 : 1;
}

#ifdef CUBIT_TEST_SERVICE
#include "native-service-probe.h"
#endif

int main(void)
{
#ifdef CUBIT_TEST_SERVICE
   return (int)cubit_test_run_logged(run_service_probe);
#else
   return (int)cubit_test_run_logged(run_probe);
#endif
}
