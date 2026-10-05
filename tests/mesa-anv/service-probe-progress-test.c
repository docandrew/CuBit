/* Probe orchestration only: mocked service/draw, no Vulkan execution. */
#include "../../userspace/mesa/service-device.h"
#include <assert.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#define CUBIT_TEST_TRIANGLE 1
#define CUBIT_TEST_TRIANGLE_CYCLES 256
#define DRAW_LOG "MESA-TRIANGLE"
struct cubit_mesa_service { unsigned placeholder; };
static struct cubit_mesa_service service;
static void *log_context;
static unsigned records, health_calls, draws, closes, fail_health, fail_draw;
static unsigned summary_completed, summary_requested;
static int summary_result;
static bool failure_seen;
static uint64_t cubit_test_render_slot(void) { return 24; }
static void report(const char *format, ...)
{
   char text[256];
   va_list args;
   va_start(args, format);
   vsnprintf(text, sizeof(text), format, args);
   va_end(args);
   records++;
   if (strstr(text, "health=-4") || strstr(text, "result=-4")) failure_seen=true;
   (void)sscanf(text, "MESA-TRIANGLE service sustained completed=%u requested=%u result=%d",
                &summary_completed, &summary_requested, &summary_result);
}
VkResult cubit_mesa_service_start(uint64_t slot, struct cubit_mesa_service **out)
{ assert(slot==24 && !*out); *out=&service; return VK_SUCCESS; }
VkBool32 cubit_mesa_service_device(struct cubit_mesa_service *owner,
                                  struct cubit_mesa_service_device *out)
{ assert(owner==&service); memset(out,0,sizeof(*out)); return VK_TRUE; }
VkResult cubit_mesa_service_status(struct cubit_mesa_service *owner)
{ assert(owner==&service); return ++health_calls==fail_health ? VK_ERROR_DEVICE_LOST : VK_SUCCESS; }
enum cubit_mesa_service_retirement cubit_mesa_service_close(struct cubit_mesa_service *owner)
{ assert(owner==&service); closes++; return CUBIT_MESA_SERVICE_RETIRED; }
static VkResult mesa_triangle_probe(VkInstance a, VkPhysicalDevice b, VkDevice c,
   PFN_vkGetInstanceProcAddr d, void (*log)(const char *, ...),
   VkResult (*consume)(VkDevice,VkDeviceMemory,VkDeviceSize,uint32_t,uint32_t,uint32_t))
{
   (void)a; (void)b; (void)c; (void)d; (void)log; assert(!consume);
   return ++draws==fail_draw ? VK_ERROR_DEVICE_LOST : VK_SUCCESS;
}
#include "native-service-probe.h"
int main(void)
{
   for (unsigned scenario=0;scenario<5;scenario++) {
      records=health_calls=draws=closes=0; failure_seen=false;
      summary_completed=summary_requested=0; summary_result=99;
      fail_health=scenario==1 ? 33 : scenario==3 ? 34 : scenario==4 ? 512 : 0;
      fail_draw=scenario==2 ? 17 : 0;
      assert(run_service_probe(&service)==(scenario ? 1 : 0));
      assert(!log_context && closes==1 && summary_requested==256);
      assert(summary_completed==(scenario==4 ? 255 : scenario ? 16 : 256));
      assert(summary_result==(scenario ? VK_ERROR_DEVICE_LOST : VK_SUCCESS));
      assert(failure_seen==(scenario!=0));
      assert(health_calls==((scenario==1 || scenario==2) ? 33 : scenario==3 ? 34 : 512));
      assert(draws==(scenario==1 ? 16 : (scenario==2 || scenario==3) ? 17 : 256));
      assert(records <= 42); /* Excludes logs inside the draw implementation. */
   }
   puts("Service probe progress PASS: bounded checkpoints, exact totals, cleanup loss including final cycle, no replay");
}
