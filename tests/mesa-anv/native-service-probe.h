/* Included by the logged native fixture, with the same admitted manifest.
 * Uses the production service bootstrap, never the diagnostic provider. */
#include "../../userspace/mesa/service-device.h"
#include "native-transport-failure.h"

static uint32_t run_service_probe(void *context)
{
   log_context = context;
   struct cubit_mesa_service *owner = NULL;
   report("MESA-SERVICE startup beginning\n");
   VkResult result = cubit_mesa_service_start(cubit_test_render_slot(), &owner);
   report_transport_failure();
   report("MESA-SERVICE startup=%d owner-retained=%u\n", result, owner != NULL);
   struct cubit_mesa_service_device facts;
   if (result == VK_SUCCESS && !cubit_mesa_service_device(owner, &facts))
      result = VK_ERROR_INITIALIZATION_FAILED;
   if (result == VK_SUCCESS) {
      report("MESA-SERVICE queue ready family=%u\n", facts.family);
#ifdef CUBIT_TEST_TRIANGLE
      unsigned completed_cycles = 0;
      for (unsigned cycle=0; cycle<CUBIT_TEST_TRIANGLE_CYCLES; cycle++) {
         const unsigned number = cycle + 1;
         const bool checkpoint = number == 1 || number % 32 == 0 ||
                                 number == CUBIT_TEST_TRIANGLE_CYCLES;
         result = cubit_mesa_service_status(owner);
         report_transport_failure();
         if (checkpoint || result != VK_SUCCESS)
            report("MESA-SERVICE health=%d before cycle=%u\n", result, number);
         if (result != VK_SUCCESS)
            break;
         if (checkpoint)
            report(DRAW_LOG " service cycle=%u/%u beginning\n",
                   number, (unsigned)CUBIT_TEST_TRIANGLE_CYCLES);
#ifdef CUBIT_TEST_SCENE
         result = mesa_scene_probe(facts.instance, facts.physical, facts.device,
            facts.instance_proc, report,
#ifdef CUBIT_TEST_PRESENT_TRIANGLE
            present_completed_triangle,
#else
            NULL,
#endif
            compose_completed_source);
#else
         result = mesa_triangle_probe(facts.instance, facts.physical, facts.device,
            facts.instance_proc, report,
#ifdef CUBIT_TEST_PRESENT_TRIANGLE
            present_completed_triangle);
#else
            NULL);
#endif
#endif
#ifdef CUBIT_TEAPOT_GALLERY
         finish_gallery_window();
#endif
         report_transport_failure();
         if (checkpoint || result != VK_SUCCESS)
            report(DRAW_LOG " service cycle=%u result=%d\n", number, result);
         if (result != VK_SUCCESS)
            break;
         /* Vulkan destruction calls are void: a draw can return success
          * while teardown has marked the device lost. Do not count that
          * cycle as retired, including the final cycle with no next check. */
         result = cubit_mesa_service_status(owner);
         report_transport_failure();
         if (result != VK_SUCCESS) {
            report("MESA-SERVICE cleanup health=%d after cycle=%u\n", result, number);
            break;
         }
         completed_cycles++;
         if (checkpoint)
            report(DRAW_LOG " service cycle=%u retired and cleaned\n", number);
      }
      report(DRAW_LOG " service sustained completed=%u requested=%u result=%d\n",
             completed_cycles, (unsigned)CUBIT_TEST_TRIANGLE_CYCLES, result);
#endif
#ifdef CUBIT_TEST_TRANSFER
      result = mesa_transfer_probe(facts.instance, facts.physical, facts.device,
                                  facts.instance_proc, report);
#endif
   }
   if (owner) {
      /* Probe callbacks return only after all GPU and presentation borrows
       * retire. Uncertainty retains them inside the callback, not here. */
      enum cubit_mesa_service_retirement state = cubit_mesa_service_close(owner);
      report_transport_failure();
      report("MESA-SERVICE retirement=%u\n", (unsigned)state);
      while (state != CUBIT_MESA_SERVICE_RETIRED) {
         usleep(100000);
         if (state == CUBIT_MESA_SERVICE_PENDING) {
            enum cubit_mesa_service_retirement next = cubit_mesa_service_close(owner);
            report_transport_failure();
            if (next != state)
               report("MESA-SERVICE retirement=%u\n", (unsigned)next);
            state = next;
         }
      }
   }
   report("MESA-SERVICE result=%d\n", result);
   log_context = NULL;
   return result == VK_SUCCESS ? 0 : 1;
}
