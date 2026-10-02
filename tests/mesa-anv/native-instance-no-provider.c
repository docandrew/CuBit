/* Native CuBit execution regression, not a GPU or Linux-hosted demo.
 * Link the actual ANV implementation. No provider callbacks are installed. */
#include <stdio.h>
#include <stdarg.h>
#include <cubit/debug.h>
#include "anv_entrypoints.h"

/* Mesa's opaque build-ID API. Keep this minimal fixture independent of the
 * driver-private headers; these are the actual linked utility functions. */
struct build_id_note;
extern const struct build_id_note *build_id_find_nhdr_for_addr(const void *);
extern unsigned build_id_length(const struct build_id_note *);
extern const uint8_t *build_id_data(const struct build_id_note *);
#ifdef CUBIT_TEST_SNAPSHOT_PROBE
extern int cubit_test_snapshot_discovery(void);
#endif
#ifdef CUBIT_TEST_STATE_TABLE_PROBE
extern int cubit_test_state_table(void);
#endif
#ifdef CUBIT_TEST_RESERVATION_PROBE
extern int cubit_test_owned_reservations(void);
#endif

/* stdout is a declared CuBit stream, not an implicit serial-console alias.
 * This authority-free test deliberately has no stream receiver. */
static void report(const char *format, ...)
{
   char text[192];
   va_list args;
   va_start(args, format);
   int length = vsnprintf(text, sizeof(text), format, args);
   va_end(args);
   if (length > 0)
      cubit_debug_write(text, (size_t)length < sizeof(text) ?
                        (size_t)length : sizeof(text) - 1);
}

int main(void)
{
   const struct build_id_note *note = build_id_find_nhdr_for_addr(main);
   const struct build_id_note *driver_note =
      build_id_find_nhdr_for_addr(anv_CreateInstance);
   unsigned nonzero = 0;
   if (note && build_id_length(note) == 20) {
      const uint8_t *bytes = build_id_data(note);
      for (unsigned i = 0; i < 20; ++i) nonzero |= bytes[i];
   }
   if (!note || driver_note != note || !nonzero ||
       build_id_find_nhdr_for_addr(NULL) ||
       build_id_find_nhdr_for_addr(&note)) {
      report("TEST: FAIL native Mesa static build-ID lookup\n");
      return 1;
   }
   report("MESA-NATIVE: real static build-ID found bytes=20 (NO GPU)\n");
   const VkApplicationInfo app = {
      .sType = VK_STRUCTURE_TYPE_APPLICATION_INFO,
      .pApplicationName = "CuBit native Mesa no-provider regression",
      .apiVersion = VK_API_VERSION_1_0,
   };
   const VkInstanceCreateInfo create = {
      .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
      .pApplicationInfo = &app,
   };
   /* Fresh instances must work after full destruction; do not mistake a
    * retained, process-global fake device for successful initialization. */
   for (unsigned attempt = 0; attempt < 2; attempt++) {
      VkInstance instance = VK_NULL_HANDLE;
      VkResult result = anv_CreateInstance(&create, NULL, &instance);
      report("MESA-NATIVE: instance %u create=%d\n", attempt, result);
      if (result != VK_SUCCESS || instance == VK_NULL_HANDLE) {
         report("TEST: FAIL native Mesa instance creation\n");
         return 1;
      }
      PFN_vkEnumeratePhysicalDevices enumerate = (PFN_vkEnumeratePhysicalDevices)
         anv_GetInstanceProcAddr(instance, "vkEnumeratePhysicalDevices");
      if (!enumerate) {
         anv_DestroyInstance(instance, NULL);
         report("TEST: FAIL native Mesa enumeration entrypoint\n");
         return 1;
      }
      uint32_t count = 0;
      result = enumerate(instance, &count, NULL);
      report("MESA-NATIVE: instance %u enumerate=%d count=%u\n",
             attempt, result, count);
      anv_DestroyInstance(instance, NULL);
      if (result != VK_ERROR_INITIALIZATION_FAILED || count != 0) {
         report("TEST: FAIL native Mesa missing-provider boundary\n");
         return 1;
      }
      report("MESA-NATIVE: instance %u destroyed\n", attempt);
   }
#ifdef CUBIT_TEST_SNAPSHOT_PROBE
   if (cubit_test_snapshot_discovery()) return 1;
#endif
#ifdef CUBIT_TEST_STATE_TABLE_PROBE
   if (cubit_test_state_table()) return 1;
#endif
#ifdef CUBIT_TEST_RESERVATION_PROBE
   if (cubit_test_owned_reservations()) return 1;
#endif
   report("TEST: PASS native Mesa lifecycle without provider (NO GPU)\n");
   return 0;
}
