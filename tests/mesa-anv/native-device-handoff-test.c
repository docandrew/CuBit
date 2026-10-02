/* Hosted callback ownership test; no native transport or Vulkan execution. */
int native_probe_main(void);
#define CUBIT_TEST_LOGICAL_DEVICE 1
#define main native_probe_main
#include "native-authorized-discovery.c"
#undef main
#include <assert.h>

static unsigned attach_calls;
static bool take_pin;
static VkResult attach_result;
static struct anv_cubit_endpoint_pin saved;
static char captured[192];
static unsigned log_calls;

void cubit_debug_write(const char *text, size_t length)
{ (void)text; (void)length; }
void cubit_test_log(void *context, const char *text, uint32_t length)
{
   assert(context == &log_calls && length > 0 && length < sizeof(captured));
   assert(!memchr(text, '\n', length));
   memcpy(captured, text, length);
   captured[length] = 0;
   log_calls++;
}

VkResult anv_cubit_attach_owned_session(struct anv_device *device, uint64_t slot,
                                      struct anv_cubit_endpoint_pin *pin)
{
   assert(device && slot == 24 && pin && pin->retired);
   attach_calls++;
   if (take_pin) {
      saved = *pin;
      *pin = (struct anv_cubit_endpoint_pin){0};
   }
   return attach_result;
}

int main(void)
{
   static struct anv_device device;
   log_context = &log_calls;
   report("bounded report %u\n", 7u);
   assert(log_calls == 1 && !strcmp(captured, "bounded report 7"));
   report("%0250u", 1u);
   assert(log_calls == 2 && strlen(captured) == 191);
   log_context = NULL;
   report("debug-only\n");
   assert(log_calls == 2);
   for (unsigned mode = 0; mode < 3; mode++) {
      struct discovery_probe owner = {.endpoint = {.slot = 24}};
      take_pin = mode != 0;
      attach_result = mode == 2 ? VK_SUCCESS : VK_ERROR_DEVICE_LOST;
      const unsigned before = attach_calls;
      assert(open_logical_device(&owner, &device) == attach_result);
      assert(owner.session_claimed);
      assert(owner.session_transferred == take_pin);
      assert(attach_calls == before + 1 && owner.retired == 0);
      /* Failure before transfer, failure after transfer and success all
       * consume the one opening attempt. No replay of uncertain admission. */
      assert(open_logical_device(&owner, &device) == VK_ERROR_INITIALIZATION_FAILED);
      assert(attach_calls == before + 1);
      if (take_pin) {
         assert(saved.context == &owner && saved.retired == session_retired);
         saved.retired(saved.context);
         assert(p_atomic_read(&owner.retired) == 1);
         assert(open_logical_device(&owner, &device) == VK_ERROR_INITIALIZATION_FAILED);
         assert(attach_calls == before + 1);
      }
   }
   puts("Native-device fixture handoff PASS: three ownership outcomes; no session reuse (hosted)");
   return 0;
}
