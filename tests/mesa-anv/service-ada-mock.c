#include "../../userspace/mesa/service-device.h"
#include <stddef.h>
#include <assert.h>
#include <string.h>

_Static_assert(sizeof(struct cubit_mesa_service_device) == 48, "Ada view size");
_Static_assert(_Alignof(struct cubit_mesa_service_device) == 8, "Ada alignment");
_Static_assert(offsetof(struct cubit_mesa_service_device, instance) == 0, "instance");
_Static_assert(offsetof(struct cubit_mesa_service_device, physical) == 8, "physical");
_Static_assert(offsetof(struct cubit_mesa_service_device, device) == 16, "device");
_Static_assert(offsetof(struct cubit_mesa_service_device, queue) == 24, "queue");
_Static_assert(offsetof(struct cubit_mesa_service_device, family) == 32, "family");
_Static_assert(offsetof(struct cubit_mesa_service_device, instance_proc) == 40, "proc");
_Static_assert(sizeof(VkResult) == 4, "result");
_Static_assert(sizeof(enum cubit_mesa_service_retirement) == 4, "retirement");

static unsigned mode, starts, borrows, closes;
static struct cubit_mesa_service *owner = (void *)(uintptr_t)0x1234;
void fixture_reset(unsigned value) { mode=value; starts=borrows=closes=0; }
unsigned fixture_starts(void) { return starts; }
unsigned fixture_borrows(void) { return borrows; }
unsigned fixture_closes(void) { return closes; }
static PFN_vkVoidFunction proc(VkInstance instance, const char *name)
{ (void)instance; (void)name; return NULL; }
VkResult cubit_mesa_service_start(uint64_t slot, struct cubit_mesa_service **out)
{
    assert(slot == 7 && *out == NULL); starts++;
    if (mode == 1) return VK_ERROR_INITIALIZATION_FAILED;
    *out = owner;
    return mode == 2 ? VK_ERROR_OUT_OF_DEVICE_MEMORY : VK_SUCCESS;
}
VkBool32 cubit_mesa_service_device(struct cubit_mesa_service *value,
                                  struct cubit_mesa_service_device *out)
{
    assert(value == owner); borrows++;
    *out = (struct cubit_mesa_service_device){
       (void *)(uintptr_t)11, (void *)(uintptr_t)22, (void *)(uintptr_t)33,
       (void *)(uintptr_t)44, 55, proc};
    /* Deliberately leave stale output on rejection: Ada must discard it. */
    return mode == 2 ? VK_FALSE : VK_TRUE;
}
VkResult cubit_mesa_service_status(struct cubit_mesa_service *value)
{ assert(value == owner); return VK_ERROR_DEVICE_LOST; }
enum cubit_mesa_service_retirement cubit_mesa_service_close(struct cubit_mesa_service *value)
{
    assert(value == owner); closes++;
    if (mode == 2) return (enum cubit_mesa_service_retirement)99;
    return closes == 1 ? CUBIT_MESA_SERVICE_PENDING : CUBIT_MESA_SERVICE_RETIRED;
}
