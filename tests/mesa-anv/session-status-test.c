#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include <assert.h>
#include <stdio.h>
static uint32_t response;
static unsigned queries;
uint32_t cubit_intel_session_status(uint64_t slot)
{ assert(slot >= 55 && slot <= 60); queries++; return response; }
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
                             int line, const char *message, ...)
{ (void)file; (void)line; (void)message;
  p_atomic_set(&device->_lost.lost, 1); return VK_ERROR_DEVICE_LOST; }
int main(void)
{
   static struct anv_device d[7];
   assert(anv_cubit_check_status(&d[6].vk) == VK_ERROR_DEVICE_LOST);
   assert(queries == 0);
   for (unsigned i=0; i<6; i++) {
      assert(anv_cubit_memory_init(&d[i], 60-i) == VK_SUCCESS);
      response=0;
      assert(anv_cubit_check_status(&d[i].vk) == VK_SUCCESS);
      assert(!vk_device_is_lost_no_report(&d[i].vk));
      response=i+1; /* includes unknown/unexpected service codes */
      assert(anv_cubit_check_status(&d[i].vk) == VK_ERROR_DEVICE_LOST);
      unsigned before=queries;
      response=0;
      assert(anv_cubit_check_status(&d[i].vk) == VK_ERROR_DEVICE_LOST);
      assert(queries == before); /* no implicit recovery/retry */
   }
   puts("ANV session status PASS: live query, denied/unavailable/protocol failures sticky (mock IPC)");
}
