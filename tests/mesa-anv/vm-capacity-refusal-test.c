/* BUG-001: a live VM bind the driver refuses before any change (its table
 * ceiling, the session's byte budget or allocation records) fails only that
 * allocation. The device is not lost, the VM generation is unchanged, the
 * next bind and later submissions work. Any other refused bind, and any
 * refused unbind, still loses the device: the VM can no longer be trusted.
 * Actual ANV types and adapter, real vk_sync, mock queue and IPC. */
#include "queue-rig.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include "../../userspace/mesa/anv/native_gpu_mapping.h"
#include <stdio.h>
#include "../../userspace/c/cubit_handles.h"
/* A test endpoint handle (generation 1). */
#define TEST_ENDPOINT(n) ((cubit_endpoint){(UINT64_C(1) << 32) | (uint64_t)(n)})
/* The index a test endpoint was made from. */
#define TEST_INDEX(e) ((unsigned)((e).opaque & 0xffffffffu))

uint32_t cubit_intel_memory_contract(cubit_endpoint endpoint) { (void)endpoint; return 1; }
uint32_t cubit_intel_session_status(cubit_endpoint endpoint) { (void)endpoint; return 0; }
bool cubit_cpu_tracker_drain(struct cubit_cpu_mapping_tracker *tracker)
{ (void)tracker; return true; }
uint32_t cubit_intel_close_session(cubit_endpoint endpoint, uint64_t *tag) { *tag = TEST_INDEX(endpoint) + 1; return 0; }
uint32_t cubit_intel_poll_session_retirement(cubit_endpoint endpoint) { (void)endpoint; return 0; }
uint32_t cubit_intel_prepare_context(cubit_endpoint endpoint) { (void)endpoint; return 0; }
uint32_t cubit_intel_register_context(cubit_endpoint endpoint) { (void)endpoint; return 0; }
uint32_t cubit_intel_close_buffer(cubit_endpoint endpoint, uint32_t handle)
{ (void)endpoint; (void)handle; return 0; }
void anv_cmd_buffer_chain_command_buffers(struct anv_cmd_buffer **c, uint32_t n) { (void)c; (void)n; }
void anv_cmd_buffer_clflush(struct anv_cmd_buffer **c, uint32_t n) { (void)c; (void)n; }
void util_flush_range(void *start, size_t size) { (void)start; (void)size; }
uint32_t cubit_cpu_mapping_release(struct cubit_cpu_mapping *record, bool replace)
{ (void)record; (void)replace; return 0; }
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
   int line, const char *message, ...)
{
   (void)file; (void)line; (void)message;
   p_atomic_set(&device->_lost.lost, 1);
   return VK_ERROR_DEVICE_LOST;
}

/* The driver: the status its next update answers, and its generations. */
static uint32_t next_status;
static uint32_t generations[64], updates;
uint32_t cubit_intel_update_binding(cubit_endpoint endpoint, uint32_t handle, uint64_t gpu,
   uint64_t offset, uint64_t bytes, uint32_t remove, uint32_t previous, uint32_t *generation)
{
   (void)handle; (void)gpu; (void)offset; (void)bytes; (void)remove;
   updates++;
   *generation = 0;
   assert(previous == generations[TEST_INDEX(endpoint)]);
   if (next_status) {
      const uint32_t status = next_status;
      next_status = 0;
      return status;
   }
   *generation = ++generations[TEST_INDEX(endpoint)];
   return 0;
}

static unsigned checks;
static void check(bool condition) { assert(condition); checks++; }

/* One frame through the session queue: the device still submits. */
static void frame(struct rig *r, struct vk_sync *fence)
{
   check(vk_sync_reset(&r->device.vk, fence) == VK_SUCCESS);
   const struct vk_sync_signal s = {.sync = fence};
   check(submit(r, 1, 0, NULL, 1, &s) == VK_SUCCESS);
   gpu_queue_mock_complete(r->endpoint, 0, gpu_queue_mock[TEST_INDEX(r->endpoint)].submitted.value[0]);
   check(vk_sync_wait(&r->device.vk, fence, 0, 0, UINT64_MAX) == VK_SUCCESS);
}

int main(void)
{
   rig_physical();
   static struct rig r, other, unbinding;
   static struct anv_bo surface = {.gem_handle = 21, .actual_size = 4 * 1024 * 1024};
   open_rig(&r, TEST_ENDPOINT(11));
   struct vk_sync *fence = make(&r, &binary_type.sync, 0);
   check(startup(&r, fence, 0, NULL) == VK_SUCCESS);
   check(anv_cubit_update_bo_binding(&r.device, &surface, 0x80000000, 0,
                                     surface.actual_size, false) == VK_SUCCESS);
   check(generations[11] == 1);

   /* The window surface the desktop could not map (laptop-15): refused
    * for capacity, nothing changed. Only this allocation fails. */
   next_status = CUBIT_INTEL_UPDATE_CAPACITY;
   check(anv_cubit_update_bo_binding(&r.device, &surface, 0x80400000, 0,
                                     surface.actual_size, false) == VK_ERROR_OUT_OF_DEVICE_MEMORY);
   check(!vk_device_is_lost_no_report(&r.device.vk));
   check(generations[11] == 1);
   frame(&r, fence);
   /* The next bind carries the unchanged generation and commits. */
   check(anv_cubit_update_bo_binding(&r.device, &surface, 0x80400000, 0,
                                     surface.actual_size, false) == VK_SUCCESS);
   check(generations[11] == 2 && !vk_device_is_lost_no_report(&r.device.vk));
   frame(&r, fence);

   /* Unavailable is not a promise that nothing changed: still fatal. */
   open_rig(&other, TEST_ENDPOINT(12));
   struct vk_sync *other_fence = make(&other, &binary_type.sync, 0);
   check(startup(&other, other_fence, 0, NULL) == VK_SUCCESS);
   next_status = 3;
   check(anv_cubit_update_bo_binding(&other.device, &surface, 0x80000000, 0,
                                     surface.actual_size, false) == VK_ERROR_DEVICE_LOST);
   check(vk_device_is_lost_no_report(&other.device.vk));

   /* A refused unbind is never a capacity refusal: still fatal. */
   open_rig(&unbinding, TEST_ENDPOINT(13));
   struct vk_sync *unbinding_fence = make(&unbinding, &binary_type.sync, 0);
   check(startup(&unbinding, unbinding_fence, 0, NULL) == VK_SUCCESS);
   next_status = CUBIT_INTEL_UPDATE_CAPACITY;
   check(anv_cubit_update_bo_binding(&unbinding.device, &surface, 0x80000000, 0,
                                     surface.actual_size, true) == VK_ERROR_DEVICE_LOST);
   check(vk_device_is_lost_no_report(&unbinding.device.vk));
   check(updates == 5);

   close_rig(&r);
   close_rig(&other);
   close_rig(&unbinding);
   printf("ANV VM capacity refusal PASS: %u checks; a bind refused for capacity fails only "
          "that allocation, the device and generation survive; other refusals stay fatal "
          "(mock queue/IPC)\n", checks);
   return 0;
}
