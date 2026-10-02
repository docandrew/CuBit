/* Test-only synchronous Desktop consumer. Not Mesa WSI or a public export API.
 * Included after report() and the render-slot declaration in native test glue.
 * Vulkan objects remain live in mesa_triangle_probe until this returns. */
#include "native_gpu_presenter.h"
#include "native_gpu_mapping.h"
extern uint64_t cubit_test_desktop_slot(void);
extern uint64_t cubit_test_triangle_create(void);
extern uint32_t cubit_test_triangle_present(uint64_t surface);
extern uint32_t cubit_test_triangle_destroy(uint64_t surface);

static VkResult triangle_export_rejected(const char *reason, VkResult result)
{
   report("MESA-TRIANGLE export rejected: %s\n",reason);
   return result;
}

static VkResult
present_completed_triangle(VkDevice handle, VkDeviceMemory allocation,
                           VkDeviceSize bytes, uint32_t width,
                           uint32_t height, uint32_t pitch)
{
   ANV_FROM_HANDLE(anv_device, device, handle);
   ANV_FROM_HANDLE(anv_device_memory, memory, allocation);
   if (!device || !memory || bytes!=16384 || width!=64 || height!=64 || pitch!=256)
      return triangle_export_rejected("invalid completed-buffer shape",VK_ERROR_INITIALIZATION_FAILED);
   if (anv_cubit_check_status(&device->vk)!=VK_SUCCESS)
      return triangle_export_rejected("device unhealthy after unmap",VK_ERROR_DEVICE_LOST);
   if (memory->map)
      return triangle_export_rejected("Vulkan CPU mapping still live",VK_ERROR_INITIALIZATION_FAILED);
   if (!device->cubit_cpu_mappings || device->cubit_cpu_mappings->lost)
      return triangle_export_rejected("CPU grant tracker unavailable/lost",VK_ERROR_DEVICE_LOST);
   if (device->cubit_cpu_mappings->slot!=cubit_test_render_slot())
      return triangle_export_rejected("render session mismatch",VK_ERROR_INITIALIZATION_FAILED);
   if (memory->vk.size<bytes || !memory->bo)
      return triangle_export_rejected("allocation backing too small/missing",VK_ERROR_INITIALIZATION_FAILED);
   struct anv_bo *bo=memory->bo;
   /* This fixture owns a standalone host-visible allocation, never an import,
    * slab, internally mapped BO or shared padding belonging to another object. */
   if (bo->slab_parent || bo->from_host_ptr || anv_bo_is_external(bo) ||
       bo->map || !bo->gem_handle || bo->size<bytes || bo->actual_size<bytes)
      return triangle_export_rejected("BO not standalone unmapped owned backing",VK_ERROR_FEATURE_NOT_PRESENT);
   uint64_t surface=cubit_test_triangle_create();
   if (!surface) return triangle_export_rejected("Desktop surface creation failed",VK_ERROR_INITIALIZATION_FAILED);
   struct cubit_presenter presenter={0};
   uint32_t attached=cubit_presenter_attach_completed_linear(&presenter,
      cubit_test_render_slot(),cubit_test_desktop_slot(),bo->gem_handle,
      surface,0,width,height,pitch);
   VkResult result=VK_ERROR_UNKNOWN;
   if (!attached && !cubit_test_triangle_present(surface)) {
      report("MESA-TRIANGLE Desktop presented (retaining Vulkan allocation)\n");
      usleep(5000000);
      result=VK_SUCCESS;
   } else {
      report("MESA-TRIANGLE Desktop attachment/present failed; retiring\n");
   }
   /* Send destroy once on every attachment outcome. An ambiguous reply is
    * not permission to free; the independently tracked grants decide that. */
   if (cubit_test_triangle_destroy(surface)) result=VK_ERROR_UNKNOWN;
   uint32_t retired=cubit_presenter_release(&presenter);
   if (retired) report("MESA-TRIANGLE Desktop retirement pending/uncertain; retained\n");
   while (retired) {
      usleep(100000);
      /* FAILED is unrecoverable: preserve the complete Vulkan allocation and
       * endpoint by never returning. Do not replay an uncertain operation. */
      if (retired==4) retired=cubit_presenter_release(&presenter);
   }
   report("MESA-TRIANGLE Desktop grants retired; Vulkan cleanup permitted\n");
   return result;
}
