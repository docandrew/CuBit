/* Synthetic read-only discovery replies, real Mesa common initialization.
 * NO hardware provider, NO logical device, NO GPU submission or capability.
 * This is a native runtime regression, not evidence of working Intel graphics.
 */
#include "anv_cubit_physical.h"
#include "anv_entrypoints.h"
#include "vk_common_entrypoints.h"
#include <cubit/debug.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

int cubit_test_snapshot_discovery(void);
void cubit_test_mesa_init_stage(const char *, int, int);

static unsigned references, queries, unexpected_opens;
static bool bad_lifetime;
static struct anv_memory_budget budget;

static void log_message(const char *format, ...)
{
   char text[192];
   va_list args;
   va_start(args, format);
   int n = vsnprintf(text, sizeof(text), format, args);
   va_end(args);
   if (n > 0) cubit_debug_write(text, (size_t)n < sizeof(text) ?
                               (size_t)n : sizeof(text) - 1);
}

void cubit_test_mesa_init_stage(const char *stage, int returned, int result)
{
   log_message("MESA-SNAPSHOT INIT %s %s result=%d\n", stage,
               returned ? "returned" : "begin", result);
}

/* Assemble the same core tables used by ADL-N device initialization, without
 * opening a device. This checks actual weak dispatch pointers, not merely the
 * presence of a function symbol. It does NOT execute these GPU commands.
 */
static bool check_triangle_dispatch(void)
{
   struct vk_device_dispatch_table table = {0};
   vk_device_dispatch_table_from_entrypoints(&table, &gfx12_device_entrypoints, true);
   vk_device_dispatch_table_from_entrypoints(&table, &anv_device_entrypoints, false);
   vk_device_dispatch_table_from_entrypoints(&table, &vk_common_device_entrypoints, false);
   unsigned checked = 0, missing = 0;
#define CHECK(name) do { \
   if (!table.name) { \
      log_message("MESA-SNAPSHOT missing dispatch %s (NO GPU)\n", #name); \
      ++missing; \
   } \
   ++checked; \
} while (0)
   CHECK(CreateImage); CHECK(DestroyImage); CHECK(GetImageMemoryRequirements);
   CHECK(AllocateMemory); CHECK(FreeMemory); CHECK(BindImageMemory);
   CHECK(CreateBuffer); CHECK(DestroyBuffer); CHECK(GetBufferMemoryRequirements);
   CHECK(BindBufferMemory); CHECK(MapMemory); CHECK(UnmapMemory);
   CHECK(CreateImageView); CHECK(DestroyImageView);
   CHECK(CreateRenderPass); CHECK(DestroyRenderPass);
   CHECK(CreateFramebuffer); CHECK(DestroyFramebuffer);
   CHECK(CreateShaderModule); CHECK(DestroyShaderModule);
   CHECK(CreatePipelineLayout); CHECK(DestroyPipelineLayout);
   CHECK(CreateGraphicsPipelines); CHECK(DestroyPipeline);
   CHECK(CreateCommandPool); CHECK(DestroyCommandPool); CHECK(AllocateCommandBuffers);
   CHECK(BeginCommandBuffer); CHECK(EndCommandBuffer);
   CHECK(CmdBeginRenderPass); CHECK(CmdEndRenderPass); CHECK(CmdBindPipeline); CHECK(CmdDraw);
   CHECK(CmdPipelineBarrier); CHECK(CmdCopyImageToBuffer);
   CHECK(CreateFence); CHECK(DestroyFence); CHECK(GetDeviceQueue);
   CHECK(QueueSubmit); CHECK(WaitForFences); CHECK(DeviceWaitIdle);
   /* QueueSubmit translates to QueueSubmit2, an internal dispatch dependency. */
   CHECK(QueueSubmit2);
   CHECK(CmdCopyImageToBuffer2);
#undef CHECK
   if (missing) {
      log_message("MESA-SNAPSHOT dispatch checked=%u missing=%u (NO GPU)\n", checked, missing);
      return false;
   }
   log_message("MESA-SNAPSHOT dispatch entries=%u present (NOT executed; NO GPU)\n", checked);
   return true;
}

/* Query real native Mesa capability policy, never an import or device open.
 * A CPU presentation grant must not accidentally select Linux FD/dma-buf
 * handling merely because the generic external-memory vocabulary exists. */
static bool check_external_policy(VkInstance instance, VkPhysicalDevice physical)
{
   PFN_vkEnumerateDeviceExtensionProperties extensions = (void *)
      anv_GetInstanceProcAddr(instance, "vkEnumerateDeviceExtensionProperties");
   PFN_vkGetPhysicalDeviceExternalBufferProperties buffer = (void *)
      anv_GetInstanceProcAddr(instance, "vkGetPhysicalDeviceExternalBufferProperties");
   PFN_vkGetPhysicalDeviceImageFormatProperties2 image = (void *)
      anv_GetInstanceProcAddr(instance, "vkGetPhysicalDeviceImageFormatProperties2");
   if (!extensions || !buffer || !image) return false;
   static VkExtensionProperties properties[512];
   uint32_t count = 512;
   if (extensions(physical, NULL, &count, properties) != VK_SUCCESS || count > 512)
      return false;
   const char *forbidden[] = {
      "VK_KHR_external_memory_fd", "VK_KHR_external_fence_fd",
      "VK_KHR_external_semaphore_fd", "VK_EXT_external_memory_dma_buf",
      "VK_EXT_external_memory_host", "VK_EXT_external_memory_acquire_unmodified",
      "VK_EXT_image_drm_format_modifier", "VK_EXT_physical_device_drm",
   };
   for (unsigned i = 0; i < count; i++)
      for (unsigned j = 0; j < sizeof(forbidden) / sizeof(forbidden[0]); j++)
         if (!strcmp(properties[i].extensionName, forbidden[j])) {
            log_message("MESA-SNAPSHOT unexpected extension %s\n", forbidden[j]);
            return false;
         }
   const VkExternalMemoryHandleTypeFlagBits types[] = {
      VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT,
      VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT,
      VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT,
   };
   for (unsigned i = 0; i < sizeof(types) / sizeof(types[0]); i++) {
      const VkPhysicalDeviceExternalBufferInfo input = {
         .sType=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO,
         .usage=VK_BUFFER_USAGE_TRANSFER_SRC_BIT, .handleType=types[i],
      };
      VkExternalBufferProperties output = {
         .sType=VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES,
         .externalMemoryProperties={.externalMemoryFeatures=~0u, .exportFromImportedHandleTypes=~0u},
      };
      buffer(physical, &input, &output);
      if (output.externalMemoryProperties.externalMemoryFeatures ||
          output.externalMemoryProperties.exportFromImportedHandleTypes)
         return false;
      const VkPhysicalDeviceExternalImageFormatInfo external = {
         .sType=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO,
         .handleType=types[i],
      };
      VkPhysicalDeviceImageFormatInfo2 format = {
         .sType=VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2,
         .pNext=&external, .format=VK_FORMAT_B8G8R8A8_UNORM,
         .type=VK_IMAGE_TYPE_2D, .tiling=VK_IMAGE_TILING_OPTIMAL,
         .usage=VK_IMAGE_USAGE_SAMPLED_BIT,
      };
      VkImageFormatProperties2 result = {.sType=VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2};
      if (image(physical, &format, &result) != VK_ERROR_FORMAT_NOT_SUPPORTED)
         return false;
      /* Positive control: ordinary private images remain supported. */
      format.pNext = NULL;
      if (image(physical, &format, &result) != VK_SUCCESS ||
          !result.imageFormatProperties.maxExtent.width)
         return false;
   }
   log_message("MESA-SNAPSHOT external policy: 8 extensions absent, 3 handle types rejected, private images supported (NO GPU)\n");
   return true;
}

static bool retain(void *context)
{
   if (context != &budget || references >= 8) return false;
   ++references;
   return true;
}
static void release(void *context)
{
   if (context != &budget || !references) bad_lifetime = true;
   else --references;
}
static VkResult deny_open(void *context, struct anv_device *device)
{
   (void)context; (void)device;
   ++unexpected_opens;
   return VK_ERROR_INITIALIZATION_FAILED;
}
static bool query(void *context, const struct cubit_gpu_query_message *request,
                  struct cubit_gpu_query_message *reply)
{
   if (context != &budget || !references || ++queries > 32 ||
       request->length != 4 || request->flags || request->reserved ||
       request->words[0] != (request->label == 0xa2e ? 2 : 1) ||
       request->words[2] || request->words[3])
      return false;
   *reply = (struct cubit_gpu_query_message){ .label = request->label, .length = 4 };
   if (request->label == 0xa2e && request->words[1] == 0) {
      reply->words[1] = 0x2000000;
      reply->words[2] = 0x8b000;
      reply->words[3] = 12;
   } else if (request->label == 0xa20) {
      reply->words[1] = 1;
      switch (request->words[1]) {
      case 0: reply->words[2] = 0x46d28086; break;
      case 1: reply->words[2] = 1; reply->words[3] = 0xffff; break;
      case 2: reply->words[2] = 19200000; break;
      case 3: reply->words[2] = 2; break;
      case 4: reply->words[2] = 48; reply->words[3] = 1; break;
      default: return false;
      }
   } else return false;
   log_message("MESA-SNAPSHOT query=%u label=%x selector=%llu (SYNTHETIC)\n",
               queries, (unsigned)request->label,
               (unsigned long long)request->words[1]);
   return true;
}

int cubit_test_snapshot_discovery(void)
{
   if (!check_triangle_dispatch()) return 1;
   const VkApplicationInfo app = {
      .sType=VK_STRUCTURE_TYPE_APPLICATION_INFO, .apiVersion=VK_API_VERSION_1_1,
   };
   const VkInstanceCreateInfo create = {
      .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO, .pApplicationInfo=&app,
   };
   VkInstance handle = VK_NULL_HANDLE;
   VkResult result = anv_CreateInstance(&create, NULL, &handle);
   log_message("MESA-SNAPSHOT create=%d (NO GPU)\n", result);
   if (result != VK_SUCCESS) return 1;
   ANV_FROM_HANDLE(anv_instance, instance, handle);
   const struct anv_cubit_provider provider = {
      .context = &budget, .retain = retain, .release = release,
      .query = query, .open_device = deny_open, .budget = &budget,
   };
   result = anv_cubit_install_discovery(instance, &provider, 1);
   uint32_t count = 0;
   if (result == VK_SUCCESS) {
      PFN_vkEnumeratePhysicalDevices enumerate = (PFN_vkEnumeratePhysicalDevices)
         anv_GetInstanceProcAddr(handle, "vkEnumeratePhysicalDevices");
      result = enumerate ? enumerate(handle, &count, NULL) : VK_ERROR_INITIALIZATION_FAILED;
      if (result == VK_SUCCESS && count == 1) {
         VkPhysicalDevice physical = VK_NULL_HANDLE;
         result = enumerate(handle, &count, &physical);
         if (result == VK_SUCCESS && (!physical || !check_external_policy(handle, physical)))
            result = VK_ERROR_INITIALIZATION_FAILED;
      }
   }
   log_message("MESA-SNAPSHOT enumerate=%d count=%u queries=%u (NO GPU)\n",
               result, count, queries);
   anv_DestroyInstance(handle, NULL);
   if (result != VK_SUCCESS || count != 1 || references || bad_lifetime ||
       unexpected_opens || queries < 7) {
      log_message("TEST: FAIL native Mesa synthetic snapshot (NO GPU)\n");
      return 1;
   }
   log_message("TEST: PASS native Mesa synthetic snapshot (NO GPU)\n");
   return 0;
}
