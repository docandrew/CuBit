/* CuBit.UI.GPU_Canvas Vulkan glue (see gpu_canvas_vk.h). */
#include "gpu_canvas_vk.h"
#include <stddef.h>

/* Explicit bound on a wait for an outstanding copy before freeing a slot
 * (resize, close). Nanoseconds; a copy of one window-sized image. */
#define GPU_CANVAS_RETIRE_TIMEOUT_NS 2000000000ull
#define GPU_CANVAS_NO_MEMORY_TYPE UINT32_MAX
#define GPU_CANVAS_MEMORY_TYPE_LIMIT 32u
#define GPU_CANVAS_MAX_EXTENT 16384u

struct slot {
   VkImage image;
   VkDeviceMemory image_memory;
   VkImageView view;
   VkSemaphore ready;
   VkBuffer buffer;
   VkDeviceMemory buffer_memory;
   VkDeviceSize bytes;
   void *mapped;
   int coherent;
   VkCommandBuffer copy;
   VkFence fence;
   int submitted;   /* a copy was submitted and not yet observed finished */
   int live;
};

static struct {
   int open;
   VkDevice device;
   VkQueue queue;
   uint32_t family;
   VkPhysicalDeviceMemoryProperties memory;
   VkCommandPool pool;
   struct slot slots[GPU_CANVAS_TARGETS];
#define FN(name) PFN_vk##name name
   FN(CreateImage); FN(DestroyImage); FN(GetImageMemoryRequirements);
   FN(AllocateMemory); FN(FreeMemory); FN(BindImageMemory); FN(BindBufferMemory);
   FN(CreateImageView); FN(DestroyImageView); FN(CreateSemaphore); FN(DestroySemaphore);
   FN(CreateBuffer); FN(DestroyBuffer); FN(GetBufferMemoryRequirements);
   FN(MapMemory); FN(UnmapMemory); FN(InvalidateMappedMemoryRanges);
   FN(CreateCommandPool); FN(DestroyCommandPool); FN(AllocateCommandBuffers);
   FN(FreeCommandBuffers); FN(BeginCommandBuffer); FN(EndCommandBuffer);
   FN(CmdPipelineBarrier); FN(CmdCopyImageToBuffer); FN(CreateFence); FN(DestroyFence);
   FN(ResetFences); FN(GetFenceStatus); FN(WaitForFences); FN(QueueSubmit); FN(QueueWaitIdle);
#undef FN
} vk;

static uint32_t memory_type(uint32_t bits, VkMemoryPropertyFlags flags)
{
   for (uint32_t n = 0; n < vk.memory.memoryTypeCount && n < GPU_CANVAS_MEMORY_TYPE_LIMIT; n++)
      if ((bits & (1u << n)) && (vk.memory.memoryTypes[n].propertyFlags & flags) == flags)
         return n;
   return GPU_CANVAS_NO_MEMORY_TYPE;
}

uint32_t gpu_canvas_vk_open(const struct gpu_canvas_device *d)
{
   if (vk.open || !d || !d->device || !d->queue || !d->instance_proc)
      return GPU_CANVAS_BAD_ARGUMENT;
   PFN_vkGetDeviceProcAddr device_proc =
      (PFN_vkGetDeviceProcAddr)d->instance_proc(d->instance, "vkGetDeviceProcAddr");
   PFN_vkGetPhysicalDeviceMemoryProperties memory_props =
      (PFN_vkGetPhysicalDeviceMemoryProperties)
         d->instance_proc(d->instance, "vkGetPhysicalDeviceMemoryProperties");
   if (!device_proc || !memory_props)
      return GPU_CANVAS_FAILED;
#define LOAD(name) \
   vk.name = (PFN_vk##name)device_proc(d->device, "vk" #name); \
   if (!vk.name) return GPU_CANVAS_FAILED
   LOAD(CreateImage); LOAD(DestroyImage); LOAD(GetImageMemoryRequirements);
   LOAD(AllocateMemory); LOAD(FreeMemory); LOAD(BindImageMemory); LOAD(BindBufferMemory);
   LOAD(CreateImageView); LOAD(DestroyImageView); LOAD(CreateSemaphore); LOAD(DestroySemaphore);
   LOAD(CreateBuffer); LOAD(DestroyBuffer); LOAD(GetBufferMemoryRequirements);
   LOAD(MapMemory); LOAD(UnmapMemory); LOAD(InvalidateMappedMemoryRanges);
   LOAD(CreateCommandPool); LOAD(DestroyCommandPool); LOAD(AllocateCommandBuffers);
   LOAD(FreeCommandBuffers); LOAD(BeginCommandBuffer); LOAD(EndCommandBuffer);
   LOAD(CmdPipelineBarrier); LOAD(CmdCopyImageToBuffer); LOAD(CreateFence); LOAD(DestroyFence);
   LOAD(ResetFences); LOAD(GetFenceStatus); LOAD(WaitForFences); LOAD(QueueSubmit); LOAD(QueueWaitIdle);
#undef LOAD
   memory_props(d->physical, &vk.memory);
   vk.device = d->device;
   vk.queue = d->queue;
   vk.family = d->family;
   const VkCommandPoolCreateInfo pool = {
      .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
      .queueFamilyIndex = d->family,
   };
   if (vk.CreateCommandPool(vk.device, &pool, NULL, &vk.pool) != VK_SUCCESS)
      return GPU_CANVAS_FAILED;
   vk.open = 1;
   return GPU_CANVAS_OK;
}

static int record_copy(struct slot *s, uint32_t width, uint32_t height)
{
   const VkCommandBufferBeginInfo begin = {
      .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
   };
   if (vk.BeginCommandBuffer(s->copy, &begin) != VK_SUCCESS)
      return 0;
   const VkBufferImageCopy region = {
      .bufferRowLength = width,
      .bufferImageHeight = height,
      .imageSubresource = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1 },
      .imageExtent = { width, height, 1 },
   };
   /* The application's render pass leaves the image TRANSFER_SRC_OPTIMAL and
    * its semaphore orders this copy after that work. */
   vk.CmdCopyImageToBuffer(s->copy, s->image, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                           s->buffer, 1, &region);
   const VkBufferMemoryBarrier host = {
      .sType = VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
      .srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT,
      .dstAccessMask = VK_ACCESS_HOST_READ_BIT,
      .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
      .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
      .buffer = s->buffer,
      .size = VK_WHOLE_SIZE,
   };
   vk.CmdPipelineBarrier(s->copy, VK_PIPELINE_STAGE_TRANSFER_BIT, VK_PIPELINE_STAGE_HOST_BIT,
                         0, 0, NULL, 1, &host, 0, NULL);
   return vk.EndCommandBuffer(s->copy) == VK_SUCCESS;
}

uint32_t gpu_canvas_vk_create(uint32_t index, uint32_t width, uint32_t height,
                              struct gpu_canvas_target *target)
{
   if (!vk.open || index >= GPU_CANVAS_TARGETS || !target || width == 0 || height == 0 ||
       width > GPU_CANVAS_MAX_EXTENT || height > GPU_CANVAS_MAX_EXTENT ||
       vk.slots[index].live)
      return GPU_CANVAS_BAD_ARGUMENT;
   struct slot *s = &vk.slots[index];
   *s = (struct slot){0};
   s->live = 1; /* destroy releases whatever was created on failure */
   const VkImageCreateInfo image = {
      .sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,
      .imageType = VK_IMAGE_TYPE_2D,
      .format = GPU_CANVAS_FORMAT,
      .extent = { width, height, 1 },
      .mipLevels = 1,
      .arrayLayers = 1,
      .samples = VK_SAMPLE_COUNT_1_BIT,
      .tiling = VK_IMAGE_TILING_OPTIMAL,
      .usage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT,
      .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
      .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED,
   };
   if (vk.CreateImage(vk.device, &image, NULL, &s->image) != VK_SUCCESS)
      goto fail;
   VkMemoryRequirements need;
   vk.GetImageMemoryRequirements(vk.device, s->image, &need);
   uint32_t type = memory_type(need.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
   if (type == GPU_CANVAS_NO_MEMORY_TYPE)
      type = memory_type(need.memoryTypeBits, 0);
   VkMemoryAllocateInfo allocate = {
      .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
      .allocationSize = need.size,
      .memoryTypeIndex = type,
   };
   if (type == GPU_CANVAS_NO_MEMORY_TYPE ||
       vk.AllocateMemory(vk.device, &allocate, NULL, &s->image_memory) != VK_SUCCESS ||
       vk.BindImageMemory(vk.device, s->image, s->image_memory, 0) != VK_SUCCESS)
      goto fail;
   const VkImageViewCreateInfo view = {
      .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
      .image = s->image,
      .viewType = VK_IMAGE_VIEW_TYPE_2D,
      .format = GPU_CANVAS_FORMAT,
      .subresourceRange = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1 },
   };
   if (vk.CreateImageView(vk.device, &view, NULL, &s->view) != VK_SUCCESS)
      goto fail;
   const VkSemaphoreCreateInfo semaphore = { .sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO };
   if (vk.CreateSemaphore(vk.device, &semaphore, NULL, &s->ready) != VK_SUCCESS)
      goto fail;
   s->bytes = (VkDeviceSize)width * height * GPU_CANVAS_BYTES_PER_PIXEL;
   const VkBufferCreateInfo buffer = {
      .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
      .size = s->bytes,
      .usage = VK_BUFFER_USAGE_TRANSFER_DST_BIT,
      .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
   };
   if (vk.CreateBuffer(vk.device, &buffer, NULL, &s->buffer) != VK_SUCCESS)
      goto fail;
   vk.GetBufferMemoryRequirements(vk.device, s->buffer, &need);
   /* Cached memory makes the CPU copy into the window fast; coherent avoids
    * an invalidate. Take both when offered, else what is there. */
   const VkMemoryPropertyFlags visible = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;
   const VkMemoryPropertyFlags coherent = visible | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
   const VkMemoryPropertyFlags cached = coherent | VK_MEMORY_PROPERTY_HOST_CACHED_BIT;
   type = memory_type(need.memoryTypeBits, cached);
   if (type == GPU_CANVAS_NO_MEMORY_TYPE)
      type = memory_type(need.memoryTypeBits, coherent);
   if (type == GPU_CANVAS_NO_MEMORY_TYPE)
      type = memory_type(need.memoryTypeBits, visible);
   if (type == GPU_CANVAS_NO_MEMORY_TYPE)
      goto fail;
   s->coherent = (vk.memory.memoryTypes[type].propertyFlags &
                  VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) != 0;
   allocate.allocationSize = need.size;
   allocate.memoryTypeIndex = type;
   if (vk.AllocateMemory(vk.device, &allocate, NULL, &s->buffer_memory) != VK_SUCCESS ||
       vk.BindBufferMemory(vk.device, s->buffer, s->buffer_memory, 0) != VK_SUCCESS ||
       vk.MapMemory(vk.device, s->buffer_memory, 0, VK_WHOLE_SIZE, 0, &s->mapped) != VK_SUCCESS)
      goto fail;
   const VkCommandBufferAllocateInfo command = {
      .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
      .commandPool = vk.pool,
      .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
      .commandBufferCount = 1,
   };
   if (vk.AllocateCommandBuffers(vk.device, &command, &s->copy) != VK_SUCCESS)
      goto fail;
   const VkFenceCreateInfo fence = { .sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO };
   if (vk.CreateFence(vk.device, &fence, NULL, &s->fence) != VK_SUCCESS ||
       !record_copy(s, width, height))
      goto fail;
   *target = (struct gpu_canvas_target){
      .image = (uint64_t)(uintptr_t)s->image,
      .view = (uint64_t)(uintptr_t)s->view,
      .ready = (uint64_t)(uintptr_t)s->ready,
      .width = width,
      .height = height,
      .format = GPU_CANVAS_FORMAT,
      .pitch = width * GPU_CANVAS_BYTES_PER_PIXEL,
   };
   return GPU_CANVAS_OK;
fail:
   gpu_canvas_vk_destroy(index);
   return GPU_CANVAS_FAILED;
}

uint32_t gpu_canvas_vk_present(uint32_t index)
{
   if (!vk.open || index >= GPU_CANVAS_TARGETS || !vk.slots[index].live ||
       vk.slots[index].submitted)
      return GPU_CANVAS_BAD_ARGUMENT;
   struct slot *s = &vk.slots[index];
   if (vk.ResetFences(vk.device, 1, &s->fence) != VK_SUCCESS)
      return GPU_CANVAS_FAILED;
   const VkPipelineStageFlags stage = VK_PIPELINE_STAGE_TRANSFER_BIT;
   const VkSubmitInfo submit = {
      .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
      .waitSemaphoreCount = 1,
      .pWaitSemaphores = &s->ready,
      .pWaitDstStageMask = &stage,
      .commandBufferCount = 1,
      .pCommandBuffers = &s->copy,
   };
   if (vk.QueueSubmit(vk.queue, 1, &submit, s->fence) != VK_SUCCESS)
      return GPU_CANVAS_FAILED;
   s->submitted = 1;
   return GPU_CANVAS_OK;
}

uint32_t gpu_canvas_vk_poll(uint32_t index)
{
   if (!vk.open || index >= GPU_CANVAS_TARGETS || !vk.slots[index].live ||
       !vk.slots[index].submitted)
      return GPU_CANVAS_BAD_ARGUMENT;
   struct slot *s = &vk.slots[index];
   const VkResult status = vk.GetFenceStatus(vk.device, s->fence);
   if (status == VK_NOT_READY)
      return GPU_CANVAS_PENDING;
   if (status != VK_SUCCESS)
      return GPU_CANVAS_FAILED;
   s->submitted = 0;
   if (!s->coherent) {
      const VkMappedMemoryRange range = {
         .sType = VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE,
         .memory = s->buffer_memory,
         .size = VK_WHOLE_SIZE,
      };
      if (vk.InvalidateMappedMemoryRanges(vk.device, 1, &range) != VK_SUCCESS)
         return GPU_CANVAS_FAILED;
   }
   return GPU_CANVAS_OK;
}

const void *gpu_canvas_vk_pixels(uint32_t index)
{
   if (!vk.open || index >= GPU_CANVAS_TARGETS || !vk.slots[index].live ||
       vk.slots[index].submitted)
      return NULL;
   return vk.slots[index].mapped;
}

void gpu_canvas_vk_destroy(uint32_t index)
{
   if (!vk.open || index >= GPU_CANVAS_TARGETS || !vk.slots[index].live)
      return;
   struct slot *s = &vk.slots[index];
   if (s->submitted)
      (void)vk.WaitForFences(vk.device, 1, &s->fence, VK_TRUE, GPU_CANVAS_RETIRE_TIMEOUT_NS);
   if (s->fence) vk.DestroyFence(vk.device, s->fence, NULL);
   if (s->copy) vk.FreeCommandBuffers(vk.device, vk.pool, 1, &s->copy);
   if (s->mapped) vk.UnmapMemory(vk.device, s->buffer_memory);
   if (s->buffer) vk.DestroyBuffer(vk.device, s->buffer, NULL);
   if (s->buffer_memory) vk.FreeMemory(vk.device, s->buffer_memory, NULL);
   if (s->ready) vk.DestroySemaphore(vk.device, s->ready, NULL);
   if (s->view) vk.DestroyImageView(vk.device, s->view, NULL);
   if (s->image) vk.DestroyImage(vk.device, s->image, NULL);
   if (s->image_memory) vk.FreeMemory(vk.device, s->image_memory, NULL);
   *s = (struct slot){0};
}

void gpu_canvas_vk_idle(void)
{
   if (vk.open)
      (void)vk.QueueWaitIdle(vk.queue);
}

void gpu_canvas_vk_close(void)
{
   if (!vk.open)
      return;
   /* An acquired target's semaphore may have been signalled without a
    * present to wait on it: let every submission finish first. */
   (void)vk.QueueWaitIdle(vk.queue);
   for (uint32_t n = 0; n < GPU_CANVAS_TARGETS; n++)
      gpu_canvas_vk_destroy(n);
   vk.DestroyCommandPool(vk.device, vk.pool, NULL);
   vk.open = 0;
}
