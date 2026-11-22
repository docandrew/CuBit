/* Teapot gallery Vulkan glue (see teapot_vk.h). */
#include "teapot_vk.h"
#include "teapot_shaders.h"   /* generated: teapot_vert_spv, teapot_frag_spv */
#include <stddef.h>
#include <string.h>

#define TEAPOT_NO_MEMORY_TYPE UINT32_MAX
#define TEAPOT_MEMORY_TYPE_LIMIT 32u
/* Explicit bound on waiting for a slot's previous frame before re-recording
 * its command buffer, and at teardown. Nanoseconds. */
#define TEAPOT_FRAME_TIMEOUT_NS 2000000000ull
#define TEAPOT_ATTRIBUTES 4u
#define TEAPOT_POSITION_OFFSET 0u
#define TEAPOT_NORMAL_OFFSET 12u
#define TEAPOT_UV_OFFSET 24u
#define TEAPOT_BARY_OFFSET 32u
#define TEAPOT_DEPTH_CLEAR 1.0f
#define TEAPOT_STAGES 2u
#define TEAPOT_ATTACHMENTS 2u
#define TEAPOT_DEPENDENCIES 2u

struct frame {
   VkImage depth;
   VkDeviceMemory depth_memory;
   VkImageView depth_view;
   VkFramebuffer framebuffer;
   uint64_t color_view;       /* the widget view the framebuffer was built on */
   uint32_t generation;
   uint32_t width, height;
   VkCommandBuffer commands;
   VkFence fence;
   int submitted;
};

static struct {
   int live;
   VkDevice device;
   VkQueue queue;
   VkPhysicalDeviceMemoryProperties memory;
   VkFormat color_format, depth_format;
   VkRenderPass pass;
   VkPipelineLayout layout;
   VkPipeline pipelines[TEAPOT_SHADINGS];
   VkBuffer vertices;
   VkDeviceMemory vertex_memory;
   uint32_t vertex_count;
   VkCommandPool pool;
   struct frame frames[TEAPOT_SLOTS];
#define FN(name) PFN_vk##name name
   FN(CreateRenderPass); FN(DestroyRenderPass); FN(CreateShaderModule); FN(DestroyShaderModule);
   FN(CreatePipelineLayout); FN(DestroyPipelineLayout); FN(CreateGraphicsPipelines);
   FN(DestroyPipeline); FN(CreateBuffer); FN(DestroyBuffer); FN(GetBufferMemoryRequirements);
   FN(AllocateMemory); FN(FreeMemory); FN(BindBufferMemory); FN(MapMemory); FN(UnmapMemory);
   FN(CreateImage); FN(DestroyImage); FN(GetImageMemoryRequirements); FN(BindImageMemory);
   FN(CreateImageView); FN(DestroyImageView); FN(CreateFramebuffer); FN(DestroyFramebuffer);
   FN(CreateCommandPool); FN(DestroyCommandPool); FN(AllocateCommandBuffers);
   FN(ResetCommandBuffer); FN(BeginCommandBuffer); FN(EndCommandBuffer);
   FN(CmdBeginRenderPass); FN(CmdEndRenderPass); FN(CmdBindPipeline); FN(CmdBindVertexBuffers);
   FN(CmdSetViewport); FN(CmdSetScissor); FN(CmdPushConstants); FN(CmdDraw);
   FN(CreateFence); FN(DestroyFence); FN(ResetFences); FN(WaitForFences); FN(QueueSubmit);
   FN(QueueWaitIdle); FN(FlushMappedMemoryRanges);
#undef FN
} r;

static uint32_t memory_type(uint32_t bits, VkMemoryPropertyFlags flags)
{
   for (uint32_t n = 0; n < r.memory.memoryTypeCount && n < TEAPOT_MEMORY_TYPE_LIMIT; n++)
      if ((bits & (1u << n)) && (r.memory.memoryTypes[n].propertyFlags & flags) == flags)
         return n;
   return TEAPOT_NO_MEMORY_TYPE;
}

static VkShaderModule shader(const uint32_t *code, size_t bytes)
{
   const VkShaderModuleCreateInfo info = {
      .sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
      .codeSize = bytes,
      .pCode = code,
   };
   VkShaderModule module = VK_NULL_HANDLE;
   return r.CreateShaderModule(r.device, &info, NULL, &module) == VK_SUCCESS ? module : VK_NULL_HANDLE;
}

static int make_pass(void)
{
   const VkAttachmentDescription attachments[TEAPOT_ATTACHMENTS] = {
      { .format = r.color_format, .samples = VK_SAMPLE_COUNT_1_BIT,
        .loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR, .storeOp = VK_ATTACHMENT_STORE_OP_STORE,
        .stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE,
        .stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE,
        .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED,
        /* The widget copies the frame out of this layout. */
        .finalLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL },
      { .format = r.depth_format, .samples = VK_SAMPLE_COUNT_1_BIT,
        .loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR, .storeOp = VK_ATTACHMENT_STORE_OP_DONT_CARE,
        .stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE,
        .stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE,
        .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED,
        .finalLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL },
   };
   const VkAttachmentReference color = { 0, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL };
   const VkAttachmentReference depth = { 1, VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL };
   const VkSubpassDescription subpass = {
      .pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS,
      .colorAttachmentCount = 1,
      .pColorAttachments = &color,
      .pDepthStencilAttachment = &depth,
   };
   const VkSubpassDependency dependencies[TEAPOT_DEPENDENCIES] = {
      { .srcSubpass = VK_SUBPASS_EXTERNAL, .dstSubpass = 0,
        .srcStageMask = VK_PIPELINE_STAGE_TRANSFER_BIT |
                        VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT |
                        VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT,
        .dstStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT |
                        VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT,
        .srcAccessMask = VK_ACCESS_TRANSFER_READ_BIT | VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT,
        .dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT |
                         VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT |
                         VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT },
      { .srcSubpass = 0, .dstSubpass = VK_SUBPASS_EXTERNAL,
        .srcStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        .dstStageMask = VK_PIPELINE_STAGE_TRANSFER_BIT,
        .srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
        .dstAccessMask = VK_ACCESS_TRANSFER_READ_BIT },
   };
   const VkRenderPassCreateInfo info = {
      .sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO,
      .attachmentCount = TEAPOT_ATTACHMENTS,
      .pAttachments = attachments,
      .subpassCount = 1,
      .pSubpasses = &subpass,
      .dependencyCount = TEAPOT_DEPENDENCIES,
      .pDependencies = dependencies,
   };
   return r.CreateRenderPass(r.device, &info, NULL, &r.pass) == VK_SUCCESS;
}

static int make_pipelines(void)
{
   const VkPushConstantRange push = {
      .stageFlags = VK_SHADER_STAGE_VERTEX_BIT | VK_SHADER_STAGE_FRAGMENT_BIT,
      .size = TEAPOT_PUSH_FLOATS * sizeof(float),
   };
   const VkPipelineLayoutCreateInfo layout = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
      .pushConstantRangeCount = 1,
      .pPushConstantRanges = &push,
   };
   if (r.CreatePipelineLayout(r.device, &layout, NULL, &r.layout) != VK_SUCCESS)
      return 0;
   VkShaderModule vert = shader(teapot_vert_spv, sizeof(teapot_vert_spv));
   VkShaderModule frag = shader(teapot_frag_spv, sizeof(teapot_frag_spv));
   int ok = vert && frag;
   const VkVertexInputBindingDescription binding = {
      .binding = 0, .stride = TEAPOT_VERTEX_BYTES,
      .inputRate = VK_VERTEX_INPUT_RATE_VERTEX,
   };
   const VkVertexInputAttributeDescription attributes[TEAPOT_ATTRIBUTES] = {
      { 0, 0, VK_FORMAT_R32G32B32_SFLOAT, TEAPOT_POSITION_OFFSET },
      { 1, 0, VK_FORMAT_R32G32B32_SFLOAT, TEAPOT_NORMAL_OFFSET },
      { 2, 0, VK_FORMAT_R32G32_SFLOAT, TEAPOT_UV_OFFSET },
      { 3, 0, VK_FORMAT_R32G32B32_SFLOAT, TEAPOT_BARY_OFFSET },
   };
   const VkPipelineVertexInputStateCreateInfo input = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO,
      .vertexBindingDescriptionCount = 1, .pVertexBindingDescriptions = &binding,
      .vertexAttributeDescriptionCount = TEAPOT_ATTRIBUTES, .pVertexAttributeDescriptions = attributes,
   };
   const VkPipelineInputAssemblyStateCreateInfo assembly = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
      .topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
   };
   const VkPipelineViewportStateCreateInfo viewport = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,
      .viewportCount = 1, .scissorCount = 1,
   };
   const VkPipelineRasterizationStateCreateInfo raster = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
      .polygonMode = VK_POLYGON_MODE_FILL,
      .cullMode = VK_CULL_MODE_NONE,   /* open at the lid seam: two-sided */
      .frontFace = VK_FRONT_FACE_COUNTER_CLOCKWISE,
      .lineWidth = 1.0f,
   };
   const VkPipelineMultisampleStateCreateInfo multisample = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,
      .rasterizationSamples = VK_SAMPLE_COUNT_1_BIT,
   };
   const VkPipelineDepthStencilStateCreateInfo depth = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO,
      .depthTestEnable = VK_TRUE, .depthWriteEnable = VK_TRUE,
      .depthCompareOp = VK_COMPARE_OP_LESS,
   };
   const VkPipelineColorBlendAttachmentState blend_attachment = {
      .colorWriteMask = VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT |
                        VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT,
   };
   const VkPipelineColorBlendStateCreateInfo blend = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,
      .attachmentCount = 1, .pAttachments = &blend_attachment,
   };
   const VkDynamicState dynamic_states[] = { VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR };
   const VkPipelineDynamicStateCreateInfo dynamic = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO,
      .dynamicStateCount = sizeof dynamic_states / sizeof dynamic_states[0],
      .pDynamicStates = dynamic_states,
   };
   for (uint32_t shading = 0; ok && shading < TEAPOT_SHADINGS; shading++) {
      const int32_t value = (int32_t)shading;
      const VkSpecializationMapEntry entry = { 0, 0, sizeof value };
      const VkSpecializationInfo specialization = {
         .mapEntryCount = 1, .pMapEntries = &entry,
         .dataSize = sizeof value, .pData = &value,
      };
      const VkPipelineShaderStageCreateInfo stages[TEAPOT_STAGES] = {
         { .sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
           .stage = VK_SHADER_STAGE_VERTEX_BIT, .module = vert, .pName = "main",
           .pSpecializationInfo = &specialization },
         { .sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
           .stage = VK_SHADER_STAGE_FRAGMENT_BIT, .module = frag, .pName = "main",
           .pSpecializationInfo = &specialization },
      };
      const VkGraphicsPipelineCreateInfo info = {
         .sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,
         .stageCount = TEAPOT_STAGES, .pStages = stages,
         .pVertexInputState = &input, .pInputAssemblyState = &assembly,
         .pViewportState = &viewport, .pRasterizationState = &raster,
         .pMultisampleState = &multisample, .pDepthStencilState = &depth,
         .pColorBlendState = &blend, .pDynamicState = &dynamic,
         .layout = r.layout, .renderPass = r.pass, .subpass = 0,
      };
      ok = r.CreateGraphicsPipelines(r.device, VK_NULL_HANDLE, 1, &info, NULL,
                                     &r.pipelines[shading]) == VK_SUCCESS;
   }
   if (vert) r.DestroyShaderModule(r.device, vert, NULL);
   if (frag) r.DestroyShaderModule(r.device, frag, NULL);
   return ok;
}

static int make_vertices(const void *data, uint32_t count, uint32_t bytes_each)
{
   const VkDeviceSize bytes = (VkDeviceSize)count * bytes_each;
   const VkBufferCreateInfo info = {
      .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
      .size = bytes,
      .usage = VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
      .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
   };
   if (r.CreateBuffer(r.device, &info, NULL, &r.vertices) != VK_SUCCESS)
      return 0;
   VkMemoryRequirements need;
   r.GetBufferMemoryRequirements(r.device, r.vertices, &need);
   /* Written once at startup; host-visible keeps this a single copy. */
   const VkMemoryPropertyFlags visible = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;
   uint32_t type = memory_type(need.memoryTypeBits, visible | VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT |
                                                    VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
   if (type == TEAPOT_NO_MEMORY_TYPE)
      type = memory_type(need.memoryTypeBits, visible | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
   if (type == TEAPOT_NO_MEMORY_TYPE)
      type = memory_type(need.memoryTypeBits, visible);
   if (type == TEAPOT_NO_MEMORY_TYPE)
      return 0;
   const VkMemoryAllocateInfo allocate = {
      .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
      .allocationSize = need.size, .memoryTypeIndex = type,
   };
   void *mapped = NULL;
   if (r.AllocateMemory(r.device, &allocate, NULL, &r.vertex_memory) != VK_SUCCESS ||
       r.BindBufferMemory(r.device, r.vertices, r.vertex_memory, 0) != VK_SUCCESS ||
       r.MapMemory(r.device, r.vertex_memory, 0, VK_WHOLE_SIZE, 0, &mapped) != VK_SUCCESS)
      return 0;
   memcpy(mapped, data, (size_t)bytes);
   if (!(r.memory.memoryTypes[type].propertyFlags & VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)) {
      const VkMappedMemoryRange range = {
         .sType = VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE,
         .memory = r.vertex_memory, .size = VK_WHOLE_SIZE,
      };
      (void)r.FlushMappedMemoryRanges(r.device, 1, &range);
   }
   r.UnmapMemory(r.device, r.vertex_memory);
   r.vertex_count = count;
   return 1;
}

uint32_t teapot_vk_create(const struct gpu_canvas_device *d, uint32_t color_format,
                          const void *vertices, uint32_t vertex_count, uint32_t vertex_bytes)
{
   if (r.live || !d || !d->device || !d->instance_proc || !vertices || !vertex_count ||
       vertex_bytes != TEAPOT_VERTEX_BYTES)
      return TEAPOT_BAD_ARGUMENT;
   PFN_vkGetDeviceProcAddr device_proc =
      (PFN_vkGetDeviceProcAddr)d->instance_proc(d->instance, "vkGetDeviceProcAddr");
   PFN_vkGetPhysicalDeviceMemoryProperties memory_props = (PFN_vkGetPhysicalDeviceMemoryProperties)
      d->instance_proc(d->instance, "vkGetPhysicalDeviceMemoryProperties");
   PFN_vkGetPhysicalDeviceFormatProperties format_props = (PFN_vkGetPhysicalDeviceFormatProperties)
      d->instance_proc(d->instance, "vkGetPhysicalDeviceFormatProperties");
   if (!device_proc || !memory_props || !format_props)
      return TEAPOT_FAILED;
#define LOAD(name) \
   r.name = (PFN_vk##name)device_proc(d->device, "vk" #name); \
   if (!r.name) return TEAPOT_FAILED
   LOAD(CreateRenderPass); LOAD(DestroyRenderPass); LOAD(CreateShaderModule); LOAD(DestroyShaderModule);
   LOAD(CreatePipelineLayout); LOAD(DestroyPipelineLayout); LOAD(CreateGraphicsPipelines);
   LOAD(DestroyPipeline); LOAD(CreateBuffer); LOAD(DestroyBuffer); LOAD(GetBufferMemoryRequirements);
   LOAD(AllocateMemory); LOAD(FreeMemory); LOAD(BindBufferMemory); LOAD(MapMemory); LOAD(UnmapMemory);
   LOAD(CreateImage); LOAD(DestroyImage); LOAD(GetImageMemoryRequirements); LOAD(BindImageMemory);
   LOAD(CreateImageView); LOAD(DestroyImageView); LOAD(CreateFramebuffer); LOAD(DestroyFramebuffer);
   LOAD(CreateCommandPool); LOAD(DestroyCommandPool); LOAD(AllocateCommandBuffers);
   LOAD(ResetCommandBuffer); LOAD(BeginCommandBuffer); LOAD(EndCommandBuffer);
   LOAD(CmdBeginRenderPass); LOAD(CmdEndRenderPass); LOAD(CmdBindPipeline); LOAD(CmdBindVertexBuffers);
   LOAD(CmdSetViewport); LOAD(CmdSetScissor); LOAD(CmdPushConstants); LOAD(CmdDraw);
   LOAD(CreateFence); LOAD(DestroyFence); LOAD(ResetFences); LOAD(WaitForFences); LOAD(QueueSubmit);
   LOAD(QueueWaitIdle); LOAD(FlushMappedMemoryRanges);
#undef LOAD
   memory_props(d->physical, &r.memory);
   r.device = d->device;
   r.queue = d->queue;
   r.color_format = (VkFormat)color_format;
   /* Vulkan requires one of the first two as a depth attachment. */
   const VkFormat depth_candidates[] = {
      VK_FORMAT_D32_SFLOAT, VK_FORMAT_X8_D24_UNORM_PACK32, VK_FORMAT_D16_UNORM,
   };
   r.depth_format = VK_FORMAT_UNDEFINED;
   for (size_t n = 0; n < sizeof depth_candidates / sizeof depth_candidates[0]; n++) {
      VkFormatProperties properties;
      format_props(d->physical, depth_candidates[n], &properties);
      if (properties.optimalTilingFeatures & VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT) {
         r.depth_format = depth_candidates[n];
         break;
      }
   }
   r.live = 1; /* destroy releases whatever exists from here on */
   const VkCommandPoolCreateInfo pool = {
      .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
      .flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
      .queueFamilyIndex = d->family,
   };
   const VkCommandBufferAllocateInfo commands = {
      .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
      .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
      .commandBufferCount = 1,
   };
   const VkFenceCreateInfo fence = {
      .sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO,
   };
   int ok = r.depth_format != VK_FORMAT_UNDEFINED &&
            r.CreateCommandPool(r.device, &pool, NULL, &r.pool) == VK_SUCCESS &&
            make_pass() && make_pipelines() &&
            make_vertices(vertices, vertex_count, vertex_bytes);
   for (uint32_t n = 0; ok && n < TEAPOT_SLOTS; n++) {
      VkCommandBufferAllocateInfo info = commands;
      info.commandPool = r.pool;
      ok = r.AllocateCommandBuffers(r.device, &info, &r.frames[n].commands) == VK_SUCCESS &&
           r.CreateFence(r.device, &fence, NULL, &r.frames[n].fence) == VK_SUCCESS;
   }
   if (!ok) {
      teapot_vk_destroy();
      return TEAPOT_FAILED;
   }
   return TEAPOT_OK;
}

static void drop_target_objects(struct frame *f)
{
   if (f->framebuffer) r.DestroyFramebuffer(r.device, f->framebuffer, NULL);
   if (f->depth_view) r.DestroyImageView(r.device, f->depth_view, NULL);
   if (f->depth) r.DestroyImage(r.device, f->depth, NULL);
   if (f->depth_memory) r.FreeMemory(r.device, f->depth_memory, NULL);
   f->framebuffer = VK_NULL_HANDLE;
   f->depth_view = VK_NULL_HANDLE;
   f->depth = VK_NULL_HANDLE;
   f->depth_memory = VK_NULL_HANDLE;
   f->color_view = 0;
}

/* Depth image and framebuffer for this target, rebuilt when the widget
 * rebuilt its targets (new generation or view). */
static int target_objects(struct frame *f, const struct teapot_target *t)
{
   if (f->framebuffer && f->color_view == t->view && f->generation == t->generation)
      return 1;
   drop_target_objects(f);
   const VkImageCreateInfo image = {
      .sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,
      .imageType = VK_IMAGE_TYPE_2D,
      .format = r.depth_format,
      .extent = { t->width, t->height, 1 },
      .mipLevels = 1, .arrayLayers = 1,
      .samples = VK_SAMPLE_COUNT_1_BIT,
      .tiling = VK_IMAGE_TILING_OPTIMAL,
      .usage = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
      .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
      .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED,
   };
   if (r.CreateImage(r.device, &image, NULL, &f->depth) != VK_SUCCESS)
      return 0;
   VkMemoryRequirements need;
   r.GetImageMemoryRequirements(r.device, f->depth, &need);
   uint32_t type = memory_type(need.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
   if (type == TEAPOT_NO_MEMORY_TYPE)
      type = memory_type(need.memoryTypeBits, 0);
   const VkMemoryAllocateInfo allocate = {
      .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
      .allocationSize = need.size, .memoryTypeIndex = type,
   };
   if (type == TEAPOT_NO_MEMORY_TYPE ||
       r.AllocateMemory(r.device, &allocate, NULL, &f->depth_memory) != VK_SUCCESS ||
       r.BindImageMemory(r.device, f->depth, f->depth_memory, 0) != VK_SUCCESS)
      return 0;
   const VkImageViewCreateInfo view = {
      .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
      .image = f->depth,
      .viewType = VK_IMAGE_VIEW_TYPE_2D,
      .format = r.depth_format,
      .subresourceRange = { VK_IMAGE_ASPECT_DEPTH_BIT, 0, 1, 0, 1 },
   };
   if (r.CreateImageView(r.device, &view, NULL, &f->depth_view) != VK_SUCCESS)
      return 0;
   const VkImageView views[TEAPOT_ATTACHMENTS] = {
      (VkImageView)(uintptr_t)t->view, f->depth_view,
   };
   const VkFramebufferCreateInfo framebuffer = {
      .sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO,
      .renderPass = r.pass,
      .attachmentCount = TEAPOT_ATTACHMENTS, .pAttachments = views,
      .width = t->width, .height = t->height, .layers = 1,
   };
   if (r.CreateFramebuffer(r.device, &framebuffer, NULL, &f->framebuffer) != VK_SUCCESS)
      return 0;
   f->color_view = t->view;
   f->generation = t->generation;
   f->width = t->width;
   f->height = t->height;
   return 1;
}

uint32_t teapot_vk_render(const struct teapot_target *t, const struct teapot_draw *draws,
                          uint32_t count, const float clear[4])
{
   if (!r.live || !t || t->slot >= TEAPOT_SLOTS || !t->image || !t->view || !t->ready ||
       !t->width || !t->height || (count && !draws) || !clear ||
       t->format != (uint32_t)r.color_format)
      return TEAPOT_BAD_ARGUMENT;
   struct frame *f = &r.frames[t->slot];
   if (f->submitted) {
      if (r.WaitForFences(r.device, 1, &f->fence, VK_TRUE, TEAPOT_FRAME_TIMEOUT_NS) != VK_SUCCESS)
         return TEAPOT_FAILED;
      f->submitted = 0;
   }
   if (!target_objects(f, t) ||
       r.ResetFences(r.device, 1, &f->fence) != VK_SUCCESS ||
       r.ResetCommandBuffer(f->commands, 0) != VK_SUCCESS)
      return TEAPOT_FAILED;
   const VkCommandBufferBeginInfo begin = {
      .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
      .flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
   };
   if (r.BeginCommandBuffer(f->commands, &begin) != VK_SUCCESS)
      return TEAPOT_FAILED;
   const VkClearValue clears[TEAPOT_ATTACHMENTS] = {
      { .color = { .float32 = { clear[0], clear[1], clear[2], clear[3] } } },
      { .depthStencil = { TEAPOT_DEPTH_CLEAR, 0 } },
   };
   const VkRenderPassBeginInfo pass = {
      .sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,
      .renderPass = r.pass,
      .framebuffer = f->framebuffer,
      .renderArea = { { 0, 0 }, { t->width, t->height } },
      .clearValueCount = TEAPOT_ATTACHMENTS,
      .pClearValues = clears,
   };
   r.CmdBeginRenderPass(f->commands, &pass, VK_SUBPASS_CONTENTS_INLINE);
   const VkDeviceSize offset = 0;
   r.CmdBindVertexBuffers(f->commands, 0, 1, &r.vertices, &offset);
   for (uint32_t n = 0; n < count; n++) {
      const struct teapot_draw *d = &draws[n];
      if (d->shading >= TEAPOT_SHADINGS || !d->width || !d->height || d->x < 0 || d->y < 0 ||
          (uint32_t)d->x + d->width > t->width || (uint32_t)d->y + d->height > t->height)
         continue;
      const VkViewport viewport = {
         (float)d->x, (float)d->y, (float)d->width, (float)d->height, 0.0f, 1.0f,
      };
      const VkRect2D scissor = { { d->x, d->y }, { d->width, d->height } };
      r.CmdBindPipeline(f->commands, VK_PIPELINE_BIND_POINT_GRAPHICS, r.pipelines[d->shading]);
      r.CmdSetViewport(f->commands, 0, 1, &viewport);
      r.CmdSetScissor(f->commands, 0, 1, &scissor);
      r.CmdPushConstants(f->commands, r.layout,
                         VK_SHADER_STAGE_VERTEX_BIT | VK_SHADER_STAGE_FRAGMENT_BIT,
                         0, sizeof d->push, d->push);
      r.CmdDraw(f->commands, r.vertex_count, 1, 0, 0);
   }
   r.CmdEndRenderPass(f->commands);
   if (r.EndCommandBuffer(f->commands) != VK_SUCCESS)
      return TEAPOT_FAILED;
   const VkSemaphore ready = (VkSemaphore)(uintptr_t)t->ready;
   const VkSubmitInfo submit = {
      .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
      .commandBufferCount = 1, .pCommandBuffers = &f->commands,
      .signalSemaphoreCount = 1, .pSignalSemaphores = &ready,
   };
   if (r.QueueSubmit(r.queue, 1, &submit, f->fence) != VK_SUCCESS)
      return TEAPOT_FAILED;
   f->submitted = 1;
   return TEAPOT_OK;
}

void teapot_vk_destroy(void)
{
   if (!r.live)
      return;
   (void)r.QueueWaitIdle(r.queue);
   for (uint32_t n = 0; n < TEAPOT_SLOTS; n++) {
      struct frame *f = &r.frames[n];
      drop_target_objects(f);
      if (f->fence) r.DestroyFence(r.device, f->fence, NULL);
      *f = (struct frame){0};
   }
   if (r.pool) r.DestroyCommandPool(r.device, r.pool, NULL);
   if (r.vertices) r.DestroyBuffer(r.device, r.vertices, NULL);
   if (r.vertex_memory) r.FreeMemory(r.device, r.vertex_memory, NULL);
   for (uint32_t n = 0; n < TEAPOT_SHADINGS; n++)
      if (r.pipelines[n]) r.DestroyPipeline(r.device, r.pipelines[n], NULL);
   if (r.layout) r.DestroyPipelineLayout(r.device, r.layout, NULL);
   if (r.pass) r.DestroyRenderPass(r.device, r.pass, NULL);
   r.pool = VK_NULL_HANDLE; r.vertices = VK_NULL_HANDLE; r.vertex_memory = VK_NULL_HANDLE;
   for (uint32_t n = 0; n < TEAPOT_SHADINGS; n++) r.pipelines[n] = VK_NULL_HANDLE;
   r.layout = VK_NULL_HANDLE; r.pass = VK_NULL_HANDLE;
   r.live = 0;
}
