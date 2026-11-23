/* Neon Blast Vulkan glue (see neon_vk.h).
 * SPDX-License-Identifier: GPL-3.0-or-later */
#include "neon_vk.h"
#include "neon_shaders.h"   /* generated: fullscreen_vert_spv, background_frag_spv, ... */
#include <stddef.h>
#include <string.h>

#define NEON_NO_MEMORY_TYPE UINT32_MAX
#define NEON_MEMORY_TYPE_LIMIT 32u
/* Explicit bound on waiting for a slot's previous frame. Nanoseconds. */
#define NEON_FRAME_TIMEOUT_NS 2000000000ull
#define NEON_STAGES 2u
#define NEON_ATTRIBUTES 3u
#define NEON_QUAD_CORNERS 4u
#define NEON_TRIANGLE_CORNERS 3u
#define NEON_TIMESTAMPS 2u
#define NEON_POST_MULTIPLY 0
#define NEON_POST_ADD 1

enum pipeline_kind { BACKGROUND, SEGMENTS, POST_MULTIPLY, POST_ADD, PIPELINES };

struct frame {
   VkFramebuffer framebuffer;
   uint64_t color_view;
   uint32_t generation;
   VkCommandBuffer commands;
   VkFence fence;
   VkBuffer uniforms, instances;
   VkDeviceMemory uniform_memory, instance_memory;
   void *uniform_map, *instance_map;
   int uniform_coherent, instance_coherent;
   VkDescriptorSet set;
   VkQueryPool queries;
   int submitted;
};

static struct {
   int live;
   VkDevice device;
   VkQueue queue;
   VkPhysicalDeviceMemoryProperties memory;
   VkFormat color_format;
   VkRenderPass pass;
   VkDescriptorSetLayout set_layout;
   VkDescriptorPool descriptors;
   VkPipelineLayout layout;
   VkPipeline pipelines[PIPELINES];
   VkCommandPool pool;
   uint32_t max_instances;
   int timing;                /* the queue family writes timestamps */
   float tick_ns;             /* VkPhysicalDeviceLimits::timestampPeriod */
   uint64_t gpu_ns;
   struct frame frames[NEON_SLOTS];
#define FN(name) PFN_vk##name name
   FN(CreateRenderPass); FN(DestroyRenderPass); FN(CreateShaderModule); FN(DestroyShaderModule);
   FN(CreatePipelineLayout); FN(DestroyPipelineLayout); FN(CreateGraphicsPipelines);
   FN(DestroyPipeline); FN(CreateBuffer); FN(DestroyBuffer); FN(GetBufferMemoryRequirements);
   FN(AllocateMemory); FN(FreeMemory); FN(BindBufferMemory); FN(MapMemory); FN(UnmapMemory);
   FN(CreateFramebuffer); FN(DestroyFramebuffer);
   FN(CreateCommandPool); FN(DestroyCommandPool); FN(AllocateCommandBuffers);
   FN(ResetCommandBuffer); FN(BeginCommandBuffer); FN(EndCommandBuffer);
   FN(CmdBeginRenderPass); FN(CmdEndRenderPass); FN(CmdBindPipeline); FN(CmdBindVertexBuffers);
   FN(CmdSetViewport); FN(CmdSetScissor); FN(CmdDraw); FN(CmdBindDescriptorSets);
   FN(CmdResetQueryPool); FN(CmdWriteTimestamp);
   FN(CreateFence); FN(DestroyFence); FN(ResetFences); FN(WaitForFences); FN(QueueSubmit);
   FN(QueueWaitIdle); FN(FlushMappedMemoryRanges);
   FN(CreateDescriptorSetLayout); FN(DestroyDescriptorSetLayout); FN(CreateDescriptorPool);
   FN(DestroyDescriptorPool); FN(AllocateDescriptorSets); FN(UpdateDescriptorSets);
   FN(CreateQueryPool); FN(DestroyQueryPool); FN(GetQueryPoolResults);
#undef FN
} r;

static uint32_t memory_type(uint32_t bits, VkMemoryPropertyFlags flags)
{
   for (uint32_t n = 0; n < r.memory.memoryTypeCount && n < NEON_MEMORY_TYPE_LIMIT; n++)
      if ((bits & (1u << n)) && (r.memory.memoryTypes[n].propertyFlags & flags) == flags)
         return n;
   return NEON_NO_MEMORY_TYPE;
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

/* A host-visible buffer, persistently mapped. */
static int mapped_buffer(VkDeviceSize bytes, VkBufferUsageFlags usage, VkBuffer *buffer,
                         VkDeviceMemory *memory, void **map, int *coherent)
{
   const VkBufferCreateInfo info = {
      .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
      .size = bytes, .usage = usage, .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
   };
   if (r.CreateBuffer(r.device, &info, NULL, buffer) != VK_SUCCESS)
      return 0;
   VkMemoryRequirements need;
   r.GetBufferMemoryRequirements(r.device, *buffer, &need);
   const VkMemoryPropertyFlags visible = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;
   uint32_t type = memory_type(need.memoryTypeBits, visible | VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT |
                                                    VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
   if (type == NEON_NO_MEMORY_TYPE)
      type = memory_type(need.memoryTypeBits, visible | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
   if (type == NEON_NO_MEMORY_TYPE)
      type = memory_type(need.memoryTypeBits, visible);
   if (type == NEON_NO_MEMORY_TYPE)
      return 0;
   const VkMemoryAllocateInfo allocate = {
      .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
      .allocationSize = need.size, .memoryTypeIndex = type,
   };
   if (r.AllocateMemory(r.device, &allocate, NULL, memory) != VK_SUCCESS ||
       r.BindBufferMemory(r.device, *buffer, *memory, 0) != VK_SUCCESS ||
       r.MapMemory(r.device, *memory, 0, VK_WHOLE_SIZE, 0, map) != VK_SUCCESS)
      return 0;
   *coherent = (r.memory.memoryTypes[type].propertyFlags & VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) != 0;
   return 1;
}

static void flush(VkDeviceMemory memory, int coherent)
{
   if (coherent)
      return;
   const VkMappedMemoryRange range = {
      .sType = VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE, .memory = memory, .size = VK_WHOLE_SIZE,
   };
   (void)r.FlushMappedMemoryRanges(r.device, 1, &range);
}

static int make_pass(void)
{
   const VkAttachmentDescription color = {
      .format = r.color_format, .samples = VK_SAMPLE_COUNT_1_BIT,
      /* The background covers every pixel. */
      .loadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE, .storeOp = VK_ATTACHMENT_STORE_OP_STORE,
      .stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE,
      .stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE,
      .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED,
      /* The widget copies the frame out of this layout. */
      .finalLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
   };
   const VkAttachmentReference reference = { 0, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL };
   const VkSubpassDescription subpass = {
      .pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS,
      .colorAttachmentCount = 1, .pColorAttachments = &reference,
   };
   const VkSubpassDependency dependencies[2] = {
      { .srcSubpass = VK_SUBPASS_EXTERNAL, .dstSubpass = 0,
        .srcStageMask = VK_PIPELINE_STAGE_TRANSFER_BIT | VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        .dstStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        .srcAccessMask = VK_ACCESS_TRANSFER_READ_BIT,
        .dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT | VK_ACCESS_COLOR_ATTACHMENT_READ_BIT },
      { .srcSubpass = 0, .dstSubpass = VK_SUBPASS_EXTERNAL,
        .srcStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        .dstStageMask = VK_PIPELINE_STAGE_TRANSFER_BIT,
        .srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
        .dstAccessMask = VK_ACCESS_TRANSFER_READ_BIT },
   };
   const VkRenderPassCreateInfo info = {
      .sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO,
      .attachmentCount = 1, .pAttachments = &color,
      .subpassCount = 1, .pSubpasses = &subpass,
      .dependencyCount = 2, .pDependencies = dependencies,
   };
   return r.CreateRenderPass(r.device, &info, NULL, &r.pass) == VK_SUCCESS;
}

static int make_layout(void)
{
   const VkDescriptorSetLayoutBinding binding = {
      .binding = 0, .descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, .descriptorCount = 1,
      .stageFlags = VK_SHADER_STAGE_VERTEX_BIT | VK_SHADER_STAGE_FRAGMENT_BIT,
   };
   const VkDescriptorSetLayoutCreateInfo set = {
      .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
      .bindingCount = 1, .pBindings = &binding,
   };
   if (r.CreateDescriptorSetLayout(r.device, &set, NULL, &r.set_layout) != VK_SUCCESS)
      return 0;
   const VkDescriptorPoolSize size = { VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, NEON_SLOTS };
   const VkDescriptorPoolCreateInfo pool = {
      .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
      .maxSets = NEON_SLOTS, .poolSizeCount = 1, .pPoolSizes = &size,
   };
   if (r.CreateDescriptorPool(r.device, &pool, NULL, &r.descriptors) != VK_SUCCESS)
      return 0;
   const VkPipelineLayoutCreateInfo layout = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
      .setLayoutCount = 1, .pSetLayouts = &r.set_layout,
   };
   return r.CreatePipelineLayout(r.device, &layout, NULL, &r.layout) == VK_SUCCESS;
}

static int make_pipeline(enum pipeline_kind kind, VkShaderModule vert, VkShaderModule frag)
{
   const int32_t mode = kind == POST_ADD ? NEON_POST_ADD : NEON_POST_MULTIPLY;
   const VkSpecializationMapEntry entry = { 0, 0, sizeof mode };
   const VkSpecializationInfo specialization = {
      .mapEntryCount = 1, .pMapEntries = &entry, .dataSize = sizeof mode, .pData = &mode,
   };
   const VkPipelineShaderStageCreateInfo stages[NEON_STAGES] = {
      { .sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        .stage = VK_SHADER_STAGE_VERTEX_BIT, .module = vert, .pName = "main" },
      { .sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        .stage = VK_SHADER_STAGE_FRAGMENT_BIT, .module = frag, .pName = "main",
        .pSpecializationInfo = &specialization },
   };
   const VkVertexInputBindingDescription binding = {
      .binding = 0, .stride = NEON_INSTANCE_BYTES, .inputRate = VK_VERTEX_INPUT_RATE_INSTANCE,
   };
   const VkVertexInputAttributeDescription attributes[NEON_ATTRIBUTES] = {
      { 0, 0, VK_FORMAT_R32G32B32A32_SFLOAT, 0 },
      { 1, 0, VK_FORMAT_R32G32B32A32_SFLOAT, 16 },
      { 2, 0, VK_FORMAT_R32G32B32A32_SFLOAT, 32 },
   };
   const int instanced = kind == SEGMENTS;
   const VkPipelineVertexInputStateCreateInfo input = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO,
      .vertexBindingDescriptionCount = instanced ? 1 : 0, .pVertexBindingDescriptions = &binding,
      .vertexAttributeDescriptionCount = instanced ? NEON_ATTRIBUTES : 0,
      .pVertexAttributeDescriptions = attributes,
   };
   const VkPipelineInputAssemblyStateCreateInfo assembly = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
      .topology = instanced ? VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP : VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
   };
   const VkPipelineViewportStateCreateInfo viewport = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,
      .viewportCount = 1, .scissorCount = 1,
   };
   const VkPipelineRasterizationStateCreateInfo raster = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
      .polygonMode = VK_POLYGON_MODE_FILL, .cullMode = VK_CULL_MODE_NONE,
      .frontFace = VK_FRONT_FACE_COUNTER_CLOCKWISE, .lineWidth = 1.0f,
   };
   const VkPipelineMultisampleStateCreateInfo multisample = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,
      .rasterizationSamples = VK_SAMPLE_COUNT_1_BIT,
   };
   VkPipelineColorBlendAttachmentState attachment = {
      .colorWriteMask = VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT |
                        VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT,
   };
   if (kind == SEGMENTS || kind == POST_ADD) {
      attachment.blendEnable = VK_TRUE;      /* additive: light adds up */
      attachment.srcColorBlendFactor = VK_BLEND_FACTOR_ONE;
      attachment.dstColorBlendFactor = VK_BLEND_FACTOR_ONE;
      attachment.colorBlendOp = VK_BLEND_OP_ADD;
      attachment.srcAlphaBlendFactor = VK_BLEND_FACTOR_ZERO;
      attachment.dstAlphaBlendFactor = VK_BLEND_FACTOR_ONE;
      attachment.alphaBlendOp = VK_BLEND_OP_ADD;
   } else if (kind == POST_MULTIPLY) {
      attachment.blendEnable = VK_TRUE;      /* dst * src */
      attachment.srcColorBlendFactor = VK_BLEND_FACTOR_ZERO;
      attachment.dstColorBlendFactor = VK_BLEND_FACTOR_SRC_COLOR;
      attachment.colorBlendOp = VK_BLEND_OP_ADD;
      attachment.srcAlphaBlendFactor = VK_BLEND_FACTOR_ZERO;
      attachment.dstAlphaBlendFactor = VK_BLEND_FACTOR_ONE;
      attachment.alphaBlendOp = VK_BLEND_OP_ADD;
   }
   const VkPipelineColorBlendStateCreateInfo blend = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,
      .attachmentCount = 1, .pAttachments = &attachment,
   };
   const VkDynamicState dynamic_states[] = { VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR };
   const VkPipelineDynamicStateCreateInfo dynamic = {
      .sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO,
      .dynamicStateCount = sizeof dynamic_states / sizeof dynamic_states[0],
      .pDynamicStates = dynamic_states,
   };
   const VkGraphicsPipelineCreateInfo info = {
      .sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,
      .stageCount = NEON_STAGES, .pStages = stages,
      .pVertexInputState = &input, .pInputAssemblyState = &assembly,
      .pViewportState = &viewport, .pRasterizationState = &raster,
      .pMultisampleState = &multisample, .pColorBlendState = &blend,
      .pDynamicState = &dynamic, .layout = r.layout, .renderPass = r.pass, .subpass = 0,
   };
   return r.CreateGraphicsPipelines(r.device, VK_NULL_HANDLE, 1, &info, NULL,
                                    &r.pipelines[kind]) == VK_SUCCESS;
}

static int make_pipelines(void)
{
   VkShaderModule full = shader(fullscreen_vert_spv, sizeof fullscreen_vert_spv);
   VkShaderModule background = shader(background_frag_spv, sizeof background_frag_spv);
   VkShaderModule segment_vert = shader(segment_vert_spv, sizeof segment_vert_spv);
   VkShaderModule segment_frag = shader(segment_frag_spv, sizeof segment_frag_spv);
   VkShaderModule post = shader(post_frag_spv, sizeof post_frag_spv);
   int ok = full && background && segment_vert && segment_frag && post &&
            make_pipeline(BACKGROUND, full, background) &&
            make_pipeline(SEGMENTS, segment_vert, segment_frag) &&
            make_pipeline(POST_MULTIPLY, full, post) &&
            make_pipeline(POST_ADD, full, post);
   if (full) r.DestroyShaderModule(r.device, full, NULL);
   if (background) r.DestroyShaderModule(r.device, background, NULL);
   if (segment_vert) r.DestroyShaderModule(r.device, segment_vert, NULL);
   if (segment_frag) r.DestroyShaderModule(r.device, segment_frag, NULL);
   if (post) r.DestroyShaderModule(r.device, post, NULL);
   return ok;
}

static int make_frame(struct frame *f)
{
   const VkCommandBufferAllocateInfo commands = {
      .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
      .commandPool = r.pool, .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY, .commandBufferCount = 1,
   };
   const VkFenceCreateInfo fence = { .sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO };
   const VkDescriptorSetAllocateInfo set = {
      .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
      .descriptorPool = r.descriptors, .descriptorSetCount = 1, .pSetLayouts = &r.set_layout,
   };
   if (r.AllocateCommandBuffers(r.device, &commands, &f->commands) != VK_SUCCESS ||
       r.CreateFence(r.device, &fence, NULL, &f->fence) != VK_SUCCESS ||
       !mapped_buffer(NEON_GLOBALS_BYTES, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT, &f->uniforms,
                      &f->uniform_memory, &f->uniform_map, &f->uniform_coherent) ||
       !mapped_buffer((VkDeviceSize)r.max_instances * NEON_INSTANCE_BYTES,
                      VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, &f->instances, &f->instance_memory,
                      &f->instance_map, &f->instance_coherent) ||
       r.AllocateDescriptorSets(r.device, &set, &f->set) != VK_SUCCESS)
      return 0;
   const VkDescriptorBufferInfo buffer = { f->uniforms, 0, NEON_GLOBALS_BYTES };
   const VkWriteDescriptorSet write = {
      .sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, .dstSet = f->set, .dstBinding = 0,
      .descriptorCount = 1, .descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
      .pBufferInfo = &buffer,
   };
   r.UpdateDescriptorSets(r.device, 1, &write, 0, NULL);
   if (r.timing) {
      const VkQueryPoolCreateInfo queries = {
         .sType = VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO,
         .queryType = VK_QUERY_TYPE_TIMESTAMP, .queryCount = NEON_TIMESTAMPS,
      };
      if (r.CreateQueryPool(r.device, &queries, NULL, &f->queries) != VK_SUCCESS)
         f->queries = VK_NULL_HANDLE;
   }
   return 1;
}

uint32_t neon_vk_create(const struct gpu_canvas_device *d, uint32_t color_format,
                        uint32_t max_instances)
{
   if (r.live || !d || !d->device || !d->instance_proc || !max_instances)
      return NEON_BAD_ARGUMENT;
   PFN_vkGetDeviceProcAddr device_proc =
      (PFN_vkGetDeviceProcAddr)d->instance_proc(d->instance, "vkGetDeviceProcAddr");
   PFN_vkGetPhysicalDeviceMemoryProperties memory_props = (PFN_vkGetPhysicalDeviceMemoryProperties)
      d->instance_proc(d->instance, "vkGetPhysicalDeviceMemoryProperties");
   PFN_vkGetPhysicalDeviceProperties device_props = (PFN_vkGetPhysicalDeviceProperties)
      d->instance_proc(d->instance, "vkGetPhysicalDeviceProperties");
   PFN_vkGetPhysicalDeviceQueueFamilyProperties family_props =
      (PFN_vkGetPhysicalDeviceQueueFamilyProperties)
      d->instance_proc(d->instance, "vkGetPhysicalDeviceQueueFamilyProperties");
   if (!device_proc || !memory_props || !device_props || !family_props)
      return NEON_FAILED;
#define LOAD(name) \
   r.name = (PFN_vk##name)device_proc(d->device, "vk" #name); \
   if (!r.name) return NEON_FAILED
   LOAD(CreateRenderPass); LOAD(DestroyRenderPass); LOAD(CreateShaderModule); LOAD(DestroyShaderModule);
   LOAD(CreatePipelineLayout); LOAD(DestroyPipelineLayout); LOAD(CreateGraphicsPipelines);
   LOAD(DestroyPipeline); LOAD(CreateBuffer); LOAD(DestroyBuffer); LOAD(GetBufferMemoryRequirements);
   LOAD(AllocateMemory); LOAD(FreeMemory); LOAD(BindBufferMemory); LOAD(MapMemory); LOAD(UnmapMemory);
   LOAD(CreateFramebuffer); LOAD(DestroyFramebuffer);
   LOAD(CreateCommandPool); LOAD(DestroyCommandPool); LOAD(AllocateCommandBuffers);
   LOAD(ResetCommandBuffer); LOAD(BeginCommandBuffer); LOAD(EndCommandBuffer);
   LOAD(CmdBeginRenderPass); LOAD(CmdEndRenderPass); LOAD(CmdBindPipeline); LOAD(CmdBindVertexBuffers);
   LOAD(CmdSetViewport); LOAD(CmdSetScissor); LOAD(CmdDraw); LOAD(CmdBindDescriptorSets);
   LOAD(CmdResetQueryPool); LOAD(CmdWriteTimestamp);
   LOAD(CreateFence); LOAD(DestroyFence); LOAD(ResetFences); LOAD(WaitForFences); LOAD(QueueSubmit);
   LOAD(QueueWaitIdle); LOAD(FlushMappedMemoryRanges);
   LOAD(CreateDescriptorSetLayout); LOAD(DestroyDescriptorSetLayout); LOAD(CreateDescriptorPool);
   LOAD(DestroyDescriptorPool); LOAD(AllocateDescriptorSets); LOAD(UpdateDescriptorSets);
   LOAD(CreateQueryPool); LOAD(DestroyQueryPool); LOAD(GetQueryPoolResults);
#undef LOAD
   memory_props(d->physical, &r.memory);
   VkPhysicalDeviceProperties properties;
   device_props(d->physical, &properties);
   r.tick_ns = properties.limits.timestampPeriod;
   uint32_t families = 0;
   family_props(d->physical, &families, NULL);
   if (d->family < families && families <= 64u) {
      VkQueueFamilyProperties list[64];
      family_props(d->physical, &families, list);
      r.timing = list[d->family].timestampValidBits > 0 && r.tick_ns > 0.0f;
   }
   r.device = d->device;
   r.queue = d->queue;
   r.color_format = (VkFormat)color_format;
   r.max_instances = max_instances;
   r.gpu_ns = 0;
   r.live = 1; /* destroy releases whatever exists from here on */
   const VkCommandPoolCreateInfo pool = {
      .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
      .flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
      .queueFamilyIndex = d->family,
   };
   int ok = r.CreateCommandPool(r.device, &pool, NULL, &r.pool) == VK_SUCCESS &&
            make_pass() && make_layout() && make_pipelines();
   for (uint32_t n = 0; ok && n < NEON_SLOTS; n++)
      ok = make_frame(&r.frames[n]);
   if (!ok) {
      neon_vk_destroy();
      return NEON_FAILED;
   }
   return NEON_OK;
}

static int target_objects(struct frame *f, const struct neon_target *t)
{
   if (f->framebuffer && f->color_view == t->view && f->generation == t->generation)
      return 1;
   if (f->framebuffer) r.DestroyFramebuffer(r.device, f->framebuffer, NULL);
   f->framebuffer = VK_NULL_HANDLE;
   const VkImageView view = (VkImageView)(uintptr_t)t->view;
   const VkFramebufferCreateInfo framebuffer = {
      .sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO,
      .renderPass = r.pass, .attachmentCount = 1, .pAttachments = &view,
      .width = t->width, .height = t->height, .layers = 1,
   };
   if (r.CreateFramebuffer(r.device, &framebuffer, NULL, &f->framebuffer) != VK_SUCCESS)
      return 0;
   f->color_view = t->view;
   f->generation = t->generation;
   return 1;
}

/* The finished frame's GPU time, if it was timed. */
static void collect_time(struct frame *f)
{
   if (!f->queries)
      return;
   uint64_t stamps[NEON_TIMESTAMPS];
   if (r.GetQueryPoolResults(r.device, f->queries, 0, NEON_TIMESTAMPS, sizeof stamps, stamps,
                             sizeof stamps[0], VK_QUERY_RESULT_64_BIT) == VK_SUCCESS &&
       stamps[1] >= stamps[0])
      r.gpu_ns = (uint64_t)((double)(stamps[1] - stamps[0]) * (double)r.tick_ns);
}

uint32_t neon_vk_render(const struct neon_target *t, const void *globals,
                        const void *instances, uint32_t count)
{
   if (!r.live || !t || t->slot >= NEON_SLOTS || !t->image || !t->view || !t->ready ||
       !t->width || !t->height || !globals || (count && !instances) || count > r.max_instances ||
       t->format != (uint32_t)r.color_format)
      return NEON_BAD_ARGUMENT;
   struct frame *f = &r.frames[t->slot];
   if (f->submitted) {
      if (r.WaitForFences(r.device, 1, &f->fence, VK_TRUE, NEON_FRAME_TIMEOUT_NS) != VK_SUCCESS)
         return NEON_FAILED;
      f->submitted = 0;
      collect_time(f);
   }
   if (!target_objects(f, t) ||
       r.ResetFences(r.device, 1, &f->fence) != VK_SUCCESS ||
       r.ResetCommandBuffer(f->commands, 0) != VK_SUCCESS)
      return NEON_FAILED;
   memcpy(f->uniform_map, globals, NEON_GLOBALS_BYTES);
   flush(f->uniform_memory, f->uniform_coherent);
   if (count) {
      memcpy(f->instance_map, instances, (size_t)count * NEON_INSTANCE_BYTES);
      flush(f->instance_memory, f->instance_coherent);
   }
   const VkCommandBufferBeginInfo begin = {
      .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
      .flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
   };
   if (r.BeginCommandBuffer(f->commands, &begin) != VK_SUCCESS)
      return NEON_FAILED;
   if (f->queries) {
      r.CmdResetQueryPool(f->commands, f->queries, 0, NEON_TIMESTAMPS);
      r.CmdWriteTimestamp(f->commands, VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT, f->queries, 0);
   }
   const VkRenderPassBeginInfo pass = {
      .sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,
      .renderPass = r.pass, .framebuffer = f->framebuffer,
      .renderArea = { { 0, 0 }, { t->width, t->height } },
   };
   r.CmdBeginRenderPass(f->commands, &pass, VK_SUBPASS_CONTENTS_INLINE);
   const VkViewport viewport = { 0.0f, 0.0f, (float)t->width, (float)t->height, 0.0f, 1.0f };
   const VkRect2D scissor = { { 0, 0 }, { t->width, t->height } };
   r.CmdSetViewport(f->commands, 0, 1, &viewport);
   r.CmdSetScissor(f->commands, 0, 1, &scissor);
   r.CmdBindDescriptorSets(f->commands, VK_PIPELINE_BIND_POINT_GRAPHICS, r.layout, 0, 1, &f->set,
                           0, NULL);
   r.CmdBindPipeline(f->commands, VK_PIPELINE_BIND_POINT_GRAPHICS, r.pipelines[BACKGROUND]);
   r.CmdDraw(f->commands, NEON_TRIANGLE_CORNERS, 1, 0, 0);
   if (count) {
      const VkDeviceSize offset = 0;
      r.CmdBindPipeline(f->commands, VK_PIPELINE_BIND_POINT_GRAPHICS, r.pipelines[SEGMENTS]);
      r.CmdBindVertexBuffers(f->commands, 0, 1, &f->instances, &offset);
      r.CmdDraw(f->commands, NEON_QUAD_CORNERS, count, 0, 0);
   }
   r.CmdBindPipeline(f->commands, VK_PIPELINE_BIND_POINT_GRAPHICS, r.pipelines[POST_MULTIPLY]);
   r.CmdDraw(f->commands, NEON_TRIANGLE_CORNERS, 1, 0, 0);
   r.CmdBindPipeline(f->commands, VK_PIPELINE_BIND_POINT_GRAPHICS, r.pipelines[POST_ADD]);
   r.CmdDraw(f->commands, NEON_TRIANGLE_CORNERS, 1, 0, 0);
   r.CmdEndRenderPass(f->commands);
   if (f->queries)
      r.CmdWriteTimestamp(f->commands, VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, f->queries, 1);
   if (r.EndCommandBuffer(f->commands) != VK_SUCCESS)
      return NEON_FAILED;
   const VkSemaphore ready = (VkSemaphore)(uintptr_t)t->ready;
   const VkSubmitInfo submit = {
      .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
      .commandBufferCount = 1, .pCommandBuffers = &f->commands,
      .signalSemaphoreCount = 1, .pSignalSemaphores = &ready,
   };
   if (r.QueueSubmit(r.queue, 1, &submit, f->fence) != VK_SUCCESS)
      return NEON_FAILED;
   f->submitted = 1;
   return NEON_OK;
}

uint64_t neon_vk_gpu_ns(void)
{
   return r.gpu_ns;
}

void neon_vk_destroy(void)
{
   if (!r.live)
      return;
   (void)r.QueueWaitIdle(r.queue);
   for (uint32_t n = 0; n < NEON_SLOTS; n++) {
      struct frame *f = &r.frames[n];
      if (f->framebuffer) r.DestroyFramebuffer(r.device, f->framebuffer, NULL);
      if (f->fence) r.DestroyFence(r.device, f->fence, NULL);
      if (f->queries) r.DestroyQueryPool(r.device, f->queries, NULL);
      if (f->uniform_map) r.UnmapMemory(r.device, f->uniform_memory);
      if (f->instance_map) r.UnmapMemory(r.device, f->instance_memory);
      if (f->uniforms) r.DestroyBuffer(r.device, f->uniforms, NULL);
      if (f->instances) r.DestroyBuffer(r.device, f->instances, NULL);
      if (f->uniform_memory) r.FreeMemory(r.device, f->uniform_memory, NULL);
      if (f->instance_memory) r.FreeMemory(r.device, f->instance_memory, NULL);
      *f = (struct frame){0};
   }
   if (r.pool) r.DestroyCommandPool(r.device, r.pool, NULL);
   for (uint32_t n = 0; n < PIPELINES; n++)
      if (r.pipelines[n]) r.DestroyPipeline(r.device, r.pipelines[n], NULL);
   if (r.layout) r.DestroyPipelineLayout(r.device, r.layout, NULL);
   if (r.descriptors) r.DestroyDescriptorPool(r.device, r.descriptors, NULL);
   if (r.set_layout) r.DestroyDescriptorSetLayout(r.device, r.set_layout, NULL);
   if (r.pass) r.DestroyRenderPass(r.device, r.pass, NULL);
   r.pool = VK_NULL_HANDLE;
   for (uint32_t n = 0; n < PIPELINES; n++) r.pipelines[n] = VK_NULL_HANDLE;
   r.layout = VK_NULL_HANDLE; r.descriptors = VK_NULL_HANDLE;
   r.set_layout = VK_NULL_HANDLE; r.pass = VK_NULL_HANDLE;
   r.live = 0;
}
