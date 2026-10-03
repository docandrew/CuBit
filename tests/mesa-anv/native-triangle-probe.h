/* Offscreen Vulkan draw through Mesa. No display ownership or raw GPU packets. */
#include <vulkan/vulkan.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include "triangle-shaders.h"
#include "completed-image.h"
#include "probe-timing.h"
#ifdef CUBIT_TEST_COMPOSITOR
/* Test the production boundary unchanged, not a second Vulkan backend. */
#include "../../userspace/lib/compositor/vulkan_submission_native.c"
#endif

static uint32_t triangle_memory_type(const VkPhysicalDeviceMemoryProperties *p,
                                     uint32_t bits, VkMemoryPropertyFlags flags)
{
   for (uint32_t n=0; n<p->memoryTypeCount && n<32; ++n)
      if ((bits & (1u<<n)) && (p->memoryTypes[n].propertyFlags & flags)==flags)
         return n;
   return UINT32_MAX;
}

static VkResult
mesa_triangle_probe_with_source(VkInstance instance, VkPhysicalDevice physical, VkDevice device,
                    PFN_vkGetInstanceProcAddr instance_proc,
                    void (*log_message)(const char *, ...),
                    /* Optional synchronous completed-buffer consumer. The
                     * allocation, buffer and device remain alive throughout.
                     * CPU mapping is removed before entry. Any exported loans
                     * MUST be confirmed retired before returning, including
                     * on error; uncertain consumers must retain/wait instead.
                     * No ownership is transferred by this callback alone. */
                    VkResult (*consume)(VkDevice, VkDeviceMemory, VkDeviceSize,
                                        uint32_t, uint32_t, uint32_t),
                    mesa_completed_image_consumer consume_image)
{
   PFN_vkGetDeviceProcAddr proc = (PFN_vkGetDeviceProcAddr)
      instance_proc(instance, "vkGetDeviceProcAddr");
   PFN_vkGetPhysicalDeviceMemoryProperties get_memory =
      (PFN_vkGetPhysicalDeviceMemoryProperties)
      instance_proc(instance, "vkGetPhysicalDeviceMemoryProperties");
   PFN_vkGetPhysicalDeviceFormatProperties get_format =
      (PFN_vkGetPhysicalDeviceFormatProperties)
      instance_proc(instance, "vkGetPhysicalDeviceFormatProperties");
   if (!proc || !get_memory || !get_format) {
      log_message("MESA-TRIANGLE missing instance dispatch device-proc=%u memory=%u format=%u\n",
                  proc != NULL, get_memory != NULL, get_format != NULL);
      return VK_ERROR_INITIALIZATION_FAILED;
   }
#define LOAD(n) PFN_vk##n n=(PFN_vk##n)proc(device,"vk" #n); \
   if (!n) { \
      log_message("MESA-TRIANGLE missing device dispatch vk%s\n", #n); \
      return VK_ERROR_INITIALIZATION_FAILED; \
   }
   LOAD(CreateImage); LOAD(DestroyImage); LOAD(GetImageMemoryRequirements);
   LOAD(AllocateMemory); LOAD(FreeMemory); LOAD(BindImageMemory);
   LOAD(CreateBuffer); LOAD(DestroyBuffer); LOAD(GetBufferMemoryRequirements);
   LOAD(BindBufferMemory); LOAD(MapMemory); LOAD(UnmapMemory);
   LOAD(CreateImageView); LOAD(DestroyImageView);
   LOAD(CreateRenderPass); LOAD(DestroyRenderPass);
   LOAD(CreateFramebuffer); LOAD(DestroyFramebuffer);
   LOAD(CreateShaderModule); LOAD(DestroyShaderModule);
   LOAD(CreatePipelineLayout); LOAD(DestroyPipelineLayout);
   LOAD(CreateGraphicsPipelines); LOAD(DestroyPipeline);
   LOAD(CreateCommandPool); LOAD(DestroyCommandPool); LOAD(AllocateCommandBuffers);
   LOAD(BeginCommandBuffer); LOAD(EndCommandBuffer);
   LOAD(CmdBeginRenderPass); LOAD(CmdEndRenderPass); LOAD(CmdBindPipeline); LOAD(CmdDraw);
   LOAD(CmdPipelineBarrier); LOAD(CmdCopyImageToBuffer);
   LOAD(CreateFence); LOAD(DestroyFence); LOAD(GetDeviceQueue);
   LOAD(QueueSubmit); LOAD(WaitForFences); LOAD(DeviceWaitIdle);
#undef LOAD
   VkResult result=VK_SUCCESS;
   struct probe_timing timing=probe_timing_start();
   VkImage image=VK_NULL_HANDLE;
   VkBuffer buffer=VK_NULL_HANDLE;
   VkDeviceMemory image_memory=VK_NULL_HANDLE, host_memory=VK_NULL_HANDLE;
   VkImageView view=VK_NULL_HANDLE;
   VkRenderPass pass=VK_NULL_HANDLE;
   VkFramebuffer framebuffer=VK_NULL_HANDLE;
   VkShaderModule vertex=VK_NULL_HANDLE, fragment=VK_NULL_HANDLE;
   VkPipelineLayout layout=VK_NULL_HANDLE;
   VkPipeline pipeline=VK_NULL_HANDLE;
   VkCommandPool pool=VK_NULL_HANDLE;
   VkCommandBuffer command=VK_NULL_HANDLE;
   VkFence fence=VK_NULL_HANDLE;
   VkQueue queue=VK_NULL_HANDLE;
   void *mapped=NULL;
   const uint32_t width=64, height=64;
   const VkDeviceSize bytes=64*64*4;
   /* Match Desktop's completed-linear BGRA8888 contract. Vulkan shaders and
    * clear values still use logical RGBA; the image-to-buffer copy preserves
    * BGRA storage bytes. No CPU swizzle/copy is needed for later presentation.
    * This remains an offscreen probe, not an exported Desktop buffer. */
   const VkFormat format=VK_FORMAT_B8G8R8A8_UNORM;
   VkFormatProperties support;
   get_format(physical, format, &support);
   const VkFormatFeatureFlags required=VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT |
      (consume_image ? VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT : 0);
   if ((support.optimalTilingFeatures & required)!=required)
      return VK_ERROR_FORMAT_NOT_SUPPORTED;
   VkPhysicalDeviceMemoryProperties memory;
   get_memory(physical, &memory);
#define TRY(expr) do { result=(expr); if (result!=VK_SUCCESS) { \
   log_message("MESA-TRIANGLE failed %s result=%d\n", #expr, result); goto cleanup; } } while (0)
   const VkImageCreateInfo image_info={
      .sType=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO, .imageType=VK_IMAGE_TYPE_2D,
      .format=format, .extent={64,64,1}, .mipLevels=1, .arrayLayers=1,
      .samples=VK_SAMPLE_COUNT_1_BIT, .tiling=VK_IMAGE_TILING_OPTIMAL,
      .usage=VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT|VK_IMAGE_USAGE_TRANSFER_SRC_BIT |
         (consume_image ? VK_IMAGE_USAGE_SAMPLED_BIT : 0),
      .sharingMode=VK_SHARING_MODE_EXCLUSIVE, .initialLayout=VK_IMAGE_LAYOUT_UNDEFINED,
   };
   TRY(CreateImage(device,&image_info,NULL,&image));
   VkMemoryRequirements req;
   GetImageMemoryRequirements(device,image,&req);
   uint32_t type=triangle_memory_type(&memory,req.memoryTypeBits,0);
   if (type==UINT32_MAX) { result=VK_ERROR_FEATURE_NOT_PRESENT; goto cleanup; }
   VkMemoryAllocateInfo allocation={.sType=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
                                    .allocationSize=req.size,.memoryTypeIndex=type};
   log_message("MESA-TRIANGLE image allocation bytes=%llu alignment=%llu type=%u bits=%x\n",
               (unsigned long long)req.size, (unsigned long long)req.alignment,
               type, req.memoryTypeBits);
   TRY(AllocateMemory(device,&allocation,NULL,&image_memory));
   TRY(BindImageMemory(device,image,image_memory,0));
   const VkBufferCreateInfo buffer_info={.sType=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
      .size=bytes,.usage=VK_BUFFER_USAGE_TRANSFER_DST_BIT,.sharingMode=VK_SHARING_MODE_EXCLUSIVE};
   TRY(CreateBuffer(device,&buffer_info,NULL,&buffer));
   GetBufferMemoryRequirements(device,buffer,&req);
   type=triangle_memory_type(&memory,req.memoryTypeBits,
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
   if (type==UINT32_MAX || req.size<bytes) { result=VK_ERROR_FEATURE_NOT_PRESENT; goto cleanup; }
   allocation.allocationSize=req.size; allocation.memoryTypeIndex=type;
   TRY(AllocateMemory(device,&allocation,NULL,&host_memory));
   TRY(BindBufferMemory(device,buffer,host_memory,0));
   TRY(MapMemory(device,host_memory,0,bytes,0,&mapped));
   memset(mapped,0,(size_t)bytes);
   const VkImageViewCreateInfo view_info={.sType=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
      .image=image,.viewType=VK_IMAGE_VIEW_TYPE_2D,.format=format,
      .subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
   TRY(CreateImageView(device,&view_info,NULL,&view));
   const VkAttachmentDescription attachment={.format=format,.samples=VK_SAMPLE_COUNT_1_BIT,
      .loadOp=VK_ATTACHMENT_LOAD_OP_CLEAR,.storeOp=VK_ATTACHMENT_STORE_OP_STORE,
      .stencilLoadOp=VK_ATTACHMENT_LOAD_OP_DONT_CARE,.stencilStoreOp=VK_ATTACHMENT_STORE_OP_DONT_CARE,
      .initialLayout=VK_IMAGE_LAYOUT_UNDEFINED,.finalLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL};
   const VkAttachmentReference reference={0,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL};
   const VkSubpassDescription subpass={.pipelineBindPoint=VK_PIPELINE_BIND_POINT_GRAPHICS,
      .colorAttachmentCount=1,.pColorAttachments=&reference};
   const VkRenderPassCreateInfo pass_info={.sType=VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO,
      .attachmentCount=1,.pAttachments=&attachment,.subpassCount=1,.pSubpasses=&subpass};
   TRY(CreateRenderPass(device,&pass_info,NULL,&pass));
   const VkFramebufferCreateInfo fb_info={.sType=VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO,
      .renderPass=pass,.attachmentCount=1,.pAttachments=&view,.width=width,.height=height,.layers=1};
   TRY(CreateFramebuffer(device,&fb_info,NULL,&framebuffer));
   VkShaderModuleCreateInfo shader={.sType=VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
      .codeSize=sizeof(triangle_vertex),.pCode=triangle_vertex};
   TRY(CreateShaderModule(device,&shader,NULL,&vertex));
   shader.codeSize=sizeof(triangle_fragment); shader.pCode=triangle_fragment;
   TRY(CreateShaderModule(device,&shader,NULL,&fragment));
   const VkPipelineShaderStageCreateInfo stages[2]={
      {.sType=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
       .stage=VK_SHADER_STAGE_VERTEX_BIT,.module=vertex,.pName="main"},
      {.sType=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
       .stage=VK_SHADER_STAGE_FRAGMENT_BIT,.module=fragment,.pName="main"}};
   const VkPipelineVertexInputStateCreateInfo input={.sType=VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO};
   const VkPipelineInputAssemblyStateCreateInfo assembly={
      .sType=VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
      .topology=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST};
   const VkViewport viewport={0,0,64,64,0,1};
   const VkRect2D scissor={{0,0},{64,64}};
   const VkPipelineViewportStateCreateInfo viewport_state={
      .sType=VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,
      .viewportCount=1,.pViewports=&viewport,.scissorCount=1,.pScissors=&scissor};
   const VkPipelineRasterizationStateCreateInfo raster={
      .sType=VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
      .polygonMode=VK_POLYGON_MODE_FILL,.cullMode=VK_CULL_MODE_NONE,
      .frontFace=VK_FRONT_FACE_COUNTER_CLOCKWISE,.lineWidth=1.0f};
   const VkPipelineMultisampleStateCreateInfo samples={
      .sType=VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,.rasterizationSamples=VK_SAMPLE_COUNT_1_BIT};
   const VkPipelineColorBlendAttachmentState blend_attachment={
      .colorWriteMask=VK_COLOR_COMPONENT_R_BIT|VK_COLOR_COMPONENT_G_BIT|VK_COLOR_COMPONENT_B_BIT|VK_COLOR_COMPONENT_A_BIT};
   const VkPipelineColorBlendStateCreateInfo blend={
      .sType=VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,
      .attachmentCount=1,.pAttachments=&blend_attachment};
   const VkPipelineLayoutCreateInfo layout_info={.sType=VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO};
   TRY(CreatePipelineLayout(device,&layout_info,NULL,&layout));
   const VkGraphicsPipelineCreateInfo graphics={.sType=VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,
      .stageCount=2,.pStages=stages,.pVertexInputState=&input,.pInputAssemblyState=&assembly,
      .pViewportState=&viewport_state,.pRasterizationState=&raster,.pMultisampleState=&samples,
      .pColorBlendState=&blend,.layout=layout,.renderPass=pass,.subpass=0};
   log_message("MESA-TRIANGLE pipeline compilation beginning\n");
   probe_timing_mark(&timing,PROBE_PIPELINE);
   TRY(CreateGraphicsPipelines(device,VK_NULL_HANDLE,1,&graphics,NULL,&pipeline));
   probe_timing_mark(&timing,PROBE_RECORD);
   log_message("MESA-TRIANGLE pipeline ready\n");
   const VkCommandPoolCreateInfo pool_info={.sType=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
      .flags=VK_COMMAND_POOL_CREATE_TRANSIENT_BIT|VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,.queueFamilyIndex=0};
   TRY(CreateCommandPool(device,&pool_info,NULL,&pool));
   const VkCommandBufferAllocateInfo cmd_info={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
      .commandPool=pool,.level=VK_COMMAND_BUFFER_LEVEL_PRIMARY,.commandBufferCount=1};
   TRY(AllocateCommandBuffers(device,&cmd_info,&command));
   const VkFenceCreateInfo fence_info={.sType=VK_STRUCTURE_TYPE_FENCE_CREATE_INFO};
   TRY(CreateFence(device,&fence_info,NULL,&fence));
   GetDeviceQueue(device,0,0,&queue);
   if (!queue) { result=VK_ERROR_INITIALIZATION_FAILED; goto cleanup; }
#ifdef CUBIT_TEST_COMPOSITOR
   struct cubit_vulkan_submission compositor;
#define COMPOSE(expr) do { if ((expr)!=0) { result=VK_ERROR_UNKNOWN; \
   log_message("MESA-COMPOSITOR failed %s\n", #expr); goto cleanup; } } while (0)
   COMPOSE(cubit_vulkan_submission_init(&compositor,device,queue,command,fence,proc));
   COMPOSE(cubit_vulkan_submission_start(&compositor));
#else
   const VkCommandBufferBeginInfo begin={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
      .flags=VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT};
   TRY(BeginCommandBuffer(command,&begin));
#endif
   const VkClearValue clear={.color={{0.0f,0.0f,1.0f,1.0f}}};
   const VkRenderPassBeginInfo render={.sType=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,
      .renderPass=pass,.framebuffer=framebuffer,.renderArea={{0,0},{64,64}},
      .clearValueCount=1,.pClearValues=&clear};
#ifdef CUBIT_TEST_COMPOSITOR
   struct cubit_vulkan_scene scene={.device=device,.begin=render};
   COMPOSE(cubit_vulkan_submission_begin_scene(&compositor,&scene,width,height));
#else
   CmdBeginRenderPass(command,&render,VK_SUBPASS_CONTENTS_INLINE);
#endif
   CmdBindPipeline(command,VK_PIPELINE_BIND_POINT_GRAPHICS,pipeline);
   CmdDraw(command,3,1,0,0);
#ifdef CUBIT_TEST_COMPOSITOR
   COMPOSE(cubit_vulkan_submission_fill(&compositor,width,height,4,4,12,12,0x00ff00));
   COMPOSE(cubit_vulkan_submission_end_scene(&compositor));
#else
   CmdEndRenderPass(command);
#endif
   const VkImageMemoryBarrier image_barrier={.sType=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,
      .srcAccessMask=VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,.dstAccessMask=VK_ACCESS_TRANSFER_READ_BIT,
      .oldLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,.newLayout=VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
      .srcQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.dstQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,
      .image=image,.subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
   CmdPipelineBarrier(command,VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
      VK_PIPELINE_STAGE_TRANSFER_BIT,0,0,NULL,0,NULL,1,&image_barrier);
   const VkBufferImageCopy copy={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},
      .imageExtent={64,64,1}};
   CmdCopyImageToBuffer(command,image,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,buffer,1,&copy);
   const VkBufferMemoryBarrier host_barrier={.sType=VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
      .srcAccessMask=VK_ACCESS_TRANSFER_WRITE_BIT,.dstAccessMask=VK_ACCESS_HOST_READ_BIT,
      .srcQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.dstQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,
      .buffer=buffer,.size=bytes};
   CmdPipelineBarrier(command,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_HOST_BIT,
      0,0,NULL,1,&host_barrier,0,NULL);
#ifdef CUBIT_TEST_COMPOSITOR
   COMPOSE(cubit_vulkan_submission_seal(&compositor));
   log_message("MESA-COMPOSITOR production submission beginning\n");
   probe_timing_mark(&timing,PROBE_SUBMIT);
   result=cubit_vulkan_submission_submit(&compositor)==0 ? VK_SUCCESS : VK_ERROR_UNKNOWN;
#else
   TRY(EndCommandBuffer(command));
   const VkSubmitInfo submit={.sType=VK_STRUCTURE_TYPE_SUBMIT_INFO,
      .commandBufferCount=1,.pCommandBuffers=&command};
   log_message("MESA-TRIANGLE submit beginning\n");
   probe_timing_mark(&timing,PROBE_SUBMIT);
   result=QueueSubmit(queue,1,&submit,fence);
#endif
   probe_timing_mark(&timing,PROBE_WAIT);
   if (result!=VK_SUCCESS) {
      VkResult idle;
      do {
         idle=DeviceWaitIdle(device);
         if (idle!=VK_SUCCESS && idle!=VK_ERROR_DEVICE_LOST) usleep(100000);
      } while (idle!=VK_SUCCESS && idle!=VK_ERROR_DEVICE_LOST);
      goto cleanup;
   }
   result=WaitForFences(device,1,&fence,VK_TRUE,1000000000ull);
   if (result!=VK_SUCCESS && result!=VK_ERROR_DEVICE_LOST) {
      log_message("MESA-TRIANGLE pending; resources retained (no resubmit)\n");
      do {
         usleep(100000);
         result=WaitForFences(device,1,&fence,VK_TRUE,1000000000ull);
      } while (result!=VK_SUCCESS && result!=VK_ERROR_DEVICE_LOST);
   }
   if (result!=VK_SUCCESS) goto cleanup;
   probe_timing_mark(&timing,PROBE_READBACK);
#ifdef CUBIT_TEST_COMPOSITOR
   COMPOSE(cubit_vulkan_submission_poll(&compositor));
#endif
   const volatile uint8_t *pixels=mapped;
   uint32_t red=0, blue=0, other=0, misplaced=0;
   uint32_t green=0;
   for (uint32_t i=0; i<width*height; ++i) {
      const volatile uint8_t *p=pixels+4*i;
      if (p[0]==0 && p[1]==0 && p[2]==255 && p[3]==255) red++;
      else if (p[0]==255 && p[1]==0 && p[2]==0 && p[3]==255) blue++;
      else if (p[0]==0 && p[1]==255 && p[2]==0 && p[3]==255) green++;
      else other++;
      /* Viewport-space vertices (8,8),(56,8),(32,56). Doubled pixel
       * centers are odd; no sample lies exactly on any triangle edge. */
      const int32_t x=2*(int32_t)(i%width)+1, y=2*(int32_t)(i/width)+1;
      const int inside=y>16 && 2*x-y>16 && 2*x+y<240;
      int fill=0;
#ifdef CUBIT_TEST_COMPOSITOR
      fill=i%width>=4 && i%width<12 && i/width>=4 && i/width<12;
#endif
      if (p[0]!=(fill || inside ? 0 : 255) || p[1]!=(fill ? 255 : 0) ||
          p[2]!=(!fill && inside ? 255 : 0) || p[3]!=255) misplaced++;
   }
   const volatile uint8_t *center=pixels+4*(32*64+32);
   log_message("MESA-TRIANGLE storage=BGRA8 pitch=256 (NOT exported)\n");
   log_message("MESA-TRIANGLE readback red=%u blue=%u other=%u\n",red,blue,other);
   log_message("MESA-TRIANGLE pixel mismatches=%u expected=0\n",misplaced);
#ifdef CUBIT_TEST_COMPOSITOR
   log_message("MESA-COMPOSITOR green=%u expected=64 mismatches=%u\n",green,misplaced);
   if (green!=64) misplaced++;
#else
   if (green!=0) misplaced++;
#endif
   if (red==0 || blue==0 || other || misplaced || center[0]!=0 || center[2]!=255 ||
       pixels[0]!=255 || pixels[2]!=0)
      result=VK_ERROR_UNKNOWN;
   if (result==VK_SUCCESS && (consume || consume_image)) {
      UnmapMemory(device,host_memory);
      mapped=NULL;
      /* Native unmap may report device loss through the device rather than
       * this void Vulkan API. The native consumer must check that state before
       * requesting its grant; the driver independently excludes live writers. */
      if (consume_image) {
         const struct mesa_completed_image source={instance,physical,device,
            instance_proc,queue,image,view,width,height,host_memory,bytes};
         result=consume_image(&source,consume);
      } else result=consume(device,host_memory,bytes,width,height,width*4);
   }
cleanup:
   probe_timing_mark(&timing,PROBE_CLEANUP);
   if (fence) DestroyFence(device,fence,NULL);
   if (pool) DestroyCommandPool(device,pool,NULL);
   if (pipeline) DestroyPipeline(device,pipeline,NULL);
   if (layout) DestroyPipelineLayout(device,layout,NULL);
   if (fragment) DestroyShaderModule(device,fragment,NULL);
   if (vertex) DestroyShaderModule(device,vertex,NULL);
   if (framebuffer) DestroyFramebuffer(device,framebuffer,NULL);
   if (pass) DestroyRenderPass(device,pass,NULL);
   if (view) DestroyImageView(device,view,NULL);
   if (image) DestroyImage(device,image,NULL);
   if (image_memory) FreeMemory(device,image_memory,NULL);
   if (mapped) UnmapMemory(device,host_memory);
   if (buffer) DestroyBuffer(device,buffer,NULL);
   if (host_memory) FreeMemory(device,host_memory,NULL);
   probe_timing_mark(&timing,PROBE_CLEANUP);
   /* This synchronous fixture is driven by one caller. Sample first/every32
    * calls, plus failures, rather than flood logstore with every iteration. */
   static unsigned timing_calls;
   const unsigned timing_call=++timing_calls;
   if (timing_call==1 || timing_call%32==0 || result!=VK_SUCCESS) {
   if (timing.valid) {
      log_message("MESA-TIMING call=%u cpu-us setup=%llu pipeline=%llu record=%llu\n", timing_call,
         (unsigned long long)(timing.ns[PROBE_SETUP]/1000),
         (unsigned long long)(timing.ns[PROBE_PIPELINE]/1000),
         (unsigned long long)(timing.ns[PROBE_RECORD]/1000));
      log_message("MESA-TIMING call=%u cpu-us submit=%llu wait=%llu readback-consumer=%llu cleanup=%llu result=%d\n", timing_call,
         (unsigned long long)(timing.ns[PROBE_SUBMIT]/1000),
         (unsigned long long)(timing.ns[PROBE_WAIT]/1000),
         (unsigned long long)(timing.ns[PROBE_READBACK]/1000),
         (unsigned long long)(timing.ns[PROBE_CLEANUP]/1000), result);
   } else log_message("MESA-TIMING unavailable (CPU clock sample invalid)\n");
   }
#undef TRY
#ifdef CUBIT_TEST_COMPOSITOR
#undef COMPOSE
#endif
   return result;
}

static inline VkResult mesa_triangle_probe(VkInstance instance,VkPhysicalDevice physical,
   VkDevice device,PFN_vkGetInstanceProcAddr get,void (*log)(const char *,...),
   mesa_completed_pixels consume)
{
   return mesa_triangle_probe_with_source(instance,physical,device,get,log,consume,NULL);
}
