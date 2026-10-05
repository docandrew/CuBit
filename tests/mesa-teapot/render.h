/* Real Vulkan mesh/depth probe, shared by hosted and native harnesses.
 * Generated assets contain vertices and SPIR-V, not Intel command packets.
 * Synchronous consumer must retire all loans before returning. */
#include <vulkan/vulkan.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#ifdef CUBIT_TEAPOT_GALLERY
#include <time.h>
static uint64_t teapot_clock_ns(void)
{
    struct timespec t;
    return clock_gettime(CLOCK_MONOTONIC,&t)==0 ?
        (uint64_t)t.tv_sec*1000000000ull+(uint64_t)t.tv_nsec : 0;
}
/* Startup wall-clock intervals, not GPU timestamps. Keep these separate from
 * the sustained frame series, and never subtract the zero failure sentinel. */
static void teapot_startup_stage(void (*log)(const char *,...),const char *stage,
                                 uint64_t start,uint64_t end)
{
    const int valid=start!=0 && end!=0 && end>=start;
    log("MESA-GALLERY startup stage=%s ns=%llu CPU-clock=%d\n",stage,
        (unsigned long long)(valid?end-start:0),valid);
}
#endif
#include "teapot-assets.h"
#ifndef CUBIT_TEAPOT_ASSET_GALLERY
#error "teapot asset mode missing: regenerate with build-assets.py"
#elif defined(CUBIT_TEAPOT_GALLERY)
_Static_assert(CUBIT_TEAPOT_ASSET_GALLERY == 1,
               "teapot asset mode mismatch: gallery renderer requires gallery shaders");
#else
_Static_assert(CUBIT_TEAPOT_ASSET_GALLERY == 0,
               "teapot asset mode mismatch: single renderer requires single shaders");
#endif
#include "../mesa-anv/completed-image.h"
#ifndef CUBIT_TEAPOT_FRAME_COUNT
#define CUBIT_TEAPOT_FRAME_COUNT 1
#endif
_Static_assert(CUBIT_TEAPOT_FRAME_COUNT >= 1 && CUBIT_TEAPOT_FRAME_COUNT <= 3600,
               "teapot reuse probe requires 1..3600 frames");

static uint32_t mesh_memory(const VkPhysicalDeviceMemoryProperties *p,
                            uint32_t bits, VkMemoryPropertyFlags flags)
{
    for(uint32_t n=0;n<p->memoryTypeCount && n<32;n++)
        if((bits&(1u<<n)) && (p->memoryTypes[n].propertyFlags&flags)==flags) return n;
    return UINT32_MAX;
}

static VkResult mesa_teapot_probe_with_source(VkInstance instance,VkPhysicalDevice physical,VkDevice device,
    PFN_vkGetInstanceProcAddr get,void (*log)(const char *,...),
    VkResult (*consume)(VkDevice,VkDeviceMemory,VkDeviceSize,uint32_t,uint32_t,uint32_t),
    mesa_completed_image_consumer consume_image)
{
    PFN_vkGetDeviceProcAddr proc=(PFN_vkGetDeviceProcAddr)get(instance,"vkGetDeviceProcAddr");
    PFN_vkGetPhysicalDeviceMemoryProperties memory_props=(PFN_vkGetPhysicalDeviceMemoryProperties)get(instance,"vkGetPhysicalDeviceMemoryProperties");
    PFN_vkGetPhysicalDeviceFormatProperties format_props=(PFN_vkGetPhysicalDeviceFormatProperties)get(instance,"vkGetPhysicalDeviceFormatProperties");
    if(!proc||!memory_props||!format_props)return VK_ERROR_INITIALIZATION_FAILED;
#define LOAD(n) PFN_vk##n n=(PFN_vk##n)proc(device,"vk" #n); if(!n)return VK_ERROR_INITIALIZATION_FAILED
    LOAD(CreateImage);LOAD(DestroyImage);LOAD(GetImageMemoryRequirements);LOAD(BindImageMemory);
    LOAD(CreateImageView);LOAD(DestroyImageView);LOAD(AllocateMemory);LOAD(FreeMemory);
    LOAD(CreateBuffer);LOAD(DestroyBuffer);LOAD(GetBufferMemoryRequirements);LOAD(BindBufferMemory);
    LOAD(MapMemory);LOAD(UnmapMemory);LOAD(CreateRenderPass);LOAD(DestroyRenderPass);
    LOAD(CreateFramebuffer);LOAD(DestroyFramebuffer);LOAD(CreateShaderModule);LOAD(DestroyShaderModule);
    LOAD(CreatePipelineLayout);LOAD(DestroyPipelineLayout);LOAD(CreateGraphicsPipelines);LOAD(DestroyPipeline);
    LOAD(CreateCommandPool);LOAD(DestroyCommandPool);LOAD(AllocateCommandBuffers);LOAD(BeginCommandBuffer);LOAD(EndCommandBuffer);
#ifdef CUBIT_TEAPOT_GALLERY
    LOAD(ResetCommandBuffer);
#endif
    LOAD(CmdBeginRenderPass);LOAD(CmdEndRenderPass);LOAD(CmdBindPipeline);LOAD(CmdBindVertexBuffers);
    LOAD(CmdPushConstants);LOAD(CmdDraw);LOAD(CmdPipelineBarrier);LOAD(CmdCopyImageToBuffer);
    LOAD(GetDeviceQueue);LOAD(CreateFence);LOAD(DestroyFence);LOAD(QueueSubmit);LOAD(WaitForFences);LOAD(DeviceWaitIdle);
#if CUBIT_TEAPOT_FRAME_COUNT > 1
    LOAD(ResetFences);
#endif
#undef LOAD
    VkResult result=VK_SUCCESS;
    VkImage images[2]={0};VkImageView views[2]={0};VkDeviceMemory image_memory[2]={0};
    VkBuffer buffers[2]={0};VkDeviceMemory buffer_memory[2]={0};void *maps[2]={0};
    VkShaderModule modules[2]={0};VkRenderPass pass=0;VkFramebuffer framebuffer=0;
    VkPipelineLayout layout=0;VkPipeline pipeline=0;VkCommandPool pool=0;VkCommandBuffer command=0;
    VkQueue queue=0;VkFence fence=0;
#ifdef CUBIT_TEAPOT_GALLERY
    const uint32_t width=800,height=600;
    log("MESA-GALLERY startup resources beginning\n");
    const uint64_t resources_start=teapot_clock_ns();
    uint64_t pipeline_start=0;
    uint64_t animation_start=0;
    uint64_t submit_ns=0,consumer_ns=0,submit_start=0,consumer_start=0;
    uint64_t previous_finish=0;
    uint64_t peak_frame_ns=0,peak_work_ns=0,peak_present_ns=0;
    unsigned peak_frame=0;
    int timing_valid=1;
#else
    const uint32_t width=256,height=256;
#endif
    const VkDeviceSize sizes[2]={sizeof(teapot_vertices),(VkDeviceSize)width*height*4};
    VkPhysicalDeviceMemoryProperties memory;memory_props(physical,&memory);
    const VkFormat formats[2]={VK_FORMAT_B8G8R8A8_UNORM,VK_FORMAT_D32_SFLOAT};
#define TRY(expr) do {result=(expr);if(result!=VK_SUCCESS){log("MESA-TEAPOT failed %s result=%d\n",#expr,result);goto cleanup;}}while(0)
    for(unsigned i=0;i<2;i++){
        VkFormatProperties support;format_props(physical,formats[i],&support);
        VkFormatFeatureFlags need=i?VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT;
        if(!i && consume_image)need|=VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT;
        if((support.optimalTilingFeatures&need)!=need){result=VK_ERROR_FORMAT_NOT_SUPPORTED;goto cleanup;}
        const VkImageCreateInfo info={.sType=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,.imageType=VK_IMAGE_TYPE_2D,
            .format=formats[i],.extent={width,height,1},.mipLevels=1,.arrayLayers=1,.samples=VK_SAMPLE_COUNT_1_BIT,
            .tiling=VK_IMAGE_TILING_OPTIMAL,.usage=i?VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT:
            VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT|VK_IMAGE_USAGE_TRANSFER_SRC_BIT |
            (consume_image?VK_IMAGE_USAGE_SAMPLED_BIT:0),.sharingMode=VK_SHARING_MODE_EXCLUSIVE};
        TRY(CreateImage(device,&info,0,&images[i]));
        VkMemoryRequirements req;GetImageMemoryRequirements(device,images[i],&req);
        log("MESA-TEAPOT %s allocation bytes=%llu alignment=%llu\n",i?"depth":"color",
            (unsigned long long)req.size,(unsigned long long)req.alignment);
        uint32_t type=mesh_memory(&memory,req.memoryTypeBits,0);
        if(type==UINT32_MAX){result=VK_ERROR_FEATURE_NOT_PRESENT;goto cleanup;}
        const VkMemoryAllocateInfo allocation={.sType=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,.allocationSize=req.size,.memoryTypeIndex=type};
        TRY(AllocateMemory(device,&allocation,0,&image_memory[i]));
        TRY(BindImageMemory(device,images[i],image_memory[i],0));
        const VkImageViewCreateInfo view={.sType=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,.image=images[i],
            .viewType=VK_IMAGE_VIEW_TYPE_2D,.format=formats[i],.subresourceRange={i?VK_IMAGE_ASPECT_DEPTH_BIT:VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
        TRY(CreateImageView(device,&view,0,&views[i]));
        const VkBufferCreateInfo buf={.sType=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,.size=sizes[i],
            .usage=i?VK_BUFFER_USAGE_TRANSFER_DST_BIT:VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,.sharingMode=VK_SHARING_MODE_EXCLUSIVE};
        TRY(CreateBuffer(device,&buf,0,&buffers[i]));GetBufferMemoryRequirements(device,buffers[i],&req);
        type=mesh_memory(&memory,req.memoryTypeBits,VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
        if(type==UINT32_MAX){result=VK_ERROR_FEATURE_NOT_PRESENT;goto cleanup;}
        const VkMemoryAllocateInfo ba={.sType=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,.allocationSize=req.size,.memoryTypeIndex=type};
        TRY(AllocateMemory(device,&ba,0,&buffer_memory[i]));TRY(BindBufferMemory(device,buffers[i],buffer_memory[i],0));
        TRY(MapMemory(device,buffer_memory[i],0,sizes[i],0,&maps[i]));
    }
    memcpy(maps[0],teapot_vertices,sizeof(teapot_vertices));
    UnmapMemory(device,buffer_memory[0]);maps[0]=0;
    memset(maps[1],0,(size_t)sizes[1]);
#ifdef CUBIT_TEAPOT_GALLERY
    teapot_startup_stage(log,"resources",resources_start,teapot_clock_ns());
#endif
    const VkAttachmentDescription attachments[2]={
        {.format=formats[0],.samples=VK_SAMPLE_COUNT_1_BIT,.loadOp=VK_ATTACHMENT_LOAD_OP_CLEAR,.storeOp=VK_ATTACHMENT_STORE_OP_STORE,
         .stencilLoadOp=VK_ATTACHMENT_LOAD_OP_DONT_CARE,.stencilStoreOp=VK_ATTACHMENT_STORE_OP_DONT_CARE,
         .initialLayout=VK_IMAGE_LAYOUT_UNDEFINED,.finalLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL},
        {.format=formats[1],.samples=VK_SAMPLE_COUNT_1_BIT,.loadOp=VK_ATTACHMENT_LOAD_OP_CLEAR,.storeOp=VK_ATTACHMENT_STORE_OP_DONT_CARE,
         .stencilLoadOp=VK_ATTACHMENT_LOAD_OP_DONT_CARE,.stencilStoreOp=VK_ATTACHMENT_STORE_OP_DONT_CARE,
         .initialLayout=VK_IMAGE_LAYOUT_UNDEFINED,.finalLayout=VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL}};
    const VkAttachmentReference color={0,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL},depth={1,VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL};
    const VkSubpassDescription sub={.pipelineBindPoint=VK_PIPELINE_BIND_POINT_GRAPHICS,.colorAttachmentCount=1,.pColorAttachments=&color,.pDepthStencilAttachment=&depth};
    const VkRenderPassCreateInfo rp={.sType=VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO,.attachmentCount=2,.pAttachments=attachments,.subpassCount=1,.pSubpasses=&sub};
    TRY(CreateRenderPass(device,&rp,0,&pass));
    const VkFramebufferCreateInfo fb={.sType=VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO,.renderPass=pass,.attachmentCount=2,.pAttachments=views,.width=width,.height=height,.layers=1};
    TRY(CreateFramebuffer(device,&fb,0,&framebuffer));
    const uint32_t *codes[2]={teapot_vertex,teapot_fragment};const size_t code_sizes[2]={sizeof(teapot_vertex),sizeof(teapot_fragment)};
    for(unsigned i=0;i<2;i++){
        const VkShaderModuleCreateInfo sm={.sType=VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,.codeSize=code_sizes[i],.pCode=codes[i]};
        TRY(CreateShaderModule(device,&sm,0,&modules[i]));
    }
    const VkPipelineShaderStageCreateInfo stages[2]={
        {.sType=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,.stage=VK_SHADER_STAGE_VERTEX_BIT,.module=modules[0],.pName="main"},
        {.sType=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,.stage=VK_SHADER_STAGE_FRAGMENT_BIT,.module=modules[1],.pName="main"}};
    const VkVertexInputBindingDescription binding={0,24,VK_VERTEX_INPUT_RATE_VERTEX};
    const VkVertexInputAttributeDescription attributes[2]={{0,0,VK_FORMAT_R32G32B32_SFLOAT,0},{1,0,VK_FORMAT_R32G32B32_SFLOAT,12}};
    const VkPipelineVertexInputStateCreateInfo input={.sType=VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO,
        .vertexBindingDescriptionCount=1,.pVertexBindingDescriptions=&binding,.vertexAttributeDescriptionCount=2,.pVertexAttributeDescriptions=attributes};
    const VkPipelineInputAssemblyStateCreateInfo assembly={.sType=VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,.topology=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST};
    const VkViewport viewport={0,0,width,height,0,1};const VkRect2D scissor={{0,0},{width,height}};
    const VkPipelineViewportStateCreateInfo vp={.sType=VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,.viewportCount=1,.pViewports=&viewport,.scissorCount=1,.pScissors=&scissor};
    const VkPipelineRasterizationStateCreateInfo raster={.sType=VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,.polygonMode=VK_POLYGON_MODE_FILL,.cullMode=VK_CULL_MODE_NONE,.lineWidth=1};
    const VkPipelineMultisampleStateCreateInfo samples={.sType=VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,.rasterizationSamples=VK_SAMPLE_COUNT_1_BIT};
    const VkPipelineDepthStencilStateCreateInfo ds={.sType=VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO,.depthTestEnable=VK_TRUE,.depthWriteEnable=VK_TRUE,.depthCompareOp=VK_COMPARE_OP_LESS};
    const VkPipelineColorBlendAttachmentState blend_attachment={.colorWriteMask=15};
    const VkPipelineColorBlendStateCreateInfo blend={.sType=VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,.attachmentCount=1,.pAttachments=&blend_attachment};
    const VkPushConstantRange push={VK_SHADER_STAGE_VERTEX_BIT,0,128};
    const VkPipelineLayoutCreateInfo pl={.sType=VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,.pushConstantRangeCount=1,.pPushConstantRanges=&push};
    TRY(CreatePipelineLayout(device,&pl,0,&layout));
    const VkGraphicsPipelineCreateInfo pipeline_info={.sType=VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,.stageCount=2,.pStages=stages,
        .pVertexInputState=&input,.pInputAssemblyState=&assembly,.pViewportState=&vp,.pRasterizationState=&raster,.pMultisampleState=&samples,
        .pDepthStencilState=&ds,.pColorBlendState=&blend,.layout=layout,.renderPass=pass};
#ifdef CUBIT_TEAPOT_GALLERY
    log("MESA-GALLERY startup pipeline beginning\n");
    pipeline_start=teapot_clock_ns();
#endif
    TRY(CreateGraphicsPipelines(device,0,1,&pipeline_info,0,&pipeline));
#ifdef CUBIT_TEAPOT_GALLERY
    teapot_startup_stage(log,"pipeline",pipeline_start,teapot_clock_ns());
#endif
    const VkCommandPoolCreateInfo pool_info={.sType=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        .flags=VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,.queueFamilyIndex=0};
    TRY(CreateCommandPool(device,&pool_info,0,&pool));
    const VkCommandBufferAllocateInfo ca={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,.commandPool=pool,.level=VK_COMMAND_BUFFER_LEVEL_PRIMARY,.commandBufferCount=1};
    TRY(AllocateCommandBuffers(device,&ca,&command));
    const VkFenceCreateInfo fc={.sType=VK_STRUCTURE_TYPE_FENCE_CREATE_INFO};TRY(CreateFence(device,&fc,0,&fence));
    GetDeviceQueue(device,0,0,&queue);if(!queue){result=VK_ERROR_INITIALIZATION_FAILED;goto cleanup;}
    const VkSubmitInfo submit={.sType=VK_STRUCTURE_TYPE_SUBMIT_INFO,.commandBufferCount=1,.pCommandBuffers=&command};
    unsigned frame=0;
#ifdef CUBIT_TEAPOT_GALLERY
    animation_start=teapot_clock_ns();
    timing_valid=animation_start!=0;
    previous_finish=animation_start;
record_frame:
#endif
    const VkCommandBufferBeginInfo begin={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO};
    TRY(BeginCommandBuffer(command,&begin));
    const VkClearValue clear[2]={ {.color={{0.02f,0.03f,0.06f,1}}}, {.depthStencil={1,0}} };
    const VkRenderPassBeginInfo rb={.sType=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,.renderPass=pass,.framebuffer=framebuffer,.renderArea={{0,0},{width,height}},.clearValueCount=2,.pClearValues=clear};
    CmdBeginRenderPass(command,&rb,VK_SUBPASS_CONTENTS_INLINE);CmdBindPipeline(command,VK_PIPELINE_BIND_POINT_GRAPHICS,pipeline);
    const VkDeviceSize offset=0;CmdBindVertexBuffers(command,0,1,&buffers[0],&offset);
    /* Column-major orthographic transform, Z-up mesh; camera above -Y.
     * Vulkan Y points down and depth is [0,1]. Model is identity. */
#ifdef CUBIT_TEAPOT_GALLERY
#ifdef CUBIT_TEAPOT_DETERMINISTIC
    const float seconds=(float)frame/15.0f;
#else
    const uint64_t now=teapot_clock_ns();
    const float seconds=animation_start && now>=animation_start ?
        (float)((double)(now-animation_start)/1000000000.0) : (float)frame/60.0f;
#endif
    const float transform[32]={seconds};
#else
    const float transform[32]={.26f,0,0,0, 0,-.13f,.0866f,0, 0,-.225f,-.05f,0, -.04f,.34f,.55f,1,
        1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1};
#endif
    CmdPushConstants(command,layout,VK_SHADER_STAGE_VERTEX_BIT,0,sizeof(transform),transform);
    CmdDraw(command,sizeof(teapot_vertices)/sizeof(teapot_vertices[0]),
#ifdef CUBIT_TEAPOT_GALLERY
        20,
#else
        1,
#endif
        0,0);CmdEndRenderPass(command);
    const VkImageMemoryBarrier ib={.sType=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,.srcAccessMask=VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
        .dstAccessMask=VK_ACCESS_TRANSFER_READ_BIT,.oldLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,.newLayout=VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
        .srcQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.dstQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.image=images[0],.subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
    CmdPipelineBarrier(command,VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,VK_PIPELINE_STAGE_TRANSFER_BIT,0,0,0,0,0,1,&ib);
    const VkBufferImageCopy copy={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},.imageExtent={width,height,1}};
    CmdCopyImageToBuffer(command,images[0],VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,buffers[1],1,&copy);
    const VkBufferMemoryBarrier bb={.sType=VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,.srcAccessMask=VK_ACCESS_TRANSFER_WRITE_BIT,.dstAccessMask=VK_ACCESS_HOST_READ_BIT,
        .srcQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.dstQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.buffer=buffers[1],.size=sizes[1]};
    CmdPipelineBarrier(command,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_HOST_BIT,0,0,0,1,&bb,0,0);TRY(EndCommandBuffer(command));
#ifndef CUBIT_TEAPOT_GALLERY
#if CUBIT_TEAPOT_FRAME_COUNT > 1
submit_frame:
#endif
    log("MESA-TEAPOT frame=%u/%u submit vertices=%u depth=D32\n",frame+1,
        CUBIT_TEAPOT_FRAME_COUNT,(unsigned)(sizeof(teapot_vertices)/sizeof(teapot_vertices[0])));
#else
    if(!frame)log("MESA-GALLERY startup first submission beginning\n");
    submit_start=teapot_clock_ns();
#endif
    result=QueueSubmit(queue,1,&submit,fence);
    if(result!=VK_SUCCESS){
        VkResult idle;do{idle=DeviceWaitIdle(device);if(idle!=VK_SUCCESS&&idle!=VK_ERROR_DEVICE_LOST)usleep(100000);}while(idle!=VK_SUCCESS&&idle!=VK_ERROR_DEVICE_LOST);
        goto cleanup;
    }
    do{result=WaitForFences(device,1,&fence,VK_TRUE,1000000000ull);if(result!=VK_SUCCESS&&result!=VK_ERROR_DEVICE_LOST)usleep(100000);}while(result!=VK_SUCCESS&&result!=VK_ERROR_DEVICE_LOST);
    if(result==VK_SUCCESS){
#ifdef CUBIT_TEAPOT_GALLERY
        consumer_start=teapot_clock_ns();
        if(!frame)teapot_startup_stage(log,"first-submit-wait",submit_start,consumer_start);
#endif
        const volatile uint8_t *pixels=maps[1];
        unsigned foreground=0,background=0,bad=0;
        for(unsigned i=0;i<width*height;i++){
            const volatile uint8_t *p=pixels+4*i;
            if(p[3]!=255)bad++;
#ifdef CUBIT_TEAPOT_GALLERY
            if(p[0]>=14&&p[0]<=16&&p[1]>=7&&p[1]<=9&&p[2]>=4&&p[2]<=6)background++;
            else foreground++;
#else
            if(p[2]>p[1]*2 && p[1]>p[0]*2)foreground++;
            else if(p[0]>=14&&p[0]<=16&&p[1]>=7&&p[1]<=9&&p[2]>=4&&p[2]<=6)background++;
            else bad++;
#endif
        }
#ifndef CUBIT_TEAPOT_GALLERY
        log("MESA-TEAPOT readback foreground=%u background=%u bad=%u\n",foreground,background,bad);
#endif
        if(foreground<6000||background<20000||bad){
            log("MESA-TEAPOT readback rejected foreground=%u background=%u bad=%u\n",foreground,background,bad);
            result=VK_ERROR_UNKNOWN;goto cleanup;
        }
        UnmapMemory(device,buffer_memory[1]);maps[1]=0;
#ifdef CUBIT_TEAPOT_GALLERY
        if(!frame)log("MESA-GALLERY startup first presentation beginning\n");
#endif
        if(consume_image){
            const struct mesa_completed_image source={instance,physical,device,get,
                queue,images[0],views[0],width,height,buffer_memory[1],sizes[1]};
            result=consume_image(&source,consume);
        }else if(consume)result=consume(device,buffer_memory[1],sizes[1],width,height,width*4);
    }
    if(result!=VK_SUCCESS){
#ifdef CUBIT_TEAPOT_GALLERY
        if(result==VK_EVENT_SET)result=VK_SUCCESS; /* User closed the window. */
#endif
        goto cleanup;
    }
#ifdef CUBIT_TEAPOT_GALLERY
    const uint64_t finished=teapot_clock_ns();
    /* A missing/backwards read invalidates the whole sample series. Never
     * combine uptime with a zero failure sentinel and label it measured work. */
    if(!frame)teapot_startup_stage(log,"first-validation-present",consumer_start,finished);
    if(!submit_start || !consumer_start || !finished || submit_start<previous_finish ||
       consumer_start<submit_start || finished<consumer_start)timing_valid=0;
    if(timing_valid){
        const uint64_t work=consumer_start-submit_start, present=finished-consumer_start;
        const uint64_t duration=finished-previous_finish;
        if(duration>peak_frame_ns){peak_frame_ns=duration;peak_frame=frame+1;}
        if(work>peak_work_ns)peak_work_ns=work;
        if(present>peak_present_ns)peak_present_ns=present;
        if(work>UINT64_MAX-submit_ns || present>UINT64_MAX-consumer_ns)timing_valid=0;
        else {submit_ns+=work;consumer_ns+=present;}
    }
    previous_finish=finished;
    if((frame+1)%60==0 || frame+1==CUBIT_TEAPOT_FRAME_COUNT){
        const uint64_t elapsed=timing_valid && finished>animation_start ? finished-animation_start : 0;
        log("MESA-GALLERY frames=%u teapots=20 elapsed-ns=%llu fps-milli=%llu CPU-clock=%u\n",
            frame+1,(unsigned long long)elapsed,
            (unsigned long long)(elapsed ? (frame+1)*1000000000000ull/elapsed : 0),elapsed!=0);
        log("MESA-GALLERY mean-submit-wait-readback-ns=%llu mean-validation-present-ns=%llu (NOT GPU timestamps)\n",
            (unsigned long long)(timing_valid?submit_ns/(frame+1):0),
            (unsigned long long)(timing_valid?consumer_ns/(frame+1):0));
        log("MESA-GALLERY interval-peak-frame=%u period-ns=%llu submit-wait-ns=%llu validation-present-ns=%llu CPU-clock=%u\n",
            timing_valid?peak_frame:0,
            (unsigned long long)(timing_valid?peak_frame_ns:0),
            (unsigned long long)(timing_valid?peak_work_ns:0),
            (unsigned long long)(timing_valid?peak_present_ns:0),timing_valid);
        peak_frame_ns=peak_work_ns=peak_present_ns=0;peak_frame=0;
    }
#endif
#if CUBIT_TEAPOT_FRAME_COUNT > 1
    /* The synchronous consumer has returned every GPU/CPU loan. Only now
     * reset the completed fence and reuse this executable command buffer. */
    if(frame+1<CUBIT_TEAPOT_FRAME_COUNT){
        TRY(MapMemory(device,buffer_memory[1],0,sizes[1],0,&maps[1]));
        TRY(ResetFences(device,1,&fence));
        ++frame;
#ifdef CUBIT_TEAPOT_GALLERY
        TRY(ResetCommandBuffer(command,0));
        goto record_frame;
#else
        goto submit_frame;
#endif
    }
#endif
cleanup:
    if(fence)DestroyFence(device,fence,0);
    if(pool)DestroyCommandPool(device,pool,0);
    if(pipeline)DestroyPipeline(device,pipeline,0);
    if(layout)DestroyPipelineLayout(device,layout,0);
    for(unsigned i=0;i<2;i++)if(modules[i])DestroyShaderModule(device,modules[i],0);
    if(framebuffer)DestroyFramebuffer(device,framebuffer,0);
    if(pass)DestroyRenderPass(device,pass,0);
    for(unsigned i=0;i<2;i++){
        if(views[i])DestroyImageView(device,views[i],0);
        if(images[i])DestroyImage(device,images[i],0);
        if(image_memory[i])FreeMemory(device,image_memory[i],0);
        if(maps[i])UnmapMemory(device,buffer_memory[i]);
        if(buffers[i])DestroyBuffer(device,buffers[i],0);
        if(buffer_memory[i])FreeMemory(device,buffer_memory[i],0);
    }
#undef TRY
    return result;
}

static inline VkResult mesa_teapot_probe(VkInstance instance,VkPhysicalDevice physical,
    VkDevice device,PFN_vkGetInstanceProcAddr get,void (*log)(const char *,...),
    mesa_completed_pixels consume)
{
    return mesa_teapot_probe_with_source(instance,physical,device,get,log,consume,NULL);
}
