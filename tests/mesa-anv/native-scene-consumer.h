/* Actual Mesa-produced texture -> production Ada scene -> completed readback.
 * Trusted same-device integration fixture, not Desktop activation/GPU import.
 * One externally serialized call; every successful/failed return retires the
 * source borrow. Sticky uncertainty deliberately retains the entire stack. */
#ifndef CUBIT_MESA_NATIVE_SCENE_CONSUMER_H
#define CUBIT_MESA_NATIVE_SCENE_CONSUMER_H
#include "completed-image.h"
#include "../compositor/native_scene_bridge.h"
#include "../compositor/native_scene_transfer.h"
#include "../../userspace/lib/compositor/vulkan_targets.h"
#include "../../userspace/lib/compositor/vulkan_owned_image.h"
#include "../../userspace/lib/compositor/vulkan_affine.h"
#include <unistd.h>

static inline VkResult mesa_scene_compose(const struct mesa_completed_image *s,
    mesa_completed_pixels present,void (*log)(const char *,...))
{
    if(!s||!s->instance_proc||!s->image||!s->view||!s->queue||!log||
       s->width<12||s->height<12||s->width>65535||s->height>65535||
       s->readback_bytes!=(VkDeviceSize)s->width*s->height*4)return VK_ERROR_INITIALIZATION_FAILED;
    const VkDevice d=s->device;
    PFN_vkGetDeviceProcAddr proc=(PFN_vkGetDeviceProcAddr)s->instance_proc(s->instance,"vkGetDeviceProcAddr");
    PFN_vkGetPhysicalDeviceMemoryProperties memory_properties=(PFN_vkGetPhysicalDeviceMemoryProperties)
        s->instance_proc(s->instance,"vkGetPhysicalDeviceMemoryProperties");
    if(!proc||!memory_properties)return VK_ERROR_INITIALIZATION_FAILED;
#define LOAD(n) PFN_vk##n n=(PFN_vk##n)proc(d,"vk" #n);if(!n){log("MESA-SCENE missing vk%s\n",#n);return VK_ERROR_INITIALIZATION_FAILED;}
    LOAD(CreateRenderPass);LOAD(DestroyRenderPass);LOAD(CreateDescriptorPool);LOAD(DestroyDescriptorPool);
    LOAD(AllocateDescriptorSets);LOAD(UpdateDescriptorSets);LOAD(CreateCommandPool);LOAD(DestroyCommandPool);
    LOAD(AllocateCommandBuffers);LOAD(CreateFence);LOAD(DestroyFence);LOAD(CreateBuffer);LOAD(DestroyBuffer);
    LOAD(GetBufferMemoryRequirements);LOAD(AllocateMemory);LOAD(FreeMemory);LOAD(BindBufferMemory);
    LOAD(MapMemory);LOAD(UnmapMemory);LOAD(CmdPipelineBarrier);LOAD(CmdCopyImageToBuffer);
#undef LOAD
#ifndef CUBIT_SCENE_HOSTED
    /* Hosted Ada main elaborates this package itself. Native C app must bind
     * once, never re-elaborate the global owner between device lifetimes. */
    static int elaborated;
    if(!elaborated){compositor_sceneinit();elaborated=1;}
#endif
    VkResult result=VK_SUCCESS;
    VkRenderPass pass=VK_NULL_HANDLE;VkDescriptorPool descriptors=VK_NULL_HANDLE;
    VkCommandPool pool=VK_NULL_HANDLE;VkCommandBuffer command=VK_NULL_HANDLE;
    VkFence fence=VK_NULL_HANDLE;VkBuffer buffer=VK_NULL_HANDLE;VkDeviceMemory memory=VK_NULL_HANDLE;
    void *pixels=NULL,*baseline=NULL;
    struct cubit_vulkan_affine_engine engine={0};
    struct cubit_vulkan_owned_image images[3]={0};
    struct cubit_vulkan_targets targets={0};
    struct cubit_vulkan_submission submission={0};
    uint32_t status=0,selected=0;
    int opened=0,recording=0,ready=0;
#define TRY(call) do{result=(call);if(result!=VK_SUCCESS){log("MESA-SCENE failed %s result=%d\n",#call,result);goto cleanup;}}while(0)
    const VkAttachmentDescription attachment={.format=VK_FORMAT_B8G8R8A8_UNORM,.samples=VK_SAMPLE_COUNT_1_BIT,
        .loadOp=VK_ATTACHMENT_LOAD_OP_CLEAR,.storeOp=VK_ATTACHMENT_STORE_OP_STORE,
        .stencilLoadOp=VK_ATTACHMENT_LOAD_OP_DONT_CARE,.stencilStoreOp=VK_ATTACHMENT_STORE_OP_DONT_CARE,
        .initialLayout=VK_IMAGE_LAYOUT_UNDEFINED,.finalLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL};
    const VkAttachmentReference color={0,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL};
    const VkSubpassDescription subpass={.pipelineBindPoint=VK_PIPELINE_BIND_POINT_GRAPHICS,
        .colorAttachmentCount=1,.pColorAttachments=&color};
    const VkSubpassDependency dependency={.srcSubpass=VK_SUBPASS_EXTERNAL,.dstSubpass=0,
        .srcStageMask=VK_PIPELINE_STAGE_TRANSFER_BIT,.dstStageMask=VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        .srcAccessMask=VK_ACCESS_TRANSFER_READ_BIT,.dstAccessMask=VK_ACCESS_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT};
    const VkRenderPassCreateInfo pc={.sType=VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO,
        .attachmentCount=1,.pAttachments=&attachment,.subpassCount=1,.pSubpasses=&subpass,
        .dependencyCount=1,.pDependencies=&dependency};
    TRY(CreateRenderPass(d,&pc,NULL,&pass));
    TRY(cubit_vulkan_affine_create(&engine,d,proc,pass));
    const VkDescriptorPoolSize ps={VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1};
    const VkDescriptorPoolCreateInfo dc={.sType=VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        .maxSets=1,.poolSizeCount=1,.pPoolSizes=&ps};
    TRY(CreateDescriptorPool(d,&dc,NULL,&descriptors));
    const VkDescriptorSetAllocateInfo da={.sType=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        .descriptorPool=descriptors,.descriptorSetCount=1,.pSetLayouts=&engine.descriptors};
    VkDescriptorSet descriptor;
    TRY(AllocateDescriptorSets(d,&da,&descriptor));
    const VkDescriptorImageInfo di={engine.sampler,s->view,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL};
    const VkWriteDescriptorSet write={.sType=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,.dstSet=descriptor,
        .dstBinding=0,.descriptorCount=1,.descriptorType=VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,.pImageInfo=&di};
    UpdateDescriptorSets(d,1,&write,0,NULL);
    const VkCommandPoolCreateInfo cp={.sType=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        .flags=VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,.queueFamilyIndex=0};
    TRY(CreateCommandPool(d,&cp,NULL,&pool));
    const VkCommandBufferAllocateInfo ca={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        .commandPool=pool,.level=VK_COMMAND_BUFFER_LEVEL_PRIMARY,.commandBufferCount=1};
    TRY(AllocateCommandBuffers(d,&ca,&command));
    const VkFenceCreateInfo fc={.sType=VK_STRUCTURE_TYPE_FENCE_CREATE_INFO};
    TRY(CreateFence(d,&fc,NULL,&fence));
    const VkBufferCreateInfo bc={.sType=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,.size=s->readback_bytes,
        .usage=VK_BUFFER_USAGE_TRANSFER_DST_BIT,.sharingMode=VK_SHARING_MODE_EXCLUSIVE};
    TRY(CreateBuffer(d,&bc,NULL,&buffer));
    VkMemoryRequirements requirements;GetBufferMemoryRequirements(d,buffer,&requirements);
    VkPhysicalDeviceMemoryProperties properties;memory_properties(s->physical,&properties);
    uint32_t allowed=0,type=UINT32_MAX;
    for(uint32_t i=0;i<properties.memoryTypeCount&&i<32;i++){
        VkMemoryPropertyFlags flags=properties.memoryTypes[i].propertyFlags;
        if(flags&(VK_MEMORY_PROPERTY_PROTECTED_BIT|VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT))continue;
        allowed|=UINT32_C(1)<<i;
        if((requirements.memoryTypeBits&(UINT32_C(1)<<i))&&
           (flags&(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))==
                  (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))type=i;
    }
    if(type==UINT32_MAX||requirements.size<s->readback_bytes){result=VK_ERROR_FEATURE_NOT_PRESENT;goto cleanup;}
    const VkMemoryAllocateInfo ma={.sType=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        .allocationSize=requirements.size,.memoryTypeIndex=type};
    TRY(AllocateMemory(d,&ma,NULL,&memory));TRY(BindBufferMemory(d,buffer,memory,0));
    for(unsigned i=0;i<3;i++)images[i]=(struct cubit_vulkan_owned_image){.physical=s->physical,
        .device=d,.instance=s->instance,.instance_proc=s->instance_proc,.proc=proc,
        .width=s->width,.height=s->height,.format=VK_FORMAT_B8G8R8A8_UNORM,
        .usage=VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT|VK_IMAGE_USAGE_TRANSFER_SRC_BIT};
    struct cubit_vulkan_target_request request={.fresh=&targets,.device=d,.proc=proc,
        .pass=pass,.width=s->width,.height=s->height};
    struct cubit_vulkan_affine_draw draw={&engine,command,descriptor,s->width,s->height};
    if(cubit_vulkan_submission_init(&submission,d,s->queue,command,fence,proc)){
        result=VK_ERROR_INITIALIZATION_FAILED;goto cleanup;
    }
    cubit_native_scene_open(&request,&images[0],&images[1],&images[2],&submission,&draw,
        allowed,s->width,s->height,&status);
    log("MESA-SCENE open=%u\n",status);
    if(status==3)goto quarantine;
    if(status){result=VK_ERROR_INITIALIZATION_FAILED;goto cleanup;}
    opened=1;
    cubit_native_scene_begin(&selected,&status);
    if(status||selected<1||selected>3)goto quarantine;
    recording=1;
    const VkImageMemoryBarrier source={.sType=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,
        .srcAccessMask=VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT|VK_ACCESS_TRANSFER_READ_BIT,
        .dstAccessMask=VK_ACCESS_SHADER_READ_BIT,.oldLayout=VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
        .newLayout=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,.srcQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,
        .dstQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.image=s->image,
        .subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
    CmdPipelineBarrier(command,VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT|VK_PIPELINE_STAGE_TRANSFER_BIT,
        VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,0,0,NULL,0,NULL,1,&source);
    cubit_native_scene_record(&status);
    if(status==3)goto quarantine;
    if(status==2){
        /* Record may already have cancelled through Ada Scene_Recording.
         * Close=0 proves that cancellation retired the unsubmitted writer.
         * Close=2 leaves ownership unchanged: cleanup must still Cancel it.
         * Never turn a second Cancel rejection into permanent quarantine. */
        cubit_native_scene_close(&status);
        if(status==0){opened=0;recording=0;}
        else if(status!=2)goto quarantine;
        result=VK_ERROR_UNKNOWN;goto cleanup;
    }
    if(status){result=VK_ERROR_UNKNOWN;goto cleanup;}
    if(cubit_native_scene_readback_commands(command,images[selected-1].image,buffer,s->width,s->height,
        CmdPipelineBarrier,CmdCopyImageToBuffer)){result=VK_ERROR_UNKNOWN;goto cleanup;}
    cubit_native_scene_submit(&status);
    if(status==3)goto quarantine;
    if(status){result=VK_ERROR_UNKNOWN;goto cleanup;}
    recording=0;
    log("MESA-SCENE submitted slot=%u (source retained)\n",selected);
    do{cubit_native_scene_poll(&status);if(status==1)usleep(1000);}while(status==1);
    if(status)goto quarantine;
    ready=1;
    TRY(MapMemory(d,memory,0,s->readback_bytes,0,&pixels));
    TRY(MapMemory(d,s->readback,0,s->readback_bytes,0,&baseline));
    uint32_t mismatches=0;
    for(uint32_t y=0;y<s->height;y++)for(uint32_t x=0;x<s->width;x++){
        size_t i=(size_t)y*s->width+x;
        uint32_t expected=x>=4&&x<12&&y>=4&&y<12?UINT32_C(0xff00ff00):((uint32_t *)baseline)[i];
        if(((uint32_t *)pixels)[i]!=expected)++mismatches;
    }
    log("MESA-SCENE composed pixels=%llu mismatches=%u\n",(unsigned long long)s->width*s->height,mismatches);
    UnmapMemory(d,s->readback);baseline=NULL;UnmapMemory(d,memory);pixels=NULL;
    if(mismatches)result=VK_ERROR_UNKNOWN;
    else if(present)result=present(d,memory,s->readback_bytes,s->width,s->height,s->width*4);
cleanup:
    if(baseline)UnmapMemory(d,s->readback);
    if(pixels)UnmapMemory(d,memory);
    if(recording){cubit_native_scene_cancel(&status);if(status)goto quarantine;}
    if(ready){cubit_native_scene_release(&status);if(status)goto quarantine;}
    if(opened){cubit_native_scene_close(&status);if(status)goto quarantine;}
    if(pool)DestroyCommandPool(d,pool,NULL);
    if(fence)DestroyFence(d,fence,NULL);
    if(descriptors)DestroyDescriptorPool(d,descriptors,NULL);
    cubit_vulkan_affine_destroy(&engine);
    if(pass)DestroyRenderPass(d,pass,NULL);
    if(buffer)DestroyBuffer(d,buffer,NULL);
    if(memory)FreeMemory(d,memory,NULL);
    return result;
quarantine:
    log("MESA-SCENE uncertain status=%u; all source/target resources retained\n",status);
    for(;;)usleep(100000);
#undef TRY
}
#endif
