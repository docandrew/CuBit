#include "vulkan_owned_targets.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
#define H(t,n) ((t)(uintptr_t)(n))
static PFN_vkVoidFunction VKAPI_CALL proc(VkDevice d,const char *n){(void)d;(void)n;return NULL;}
static unsigned barrier_calls, expected_discard, expected_slot;
static void VKAPI_CALL record_barrier(VkCommandBuffer command,VkPipelineStageFlags source,
    VkPipelineStageFlags destination,VkDependencyFlags flags,uint32_t memory_count,
    const VkMemoryBarrier *memory,uint32_t buffer_count,const VkBufferMemoryBarrier *buffers,
    uint32_t image_count,const VkImageMemoryBarrier *images)
{
    assert(command==H(VkCommandBuffer,1) && !flags && !memory_count && !memory && !buffer_count && !buffers);
    assert(source==(expected_discard?VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT:VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT));
    assert(destination==VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT && image_count==1 && images);
    assert(images->image==H(VkImage,10+expected_slot-1));
    assert(images->oldLayout==(expected_discard?VK_IMAGE_LAYOUT_UNDEFINED:VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL));
    assert(images->newLayout==VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL);
    assert(images->srcQueueFamilyIndex==VK_QUEUE_FAMILY_IGNORED && images->dstQueueFamilyIndex==VK_QUEUE_FAMILY_IGNORED);
    assert(images->subresourceRange.aspectMask==VK_IMAGE_ASPECT_COLOR_BIT && images->subresourceRange.levelCount==1 && images->subresourceRange.layerCount==1);
    ++barrier_calls;
}
static PFN_vkVoidFunction VKAPI_CALL barrier_proc(VkDevice d,const char *name)
{ assert(d==H(VkDevice,1) && !strcmp(name,"vkCmdPipelineBarrier"));return (PFN_vkVoidFunction)record_barrier; }
static void layout_faults(void)
{
    for(unsigned discard=0;discard<2;discard++)for(unsigned slot=1;slot<=3;slot++)for(unsigned fault=0;fault<12;fault++) {
        struct cubit_vulkan_targets views={0};
        struct cubit_vulkan_target_request r={.fresh=&views,.device=H(VkDevice,1),.proc=barrier_proc,.width=32,.height=24};
        struct cubit_vulkan_submission owner={.device=r.device,.command=H(VkCommandBuffer,1)};
        for(unsigned i=0;i<3;i++){r.images[i]=H(VkImage,10+i);views.views[i]=H(VkImageView,20+i);views.framebuffers[i]=H(VkFramebuffer,30+i);}
        void *description=&r,*submission=&owner;uint32_t index=slot,mode=discard,w=32,h=24;
        switch(fault){
        case 1:description=NULL;break;case 2:submission=NULL;break;case 3:owner.device=H(VkDevice,2);break;
        case 4:owner.command=VK_NULL_HANDLE;break;case 5:index=0;break;case 6:index=4;break;
        case 7:mode=2;break;case 8:w=0;break;case 9:h=25;break;case 10:r.images[slot-1]=VK_NULL_HANDLE;break;
        case 11:r.proc=proc;break;
        }
        barrier_calls=0;expected_discard=discard;expected_slot=slot;
        uint32_t result=cubit_vulkan_owned_targets_prepare_frame(description,submission,index,w,h,mode);
        assert(result==(fault?2u:0u) && barrier_calls==(fault?0u:1u));
    }
    puts("Target layout: 6 cold/retained recordings + 66 pre-recording rejection cases PASS");
}
int main(void)
{
    for(unsigned fault=0;fault<19;fault++){
        struct cubit_vulkan_targets views={0};
        struct cubit_vulkan_target_request r={.fresh=&views,.device=H(VkDevice,1),.proc=proc,
            .pass=H(VkRenderPass,2),.width=32,.height=24};
        struct cubit_vulkan_owned_image s[3];
        for(unsigned i=0;i<3;i++)s[i]=(struct cubit_vulkan_owned_image){.stage=2,.device=r.device,.proc=proc,
            .width=32,.height=24,.format=VK_FORMAT_B8G8R8A8_UNORM,
            .usage=VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT|VK_IMAGE_USAGE_TRANSFER_SRC_BIT,
            .image=H(VkImage,10+i),.memory=H(VkDeviceMemory,20+i)};
        void *a=&s[0],*b=&s[1],*c=&s[2];
        struct cubit_vulkan_submission owner={.device=r.device,.queue=H(VkQueue,1),.command=H(VkCommandBuffer,1),.fence=H(VkFence,1)};
        void *context=&owner;
        switch(fault){
        case 1:s[2].stage=1;break;case 2:s[2].memory=s[1].memory;break;
        case 3:s[2].image=s[1].image;break;case 4:c=b;break;
        case 5:s[2].width=33;break;case 6:s[2].height=25;break;
        case 7:s[2].format=VK_FORMAT_R8_UNORM;break;case 8:s[2].usage=VK_IMAGE_USAGE_SAMPLED_BIT;break;
        case 9:s[2].device=H(VkDevice,4);break;case 10:s[2].proc=NULL;break;
        case 11:r.pass=VK_NULL_HANDLE;break;case 12:b=NULL;break;case 13:s[2].memory=VK_NULL_HANDLE;break;
        case 14:owner.device=H(VkDevice,9);break;case 15:owner.queue=VK_NULL_HANDLE;break;
        case 16:owner.command=VK_NULL_HANDLE;break;case 17:owner.fence=VK_NULL_HANDLE;break;case 18:context=NULL;break;
        }
        uint32_t result=cubit_vulkan_owned_targets_bind(&r,a,b,c,context);
        if(!fault){assert(result==0);for(unsigned i=0;i<3;i++)assert(r.images[i]==s[i].image);}
        else {assert(result==2);for(unsigned i=0;i<3;i++)assert(!r.images[i]);}
    }
    layout_faults();
    puts("Owned target binding: accepted independent backing and 18 device/role/extent/alias/lifetime/submission rejections, no partial publication PASS");
}
