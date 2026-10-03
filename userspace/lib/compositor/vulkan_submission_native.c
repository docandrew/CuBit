#include "vulkan_submission.h"
#include "vulkan_affine.h"
uint32_t cubit_vulkan_submission_matches(void *borrowed,void *draw)
{
    const struct cubit_vulkan_submission *s=borrowed;
    const struct cubit_vulkan_affine_draw *d=draw;
    return s&&d&&d->engine&&s->command&&s->device&&
        s->command==d->command&&s->device==d->engine->device?0:2;
}
uint32_t cubit_vulkan_submission_init(struct cubit_vulkan_submission *s,
    VkDevice d,VkQueue q,VkCommandBuffer c,VkFence f,PFN_vkGetDeviceProcAddr proc)
{
    if(!s)return 2;
    *s=(struct cubit_vulkan_submission){.device=d,.queue=q,.command=c,.fence=f};
    if(!d||!q||!c||!f||!proc)return 2;
#define LOAD(field,name) s->field=(PFN_vk##name)proc(d,"vk" #name);if(!s->field)return 2
    LOAD(reset_fences,ResetFences);LOAD(reset_command,ResetCommandBuffer);LOAD(begin,BeginCommandBuffer);
    LOAD(end,EndCommandBuffer);LOAD(submit,QueueSubmit);LOAD(status,GetFenceStatus);
    LOAD(begin_scene,CmdBeginRenderPass);LOAD(end_scene,CmdEndRenderPass);LOAD(fill,CmdClearAttachments);
#undef LOAD
    return 0;
}
uint32_t cubit_vulkan_submission_start(void *borrowed)
{
    const struct cubit_vulkan_submission *s=borrowed;
    if(!s||!s->device||!s->fence||!s->command||!s->reset_fences||!s->reset_command||!s->begin)return 2;
    if(s->reset_fences(s->device,1,&s->fence)!=VK_SUCCESS)return 2;
    if(s->reset_command(s->command,0)!=VK_SUCCESS)return 2;
    const VkCommandBufferBeginInfo info={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,.flags=VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT};
    return s->begin(s->command,&info)==VK_SUCCESS?0:2;
}
uint32_t cubit_vulkan_submission_seal(void *borrowed)
{
    const struct cubit_vulkan_submission *s=borrowed;
    return s&&s->command&&s->end&&s->end(s->command)==VK_SUCCESS?0:2;
}
uint32_t cubit_vulkan_submission_submit(void *borrowed)
{
    const struct cubit_vulkan_submission *s=borrowed;
    if(!s||!s->command||!s->queue||!s->fence||!s->submit)return 2;
    const VkSubmitInfo info={.sType=VK_STRUCTURE_TYPE_SUBMIT_INFO,.commandBufferCount=1,.pCommandBuffers=&s->command};
    return s->submit(s->queue,1,&info,s->fence)==VK_SUCCESS?0:2;
}
uint32_t cubit_vulkan_submission_poll(void *borrowed)
{
    const struct cubit_vulkan_submission *s=borrowed;
    if(!s||!s->device||!s->fence||!s->status)return 2;
    VkResult result=s->status(s->device,s->fence);
    return result==VK_SUCCESS?0:result==VK_NOT_READY?1:2;
}
uint32_t cubit_vulkan_submission_cancel(void *borrowed)
{
    const struct cubit_vulkan_submission *s=borrowed;
    return s&&s->command&&s->reset_command&&s->reset_command(s->command,0)==VK_SUCCESS?0:2;
}

uint32_t cubit_vulkan_submission_begin_scene(void *borrowed,void *pass,uint32_t width,uint32_t height)
{
    const struct cubit_vulkan_submission *s=borrowed;
    const struct cubit_vulkan_scene *p=pass;
    if(!s||!p||!s->device||s->device!=p->device||!s->command||!s->begin_scene||
       p->begin.sType!=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO||p->begin.pNext||
       !p->begin.renderPass||!p->begin.framebuffer||!width||width>65535||!height||height>65535||
       p->begin.renderArea.offset.x||p->begin.renderArea.offset.y||
       p->begin.renderArea.extent.width!=width||p->begin.renderArea.extent.height!=height||
       p->begin.clearValueCount>1||(p->begin.clearValueCount&&!p->begin.pClearValues))return 2;
    s->begin_scene(s->command,&p->begin,VK_SUBPASS_CONTENTS_INLINE);
    return 0;
}
uint32_t cubit_vulkan_submission_end_scene(void *borrowed)
{
    const struct cubit_vulkan_submission *s=borrowed;
    if(!s||!s->command||!s->end_scene)return 2;
    s->end_scene(s->command);return 0;
}

uint32_t cubit_vulkan_submission_fill(void *borrowed,uint32_t width,uint32_t height,
    uint32_t left,uint32_t top,uint32_t right,uint32_t bottom,uint32_t rgb)
{
    const struct cubit_vulkan_submission *s=borrowed;
    if(!s||!s->command||!s->fill||!width||width>65535||!height||height>65535||
       left>=right||top>=bottom||right>width||bottom>height)return 2;
    const VkClearAttachment clear={.aspectMask=VK_IMAGE_ASPECT_COLOR_BIT,.colorAttachment=0,
        .clearValue={.color={{((rgb>>16)&255)/255.0f,((rgb>>8)&255)/255.0f,(rgb&255)/255.0f,1}}}};
    const VkClearRect rect={.rect={{(int32_t)left,(int32_t)top},{right-left,bottom-top}},.baseArrayLayer=0,.layerCount=1};
    s->fill(s->command,1,&clear,1,&rect);return 0;
}
