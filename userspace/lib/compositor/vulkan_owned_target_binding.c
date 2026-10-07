#include "vulkan_owned_targets.h"
#include "vulkan_upload_buffer.h"
/* Populate only after SPARK owns all three backing allocations. No Vulkan call,
 * allocation, publication, or queue submission occurs here. Description remains
 * private and immutable after this binding until its target owner closes. */
uint32_t cubit_vulkan_owned_targets_bind(void *description,void *a,void *b,void *c,void *submission)
{
    struct cubit_vulkan_target_request *r=description;
    const struct cubit_vulkan_owned_image *s[3]={a,b,c};
    const struct cubit_vulkan_submission *owner=submission;
    if(!r||!r->fresh||!r->device||!r->proc||!r->pass)return 2;
    if(!owner||owner->device!=r->device||!owner->queue||!owner->command||!owner->fence)return 2;
    for(unsigned n=0;n<3;n++){
        if(!s[n]||s[n]->stage!=2||!s[n]->image||!s[n]->memory||
           s[n]->device!=r->device||s[n]->proc!=r->proc||
           s[n]->width!=r->width||s[n]->height!=r->height||
           s[n]->format!=VK_FORMAT_B8G8R8A8_UNORM||
           s[n]->usage!=(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT|VK_IMAGE_USAGE_TRANSFER_SRC_BIT))return 2;
        for(unsigned m=0;m<n;m++)
            if(s[n]==s[m]||s[n]->image==s[m]->image||s[n]->memory==s[m]->memory)return 2;
    }
    for(unsigned n=0;n<3;n++)r->images[n]=s[n]->image;
    return 0;
}

/* No CPU pixel writes or submission. Caller owns a recording command and the
 * pool writer. All rejection happens before recording. Discard is authorized
 * only by SPARK's cold target + full repaint gate. Cancelled commands do not
 * change actual layout; completed commands remain COLOR_ATTACHMENT_OPTIMAL. */
uint32_t cubit_vulkan_owned_targets_prepare_frame(void *description,void *submission,
    uint32_t slot,uint32_t width,uint32_t height,uint32_t discard)
{
    const struct cubit_vulkan_target_request *r=description;
    const struct cubit_vulkan_submission *s=submission;
    if(!r||!s||!r->fresh||!r->device||!r->proc||r->device!=s->device||
       !s->command||slot<1||slot>3||discard>1||!width||!height||
       r->width!=width||r->height!=height||!r->images[slot-1]||
       !r->fresh->views[slot-1]||!r->fresh->framebuffers[slot-1])return 2;
    PFN_vkCmdPipelineBarrier barrier=(PFN_vkCmdPipelineBarrier)r->proc(r->device,"vkCmdPipelineBarrier");
    if(!barrier)return 2;
    const VkImageMemoryBarrier transition={.sType=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,
        .srcAccessMask=discard?0:VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
        .dstAccessMask=VK_ACCESS_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
        .oldLayout=discard?VK_IMAGE_LAYOUT_UNDEFINED:VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
        .newLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
        .srcQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.dstQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,
        .image=r->images[slot-1],.subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
    barrier(s->command,discard?VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT:VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,0,0,NULL,0,NULL,1,&transition);
    return 0;
}

uint32_t cubit_vulkan_owned_targets_record_readback(void *description,void *submission,
    void *staging,uint32_t slot)
{
    const struct cubit_vulkan_target_request *r=description;
    const struct cubit_vulkan_submission *s=submission;
    const struct cubit_vulkan_upload_buffer *b=staging;
    if(!r||!s||!b||!r->fresh||!r->device||!r->proc||r->device!=s->device||
       b->device!=r->device||b->proc!=r->proc||!s->command||slot<1||slot>3||
       !r->width||r->width>65535||!r->height||r->height>65535||
       !r->images[slot-1]||!r->fresh->views[slot-1]||!r->fresh->framebuffers[slot-1]||
       b->stage!=2||!b->buffer||!b->memory||!b->mapped||
       b->usage!=VK_BUFFER_USAGE_TRANSFER_DST_BIT||
       !b->capacity||b->capacity>CUBIT_VULKAN_UPLOAD_MAX_BYTES||
       (uint64_t)r->width*r->height*4>b->capacity)return 2;
    PFN_vkCmdPipelineBarrier barrier=(PFN_vkCmdPipelineBarrier)r->proc(r->device,"vkCmdPipelineBarrier");
    PFN_vkCmdCopyImageToBuffer copy=(PFN_vkCmdCopyImageToBuffer)r->proc(r->device,"vkCmdCopyImageToBuffer");
    if(!barrier||!copy)return 2;
    VkImageMemoryBarrier layout={.sType=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,
        .srcAccessMask=VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,.dstAccessMask=VK_ACCESS_TRANSFER_READ_BIT,
        .oldLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,.newLayout=VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
        .srcQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.dstQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,
        .image=r->images[slot-1],.subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
    barrier(s->command,VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,VK_PIPELINE_STAGE_TRANSFER_BIT,
        0,0,NULL,0,NULL,1,&layout);
    const VkBufferImageCopy region={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},
        .imageExtent={r->width,r->height,1}};
    copy(s->command,r->images[slot-1],VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,b->buffer,1,&region);
    layout.srcAccessMask=VK_ACCESS_TRANSFER_READ_BIT;
    layout.dstAccessMask=VK_ACCESS_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
    layout.oldLayout=VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;layout.newLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
    barrier(s->command,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        0,0,NULL,0,NULL,1,&layout);
    const VkMemoryBarrier host={.sType=VK_STRUCTURE_TYPE_MEMORY_BARRIER,
        .srcAccessMask=VK_ACCESS_TRANSFER_WRITE_BIT,.dstAccessMask=VK_ACCESS_HOST_READ_BIT};
    barrier(s->command,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_HOST_BIT,0,1,&host,0,NULL,0,NULL);
    return 0;
}
