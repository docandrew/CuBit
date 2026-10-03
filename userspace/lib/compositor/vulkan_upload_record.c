#include "vulkan_upload_record.h"
uint32_t cubit_vulkan_upload_record(void *submission,void *upload,void *image,
    const struct cubit_vulkan_upload_region *r)
{
    struct cubit_vulkan_submission *c=submission;
    struct cubit_vulkan_upload_buffer *u=upload;
    struct cubit_vulkan_owned_image *s=image;
    if(!c||!u||!s||!r||!c->device||!c->command||u->stage!=2||s->stage!=2||
       !u->buffer||!u->memory||!u->mapped||!s->image||!s->memory||!u->proc||
       u->device!=c->device||s->device!=c->device||s->proc!=u->proc||u->memory==s->memory||
       !u->capacity||u->capacity>CUBIT_VULKAN_UPLOAD_MAX_BYTES||r->mask>1||r->discard>1||
       s->format!=(r->mask?VK_FORMAT_R8_UNORM:VK_FORMAT_B8G8R8A8_UNORM)||
       s->usage!=(VK_IMAGE_USAGE_SAMPLED_BIT|VK_IMAGE_USAGE_TRANSFER_DST_BIT)||
       !s->width||s->width>65535||!s->height||s->height>65535||
       r->image_width!=s->width||r->image_height!=s->height||!r->width||!r->height||
       (uint64_t)r->x+r->width>s->width||(uint64_t)r->y+r->height>s->height||
       r->row_pixels>65535||(r->row_pixels&&r->row_pixels<r->width)||r->offset%4)return 1;
    uint64_t row=r->row_pixels?r->row_pixels:r->width;
    uint64_t end=(uint64_t)r->offset+((r->height-1)*row+r->width)*(r->mask?1u:4u);
    if(end>u->capacity)return 1;
    PFN_vkCmdPipelineBarrier barrier=(PFN_vkCmdPipelineBarrier)u->proc(c->device,"vkCmdPipelineBarrier");
    PFN_vkCmdCopyBufferToImage copy=(PFN_vkCmdCopyBufferToImage)u->proc(c->device,"vkCmdCopyBufferToImage");
    if(!barrier||!copy)return 1;
    const VkMemoryBarrier producer={.sType=VK_STRUCTURE_TYPE_MEMORY_BARRIER,
        .srcAccessMask=VK_ACCESS_HOST_WRITE_BIT,.dstAccessMask=VK_ACCESS_TRANSFER_READ_BIT};
    VkImageMemoryBarrier layout={.sType=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,
        .srcAccessMask=r->discard?0:VK_ACCESS_SHADER_READ_BIT,.dstAccessMask=VK_ACCESS_TRANSFER_WRITE_BIT,
        .oldLayout=r->discard?VK_IMAGE_LAYOUT_UNDEFINED:VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
        .newLayout=VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,.srcQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,
        .dstQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.image=s->image,
        .subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
    barrier(c->command,VK_PIPELINE_STAGE_HOST_BIT|(r->discard?VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT:VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
        VK_PIPELINE_STAGE_TRANSFER_BIT,0,1,&producer,0,NULL,1,&layout);
    const VkBufferImageCopy region={.bufferOffset=r->offset,.bufferRowLength=r->row_pixels,
        .imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},.imageOffset={(int32_t)r->x,(int32_t)r->y,0},
        .imageExtent={r->width,r->height,1}};
    copy(c->command,u->buffer,s->image,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,1,&region);
    layout.srcAccessMask=VK_ACCESS_TRANSFER_WRITE_BIT;layout.dstAccessMask=VK_ACCESS_SHADER_READ_BIT;
    layout.oldLayout=VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;layout.newLayout=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
    barrier(c->command,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,0,0,NULL,0,NULL,1,&layout);
    return 0;
}
