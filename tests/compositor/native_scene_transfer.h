#ifndef CUBIT_NATIVE_SCENE_TRANSFER_H
#define CUBIT_NATIVE_SCENE_TRANSFER_H
#include <vulkan/vulkan.h>
/* Shared hosted/native audited command adapter. No allocation, submission,
 * wait, lifecycle decision or ownership transfer. Call after Ada Record=0
 * and before Submit. All handles belong to that recording command's device;
 * output is the selected, nonaliased BGRA8 target in COLOR_ATTACHMENT_OPTIMAL;
 * readback is a live TRANSFER_DST buffer with >= width*height*4 bytes at offset0.
 * Caller retains it through the SAME submission fence and CPU consumer return.
 * Host coherent mapping or explicit invalidation is still caller-owned.
 * Return 0 means commands recorded, never GPU completion. Rejection records
 * nothing; caller must cancel the unsubmitted Ada frame before cleanup.
 */
static inline uint32_t cubit_native_scene_readback_commands(
    VkCommandBuffer command,VkImage output,VkBuffer readback,uint32_t width,uint32_t height,
    PFN_vkCmdPipelineBarrier pipeline_barrier,PFN_vkCmdCopyImageToBuffer copy_to_buffer)
{
    if(!command||!output||!readback||!pipeline_barrier||!copy_to_buffer||
       !width||!height||width>65535||height>65535)return 2;
    const VkImageMemoryBarrier image={.sType=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,
        .srcAccessMask=VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,.dstAccessMask=VK_ACCESS_TRANSFER_READ_BIT,
        .oldLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,.newLayout=VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
        .srcQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.dstQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,
        .image=output,.subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
    pipeline_barrier(command,VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,VK_PIPELINE_STAGE_TRANSFER_BIT,
        0,0,NULL,0,NULL,1,&image);
    const VkBufferImageCopy copy={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},
        .imageExtent={width,height,1}};
    copy_to_buffer(command,output,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,readback,1,&copy);
    const VkMemoryBarrier host={.sType=VK_STRUCTURE_TYPE_MEMORY_BARRIER,
        .srcAccessMask=VK_ACCESS_TRANSFER_WRITE_BIT,.dstAccessMask=VK_ACCESS_HOST_READ_BIT};
    pipeline_barrier(command,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_HOST_BIT,
        0,1,&host,0,NULL,0,NULL);
    return 0;
}
#endif
