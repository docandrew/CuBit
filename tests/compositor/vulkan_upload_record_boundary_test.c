#include "vulkan_upload_record.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
#define H(t,n) ((t)(uintptr_t)(n))
static unsigned fault,step;
static void VKAPI_CALL barrier(VkCommandBuffer command,VkPipelineStageFlags src,VkPipelineStageFlags dst,
    VkDependencyFlags dep,uint32_t nm,const VkMemoryBarrier *m,uint32_t nb,const VkBufferMemoryBarrier *b,
    uint32_t ni,const VkImageMemoryBarrier *i)
{
    assert(command==H(VkCommandBuffer,60)&&!dep&&!nb&&!b&&ni==1&&i->image==H(VkImage,50));
    assert(i->srcQueueFamilyIndex==VK_QUEUE_FAMILY_IGNORED&&i->dstQueueFamilyIndex==VK_QUEUE_FAMILY_IGNORED);
    assert(i->subresourceRange.aspectMask==VK_IMAGE_ASPECT_COLOR_BIT&&i->subresourceRange.levelCount==1&&i->subresourceRange.layerCount==1);
    if(step==0){
        assert(nm==1&&m->srcAccessMask==VK_ACCESS_HOST_WRITE_BIT&&m->dstAccessMask==VK_ACCESS_TRANSFER_READ_BIT);
        assert((src&VK_PIPELINE_STAGE_HOST_BIT)&&dst==VK_PIPELINE_STAGE_TRANSFER_BIT);
        assert(i->oldLayout==(fault==1?VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:VK_IMAGE_LAYOUT_UNDEFINED));
        assert(i->newLayout==VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL&&i->dstAccessMask==VK_ACCESS_TRANSFER_WRITE_BIT);step=1;
    }else{
        assert(step==2&&!nm&&!m&&src==VK_PIPELINE_STAGE_TRANSFER_BIT&&dst==VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT);
        assert(i->oldLayout==VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL&&i->newLayout==VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
        assert(i->srcAccessMask==VK_ACCESS_TRANSFER_WRITE_BIT&&i->dstAccessMask==VK_ACCESS_SHADER_READ_BIT);step=3;
    }
}
static void VKAPI_CALL copy(VkCommandBuffer command,VkBuffer buffer,VkImage image,VkImageLayout layout,uint32_t count,const VkBufferImageCopy *r)
{
    assert(step==1&&command==H(VkCommandBuffer,60)&&buffer==H(VkBuffer,40)&&image==H(VkImage,50));
    assert(layout==VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL&&count==1&&r->bufferOffset==4&&r->bufferRowLength==8&&!r->bufferImageHeight);
    assert(r->imageOffset.x==1&&r->imageOffset.y==1&&!r->imageOffset.z&&r->imageExtent.width==4&&r->imageExtent.height==2&&r->imageExtent.depth==1);
    assert(r->imageSubresource.aspectMask==VK_IMAGE_ASPECT_COLOR_BIT&&!r->imageSubresource.mipLevel&&r->imageSubresource.layerCount==1);step=2;
}
static PFN_vkVoidFunction VKAPI_CALL proc(VkDevice d,const char *n)
{assert(d==H(VkDevice,3));if(!strcmp(n,"vkCmdPipelineBarrier"))return fault==35?NULL:(PFN_vkVoidFunction)barrier;if(!strcmp(n,"vkCmdCopyBufferToImage"))return fault==36?NULL:(PFN_vkVoidFunction)copy;return NULL;}
static PFN_vkVoidFunction VKAPI_CALL other(VkDevice d,const char *n){(void)d;(void)n;return NULL;}
int main(void)
{
    for(fault=0;fault<43;fault++){
        step=0;
        struct cubit_vulkan_submission c={.device=H(VkDevice,3),.command=H(VkCommandBuffer,60)};
        struct cubit_vulkan_upload_buffer u={.device=c.device,.proc=proc,.buffer=H(VkBuffer,40),.memory=H(VkDeviceMemory,41),.mapped=(void *)1,.capacity=128,.stage=2};
        struct cubit_vulkan_owned_image s={.device=c.device,.proc=proc,.image=H(VkImage,50),.memory=H(VkDeviceMemory,51),.width=8,.height=4,.format=VK_FORMAT_B8G8R8A8_UNORM,.usage=VK_IMAGE_USAGE_SAMPLED_BIT|VK_IMAGE_USAGE_TRANSFER_DST_BIT,.stage=2};
        struct cubit_vulkan_upload_region r={8,4,1,1,4,2,4,8,0,1};
        void *cp=&c,*up=&u,*sp=&s;const struct cubit_vulkan_upload_region *rp=&r;
        switch(fault){
        case 1:r.discard=0;break;case 2:r.mask=1;s.format=VK_FORMAT_R8_UNORM;break;
        case 3:cp=NULL;break;case 4:up=NULL;break;case 5:sp=NULL;break;case 6:rp=NULL;break;
        case 7:c.device=H(VkDevice,99);break;case 8:c.command=VK_NULL_HANDLE;break;case 9:u.stage=1;break;case 10:s.stage=3;break;
        case 11:u.mapped=NULL;break;case 12:u.buffer=VK_NULL_HANDLE;break;case 13:u.memory=s.memory;break;
        case 14:u.capacity=0;break;case 15:u.capacity=CUBIT_VULKAN_UPLOAD_MAX_BYTES+1;break;
        case 16:s.format=VK_FORMAT_R32_SFLOAT;break;case 17:s.usage=VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;break;
        case 18:s.width=0;break;case 19:r.image_width=9;break;case 20:r.x=UINT32_MAX;break;case 21:r.y=UINT32_MAX;break;
        case 22:r.width=0;break;case 23:r.height=0;break;case 24:r.row_pixels=3;break;case 25:r.row_pixels=65536;break;
        case 26:r.offset=1;break;case 27:r.offset=UINT32_MAX-3;break;case 28:r.offset=128;break;
        case 29:r.mask=2;break;case 30:r.discard=2;break;case 31:u.proc=NULL;break;case 32:s.proc=other;break;
        case 33:s.device=H(VkDevice,99);break;case 34:u.device=H(VkDevice,99);break;
        case 37:u.memory=VK_NULL_HANDLE;break;case 38:s.image=VK_NULL_HANDLE;break;case 39:s.memory=VK_NULL_HANDLE;break;
        case 40:s.width=r.image_width=65535;r.width=UINT32_MAX;break;case 41:s.height=r.image_height=65536;break;
        case 42:r.image_height=3;break;default:break;
        }
        const uint32_t result=cubit_vulkan_upload_record(cp,up,sp,rp);
        assert(result==(fault<3?0u:1u));assert(step==(fault<3?3u:0u));
    }
    puts("PASS upload record C boundary: cold/retained BGRA and R8 barrier/copy sequences, 40 pre-command rejection paths");
}
