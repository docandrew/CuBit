#include "vulkan_owned_image.h"

static uint32_t discard(struct cubit_vulkan_owned_image *s)
{
    if(s->image)s->destroy(s->device,s->image,NULL);
    if(s->memory)s->free_memory(s->device,s->memory,NULL);
    s->image=VK_NULL_HANDLE;s->memory=VK_NULL_HANDLE;s->stage=3;
    return 1;
}
uint32_t cubit_vulkan_owned_image_prepare(void *context,uint64_t *bytes,uint32_t *types)
{
    if(!bytes||!types)return 1;
    *bytes=0;*types=0;
    struct cubit_vulkan_owned_image *s=context;
    if(!s||s->stage||s->image||s->memory)return 2;
    if(!s->physical||!s->device||!s->instance||!s->instance_proc||!s->proc||
       !s->width||s->width>65535||!s->height||s->height>65535)return 1;
    const VkImageUsageFlags sampled=VK_IMAGE_USAGE_SAMPLED_BIT|VK_IMAGE_USAGE_TRANSFER_DST_BIT;
    const VkImageUsageFlags target=VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT|VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
    if((s->format!=VK_FORMAT_R8_UNORM&&s->format!=VK_FORMAT_B8G8R8A8_UNORM)||
       (s->usage!=sampled&&s->usage!=target)||
       (s->format==VK_FORMAT_R8_UNORM&&s->usage!=sampled))return 1;
    PFN_vkGetPhysicalDeviceImageFormatProperties format_properties=(PFN_vkGetPhysicalDeviceImageFormatProperties)
        s->instance_proc(s->instance,"vkGetPhysicalDeviceImageFormatProperties");
    PFN_vkCreateImage create=(PFN_vkCreateImage)s->proc(s->device,"vkCreateImage");
    PFN_vkGetImageMemoryRequirements2 requirements=(PFN_vkGetImageMemoryRequirements2)
        s->proc(s->device,"vkGetImageMemoryRequirements2");
    s->destroy=(PFN_vkDestroyImage)s->proc(s->device,"vkDestroyImage");
    s->free_memory=(PFN_vkFreeMemory)s->proc(s->device,"vkFreeMemory");
    s->allocate=(PFN_vkAllocateMemory)s->proc(s->device,"vkAllocateMemory");
    s->bind=(PFN_vkBindImageMemory)s->proc(s->device,"vkBindImageMemory");
    if(!format_properties||!create||!requirements||!s->destroy||!s->free_memory||!s->allocate||!s->bind)return 1;
    VkImageFormatProperties limits={0};
    if(format_properties(s->physical,s->format,VK_IMAGE_TYPE_2D,VK_IMAGE_TILING_OPTIMAL,s->usage,0,&limits)!=VK_SUCCESS||
       s->width>limits.maxExtent.width||s->height>limits.maxExtent.height||limits.maxExtent.depth<1||
       !limits.maxMipLevels||!limits.maxArrayLayers||!(limits.sampleCounts&VK_SAMPLE_COUNT_1_BIT))return 1;
    const VkImageCreateInfo info={.sType=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,.imageType=VK_IMAGE_TYPE_2D,
        .format=s->format,.extent={s->width,s->height,1},.mipLevels=1,.arrayLayers=1,
        .samples=VK_SAMPLE_COUNT_1_BIT,.tiling=VK_IMAGE_TILING_OPTIMAL,.usage=s->usage,
        .sharingMode=VK_SHARING_MODE_EXCLUSIVE,.initialLayout=VK_IMAGE_LAYOUT_UNDEFINED};
    VkImage image=VK_NULL_HANDLE;
    VkResult result=create(s->device,&info,NULL,&image);
    if(result!=VK_SUCCESS){s->stage=3;return 1;}
    s->image=image;
    if(!image){s->stage=4;return 2;}
    const VkImageMemoryRequirementsInfo2 query={.sType=VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2,.image=image};
    VkMemoryDedicatedRequirements dedicated={.sType=VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS};
    VkMemoryRequirements2 out={.sType=VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2,.pNext=&dedicated};
    requirements(s->device,&query,&out);
    s->requirements=out.memoryRequirements;
    if(!s->requirements.size||!s->requirements.alignment||!s->requirements.memoryTypeBits){s->stage=4;return 2;}
    s->stage=1;*bytes=s->requirements.size;*types=s->requirements.memoryTypeBits;return 0;
}
uint32_t cubit_vulkan_owned_image_bind(void *context,uint64_t charged,uint32_t type)
{
    struct cubit_vulkan_owned_image *s=context;
    if(!s||s->stage!=1)return 2;
    if(charged!=s->requirements.size||type>=32||!(s->requirements.memoryTypeBits&(UINT32_C(1)<<type)))
        return discard(s);
    /* Dedicated backing handles mandatory-dedicated images too. Atlas policy
     * belongs above this boundary; this is not a per-glyph allocation policy. */
    const VkMemoryDedicatedAllocateInfo dedicated={.sType=VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO,.image=s->image};
    const VkMemoryAllocateInfo info={.sType=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,.pNext=&dedicated,
        .allocationSize=charged,.memoryTypeIndex=type};
    VkDeviceMemory memory=VK_NULL_HANDLE;
    VkResult result=s->allocate(s->device,&info,NULL,&memory);
    if(result!=VK_SUCCESS)return discard(s);
    s->memory=memory;
    if(!memory){s->stage=4;return 2;}
    result=s->bind(s->device,s->image,memory,0);
    if(result!=VK_SUCCESS){s->stage=4;return 2;}
    s->stage=2;return 0;
}
uint32_t cubit_vulkan_owned_image_release(void *context)
{
    struct cubit_vulkan_owned_image *s=context;
    if(!s||(s->stage!=1&&s->stage!=2)||!s->destroy||!s->free_memory)return 2;
    (void)discard(s);return 0;
}
