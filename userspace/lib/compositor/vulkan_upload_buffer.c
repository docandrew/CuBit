#include "vulkan_upload_buffer.h"
static uint32_t discard(struct cubit_vulkan_upload_buffer *s)
{
    if(s->mapped)s->unmap(s->device,s->memory);
    if(s->buffer)s->destroy(s->device,s->buffer,NULL);
    if(s->memory)s->free_memory(s->device,s->memory,NULL);
    s->mapped=NULL;s->buffer=VK_NULL_HANDLE;s->memory=VK_NULL_HANDLE;s->stage=3;
    return 1;
}
uint32_t cubit_vulkan_upload_prepare(void *request,uint32_t capacity,uint64_t *bytes,uint32_t *types)
{
    if(!bytes||!types)return 1;
    *bytes=0;*types=0;
    struct cubit_vulkan_upload_buffer *s=request;
    if(!s||s->stage||s->buffer||s->memory||s->mapped)return 2;
    if(!capacity||capacity>CUBIT_VULKAN_UPLOAD_MAX_BYTES||!s->instance||!s->physical||
       !s->device||!s->instance_proc||!s->proc)return 1;
    PFN_vkGetPhysicalDeviceMemoryProperties properties=(PFN_vkGetPhysicalDeviceMemoryProperties)
        s->instance_proc(s->instance,"vkGetPhysicalDeviceMemoryProperties");
    PFN_vkCreateBuffer create=(PFN_vkCreateBuffer)s->proc(s->device,"vkCreateBuffer");
    PFN_vkGetBufferMemoryRequirements2 requirements=(PFN_vkGetBufferMemoryRequirements2)
        s->proc(s->device,"vkGetBufferMemoryRequirements2");
    s->destroy=(PFN_vkDestroyBuffer)s->proc(s->device,"vkDestroyBuffer");
    s->free_memory=(PFN_vkFreeMemory)s->proc(s->device,"vkFreeMemory");
    s->unmap=(PFN_vkUnmapMemory)s->proc(s->device,"vkUnmapMemory");
    s->allocate=(PFN_vkAllocateMemory)s->proc(s->device,"vkAllocateMemory");
    s->bind=(PFN_vkBindBufferMemory)s->proc(s->device,"vkBindBufferMemory");
    s->map=(PFN_vkMapMemory)s->proc(s->device,"vkMapMemory");
    if(!properties||!create||!requirements||!s->destroy||!s->free_memory||!s->unmap||
       !s->allocate||!s->bind||!s->map)return 1;
    VkPhysicalDeviceMemoryProperties memory={0};properties(s->physical,&memory);
    if(!memory.memoryTypeCount||memory.memoryTypeCount>32)return 1;
    uint32_t allowed=0;
    for(uint32_t n=0;n<memory.memoryTypeCount;n++){
        const VkMemoryPropertyFlags flags=memory.memoryTypes[n].propertyFlags;
        const VkMemoryPropertyFlags needed=VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
        if((flags&needed)==needed&&!(flags&(VK_MEMORY_PROPERTY_PROTECTED_BIT|VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT)))allowed|=UINT32_C(1)<<n;
    }
    if(!allowed)return 1;
    const VkBufferCreateInfo info={.sType=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,.size=capacity,
        .usage=VK_BUFFER_USAGE_TRANSFER_SRC_BIT,.sharingMode=VK_SHARING_MODE_EXCLUSIVE};
    VkBuffer buffer=VK_NULL_HANDLE;
    if(create(s->device,&info,NULL,&buffer)!=VK_SUCCESS){s->stage=3;return 1;}
    s->buffer=buffer;s->capacity=capacity;
    if(!buffer){s->stage=4;return 2;}
    const VkBufferMemoryRequirementsInfo2 query={.sType=VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2,.buffer=buffer};
    VkMemoryDedicatedRequirements dedicated={.sType=VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS};
    VkMemoryRequirements2 result={.sType=VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2,.pNext=&dedicated};
    requirements(s->device,&query,&result);s->requirements=result.memoryRequirements;
    if(s->requirements.size<capacity||!s->requirements.alignment||!s->requirements.memoryTypeBits){s->stage=4;return 2;}
    s->types=allowed&s->requirements.memoryTypeBits;
    if(!s->types)return discard(s);
    s->stage=1;*bytes=s->requirements.size;*types=s->types;return 0;
}
uint32_t cubit_vulkan_upload_bind(void *request,uint64_t charged,uint32_t type,void **mapped)
{
    if(!mapped)return 2;
    *mapped=NULL;
    struct cubit_vulkan_upload_buffer *s=request;
    if(!s||s->stage!=1||s->memory||s->mapped)return 2;
    if(charged!=s->requirements.size||type>=32||!(s->types&(UINT32_C(1)<<type)))return discard(s);
    const VkMemoryDedicatedAllocateInfo dedicated={.sType=VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO,.buffer=s->buffer};
    const VkMemoryAllocateInfo info={.sType=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,.pNext=&dedicated,
        .allocationSize=charged,.memoryTypeIndex=type};
    VkDeviceMemory memory=VK_NULL_HANDLE;
    if(s->allocate(s->device,&info,NULL,&memory)!=VK_SUCCESS)return discard(s);
    s->memory=memory;
    if(!memory||s->bind(s->device,s->buffer,memory,0)!=VK_SUCCESS){s->stage=4;return 2;}
    void *address=NULL;
    if(s->map(s->device,memory,0,charged,0,&address)!=VK_SUCCESS||!address){s->stage=4;return 2;}
    s->mapped=address;s->stage=2;*mapped=address;return 0;
}
uint32_t cubit_vulkan_upload_release(void *request)
{
    struct cubit_vulkan_upload_buffer *s=request;
    if(!s||(s->stage!=1&&s->stage!=2)||!s->destroy||!s->free_memory||!s->unmap)return 2;
    (void)discard(s);return 0;
}
