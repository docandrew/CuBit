#include "vulkan_owned_image.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static unsigned creates,allocations,binds,destroys,frees,fault;
static const char *missing;
#define HANDLE(t,n) ((t)(uintptr_t)(n))
static VkResult VKAPI_CALL formats(VkPhysicalDevice p,VkFormat f,VkImageType t,VkImageTiling tiling,
                                   VkImageUsageFlags usage,VkImageCreateFlags flags,VkImageFormatProperties *out)
{
    (void)p;(void)f;(void)t;(void)tiling;(void)usage;assert(!flags);
    *out=(VkImageFormatProperties){.maxExtent={4096,4096,1},.maxMipLevels=1,.maxArrayLayers=1,
        .sampleCounts=VK_SAMPLE_COUNT_1_BIT,.maxResourceSize=UINT64_MAX};
    return fault==1?VK_ERROR_FORMAT_NOT_SUPPORTED:VK_SUCCESS;
}
static VkResult VKAPI_CALL create(VkDevice d,const VkImageCreateInfo *i,const VkAllocationCallbacks *a,VkImage *out)
{
    (void)d;assert(!a);assert(i->extent.width==32&&i->extent.height==24);
    assert(!i->pNext&&i->tiling==VK_IMAGE_TILING_OPTIMAL&&i->initialLayout==VK_IMAGE_LAYOUT_UNDEFINED);
    ++creates;
    if(fault==2)return VK_ERROR_OUT_OF_HOST_MEMORY;
    *out=fault==3?VK_NULL_HANDLE:HANDLE(VkImage,2);return VK_SUCCESS;
}
static void VKAPI_CALL requirements(VkDevice d,const VkImageMemoryRequirementsInfo2 *i,VkMemoryRequirements2 *out)
{
    (void)d;assert(i->image==HANDLE(VkImage,2));
    VkMemoryDedicatedRequirements *dedicated=out->pNext;
    assert(dedicated&&dedicated->sType==VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS);
    dedicated->requiresDedicatedAllocation=VK_TRUE;
    out->memoryRequirements=(VkMemoryRequirements){.size=fault==4?0:4096,.alignment=256,.memoryTypeBits=UINT32_C(1)<<31};
}
static VkResult VKAPI_CALL allocate(VkDevice d,const VkMemoryAllocateInfo *i,const VkAllocationCallbacks *a,VkDeviceMemory *out)
{
    (void)d;assert(!a);assert(i->allocationSize==4096&&i->memoryTypeIndex==31);
    const VkMemoryDedicatedAllocateInfo *dedicated=i->pNext;
    assert(dedicated&&dedicated->image==HANDLE(VkImage,2)&&!dedicated->buffer);
    ++allocations;
    if(fault==5)return VK_ERROR_OUT_OF_DEVICE_MEMORY;
    *out=fault==6?VK_NULL_HANDLE:HANDLE(VkDeviceMemory,3);return VK_SUCCESS;
}
static VkResult VKAPI_CALL bind(VkDevice d,VkImage i,VkDeviceMemory m,VkDeviceSize offset)
{
    (void)d;assert(i==HANDLE(VkImage,2)&&m==HANDLE(VkDeviceMemory,3)&&!offset);++binds;
    return fault==7?VK_ERROR_DEVICE_LOST:VK_SUCCESS;
}
static void VKAPI_CALL destroy(VkDevice d,VkImage i,const VkAllocationCallbacks *a)
{(void)d;assert(i==HANDLE(VkImage,2)&&!a);++destroys;}
static void VKAPI_CALL free_memory(VkDevice d,VkDeviceMemory m,const VkAllocationCallbacks *a)
{(void)d;assert(m==HANDLE(VkDeviceMemory,3)&&!a);assert(destroys==1);++frees;}
static PFN_vkVoidFunction VKAPI_CALL proc(VkDevice d,const char *name)
{
    (void)d;if(missing&&!strcmp(name,missing))return NULL;
#define PROC(n,f) if(!strcmp(name,n))return (PFN_vkVoidFunction)f
    PROC("vkCreateImage",create);PROC("vkGetImageMemoryRequirements2",requirements);
    PROC("vkDestroyImage",destroy);PROC("vkFreeMemory",free_memory);
    PROC("vkAllocateMemory",allocate);PROC("vkBindImageMemory",bind);
    return NULL;
}
static PFN_vkVoidFunction VKAPI_CALL instance_proc(VkInstance instance,const char *name)
{(void)instance;if(missing&&!strcmp(name,missing))return NULL;return (PFN_vkVoidFunction)formats;}
static struct cubit_vulkan_owned_image fresh(void)
{
    creates=allocations=binds=destroys=frees=0;
    return (struct cubit_vulkan_owned_image){.physical=HANDLE(VkPhysicalDevice,1),.device=HANDLE(VkDevice,1),
        .instance=HANDLE(VkInstance,1),.instance_proc=instance_proc,.proc=proc,.width=32,.height=24,
        .format=VK_FORMAT_B8G8R8A8_UNORM,.usage=VK_IMAGE_USAGE_SAMPLED_BIT|VK_IMAGE_USAGE_TRANSFER_DST_BIT};
}
int main(void)
{
    uint64_t bytes;uint32_t types;
    const char *names[]={"vkCreateImage","vkGetImageMemoryRequirements2","vkDestroyImage","vkFreeMemory",
        "vkAllocateMemory","vkBindImageMemory","vkGetPhysicalDeviceImageFormatProperties"};
    for(unsigned i=0;i<7;i++){
        struct cubit_vulkan_owned_image s=fresh();missing=names[i];
        assert(cubit_vulkan_owned_image_prepare(&s,&bytes,&types)==1);
        assert(!creates&&!allocations&&!bytes&&!types);
    }
    missing=NULL;
    for(fault=0;fault<=7;fault++){
        struct cubit_vulkan_owned_image s=fresh();
        uint32_t result=cubit_vulkan_owned_image_prepare(&s,&bytes,&types);
        if(fault==1||fault==2){assert(result==1&&!allocations);continue;}
        if(fault==3||fault==4){assert(result==2&&!allocations&&!destroys);assert(cubit_vulkan_owned_image_release(&s)==2);continue;}
        assert(result==0&&bytes==4096&&types==(UINT32_C(1)<<31));
        assert(!allocations);result=cubit_vulkan_owned_image_bind(&s,bytes,31);
        if(fault==5){assert(result==1&&destroys==1&&!frees);continue;}
        if(fault==6||fault==7){assert(result==2&&!destroys&&!frees);assert(cubit_vulkan_owned_image_release(&s)==2);continue;}
        assert(result==0&&allocations==1&&binds==1);
        assert(cubit_vulkan_owned_image_bind(&s,bytes,31)==2&&allocations==1);
        assert(cubit_vulkan_owned_image_release(&s)==0&&destroys==1&&frees==1);
        assert(cubit_vulkan_owned_image_release(&s)==2&&destroys==1&&frees==1);
    }
    fault=0;
    for(unsigned i=0;i<4;i++){
        struct cubit_vulkan_owned_image s=fresh();
        assert(cubit_vulkan_owned_image_prepare(&s,&bytes,&types)==0);
        if(i==3)assert(cubit_vulkan_owned_image_release(&s)==0);
        else assert(cubit_vulkan_owned_image_bind(&s,i==0?4095:4096,i==1?32:i==2?0:31)==1);
        assert(destroys==1&&!frees&&!allocations);
    }
    for(unsigned i=0;i<7;i++){
        struct cubit_vulkan_owned_image s=fresh();
        switch(i){case 0:s.width=0;break;case 1:s.height=65536;break;case 2:s.width=4097;break;
        case 3:s.format=VK_FORMAT_R8_UNORM;s.usage=VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT|VK_IMAGE_USAGE_TRANSFER_SRC_BIT;break;
        case 4:s.usage|=VK_IMAGE_USAGE_STORAGE_BIT;break;case 5:s.format=VK_FORMAT_R32_SFLOAT;break;
        case 6:s.physical=VK_NULL_HANDLE;break;}
        assert(cubit_vulkan_owned_image_prepare(&s,&bytes,&types)==1&&!creates&&!allocations);
    }
    puts("Owned image FFI: missing dispatch, format/extent/usage, allocation/bind faults, dedicated backing, retirement order and double-call guards PASS");
    return 0;
}
