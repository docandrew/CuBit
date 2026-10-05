#include "vulkan_upload_buffer.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
#define H(t,n) ((t)(uintptr_t)(n))
static unsigned fault,creates,destroys,allocations,frees,maps,unmaps;
static unsigned char pixels[8192];
static void VKAPI_CALL properties(VkPhysicalDevice p,VkPhysicalDeviceMemoryProperties *m)
{
    (void)p;memset(m,0,sizeof(*m));m->memoryTypeCount=fault==18?33:4;
    m->memoryTypes[0].propertyFlags=VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
    m->memoryTypes[1].propertyFlags=VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;
    m->memoryTypes[2].propertyFlags=fault==8?0:VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
    m->memoryTypes[3].propertyFlags=VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|VK_MEMORY_PROPERTY_HOST_COHERENT_BIT|VK_MEMORY_PROPERTY_PROTECTED_BIT;
}
static VkResult VKAPI_CALL create(VkDevice d,const VkBufferCreateInfo *i,const VkAllocationCallbacks *a,VkBuffer *b)
{(void)d;(void)a;++creates;assert(i->size==3072&&i->usage==VK_BUFFER_USAGE_TRANSFER_SRC_BIT);*b=fault==5?VK_NULL_HANDLE:H(VkBuffer,50);return fault==4?VK_ERROR_OUT_OF_HOST_MEMORY:VK_SUCCESS;}
static void VKAPI_CALL requirements(VkDevice d,const VkBufferMemoryRequirementsInfo2 *i,VkMemoryRequirements2 *o)
{(void)d;assert(i->buffer==H(VkBuffer,50));o->memoryRequirements=(VkMemoryRequirements){fault==6?3071:8192,fault==7?0:256,fault==9?1:15};((VkMemoryDedicatedRequirements *)o->pNext)->requiresDedicatedAllocation=VK_TRUE;}
static void VKAPI_CALL destroy(VkDevice d,VkBuffer b,const VkAllocationCallbacks *a)
{(void)d;(void)a;assert(b==H(VkBuffer,50));++destroys;}
static void VKAPI_CALL free_memory(VkDevice d,VkDeviceMemory m,const VkAllocationCallbacks *a)
{(void)d;(void)a;assert(m==H(VkDeviceMemory,60));++frees;}
static void VKAPI_CALL unmap(VkDevice d,VkDeviceMemory m)
{(void)d;assert(m==H(VkDeviceMemory,60));++unmaps;}
static VkResult VKAPI_CALL allocate(VkDevice d,const VkMemoryAllocateInfo *i,const VkAllocationCallbacks *a,VkDeviceMemory *m)
{(void)d;(void)a;++allocations;assert(i->allocationSize==8192&&i->memoryTypeIndex==2);assert(((const VkMemoryDedicatedAllocateInfo *)i->pNext)->buffer==H(VkBuffer,50));*m=fault==13?VK_NULL_HANDLE:H(VkDeviceMemory,60);return fault==12?VK_ERROR_OUT_OF_DEVICE_MEMORY:VK_SUCCESS;}
static VkResult VKAPI_CALL bind(VkDevice d,VkBuffer b,VkDeviceMemory m,VkDeviceSize offset)
{(void)d;assert(b==H(VkBuffer,50)&&m==H(VkDeviceMemory,60)&&offset==0);return fault==14?VK_ERROR_DEVICE_LOST:VK_SUCCESS;}
static VkResult VKAPI_CALL map(VkDevice d,VkDeviceMemory m,VkDeviceSize offset,VkDeviceSize size,VkMemoryMapFlags flags,void **p)
{(void)d;++maps;assert(m==H(VkDeviceMemory,60)&&offset==0&&size==8192&&!flags);*p=fault==16?NULL:pixels;return fault==15?VK_ERROR_MEMORY_MAP_FAILED:VK_SUCCESS;}
static PFN_vkVoidFunction VKAPI_CALL proc(VkDevice d,const char *n)
{
    (void)d;
#define F(name,fn) if(!strcmp(n,name))return (PFN_vkVoidFunction)fn;
    F("vkCreateBuffer",create) F("vkGetBufferMemoryRequirements2",requirements)
    F("vkDestroyBuffer",destroy) F("vkFreeMemory",free_memory)
    if(!strcmp(n,"vkUnmapMemory"))return fault==17?NULL:(PFN_vkVoidFunction)unmap;
    F("vkAllocateMemory",allocate) F("vkBindBufferMemory",bind) F("vkMapMemory",map)
#undef F
    return NULL;
}
static PFN_vkVoidFunction VKAPI_CALL instance_proc(VkInstance i,const char *n)
{(void)i;return !strcmp(n,"vkGetPhysicalDeviceMemoryProperties")?(PFN_vkVoidFunction)properties:NULL;}
int main(void)
{
    for(fault=0;fault<19;fault++){
        creates=destroys=allocations=frees=maps=unmaps=0;
        struct cubit_vulkan_upload_buffer s={.instance=H(VkInstance,1),.physical=H(VkPhysicalDevice,2),
            .device=H(VkDevice,3),.instance_proc=instance_proc,.proc=fault==3?NULL:proc};
        uint64_t bytes=99;uint32_t types=99;void *mapping=(void *)1;
        uint32_t result=cubit_vulkan_upload_prepare(&s,fault==1?0:fault==2?CUBIT_VULKAN_UPLOAD_MAX_BYTES+1:3072,&bytes,&types);
        if(result==0){
            assert(bytes==8192&&types==4&&s.stage==1);
            result=cubit_vulkan_upload_bind(&s,fault==10?8191:bytes,fault==11?0:2,&mapping);
            if(result!=0)assert(mapping==NULL);
        }else assert(!bytes&&!types);
        if(fault==0){
            assert(!result&&mapping==pixels&&s.stage==2&&s.capacity==3072);
            struct cubit_vulkan_upload_buffer before=s;
            assert(cubit_vulkan_upload_prepare(&s,3072,&bytes,&types)==2);
            assert(!memcmp(&before,&s,sizeof(s)));
            assert(cubit_vulkan_upload_release(&s)==0);
            assert(!s.buffer&&!s.memory&&!s.mapped&&s.stage==3);
            assert(creates==1&&allocations==1&&maps==1&&destroys==1&&frees==1&&unmaps==1);
        }else{
            assert(result!=0);
            if(s.stage==4){
                assert(result==2&&destroys==0&&frees==0&&unmaps==0);
                assert(cubit_vulkan_upload_release(&s)==2);
            }else assert(result==1&&!s.buffer&&!s.memory&&!s.mapped);
        }
    }
    puts("PASS upload C boundary: coherent type filtering, dedicated actual-size binding/map, 19 paths, dirty/repeated/live/uncertain retention and confirmed unmap/free");
}
