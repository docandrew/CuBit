#include "vulkan_sources.h"
#include <stddef.h>
_Static_assert(offsetof(struct cubit_vulkan_source,draw)==0,"release key layout");
VkResult cubit_vulkan_sources_init(struct cubit_vulkan_sources *s,
    const struct cubit_vulkan_affine_engine *engine,VkCommandBuffer command,
    PFN_vkGetDeviceProcAddr proc)
{
    if(!s)return VK_ERROR_INITIALIZATION_FAILED;
    *s=(struct cubit_vulkan_sources){.engine=engine,.command=command};
    if(!engine||!engine->device||!engine->descriptors||!engine->sampler||!command||!proc)
        return VK_ERROR_INITIALIZATION_FAILED;
#define LOAD(field,name) s->field=(PFN_vk##name)proc(engine->device,"vk" #name);if(!s->field)return VK_ERROR_INITIALIZATION_FAILED
    LOAD(destroy_pool,DestroyDescriptorPool);LOAD(create_view,CreateImageView);
    LOAD(destroy_view,DestroyImageView);LOAD(update,UpdateDescriptorSets);
#undef LOAD
    PFN_vkCreateDescriptorPool create=(PFN_vkCreateDescriptorPool)proc(engine->device,"vkCreateDescriptorPool");
    PFN_vkAllocateDescriptorSets allocate=(PFN_vkAllocateDescriptorSets)proc(engine->device,"vkAllocateDescriptorSets");
    if(!create||!allocate)return VK_ERROR_INITIALIZATION_FAILED;
    const VkDescriptorPoolSize size={VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,CUBIT_VULKAN_SOURCE_CAPACITY};
    const VkDescriptorPoolCreateInfo info={.sType=VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        .maxSets=CUBIT_VULKAN_SOURCE_CAPACITY,.poolSizeCount=1,.pPoolSizes=&size};
    VkResult result=create(engine->device,&info,NULL,&s->pool);
    if(result!=VK_SUCCESS){s->pool=VK_NULL_HANDLE;return result;}
    VkDescriptorSetLayout layouts[CUBIT_VULKAN_SOURCE_CAPACITY];
    VkDescriptorSet sets[CUBIT_VULKAN_SOURCE_CAPACITY];
    for(unsigned i=0;i<CUBIT_VULKAN_SOURCE_CAPACITY;i++)layouts[i]=engine->descriptors;
    const VkDescriptorSetAllocateInfo allocation={.sType=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        .descriptorPool=s->pool,.descriptorSetCount=CUBIT_VULKAN_SOURCE_CAPACITY,.pSetLayouts=layouts};
    result=allocate(engine->device,&allocation,sets);
    if(result!=VK_SUCCESS){s->destroy_pool(engine->device,s->pool,NULL);s->pool=VK_NULL_HANDLE;return result;}
    for(unsigned i=0;i<CUBIT_VULKAN_SOURCE_CAPACITY;i++){
        s->entries[i].owner=s;s->entries[i].descriptor=sets[i];
    }
    return VK_SUCCESS;
}
uint32_t cubit_vulkan_sources_destroy(struct cubit_vulkan_sources *s)
{
    if(!s)return 2;
    for(unsigned i=0;i<CUBIT_VULKAN_SOURCE_CAPACITY;i++)if(s->entries[i].view)return 2;
    if(s->pool){
        if(!s->engine||!s->engine->device||!s->destroy_pool)return 2;
        s->destroy_pool(s->engine->device,s->pool,NULL);
    }
    *s=(struct cubit_vulkan_sources){0};return 0;
}
uint32_t cubit_vulkan_source_import(void *description,void **draw)
{
    if(!draw)return 1;
    *draw=NULL;
    const struct cubit_vulkan_source_request *r=description;
    if(!r||!r->provider||r->slot>=CUBIT_VULKAN_SOURCE_CAPACITY||!r->image||
       (r->format!=VK_FORMAT_B8G8R8A8_UNORM&&r->format!=VK_FORMAT_R8_UNORM)||
       !r->output_width||r->output_width>65535||!r->output_height||r->output_height>65535)return 1;
    struct cubit_vulkan_sources *s=r->provider;
    if(!s->pool||!s->engine||!s->engine->device||!s->command||!s->create_view||!s->destroy_view||!s->update)return 1;
    struct cubit_vulkan_source *entry=&s->entries[r->slot];
    if(entry->view||entry->owner!=s||!entry->descriptor)return 1;
    const VkImageViewCreateInfo view={.sType=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
        .image=r->image,.viewType=VK_IMAGE_VIEW_TYPE_2D,.format=r->format,
        .subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
    VkImageView created=VK_NULL_HANDLE;
    VkResult result=s->create_view(s->engine->device,&view,NULL,&created);
    if(result!=VK_SUCCESS)return 1;
    if(!created)return 2; /* Successful creation with no handle is not trusted. */
    const VkDescriptorImageInfo image={s->engine->sampler,created,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL};
    const VkWriteDescriptorSet write={.sType=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
        .dstSet=entry->descriptor,.dstBinding=0,.descriptorCount=1,
        .descriptorType=VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,.pImageInfo=&image};
    s->update(s->engine->device,1,&write,0,NULL);
    entry->view=created;
    entry->draw=(struct cubit_vulkan_affine_draw){s->engine,s->command,entry->descriptor,r->output_width,r->output_height};
    *draw=&entry->draw;return 0;
}
uint32_t cubit_vulkan_source_release(void *draw)
{
    if(!draw)return 2;
    struct cubit_vulkan_source *entry=draw;
    struct cubit_vulkan_sources *s=entry->owner;
    if(!s||!s->engine||!s->engine->device||!s->destroy_view||!entry->view)return 2;
    s->destroy_view(s->engine->device,entry->view,NULL);
    entry->view=VK_NULL_HANDLE;entry->draw=(struct cubit_vulkan_affine_draw){0};
    return 0;
}
