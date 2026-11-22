#include "vulkan_context.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define CHECK(x) do {if(!(x)){fprintf(stderr,"context check failed line %d: %s\n",__LINE__,#x);exit(1);}}while(0)
#define HANDLE(type,n) ((type)(uintptr_t)(n))
static unsigned stage,fail_at,lookup,missing,live_pool,live_fence,live_pass,destroyed;
static VkDevice device;
static VkResult create_pool(VkDevice d,const VkCommandPoolCreateInfo *i,const VkAllocationCallbacks *a,VkCommandPool *out)
{
    CHECK(d==device&&!a&&i->queueFamilyIndex==3&&i->flags==VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT);
    *out=HANDLE(VkCommandPool,99);if(++stage==fail_at)return VK_ERROR_OUT_OF_HOST_MEMORY;
    *out=HANDLE(VkCommandPool,2);live_pool++;return VK_SUCCESS;
}
static VkResult allocate(VkDevice d,const VkCommandBufferAllocateInfo *i,VkCommandBuffer *out)
{
    CHECK(d==device&&i->commandPool==HANDLE(VkCommandPool,2)&&i->commandBufferCount==1&&i->level==VK_COMMAND_BUFFER_LEVEL_PRIMARY);
    *out=HANDLE(VkCommandBuffer,99);if(++stage==fail_at)return VK_ERROR_OUT_OF_HOST_MEMORY;
    *out=HANDLE(VkCommandBuffer,3);return VK_SUCCESS;
}
static VkResult create_fence(VkDevice d,const VkFenceCreateInfo *i,const VkAllocationCallbacks *a,VkFence *out)
{
    CHECK(d==device&&!a&&!i->flags);*out=HANDLE(VkFence,99);
    if(++stage==fail_at)return VK_ERROR_OUT_OF_HOST_MEMORY;
    *out=HANDLE(VkFence,4);live_fence++;return VK_SUCCESS;
}
static VkResult create_pass(VkDevice d,const VkRenderPassCreateInfo *i,const VkAllocationCallbacks *a,VkRenderPass *out)
{
    CHECK(d==device&&!a&&i->attachmentCount==1&&i->subpassCount==1&&i->dependencyCount==1);
    CHECK(i->pAttachments->format==VK_FORMAT_B8G8R8A8_UNORM&&i->pAttachments->loadOp==VK_ATTACHMENT_LOAD_OP_LOAD);
    CHECK(i->pAttachments->storeOp==VK_ATTACHMENT_STORE_OP_STORE&&i->pAttachments->initialLayout==VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL);
    CHECK(i->pSubpasses->colorAttachmentCount==1&&i->pDependencies->dstAccessMask==(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT));
    *out=HANDLE(VkRenderPass,99);if(++stage==fail_at)return VK_ERROR_OUT_OF_HOST_MEMORY;
    *out=HANDLE(VkRenderPass,5);live_pass++;return VK_SUCCESS;
}
static void destroy_pool(VkDevice d,VkCommandPool p,const VkAllocationCallbacks *a)
{CHECK(d==device&&!a&&p==HANDLE(VkCommandPool,2)&&live_pool==1);live_pool--;destroyed++;}
static void destroy_fence(VkDevice d,VkFence p,const VkAllocationCallbacks *a)
{CHECK(d==device&&!a&&p==HANDLE(VkFence,4)&&live_fence==1);live_fence--;destroyed++;}
static void destroy_pass(VkDevice d,VkRenderPass p,const VkAllocationCallbacks *a)
{CHECK(d==device&&!a&&p==HANDLE(VkRenderPass,5)&&live_pass==1);live_pass--;destroyed++;}
static void unused(void){CHECK(0);}
static PFN_vkVoidFunction get_device(VkDevice d,const char *name)
{
    CHECK(d==device);if(++lookup==missing)return NULL;
#define PROC(n,f) if(!strcmp(name,n))return (PFN_vkVoidFunction)f
    PROC("vkCreateCommandPool",create_pool);PROC("vkAllocateCommandBuffers",allocate);
    PROC("vkCreateFence",create_fence);PROC("vkCreateRenderPass",create_pass);
    PROC("vkDestroyCommandPool",destroy_pool);PROC("vkDestroyFence",destroy_fence);PROC("vkDestroyRenderPass",destroy_pass);
#undef PROC
    return unused;
}
static PFN_vkVoidFunction get_instance(VkInstance i,const char *name)
{CHECK(i==HANDLE(VkInstance,6)&&!strcmp(name,"vkGetDeviceProcAddr"));return missing==100?NULL:(PFN_vkVoidFunction)get_device;}
static void run(unsigned fail,unsigned miss)
{
    struct cubit_vulkan_context c={0};
    struct cubit_mesa_service_device v={.instance=HANDLE(VkInstance,6),.physical=HANDLE(VkPhysicalDevice,7),
        .device=HANDLE(VkDevice,1),.queue=HANDLE(VkQueue,8),.family=3,.instance_proc=get_instance};
    struct cubit_vulkan_context_request r={&c,&v};
    device=v.device;stage=lookup=destroyed=0;fail_at=fail;missing=miss;
    void *out=(void *)(uintptr_t)99;
    uint32_t result=cubit_vulkan_context_create(&r,&out);
    if(fail||miss){CHECK(result==1&&!out&&!c.live&&!live_pool&&!live_fence&&!live_pass);}
    else {
        CHECK(result==0&&out==&c.submission&&c.live&&live_pool==1&&live_fence==1&&live_pass==1);
        CHECK(c.submission.device==v.device&&c.submission.queue==v.queue&&c.submission.command==HANDLE(VkCommandBuffer,3));
    }
    unsigned before=lookup,old_stage=stage;
    CHECK(cubit_vulkan_context_create(&r,&out)==2&&!out&&lookup==before&&stage==old_stage);
    CHECK(cubit_vulkan_context_release(&r)==((fail||miss)?2:0));
    CHECK(!live_pool&&!live_fence&&!live_pass&&!c.live);
    before=destroyed;CHECK(cubit_vulkan_context_release(&r)==2&&destroyed==before);
    CHECK(cubit_vulkan_context_create(&r,&out)==2&&!out);
}
int main(void)
{
    run(0,0);
    for(unsigned i=1;i<=4;i++)run(i,0);
    for(unsigned i=1;i<=16;i++)run(0,i);
    run(0,100);
    void *out=(void *)(uintptr_t)99;
    CHECK(cubit_vulkan_context_create(NULL,&out)==1&&!out);
    CHECK(cubit_vulkan_context_create(NULL,NULL)==1);
    CHECK(cubit_vulkan_context_release(NULL)==2);
    struct cubit_vulkan_context c={0};struct cubit_vulkan_context_request r={&c,NULL};
    CHECK(cubit_vulkan_context_create(&r,&out)==1&&!out&&c.attempted);
    CHECK(cubit_vulkan_context_create(&r,&out)==2&&!out);
    puts("VULKAN CONTEXT: PASS four allocation faults, sixteen missing dispatch entries, one-attempt ownership and exact rollback");
}
