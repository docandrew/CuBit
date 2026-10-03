#include "vulkan_context.h"
static void discard(struct cubit_vulkan_context *c)
{
    if(c->pass) { c->destroy_pass(c->device,c->pass,NULL); c->pass=VK_NULL_HANDLE; }
    if(c->fence) { c->destroy_fence(c->device,c->fence,NULL); c->fence=VK_NULL_HANDLE; }
    if(c->pool) { c->destroy_pool(c->device,c->pool,NULL); c->pool=VK_NULL_HANDLE; }
    c->submission=(struct cubit_vulkan_submission){0}; c->live=0;
}
uint32_t cubit_vulkan_context_create(void *description,void **submission)
{
    const struct cubit_vulkan_context_request *r=description;
    if(!submission)return 1;
    *submission=NULL;
    if(!r||!r->fresh)return 1;
    struct cubit_vulkan_context *c=r->fresh;
    if(c->attempted)return 2;
    c->attempted=1;
    const struct cubit_mesa_service_device *v=r->device;
    if(!v||!v->instance||!v->physical||!v->device||!v->queue||!v->instance_proc||
       v->family==VK_QUEUE_FAMILY_IGNORED)return 1;
    PFN_vkGetDeviceProcAddr proc=(PFN_vkGetDeviceProcAddr)v->instance_proc(v->instance,"vkGetDeviceProcAddr");
    if(!proc)return 1;
    c->device=v->device;
#define LOAD(type,var,name) PFN_vk##type var=(PFN_vk##type)proc(c->device,name);if(!var)return 1
    LOAD(CreateCommandPool,create_pool,"vkCreateCommandPool");
    LOAD(AllocateCommandBuffers,allocate,"vkAllocateCommandBuffers");
    LOAD(CreateFence,create_fence,"vkCreateFence");
    LOAD(CreateRenderPass,create_pass,"vkCreateRenderPass");
#undef LOAD
    c->destroy_pool=(PFN_vkDestroyCommandPool)proc(c->device,"vkDestroyCommandPool");
    c->destroy_fence=(PFN_vkDestroyFence)proc(c->device,"vkDestroyFence");
    c->destroy_pass=(PFN_vkDestroyRenderPass)proc(c->device,"vkDestroyRenderPass");
    if(!c->destroy_pool||!c->destroy_fence||!c->destroy_pass)return 1;
    const VkCommandPoolCreateInfo pi={.sType=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        .flags=VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,.queueFamilyIndex=v->family};
    VkCommandPool pool=VK_NULL_HANDLE;
    if(create_pool(c->device,&pi,NULL,&pool)!=VK_SUCCESS)return 1;
    c->pool=pool;
    if(!pool)goto failed;
    const VkCommandBufferAllocateInfo ai={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        .commandPool=pool,.level=VK_COMMAND_BUFFER_LEVEL_PRIMARY,.commandBufferCount=1};
    VkCommandBuffer command=VK_NULL_HANDLE;
    if(allocate(c->device,&ai,&command)!=VK_SUCCESS||!command)goto failed;
    const VkFenceCreateInfo fi={.sType=VK_STRUCTURE_TYPE_FENCE_CREATE_INFO};
    VkFence fence=VK_NULL_HANDLE;
    if(create_fence(c->device,&fi,NULL,&fence)!=VK_SUCCESS)goto failed;
    c->fence=fence;
    if(!fence)goto failed;
    const VkAttachmentDescription attachment={.format=VK_FORMAT_B8G8R8A8_UNORM,
        .samples=VK_SAMPLE_COUNT_1_BIT,.loadOp=VK_ATTACHMENT_LOAD_OP_LOAD,
        .storeOp=VK_ATTACHMENT_STORE_OP_STORE,.stencilLoadOp=VK_ATTACHMENT_LOAD_OP_DONT_CARE,
        .stencilStoreOp=VK_ATTACHMENT_STORE_OP_DONT_CARE,
        .initialLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,.finalLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL};
    const VkAttachmentReference color={0,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL};
    const VkSubpassDescription subpass={.pipelineBindPoint=VK_PIPELINE_BIND_POINT_GRAPHICS,
        .colorAttachmentCount=1,.pColorAttachments=&color};
    const VkSubpassDependency dependency={.srcSubpass=VK_SUBPASS_EXTERNAL,.dstSubpass=0,
        .srcStageMask=VK_PIPELINE_STAGE_ALL_COMMANDS_BIT,.dstStageMask=VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        .srcAccessMask=VK_ACCESS_MEMORY_WRITE_BIT,
        .dstAccessMask=VK_ACCESS_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT};
    const VkRenderPassCreateInfo pci={.sType=VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO,
        .attachmentCount=1,.pAttachments=&attachment,.subpassCount=1,.pSubpasses=&subpass,
        .dependencyCount=1,.pDependencies=&dependency};
    VkRenderPass pass=VK_NULL_HANDLE;
    if(create_pass(c->device,&pci,NULL,&pass)!=VK_SUCCESS)goto failed;
    c->pass=pass;
    if(!pass)goto failed;
    if(cubit_vulkan_submission_init(&c->submission,c->device,v->queue,command,fence,proc)!=0)goto failed;
    c->live=1; *submission=&c->submission; return 0;
failed:
    discard(c); return 1;
}
uint32_t cubit_vulkan_context_release(void *description)
{
    const struct cubit_vulkan_context_request *r=description;
    if(!r||!r->fresh||!r->fresh->attempted||!r->fresh->live)return 2;
    discard(r->fresh); return 0;
}
