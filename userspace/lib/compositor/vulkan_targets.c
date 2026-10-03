#include "vulkan_targets.h"
static void clear_views(struct cubit_vulkan_targets *s)
{
    for(unsigned i=0;i<CUBIT_VULKAN_TARGET_COUNT;i++){
        if(s->framebuffers[i])s->destroy_framebuffer(s->device,s->framebuffers[i],NULL);
        if(s->views[i])s->destroy_view(s->device,s->views[i],NULL);
        s->framebuffers[i]=VK_NULL_HANDLE;s->views[i]=VK_NULL_HANDLE;
        s->scenes[i]=(struct cubit_vulkan_scene){0};
    }
}
uint32_t cubit_vulkan_targets_create(void *description,void **a,void **b,void **c)
{
    if(!a||!b||!c)return 1;
    *a=NULL;*b=NULL;*c=NULL;
    const struct cubit_vulkan_target_request *r=description;
    if(!r||!r->fresh||!r->device||!r->proc||!r->pass||!r->width||r->width>65535||!r->height||r->height>65535)return 1;
    for(unsigned i=0;i<CUBIT_VULKAN_TARGET_COUNT;i++){
        if(!r->images[i])return 1;
        for(unsigned j=0;j<i;j++)if(r->images[i]==r->images[j])return 1;
    }
    PFN_vkCreateImageView create_view=(PFN_vkCreateImageView)r->proc(r->device,"vkCreateImageView");
    PFN_vkDestroyImageView destroy_view=(PFN_vkDestroyImageView)r->proc(r->device,"vkDestroyImageView");
    PFN_vkCreateFramebuffer create_framebuffer=(PFN_vkCreateFramebuffer)r->proc(r->device,"vkCreateFramebuffer");
    PFN_vkDestroyFramebuffer destroy_framebuffer=(PFN_vkDestroyFramebuffer)r->proc(r->device,"vkDestroyFramebuffer");
    if(!create_view||!destroy_view||!create_framebuffer||!destroy_framebuffer)return 1;
    struct cubit_vulkan_targets *s=r->fresh;
    *s=(struct cubit_vulkan_targets){.device=r->device,.destroy_view=destroy_view,
        .destroy_framebuffer=destroy_framebuffer,.clear=r->clear};
    for(unsigned i=0;i<CUBIT_VULKAN_TARGET_COUNT;i++){
        const VkImageViewCreateInfo v={.sType=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
            .image=r->images[i],.viewType=VK_IMAGE_VIEW_TYPE_2D,.format=VK_FORMAT_B8G8R8A8_UNORM,
            .subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
        VkImageView view=VK_NULL_HANDLE;
        VkResult result=create_view(r->device,&v,NULL,&view);
        if(result!=VK_SUCCESS){clear_views(s);return 1;}
        if(!view)return 2;
        s->views[i]=view;
        const VkFramebufferCreateInfo f={.sType=VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO,
            .renderPass=r->pass,.attachmentCount=1,.pAttachments=&s->views[i],
            .width=r->width,.height=r->height,.layers=1};
        VkFramebuffer framebuffer=VK_NULL_HANDLE;
        result=create_framebuffer(r->device,&f,NULL,&framebuffer);
        if(result!=VK_SUCCESS){clear_views(s);return 1;}
        if(!framebuffer)return 2;
        s->framebuffers[i]=framebuffer;
        s->scenes[i]=(struct cubit_vulkan_scene){.device=r->device,
            .begin={.sType=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,.renderPass=r->pass,
                .framebuffer=framebuffer,.renderArea={{0,0},{r->width,r->height}},
                .clearValueCount=1,.pClearValues=&s->clear}};
    }
    *a=&s->scenes[0];*b=&s->scenes[1];*c=&s->scenes[2];return 0;
}
uint32_t cubit_vulkan_targets_release(void *description)
{
    const struct cubit_vulkan_target_request *r=description;
    if(!r||!r->fresh)return 2;
    struct cubit_vulkan_targets *s=r->fresh;
    if(!s->device||!s->destroy_view||!s->destroy_framebuffer)return 2;
    clear_views(s);*s=(struct cubit_vulkan_targets){0};return 0;
}
