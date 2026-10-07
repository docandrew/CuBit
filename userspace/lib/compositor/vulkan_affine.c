#include "vulkan_affine.h"
#include "vulkan-affine-shaders.h"
#include <stddef.h>
#include <string.h>
struct push_data { uint32_t u0[2], v0[2], ud[2], vd[2]; int32_t steps[4]; float tint[4]; uint32_t mask, padding[3], region[4]; };
_Static_assert(sizeof(struct push_data)==96,"push ABI");
_Static_assert(offsetof(struct push_data,mask)==64,"push mask ABI");
_Static_assert(offsetof(struct push_data,padding)==68,"preview logical extent ABI");
_Static_assert(offsetof(struct push_data,region)==80,"push region ABI");
_Static_assert(sizeof(struct cubit_vulkan_preview)==16,"preview placement ABI");
_Static_assert(sizeof(struct cubit_mesa_affine)==56,"Ada affine ABI");
_Static_assert(sizeof(struct cubit_vulkan_coefficients)==64,"Ada coefficient ABI");
void cubit_vulkan_affine_destroy(struct cubit_vulkan_affine_engine *e)
{
    if(!e)return;
    for(unsigned i=0;i<3;i++) if(e->pipeline[i]) e->destroy_pipeline(e->device,e->pipeline[i],NULL);
    if(e->sampler)e->destroy_sampler(e->device,e->sampler,NULL);
    if(e->layout)e->destroy_layout(e->device,e->layout,NULL);
    if(e->descriptors)e->destroy_descriptors(e->device,e->descriptors,NULL);
    memset(e,0,sizeof *e);
}
VkResult cubit_vulkan_affine_create(struct cubit_vulkan_affine_engine *e,
    VkDevice device,PFN_vkGetDeviceProcAddr proc,VkRenderPass pass)
{
    if(!e)return VK_ERROR_INITIALIZATION_FAILED;
    *e=(struct cubit_vulkan_affine_engine){.device=device};
    if(!device || !proc || !pass)return VK_ERROR_INITIALIZATION_FAILED;
#define LOAD(field,name) e->field=(PFN_vk##name)proc(device,"vk" #name); if(!e->field)return VK_ERROR_INITIALIZATION_FAILED
    LOAD(destroy_descriptors,DestroyDescriptorSetLayout); LOAD(destroy_layout,DestroyPipelineLayout);
    LOAD(destroy_pipeline,DestroyPipeline); LOAD(destroy_sampler,DestroySampler);
    LOAD(bind_pipeline,CmdBindPipeline); LOAD(bind_descriptors,CmdBindDescriptorSets);
    LOAD(viewport,CmdSetViewport); LOAD(scissor,CmdSetScissor); LOAD(constants,CmdPushConstants); LOAD(draw,CmdDraw);
#undef LOAD
#define LOAD(name) PFN_vk##name name=(PFN_vk##name)proc(device,"vk" #name); if(!name)return VK_ERROR_INITIALIZATION_FAILED
    LOAD(CreateDescriptorSetLayout); LOAD(CreatePipelineLayout); LOAD(CreateSampler);
    LOAD(CreateShaderModule); LOAD(DestroyShaderModule); LOAD(CreateGraphicsPipelines);
#undef LOAD
    VkResult result; VkShaderModule vertex=VK_NULL_HANDLE,fragment=VK_NULL_HANDLE;
#define TRY(expr) do { result=(expr); if(result!=VK_SUCCESS)goto done; } while(0)
    const VkDescriptorSetLayoutBinding binding={.binding=0,.descriptorType=VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
        .descriptorCount=1,.stageFlags=VK_SHADER_STAGE_FRAGMENT_BIT};
    const VkDescriptorSetLayoutCreateInfo descriptor_info={.sType=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        .bindingCount=1,.pBindings=&binding};
    TRY(CreateDescriptorSetLayout(device,&descriptor_info,NULL,&e->descriptors));
    const VkPushConstantRange range={VK_SHADER_STAGE_VERTEX_BIT|VK_SHADER_STAGE_FRAGMENT_BIT,0,sizeof(struct push_data)};
    const VkPipelineLayoutCreateInfo layout_info={.sType=VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        .setLayoutCount=1,.pSetLayouts=&e->descriptors,.pushConstantRangeCount=1,.pPushConstantRanges=&range};
    TRY(CreatePipelineLayout(device,&layout_info,NULL,&e->layout));
    const VkSamplerCreateInfo sampler_info={.sType=VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO,
        .magFilter=VK_FILTER_NEAREST,.minFilter=VK_FILTER_NEAREST,.mipmapMode=VK_SAMPLER_MIPMAP_MODE_NEAREST,
        .addressModeU=VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,.addressModeV=VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
        .addressModeW=VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,.minLod=0,.maxLod=0};
    TRY(CreateSampler(device,&sampler_info,NULL,&e->sampler));
    const VkShaderModuleCreateInfo vi={.sType=VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        .codeSize=sizeof(vulkan_affine_vertex),.pCode=vulkan_affine_vertex};
    const VkShaderModuleCreateInfo fi={.sType=VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        .codeSize=sizeof(vulkan_affine_fragment),.pCode=vulkan_affine_fragment};
    TRY(CreateShaderModule(device,&vi,NULL,&vertex)); TRY(CreateShaderModule(device,&fi,NULL,&fragment));
    const VkPipelineShaderStageCreateInfo stages[2]={
        {.sType=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,.stage=VK_SHADER_STAGE_VERTEX_BIT,.module=vertex,.pName="main"},
        {.sType=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,.stage=VK_SHADER_STAGE_FRAGMENT_BIT,.module=fragment,.pName="main"}};
    const VkPipelineVertexInputStateCreateInfo input={.sType=VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO};
    const VkPipelineInputAssemblyStateCreateInfo assembly={.sType=VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
        .topology=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST};
    const VkPipelineViewportStateCreateInfo view={.sType=VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,.viewportCount=1,.scissorCount=1};
    const VkPipelineRasterizationStateCreateInfo raster={.sType=VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
        .polygonMode=VK_POLYGON_MODE_FILL,.cullMode=VK_CULL_MODE_NONE,.frontFace=VK_FRONT_FACE_COUNTER_CLOCKWISE,.lineWidth=1};
    const VkPipelineMultisampleStateCreateInfo samples={.sType=VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,.rasterizationSamples=VK_SAMPLE_COUNT_1_BIT};
    const VkDynamicState dynamic_states[]={VK_DYNAMIC_STATE_VIEWPORT,VK_DYNAMIC_STATE_SCISSOR};
    const VkPipelineDynamicStateCreateInfo dynamic={.sType=VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO,.dynamicStateCount=2,.pDynamicStates=dynamic_states};
    for(unsigned over=0;over<3;over++) {
        const VkPipelineColorBlendAttachmentState attachment={.blendEnable=over!=0,
            .srcColorBlendFactor=over==2?VK_BLEND_FACTOR_SRC_ALPHA:VK_BLEND_FACTOR_ONE,.dstColorBlendFactor=VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,.colorBlendOp=VK_BLEND_OP_ADD,
            .srcAlphaBlendFactor=VK_BLEND_FACTOR_ONE,.dstAlphaBlendFactor=VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,.alphaBlendOp=VK_BLEND_OP_ADD,
            .colorWriteMask=VK_COLOR_COMPONENT_R_BIT|VK_COLOR_COMPONENT_G_BIT|VK_COLOR_COMPONENT_B_BIT|VK_COLOR_COMPONENT_A_BIT};
        const VkPipelineColorBlendStateCreateInfo blend={.sType=VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,.attachmentCount=1,.pAttachments=&attachment};
        const VkGraphicsPipelineCreateInfo graphics={.sType=VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,.stageCount=2,.pStages=stages,
            .pVertexInputState=&input,.pInputAssemblyState=&assembly,.pViewportState=&view,.pRasterizationState=&raster,.pMultisampleState=&samples,
            .pColorBlendState=&blend,.pDynamicState=&dynamic,.layout=e->layout,.renderPass=pass,.subpass=0};
        TRY(CreateGraphicsPipelines(device,VK_NULL_HANDLE,1,&graphics,NULL,&e->pipeline[over]));
    }
    result=VK_SUCCESS;
done:
    if(vertex)DestroyShaderModule(device,vertex,NULL);
    if(fragment)DestroyShaderModule(device,fragment,NULL);
    if(result!=VK_SUCCESS)cubit_vulkan_affine_destroy(e);
    return result;
#undef TRY
}
static uint32_t record_affine(void *borrowed,const struct cubit_mesa_affine *d,
    const struct cubit_vulkan_coefficients *c,uint32_t width,uint32_t height,uint32_t mask,uint32_t argb,
    const struct cubit_vulkan_source_region *region,const struct cubit_vulkan_preview *preview)
{
    const struct cubit_vulkan_affine_draw *b=borrowed;
    if(!b||!b->engine||!b->command||!b->source||!d||!c)return 1;
    const struct cubit_vulkan_affine_engine *e=b->engine;
    if(!e->layout||!e->pipeline[0]||!e->pipeline[1]||!e->pipeline[2]||!e->bind_pipeline||!e->bind_descriptors||
       !e->viewport||!e->scissor||!e->constants||!e->draw)return 1;
    if(!b->width||b->width>65535||!b->height||b->height>65535||
       (preview?(mask!=4||d->over!=0):(mask>1||d->over>2||(mask&&d->over!=1)))||
       d->clip_x>=b->width||d->clip_y>=b->height||!d->clip_w||!d->clip_h||
       d->clip_w>b->width-d->clip_x||d->clip_h>b->height-d->clip_y||
       width!=b->width||height!=b->height||c->ud<1||c->vd<1||
       c->ud>INT64_C(34359738368)||c->vd>INT64_C(34359738368)||
       c->u0 < -INT64_C(68719476736)||c->u0 > INT64_C(68719476736)||
       c->v0 < -INT64_C(68719476736)||c->v0 > INT64_C(68719476736)||
       c->ux < -16||c->ux > 16||c->uy < -16||c->uy > 16||
       c->vx < -16||c->vx > 16||c->vy < -16||c->vy > 16)return 1;
    if(region&&(!region->image_width||region->image_width>65535||
       !region->image_height||region->image_height>65535||
       !region->width||!region->height||region->x>=region->image_width||region->y>=region->image_height||
       region->width>region->image_width-region->x||region->height>region->image_height-region->y))return 1;
    if(preview&&(!d->logical_w||d->logical_w>65535||!d->logical_h||d->logical_h>65535||
       !preview->width||preview->width>UINT32_C(4294836225)||
       !preview->height||preview->height>UINT32_C(4294836225)||
       (int64_t)preview->left!=((int64_t)d->logical_w-preview->width)/2||
       (int64_t)preview->top!=((int64_t)d->logical_h-preview->height)/2))return 1;
    struct push_data push={.mask=mask};
    if(preview){push.padding[0]=d->logical_w;push.padding[1]=d->logical_h;
        push.region[0]=(uint32_t)preview->left;push.region[1]=(uint32_t)preview->top;
        push.region[2]=preview->width;push.region[3]=preview->height;}
    if(region){push.region[0]=region->x;push.region[1]=region->y;
        push.region[2]=region->width;push.region[3]=region->height;}
    const uint64_t values[4]={(uint64_t)c->u0,(uint64_t)c->v0,(uint64_t)c->ud,(uint64_t)c->vd};
    uint32_t *pairs[4]={push.u0,push.v0,push.ud,push.vd};
    for(unsigned i=0;i<4;i++){pairs[i][0]=(uint32_t)values[i];pairs[i][1]=(uint32_t)(values[i]>>32);}
    push.steps[0]=(int32_t)c->ux;push.steps[1]=(int32_t)c->uy;
    push.steps[2]=(int32_t)c->vx;push.steps[3]=(int32_t)c->vy;
    const float alpha=(float)(argb>>24)/255.0f;
    push.tint[0]=((argb>>16)&255)/255.0f*alpha;
    push.tint[1]=((argb>>8)&255)/255.0f*alpha;
    push.tint[2]=(argb&255)/255.0f*alpha; push.tint[3]=alpha;
    const VkViewport viewport={0,0,(float)b->width,(float)b->height,0,1};
    const VkRect2D clip={{(int32_t)d->clip_x,(int32_t)d->clip_y},{d->clip_w,d->clip_h}};
    e->bind_pipeline(b->command,VK_PIPELINE_BIND_POINT_GRAPHICS,e->pipeline[d->over]);
    e->bind_descriptors(b->command,VK_PIPELINE_BIND_POINT_GRAPHICS,e->layout,0,1,&b->source,0,NULL);
    e->viewport(b->command,0,1,&viewport); e->scissor(b->command,0,1,&clip);
    e->constants(b->command,e->layout,VK_SHADER_STAGE_VERTEX_BIT|VK_SHADER_STAGE_FRAGMENT_BIT,0,sizeof push,&push);
    e->draw(b->command,6,1,0,0);
    return 0;
}

uint32_t cubit_vulkan_record_affine(void *borrowed,const struct cubit_mesa_affine *d,
    const struct cubit_vulkan_coefficients *c,uint32_t width,uint32_t height,uint32_t mask,uint32_t argb)
{
    return record_affine(borrowed,d,c,width,height,mask,argb,NULL,NULL);
}
uint32_t cubit_vulkan_record_affine_region(void *borrowed,const struct cubit_mesa_affine *d,
    const struct cubit_vulkan_coefficients *c,uint32_t width,uint32_t height,uint32_t mask,uint32_t argb,
    const struct cubit_vulkan_source_region *region)
{
    if(!region)return 1;
    return record_affine(borrowed,d,c,width,height,mask,argb,region,NULL);
}
uint32_t cubit_vulkan_record_preview(void *borrowed,const struct cubit_mesa_affine *d,
    const struct cubit_vulkan_coefficients *c,uint32_t width,uint32_t height,
    const struct cubit_vulkan_preview *preview)
{
    if(!preview)return 1;
    return record_affine(borrowed,d,c,width,height,4,0,NULL,preview);
}
