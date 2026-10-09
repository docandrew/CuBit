#include "vulkan_pipeline_diagnostic.h"
#include "vulkan_checker.h"
#include "vulkan-checker-shaders.h"
#include <stddef.h>
#include <string.h>
struct checker_push {
    int32_t area[4],origin[2]; uint32_t scale[2],output[4]; float tint[4];
};
_Static_assert(sizeof(struct checker_push)==64,"checker push size");
_Static_assert(offsetof(struct checker_push,tint)==48,"checker tint offset");
_Static_assert(sizeof(struct cubit_vulkan_checker_request)==64,"checker request ABI");
void cubit_vulkan_checker_destroy(struct cubit_vulkan_checker *e)
{
    if(!e)return;
    if(e->pipeline)e->destroy_pipeline(e->device,e->pipeline,NULL);
    if(e->layout)e->destroy_layout(e->device,e->layout,NULL);
    memset(e,0,sizeof *e);
}
VkResult cubit_vulkan_checker_create(struct cubit_vulkan_checker *e,
    VkDevice device,PFN_vkGetDeviceProcAddr proc,VkRenderPass pass)
{
    if(!e){pipeline_failure(200,0,VK_ERROR_INITIALIZATION_FAILED);return VK_ERROR_INITIALIZATION_FAILED;}
    *e=(struct cubit_vulkan_checker){.device=device};
    if(!device||!proc||!pass){pipeline_failure(200,0,VK_ERROR_INITIALIZATION_FAILED);return VK_ERROR_INITIALIZATION_FAILED;}
#define LOAD(index,field,name) e->field=(PFN_vk##name)proc(device,"vk" #name); if(!e->field){pipeline_failure(210,index,VK_ERROR_INITIALIZATION_FAILED);return VK_ERROR_INITIALIZATION_FAILED;}
    LOAD(1,destroy_layout,DestroyPipelineLayout); LOAD(2,destroy_pipeline,DestroyPipeline);
    LOAD(3,bind,CmdBindPipeline); LOAD(4,viewport,CmdSetViewport); LOAD(5,scissor,CmdSetScissor);
    LOAD(6,constants,CmdPushConstants); LOAD(7,draw,CmdDraw);
#undef LOAD
#define LOAD(index,name) PFN_vk##name name=(PFN_vk##name)proc(device,"vk" #name); if(!name){pipeline_failure(210,index,VK_ERROR_INITIALIZATION_FAILED);return VK_ERROR_INITIALIZATION_FAILED;}
    LOAD(8,CreatePipelineLayout); LOAD(9,CreateShaderModule); LOAD(10,DestroyShaderModule); LOAD(11,CreateGraphicsPipelines);
#undef LOAD
    VkResult result; VkShaderModule vertex=VK_NULL_HANDLE,fragment=VK_NULL_HANDLE;
#define TRY(stage,index,expr) do { result=(expr); if(result!=VK_SUCCESS){pipeline_failure(stage,index,result);goto done;} } while(0)
    const VkPushConstantRange range={VK_SHADER_STAGE_FRAGMENT_BIT,0,sizeof(struct checker_push)};
    const VkPipelineLayoutCreateInfo layout={.sType=VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        .pushConstantRangeCount=1,.pPushConstantRanges=&range};
    TRY(201,0,CreatePipelineLayout(device,&layout,NULL,&e->layout));
    const VkShaderModuleCreateInfo vi={.sType=VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        .codeSize=sizeof(vulkan_checker_vertex),.pCode=vulkan_checker_vertex};
    const VkShaderModuleCreateInfo fi={.sType=VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        .codeSize=sizeof(vulkan_checker_fragment),.pCode=vulkan_checker_fragment};
    TRY(202,0,CreateShaderModule(device,&vi,NULL,&vertex));
    TRY(203,0,CreateShaderModule(device,&fi,NULL,&fragment));
    const VkPipelineShaderStageCreateInfo stages[2]={
        {.sType=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,.stage=VK_SHADER_STAGE_VERTEX_BIT,.module=vertex,.pName="main"},
        {.sType=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,.stage=VK_SHADER_STAGE_FRAGMENT_BIT,.module=fragment,.pName="main"}};
    const VkPipelineVertexInputStateCreateInfo input={.sType=VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO};
    const VkPipelineInputAssemblyStateCreateInfo assembly={.sType=VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,.topology=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST};
    const VkPipelineViewportStateCreateInfo view={.sType=VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,.viewportCount=1,.scissorCount=1};
    const VkPipelineRasterizationStateCreateInfo raster={.sType=VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,.polygonMode=VK_POLYGON_MODE_FILL,.cullMode=VK_CULL_MODE_NONE,.frontFace=VK_FRONT_FACE_COUNTER_CLOCKWISE,.lineWidth=1};
    const VkPipelineMultisampleStateCreateInfo samples={.sType=VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,.rasterizationSamples=VK_SAMPLE_COUNT_1_BIT};
    const VkDynamicState states[]={VK_DYNAMIC_STATE_VIEWPORT,VK_DYNAMIC_STATE_SCISSOR};
    const VkPipelineDynamicStateCreateInfo dynamic={.sType=VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO,.dynamicStateCount=2,.pDynamicStates=states};
    const VkPipelineColorBlendAttachmentState attachment={.colorWriteMask=VK_COLOR_COMPONENT_R_BIT|VK_COLOR_COMPONENT_G_BIT|VK_COLOR_COMPONENT_B_BIT|VK_COLOR_COMPONENT_A_BIT};
    const VkPipelineColorBlendStateCreateInfo blend={.sType=VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,.attachmentCount=1,.pAttachments=&attachment};
    const VkGraphicsPipelineCreateInfo graphics={.sType=VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,.stageCount=2,.pStages=stages,
        .pVertexInputState=&input,.pInputAssemblyState=&assembly,.pViewportState=&view,.pRasterizationState=&raster,.pMultisampleState=&samples,
        .pColorBlendState=&blend,.pDynamicState=&dynamic,.layout=e->layout,.renderPass=pass,.subpass=0};
    TRY(204,0,CreateGraphicsPipelines(device,VK_NULL_HANDLE,1,&graphics,NULL,&e->pipeline));
done:
    if(fragment)DestroyShaderModule(device,fragment,NULL);
    if(vertex)DestroyShaderModule(device,vertex,NULL);
    if(result!=VK_SUCCESS)cubit_vulkan_checker_destroy(e);
    return result;
#undef TRY
}
uint32_t cubit_vulkan_checker_record(const struct cubit_vulkan_checker *e,
    VkCommandBuffer command,const struct cubit_vulkan_checker_request *r)
{
    if(!e||!r||!command||!e->layout||!e->pipeline||!e->bind||!e->viewport||
       !e->scissor||!e->constants||!e->draw)return 1;
    if(!r->width||r->width>65535||!r->height||r->height>65535||r->rotation>3||
       !r->numerator||r->numerator>16||!r->denominator||r->denominator>16||
       r->left < -1073741824||r->right > 1073741824||r->top < -1073741824||r->bottom > 1073741824||
       r->left>=r->right||r->top>=r->bottom||
       r->origin_x < -16777216||r->origin_x > 16777216||r->origin_y < -16777216||r->origin_y > 16777216||
       r->clip_x>=r->width||r->clip_y>=r->height||!r->clip_w||!r->clip_h||
       r->clip_w>r->width-r->clip_x||r->clip_h>r->height-r->clip_y)return 1;
    const struct checker_push push={
        .area={r->left,r->top,r->right,r->bottom},.origin={r->origin_x,r->origin_y},
        .scale={r->numerator,r->denominator},.output={r->width,r->height,r->rotation,0},
        .tint={((r->rgb>>16)&255)/255.0f,((r->rgb>>8)&255)/255.0f,(r->rgb&255)/255.0f,1.0f}};
    const VkViewport viewport={0,0,(float)r->width,(float)r->height,0,1};
    const VkRect2D clip={{(int32_t)r->clip_x,(int32_t)r->clip_y},{r->clip_w,r->clip_h}};
    e->bind(command,VK_PIPELINE_BIND_POINT_GRAPHICS,e->pipeline);
    e->viewport(command,0,1,&viewport);e->scissor(command,0,1,&clip);
    e->constants(command,e->layout,VK_SHADER_STAGE_FRAGMENT_BIT,0,sizeof push,&push);
    e->draw(command,6,1,0,0);
    return 0;
}
