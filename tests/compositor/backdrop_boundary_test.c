#include "vulkan_backdrop.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static unsigned commands;
static uint32_t payload[24];
static VKAPI_ATTR void VKAPI_CALL bind_pipeline(VkCommandBuffer c,VkPipelineBindPoint b,VkPipeline p)
{(void)c;(void)b;(void)p;commands++;}
static VKAPI_ATTR void VKAPI_CALL bind_descriptors(VkCommandBuffer c,VkPipelineBindPoint b,VkPipelineLayout l,
    uint32_t first,uint32_t n,const VkDescriptorSet *sets,uint32_t count,const uint32_t *offsets)
{(void)c;(void)b;(void)l;(void)first;(void)n;(void)sets;(void)count;(void)offsets;commands++;}
static VKAPI_ATTR void VKAPI_CALL viewport(VkCommandBuffer c,uint32_t first,uint32_t n,const VkViewport *v)
{(void)c;(void)first;(void)n;(void)v;commands++;}
static VKAPI_ATTR void VKAPI_CALL scissor(VkCommandBuffer c,uint32_t first,uint32_t n,const VkRect2D *v)
{(void)c;(void)first;(void)n;(void)v;commands++;}
static VKAPI_ATTR void VKAPI_CALL constants(VkCommandBuffer c,VkPipelineLayout l,VkShaderStageFlags s,
    uint32_t offset,uint32_t bytes,const void *data)
{(void)c;(void)l;(void)s;assert(offset==0&&bytes==sizeof payload);memcpy(payload,data,bytes);commands++;}
static VKAPI_ATTR void VKAPI_CALL draw(VkCommandBuffer c,uint32_t vertices,uint32_t instances,uint32_t first,uint32_t base)
{(void)c;assert(vertices==6&&instances==1&&first==0&&base==0);commands++;}
int main(void)
{
    struct cubit_vulkan_affine_engine engine={.device=(VkDevice)(uintptr_t)1,.layout=(VkPipelineLayout)(uintptr_t)1,
        .pipeline={(VkPipeline)(uintptr_t)1,VK_NULL_HANDLE},.bind_pipeline=bind_pipeline,.bind_descriptors=bind_descriptors,
        .viewport=viewport,.scissor=scissor,.constants=constants,.draw=draw};
    const struct cubit_vulkan_backdrop valid={-9,-12,50,60,0,0,32,24,65,3};
    for(unsigned fault=0;fault<32;fault++){
        struct cubit_vulkan_affine_engine altered=engine;
        struct cubit_vulkan_affine_draw borrowed={&altered,(VkCommandBuffer)(uintptr_t)1,(VkDescriptorSet)(uintptr_t)1,32,24};
        struct cubit_vulkan_backdrop description=valid;
        void *context=&borrowed;const struct cubit_vulkan_backdrop *d=&description;
        uint32_t w=32,h=24;
        switch(fault){
            case 0:context=NULL;break;case 1:d=NULL;break;case 2:borrowed.engine=NULL;break;
            case 3:borrowed.command=VK_NULL_HANDLE;break;case 4:borrowed.source=VK_NULL_HANDLE;break;
            case 5:altered.device=VK_NULL_HANDLE;break;case 6:altered.layout=VK_NULL_HANDLE;break;
            case 7:altered.pipeline[0]=VK_NULL_HANDLE;break;case 8:altered.bind_pipeline=NULL;break;
            case 9:altered.bind_descriptors=NULL;break;case 10:altered.viewport=NULL;break;
            case 11:altered.scissor=NULL;break;case 12:altered.constants=NULL;break;case 13:altered.draw=NULL;break;
            case 14:w=0;break;case 15:h=0;break;case 16:w=65536;break;case 17:h=65536;break;
            case 18:borrowed.width++;break;case 19:borrowed.height++;break;
            case 20:description.width=0;break;case 21:description.height=UINT64_MAX;break;
            case 22:description.left=INT64_MIN;break;case 23:description.top=INT64_MAX;break;
            case 24:description.source_w=0;break;case 25:description.source_h=65536;break;
            case 26:description.clip_x=32;break;case 27:description.clip_y=24;break;
            case 28:description.clip_w=0;break;case 29:description.clip_h=0;break;
            case 30:description.clip_w=UINT32_MAX;break;case 31:description.clip_h=25;break;
        }
        commands=0;
        assert(cubit_vulkan_record_backdrop(context,d,w,h)==1);
        assert(commands==0);
    }
    struct cubit_vulkan_affine_draw borrowed={&engine,(VkCommandBuffer)(uintptr_t)1,(VkDescriptorSet)(uintptr_t)1,32,24};
    assert(cubit_vulkan_record_backdrop(&borrowed,&valid,32,24)==0);
    assert(commands==6&&payload[16]==2&&payload[0]==(uint32_t)-9&&payload[1]==UINT32_MAX);
    for(unsigned i=17;i<24;i++)assert(payload[i]==0);
    assert(payload[2]==(uint32_t)-12&&payload[3]==UINT32_MAX&&payload[4]==50&&payload[6]==60);
    assert(payload[8]==65&&payload[9]==3);
    puts("BACKDROP BOUNDARY: PASS32 rejected descriptors/dispatch states without any commands; valid68byte push/6commands");
    return 0;
}
