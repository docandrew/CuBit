#include "vulkan_affine.h"
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
    const struct cubit_vulkan_affine_engine engine={.device=(VkDevice)(uintptr_t)1,.layout=(VkPipelineLayout)(uintptr_t)1,
      .pipeline={(VkPipeline)(uintptr_t)1,(VkPipeline)(uintptr_t)2,(VkPipeline)(uintptr_t)3},
      .bind_pipeline=bind_pipeline,.bind_descriptors=bind_descriptors,.viewport=viewport,.scissor=scissor,.constants=constants,.draw=draw};
    const struct cubit_vulkan_affine_draw borrowed={&engine,(VkCommandBuffer)(uintptr_t)1,(VkDescriptorSet)(uintptr_t)1,32,24};
    const struct cubit_mesa_affine geometry={.clip_w=32,.clip_h=24,.over=2};
    const struct cubit_vulkan_coefficients coefficients={.ud=7,.vd=9};
    const struct cubit_vulkan_source_region valid={2,3,7,9,32,24};
    for(unsigned fault=0;fault<11;fault++){
        struct cubit_vulkan_source_region altered=valid;
        const struct cubit_vulkan_source_region *region=&altered;
        switch(fault){
          case 0:region=NULL;break;case 1:altered.image_width=0;break;
          case 2:altered.image_width=65536;break;case 3:altered.image_height=0;break;
          case 4:altered.image_height=65536;break;case 5:altered.width=0;break;
          case 6:altered.height=0;break;case 7:altered.x=32;break;
          case 8:altered.y=24;break;case 9:altered.width=UINT32_MAX;break;
          case 10:altered.height=UINT32_MAX;break;
        }
        commands=0;
        assert(cubit_vulkan_record_affine_region((void *)&borrowed,&geometry,&coefficients,32,24,0,0,region)==1);
        assert(commands==0);
    }
    assert(cubit_vulkan_record_affine_region((void *)&borrowed,&geometry,&coefficients,32,24,0,0,&valid)==0);
    assert(commands==6&&payload[20]==2&&payload[21]==3&&payload[22]==7&&payload[23]==9);
    commands=0;
    assert(cubit_vulkan_record_affine((void *)&borrowed,&geometry,&coefficients,32,24,0,0)==0);
    assert(commands==6);
    for(unsigned i=17;i<24;i++)assert(payload[i]==0);
    puts("PASS affine region boundary: 11 malformed windows record no commands; exact 96-byte push, valid subregion and whole-image controls");
    return 0;
}
