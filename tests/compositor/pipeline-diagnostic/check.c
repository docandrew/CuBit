#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define CUBIT_PIPELINE_DIAGNOSTIC_STORAGE
#include "vulkan_pipeline_diagnostic.h"
#include "vulkan_affine.h"
#include "vulkan_checker.h"
#include "vulkan_sources.h"
static const char *missing;
static int32_t injected=-7; static unsigned calls, fail, destroyed; static uintptr_t handle=100;
static VkResult status(void){return ++calls==fail?(VkResult)injected:VK_SUCCESS;}
#define CREATE(Name,Info,Handle) static VkResult f##Name(VkDevice d,const Info*i,const VkAllocationCallbacks*a,Handle*out){(void)d;(void)i;(void)a;VkResult r=status();if(r==VK_SUCCESS)*out=(Handle)++handle;return r;}
#define DESTROY(Name,Handle) static void f##Name(VkDevice d,Handle h,const VkAllocationCallbacks*a){(void)d;(void)a;assert(h);destroyed++;}
CREATE(CreateDescriptorSetLayout,VkDescriptorSetLayoutCreateInfo,VkDescriptorSetLayout)
CREATE(CreatePipelineLayout,VkPipelineLayoutCreateInfo,VkPipelineLayout)
CREATE(CreateSampler,VkSamplerCreateInfo,VkSampler)
CREATE(CreateShaderModule,VkShaderModuleCreateInfo,VkShaderModule)
CREATE(CreateDescriptorPool,VkDescriptorPoolCreateInfo,VkDescriptorPool)
DESTROY(DestroyDescriptorSetLayout,VkDescriptorSetLayout)
DESTROY(DestroyPipelineLayout,VkPipelineLayout)
DESTROY(DestroySampler,VkSampler)
DESTROY(DestroyShaderModule,VkShaderModule)
DESTROY(DestroyPipeline,VkPipeline)
DESTROY(DestroyDescriptorPool,VkDescriptorPool)
static VkResult fCreateGraphicsPipelines(VkDevice d,VkPipelineCache c,uint32_t n,const VkGraphicsPipelineCreateInfo*i,const VkAllocationCallbacks*a,VkPipeline*out){(void)d;(void)c;(void)i;(void)a;assert(n==1);VkResult r=status();if(!r)*out=(VkPipeline)++handle;return r;}
static VkResult fAllocateDescriptorSets(VkDevice d,const VkDescriptorSetAllocateInfo*i,VkDescriptorSet*out){(void)d;VkResult r=status();if(!r)for(unsigned n=0;n<i->descriptorSetCount;n++)out[n]=(VkDescriptorSet)++handle;return r;}
static void unused(void){}
static PFN_vkVoidFunction proc(VkDevice d,const char*n){(void)d;if(missing&&!strcmp(n,missing))return NULL;
#define GET(Name) if(!strcmp(n,"vk"#Name))return (PFN_vkVoidFunction)f##Name;
GET(CreateDescriptorSetLayout) GET(CreatePipelineLayout) GET(CreateSampler) GET(CreateShaderModule) GET(CreateDescriptorPool) GET(CreateGraphicsPipelines) GET(AllocateDescriptorSets)
GET(DestroyDescriptorSetLayout) GET(DestroyPipelineLayout) GET(DestroySampler) GET(DestroyShaderModule) GET(DestroyPipeline) GET(DestroyDescriptorPool)
#undef GET
return unused;}
int main(int argc,char**argv){assert(argc==2||argc==3||argc==5);fail=(unsigned)atoi(argv[1]);if(argc==3)injected=(int32_t)strtol(argv[2],NULL,10);
uint32_t stage=99,index=99;int32_t result=99;
assert(!cubit_vulkan_pipeline_last_failure(&stage,&index,&result));assert(!stage&&!index&&!result);
pipeline_failure(999,0,0);assert(!pipeline_diagnostic.valid);
struct cubit_vulkan_affine_engine a={0};struct cubit_vulkan_checker c={0};struct cubit_vulkan_sources s={0};
VkDevice device=(VkDevice)1;VkRenderPass pass=(VkRenderPass)2;
if(fail==15){assert(cubit_vulkan_affine_create(NULL,device,proc,pass)==VK_ERROR_INITIALIZATION_FAILED);assert(cubit_vulkan_pipeline_last_failure(&stage,&index,&result));assert(stage==100&&index==0&&result==VK_ERROR_INITIALIZATION_FAILED);puts("PASS missing engine guard");return 0;}

if(argc==5){
 unsigned target=(unsigned)atoi(argv[3]), expected=(unsigned)atoi(argv[4]);
 // Resolve earlier stages successfully before removing a proc from the target.
 if(target>=210)assert(cubit_vulkan_affine_create(&a,device,proc,pass)==VK_SUCCESS);
 if(target>=310)assert(cubit_vulkan_checker_create(&c,device,proc,pass)==VK_SUCCESS);
 unsigned before=calls;missing=argv[2];
 VkResult r=target==110?cubit_vulkan_affine_create(&a,device,proc,pass):target==210?cubit_vulkan_checker_create(&c,device,proc,pass):cubit_vulkan_sources_init(&s,&a,(VkCommandBuffer)3,proc);
 assert(r==VK_ERROR_INITIALIZATION_FAILED&&calls==before);
 assert(cubit_vulkan_pipeline_last_failure(&stage,&index,&result));
 assert(stage==target&&index==expected&&result==VK_ERROR_INITIALIZATION_FAILED);
 pipeline_failure(999,77,-4);assert(pipeline_diagnostic.stage==target&&pipeline_diagnostic.index==expected);
 cubit_vulkan_checker_destroy(&c);cubit_vulkan_affine_destroy(&a);
 printf("PASS missing %s stage=%u index=%u\n",missing,stage,index);return 0;
}
VkResult r=cubit_vulkan_affine_create(&a,device,proc,pass);
if(!r)r=cubit_vulkan_checker_create(&c,device,proc,pass);
if(!r)r=cubit_vulkan_sources_init(&s,&a,(VkCommandBuffer)3,proc);
if(fail){static const unsigned stages[]={0,101,102,103,104,105,106,106,106,201,202,203,204,301,302};assert(fail<=14);assert(r==injected);assert(calls==fail);assert(cubit_vulkan_pipeline_last_failure(&stage,&index,&result));assert(stage==stages[fail]&&result==injected);assert(index==(fail>=6&&fail<=8?fail-6:0));
// Cleanup/wrapper evidence must not overwrite the first failure.
pipeline_failure(999,77,-4);assert(cubit_vulkan_pipeline_last_failure(&stage,&index,&result));assert(stage==stages[fail]&&result==injected);
}else{assert(r==VK_SUCCESS&&calls==14);assert(!cubit_vulkan_pipeline_last_failure(&stage,&index,&result));assert(cubit_vulkan_sources_destroy(&s)==0);}
cubit_vulkan_checker_destroy(&c);cubit_vulkan_affine_destroy(&a);
assert(!cubit_vulkan_pipeline_last_failure(NULL,&index,&result));
printf("PASS fail=%u calls=%u destroyed=%u\n",fail,calls,destroyed);return 0;}
