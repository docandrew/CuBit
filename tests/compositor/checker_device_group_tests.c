/* Actual device-storage adapter, mock Vulkan resource factories. Each process
 * is a fresh admitted device lifetime; no quarantined owner is reset/reused. */
#include "vulkan_device_storage.h"
#include "vulkan_sources.h"
#include "vulkan_checker.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define H(t,n) ((t)(uintptr_t)(n))
static unsigned scenario,affine_live,checker_live,sources_live,record_calls,close_failure;
static PFN_vkVoidFunction VKAPI_CALL device_proc(VkDevice d,const char *n){(void)d;(void)n;return NULL;}
static void VKAPI_CALL properties(VkPhysicalDevice p,VkPhysicalDeviceMemoryProperties *m)
{(void)p;memset(m,0,sizeof(*m));m->memoryTypeCount=1;m->memoryTypes[0].propertyFlags=VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;}
static PFN_vkVoidFunction VKAPI_CALL instance_proc(VkInstance i,const char *n)
{(void)i;return !strcmp(n,"vkGetDeviceProcAddr")?(PFN_vkVoidFunction)device_proc:(PFN_vkVoidFunction)properties;}
VkResult cubit_vulkan_affine_create(struct cubit_vulkan_affine_engine *e,VkDevice d,PFN_vkGetDeviceProcAddr p,VkRenderPass r)
{(void)p;(void)r;if(scenario==1)return VK_ERROR_OUT_OF_DEVICE_MEMORY;assert(!affine_live);affine_live=1;e->device=d;return VK_SUCCESS;}
void cubit_vulkan_affine_destroy(struct cubit_vulkan_affine_engine *e)
{assert(affine_live&&!sources_live&&!checker_live);affine_live=0;memset(e,0,sizeof(*e));}
VkResult cubit_vulkan_checker_create(struct cubit_vulkan_checker *e,VkDevice d,PFN_vkGetDeviceProcAddr p,VkRenderPass r)
{(void)p;(void)r;assert(affine_live);if(scenario==2)return VK_ERROR_OUT_OF_DEVICE_MEMORY;checker_live=1;e->device=d;return VK_SUCCESS;}
void cubit_vulkan_checker_destroy(struct cubit_vulkan_checker *e)
{assert(checker_live&&affine_live&&!sources_live);checker_live=0;memset(e,0,sizeof(*e));}
VkResult cubit_vulkan_sources_init(struct cubit_vulkan_sources *s,const struct cubit_vulkan_affine_engine *e,VkCommandBuffer c,PFN_vkGetDeviceProcAddr p)
{(void)p;assert(affine_live&&checker_live);if(scenario==3)return VK_ERROR_OUT_OF_DEVICE_MEMORY;sources_live=1;s->engine=e;s->command=c;s->pool=H(VkDescriptorPool,77);return VK_SUCCESS;}
uint32_t cubit_vulkan_sources_destroy(struct cubit_vulkan_sources *s)
{assert(affine_live&&checker_live&&sources_live);if(close_failure)return 2;sources_live=0;memset(s,0,sizeof(*s));return 0;}
uint32_t cubit_vulkan_checker_record(const struct cubit_vulkan_checker *e,VkCommandBuffer c,const struct cubit_vulkan_checker_request *r)
{assert(affine_live&&checker_live&&sources_live&&e->device==H(VkDevice,3)&&c==H(VkCommandBuffer,6)&&r);++record_calls;return scenario==5?1:0;}
int main(int argc,char **argv)
{
    assert(argc==2);scenario=(unsigned)strtoul(argv[1],NULL,10);assert(scenario<=5);
    struct cubit_vulkan_checker_request q={.left=1,.top=2,.right=9,.bottom=10,.width=32,.height=24,.numerator=5,.denominator=4,.clip_w=32,.clip_h=24};
    assert(cubit_vulkan_device_checker_record(NULL,&q)==1);
    struct cubit_mesa_service_device d={H(VkInstance,1),H(VkPhysicalDevice,2),H(VkDevice,3),H(VkQueue,4),0,instance_proc};
    struct cubit_vulkan_context_request *c=cubit_vulkan_device_context_request(&d);assert(c);
    c->fresh->live=1;c->fresh->device=d.device;c->fresh->pass=H(VkRenderPass,5);
    c->fresh->submission.command=H(VkCommandBuffer,6);c->fresh->submission.device=d.device;
    struct cubit_vulkan_device_targets targets;assert(!cubit_vulkan_device_targets_prepare(32,24,&targets));
    assert(cubit_vulkan_device_checker_record(&c->fresh->submission,&q)==1&&record_calls==0);
    unsigned result=cubit_vulkan_device_pipeline_create();
    assert(cubit_vulkan_device_pipeline_create()==2);
    if(scenario>=1&&scenario<=3){
        assert(result==1&&!affine_live&&!checker_live&&!sources_live);
        assert(cubit_vulkan_device_checker_record(&c->fresh->submission,&q)==1&&record_calls==0);
        assert(cubit_vulkan_device_pipeline_close()==2);
        printf("PASS checker group clean creation failure %u\n",scenario);return 0;
    }
    assert(result==0&&affine_live&&checker_live&&sources_live);
    for(unsigned fault=0;fault<7;fault++){
        struct cubit_vulkan_checker_request r=q;const struct cubit_vulkan_checker_request *rp=&r;
        void *borrowed=&c->fresh->submission;
        switch(fault){
        case 0:borrowed=NULL;break;case 1:borrowed=c;break;case 2:rp=NULL;break;
        case 3:r.width++;break;case 4:r.height++;break;
        case 5:c->fresh->live=0;break;case 6:c->fresh->device=H(VkDevice,100);break;
        }
        assert(cubit_vulkan_device_checker_record(borrowed,rp)==1&&record_calls==0);
        c->fresh->live=1;c->fresh->device=d.device;
    }
    assert(cubit_vulkan_device_checker_record(&c->fresh->submission,&q)==(scenario==5?1:0)&&record_calls==1);
    if(scenario==4){
        close_failure=1;assert(cubit_vulkan_device_pipeline_close()==2);
        assert(affine_live&&checker_live&&sources_live);
        /* Test fixture supplies fresh confirmed provider cleanup. Production
         * SPARK quarantine does not automatically retry this uncertain close. */
        close_failure=0;
    }
    assert(cubit_vulkan_device_pipeline_close()==0&&!affine_live&&!checker_live&&!sources_live);
    assert(cubit_vulkan_device_checker_record(&c->fresh->submission,&q)==1&&record_calls==1);
    assert(cubit_vulkan_device_pipeline_close()==2);
    printf("PASS checker group %u: borrowed context/extent guards, draw result, retained failed close and ordered cleanup\n",scenario);
}
