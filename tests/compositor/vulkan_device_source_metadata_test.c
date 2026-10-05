#include "vulkan_device_storage.h"
#include "vulkan_sources.h"
#include "vulkan_checker.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
#define H(t,n) ((t)(uintptr_t)(n))
static PFN_vkVoidFunction VKAPI_CALL device_proc(VkDevice d,const char *n){(void)d;(void)n;return NULL;}
static void VKAPI_CALL properties(VkPhysicalDevice p,VkPhysicalDeviceMemoryProperties *m)
{(void)p;memset(m,0,sizeof(*m));m->memoryTypeCount=1;m->memoryTypes[0].propertyFlags=VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;}
static PFN_vkVoidFunction VKAPI_CALL instance_proc(VkInstance i,const char *n)
{(void)i;return !strcmp(n,"vkGetDeviceProcAddr")?(PFN_vkVoidFunction)device_proc:(PFN_vkVoidFunction)properties;}
VkResult cubit_vulkan_affine_create(struct cubit_vulkan_affine_engine *e,VkDevice d,PFN_vkGetDeviceProcAddr p,VkRenderPass r)
{(void)p;(void)r; e->device=d;return VK_SUCCESS;}
void cubit_vulkan_affine_destroy(struct cubit_vulkan_affine_engine *e){memset(e,0,sizeof(*e));}
VkResult cubit_vulkan_sources_init(struct cubit_vulkan_sources *s,const struct cubit_vulkan_affine_engine *e,VkCommandBuffer c,PFN_vkGetDeviceProcAddr p)
{(void)p;s->engine=e;s->command=c;s->pool=H(VkDescriptorPool,77);return VK_SUCCESS;}
uint32_t cubit_vulkan_sources_destroy(struct cubit_vulkan_sources *s){memset(s,0,sizeof(*s));return 0;}
VkResult cubit_vulkan_checker_create(struct cubit_vulkan_checker *e,VkDevice d,PFN_vkGetDeviceProcAddr p,VkRenderPass r)
{(void)p;(void)r;e->device=d;return VK_SUCCESS;}
void cubit_vulkan_checker_destroy(struct cubit_vulkan_checker *e){memset(e,0,sizeof(*e));}
uint32_t cubit_vulkan_checker_record(const struct cubit_vulkan_checker *e,VkCommandBuffer c,const struct cubit_vulkan_checker_request *r)
{(void)e;(void)c;(void)r;return 1;}
int main(void)
{
    assert(!cubit_vulkan_device_upload_prepare());
    assert(!cubit_vulkan_device_source_request(0,NULL));
    struct cubit_mesa_service_device d={H(VkInstance,1),H(VkPhysicalDevice,2),H(VkDevice,3),H(VkQueue,4),0,instance_proc};
    struct cubit_vulkan_context_request *c=cubit_vulkan_device_context_request(&d);assert(c);
    c->fresh->live=1;c->fresh->device=d.device;c->fresh->pass=H(VkRenderPass,5);c->fresh->submission.command=H(VkCommandBuffer,6);
    struct cubit_vulkan_device_targets t;
    assert(!cubit_vulkan_device_targets_prepare(32,24,&t));
    struct cubit_vulkan_target_request *r=t.description;
    for(unsigned n=0;n<3;n++){
        struct cubit_vulkan_owned_image *a=t.images[n];a->stage=2;a->image=H(VkImage,10+n);a->memory=H(VkDeviceMemory,20+n);
        r->fresh->views[n]=H(VkImageView,30+n);r->fresh->framebuffers[n]=H(VkFramebuffer,40+n);
    }
    struct cubit_vulkan_owned_image source={.physical=d.physical,.device=d.device,.instance=d.instance,
        .instance_proc=instance_proc,.proc=device_proc,.width=64,.height=48,.format=VK_FORMAT_B8G8R8A8_UNORM,
        .usage=VK_IMAGE_USAGE_SAMPLED_BIT|VK_IMAGE_USAGE_TRANSFER_DST_BIT,.image=H(VkImage,99),.memory=H(VkDeviceMemory,100),.stage=2};
    assert(!cubit_vulkan_device_source_request(0,&source));assert(!cubit_vulkan_device_pipeline_create());
    struct cubit_vulkan_upload_buffer *upload=cubit_vulkan_device_upload_prepare();
    assert(upload&&upload->instance==d.instance&&upload->physical==d.physical&&
           upload->device==d.device&&upload->instance_proc==instance_proc&&upload->proc==device_proc);
    for(unsigned fault=0;fault<9;fault++){
        struct cubit_vulkan_upload_buffer saved=*upload;
        struct cubit_vulkan_owned_image *target=t.images[0];
        switch(fault){
        case 0:upload->stage=1;break;case 1:upload->stage=2;break;case 2:upload->stage=4;break;
        case 3:upload->buffer=H(VkBuffer,101);break;case 4:upload->memory=H(VkDeviceMemory,102);break;
        case 5:upload->mapped=(void *)103;break;case 6:c->fresh->live=0;break;
        case 7:c->fresh->device=H(VkDevice,104);break;default:target->stage=1;break;
        }
        struct cubit_vulkan_upload_buffer before=*upload;
        assert(!cubit_vulkan_device_upload_prepare());assert(!memcmp(&before,upload,sizeof(before)));
        *upload=saved;c->fresh->live=1;c->fresh->device=d.device;target->stage=2;
    }
    upload->stage=3;assert(cubit_vulkan_device_upload_prepare()==upload&&upload->stage==0);
    struct cubit_vulkan_device_source backing;
    assert(!cubit_vulkan_device_source_prepare(0,64,48,0,&backing));
    struct cubit_vulkan_owned_image *owned=backing.image;
    assert(owned&&backing.allowed_types==1&&owned->device==d.device&&owned->physical==d.physical);
    assert(owned->width==64&&owned->height==48&&owned->format==VK_FORMAT_B8G8R8A8_UNORM);
    for(unsigned fault=0;fault<12;fault++){
        struct cubit_vulkan_owned_image saved=*owned;
        uint32_t slot=0,width=64,height=48,mask=0;
        switch(fault){
        case 0:slot=CUBIT_VULKAN_OWNED_SOURCE_CAPACITY;break;case 1:width=0;break;case 2:width=65536;break;
        case 3:height=0;break;case 4:height=65536;break;case 5:mask=2;break;
        case 6:owned->stage=1;break;case 7:owned->stage=2;break;case 8:owned->stage=4;break;
        case 9:owned->image=H(VkImage,101);break;case 10:owned->memory=H(VkDeviceMemory,102);break;
        default:c->fresh->live=0;break;
        }
        struct cubit_vulkan_owned_image before=*owned;
        backing=(struct cubit_vulkan_device_source){(void *)1,99};
        assert(cubit_vulkan_device_source_prepare(slot,width,height,mask,&backing)==1);
        assert(!backing.image&&!backing.allowed_types&&!memcmp(&before,owned,sizeof(before)));
        *owned=saved;c->fresh->live=1;
    }
    owned->stage=3; /* Confirmed retired storage, not an arbitrary reset. */
    assert(!cubit_vulkan_device_source_prepare(0,127,33,1,&backing));
    assert(backing.image==owned&&owned->stage==0&&owned->format==VK_FORMAT_R8_UNORM&&owned->width==127&&owned->height==33);
    for(unsigned slot=1;slot<CUBIT_VULKAN_OWNED_SOURCE_CAPACITY;slot++){
        assert(!cubit_vulkan_device_source_prepare(slot,32,24,0,&backing));
        assert(backing.image!=owned);
    }
    for(unsigned fault=0;fault<17;fault++){
        struct cubit_vulkan_owned_image s=source;uint32_t slot=CUBIT_VULKAN_OWNED_SOURCE_CAPACITY-1;void *input=&s;
        switch(fault){
        case 1:s.stage=1;break;case 2:s.image=VK_NULL_HANDLE;break;case 3:s.memory=VK_NULL_HANDLE;break;
        case 4:s.device=H(VkDevice,66);break;case 5:s.physical=H(VkPhysicalDevice,66);break;
        case 6:s.instance=H(VkInstance,66);break;case 7:s.proc=NULL;break;case 8:s.instance_proc=NULL;break;
        case 9:s.width=0;break;case 10:s.height=65536;break;case 11:s.format=VK_FORMAT_R32_SFLOAT;break;
        case 12:s.usage=VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;break;case 13:s.image=H(VkImage,10);break;
        case 14:s.memory=H(VkDeviceMemory,21);break;case 15:slot=CUBIT_VULKAN_OWNED_SOURCE_CAPACITY;break;case 16:input=NULL;break;
        }
        struct cubit_vulkan_source_request *request=cubit_vulkan_device_source_request(slot,input);
        assert((request!=NULL)==(fault==0));
        if(request)assert(request->slot==CUBIT_VULKAN_OWNED_SOURCE_CAPACITY-1&&request->image==s.image&&request->format==s.format&&request->output_width==32&&request->output_height==24);
    }
    source.format=VK_FORMAT_R8_UNORM;
    struct cubit_vulkan_source_request *request=cubit_vulkan_device_source_request(0,&source);assert(request&&request->format==VK_FORMAT_R8_UNORM);
    struct cubit_vulkan_source_request saved=*request;
    request->provider->entries[0].view=H(VkImageView,101);
    struct cubit_vulkan_owned_image occupied=*owned;
    assert(cubit_vulkan_device_source_prepare(0,32,24,0,&backing)==1);
    assert(!backing.image&&!backing.allowed_types&&!memcmp(&occupied,owned,sizeof(occupied)));
    source.image=H(VkImage,102);assert(!cubit_vulkan_device_source_request(0,&source));
    assert(!memcmp(&saved,request,sizeof(saved)));
    request->provider->entries[0].view=VK_NULL_HANDLE;
    assert(!cubit_vulkan_device_pipeline_close());assert(!cubit_vulkan_device_source_request(0,&source));
    assert(cubit_vulkan_device_source_prepare(0,32,24,0,&backing)==1);
    assert(!backing.image&&!backing.allowed_types);
    assert(!cubit_vulkan_device_upload_prepare());
    puts("PASS upload metadata: matching admitted device, nine rejection/preservation cases, closed reuse, retired provider");
    puts("PASS owned-source metadata: 140 bounded backing slots, reuse/format/extent/live/quarantine guards, normalized rejection,  device/role/extent/alias/bounds, BGRA/R8, occupied-request preservation, retired provider");
}
