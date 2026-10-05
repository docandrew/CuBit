/* HOST ONLY. Real Desktop/Vulkan; service admission borrows the harness device.
 * Wrappers observe private metadata/selected framebuffer, never alter commands. */
#include "vulkan_device_storage.h"
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#define CHECK(x) do { if(!(x)){fprintf(stderr,"Desktop oracle line %d: %s\n",__LINE__,#x);return 1;} } while(0)
#define VK(x) CHECK((x)==VK_SUCCESS)
extern int desktop_gpu_scene_real_open(void),desktop_gpu_scene_real_submit(void),desktop_gpu_scene_real_poll_upload(void);
extern int desktop_gpu_scene_real_import(void),desktop_gpu_scene_real_render(void),desktop_gpu_scene_real_poll_frame(void);
extern int desktop_gpu_scene_real_restart(void),desktop_gpu_scene_real_close(void),desktop_gpu_scene_real_reconfigure(int);
extern void *desktop_gpu_scene_real_begin(uint32_t *,uint32_t *);
static struct cubit_mesa_service_device borrowed;
struct cubit_mesa_service { unsigned host; };
static struct cubit_mesa_service service;
static struct cubit_vulkan_context *context;
static struct cubit_vulkan_device_targets targets;
static VkFramebuffer selected;
static unsigned closes,source_creates,source_destroys;
static VkImage sampled_images[CUBIT_VULKAN_OWNED_SOURCE_CAPACITY];
static unsigned source_live;
static VkResult VKAPI_CALL create_image(VkDevice d,const VkImageCreateInfo *info,const VkAllocationCallbacks *a,VkImage *out)
{
    VkResult result=vkCreateImage(d,info,a,out);
    if(result==VK_SUCCESS&&(info->usage&VK_IMAGE_USAGE_SAMPLED_BIT)){
        unsigned slot=0;while(slot<CUBIT_VULKAN_OWNED_SOURCE_CAPACITY&&sampled_images[slot])++slot;assert(slot<CUBIT_VULKAN_OWNED_SOURCE_CAPACITY);
        sampled_images[slot]=*out;++source_creates;++source_live;
    }
    return result;
}
static void VKAPI_CALL destroy_image(VkDevice d,VkImage image,const VkAllocationCallbacks *a)
{
    for(unsigned slot=0;slot<CUBIT_VULKAN_OWNED_SOURCE_CAPACITY;slot++)if(sampled_images[slot]==image){
        sampled_images[slot]=VK_NULL_HANDLE;++source_destroys;--source_live;break;
    }
    vkDestroyImage(d,image,a);
}

static void VKAPI_CALL begin_pass(VkCommandBuffer command,const VkRenderPassBeginInfo *info,VkSubpassContents contents)
{selected=info->framebuffer;vkCmdBeginRenderPass(command,info,contents);}
static PFN_vkVoidFunction VKAPI_CALL device_proc(VkDevice device,const char *name)
{
    if(!strcmp(name,"vkCmdBeginRenderPass"))return (PFN_vkVoidFunction)begin_pass;
    if(!strcmp(name,"vkCreateImage"))return (PFN_vkVoidFunction)create_image;
    if(!strcmp(name,"vkDestroyImage"))return (PFN_vkVoidFunction)destroy_image;
    return vkGetDeviceProcAddr(device,name);
}
static PFN_vkVoidFunction VKAPI_CALL instance_proc(VkInstance instance,const char *name)
{return !strcmp(name,"vkGetDeviceProcAddr")?(PFN_vkVoidFunction)device_proc:vkGetInstanceProcAddr(instance,name);}
VkResult cubit_mesa_service_start(uint64_t slot,struct cubit_mesa_service **owner)
{if(slot!=25||!borrowed.device)return -3;*owner=&service;return 0;}
VkBool32 cubit_mesa_service_device(struct cubit_mesa_service *owner,struct cubit_mesa_service_device *view)
{if(owner!=&service||closes)return 0;*view=borrowed;return 1;}
VkResult cubit_mesa_service_status(struct cubit_mesa_service *owner){return owner==&service&&!closes?0:-3;}
enum cubit_mesa_service_retirement cubit_mesa_service_close(struct cubit_mesa_service *owner)
{if(owner!=&service||closes||!context||context->live)return 2;++closes;return 0;}
void *__real_cubit_vulkan_device_context_request(const struct cubit_mesa_service_device *);
void *__wrap_cubit_vulkan_device_context_request(const struct cubit_mesa_service_device *view)
{struct cubit_vulkan_context_request *r=__real_cubit_vulkan_device_context_request(view);if(r)context=r->fresh;return r;}
uint32_t __real_cubit_vulkan_device_targets_prepare(uint32_t,uint32_t,struct cubit_vulkan_device_targets *);
uint32_t __wrap_cubit_vulkan_device_targets_prepare(uint32_t w,uint32_t h,struct cubit_vulkan_device_targets *out)
{uint32_t result=__real_cubit_vulkan_device_targets_prepare(w,h,out);if(!result)targets=*out;return result;}
struct font_request {uint32_t face,code,n,d,width,height,pitch,capacity;};
struct font_metrics {uint32_t advance,height;};
extern int desktop_gpu_scene_real_start(int);
extern uint32_t __real_cubit_font_raster_mask(const struct font_request *,void *,struct font_metrics *);
static struct cubit_vulkan_upload_buffer *upload;
static unsigned raster_calls;
void *__real_cubit_vulkan_device_upload_prepare(void);
void *__wrap_cubit_vulkan_device_upload_prepare(void)
{void *p=__real_cubit_vulkan_device_upload_prepare();if(p)upload=p;return p;}
uint32_t __wrap_cubit_font_raster_mask(const struct font_request *r,void *pixels,struct font_metrics *m)
{
    if(!upload||pixels!=upload->mapped||r->capacity>4096){fprintf(stderr,"font did not target owned Vulkan staging\n");return 1;}
    ++raster_calls;return __real_cubit_font_raster_mask(r,pixels,m);
}
int run_desktop_real(VkInstance instance,VkPhysicalDevice physical,VkDevice device,VkQueue queue,uint32_t family)
{
    borrowed=(struct cubit_mesa_service_device){instance,physical,device,queue,family,instance_proc};
    CHECK(desktop_gpu_scene_real_open()==0&&context&&context->live&&targets.description);
    VkBuffer readback;VkDeviceMemory memory;void *pixels;
    const VkBufferCreateInfo bi={.sType=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,.size=96*64*4,.usage=VK_BUFFER_USAGE_TRANSFER_DST_BIT};
    VK(vkCreateBuffer(device,&bi,NULL,&readback));VkMemoryRequirements req;vkGetBufferMemoryRequirements(device,readback,&req);
    VkPhysicalDeviceMemoryProperties props;vkGetPhysicalDeviceMemoryProperties(physical,&props);uint32_t type=32;
    for(uint32_t i=0;i<props.memoryTypeCount;i++)if((req.memoryTypeBits&(1u<<i))&&
        (props.memoryTypes[i].propertyFlags&(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))==
        (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)){type=i;break;}
    CHECK(type<32);const VkMemoryAllocateInfo ai={.sType=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,.allocationSize=req.size,.memoryTypeIndex=type};
    VK(vkAllocateMemory(device,&ai,NULL,&memory));VK(vkBindBufferMemory(device,readback,memory,0));VK(vkMapMemory(device,memory,0,req.size,0,&pixels));
    const VkCommandPoolCreateInfo pi={.sType=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,.flags=VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,.queueFamilyIndex=family};
    VkCommandPool pool;VK(vkCreateCommandPool(device,&pi,NULL,&pool));
    const VkCommandBufferAllocateInfo ca={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,.commandPool=pool,.level=VK_COMMAND_BUFFER_LEVEL_PRIMARY,.commandBufferCount=1};
    VkCommandBuffer cmd;VK(vkAllocateCommandBuffers(device,&ca,&cmd));
    const VkFenceCreateInfo fi={.sType=VK_STRUCTURE_TYPE_FENCE_CREATE_INFO};VkFence fence;VK(vkCreateFence(device,&fi,NULL,&fence));
    for(unsigned version=0;version<8;version++){
        const unsigned ns[]={1,5,3,2},ds[]={1,4,2,1};
        const char *codes="AgW?jQm@";
        unsigned n=ns[version/2],d=ds[version/2],w=(32*n+d-1)/d,h=(17*n+d-1)/d,pitch=(w+15)/16*16;
        unsigned origin_x=(4*n+d)/(2*d),origin_y=(6*n+d)/(2*d);
        unsigned char reference[4096];memset(reference,0xa5,sizeof reference);
        const struct font_request request={version%2,(unsigned char)codes[version],13*n,d,w,h,pitch,pitch*h};
        struct font_metrics metrics={0};
        CHECK(__real_cubit_font_raster_mask(&request,reference,&metrics)==0&&metrics.advance>0&&metrics.height==h);
        int advance=desktop_gpu_scene_real_start((int)version);
        CHECK(advance==0&&raster_calls==version+1);
        CHECK(source_creates==version+1&&source_destroys==0);
        CHECK(desktop_gpu_scene_real_import()==1);
        VK(vkWaitForFences(device,1,&context->fence,VK_TRUE,5000000000ull));
        CHECK(desktop_gpu_scene_real_import()==1);
        CHECK(desktop_gpu_scene_real_poll_upload()==0);
        CHECK(desktop_gpu_scene_real_import()==0);selected=VK_NULL_HANDLE;CHECK(desktop_gpu_scene_real_render()==0&&selected);
        VK(vkWaitForFences(device,1,&context->fence,VK_TRUE,5000000000ull));CHECK(desktop_gpu_scene_real_poll_frame()==0);
        struct cubit_vulkan_target_request *target=targets.description;unsigned index=3;
        for(unsigned n=0;n<3;n++)if(target->fresh->framebuffers[n]==selected)index=n;
        CHECK(index<3);struct cubit_vulkan_owned_image *image=targets.images[index];CHECK(image&&image->stage==2);
        VK(vkResetCommandBuffer(cmd,0));const VkCommandBufferBeginInfo cb={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,.flags=VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT};
        VK(vkBeginCommandBuffer(cmd,&cb));
        VkImageMemoryBarrier barrier={.sType=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,.srcAccessMask=VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
            .dstAccessMask=VK_ACCESS_TRANSFER_READ_BIT,.oldLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,.newLayout=VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
            .srcQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.dstQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.image=image->image,
            .subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
        vkCmdPipelineBarrier(cmd,VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,VK_PIPELINE_STAGE_TRANSFER_BIT,0,0,NULL,0,NULL,1,&barrier);
        const VkBufferImageCopy region={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},.imageExtent={96,64,1}};
        vkCmdCopyImageToBuffer(cmd,image->image,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,readback,1,&region);
        barrier.srcAccessMask=VK_ACCESS_TRANSFER_READ_BIT;barrier.dstAccessMask=VK_ACCESS_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
        barrier.oldLayout=VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;barrier.newLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
        vkCmdPipelineBarrier(cmd,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,0,0,NULL,0,NULL,1,&barrier);
        const VkMemoryBarrier host={.sType=VK_STRUCTURE_TYPE_MEMORY_BARRIER,.srcAccessMask=VK_ACCESS_TRANSFER_WRITE_BIT,.dstAccessMask=VK_ACCESS_HOST_READ_BIT};
        vkCmdPipelineBarrier(cmd,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_HOST_BIT,0,1,&host,0,NULL,0,NULL);
        VK(vkEndCommandBuffer(cmd));VK(vkResetFences(device,1,&fence));
        const VkSubmitInfo submit={.sType=VK_STRUCTURE_TYPE_SUBMIT_INFO,.commandBufferCount=1,.pCommandBuffers=&cmd};
        VK(vkQueueSubmit(queue,1,&submit,fence));VK(vkWaitForFences(device,1,&fence,VK_TRUE,5000000000ull));
        unsigned lit=0;
        for(unsigned y=0;y<64;y++)for(unsigned x=0;x<96;x++){
            unsigned alpha=0;
            if(x>=origin_x&&y>=origin_y&&x-origin_x<w&&y-origin_y<h)
                alpha=reference[(y-origin_y)*pitch+x-origin_x];
            if(alpha)++lit;
            uint32_t expected=0xff000000u|alpha*0x010101u;
            if(((uint32_t *)pixels)[y*96+x]!=expected)
                fprintf(stderr,"glyph=%u scale=%u/%u pixel=%u,%u got=%08x want=%08x\n",version,n,d,x,y,((uint32_t *)pixels)[y*96+x],expected);
            CHECK(((uint32_t *)pixels)[y*96+x]==expected);
        }
        CHECK(lit>0&&raster_calls==version+1);
        const char *capture=getenv("CUBIT_GLYPH_CAPTURE_DIR");
        if(capture){
            char path[4096];int length=snprintf(path,sizeof path,"%s/glyph-%u.ppm",capture,version);
            CHECK(length>0&&(size_t)length<sizeof path);FILE *out=fopen(path,"wb");CHECK(out);
            CHECK(fprintf(out,"P6\n96 64\n255\n")>0);
            for(unsigned y=0;y<64;y++)for(unsigned x=0;x<96;x++){
                uint32_t pixel=((uint32_t *)pixels)[y*96+x];
                unsigned char rgb[]={(unsigned char)(pixel>>16),(unsigned char)(pixel>>8),(unsigned char)pixel};
                CHECK(fwrite(rgb,1,3,out)==3);
            }
            CHECK(fclose(out)==0);
        }

    }
    CHECK(desktop_gpu_scene_real_close()==0&&closes==1&&!context->live&&source_creates==8&&source_destroys==8&&source_live==0);
    for(unsigned n=0;n<3;n++){struct cubit_vulkan_owned_image *image=targets.images[n];CHECK(!image->image&&!image->memory&&image->stage==3);}
    vkDestroyFence(device,fence,NULL);vkDestroyCommandPool(device,pool,NULL);
    vkUnmapMemory(device,memory);vkDestroyBuffer(device,readback,NULL);vkFreeMemory(device,memory,NULL);
    puts("HOST ONLY real fonts -> owned Vulkan staging -> glyph scene: 8 glyphs, 2 faces, 100/125/150/200 percent, 8 retained allocations, cache hits avoid rasterization, 49152 exact pixels, direct mapped destination, completion-gated import and accounted cleanup PASS");
    return 0;
}
