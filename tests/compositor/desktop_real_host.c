/* HOST ONLY. Real Desktop/Vulkan; service admission borrows the harness device.
 * Wrappers observe private metadata/selected framebuffer, never alter commands. */
#include "vulkan_device_storage.h"
#include <stdio.h>
#include <string.h>
#define CHECK(x) do { if(!(x)){fprintf(stderr,"Desktop oracle line %d: %s\n",__LINE__,#x);return 1;} } while(0)
#define VK(x) CHECK((x)==VK_SUCCESS)
extern int desktop_real_open(void),desktop_real_submit(void),desktop_real_poll_upload(void);
extern int desktop_real_import(void),desktop_real_render(void),desktop_real_poll_frame(void);
extern int desktop_real_restart(void),desktop_real_close(void),desktop_real_reconfigure(int);
extern void *desktop_real_begin(uint32_t *,uint32_t *);
static struct cubit_mesa_service_device borrowed;
struct cubit_mesa_service { unsigned host; };
static struct cubit_mesa_service service;
static struct cubit_vulkan_context *context;
static struct cubit_vulkan_device_targets targets;
static VkFramebuffer selected;
static unsigned closes,source_creates,source_destroys;
static VkImage sampled_image;
static VkResult VKAPI_CALL create_image(VkDevice d,const VkImageCreateInfo *info,const VkAllocationCallbacks *a,VkImage *out)
{
    VkResult result=vkCreateImage(d,info,a,out);
    if(result==VK_SUCCESS&&(info->usage&VK_IMAGE_USAGE_SAMPLED_BIT)){++source_creates;sampled_image=*out;}
    return result;
}
static void VKAPI_CALL destroy_image(VkDevice d,VkImage image,const VkAllocationCallbacks *a)
{if(image==sampled_image){++source_destroys;sampled_image=VK_NULL_HANDLE;}vkDestroyImage(d,image,a);}

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
static uint32_t color(unsigned x,unsigned y,unsigned version)
{return 0xff000000u|(((x*7+version*17)&255)<<16)|(((y*9+version*5)&255)<<8)|((x+y+version*11)&255);}
int run_desktop_real(VkInstance instance,VkPhysicalDevice physical,VkDevice device,VkQueue queue,uint32_t family)
{
    borrowed=(struct cubit_mesa_service_device){instance,physical,device,queue,family,instance_proc};
    CHECK(desktop_real_open()==0&&context&&context->live&&targets.description);
    VkBuffer readback;VkDeviceMemory memory;void *pixels;
    const VkBufferCreateInfo bi={.sType=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,.size=32*24*4,.usage=VK_BUFFER_USAGE_TRANSFER_DST_BIT};
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
    unsigned chunks=0;
    for(unsigned version=0;version<8;version++){
        unsigned mask=(version/2)%2;
        if(version%2){VkImage old=sampled_image;unsigned allocations=source_creates;
            CHECK(desktop_real_restart()==0&&sampled_image==old&&source_creates==allocations);
        }else if(version)CHECK(desktop_real_reconfigure((int)mask)==0);
        CHECK(source_creates==1+version/2&&source_destroys==version/2);
        CHECK(desktop_real_import()==1);
        for(unsigned done=0;done<24;){
            uint32_t first=0,rows=0;uint32_t *mapping=desktop_real_begin(&first,&rows);
            CHECK(mapping&&first==done&&rows==(mask?8u:2u));
            memset(mapping,0xa5,384);
            for(unsigned y=0;y<rows;y++)for(unsigned x=0;x<32;x++){
                if(mask)((unsigned char *)mapping)[y*48+x]=(x+first+y+version)%2?255:0;
                else mapping[y*48+x]=color(x,first+y,version);
            }
            CHECK(desktop_real_submit()==0);CHECK(desktop_real_import()==1);
            VK(vkWaitForFences(device,1,&context->fence,VK_TRUE,5000000000ull));
            CHECK(desktop_real_import()==1); /* Fence alone must not publish. */
            CHECK(desktop_real_poll_upload()==0);done+=rows;++chunks;
            if(done<24)CHECK(desktop_real_import()==1);
        }
        CHECK(desktop_real_import()==0);selected=VK_NULL_HANDLE;CHECK(desktop_real_render()==0&&selected);
        VK(vkWaitForFences(device,1,&context->fence,VK_TRUE,5000000000ull));CHECK(desktop_real_poll_frame()==0);
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
        const VkBufferImageCopy region={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},.imageExtent={32,24,1}};
        vkCmdCopyImageToBuffer(cmd,image->image,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,readback,1,&region);
        barrier.srcAccessMask=VK_ACCESS_TRANSFER_READ_BIT;barrier.dstAccessMask=VK_ACCESS_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
        barrier.oldLayout=VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;barrier.newLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
        vkCmdPipelineBarrier(cmd,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,0,0,NULL,0,NULL,1,&barrier);
        const VkMemoryBarrier host={.sType=VK_STRUCTURE_TYPE_MEMORY_BARRIER,.srcAccessMask=VK_ACCESS_TRANSFER_WRITE_BIT,.dstAccessMask=VK_ACCESS_HOST_READ_BIT};
        vkCmdPipelineBarrier(cmd,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_HOST_BIT,0,1,&host,0,NULL,0,NULL);
        VK(vkEndCommandBuffer(cmd));VK(vkResetFences(device,1,&fence));
        const VkSubmitInfo submit={.sType=VK_STRUCTURE_TYPE_SUBMIT_INFO,.commandBufferCount=1,.pCommandBuffers=&cmd};
        VK(vkQueueSubmit(queue,1,&submit,fence));VK(vkWaitForFences(device,1,&fence,VK_TRUE,5000000000ull));
        for(unsigned y=0;y<24;y++)for(unsigned x=0;x<32;x++){uint32_t expected=mask?((x+y+version)%2?0xffff0000u:0xff204060u):color(x,y,version);if(((uint32_t *)pixels)[y*32+x]!=expected)fprintf(stderr,"version=%u target=%u pixel=%u,%u got=%08x want=%08x\n",version,index,x,y,((uint32_t *)pixels)[y*32+x],expected);CHECK(((uint32_t *)pixels)[y*32+x]==expected);}
    }
    CHECK(desktop_real_close()==0&&closes==1&&!context->live&&source_creates==4&&source_destroys==4&&!sampled_image);
    for(unsigned n=0;n<3;n++){struct cubit_vulkan_owned_image *image=targets.images[n];CHECK(!image->image&&!image->memory&&image->stage==3);}
    vkDestroyFence(device,fence,NULL);vkDestroyCommandPool(device,pool,NULL);
    vkUnmapMemory(device,memory);vkDestroyBuffer(device,readback,NULL);vkFreeMemory(device,memory,NULL);
    printf("HOST ONLY strided Desktop scheduler: %u chunks, 8 BGRA/R8 contents (4 allocations +4 same-backing rewrites), 6144 exact pixels, premature import/write/retirement gates, accounted cleanup PASS; service admission is a host adapter\n",chunks);
    return 0;
}
