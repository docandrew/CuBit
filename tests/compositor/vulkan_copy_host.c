/* Linux lavapipe oracle. No CuBit/scanout/hardware timing claim. */
#include "vulkan_copy.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define W 32
#define H 24
#define BYTES (W*H*4)
#define CHECK(x) do { if (!(x)) { fprintf(stderr,"FAIL line %d: %s\n",__LINE__,#x); return 1; } } while (0)
#define VK(x) CHECK((x)==VK_SUCCESS)
struct input { int32_t tw,th,sw,sh,x,y,w,h,clipped,cx,cy,cw,ch; };
extern int32_t test_plan_and_record(void *, const struct input *);
static unsigned errors, calls;
static VKAPI_ATTR VkBool32 VKAPI_CALL diagnostic(VkDebugUtilsMessageSeverityFlagBitsEXT severity,
    VkDebugUtilsMessageTypeFlagsEXT type, const VkDebugUtilsMessengerCallbackDataEXT *data, void *arg)
{
    (void)type; (void)arg;
    if (severity & VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT) ++errors;
    fprintf(stderr,"VULKAN VALIDATION: %s\n",data->pMessage);
    return VK_FALSE;
}
static VKAPI_ATTR void VKAPI_CALL counted_copy(VkCommandBuffer c,VkImage s,VkImageLayout sl,
    VkImage d,VkImageLayout dl,uint32_t count,const VkImageCopy *regions)
{
    ++calls;
    vkCmdCopyImage(c,s,sl,d,dl,count,regions);
}
static uint32_t memory_type(VkPhysicalDevice p,uint32_t bits,VkMemoryPropertyFlags flags)
{
    VkPhysicalDeviceMemoryProperties m; vkGetPhysicalDeviceMemoryProperties(p,&m);
    for(uint32_t n=0;n<m.memoryTypeCount;n++)
        if ((bits&(1u<<n)) && (m.memoryTypes[n].propertyFlags&flags)==flags) return n;
    return UINT32_MAX;
}
static int image(VkPhysicalDevice p,VkDevice d,VkImage *im,VkDeviceMemory *mem)
{
    const VkImageCreateInfo i={.sType=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,.imageType=VK_IMAGE_TYPE_2D,
        .format=VK_FORMAT_B8G8R8A8_UNORM,.extent={W,H,1},.mipLevels=1,.arrayLayers=1,
        .samples=VK_SAMPLE_COUNT_1_BIT,.tiling=VK_IMAGE_TILING_OPTIMAL,
        .usage=VK_IMAGE_USAGE_TRANSFER_SRC_BIT|VK_IMAGE_USAGE_TRANSFER_DST_BIT,
        .sharingMode=VK_SHARING_MODE_EXCLUSIVE,.initialLayout=VK_IMAGE_LAYOUT_UNDEFINED};
    VK(vkCreateImage(d,&i,NULL,im));
    VkMemoryRequirements r; vkGetImageMemoryRequirements(d,*im,&r);
    const uint32_t mt=memory_type(p,r.memoryTypeBits,0); CHECK(mt!=UINT32_MAX);
    const VkMemoryAllocateInfo a={.sType=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,.allocationSize=r.size,.memoryTypeIndex=mt};
    VK(vkAllocateMemory(d,&a,NULL,mem)); VK(vkBindImageMemory(d,*im,*mem,0)); return 0;
}
static int buffer(VkPhysicalDevice p,VkDevice d,VkBuffer *b,VkDeviceMemory *mem,void **mapped)
{
    const VkBufferCreateInfo i={.sType=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,.size=BYTES,
        .usage=VK_BUFFER_USAGE_TRANSFER_SRC_BIT|VK_BUFFER_USAGE_TRANSFER_DST_BIT,.sharingMode=VK_SHARING_MODE_EXCLUSIVE};
    VK(vkCreateBuffer(d,&i,NULL,b)); VkMemoryRequirements r; vkGetBufferMemoryRequirements(d,*b,&r);
    const uint32_t mt=memory_type(p,r.memoryTypeBits,VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
    CHECK(mt!=UINT32_MAX);
    const VkMemoryAllocateInfo a={.sType=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,.allocationSize=r.size,.memoryTypeIndex=mt};
    VK(vkAllocateMemory(d,&a,NULL,mem)); VK(vkBindBufferMemory(d,*b,*mem,0)); VK(vkMapMemory(d,*mem,0,BYTES,0,mapped)); return 0;
}
static void barrier(VkCommandBuffer cmd,VkImage im,VkImageLayout old,VkAccessFlags src,VkAccessFlags dst)
{
    const VkImageMemoryBarrier b={.sType=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,.srcAccessMask=src,.dstAccessMask=dst,
        .oldLayout=old,.newLayout=VK_IMAGE_LAYOUT_GENERAL,.srcQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,
        .dstQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.image=im,.subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
    vkCmdPipelineBarrier(cmd,old==VK_IMAGE_LAYOUT_UNDEFINED ? VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT : VK_PIPELINE_STAGE_TRANSFER_BIT,
        VK_PIPELINE_STAGE_TRANSFER_BIT,0,0,NULL,0,NULL,1,&b);
}
static int submit(VkDevice d,VkQueue q,VkCommandBuffer cmd,VkFence fence)
{
    VK(vkEndCommandBuffer(cmd));
    const VkSubmitInfo s={.sType=VK_STRUCTURE_TYPE_SUBMIT_INFO,.commandBufferCount=1,.pCommandBuffers=&cmd};
    VK(vkQueueSubmit(q,1,&s,fence)); VK(vkWaitForFences(d,1,&fence,VK_TRUE,5000000000ull));
    VK(vkResetFences(d,1,&fence)); VK(vkResetCommandBuffer(cmd,0)); return 0;
}
int run_vulkan_copy_tests(void)
{
    const char *layers[]={"VK_LAYER_KHRONOS_validation"};
    const char *extensions[]={VK_EXT_DEBUG_UTILS_EXTENSION_NAME,VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME};
    const VkValidationFeatureEnableEXT sync=VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT;
    const VkValidationFeaturesEXT validation={.sType=VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT,.enabledValidationFeatureCount=1,.pEnabledValidationFeatures=&sync};
    VkDebugUtilsMessengerCreateInfoEXT debug={.sType=VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,.pNext=&validation,
        .messageSeverity=VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
        .messageType=VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT|VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT,
        .pfnUserCallback=diagnostic};
    const VkInstanceCreateInfo create={.sType=VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,.pNext=&debug,
        .enabledLayerCount=1,.ppEnabledLayerNames=layers,.enabledExtensionCount=2,.ppEnabledExtensionNames=extensions};
    VkInstance inst; VK(vkCreateInstance(&create,NULL,&inst));
    PFN_vkCreateDebugUtilsMessengerEXT make_debug=(PFN_vkCreateDebugUtilsMessengerEXT)vkGetInstanceProcAddr(inst,"vkCreateDebugUtilsMessengerEXT");
    PFN_vkDestroyDebugUtilsMessengerEXT free_debug=(PFN_vkDestroyDebugUtilsMessengerEXT)vkGetInstanceProcAddr(inst,"vkDestroyDebugUtilsMessengerEXT");
    CHECK(make_debug && free_debug); debug.pNext=NULL;
    VkDebugUtilsMessengerEXT messenger; VK(make_debug(inst,&debug,NULL,&messenger));
    uint32_t count=1; VkPhysicalDevice phy; VK(vkEnumeratePhysicalDevices(inst,&count,&phy)); CHECK(count==1);
    VkPhysicalDeviceProperties properties; vkGetPhysicalDeviceProperties(phy,&properties);
    printf("HOST ONLY Vulkan copy device: %s\n",properties.deviceName); CHECK(properties.deviceType==VK_PHYSICAL_DEVICE_TYPE_CPU);
    uint32_t n=0; vkGetPhysicalDeviceQueueFamilyProperties(phy,&n,NULL); CHECK(n>0);
    VkQueueFamilyProperties *families=calloc(n,sizeof(*families)); CHECK(families);
    vkGetPhysicalDeviceQueueFamilyProperties(phy,&n,families); uint32_t family=UINT32_MAX;
    for(uint32_t j=0;j<n;j++) if(families[j].queueFlags&VK_QUEUE_GRAPHICS_BIT) {family=j;break;}
    free(families); CHECK(family!=UINT32_MAX);
    const float priority=1;
    const VkDeviceQueueCreateInfo qi={.sType=VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,.queueFamilyIndex=family,.queueCount=1,.pQueuePriorities=&priority};
    const VkDeviceCreateInfo di={.sType=VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,.queueCreateInfoCount=1,.pQueueCreateInfos=&qi};
    VkDevice d; VK(vkCreateDevice(phy,&di,NULL,&d)); VkQueue queue; vkGetDeviceQueue(d,family,0,&queue);
    VkImage source,target; VkDeviceMemory sm,tm,um,rm; VkBuffer upload,readback; void *up,*rp;
    CHECK(!image(phy,d,&source,&sm) && !image(phy,d,&target,&tm));
    CHECK(!buffer(phy,d,&upload,&um,&up) && !buffer(phy,d,&readback,&rm,&rp));
    uint32_t *pixels=up;
    for(unsigned y=0;y<H;y++) for(unsigned x=0;x<W;x++) pixels[y*W+x]=0xff000000u|((x+1)<<16)|((y+1)<<8)|(x^y^85);
    const VkCommandPoolCreateInfo pi={.sType=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,.flags=VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,.queueFamilyIndex=family};
    VkCommandPool pool; VK(vkCreateCommandPool(d,&pi,NULL,&pool));
    const VkCommandBufferAllocateInfo ai={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,.commandPool=pool,.level=VK_COMMAND_BUFFER_LEVEL_PRIMARY,.commandBufferCount=1};
    VkCommandBuffer cmd; VK(vkAllocateCommandBuffers(d,&ai,&cmd));
    const VkFenceCreateInfo fi={.sType=VK_STRUCTURE_TYPE_FENCE_CREATE_INFO}; VkFence fence; VK(vkCreateFence(d,&fi,NULL,&fence));
    const VkCommandBufferBeginInfo bi={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,.flags=VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT};
    const VkBufferImageCopy whole={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},.imageExtent={W,H,1}};
    VK(vkBeginCommandBuffer(cmd,&bi));
    barrier(cmd,source,VK_IMAGE_LAYOUT_UNDEFINED,0,VK_ACCESS_TRANSFER_WRITE_BIT);
    barrier(cmd,target,VK_IMAGE_LAYOUT_UNDEFINED,0,VK_ACCESS_TRANSFER_WRITE_BIT);
    vkCmdCopyBufferToImage(cmd,upload,source,VK_IMAGE_LAYOUT_GENERAL,1,&whole);
    barrier(cmd,source,VK_IMAGE_LAYOUT_GENERAL,VK_ACCESS_TRANSFER_WRITE_BIT,VK_ACCESS_TRANSFER_READ_BIT);
    CHECK(!submit(d,queue,cmd,fence));
    const int negative=getenv("CUBIT_VULKAN_COPY_NEGATIVE")!=NULL;
    unsigned visible=0,empty=0,rejected=0;
    for(unsigned test=0;test<160;test++) {
        struct input in={W,H,W,H,(int)(test%37),(int)(test%29),1+(int)(test%40),1+(int)(test%31),test%2,
                         (int)((test*3)%40),(int)((test*7)%30),1+(int)(test%24),1+(int)(test%22)};
        if(test<8 || test>=152) in=(struct input){W,H,W,H,3,2,17,13,1,7,4,9,7};
        struct cubit_vulkan_copy b={counted_copy,cmd,target,source};
        void *borrowed=&b;
        const unsigned fault=test>=152 ? test-152 : 99;
        switch(fault) {case 0:borrowed=NULL;break;case 1:b.record=NULL;break;case 2:b.command=VK_NULL_HANDLE;break;
            case 3:b.target=VK_NULL_HANDLE;break;case 4:b.source=VK_NULL_HANDLE;break;case 5:b.source=b.target;break;
            default:break;}
        if(fault==6) in.w=0;
        if(fault==7) {in.x=INT32_MAX;in.w=INT32_MAX;in.cx=INT32_MAX;in.cw=INT32_MAX;}
        VK(vkBeginCommandBuffer(cmd,&bi));
        barrier(cmd,target,VK_IMAGE_LAYOUT_GENERAL,VK_ACCESS_TRANSFER_READ_BIT|VK_ACCESS_TRANSFER_WRITE_BIT,VK_ACCESS_TRANSFER_WRITE_BIT);
        const VkClearColorValue black={.float32={0,0,0,1}};
        const VkImageSubresourceRange range={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1};
        vkCmdClearColorImage(cmd,target,VK_IMAGE_LAYOUT_GENERAL,&black,1,&range);
        if(!negative) barrier(cmd,target,VK_IMAGE_LAYOUT_GENERAL,VK_ACCESS_TRANSFER_WRITE_BIT,VK_ACCESS_TRANSFER_WRITE_BIT);
        const unsigned before=calls;
        const int32_t outcome=test_plan_and_record(borrowed,&in);
        unsigned touched=0;
        uint32_t expected[W*H];
        for(int y=0;y<H;y++) for(int x=0;x<W;x++) {
            const int64_t sx=(int64_t)x-in.x,sy=(int64_t)y-in.y;
            const int inside=sx>=0 && sy>=0 && sx<in.w && sy<in.h && sx<in.sw && sy<in.sh &&
                (!in.clipped || ((int64_t)x>=in.cx && (int64_t)y>=in.cy && (int64_t)x-in.cx<in.cw && (int64_t)y-in.cy<in.ch));
            touched+=inside;
            expected[y*W+x]=inside && fault>=6 ? pixels[sy*W+sx] : 0xff000000u;
        }
        const int want=touched==0 ? 0 : fault<6 ? 2 : 1;
        CHECK(outcome==want); CHECK(calls==before+(want==1));
        if(want==0)empty++; else if(want==1)visible++; else rejected++;
        barrier(cmd,target,VK_IMAGE_LAYOUT_GENERAL,VK_ACCESS_TRANSFER_WRITE_BIT,VK_ACCESS_TRANSFER_READ_BIT);
        vkCmdCopyImageToBuffer(cmd,target,VK_IMAGE_LAYOUT_GENERAL,readback,1,&whole);
        const VkMemoryBarrier host={.sType=VK_STRUCTURE_TYPE_MEMORY_BARRIER,.srcAccessMask=VK_ACCESS_TRANSFER_WRITE_BIT,.dstAccessMask=VK_ACCESS_HOST_READ_BIT};
        vkCmdPipelineBarrier(cmd,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_HOST_BIT,0,1,&host,0,NULL,0,NULL);
        CHECK(!submit(d,queue,cmd,fence));
        const uint32_t *got=rp;
        for(unsigned k=0;k<W*H;k++) if(got[k]!=expected[k]) {
            fprintf(stderr,"PIXEL FAIL test=%u x=%u y=%u got=%08x expected=%08x\n",test,k%W,k/W,got[k],expected[k]); return 1;
        }
    }
    VK(vkDeviceWaitIdle(d));
    vkDestroyFence(d,fence,NULL); vkDestroyCommandPool(d,pool,NULL);
    vkUnmapMemory(d,um); vkUnmapMemory(d,rm);
    vkDestroyBuffer(d,upload,NULL); vkDestroyBuffer(d,readback,NULL); vkFreeMemory(d,um,NULL); vkFreeMemory(d,rm,NULL);
    vkDestroyImage(d,source,NULL); vkDestroyImage(d,target,NULL); vkFreeMemory(d,sm,NULL); vkFreeMemory(d,tm,NULL);
    vkDestroyDevice(d,NULL); free_debug(inst,messenger,NULL); vkDestroyInstance(inst,NULL);
    printf("HOST ONLY 160 submissions, %u draws, %u empty, %u rejected, 122880 pixel comparisons, validation errors=%u\n",visible,empty,rejected,errors);
    CHECK(errors==0); CHECK(visible>20 && empty>20 && rejected==6); return 0;
}
