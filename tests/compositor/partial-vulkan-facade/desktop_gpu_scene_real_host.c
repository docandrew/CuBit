extern int desktop_gpu_scene_real_partial(int);
/* HOST ONLY. Real Desktop/Vulkan; service admission borrows the harness device.
 * Wrappers observe private metadata/selected framebuffer, never alter commands. */
#include "vulkan_device_storage.h"
#include "vulkan_owned_targets.h"
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
static uintptr_t watched_output;
static size_t watched_size;
static uint64_t copied_bytes,total_copied_bytes,readback_bytes,total_readback_bytes;
void watch_output_copy(void *base,uint64_t bytes){watched_output=(uintptr_t)base;watched_size=(size_t)bytes;copied_bytes=0;readback_bytes=0;}
uint64_t output_copy_bytes(void){return copied_bytes;}
uint64_t recorded_readback_bytes(void){return readback_bytes;}
static void VKAPI_CALL readback_copy(VkCommandBuffer command,VkImage image,VkImageLayout layout,
 VkBuffer buffer,uint32_t count,const VkBufferImageCopy *regions){
 assert(count>0&&count<=8);
 for(uint32_t i=0;i<count;i++){
  const VkBufferImageCopy *r=&regions[i];
  assert(r->imageOffset.x>=0&&r->imageOffset.y>=0&&r->imageOffset.z==0);
  assert(r->bufferRowLength==96&&r->bufferImageHeight==64);
  assert(r->bufferOffset==((uint64_t)r->imageOffset.y*96+(uint32_t)r->imageOffset.x)*4);
  assert((uint32_t)r->imageOffset.x+r->imageExtent.width<=96);
  assert((uint32_t)r->imageOffset.y+r->imageExtent.height<=64&&r->imageExtent.depth==1);
  uint64_t bytes=(uint64_t)r->imageExtent.width*r->imageExtent.height*4;
  readback_bytes+=bytes;total_readback_bytes+=bytes;
 }
 vkCmdCopyImageToBuffer(command,image,layout,buffer,count,regions);
}
void *__real_memcpy(void *,const void *,size_t);
void *__wrap_memcpy(void *dst,const void *src,size_t bytes){
 uintptr_t address=(uintptr_t)dst;
 if(watched_output && address>=watched_output && address-watched_output<watched_size){
  assert(bytes<=watched_size-(address-watched_output));copied_bytes+=bytes;total_copied_bytes+=bytes;
 }
 return __real_memcpy(dst,src,bytes);
}
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
static uint64_t barrier_calls;
static unsigned missing_readback_dispatch;
static struct cubit_vulkan_upload_buffer *readback;
static void VKAPI_CALL watched_barrier(VkCommandBuffer command,VkPipelineStageFlags source,
 VkPipelineStageFlags target,VkDependencyFlags dependencies,uint32_t memory_count,const VkMemoryBarrier *memory,
 uint32_t buffer_count,const VkBufferMemoryBarrier *buffers,uint32_t image_count,const VkImageMemoryBarrier *images){
 ++barrier_calls;
 vkCmdPipelineBarrier(command,source,target,dependencies,memory_count,memory,buffer_count,buffers,image_count,images);
}
void *__real_cubit_vulkan_device_readback_prepare(void);
void *__wrap_cubit_vulkan_device_readback_prepare(void){
 void *p=__real_cubit_vulkan_device_readback_prepare();if(p)readback=p;return p;
}
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
    if(!strcmp(name,"vkCmdPipelineBarrier"))return (PFN_vkVoidFunction)watched_barrier;
    if(!strcmp(name,"vkCmdCopyImageToBuffer"))return missing_readback_dispatch?NULL:(PFN_vkVoidFunction)readback_copy;
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
extern int facade_frame(int),facade_pump(void),facade_wrong_writer(void);
extern uint32_t facade_pixel(int);
/* These assets are not drawn by this fill-only facade oracle. */
const uint32_t cubit_desktop_wallpaper[2048*576]={0};
const uint32_t cubit_desktop_wallpaper_cubie[2048*1152]={0};
static int rejected_readback_regions(void){
 const struct cubit_vulkan_readback_region good={1,1,4,4};
 const struct cubit_vulkan_readback_region invalid[]={
  {1,1,1,4},{4,1,1,4},{1,1,97,4},{1,1,4,65},{1,4,4,1}};
 uint64_t before=barrier_calls;
 CHECK(readback&&readback->mapped);
 CHECK(cubit_vulkan_owned_targets_record_readback_regions(targets.description,&context->submission,readback,1,0,&good)==2);
 CHECK(cubit_vulkan_owned_targets_record_readback_regions(targets.description,&context->submission,readback,1,9,&good)==2);
 CHECK(cubit_vulkan_owned_targets_record_readback_regions(targets.description,&context->submission,readback,1,1,NULL)==2);
 CHECK(cubit_vulkan_owned_targets_record_readback_regions(targets.description,&context->submission,readback,0,1,&good)==2);
 CHECK(cubit_vulkan_owned_targets_record_readback_regions(targets.description,&context->submission,readback,4,1,&good)==2);
 for(unsigned i=0;i<sizeof invalid/sizeof invalid[0];i++){
  struct cubit_vulkan_readback_region pair[2]={good,invalid[i]};
  CHECK(cubit_vulkan_owned_targets_record_readback_regions(targets.description,&context->submission,readback,1,2,pair)==2);
 }
 struct cubit_vulkan_readback_region overlap[2]={good,good};
 CHECK(cubit_vulkan_owned_targets_record_readback_regions(targets.description,&context->submission,readback,1,2,overlap)==2);
 struct cubit_vulkan_upload_buffer short_buffer=*readback;short_buffer.capacity=96*64*4-1;
 CHECK(cubit_vulkan_owned_targets_record_readback_regions(targets.description,&context->submission,&short_buffer,1,1,&good)==2);
 missing_readback_dispatch=1;
 CHECK(cubit_vulkan_owned_targets_record_readback_regions(targets.description,&context->submission,readback,1,1,&good)==2);
 missing_readback_dispatch=0;
 CHECK(barrier_calls==before&&readback_bytes==0);
 puts("HOST ONLY readback boundary: 13 invalid region/ownership/capacity/dispatch cases rejected before recording PASS");
 return 0;
}
int run_desktop_real(VkInstance instance,VkPhysicalDevice physical,VkDevice device,VkQueue queue,uint32_t family)
{
 borrowed=(struct cubit_mesa_service_device){instance,physical,device,queue,family,instance_proc};
 CHECK(desktop_gpu_scene_real_open()==0&&context&&context->live);
 CHECK(rejected_readback_regions()==0);
 for(unsigned frame=0;frame<16;frame++){
  int begin=facade_frame((int)frame);if(begin)fprintf(stderr,"frame=%u begin=%d\n",frame,begin);CHECK(begin==0);
  unsigned polls=0;int state;
  do{CHECK(facade_wrong_writer()==0);state=facade_pump();if(state>1)fprintf(stderr,"frame=%u copy-state=%d\n",frame,state);CHECK(state==0||state==1);if(state==1)VK(vkQueueWaitIdle(queue));}while(state==1&&++polls<32);
  CHECK(state==0);
  for(unsigned y=0;y<64;y++)for(unsigned x=0;x<96;x++){
   unsigned left=20+(frame%4)*4;
   unsigned top=8+(frame%4)*3;
   uint32_t expected=x>=left&&x<left+8&&y>=24&&y<32?0xff123456u:
      x>=70&&x<74&&y>=top&&y<top+4?0xffabcdefu:0xff102030u;
   uint32_t got=facade_pixel((int)(y*96+x));
   if(got!=expected)fprintf(stderr,"frame=%u pixel=%u,%u got=%08x expected=%08x\n",frame,x,y,got,expected);
   CHECK(got==expected);
  }
 }
 CHECK(desktop_gpu_scene_real_close()==0&&closes==1&&!context->live);
 printf("HOST ONLY output copy: %llu bytes versus 393216 full-frame bytes\n",(unsigned long long)total_copied_bytes);
 CHECK(total_copied_bytes<120000);
 printf("HOST ONLY GPU readback: %llu bytes versus 393216 full-frame bytes\n",(unsigned long long)total_readback_bytes);
 CHECK(total_readback_bytes==total_copied_bytes);
 puts("HOST ONLY separated repair regions and wrong-writer rejection while pending PASS");
 puts("HOST ONLY full Vulkan compositor facade: 16 frames, 98304 exact CPU output pixels, sparse repair, actual GPU readback and copy, accounted cleanup PASS");
 return 0;
}
