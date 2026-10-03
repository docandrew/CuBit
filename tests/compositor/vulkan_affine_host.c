/* Linux lavapipe oracle. No CuBit/scanout/hardware timing claim. */
#include "vulkan_affine.h"
#ifdef CUBIT_DESKTOP_REAL_TEST
extern int run_desktop_real(VkInstance,VkPhysicalDevice,VkDevice,VkQueue,uint32_t);
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef CUBIT_NATIVE_SCENE_TEST
#include "native_scene_bridge.h"
#include "vulkan_owned_targets.h"
#endif
#ifdef CUBIT_VULKAN_OWNED_TARGET_TEST
#include "vulkan_device_storage.h"
#include "vulkan_upload_buffer.h"
extern int test_target_upload_open(void *,uint32_t),test_target_upload_close(void);
extern void *test_target_upload_mapping(void);
extern int test_target_upload_begin(void),test_target_upload_submit(void),test_target_upload_finish(void);
extern uint32_t test_target_upload_first_row(void);
extern int test_target_source_restart(uint32_t,uint32_t,uint32_t),test_target_source_detach(void);
extern int test_target_device_open(uint32_t,uint32_t);
extern int test_target_source_configure(uint32_t,uint32_t,uint32_t),test_target_source_import(void *);
extern void *test_target_source_image(void);
extern int test_target_source_release(void),test_target_textured_fill(void);
extern void *test_target_device_request(int);
extern void *test_target_context_open(void *);
extern int test_target_context_close(void);
extern int test_target_cancel_first(void),test_target_partial_fill(void),test_target_settle_fill(void);
#include "vulkan_owned_targets.h"
extern int test_target_bundle_open(void *,void *,void *,void *,void *,uint32_t);
extern int test_target_bundle_close(int);
extern int test_target_bundle_fill(int,int,int,int,int,int,int),test_target_bundle_finish(void);
#endif
#ifdef CUBIT_VULKAN_OWNED_IMAGE_TEST
#include "vulkan_owned_image.h"
extern int test_owned_allocate(uint32_t,void *,uint32_t),test_owned_release(uint32_t);
extern int test_owned_empty(void);
static VkInstance owned_instance;
static struct cubit_vulkan_owned_image owned_images[8];
static unsigned owned_count;
#endif
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
#include "vulkan_submission.h"
#include "vulkan_sources.h"
#include "vulkan_targets.h"
extern void test_submission_open(void *);
extern int test_submission_initialize_targets(void *),test_submission_close_targets(void);
extern void test_submission_set_targets(void *,void *,void *);
extern int test_submission_target_index(void);
extern void test_submission_damage(int,int,int,int),test_submission_repaint_box(int,int *,int *,int *,int *);
extern int test_submission_repaint_count(void);
extern void test_submission_display_tick(int),test_submission_finish_display(void);
extern int test_submission_register_source(void *);
extern int test_submission_import_source(void *,void *),test_submission_import_mask(void *,void *);
extern void *test_submission_release_mask(void);
extern void *test_submission_release_source(void);
extern int test_submission_start(void),test_submission_finish(void),test_submission_poll(void);
extern int test_submission_budget(void),test_submission_releasable(void),test_submission_cancel(void);
extern int test_submission_begin_scene(void *,uint32_t,uint32_t),test_submission_end_scene(void);
static unsigned forced_pending,observed_pending;
static VkResult VKAPI_CALL delayed_fence(VkDevice device,VkFence fence)
{
    if(forced_pending){--forced_pending;return VK_NOT_READY;}
    return vkGetFenceStatus(device,fence);
}
#endif
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
#define OUTPUTS 3
#else
#define OUTPUTS 1
#endif
#ifdef CUBIT_NATIVE_SCENE_EXTENT
#define W CUBIT_NATIVE_SCENE_EXTENT
#define H CUBIT_NATIVE_SCENE_EXTENT
#else
#define W 32
#define H 24
#endif
#define BYTES (W*H*4)
#define CHECK(x) do { if (!(x)) { fprintf(stderr,"FAIL line %d: %s\n",__LINE__,#x); return 1; } } while (0)
#define VK(x) CHECK((x)==VK_SUCCESS)
struct input { int32_t w,h,n,d,rotation,x,y,l,t,r,b,dl,dt,dr,db,over,mask; uint32_t tint; };
extern int32_t test_affine_and_record(void *, const struct input *);
static unsigned errors, calls;
static VKAPI_ATTR VkBool32 VKAPI_CALL diagnostic(VkDebugUtilsMessageSeverityFlagBitsEXT severity,
    VkDebugUtilsMessageTypeFlagsEXT type, const VkDebugUtilsMessengerCallbackDataEXT *data, void *arg)
{
    (void)type; (void)arg;
    if (severity & VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT) ++errors;
    fprintf(stderr,"VULKAN VALIDATION: %s\n",data->pMessage);
    return VK_FALSE;
}
static VKAPI_ATTR void VKAPI_CALL counted_draw(VkCommandBuffer c,uint32_t vertices,uint32_t instances,uint32_t first,uint32_t base)
{
    ++calls; vkCmdDraw(c,vertices,instances,first,base);
}
static uint32_t memory_type(VkPhysicalDevice p,uint32_t bits,VkMemoryPropertyFlags flags)
{
    VkPhysicalDeviceMemoryProperties m; vkGetPhysicalDeviceMemoryProperties(p,&m);
    for(uint32_t n=0;n<m.memoryTypeCount;n++)
        if ((bits&(1u<<n)) && (m.memoryTypes[n].propertyFlags&flags)==flags) return n;
    return UINT32_MAX;
}
static int image_size(VkPhysicalDevice p,VkDevice d,VkImage *im,VkDeviceMemory *mem,VkFormat format,VkImageUsageFlags usage,uint32_t width,uint32_t height)
{
#ifdef CUBIT_VULKAN_OWNED_IMAGE_TEST
    CHECK(owned_count<8);
    struct cubit_vulkan_owned_image *s=&owned_images[owned_count];
    *s=(struct cubit_vulkan_owned_image){.physical=p,.device=d,.instance=owned_instance,
        .instance_proc=vkGetInstanceProcAddr,.proc=vkGetDeviceProcAddr,
        .width=width,.height=height,.format=format,.usage=usage};
    VkPhysicalDeviceMemoryProperties props;vkGetPhysicalDeviceMemoryProperties(p,&props);
    uint32_t allowed=0;
    for(uint32_t i=0;i<props.memoryTypeCount&&i<32;i++)
        if(!(props.memoryTypes[i].propertyFlags&(VK_MEMORY_PROPERTY_PROTECTED_BIT|VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT)))allowed|=UINT32_C(1)<<i;
    CHECK(test_owned_allocate(owned_count,s,allowed)==0);
    *im=s->image;*mem=s->memory;++owned_count;return 0;
#else
    const VkImageCreateInfo i={.sType=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,.imageType=VK_IMAGE_TYPE_2D,
        .format=format,.extent={width,height,1},.mipLevels=1,.arrayLayers=1,
        .samples=VK_SAMPLE_COUNT_1_BIT,.tiling=VK_IMAGE_TILING_OPTIMAL,
        .usage=usage,
        .sharingMode=VK_SHARING_MODE_EXCLUSIVE,.initialLayout=VK_IMAGE_LAYOUT_UNDEFINED};
    VK(vkCreateImage(d,&i,NULL,im));
    VkMemoryRequirements r; vkGetImageMemoryRequirements(d,*im,&r);
    const uint32_t mt=memory_type(p,r.memoryTypeBits,0); CHECK(mt!=UINT32_MAX);
    const VkMemoryAllocateInfo a={.sType=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,.allocationSize=r.size,.memoryTypeIndex=mt};
    VK(vkAllocateMemory(d,&a,NULL,mem)); VK(vkBindImageMemory(d,*im,*mem,0)); return 0;
#endif
}
static int destroy_image(VkDevice d,VkImage image,VkDeviceMemory memory)
{
#ifdef CUBIT_VULKAN_OWNED_IMAGE_TEST
    for(unsigned i=0;i<owned_count;i++)if(owned_images[i].image==image){
        CHECK(owned_images[i].device==d&&owned_images[i].memory==memory);
        CHECK(test_owned_release(i)==0);return 0;
    }
    CHECK(0);return 1;
#else
    vkDestroyImage(d,image,NULL);vkFreeMemory(d,memory,NULL);return 0;
#endif
}
static int image(VkPhysicalDevice p,VkDevice d,VkImage *im,VkDeviceMemory *mem,VkFormat format,VkImageUsageFlags usage)
{return image_size(p,d,im,mem,format,usage,W,H);}
static int buffer_size(VkPhysicalDevice p,VkDevice d,VkBuffer *b,VkDeviceMemory *mem,void **mapped,VkDeviceSize bytes)
{
    const VkBufferCreateInfo i={.sType=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,.size=bytes,
        .usage=VK_BUFFER_USAGE_TRANSFER_SRC_BIT|VK_BUFFER_USAGE_TRANSFER_DST_BIT,.sharingMode=VK_SHARING_MODE_EXCLUSIVE};
    VK(vkCreateBuffer(d,&i,NULL,b)); VkMemoryRequirements r; vkGetBufferMemoryRequirements(d,*b,&r);
    const uint32_t mt=memory_type(p,r.memoryTypeBits,VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
    CHECK(mt!=UINT32_MAX);
    const VkMemoryAllocateInfo a={.sType=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,.allocationSize=r.size,.memoryTypeIndex=mt};
    VK(vkAllocateMemory(d,&a,NULL,mem)); VK(vkBindBufferMemory(d,*b,*mem,0)); VK(vkMapMemory(d,*mem,0,bytes,0,mapped)); return 0;
}
static int buffer(VkPhysicalDevice p,VkDevice d,VkBuffer *b,VkDeviceMemory *mem,void **mapped)
{return buffer_size(p,d,b,mem,mapped,BYTES);}
static void barrier(VkCommandBuffer cmd,VkImage im,VkImageLayout old,VkImageLayout next,
    VkPipelineStageFlags ss,VkPipelineStageFlags ds,VkAccessFlags src,VkAccessFlags dst)
{
    const VkImageMemoryBarrier b={.sType=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,.srcAccessMask=src,.dstAccessMask=dst,
        .oldLayout=old,.newLayout=next,.srcQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,
        .dstQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.image=im,.subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
    vkCmdPipelineBarrier(cmd,ss,ds,0,0,NULL,0,NULL,1,&b);
}
static int submit(VkDevice d,VkQueue q,VkCommandBuffer cmd,VkFence fence)
{
    VK(vkEndCommandBuffer(cmd));
    const VkSubmitInfo s={.sType=VK_STRUCTURE_TYPE_SUBMIT_INFO,.commandBufferCount=1,.pCommandBuffers=&cmd};
    VK(vkQueueSubmit(q,1,&s,fence)); VK(vkWaitForFences(d,1,&fence,VK_TRUE,5000000000ull));
    VK(vkResetFences(d,1,&fence)); VK(vkResetCommandBuffer(cmd,0)); return 0;
}
#ifdef CUBIT_NATIVE_SCENE_TEST
#include "native_scene_pixels.h"
#endif
#ifdef CUBIT_VULKAN_OWNED_TARGET_TEST
static uint32_t upload_color(unsigned x,unsigned y,unsigned version)
{
    return 0xff000000u|((y+1)*7u<<16)|((x+version)%32u*5u);
}
static int upload_rows(VkDevice device,VkFence fence,struct cubit_vulkan_upload_buffer *upload,
                       struct cubit_vulkan_owned_image *sampled,unsigned version)
{
    unsigned done=0,chunks=0;
    CHECK(test_target_source_import(sampled)==1);CHECK(!test_target_upload_mapping());
    while(done<sampled->height){
        const int rows=test_target_upload_begin();CHECK(rows>0);
        const unsigned first=test_target_upload_first_row();CHECK(first==done&&first+(unsigned)rows<=sampled->height);
        void *mapping=test_target_upload_mapping();CHECK(mapping&&mapping==upload->mapped);
        CHECK(test_target_upload_begin()==0);CHECK(test_target_upload_close()==1);
        for(unsigned y=0;y<(unsigned)rows;y++)for(unsigned x=0;x<sampled->width;x++){
            if(sampled->format==VK_FORMAT_R8_UNORM)((uint8_t *)mapping)[y*sampled->width+x]=(x+first+y+version)%2?255:0;
            else ((uint32_t *)mapping)[y*sampled->width+x]=upload_color(x,first+y,version);
        }
        CHECK(test_target_upload_submit()==0);CHECK(!test_target_upload_mapping());
        CHECK(test_target_upload_close()==1);CHECK(upload->mapped&&upload->memory&&upload->buffer);
        CHECK(test_target_upload_begin()==0);
        VK(vkWaitForFences(device,1,&fence,VK_TRUE,5000000000ull));
        CHECK(test_target_source_import(sampled)==1); /* Fence completion must be observed by policy first. */
        CHECK(test_target_upload_finish()==0);CHECK(!test_target_upload_mapping());
        done+=(unsigned)rows;++chunks;
        if(done<sampled->height)CHECK(test_target_source_import(sampled)==1);
    }
    CHECK(chunks>1);return 0;
}
static int owned_target_pixels(VkInstance instance,VkPhysicalDevice physical,VkDevice device,
                              VkRenderPass pass,VkCommandBuffer command,VkQueue queue,VkFence fence,uint32_t family)
{
    const struct cubit_mesa_service_device device_view={instance,physical,device,queue,family,vkGetInstanceProcAddr};
    struct cubit_vulkan_context_request *context_request=cubit_vulkan_device_context_request(&device_view);
    CHECK(context_request && context_request->fresh);
    struct cubit_vulkan_context *context=context_request->fresh;
    void *owned=test_target_context_open(context_request);CHECK(owned==&context->submission);
    pass=context->pass;command=context->submission.command;fence=context->fence;
    CHECK(cubit_vulkan_device_pipeline_create()==0);
    CHECK(cubit_vulkan_device_pipeline_create()==2);
    CHECK(test_target_device_open(W,H)==0);
    struct cubit_vulkan_target_request *request=test_target_device_request(0);
    CHECK(request && request->fresh && request->pass==pass);
    struct cubit_vulkan_targets *views=request->fresh;
    struct cubit_vulkan_owned_image *images[3];
    for(unsigned i=0;i<3;i++) {
        images[i]=test_target_device_request((int)i+1);
        CHECK(images[i] && images[i]->width==W && images[i]->height==H);
        for(unsigned j=0;j<i;j++)CHECK(images[i]!=images[j]);
    }
    CHECK(test_target_context_close()==2);
    VkBuffer readback;VkDeviceMemory memory;void *pixels;
    CHECK(!buffer(physical,device,&readback,&memory,&pixels));
    CHECK(test_target_cancel_first()==0);
    const uint32_t expected[3]={0xffff0000,0xff00ff00,0xff0000ff};
    for(unsigned i=0;i<3;i++){
        const VkCommandBufferBeginInfo begin={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,.flags=VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT};
        VK(vkBeginCommandBuffer(command,&begin));
        VkClearValue clear={.color={{0,0,0,1}}};clear.color.float32[i]=1;
        VkRenderPassBeginInfo render=views->scenes[i].begin;render.pClearValues=&clear;
        CHECK(cubit_vulkan_owned_targets_prepare_frame(request,owned,i+1,W,H,1)==0);
        vkCmdBeginRenderPass(command,&render,VK_SUBPASS_CONTENTS_INLINE);
        const VkClearAttachment color={.aspectMask=VK_IMAGE_ASPECT_COLOR_BIT,.colorAttachment=0,.clearValue=clear};
        const VkClearRect area={.rect={{0,0},{W,H}},.baseArrayLayer=0,.layerCount=1};
        vkCmdClearAttachments(command,1,&color,1,&area);vkCmdEndRenderPass(command);
        barrier(command,images[i]->image,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
            VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,VK_PIPELINE_STAGE_TRANSFER_BIT,
            VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,VK_ACCESS_TRANSFER_READ_BIT);
        const VkBufferImageCopy copy={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},.imageExtent={W,H,1}};
        vkCmdCopyImageToBuffer(command,images[i]->image,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,readback,1,&copy);
        barrier(command,images[i]->image,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
            VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
            VK_ACCESS_TRANSFER_READ_BIT,VK_ACCESS_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
        const VkMemoryBarrier host={.sType=VK_STRUCTURE_TYPE_MEMORY_BARRIER,.srcAccessMask=VK_ACCESS_TRANSFER_WRITE_BIT,.dstAccessMask=VK_ACCESS_HOST_READ_BIT};
        vkCmdPipelineBarrier(command,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_HOST_BIT,0,1,&host,0,NULL,0,NULL);
        CHECK(!submit(device,queue,command,fence));
        for(unsigned j=0;j<W*H;j++)CHECK(((uint32_t *)pixels)[j]==expected[i]);
    }
    struct cubit_vulkan_upload_buffer *upload=cubit_vulkan_device_upload_prepare();CHECK(upload);
    CHECK(test_target_upload_open(upload,W*4*3)==0);
    CHECK(upload->mapped&&!test_target_upload_mapping());
    CHECK(test_target_source_configure(W,H,0)==0);
    struct cubit_vulkan_owned_image *sampled=test_target_source_image();CHECK(sampled);
    CHECK(!upload_rows(device,fence,upload,sampled,10));
    CHECK(test_target_source_import(sampled)==0);
    const int scales[6][2]={{1,1},{5,4},{3,2},{7,4},{2,1},{3,1}};
    unsigned fill_pixels=0;
    for(unsigned scale=0;scale<42;scale++)for(unsigned rotation=0;rotation<(scale>=6?1u:4u);rotation++)for(unsigned variant=0;variant<(scale>=6?1u:3u);variant++){
        if(scale>=11){
            if(scale%2){
                VkImage previous=sampled->image;VkDeviceMemory backing=sampled->memory;
                CHECK(test_target_source_restart(sampled->width,sampled->height,sampled->format==VK_FORMAT_R8_UNORM)==0);
                CHECK(sampled->image==previous&&sampled->memory==backing);
            }else{
                CHECK(!sampled->image&&!sampled->memory&&sampled->stage==3);
                CHECK(test_target_source_configure(W-scale%3,H-scale%5,(scale/2)%2)==0);
                sampled=test_target_source_image();CHECK(sampled);
            }
            CHECK(!upload_rows(device,fence,upload,sampled,scale));
            CHECK(test_target_source_import(sampled)==0);
        }
        const int l=scale>=6&&scale<9?32:variant==0?1:variant==1?30:32,t=scale>=6&&scale<9?24:variant==0?1:variant==1?22:24;
        const int r=scale>=9?4:scale>=6?32:variant==0?7:variant==1?65535:32,b=scale>=9?3:scale>=6?24:variant==0?5:variant==1?65535:24;
        const int selected=scale>=10?test_target_textured_fill():scale==9?test_target_partial_fill():scale>=6?test_target_settle_fill():test_target_bundle_fill((int)rotation,scales[scale][0],scales[scale][1],l,t,r,b);
        CHECK(selected>=1&&selected<=3);const unsigned slot=(unsigned)selected-1;
        if(scale>=10){CHECK(test_target_source_release()==1);CHECK(sampled->image&&sampled->memory);CHECK(test_target_upload_close()==1);CHECK(upload->mapped&&upload->memory&&upload->buffer&&!test_target_upload_mapping());}
        VK(vkWaitForFences(device,1,&fence,VK_TRUE,5000000000ull));
        CHECK(test_target_bundle_finish()==0);
        VK(vkResetFences(device,1,&fence));VK(vkResetCommandBuffer(command,0));
        const VkCommandBufferBeginInfo begin={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,.flags=VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT};
        VK(vkBeginCommandBuffer(command,&begin));
        barrier(command,images[slot]->image,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
            VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,VK_ACCESS_TRANSFER_READ_BIT);
        const VkBufferImageCopy copy={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},.imageExtent={W,H,1}};
        vkCmdCopyImageToBuffer(command,images[slot]->image,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,readback,1,&copy);
        barrier(command,images[slot]->image,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
            VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
            VK_ACCESS_TRANSFER_READ_BIT,VK_ACCESS_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
        const VkMemoryBarrier host={.sType=VK_STRUCTURE_TYPE_MEMORY_BARRIER,.srcAccessMask=VK_ACCESS_TRANSFER_WRITE_BIT,.dstAccessMask=VK_ACCESS_HOST_READ_BIT};
        vkCmdPipelineBarrier(command,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_HOST_BIT,0,1,&host,0,NULL,0,NULL);
        CHECK(!submit(device,queue,command,fence));
        for(int y=0;y<H;y++)for(int x=0;x<W;x++){
            uint32_t texture=0xff204060;
            if(scale>=10){
                const unsigned sx=((2*(unsigned)x+1)*sampled->width)/(2*W);
                const unsigned sy=((2*(unsigned)y+1)*sampled->height)/(2*H);
                texture=sampled->format==VK_FORMAT_R8_UNORM?((sx+sy+scale)%2?0xffff0000:0xff204060):upload_color(sx,sy,scale);
            }
            const uint32_t color=x>=l&&x<r&&y>=t&&y<b?0xff00a0e0:scale>=10?texture:0xff204060;
            CHECK(((uint32_t *)pixels)[y*W+x]==color);++fill_pixels;
        }
        if(scale>=10){
            if(scale%2==0){CHECK(test_target_source_detach()==0);CHECK(sampled->image&&sampled->memory);}
            else {CHECK(test_target_source_release()==0);CHECK(!sampled->image&&!sampled->memory);}
        }
    }
    printf("HOST ONLY physical fills: 72 full scenes + 3 settle + retained partial, cancelled cold initialization, 6 scales/4 rotations, %u exact output pixels without a second transform PASS\n",fill_pixels);
    printf("HOST ONLY owned sampled source: 16 resized lifetimes +16 same-backing rewrites, actual provider/pipeline, %u exact textured pixels, pending release blocked, descriptor/backing retired PASS\n",32*W*H);
    CHECK(test_target_upload_close()==0);CHECK(!upload->mapped&&!upload->memory&&!upload->buffer);
    printf("HOST ONLY admitted-device coherent upload: 32 chunked patterned BGRA/R8 contents, publication/write gates, production transfer recording/submission, pending mapping/backing retained, final unmap/free/refund PASS\n");
    /* All GPU work completed, but a simulated display reader still holds front. */
    CHECK(test_target_bundle_close(1)==2);
    CHECK(test_target_context_close()==2);
    for(unsigned i=0;i<3;i++)CHECK(images[i]->image&&images[i]->memory&&views->views[i]&&views->framebuffers[i]);
    CHECK(test_target_bundle_close(0)==0);
    CHECK(cubit_vulkan_device_pipeline_close()==0);
    CHECK(cubit_vulkan_device_pipeline_close()==2);
    CHECK(test_target_context_close()==0);
    for(unsigned i=0;i<3;i++)CHECK(!images[i]->image&&!images[i]->memory&&!views->views[i]&&!views->framebuffers[i]);
    vkUnmapMemory(device,memory);vkDestroyBuffer(device,readback,NULL);vkFreeMemory(device,memory,NULL);
    printf("HOST ONLY admitted-view metadata/Ada FFI/owned targets: 3 independent allocations, %u exact RGB pixels, held front retains images/views, final retirement refunds budget PASS\n",3*W*H);
    return 0;
}
#endif
static int covered(const struct input *p,int x,int y,int *sx,int *sy)
{
    int64_t px=2*x+1,py=2*y+1,nx,ny;
    switch(p->rotation) {case 0:nx=px;ny=py;break;case 1:nx=py;ny=2*p->w-px;break;
        case 2:nx=2*p->w-px;ny=2*p->h-py;break;default:nx=2*p->h-py;ny=px;break;}
    const int64_t u=((int64_t)p->x-p->l)*2*p->n+nx*p->d;
    const int64_t v=((int64_t)p->y-p->t)*2*p->n+ny*p->d;
    const int64_t ud=((int64_t)p->r-p->l)*2*p->n,vd=((int64_t)p->b-p->t)*2*p->n;
    if(ud<=0||vd<=0||u<0||v<0||u>=ud||v>=vd||x<p->dl||y<p->dt||x>=p->dr||y>=p->db)return 0;
    *sx=(int)(u*W/ud);*sy=(int)(v*H/vd);return 1;
}
static uint32_t reference_over(uint32_t source,uint8_t coverage,const struct input *p,uint32_t background)
{
    uint32_t out=0;
    const double alpha=p->mask ? (double)(p->tint>>24)/255.0*coverage/255.0 : (double)(source>>24)/255.0;
    for(unsigned k=0;k<4;k++) {
        double value=p->mask ? (k==3 ? alpha*255.0 : ((p->tint>>(k*8))&255)*alpha) : (double)((source>>(k*8))&255);
        if(!p->mask&&p->over==2&&k<3)value*=alpha;
        if(p->mask||p->over)value+=((background>>(k*8))&255)*(1.0-alpha);
        unsigned byte=(unsigned)(value+0.5); if(byte>255)byte=255; out|=byte<<(k*8);
    }
    return out;
}
static uint32_t reference(uint32_t source,uint8_t coverage,const struct input *p)
{
    return reference_over(source,coverage,p,0xff204060u);
}
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
extern void test_submission_capture_begin(const struct input *);
extern int test_submission_capture(const struct input *),test_submission_capture_fill(const struct input *),test_submission_capture_end(void),test_submission_replay_scene(void);
extern int test_submission_capture_gradient(const struct input *,uint32_t);
#ifdef CUBIT_VULKAN_BACKDROP_SCENE_TEST
extern int test_submission_capture_backdrop(const struct input *,int);
#endif
extern int test_submission_capture_clip(const struct input *,int);
static unsigned scene_layers(unsigned frame,struct input layers[4])
{
    layers[0]=(struct input){W,H,1,1,0,0,0,0,0,W,H,0,0,W,H,1,0,0};
    layers[1]=layers[0];layers[2]=layers[0];
    layers[2].mask=1;layers[2].tint=0xa0e02080;
    layers[1].l=1+frame%17;layers[1].t=1+(frame*3)%12;
    layers[1].r=layers[1].l+8;layers[1].b=layers[1].t+7;
    layers[2].l=8+(frame*2)%16;layers[2].t=2+frame%10;
    layers[2].r=layers[2].l+7;layers[2].b=layers[2].t+8;
    if(frame%2){struct input swap=layers[1];layers[1]=layers[2];layers[2]=swap;}
    unsigned count=frame%5?3:2;
    for(unsigned i=count;i>1;i--)layers[i]=layers[i-1];
    layers[1]=layers[0];layers[1].l=2;layers[1].r=29;
    layers[1].t=2+frame%16;layers[1].b=layers[1].t+2;
    layers[1].mask=2;layers[1].over=0;layers[1].tint=0x305080;
    /* Alternate foreground icon alpha conventions; keep the undamaged
       background unchanged so sparse-damage accounting remains meaningful. */
    if(frame%2)for(unsigned i=1;i<=count;i++)if(layers[i].mask==0)layers[i].over=2;
    return count+1;
}
static int64_t floor_ratio(int64_t n,int64_t d)
{
    return n>=0?n/d:-((-n+d-1)/d);
}
/* Independent outward edge mapping, unlike texture pixel-centre sampling. */
static int solid_covered(const struct input *p,int x,int y)
{
    if(p->l>=p->r||p->t>=p->b)return 0;
    int64_t l=floor_ratio((int64_t)(p->l-p->x)*p->n,p->d);
    int64_t t=floor_ratio((int64_t)(p->t-p->y)*p->n,p->d);
    int64_t r=-floor_ratio(-(int64_t)(p->r-p->x)*p->n,p->d);
    int64_t b=-floor_ratio(-(int64_t)(p->b-p->y)*p->n,p->d);
    int64_t left=l,top=t,right=r,bottom=b;
    switch(p->rotation){
    case 1:left=p->w-b;top=l;right=p->w-t;bottom=r;break;
    case 2:left=p->w-r;top=p->h-b;right=p->w-l;bottom=p->h-t;break;
    case 3:left=t;top=p->h-r;right=b;bottom=p->h-l;break;
    default:break;
    }
    return x>=left&&x<right&&y>=top&&y<bottom;
}
/* Independent pixel-space glyph oracle. Raster texels are never rescaled. */
static int glyph_covered(const struct input *p,int x,int y,int *sx,int *sy)
{
    int ux=x,uy=y;
    switch(p->rotation){
    case 1:ux=y;uy=p->w-1-x;break;
    case 2:ux=p->w-1-x;uy=p->h-1-y;break;
    case 3:ux=p->h-1-y;uy=x;break;
    default:break;
    }
    int64_t ox=floor_ratio(2*(int64_t)(p->l-p->x)*p->n+p->d,2*p->d);
    int64_t oy=floor_ratio(2*(int64_t)(p->t-p->y)*p->n+p->d,2*p->d);
    *sx=ux-(int)ox;*sy=uy-(int)oy;
    /* Text cells use half-open pixel-centre inclusion, unlike outward damage.
     * Evaluate that rule directly in logical units, independently of Ada edges. */
    int64_t cx=(2*(int64_t)ux+1)*p->d,cy=(2*(int64_t)uy+1)*p->d;
    return cx>=2*(int64_t)(p->l-p->x)*p->n&&cx<2*(int64_t)(p->r-p->x)*p->n&&
        cy>=2*(int64_t)(p->t-p->y)*p->n&&cy<2*(int64_t)(p->b-p->y)*p->n&&
        x>=p->dl&&x<p->dr&&y>=p->dt&&y<p->db&&
        *sx>=0&&*sy>=0&&*sx<(32*p->n+p->d-1)/p->d&&*sy<(17*p->n+p->d-1)/p->d;
}
static int layer_covered(const struct input *p,int x,int y,int *sx,int *sy)
{return p->mask==4?glyph_covered(p,x,y,sx,sy):covered(p,x,y,sx,sy);}
/* Independent viewport oracle: complete source geometry remains untouched. */
static int scene_clip(unsigned test,unsigned layer,unsigned count,const struct input *screen,struct input *clip)
{
    *clip=*screen;
    if(test<472)return 0;
    if(test>=616){
        unsigned variant=(test-616)/24;
        if(variant==0||variant==3)return 0;
        clip->l=screen->x+2;clip->t=screen->y+2;clip->r=screen->x+8;clip->b=screen->y+9;
        if(variant==2)clip->r=clip->l;
        return 1;
    }
    unsigned variant=(test-472)/24;
    if(variant==4&&layer+1==count)return 0;
    clip->l=screen->x+2;clip->t=screen->y+1;clip->r=screen->x+9;clip->b=screen->y+7;
    if(variant==1)clip->r=clip->l;
    if(variant==2){clip->l=screen->x+9;clip->r=screen->x+2;}
    if(variant==3){clip->l=screen->x+1000;clip->r=screen->x+1002;}
    if(variant==5&&layer+1==count){clip->l=screen->x+4;clip->t=screen->y+3;clip->r=screen->x+12;clip->b=screen->y+9;}
    return 1;
}
static int clip_contains(unsigned test,unsigned layer,unsigned count,const struct input *screen,int x,int y)
{
    struct input clip;
    return !scene_clip(test,layer,count,screen,&clip)||solid_covered(&clip,x,y);
}
#endif
static const char *hidden;
static PFN_vkVoidFunction VKAPI_CALL hide_proc(VkDevice d,const char *name)
{
    return !strcmp(name,hidden) ? NULL : vkGetDeviceProcAddr(d,name);
}
static int fault_stage,creates;
static VkResult VKAPI_CALL fail_shader(VkDevice d,const VkShaderModuleCreateInfo *info,const VkAllocationCallbacks *a,VkShaderModule *out)
{
    if(++creates==2){*out=VK_NULL_HANDLE;return VK_ERROR_OUT_OF_DEVICE_MEMORY;}
    return vkCreateShaderModule(d,info,a,out);
}
static VkResult VKAPI_CALL fail_pipeline(VkDevice d,VkPipelineCache cache,uint32_t count,const VkGraphicsPipelineCreateInfo *info,const VkAllocationCallbacks *a,VkPipeline *out)
{
    if(++creates==fault_stage){*out=VK_NULL_HANDLE;return VK_ERROR_OUT_OF_DEVICE_MEMORY;}
    return vkCreateGraphicsPipelines(d,cache,count,info,a,out);
}
static PFN_vkVoidFunction VKAPI_CALL fail_proc(VkDevice d,const char *name)
{
    if(fault_stage==1&&!strcmp(name,"vkCreateShaderModule"))return (PFN_vkVoidFunction)fail_shader;
    if(fault_stage>=2&&!strcmp(name,"vkCreateGraphicsPipelines"))return (PFN_vkVoidFunction)fail_pipeline;
    return vkGetDeviceProcAddr(d,name);
}
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
static unsigned target_step,target_fail,target_views_destroyed,target_frames_destroyed;
static int target_lie;
static VkResult VKAPI_CALL target_create_view(VkDevice d,const VkImageViewCreateInfo *i,const VkAllocationCallbacks *a,VkImageView *v)
{
    if(++target_step==target_fail){*v=VK_NULL_HANDLE;return target_lie?VK_SUCCESS:VK_ERROR_OUT_OF_DEVICE_MEMORY;}
    return vkCreateImageView(d,i,a,v);
}
static VkResult VKAPI_CALL target_create_frame(VkDevice d,const VkFramebufferCreateInfo *i,const VkAllocationCallbacks *a,VkFramebuffer *f)
{
    if(++target_step==target_fail){*f=VK_NULL_HANDLE;return target_lie?VK_SUCCESS:VK_ERROR_OUT_OF_DEVICE_MEMORY;}
    return vkCreateFramebuffer(d,i,a,f);
}
static void VKAPI_CALL target_destroy_view(VkDevice d,VkImageView v,const VkAllocationCallbacks *a)
{++target_views_destroyed;vkDestroyImageView(d,v,a);}
static void VKAPI_CALL target_destroy_frame(VkDevice d,VkFramebuffer f,const VkAllocationCallbacks *a)
{++target_frames_destroyed;vkDestroyFramebuffer(d,f,a);}
static PFN_vkVoidFunction VKAPI_CALL target_proc(VkDevice d,const char *name)
{
    if(!strcmp(name,"vkCreateImageView"))return (PFN_vkVoidFunction)target_create_view;
    if(!strcmp(name,"vkCreateFramebuffer"))return (PFN_vkVoidFunction)target_create_frame;
    if(!strcmp(name,"vkDestroyImageView"))return (PFN_vkVoidFunction)target_destroy_view;
    if(!strcmp(name,"vkDestroyFramebuffer"))return (PFN_vkVoidFunction)target_destroy_frame;
    return vkGetDeviceProcAddr(d,name);
}
static int target_tests(VkDevice device,VkRenderPass pass,const VkImage *images)
{
    struct cubit_vulkan_targets set={0};
    struct cubit_vulkan_target_request base={.fresh=&set,.device=device,.proc=target_proc,.pass=pass,
        .images={images[0],images[1],images[2]},.width=W,.height=H};
    for(target_fail=1;target_fail<=6;target_fail++){
        target_step=0;target_views_destroyed=0;target_frames_destroyed=0;target_lie=0;
        void *a=NULL,*b=NULL,*c=NULL;
        CHECK(cubit_vulkan_targets_create(&base,&a,&b,&c)==1&&!a&&!b&&!c);
        CHECK(target_step==target_fail);
        CHECK(target_views_destroyed==target_fail/2&&target_frames_destroyed==(target_fail-1)/2);
        for(unsigned i=0;i<3;i++)CHECK(!set.views[i]&&!set.framebuffers[i]);
    }
    target_fail=4;target_step=0;target_lie=1;target_views_destroyed=target_frames_destroyed=0;
    void *a=NULL,*b=NULL,*c=NULL;
    CHECK(cubit_vulkan_targets_create(&base,&a,&b,&c)==2&&!a&&!b&&!c);
    CHECK(set.views[0]&&set.views[1]&&set.framebuffers[0]&&!set.framebuffers[1]);
    CHECK(target_views_destroyed==0&&target_frames_destroyed==0);
    /* The lying callback created nothing at the failing call. Only this fixture
     * knows that cleanup is safe; production SPARK quarantines without replay. */
    CHECK(cubit_vulkan_targets_release(&base)==0);
    target_lie=0;target_fail=0;target_step=0;
    const char *missing[]={"vkCreateImageView","vkDestroyImageView","vkCreateFramebuffer","vkDestroyFramebuffer"};
    for(unsigned i=0;i<4;i++){
        struct cubit_vulkan_target_request r=base;r.proc=hide_proc;hidden=missing[i];
        CHECK(cubit_vulkan_targets_create(&r,&a,&b,&c)==1&&!a&&!b&&!c);
    }
    for(unsigned fault=0;fault<12;fault++){
        struct cubit_vulkan_target_request r=base;void *description=&r;
        switch(fault){
        case 0:description=NULL;break;case 1:r.fresh=NULL;break;case 2:r.device=VK_NULL_HANDLE;break;
        case 3:r.proc=NULL;break;case 4:r.pass=VK_NULL_HANDLE;break;case 5:r.width=0;break;
        case 6:r.height=0;break;case 7:r.width=65536;break;case 8:r.height=65536;break;
        case 9:r.images[0]=VK_NULL_HANDLE;break;case 10:r.images[1]=r.images[0];break;case 11:r.images[2]=r.images[0];break;
        }
        CHECK(cubit_vulkan_targets_create(description,&a,&b,&c)==1&&!a&&!b&&!c);
        CHECK(target_step==0);
    }
    CHECK(cubit_vulkan_targets_create(&base,NULL,&b,&c)==1);
    CHECK(cubit_vulkan_targets_create(&base,&a,NULL,&c)==1);
    CHECK(cubit_vulkan_targets_create(&base,&a,&b,NULL)==1);
    CHECK(target_step==0);
    printf("HOST ONLY target provider: 6 partial-create rollbacks, unknown retention, 4 missing dispatch, 15 malformed requests PASS; metadata=%zu bytes\n",sizeof set);
    return 0;
}
static unsigned provider_fault,destroyed_pools;
static VkResult VKAPI_CALL fail_descriptor_pool(VkDevice d,const VkDescriptorPoolCreateInfo *i,
    const VkAllocationCallbacks *a,VkDescriptorPool *p)
{(void)d;(void)i;(void)a;*p=VK_NULL_HANDLE;return VK_ERROR_OUT_OF_HOST_MEMORY;}
static VkResult VKAPI_CALL fail_descriptor_sets(VkDevice d,const VkDescriptorSetAllocateInfo *i,VkDescriptorSet *sets)
{(void)d;(void)i;(void)sets;return VK_ERROR_OUT_OF_POOL_MEMORY;}
static VkResult VKAPI_CALL fail_source_view(VkDevice d,const VkImageViewCreateInfo *i,
    const VkAllocationCallbacks *a,VkImageView *view)
{(void)d;(void)i;(void)a;*view=VK_NULL_HANDLE;return provider_fault==4?VK_SUCCESS:VK_ERROR_OUT_OF_HOST_MEMORY;}
static void VKAPI_CALL count_destroy_pool(VkDevice d,VkDescriptorPool pool,const VkAllocationCallbacks *a)
{++destroyed_pools;vkDestroyDescriptorPool(d,pool,a);}
static PFN_vkVoidFunction VKAPI_CALL provider_proc(VkDevice d,const char *name)
{
    if(!strcmp(name,"vkDestroyDescriptorPool"))return (PFN_vkVoidFunction)count_destroy_pool;
    if(provider_fault==1&&!strcmp(name,"vkCreateDescriptorPool"))return (PFN_vkVoidFunction)fail_descriptor_pool;
    if(provider_fault==2&&!strcmp(name,"vkAllocateDescriptorSets"))return (PFN_vkVoidFunction)fail_descriptor_sets;
    if(provider_fault>=3&&!strcmp(name,"vkCreateImageView"))return (PFN_vkVoidFunction)fail_source_view;
    return vkGetDeviceProcAddr(d,name);
}
static int provider_tests(const struct cubit_vulkan_affine_engine *engine,VkCommandBuffer command,VkImage image)
{
    const char *missing[]={"vkCreateDescriptorPool","vkDestroyDescriptorPool","vkAllocateDescriptorSets",
        "vkCreateImageView","vkDestroyImageView","vkUpdateDescriptorSets"};
    for(unsigned i=0;i<sizeof missing/sizeof *missing;i++){
        struct cubit_vulkan_sources s;hidden=missing[i];
        CHECK(cubit_vulkan_sources_init(&s,engine,command,hide_proc)==VK_ERROR_INITIALIZATION_FAILED);
        CHECK(!s.pool);CHECK(cubit_vulkan_sources_destroy(&s)==0);
    }
    for(provider_fault=1;provider_fault<=4;provider_fault++){
        struct cubit_vulkan_sources s;destroyed_pools=0;
        const VkResult result=cubit_vulkan_sources_init(&s,engine,command,provider_proc);
        if(provider_fault<=2){
            CHECK(result!=VK_SUCCESS&&!s.pool);
            CHECK(destroyed_pools==(provider_fault==2?1u:0u));
        }else{
            CHECK(result==VK_SUCCESS);
            struct cubit_vulkan_source_request r={&s,0,image,VK_FORMAT_B8G8R8A8_UNORM,W,H};
            void *draw=NULL;
            CHECK(cubit_vulkan_source_import(&r,&draw)==(provider_fault==3?1u:2u));
            CHECK(!draw&&!s.entries[0].view);
        }
        /* Fault4 is a test-only lying Vulkan callback with no actual created
         * object. Production SPARK quarantines that outcome instead. */
        CHECK(cubit_vulkan_sources_destroy(&s)==0);
    }
    struct cubit_vulkan_sources s;VK(cubit_vulkan_sources_init(&s,engine,command,vkGetDeviceProcAddr));
    struct cubit_vulkan_source_request request={&s,0,image,VK_FORMAT_B8G8R8A8_UNORM,W,H};
    for(unsigned fault=0;fault<10;fault++){
        struct cubit_vulkan_source_request r=request;void *draw=NULL,*description=&r;
        switch(fault){
        case 0:description=NULL;break;case 1:r.provider=NULL;break;
        case 2:r.slot=CUBIT_VULKAN_SOURCE_CAPACITY;break;case 3:r.image=VK_NULL_HANDLE;break;
        case 4:r.format=VK_FORMAT_UNDEFINED;break;case 5:r.output_width=0;break;
        case 6:r.output_height=0;break;case 7:r.output_width=65536;break;
        case 8:r.output_height=65536;break;case 9:r.slot=UINT32_MAX;break;
        }
        CHECK(cubit_vulkan_source_import(description,&draw)==1&&!draw);
    }
    CHECK(cubit_vulkan_source_import(&request,NULL)==1);
    for(unsigned i=0;i<CUBIT_VULKAN_SOURCE_CAPACITY;i++){
        void *draw=NULL;request.slot=i;
        CHECK(cubit_vulkan_source_import(&request,&draw)==0&&draw==&s.entries[i].draw);
        CHECK(cubit_vulkan_sources_destroy(&s)==2);
        CHECK(cubit_vulkan_source_import(&request,&draw)==1&&!draw);
    }
    for(unsigned i=0;i<CUBIT_VULKAN_SOURCE_CAPACITY;i++){
        CHECK(cubit_vulkan_source_release(&s.entries[i].draw)==0);
        CHECK(cubit_vulkan_source_release(&s.entries[i].draw)==2);
    }
    CHECK(cubit_vulkan_sources_destroy(&s)==0);
    printf("HOST ONLY source provider: 140 slots/occupied/double-release, 6 missing dispatch, 4 allocation/lying-result faults, 11 malformed requests PASS; metadata=%zu bytes\n",sizeof s);
    return 0;
}
static unsigned scene_calls;
static void VKAPI_CALL observe_begin(VkCommandBuffer command,
    const VkRenderPassBeginInfo *info,VkSubpassContents contents)
{
    (void)command; (void)info; (void)contents; ++scene_calls;
}
static void VKAPI_CALL observe_end(VkCommandBuffer command)
{
    (void)command; ++scene_calls;
}
static void VKAPI_CALL observe_fill(VkCommandBuffer command,uint32_t count,const VkClearAttachment *clear,
    uint32_t rect_count,const VkClearRect *rect)
{
    (void)command;(void)count;(void)clear;(void)rect_count;(void)rect;++scene_calls;
}
/* Exercise the real foreign adapter with valid borrowed storage but malformed
 * fields. Observers prove rejection occurs before any Vulkan command escapes.
 * The main oracle below separately executes accepted passes on actual Mesa. */
static int submission_guards(const struct cubit_vulkan_submission *valid,
    const struct cubit_vulkan_scene *scene)
{
    const char *missing_control[]={"vkResetFences","vkResetCommandBuffer",
        "vkBeginCommandBuffer","vkEndCommandBuffer","vkQueueSubmit",
        "vkGetFenceStatus","vkCmdBeginRenderPass","vkCmdEndRenderPass","vkCmdClearAttachments"};
    for(unsigned i=0;i<sizeof missing_control/sizeof *missing_control;i++) {
        struct cubit_vulkan_submission rejected;
        hidden=missing_control[i];
        CHECK(cubit_vulkan_submission_init(&rejected,valid->device,valid->queue,
            valid->command,valid->fence,hide_proc)==2);
    }
    for(unsigned fault=0;fault<22;fault++) {
        struct cubit_vulkan_submission s=*valid;
        struct cubit_vulkan_scene p=*scene;
        void *context=&s,*pass=&p; uint32_t width=W,height=H;
        s.begin_scene=observe_begin; scene_calls=0;
        switch(fault) {
        case 0:context=NULL;break; case 1:pass=NULL;break;
        case 2:s.device=VK_NULL_HANDLE;break;
        case 3:p.device=VK_NULL_HANDLE;break;
        case 4:s.command=VK_NULL_HANDLE;break;
        case 5:s.begin_scene=NULL;break;
        case 6:p.begin.sType=VK_STRUCTURE_TYPE_APPLICATION_INFO;break;
        case 7:p.begin.pNext=&p;break;
        case 8:p.begin.renderPass=VK_NULL_HANDLE;break;
        case 9:p.begin.framebuffer=VK_NULL_HANDLE;break;
        case 10:width=0;break; case 11:width=65536;break;
        case 12:height=0;break; case 13:height=65536;break;
        case 14:p.begin.renderArea.offset.x=1;break;
        case 15:p.begin.renderArea.offset.y=-1;break;
        case 16:p.begin.renderArea.extent.width=W+1;break;
        case 17:p.begin.renderArea.extent.height=H+1;break;
        case 18:p.begin.clearValueCount=2;break;
        case 19:p.begin.pClearValues=NULL;break;
        case 20:width=UINT32_MAX;break; case 21:height=UINT32_MAX;break;
        }
        CHECK(cubit_vulkan_submission_begin_scene(context,pass,width,height)==2);
        CHECK(scene_calls==0);
    }
    for(unsigned fault=0;fault<4;fault++) {
        struct cubit_vulkan_submission s=*valid;void *context=&s;
        s.end_scene=observe_end;scene_calls=0;
        switch(fault) {
        case 0:context=NULL;break; case 1:s.command=VK_NULL_HANDLE;break;
        case 2:s.end_scene=NULL;break; case 3:break;
        }
        CHECK(cubit_vulkan_submission_end_scene(context)==(fault==3?0:2));
        CHECK(scene_calls==(fault==3?1:0));
    }
    struct cubit_vulkan_submission s=*valid;
    struct cubit_vulkan_scene p=*scene;s.begin_scene=observe_begin;
    /* Both supported clear descriptions must reach the callback once. */
    for(unsigned count=0;count<=1;count++) {
        p.begin.clearValueCount=count;scene_calls=0;
        CHECK(cubit_vulkan_submission_begin_scene(&s,&p,W,H)==0);
        CHECK(scene_calls==1);
    }
    for(unsigned fault=0;fault<14;fault++){
        struct cubit_vulkan_submission fill=*valid;void *context=&fill;
        uint32_t w=W,h=H,l=0,t=0,r=W,b=H;
        fill.fill=observe_fill;scene_calls=0;
        switch(fault){
        case 0:context=NULL;break;case 1:fill.command=VK_NULL_HANDLE;break;
        case 2:fill.fill=NULL;break;case 3:w=0;break;case 4:w=65536;break;
        case 5:h=0;break;case 6:h=65536;break;case 7:l=r;break;
        case 8:t=b;break;case 9:r=W+1;break;case 10:b=H+1;break;
        case 11:l=UINT32_MAX;break;case 12:b=UINT32_MAX;break;case 13:break;
        }
        CHECK(cubit_vulkan_submission_fill(context,w,h,l,t,r,b,0x204060)==(fault==13?0:2));
        CHECK(scene_calls==(fault==13?1:0));
    }
    printf("HOST ONLY fill guards: 13 malformed contexts/rectangles and one accepted fill PASS\n");
    printf("HOST ONLY submission guards: 9 missing dispatch, 22 invalid pass descriptions, 3 invalid end contexts and 3 accepted controls PASS\n");
    return 0;
}
#endif
#ifdef CUBIT_VULKAN_BACKDROP_TEST
#include "vulkan_backdrop_host.h"
#endif
int run_vulkan_affine_tests(void)
{
    const char *layers[]={"VK_LAYER_KHRONOS_validation"};
    const char *extensions[]={VK_EXT_DEBUG_UTILS_EXTENSION_NAME,VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME};
    const VkValidationFeatureEnableEXT sync=VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT;
    const VkValidationFeaturesEXT validation={.sType=VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT,.enabledValidationFeatureCount=1,.pEnabledValidationFeatures=&sync};
    VkDebugUtilsMessengerCreateInfoEXT debug={.sType=VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,.pNext=&validation,
        .messageSeverity=VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
        .messageType=VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT|VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT,
        .pfnUserCallback=diagnostic};
    const VkApplicationInfo app={.sType=VK_STRUCTURE_TYPE_APPLICATION_INFO,.apiVersion=VK_API_VERSION_1_1};
    const VkInstanceCreateInfo create={.sType=VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,.pNext=&debug,.pApplicationInfo=&app,
        .enabledLayerCount=1,.ppEnabledLayerNames=layers,.enabledExtensionCount=2,.ppEnabledExtensionNames=extensions};
    VkInstance inst; VK(vkCreateInstance(&create,NULL,&inst));
#ifdef CUBIT_VULKAN_OWNED_IMAGE_TEST
    owned_instance=inst;
#endif
    PFN_vkCreateDebugUtilsMessengerEXT make_debug=(PFN_vkCreateDebugUtilsMessengerEXT)vkGetInstanceProcAddr(inst,"vkCreateDebugUtilsMessengerEXT");
    PFN_vkDestroyDebugUtilsMessengerEXT free_debug=(PFN_vkDestroyDebugUtilsMessengerEXT)vkGetInstanceProcAddr(inst,"vkDestroyDebugUtilsMessengerEXT");
    CHECK(make_debug && free_debug); debug.pNext=NULL;
    VkDebugUtilsMessengerEXT messenger; VK(make_debug(inst,&debug,NULL,&messenger));
    uint32_t count=1; VkPhysicalDevice phy; VK(vkEnumeratePhysicalDevices(inst,&count,&phy)); CHECK(count==1);
    VkPhysicalDeviceProperties properties; vkGetPhysicalDeviceProperties(phy,&properties);
    printf("HOST ONLY Vulkan affine device: %s\n",properties.deviceName); CHECK(properties.deviceType==VK_PHYSICAL_DEVICE_TYPE_CPU);
    uint32_t n=0; vkGetPhysicalDeviceQueueFamilyProperties(phy,&n,NULL); CHECK(n>0);
    VkQueueFamilyProperties *families=calloc(n,sizeof(*families)); CHECK(families);
    vkGetPhysicalDeviceQueueFamilyProperties(phy,&n,families); uint32_t family=UINT32_MAX;
    for(uint32_t j=0;j<n;j++) if(families[j].queueFlags&VK_QUEUE_GRAPHICS_BIT) {family=j;break;}
    free(families); CHECK(family!=UINT32_MAX);
    const float priority=1;
    const VkDeviceQueueCreateInfo qi={.sType=VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,.queueFamilyIndex=family,.queueCount=1,.pQueuePriorities=&priority};
    const VkDeviceCreateInfo di={.sType=VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,.queueCreateInfoCount=1,.pQueueCreateInfos=&qi};
    VkDevice d; VK(vkCreateDevice(phy,&di,NULL,&d)); VkQueue queue; vkGetDeviceQueue(d,family,0,&queue);
    VkImage source,mask,target,targets[OUTPUTS]; VkDeviceMemory sm,mm,um,cm,target_memory[OUTPUTS],readback_memory[OUTPUTS];
    VkBuffer upload,coverage,readback,readbacks[OUTPUTS]; void *up,*cp,*rp,*results[OUTPUTS];
    CHECK(!image(phy,d,&source,&sm,VK_FORMAT_B8G8R8A8_UNORM,VK_IMAGE_USAGE_SAMPLED_BIT|VK_IMAGE_USAGE_TRANSFER_DST_BIT));
    CHECK(!image(phy,d,&mask,&mm,VK_FORMAT_R8_UNORM,VK_IMAGE_USAGE_SAMPLED_BIT|VK_IMAGE_USAGE_TRANSFER_DST_BIT));
    for(unsigned i=0;i<OUTPUTS;i++){
        CHECK(!image(phy,d,&targets[i],&target_memory[i],VK_FORMAT_B8G8R8A8_UNORM,VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT|VK_IMAGE_USAGE_TRANSFER_SRC_BIT));
        CHECK(!buffer(phy,d,&readbacks[i],&readback_memory[i],&results[i]));
    }
    target=targets[0];readback=readbacks[0];rp=results[0];
    CHECK(!buffer(phy,d,&upload,&um,&up) && !buffer(phy,d,&coverage,&cm,&cp));
    uint32_t *pixels=up; uint8_t *masks=cp;
    for(unsigned y=0;y<H;y++) for(unsigned x=0;x<W;x++) {
        pixels[y*W+x]=((128+(x+y)%128)<<24)|((x+y)<<16)|(y*2<<8)|(x*2);
        masks[y*W+x]=(uint8_t)((x*31+y*47)%256);
    }
    const VkCommandPoolCreateInfo pi={.sType=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,.flags=VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,.queueFamilyIndex=family};
    VkCommandPool pool; VK(vkCreateCommandPool(d,&pi,NULL,&pool));
    const VkCommandBufferAllocateInfo ai={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,.commandPool=pool,.level=VK_COMMAND_BUFFER_LEVEL_PRIMARY,.commandBufferCount=1};
    VkCommandBuffer cmd; VK(vkAllocateCommandBuffers(d,&ai,&cmd));
    const VkFenceCreateInfo fi={.sType=VK_STRUCTURE_TYPE_FENCE_CREATE_INFO}; VkFence fence; VK(vkCreateFence(d,&fi,NULL,&fence));
    const VkCommandBufferBeginInfo bi={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,.flags=VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT};
    const VkBufferImageCopy whole={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},.imageExtent={W,H,1}};
    VK(vkBeginCommandBuffer(cmd,&bi));
    const VkImage images[2]={source,mask};const VkBuffer buffers[2]={upload,coverage};
    for(unsigned i=0;i<2;i++) {
        barrier(cmd,images[i],VK_IMAGE_LAYOUT_UNDEFINED,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
            VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,VK_PIPELINE_STAGE_TRANSFER_BIT,0,VK_ACCESS_TRANSFER_WRITE_BIT);
        vkCmdCopyBufferToImage(cmd,buffers[i],images[i],VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,1,&whole);
        barrier(cmd,images[i],VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
            VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,VK_ACCESS_TRANSFER_WRITE_BIT,VK_ACCESS_SHADER_READ_BIT);
    }
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
    VkImage glyph_images[6];VkDeviceMemory glyph_memory[6],glyph_upload_memory[6];
    VkBuffer glyph_upload[6];void *glyph_pixels[6];uint32_t glyph_width[6],glyph_height[6];
    const int glyph_scales[6][2]={{1,1},{5,4},{3,2},{2,1},{3,1},{2,3}};
    for(unsigned i=0;i<6;i++){
        glyph_width[i]=(32*glyph_scales[i][0]+glyph_scales[i][1]-1)/glyph_scales[i][1];
        glyph_height[i]=(17*glyph_scales[i][0]+glyph_scales[i][1]-1)/glyph_scales[i][1];
        CHECK(!image_size(phy,d,&glyph_images[i],&glyph_memory[i],VK_FORMAT_R8_UNORM,
            VK_IMAGE_USAGE_SAMPLED_BIT|VK_IMAGE_USAGE_TRANSFER_DST_BIT,glyph_width[i],glyph_height[i]));
        CHECK(!buffer_size(phy,d,&glyph_upload[i],&glyph_upload_memory[i],&glyph_pixels[i],glyph_width[i]*glyph_height[i]));
        for(unsigned y=0;y<glyph_height[i];y++)for(unsigned x=0;x<glyph_width[i];x++)
            ((uint8_t *)glyph_pixels[i])[y*glyph_width[i]+x]=(uint8_t)((x*31+y*47)%256);
        const VkBufferImageCopy copy={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},
            .imageExtent={glyph_width[i],glyph_height[i],1}};
        barrier(cmd,glyph_images[i],VK_IMAGE_LAYOUT_UNDEFINED,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
            VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,VK_PIPELINE_STAGE_TRANSFER_BIT,0,VK_ACCESS_TRANSFER_WRITE_BIT);
        vkCmdCopyBufferToImage(cmd,glyph_upload[i],glyph_images[i],VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,1,&copy);
        barrier(cmd,glyph_images[i],VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
            VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,VK_ACCESS_TRANSFER_WRITE_BIT,VK_ACCESS_SHADER_READ_BIT);
    }
    for(unsigned i=0;i<OUTPUTS;i++)barrier(cmd,targets[i],VK_IMAGE_LAYOUT_UNDEFINED,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
        VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,0,VK_ACCESS_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
#endif
    CHECK(!submit(d,queue,cmd,fence));
    VkAttachmentDescription attachment={.format=VK_FORMAT_B8G8R8A8_UNORM,.samples=VK_SAMPLE_COUNT_1_BIT,
        .loadOp=VK_ATTACHMENT_LOAD_OP_CLEAR,.storeOp=VK_ATTACHMENT_STORE_OP_STORE,.stencilLoadOp=VK_ATTACHMENT_LOAD_OP_DONT_CARE,
        .stencilStoreOp=VK_ATTACHMENT_STORE_OP_DONT_CARE,.initialLayout=VK_IMAGE_LAYOUT_UNDEFINED,.finalLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL};
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
    attachment.loadOp=VK_ATTACHMENT_LOAD_OP_LOAD;attachment.initialLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
#endif
    const VkAttachmentReference color={0,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL};
    const VkSubpassDescription subpass={.pipelineBindPoint=VK_PIPELINE_BIND_POINT_GRAPHICS,.colorAttachmentCount=1,.pColorAttachments=&color};
    const VkSubpassDependency dependency={.srcSubpass=VK_SUBPASS_EXTERNAL,.dstSubpass=0,
        .srcStageMask=VK_PIPELINE_STAGE_TRANSFER_BIT,.dstStageMask=VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        .srcAccessMask=VK_ACCESS_TRANSFER_READ_BIT,.dstAccessMask=VK_ACCESS_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT};
    const VkRenderPassCreateInfo pci={.sType=VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO,.attachmentCount=1,.pAttachments=&attachment,
        .subpassCount=1,.pSubpasses=&subpass,.dependencyCount=1,.pDependencies=&dependency};
    VkRenderPass pass;VK(vkCreateRenderPass(d,&pci,NULL,&pass));
#ifdef CUBIT_VULKAN_OWNED_TARGET_TEST
    CHECK(owned_target_pixels(inst,phy,d,pass,cmd,queue,fence,family)==0);
#endif
#ifdef CUBIT_DESKTOP_REAL_TEST
    CHECK(run_desktop_real(inst,phy,d,queue,family)==0);
#endif
    VkImageView views[2+OUTPUTS]; VkImage all_images[2+OUTPUTS]={source,mask};
    for(unsigned i=0;i<OUTPUTS;i++)all_images[2+i]=targets[i];
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
    const unsigned manual_views=2;
#else
    const unsigned manual_views=2+OUTPUTS;
#endif
    for(unsigned i=0;i<manual_views;i++) {
        const VkImageViewCreateInfo v={.sType=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,.image=all_images[i],.viewType=VK_IMAGE_VIEW_TYPE_2D,
            .format=i==1?VK_FORMAT_R8_UNORM:VK_FORMAT_B8G8R8A8_UNORM,.subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
        VK(vkCreateImageView(d,&v,NULL,&views[i]));
    }
    VkFramebuffer framebuffers[OUTPUTS],framebuffer;
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
    CHECK(target_tests(d,pass,targets)==0);
    struct cubit_vulkan_targets target_set={0};
    const struct cubit_vulkan_target_request target_request={.fresh=&target_set,.device=d,.proc=vkGetDeviceProcAddr,
        .pass=pass,.images={targets[0],targets[1],targets[2]},.width=W,.height=H,
        .clear={.color={{32.0f/255,64.0f/255,96.0f/255,1}}}};
    CHECK(test_submission_initialize_targets((void *)&target_request)==0);
    for(unsigned i=0;i<OUTPUTS;i++){
        framebuffers[i]=target_set.framebuffers[i];views[2+i]=target_set.views[i];
    }
#else
    for(unsigned i=0;i<OUTPUTS;i++){
        const VkFramebufferCreateInfo fbi={.sType=VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO,.renderPass=pass,
            .attachmentCount=1,.pAttachments=&views[2+i],.width=W,.height=H,.layers=1};
        VK(vkCreateFramebuffer(d,&fbi,NULL,&framebuffers[i]));
    }
#endif
    framebuffer=framebuffers[0];
    const char *missing[]={"vkDestroyDescriptorSetLayout","vkDestroyPipelineLayout","vkDestroyPipeline","vkDestroySampler",
        "vkCmdBindPipeline","vkCmdBindDescriptorSets","vkCmdSetViewport","vkCmdSetScissor","vkCmdPushConstants","vkCmdDraw",
        "vkCreateDescriptorSetLayout","vkCreatePipelineLayout","vkCreateSampler","vkCreateShaderModule","vkDestroyShaderModule","vkCreateGraphicsPipelines"};
    for(unsigned i=0;i<sizeof missing/sizeof *missing;i++) {
        struct cubit_vulkan_affine_engine rejected;
        hidden=missing[i];CHECK(cubit_vulkan_affine_create(&rejected,d,hide_proc,pass)==VK_ERROR_INITIALIZATION_FAILED);
        CHECK(!rejected.pipeline[0]&&!rejected.pipeline[1]&&!rejected.pipeline[2]&&!rejected.layout&&!rejected.descriptors&&!rejected.sampler);
        cubit_vulkan_affine_destroy(&rejected);
    }
    for(fault_stage=1;fault_stage<=3;fault_stage++) {
        struct cubit_vulkan_affine_engine rejected;creates=0;
        CHECK(cubit_vulkan_affine_create(&rejected,d,fail_proc,pass)==VK_ERROR_OUT_OF_DEVICE_MEMORY);
        CHECK(creates==(fault_stage==1?2:fault_stage)&&!rejected.pipeline[0]&&!rejected.pipeline[1]&&!rejected.pipeline[2]&&!rejected.layout&&!rejected.descriptors&&!rejected.sampler);
    }
    struct cubit_vulkan_affine_engine engine;
    VK(cubit_vulkan_affine_create(&engine,d,vkGetDeviceProcAddr,pass)); engine.draw=counted_draw;
    const VkDescriptorPoolSize size={VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,2};
    const VkDescriptorPoolCreateInfo dpi={.sType=VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,.maxSets=2,.poolSizeCount=1,.pPoolSizes=&size};
    VkDescriptorPool dp;VK(vkCreateDescriptorPool(d,&dpi,NULL,&dp));
    const VkDescriptorSetLayout layouts[2]={engine.descriptors,engine.descriptors};
    const VkDescriptorSetAllocateInfo dai={.sType=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,.descriptorPool=dp,.descriptorSetCount=2,.pSetLayouts=layouts};
    VkDescriptorSet descriptors[2];VK(vkAllocateDescriptorSets(d,&dai,descriptors));
    for(unsigned i=0;i<2;i++) {
        const VkDescriptorImageInfo dii={engine.sampler,views[i],VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL};
        const VkWriteDescriptorSet write={.sType=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,.dstSet=descriptors[i],.dstBinding=0,
            .descriptorCount=1,.descriptorType=VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,.pImageInfo=&dii};
        vkUpdateDescriptorSets(d,1,&write,0,NULL);
    }
#ifdef CUBIT_NATIVE_SCENE_TEST
    CHECK(native_scene_pixels(inst,phy,d,pass,cmd,queue,fence,&engine,descriptors[0],pixels)==0);
    VK(vkResetFences(d,1,&fence));VK(vkResetCommandBuffer(cmd,0));
#endif
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
    CHECK(provider_tests(&engine,cmd,source)==0);
    struct cubit_vulkan_sources provider;
    VK(cubit_vulkan_sources_init(&provider,&engine,cmd,vkGetDeviceProcAddr));
    struct cubit_vulkan_submission submission;
    CHECK(cubit_vulkan_submission_init(&submission,d,queue,cmd,fence,vkGetDeviceProcAddr)==0);
    submission.status=delayed_fence;test_submission_open(&submission);
    CHECK(test_submission_budget()==0);CHECK(test_submission_releasable()==1);
    struct input mismatch_input={W,H,1,1,0,0,0,0,0,16,12,0,0,W,H,0,0,0};
    const VkClearValue mismatch_clear={.color={{0,0,0,1}}};
    const struct cubit_vulkan_scene mismatch_scene={.device=d,.begin={.sType=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,
        .renderPass=pass,.framebuffer=framebuffer,.renderArea={{0,0},{W,H}},.clearValueCount=1,.pClearValues=&mismatch_clear}};
    CHECK(submission_guards(&submission,&mismatch_scene)==0);
    for(unsigned fault=0;fault<3;fault++) {
        struct cubit_vulkan_affine_engine other_engine=engine;
        struct cubit_vulkan_affine_draw other={&other_engine,cmd,descriptors[0],W,H};
        if(fault==0)other.command=VK_NULL_HANDLE;else if(fault==1)other_engine.device=VK_NULL_HANDLE;
        mismatch_input.w=fault==2?W+1:W;
        const unsigned before=calls;
        CHECK(test_submission_register_source(&other)==0);
        CHECK(test_submission_start()==0);
        CHECK(test_submission_release_source()==NULL);
        CHECK(test_submission_register_source(&other)==2);
        CHECK(test_submission_begin_scene((void *)&mismatch_scene,W,H)==0);
        CHECK(test_affine_and_record(&other,&mismatch_input)==2);CHECK(calls==before);
        CHECK(test_submission_releasable()==0);CHECK(test_submission_cancel()==0);
        CHECK(test_submission_releasable()==1);
        CHECK(test_submission_release_source()==&other);
    }
#endif
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
    struct cubit_vulkan_scene *target_scenes=target_set.scenes;
    test_submission_set_targets(&target_scenes[0],&target_scenes[1],&target_scenes[2]);
    uint32_t snapshots[OUTPUTS][W*H];unsigned written[OUTPUTS]={0},untouched=0,current=0;
    unsigned painted_pixels=0,future_x=0,future_y=0,future_pending=0;
    unsigned layered_pixels=0;
    const unsigned cases=712;
#else
    const unsigned cases=304;
#endif
    unsigned visible=0,empty=0,comparisons=0,straight_requests=0;
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
    unsigned gradient_pixels=0,clip_background_pixels=0,clip_solid_pixels=0,glyph_comparisons=0;
#endif
    const int scales[6][2]={{1,1},{5,4},{3,2},{2,1},{3,1},{2,3}};
    for(unsigned test=0;test<cases;test++) {
        const unsigned scale=test%6,rotation=(test/6)%4,mode=(test/24)%3,origin=test/72;
        struct input in={W,H,scales[scale][0],scales[scale][1],(int)rotation,(int)origin-1,1-(int)origin,
            -2,-1,14,11,3,2,W-2,H-1,mode==1,mode==2,0x8040c0e0};
        if(test>=216) {
            in.n=test%2?16:1;in.d=test%2?1:16;in.rotation=(test/2)%4;
            in.x=test%2?16777216:-16777216;in.y=-in.x;
            in.l=-1073741824;in.t=-1073741824;in.r=1073741824;in.b=1073741824;
            in.dl=0;in.dt=0;in.dr=W;in.db=H;in.over=0;in.mask=0;
        }
#ifndef CUBIT_VULKAN_SUBMISSION_TEST
        if(test>=232) {
            const unsigned variant=test-232,sc=variant%6,rot=(variant/6)%4,org=variant/24;
            in=(struct input){W,H,scales[sc][0],scales[sc][1],(int)rot,(int)org-1,1-(int)org,
                -2,-1,14,11,3,2,W-2,H-1,2,0,0};
        }
#endif
        if(test%11==0){in.dl=W;in.dr=W;}
        if(test%13==0){in.dl=0;in.dt=0;in.dr=W;in.db=H;}
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
        struct input layers[5];unsigned layer_count=0;
        if(test>=328){
            in=(struct input){W,H,1,1,0,0,0,0,0,W,H,0,0,W,H,1,0,0};
            if(test==328){
#ifdef CUBIT_VULKAN_BACKDROP_SCENE_TEST
                for(unsigned i=0;i<W*H;i++)pixels[i]=(pixels[i]&0x007f7f7fu)|0xff000000u;
#else
                for(unsigned i=0;i<W*H;i++)pixels[i]=(pixels[i]&0x007f7f7fu)|0x80000000u;
#endif
                test_submission_damage(0,0,W,H);
            }
            struct input old[4];unsigned old_count=test==328?0:scene_layers(test-329,old);
            layer_count=scene_layers(test-328,layers);
#ifdef CUBIT_VULKAN_BACKDROP_SCENE_TEST
            if(test<424)layers[0].over=0;
#endif
            if(test>=424){
                unsigned sample=(test-424)%24;
                in.n=scales[sample%6][0];in.d=scales[sample%6][1];in.rotation=sample/6;
                in.x=(int)(sample%3)*3-3;in.y=3-(int)(sample%3)*3;
                for(unsigned layer=0;layer<layer_count;layer++){
                    layers[layer].n=in.n;layers[layer].d=in.d;layers[layer].rotation=in.rotation;
                    layers[layer].x=in.x;layers[layer].y=in.y;
                }
                if(test>=448){
                    const int heights[6]={1,2,17,257,2160,4096};
                    struct input gradient=layers[1];
                    gradient.mask=3;gradient.t=2-heights[sample%6]/2;
                    gradient.b=gradient.t+heights[sample%6];
                    for(unsigned i=1;i+1<layer_count;i++)layers[i]=layers[i+1];
                    layers[layer_count-1]=gradient;
                    if(test>=472){
                        struct input solid=gradient;
                        solid.mask=2;solid.t=in.y+2;solid.b=in.y+6;
                        layers[layer_count-1]=solid;layers[layer_count++]=gradient;
                    }
                }
                test_submission_damage(0,0,W,H);
            }
            if(test>=616){
                layer_count=1;layers[0]=in;layers[0].mask=4;layers[0].over=1;layers[0].tint=0xD04AC0E8;
                layers[0].l=in.x+1;layers[0].t=in.y+1;
                if(test>=688){layers[0].l=in.x-2;layers[0].t=in.y-1;}
                layers[0].r=layers[0].l+11;layers[0].b=layers[0].t+17;
            }
            if(test<424){ /* Transformed cases already dirty the complete physical output. */
                for(unsigned i=1;i<old_count;i++)test_submission_damage(old[i].l,old[i].t,old[i].r,old[i].b);
                for(unsigned i=1;i<layer_count;i++)test_submission_damage(layers[i].l,layers[i].t,layers[i].r,layers[i].b);
            }
        }else if(test>=232){
            in=(struct input){W,H,1,1,0,0,0,0,0,W,H,0,0,W,H,0,0,0};
            if(test==232){
                for(unsigned i=0;i<W*H;i++)pixels[i]|=0xff000000u;
                test_submission_damage(0,0,W,H);
            }
            if(future_pending){pixels[future_y*W+future_x]=0xff004000u+test;future_pending=0;}
            const unsigned x=(test*7)%W,y=(test*11)%H;
            pixels[y*W+x]=0xff800000u+test;
            test_submission_damage((int)x,(int)y,(int)x+1,(int)y+1);
        }else test_submission_damage(0,0,W,H);
#endif
#ifndef CUBIT_VULKAN_SUBMISSION_TEST
        if(in.over==2)++straight_requests;
        struct cubit_vulkan_affine_draw borrowed={&engine,cmd,descriptors[in.mask!=0],W,H};
#endif
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
        const struct cubit_vulkan_source_request source_request={&provider,0,
            in.mask?mask:source,in.mask?VK_FORMAT_R8_UNORM:VK_FORMAT_B8G8R8A8_UNORM,W,H};
        CHECK(test_submission_import_source((void *)&source_request,&provider.entries[0].draw)==0);
        CHECK(cubit_vulkan_sources_destroy(&provider)==2);
        if(test>=328){
            const unsigned gi=test>=616?(test-616)%6:0;
            /* Request dimensions describe the destination viewport, not the sampled image. */
            const struct cubit_vulkan_source_request mask_request={&provider,1,test>=616?glyph_images[gi]:mask,VK_FORMAT_R8_UNORM,W,H};
            CHECK(test_submission_import_mask((void *)&mask_request,&provider.entries[1].draw)==0);
            test_submission_capture_begin(&in);
            for(unsigned layer=0;layer<layer_count;layer++){
                struct input captured=layers[layer];
                if(captured.over==2&&!captured.mask)++straight_requests;
                if(test>=472){
                    struct input clip;int active=scene_clip(test,layer,layer_count,&in,&clip);
                    CHECK(test_submission_capture_clip(&clip,!active)==0);
                }
#ifdef CUBIT_VULKAN_BACKDROP_SCENE_TEST
                if(test<424&&layer==0)CHECK(test_submission_capture_backdrop(&captured,(int)(test%3))==0);
                else
#endif
                CHECK((captured.mask==3?test_submission_capture_gradient(&captured,0xE07020u):
                       captured.mask==2?test_submission_capture_fill(&captured):test_submission_capture(&captured))==0);
                memset(&captured,0,sizeof captured);
            }
            CHECK(test_submission_capture_end()==0);
        }
        CHECK(test_submission_start()==0);CHECK(test_submission_releasable()==0);
        CHECK(test_submission_close_targets()==2);
        current=(unsigned)test_submission_target_index()-1;CHECK(current<OUTPUTS);
        target=targets[current];readback=readbacks[current];rp=results[current];
        barrier(cmd,target,written[current]?VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL:VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
            VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,VK_PIPELINE_STAGE_TRANSFER_BIT|VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
            VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,written[current]?VK_ACCESS_TRANSFER_READ_BIT:0,
            VK_ACCESS_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
        if(test>=232){
            barrier(cmd,source,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_ACCESS_SHADER_READ_BIT,VK_ACCESS_TRANSFER_WRITE_BIT);
            vkCmdCopyBufferToImage(cmd,upload,source,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,1,&whole);
            barrier(cmd,source,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,VK_ACCESS_TRANSFER_WRITE_BIT,VK_ACCESS_SHADER_READ_BIT);
        }
        CHECK(test_submission_release_source()==NULL);
#else
        VK(vkBeginCommandBuffer(cmd,&bi));
#endif
#ifndef CUBIT_VULKAN_SUBMISSION_TEST
        const VkClearValue clear={.color={{32.0f/255,64.0f/255,96.0f/255,1}}};
        const VkRenderPassBeginInfo render={.sType=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,.renderPass=pass,.framebuffer=framebuffer,
            .renderArea={{0,0},{W,H}},.clearValueCount=1,.pClearValues=&clear};
#endif
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
        CHECK(test_submission_begin_scene(&target_scenes[current],W,H)==0);
        if(test<232){
            const VkClearAttachment clear={.aspectMask=VK_IMAGE_ASPECT_COLOR_BIT,.colorAttachment=0,.clearValue=target_request.clear};
            const VkClearRect rect={.rect={{0,0},{W,H}},.baseArrayLayer=0,.layerCount=1};
            vkCmdClearAttachments(cmd,1,&clear,1,&rect);
        }
#else
        vkCmdBeginRenderPass(cmd,&render,VK_SUBPASS_CONTENTS_INLINE);
#endif
        const unsigned before=calls;
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
        int outcome;
        unsigned draws_this_frame=0;
        if(test>=328){
            const int regions=test_submission_repaint_count();CHECK(regions>0&&regions<=8);
            for(int region=1;region<=regions;region++){
                int l,t,r,b;test_submission_repaint_box(region,&l,&t,&r,&b);
                layered_pixels+=(unsigned)((r-l)*(b-t));
                for(unsigned layer=0;layer<layer_count;layer++){
                    if(layers[layer].mask==2||layers[layer].mask==3)continue; /* Clear calls do not use the affine draw counter. */
                    struct input part=layers[layer];part.dl=l;part.dt=t;part.dr=r;part.db=b;
                    unsigned any=0;
                    for(int y=t;y<b;y++)for(int x=l;x<r;x++){int sx,sy;any|=clip_contains(test,layer,layer_count,&in,x,y)&&layer_covered(&part,x,y,&sx,&sy);}
                    draws_this_frame+=(any!=0);
                }
            }
            CHECK(test_submission_replay_scene()==0);
            outcome=1;
        }else if(test>=232){
            outcome=0;const int regions=test_submission_repaint_count();CHECK(regions>0&&regions<=8);
            for(int region=1;region<=regions;region++){
                struct input part=in;
                test_submission_repaint_box(region,&part.dl,&part.dt,&part.dr,&part.db);
                CHECK(test_affine_and_record(&provider.entries[0].draw,&part)==1);
                painted_pixels+=(unsigned)((part.dr-part.dl)*(part.db-part.dt));++draws_this_frame;
            }
            outcome=1;
        }else outcome=test_affine_and_record(&provider.entries[0].draw,&in);
#else
        const int outcome=test_affine_and_record(&borrowed,&in);
#endif
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
        CHECK(test_submission_end_scene()==0);
#else
        vkCmdEndRenderPass(cmd);
#endif
        barrier(cmd,target,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
            VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,VK_ACCESS_TRANSFER_READ_BIT);
        vkCmdCopyImageToBuffer(cmd,target,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,readback,1,&whole);
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
        for(unsigned i=0;i<OUTPUTS;i++)if(i!=current&&written[i])
            vkCmdCopyImageToBuffer(cmd,targets[i],VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,readbacks[i],1,&whole);
#endif
        const VkMemoryBarrier host={.sType=VK_STRUCTURE_TYPE_MEMORY_BARRIER,.srcAccessMask=VK_ACCESS_TRANSFER_WRITE_BIT,.dstAccessMask=VK_ACCESS_HOST_READ_BIT};
        vkCmdPipelineBarrier(cmd,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_HOST_BIT,0,1,&host,0,NULL,0,NULL);
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
        CHECK(test_submission_finish()==0);CHECK(test_submission_releasable()==0);
        if(test>=232&&test<328&&test%7==0){
            future_x=(test*5+3)%W;future_y=(test*3+1)%H;future_pending=1;
            test_submission_damage((int)future_x,(int)future_y,(int)future_x+1,(int)future_y+1);
        }
        forced_pending=3;
        unsigned attempts=0;int completion;
        do {
            completion=test_submission_poll();CHECK(completion!=2);CHECK(++attempts<1000000);
            if(completion==1){++observed_pending;CHECK(test_submission_releasable()==0);}
        }while(completion==1);
        CHECK(attempts>=4);CHECK(test_submission_releasable()==1);
        CHECK(test_submission_release_source()==&provider.entries[0].draw);
        CHECK(provider.entries[0].view==VK_NULL_HANDLE);
        if(test>=328){
            CHECK(provider.entries[1].view!=VK_NULL_HANDLE);
            CHECK(test_submission_close_targets()==2);
            CHECK(test_submission_release_mask()==&provider.entries[1].draw);
            CHECK(provider.entries[1].view==VK_NULL_HANDLE);
        }
        CHECK(test_submission_close_targets()==2);
#else
        CHECK(!submit(d,queue,cmd,fence));
#endif
        unsigned touched=0;
        const uint32_t *got=rp;
        for(int y=0;y<H;y++)for(int x=0;x<W;x++) {
            int sx=0,sy=0;const int inside=covered(&in,x,y,&sx,&sy);touched+=inside;
            uint32_t expected=inside?reference(pixels[sy*W+sx],masks[sy*W+sx],&in):0xff204060u;
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
            int exact_gradient=0,exact_solid=0,any_clip_pass=0,glyph_hit=0;
            if(test>=328){
                expected=0xff204060u;
                for(unsigned layer=0;layer<layer_count;layer++){
                    int lx,ly;
                    if(!clip_contains(test,layer,layer_count,&in,x,y))continue;
                    any_clip_pass=1;
                    if(layers[layer].mask==3){
                        /* Independent ungrouped row painter, including overlap
                         * from outward-rounded edges. Later logical rows win. */
                        const struct input *g=&layers[layer];
                        const int height=g->b-g->t;
                        for(int row=0;row<height;row++){
                            struct input stripe=*g;stripe.t=g->t+row;stripe.b=stripe.t+1;
                            if(solid_covered(&stripe,x,y)){
                                unsigned alpha=height==1?0:(unsigned)row*255u/(unsigned)(height-1);
                                expected=0xff000000u;
                                for(unsigned k=0;k<3;k++){
                                    int top=(g->tint>>(8*k))&255,bottom=(0xE07020u>>(8*k))&255;
                                    expected|=(uint32_t)((top*255+(bottom-top)*(int)alpha+127)/255)<<(8*k);
                                }
                                exact_gradient=1;exact_solid=0;
                            }
                        }
                    }else if(layers[layer].mask==2){
                        if(solid_covered(&layers[layer],x,y)){
                            expected=0xff000000u|layers[layer].tint;exact_solid=1;exact_gradient=0;
                        }
                    }else if(layer_covered(&layers[layer],x,y,&lx,&ly)){
                        if(layers[layer].mask==4){
                            expected=reference_over(0,(uint8_t)((lx*31+ly*47)%256),&layers[layer],expected);glyph_hit=1;
                        }else expected=reference_over(pixels[ly*W+lx],masks[ly*W+lx],&layers[layer],expected);
                        exact_solid=0;exact_gradient=0;
                    }
                }
            }
#endif
            for(unsigned k=0;k<4;k++) {
                const int value=(got[y*W+x]>>(k*8))&255, wanted=(expected>>(k*8))&255;
                int tolerance=inside&&(in.over||in.mask)?1:0;
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
                if(test>=328)tolerance=(exact_gradient||exact_solid||!any_clip_pass)?0:(int)layer_count;
                if(test>=616)tolerance=glyph_hit?1:0;
                if(k==0&&glyph_hit)++glyph_comparisons;
                if(k==0&&test>=472&&test<616&&!any_clip_pass)++clip_background_pixels;
                if(k==0&&test>=472&&test<616&&exact_solid)++clip_solid_pixels;
                if(k==0&&exact_gradient)++gradient_pixels;
#endif
                if(abs(value-wanted)>tolerance) {
                    fprintf(stderr,"PIXEL FAIL test=%u scale=%d/%d rot=%d mode=%u x=%d y=%d sx=%d sy=%d got=%08x expected=%08x\n",test,in.n,in.d,in.rotation,mode,x,y,sx,sy,got[y*W+x],expected);return 1;
                }
            }
            comparisons++;
        }
        CHECK(outcome==(touched?1:0));
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
        CHECK(calls==before+(test>=232?draws_this_frame:(touched!=0)));
#else
        CHECK(calls==before+(touched!=0));
#endif
        if(touched)visible++;else empty++;
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
        for(unsigned i=0;i<OUTPUTS;i++)if(i!=current&&written[i]){
            CHECK(memcmp(snapshots[i],results[i],BYTES)==0);untouched+=W*H;
        }
        memcpy(snapshots[current],rp,BYTES);written[current]=1;
        test_submission_display_tick(test==0||(test%4==3&&test+1!=cases));
#endif
    }
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
    CHECK(test_submission_releasable()==1);
    test_submission_finish_display();
    CHECK(written[0]&&written[1]&&written[2]&&untouched>300000);
#ifdef CUBIT_VULKAN_BACKDROP_SCENE_TEST
    printf("HOST ONLY retained wallpaper: 96 ordered scenes, all 3 placement modes at source density, actual source import/queue/fence/retirement PASS\n");
#endif
    printf("HOST ONLY retained targets: three real images, %u unchanged nonwriter pixels, >50 simulated latches/>100 ready replacements PASS\n",untouched);
    VK(vkResetFences(d,1,&fence));VK(vkResetCommandBuffer(cmd,0));
    CHECK(!future_pending&&painted_pixels<96*W*H/2);
    printf("HOST ONLY partial repaint: 96 changing opaque scenes, %u painted pixels vs %u full-frame pixels, including in-flight changes PASS\n",painted_pixels,96*W*H);
    CHECK(layered_pixels<(120+144+96)*W*H);
    CHECK(gradient_pixels>0);
    printf("HOST ONLY gradients: %u scaled/rotated scenes, %u exact gradient pixels PASS\n",616-448,gradient_pixels);
    CHECK(clip_background_pixels>0&&clip_solid_pixels>0);
    printf("HOST ONLY clips: 144 scenes/6 viewport modes/6 scales/4 rotations, %u exact clipped-background and %u exact solid pixels PASS\n",clip_background_pixels,clip_solid_pixels);
    printf("HOST ONLY layered repaint: %u ordered solid/gradient/BGRA/R8 scenes, %u scaled/rotated, %u restored pixels vs %u full-frame pixels PASS\n",cases-328,cases-424,layered_pixels,(cases-328)*W*H);
    printf("HOST ONLY submission: %u actual queues, %u retained pending observations, retained source descriptors, 4096-draw cancellation and 3 command/device/dimension mismatch cancellations PASS\n",cases,observed_pending);
#endif
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
    CHECK(glyph_comparisons>0);
    printf("HOST ONLY glyphs: 96 scenes/6 density-matched R8 images/4 rotations, %u covered pixels, exact outside-cell background, <=1 blend tolerance PASS\n",glyph_comparisons);
#endif
#ifdef CUBIT_VULKAN_BACKDROP_TEST
    CHECK(!backdrop_cases(phy,d,queue,cmd,fence,pass,framebuffer,target,readback,rp,&engine));
#endif
    // Audit rejection before recording using the actual Ada->C boundary.
    VK(vkBeginCommandBuffer(cmd,&bi));
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
    barrier(cmd,targets[0],VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
        VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,VK_ACCESS_TRANSFER_READ_BIT,
        VK_ACCESS_COLOR_ATTACHMENT_READ_BIT|VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
#endif
    const VkClearValue rejected_clear={.color={{0,0,0,1}}};
    const VkRenderPassBeginInfo rejected_pass={.sType=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,.renderPass=pass,.framebuffer=framebuffer,
        .renderArea={{0,0},{W,H}},.clearValueCount=1,.pClearValues=&rejected_clear};
    vkCmdBeginRenderPass(cmd,&rejected_pass,VK_SUBPASS_CONTENTS_INLINE);
    const struct input valid={W,H,1,1,0,0,0,0,0,16,12,0,0,W,H,0,0,0};
    for(unsigned fault=0;fault<11;fault++) {
        struct cubit_vulkan_affine_engine altered=engine;
        struct cubit_vulkan_affine_draw borrowed={&altered,cmd,descriptors[0],W,H};
        void *context=&borrowed;
        switch(fault) {
            case 0:context=NULL;break;case 1:borrowed.engine=NULL;break;case 2:borrowed.command=VK_NULL_HANDLE;break;
            case 3:borrowed.source=VK_NULL_HANDLE;break;case 4:borrowed.width=W+1;break;case 5:borrowed.height=0;break;
            case 6:altered.layout=VK_NULL_HANDLE;break;case 7:altered.pipeline[0]=VK_NULL_HANDLE;break;
            case 8:altered.pipeline[1]=VK_NULL_HANDLE;break;case 9:altered.draw=NULL;break;case 10:altered.pipeline[2]=VK_NULL_HANDLE;break;
        }
        const unsigned before=calls;CHECK(test_affine_and_record(context,&valid)==2);CHECK(calls==before);
    }
    vkCmdEndRenderPass(cmd);CHECK(!submit(d,queue,cmd,fence));
    VK(vkDeviceWaitIdle(d));
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
    CHECK(cubit_vulkan_sources_destroy(&provider)==0);
#endif
    vkDestroyDescriptorPool(d,dp,NULL); cubit_vulkan_affine_destroy(&engine);
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
    CHECK(test_submission_close_targets()==0);
    for(unsigned i=0;i<OUTPUTS;i++)CHECK(!target_set.framebuffers[i]&&!target_set.views[i]);
    CHECK(test_submission_close_targets()==2);
#else
    for(unsigned i=0;i<OUTPUTS;i++)vkDestroyFramebuffer(d,framebuffers[i],NULL);
#endif
    vkDestroyRenderPass(d,pass,NULL);
    for(unsigned i=0;i<manual_views;i++)vkDestroyImageView(d,views[i],NULL);
    vkDestroyFence(d,fence,NULL);vkDestroyCommandPool(d,pool,NULL);
    vkUnmapMemory(d,um);vkUnmapMemory(d,cm);
    vkDestroyBuffer(d,upload,NULL);vkDestroyBuffer(d,coverage,NULL);
    vkFreeMemory(d,um,NULL);vkFreeMemory(d,cm,NULL);
    CHECK(!destroy_image(d,source,sm));CHECK(!destroy_image(d,mask,mm));
    for(unsigned i=0;i<OUTPUTS;i++){
        vkUnmapMemory(d,readback_memory[i]);vkDestroyBuffer(d,readbacks[i],NULL);vkFreeMemory(d,readback_memory[i],NULL);
        CHECK(!destroy_image(d,targets[i],target_memory[i]));
    }
#ifdef CUBIT_VULKAN_SUBMISSION_TEST
    for(unsigned i=0;i<6;i++){
        vkUnmapMemory(d,glyph_upload_memory[i]);vkDestroyBuffer(d,glyph_upload[i],NULL);
        vkFreeMemory(d,glyph_upload_memory[i],NULL);CHECK(!destroy_image(d,glyph_images[i],glyph_memory[i]));
    }
#endif
#ifdef CUBIT_VULKAN_OWNED_IMAGE_TEST
    CHECK(test_owned_empty());printf("HOST ONLY SPARK-owned images: %u allocated, rendered, retired and refunded\n",owned_count);
#endif
    vkDestroyDevice(d,NULL);free_debug(inst,messenger,NULL);vkDestroyInstance(inst,NULL);
    printf("HOST ONLY affine: %u draws, %u empty, %u pixels, 6 scales/4 rotations/3 modes/3 origins +16 extreme transforms; %u straight-alpha requests; 16 missing dispatch +3 partial-create +11 recording faults; validation errors=%u\n",visible,empty,comparisons,straight_requests,errors);
    CHECK(errors==0);CHECK(visible>150&&empty>10);return 0;
}
