/* Included by the real Vulkan harness after its allocation/barrier helpers. */
#include "vulkan_backdrop.h"
extern int test_backdrop_record(void *,int,int,int,int,int,int,int,int,int);
static uint32_t backdrop_reference_blend(uint32_t a,uint32_t b,uint32_t f)
{
    uint32_t result=0xff000000u;
    for(unsigned shift=0;shift<24;shift+=8)
        result|=((((a>>shift)&255)*(256-f)+((b>>shift)&255)*f+128)/256)<<shift;
    return result;
}
static int backdrop_cases(VkPhysicalDevice phy,VkDevice d,VkQueue queue,VkCommandBuffer cmd,
    VkFence fence,VkRenderPass pass,VkFramebuffer framebuffer,VkImage target,VkBuffer readback,
    void *rp,const struct cubit_vulkan_affine_engine *engine)
{
    static const unsigned sizes[][2]={{1,1},{1,7},{9,1},{3,7},{17,5},{31,29},{65,3},{2,97},{8192,1},{1,8192},{2048,576},{2048,1152},{8191,1},{1,8191}};
    const uint32_t background=0xff123456u;
    unsigned frames=0,pixels_checked=0;
    for(unsigned size=0;size<sizeof sizes/sizeof sizes[0];size++){
        const unsigned sw=sizes[size][0],sh=sizes[size][1];
        VkImage source;VkDeviceMemory sm,um;VkBuffer upload;void *up;
        CHECK(!image_size(phy,d,&source,&sm,VK_FORMAT_B8G8R8A8_UNORM,
            VK_IMAGE_USAGE_SAMPLED_BIT|VK_IMAGE_USAGE_TRANSFER_DST_BIT,sw,sh));
        CHECK(!buffer_size(phy,d,&upload,&um,&up,sw*sh*4));
        uint32_t *source_pixels=up;
        for(unsigned y=0;y<sh;y++)for(unsigned x=0;x<sw;x++)
            source_pixels[y*sw+x]=((x*13+y*41)%256)<<24|((x*47+y*31)%256)<<16|
                ((x*19+y*71)%256)<<8|((x*91+y*11)%256);
        const VkCommandBufferBeginInfo bi={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO};
        VK(vkBeginCommandBuffer(cmd,&bi));
        barrier(cmd,source,VK_IMAGE_LAYOUT_UNDEFINED,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
            VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,VK_PIPELINE_STAGE_TRANSFER_BIT,0,VK_ACCESS_TRANSFER_WRITE_BIT);
        const VkBufferImageCopy copy_source={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},.imageExtent={sw,sh,1}};
        vkCmdCopyBufferToImage(cmd,upload,source,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,1,&copy_source);
        barrier(cmd,source,VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
            VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,VK_ACCESS_TRANSFER_WRITE_BIT,VK_ACCESS_SHADER_READ_BIT);
        CHECK(!submit(d,queue,cmd,fence));
        const VkImageViewCreateInfo vi={.sType=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,.image=source,
            .viewType=VK_IMAGE_VIEW_TYPE_2D,.format=VK_FORMAT_B8G8R8A8_UNORM,.subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
        VkImageView view;VK(vkCreateImageView(d,&vi,NULL,&view));
        const VkDescriptorPoolSize ps={VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,1};
        const VkDescriptorPoolCreateInfo pi={.sType=VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
            .maxSets=1,.poolSizeCount=1,.pPoolSizes=&ps};
        VkDescriptorPool pool;VK(vkCreateDescriptorPool(d,&pi,NULL,&pool));
        const VkDescriptorSetAllocateInfo ai={.sType=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
            .descriptorPool=pool,.descriptorSetCount=1,.pSetLayouts=&engine->descriptors};
        VkDescriptorSet descriptor;VK(vkAllocateDescriptorSets(d,&ai,&descriptor));
        const VkDescriptorImageInfo image_info={engine->sampler,view,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL};
        const VkWriteDescriptorSet write={.sType=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,.dstSet=descriptor,
            .dstBinding=0,.descriptorCount=1,.descriptorType=VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,.pImageInfo=&image_info};
        vkUpdateDescriptorSets(d,1,&write,0,NULL);
        struct cubit_vulkan_affine_draw borrowed={engine,cmd,descriptor,W,H};
        for(int mode=0;mode<3;mode++)for(unsigned clip=0;clip<5;clip++){
            int l=0,t=0,r=W,b=H;
            if(clip==1){l=3;t=2;r=W-2;b=H-1;}
            if(clip==2){l=W-1;t=H-1;}
            if(clip==3){l=W;r=0;}
            if(clip==4){r=0;}
            VK(vkBeginCommandBuffer(cmd,&bi));
            const VkClearValue clear={.color={{18/255.0f,52/255.0f,86/255.0f,1}}};
            const VkRenderPassBeginInfo begin={.sType=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,.renderPass=pass,
                .framebuffer=framebuffer,.renderArea={{0,0},{W,H}},.clearValueCount=1,.pClearValues=&clear};
            vkCmdBeginRenderPass(cmd,&begin,VK_SUBPASS_CONTENTS_INLINE);
            const unsigned before=calls;
            CHECK(test_backdrop_record(&borrowed,W,H,sw,sh,mode,l,t,r,b)==(clip<3?1:0));
            CHECK(calls==before+(clip<3));
            vkCmdEndRenderPass(cmd);
            barrier(cmd,target,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,VK_PIPELINE_STAGE_TRANSFER_BIT,
                VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,VK_ACCESS_TRANSFER_READ_BIT);
            const VkBufferImageCopy copy={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},.imageExtent={W,H,1}};
            vkCmdCopyImageToBuffer(cmd,target,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,readback,1,&copy);
            const VkMemoryBarrier host={.sType=VK_STRUCTURE_TYPE_MEMORY_BARRIER,
                .srcAccessMask=VK_ACCESS_TRANSFER_WRITE_BIT,.dstAccessMask=VK_ACCESS_HOST_READ_BIT};
            vkCmdPipelineBarrier(cmd,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_HOST_BIT,0,1,&host,0,NULL,0,NULL);
            CHECK(!submit(d,queue,cmd,fence));
            unsigned dw=sw,dh=sh;
            if(mode!=2){
                if(((uint64_t)W*sh>=(uint64_t)H*sw)==(mode==0)){
                    dw=W;dh=((uint64_t)W*sh+sw-1)/sw;
                }else{dh=H;dw=((uint64_t)H*sw+sh-1)/sh;}
            }
            const int left=(W-(int)dw)/2,top=(H-(int)dh)/2;
            for(int y=0;y<H;y++)for(int x=0;x<W;x++){
                uint32_t expected=background;
                const int ix=x-left,iy=y-top;
                if(x>=l&&x<r&&y>=t&&y<b&&ix>=0&&iy>=0&&(unsigned)ix<dw&&(unsigned)iy<dh){
                    const uint64_t fx=dw==1?0:(uint64_t)ix*(sw-1)*256/(dw-1);
                    const uint64_t fy=dh==1?0:(uint64_t)iy*(sh-1)*256/(dh-1);
                    unsigned x0=fx/256,y0=fy/256,x1=x0+1<sw?x0+1:x0,y1=y0+1<sh?y0+1:y0;
                    expected=backdrop_reference_blend(
                        backdrop_reference_blend(source_pixels[y0*sw+x0],source_pixels[y0*sw+x1],fx%256),
                        backdrop_reference_blend(source_pixels[y1*sw+x0],source_pixels[y1*sw+x1],fx%256),fy%256);
                }
                if(((uint32_t*)rp)[y*W+x]!=expected){
                    fprintf(stderr,"BACKDROP PIXEL FAIL size=%u mode=%d clip=%u x=%d y=%d got=%08x expected=%08x\n",
                        size,mode,clip,x,y,((uint32_t*)rp)[y*W+x],expected);return 1;
                }
                pixels_checked++;
            }
            frames++;
        }
        vkDestroyDescriptorPool(d,pool,NULL);vkDestroyImageView(d,view,NULL);
        vkUnmapMemory(d,um);vkDestroyBuffer(d,upload,NULL);vkFreeMemory(d,um,NULL);
        CHECK(!destroy_image(d,source,sm));
    }
    printf("HOST ONLY backdrop: %u frames, %u exact pixels, Fill/Fit/Center, singleton axes, clipping/preserved backgrounds PASS\n",frames,pixels_checked);
    return 0;
}
