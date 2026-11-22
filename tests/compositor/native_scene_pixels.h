/* Included by the hosted Vulkan oracle after its resource helpers.
 * Production Ada bridge and C adapters are linked unchanged. */
#include "native_scene_transfer.h"
static unsigned native_scene_waits,native_scene_queues;
static VkResult VKAPI_CALL native_scene_status(VkDevice device,VkFence fence)
{
    if(native_scene_waits){--native_scene_waits;return VK_NOT_READY;}
    return vkGetFenceStatus(device,fence);
}
static VkResult VKAPI_CALL native_scene_queue(VkQueue queue,uint32_t count,
                                             const VkSubmitInfo *submits,VkFence fence)
{
    ++native_scene_queues;return vkQueueSubmit(queue,count,submits,fence);
}
static int native_scene_pixels(VkInstance instance,VkPhysicalDevice physical,VkDevice device,
    VkRenderPass pass,VkCommandBuffer command,VkQueue queue,VkFence fence,
    const struct cubit_vulkan_affine_engine *engine,VkDescriptorSet descriptor,
    const uint32_t *expected_source)
{
    const int omit_barrier=getenv("CUBIT_NATIVE_SCENE_OMIT_BARRIER")!=NULL;
    unsigned compared=0,consumer_rejections=0;
    VkBuffer readback;VkDeviceMemory memory;void *pixels;
    CHECK(!buffer(physical,device,&readback,&memory,&pixels));
    VkPhysicalDeviceMemoryProperties properties;vkGetPhysicalDeviceMemoryProperties(physical,&properties);
    uint32_t allowed=0;
    for(uint32_t i=0;i<properties.memoryTypeCount&&i<32;i++)
        if(!(properties.memoryTypes[i].propertyFlags&(VK_MEMORY_PROPERTY_PROTECTED_BIT|VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT)))allowed|=UINT32_C(1)<<i;
    for(unsigned cycle=0;cycle<8;cycle++){
        struct cubit_vulkan_owned_image images[3];
        for(unsigned i=0;i<3;i++)images[i]=(struct cubit_vulkan_owned_image){
            .physical=physical,.device=device,.instance=instance,.instance_proc=vkGetInstanceProcAddr,
            .proc=vkGetDeviceProcAddr,.width=W,.height=H,.format=VK_FORMAT_B8G8R8A8_UNORM,
            .usage=VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT|VK_IMAGE_USAGE_TRANSFER_SRC_BIT};
        struct cubit_vulkan_targets views={0};
        struct cubit_vulkan_target_request request={.fresh=&views,.device=device,.proc=vkGetDeviceProcAddr,
            .pass=pass,.width=W,.height=H};
        struct cubit_vulkan_submission owner={0};
        CHECK(cubit_vulkan_submission_init(&owner,device,queue,command,fence,vkGetDeviceProcAddr)==0);
        owner.status=native_scene_status;owner.submit=native_scene_queue;
        struct cubit_vulkan_affine_draw draw={engine,command,descriptor,W,H};
        uint32_t result=99,selected=0,ignored=0;
        cubit_native_scene_open(&request,&images[0],&images[1],&images[2],&owner,&draw,allowed,W,H,&result);
        CHECK(result==0);
        /* Both before-pass and after-pass cancellation are never submitted. */
        for(unsigned cancel=0;cancel<2;cancel++){
            const unsigned before=native_scene_queues;
            cubit_native_scene_begin(&selected,&result);CHECK(result==0&&selected>=1&&selected<=3);
            if(cancel){cubit_native_scene_record(&result);CHECK(result==0);}
            cubit_native_scene_cancel(&result);CHECK(result==0&&native_scene_queues==before);
        }
        for(unsigned frame=0;frame<16;frame++){
            const unsigned before=native_scene_queues;
            cubit_native_scene_begin(&selected,&result);CHECK(result==0&&selected>=1&&selected<=3);
            const unsigned slot=selected-1;
            cubit_native_scene_record(&result);CHECK(result==0&&native_scene_queues==before);
            cubit_native_scene_release(&result);CHECK(result==2);
            cubit_native_scene_close(&result);CHECK(result==2);
            if(!omit_barrier){
                CHECK(cubit_native_scene_readback_commands(command,images[slot].image,readback,W,H,
                    vkCmdPipelineBarrier,vkCmdCopyImageToBuffer)==0);
            }else{
                /* Deliberate negative control bypasses the shared adapter. */
                const VkBufferImageCopy copy={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},.imageExtent={W,H,1}};
                vkCmdCopyImageToBuffer(command,images[slot].image,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,readback,1,&copy);
            }
            cubit_native_scene_submit(&result);CHECK(result==0&&native_scene_queues==before+1);
            native_scene_waits=3;
            for(unsigned pending=0;pending<3;pending++){
                cubit_native_scene_poll(&result);CHECK(result==1);
                cubit_native_scene_begin(&ignored,&result);CHECK(result==2&&ignored==0);
                cubit_native_scene_cancel(&result);CHECK(result==2);
                cubit_native_scene_release(&result);CHECK(result==2);
                cubit_native_scene_close(&result);CHECK(result==2);
                CHECK(images[slot].image&&images[slot].memory&&views.framebuffers[slot]);
            }
            VK(vkWaitForFences(device,1,&fence,VK_TRUE,5000000000ull));
            cubit_native_scene_poll(&result);CHECK(result==0&&native_scene_queues==before+1);
            cubit_native_scene_begin(&ignored,&result);CHECK(result==2&&ignored==0);
            cubit_native_scene_close(&result);CHECK(result==2);
            for(unsigned y=0;y<H;y++)for(unsigned x=0;x<W;x++){
                const uint32_t expected=x>=4&&x<12&&y>=4&&y<12?0xff00ff00u:expected_source[y*W+x];
                CHECK(((uint32_t *)pixels)[y*W+x]==expected);++compared;
            }
            /* A simulated CPU consumer may reject presentation, but only
             * returns this borrow after examining the completed readback. */
            if(frame%2)++consumer_rejections;
            cubit_native_scene_release(&result);CHECK(result==0);
        }
        cubit_native_scene_close(&result);CHECK(result==0);
        for(unsigned i=0;i<3;i++)CHECK(!images[i].image&&!images[i].memory&&!views.views[i]&&!views.framebuffers[i]);
    }
    vkUnmapMemory(device,memory);vkDestroyBuffer(device,readback,NULL);vkFreeMemory(device,memory,NULL);
    CHECK(native_scene_queues==128);
    printf("HOST ONLY native Ada bridge: 8 lifetimes/128 queues, %u exact pixels, 384 pending polls, 16 cancelled scenes, %u simulated consumer rejections PASS\n",compared,consumer_rejections);
    return 0;
}
