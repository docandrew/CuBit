    VkImage target; VkDeviceMemory tm,rm; VkBuffer readback; void *rp;
    CHECK(!image(phy,d,&target,&tm,VK_FORMAT_B8G8R8A8_UNORM,VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT|VK_IMAGE_USAGE_TRANSFER_SRC_BIT));
    CHECK(!buffer(phy,d,&readback,&rm,&rp));
    const VkCommandPoolCreateInfo pi={.sType=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,.flags=VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,.queueFamilyIndex=family};
    VkCommandPool pool;VK(vkCreateCommandPool(d,&pi,NULL,&pool));
    const VkCommandBufferAllocateInfo ai={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,.commandPool=pool,.level=VK_COMMAND_BUFFER_LEVEL_PRIMARY,.commandBufferCount=1};
    VkCommandBuffer cmd;VK(vkAllocateCommandBuffers(d,&ai,&cmd));
    const VkFenceCreateInfo fi={.sType=VK_STRUCTURE_TYPE_FENCE_CREATE_INFO};VkFence fence;VK(vkCreateFence(d,&fi,NULL,&fence));
    const VkAttachmentDescription attachment={.format=VK_FORMAT_B8G8R8A8_UNORM,.samples=VK_SAMPLE_COUNT_1_BIT,
        .loadOp=VK_ATTACHMENT_LOAD_OP_CLEAR,.storeOp=VK_ATTACHMENT_STORE_OP_STORE,.stencilLoadOp=VK_ATTACHMENT_LOAD_OP_DONT_CARE,
        .stencilStoreOp=VK_ATTACHMENT_STORE_OP_DONT_CARE,.initialLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,.finalLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL};
    const VkAttachmentReference color={0,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL};
    const VkSubpassDescription subpass={.pipelineBindPoint=VK_PIPELINE_BIND_POINT_GRAPHICS,.colorAttachmentCount=1,.pColorAttachments=&color};
    const VkRenderPassCreateInfo pci={.sType=VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO,.attachmentCount=1,.pAttachments=&attachment,.subpassCount=1,.pSubpasses=&subpass};
    VkRenderPass pass;VK(vkCreateRenderPass(d,&pci,NULL,&pass));
    const VkImageViewCreateInfo vi={.sType=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,.image=target,.viewType=VK_IMAGE_VIEW_TYPE_2D,.format=VK_FORMAT_B8G8R8A8_UNORM,.subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
    VkImageView view;VK(vkCreateImageView(d,&vi,NULL,&view));
    const VkFramebufferCreateInfo fbi={.sType=VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO,.renderPass=pass,.attachmentCount=1,.pAttachments=&view,.width=W,.height=H,.layers=1};
    VkFramebuffer frame;VK(vkCreateFramebuffer(d,&fbi,NULL,&frame));
    struct cubit_vulkan_checker engine;VK(cubit_vulkan_checker_create(&engine,d,vkGetDeviceProcAddr,pass));
    engine.draw=counted_draw;
    CHECK(!checker_guards(d,pass,&engine));
    unsigned frames=0,pixels=0,painted=0,untouched=0;
    const VkCommandBufferBeginInfo bi={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO};
    const VkClearValue clear={.color={{17.0f/255,51.0f/255,85.0f/255,1}}};
    const VkRenderPassBeginInfo render={.sType=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,.renderPass=pass,.framebuffer=frame,.renderArea={{0,0},{W,H}},.clearValueCount=1,.pClearValues=&clear};
    const VkBufferImageCopy whole={.imageSubresource={VK_IMAGE_ASPECT_COLOR_BIT,0,0,1},.imageExtent={W,H,1}};
    for(unsigned rot=0;rot<4;rot++)for(unsigned n=1;n<=16;n++)for(unsigned den=1;den<=16;den++)
    for(int origin=-1;origin<=1;origin++)for(unsigned clipped=0;clipped<2;clipped++){
        struct cubit_vulkan_checker_request r={.left=-2,.top=1,.right=9,.bottom=8,
            .origin_x=origin*7,.origin_y=-origin*4,.numerator=n,.denominator=den,
            .width=W,.height=H,.rotation=rot,.clip_x=clipped?3:0,.clip_y=clipped?4:0,
            .clip_w=clipped?W-6:W,.clip_h=clipped?H-8:H,.rgb=0x2468ac};
        uint32_t expected[W*H];for(unsigned i=0;i<W*H;i++)expected[i]=0xff113355;
        /* Independent forward per-cell oracle, matching software's union of
           outward-rounded cells. Shader uses inverse cell ranges instead. */
        for(int y=r.top;y<r.bottom;y++)for(int x=r.left;x<r.right;x++)if((x+y)%2==0){
            int l=floor_div((x-r.origin_x)*(int)n,(int)den),t=floor_div((y-r.origin_y)*(int)n,(int)den);
            int rr=-floor_div(-(x+1-r.origin_x)*(int)n,(int)den),bb=-floor_div(-(y+1-r.origin_y)*(int)n,(int)den);
            int a=l,b=t,c=rr,e=bb;
            if(rot==1){a=W-bb;b=l;c=W-t;e=rr;}
            if(rot==2){a=W-rr;b=H-bb;c=W-l;e=H-t;}
            if(rot==3){a=t;b=H-rr;c=bb;e=H-l;}
            a=imax(a,(int)r.clip_x);b=imax(b,(int)r.clip_y);
            c=imin(c,(int)(r.clip_x+r.clip_w));e=imin(e,(int)(r.clip_y+r.clip_h));
            for(int py=b;py<e;py++)for(int px=a;px<c;px++)expected[py*W+px]=0xff2468ac;
        }
        VK(vkBeginCommandBuffer(cmd,&bi));
        barrier(cmd,target,frames?VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL:VK_IMAGE_LAYOUT_UNDEFINED,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
            frames?VK_PIPELINE_STAGE_TRANSFER_BIT:VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT,VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
            frames?VK_ACCESS_TRANSFER_READ_BIT:0,VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
        vkCmdBeginRenderPass(cmd,&render,VK_SUBPASS_CONTENTS_INLINE);
        const unsigned before=calls;CHECK(cubit_vulkan_checker_record(&engine,cmd,&r)==0);CHECK(calls==before+1);
        vkCmdEndRenderPass(cmd);
        barrier(cmd,target,VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
            VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,VK_ACCESS_TRANSFER_READ_BIT);
        vkCmdCopyImageToBuffer(cmd,target,VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,readback,1,&whole);
        const VkMemoryBarrier host={.sType=VK_STRUCTURE_TYPE_MEMORY_BARRIER,.srcAccessMask=VK_ACCESS_TRANSFER_WRITE_BIT,.dstAccessMask=VK_ACCESS_HOST_READ_BIT};
        vkCmdPipelineBarrier(cmd,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_HOST_BIT,0,1,&host,0,NULL,0,NULL);
        CHECK(!submit(d,queue,cmd,fence));
        for(unsigned i=0;i<W*H;i++){
            if(((uint32_t *)rp)[i]!=expected[i]){fprintf(stderr,"checker pixel mismatch frame=%u pixel=%u n=%u d=%u rotation=%u got=%08x expected=%08x\n",frames,i,n,den,rot,((uint32_t *)rp)[i],expected[i]);return 1;}
            if(expected[i]==0xff2468ac)painted++;else untouched++;pixels++;
        }
        frames++;
    }
    CHECK(painted&&untouched&&errors==0);
    cubit_vulkan_checker_destroy(&engine);CHECK(!engine.pipeline&&!engine.layout);
    vkDestroyFramebuffer(d,frame,NULL);vkDestroyImageView(d,view,NULL);vkDestroyRenderPass(d,pass,NULL);
    vkDestroyFence(d,fence,NULL);vkDestroyCommandPool(d,pool,NULL);
    vkUnmapMemory(d,rm);vkDestroyBuffer(d,readback,NULL);vkFreeMemory(d,rm,NULL);CHECK(!destroy_image(d,target,tm));
    vkDestroyDevice(d,NULL);free_debug(inst,messenger,NULL);vkDestroyInstance(inst,NULL);
    CHECK(errors==0);
    printf("PASS hosted Vulkan checker: %u frames, %u exact pixels, %u painted/%u preserved, no image descriptors, validation errors=%u\n",frames,pixels,painted,untouched,errors);
    return 0;
}
