/* Linux real-Vulkan handoff regression, not the compositor implementation.
 * Validate source layout/usage and a second GPU submission while the producer
 * retains its image, then exercise the unmapped CPU baseline consumer. */
static unsigned image_borrows;
static inline VkResult borrow_completed_image(const struct mesa_completed_image *s,
                                       mesa_completed_pixels present)
{
   if(!s || !s->image || !s->view || !s->queue || !present ||
      !s->width || !s->height || s->readback_bytes!=(VkDeviceSize)s->width*s->height*4)
      return VK_ERROR_UNKNOWN;
   VkCommandPool pool=VK_NULL_HANDLE;
   VkFence fence=VK_NULL_HANDLE;
   VkCommandBuffer command=VK_NULL_HANDLE;
   VkResult result;
#define BORROW_TRY(call) do {result=(call);if(result!=VK_SUCCESS)goto done;}while(0)
   const VkCommandPoolCreateInfo pc={.sType=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
      .queueFamilyIndex=0};
   BORROW_TRY(vkCreateCommandPool(s->device,&pc,NULL,&pool));
   const VkCommandBufferAllocateInfo ac={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
      .commandPool=pool,.level=VK_COMMAND_BUFFER_LEVEL_PRIMARY,.commandBufferCount=1};
   BORROW_TRY(vkAllocateCommandBuffers(s->device,&ac,&command));
   const VkFenceCreateInfo fc={.sType=VK_STRUCTURE_TYPE_FENCE_CREATE_INFO};
   BORROW_TRY(vkCreateFence(s->device,&fc,NULL,&fence));
   const VkCommandBufferBeginInfo bc={.sType=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO};
   BORROW_TRY(vkBeginCommandBuffer(command,&bc));
   const VkImageMemoryBarrier barrier={.sType=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,
      .srcAccessMask=VK_ACCESS_TRANSFER_READ_BIT,.dstAccessMask=VK_ACCESS_SHADER_READ_BIT,
      .oldLayout=VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,.newLayout=VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
      .srcQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,.dstQueueFamilyIndex=VK_QUEUE_FAMILY_IGNORED,
      .image=s->image,.subresourceRange={VK_IMAGE_ASPECT_COLOR_BIT,0,1,0,1}};
   vkCmdPipelineBarrier(command,VK_PIPELINE_STAGE_TRANSFER_BIT,VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,
      0,0,NULL,0,NULL,1,&barrier);
   BORROW_TRY(vkEndCommandBuffer(command));
   const VkSubmitInfo submit={.sType=VK_STRUCTURE_TYPE_SUBMIT_INFO,
      .commandBufferCount=1,.pCommandBuffers=&command};
   result=vkQueueSubmit(s->queue,1,&submit,fence);
   if(result==VK_SUCCESS){
      do{result=vkWaitForFences(s->device,1,&fence,VK_TRUE,1000000000ull);}
      while(result==VK_TIMEOUT);
   }
   if(result!=VK_SUCCESS){
      /* Error is not evidence of quiescence. Keep producer borrow until idle
       * or terminal device loss; process timeout remains an external harness. */
      VkResult idle;
      do{idle=vkDeviceWaitIdle(s->device);}while(idle!=VK_SUCCESS&&idle!=VK_ERROR_DEVICE_LOST);
      goto done;
   }
   ++image_borrows;
   result=present(s->device,s->readback,s->readback_bytes,s->width,s->height,s->width*4);
done:
   if(fence)vkDestroyFence(s->device,fence,NULL);
   if(pool)vkDestroyCommandPool(s->device,pool,NULL);
#undef BORROW_TRY
   return result;
}
