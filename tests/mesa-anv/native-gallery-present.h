/* GPU rendering, explicit CPU readback/copy presentation baseline. The normal
 * immutable Desktop frame pair keeps one window alive without retaining a
 * Vulkan allocation or granting concurrent write access to displayed pixels. */
extern uint32_t cubit_test_gallery_frame(const void *,uint32_t,uint32_t,uint32_t);
extern uint32_t cubit_test_gallery_close(void);
extern void cubit_test_gallery_rate(uint64_t);
extern void mesa_gallery_surface___elabb(void);
static unsigned gallery_frames;
static uint64_t gallery_rate_start;
static uint64_t gallery_rate_last;
static int gallery_rate_valid;
static void finish_gallery_window(void)
{
    if(!gallery_frames)return;
    while(cubit_test_gallery_close())usleep(100000);
}
static VkResult present_completed_triangle(VkDevice device,VkDeviceMemory memory,
    VkDeviceSize bytes,uint32_t width,uint32_t height,uint32_t pitch)
{
    if(!gallery_frames)mesa_gallery_surface___elabb(); /* No Ada binder in this C entrypoint. */
    if(!gallery_frames){
        gallery_rate_start=teapot_clock_ns();
        gallery_rate_last=gallery_rate_start;
        gallery_rate_valid=gallery_rate_start!=0;
    }
    ANV_FROM_HANDLE(anv_device, native, device);
    PFN_vkMapMemory map=native->vk.dispatch_table.MapMemory;
    PFN_vkUnmapMemory unmap=native->vk.dispatch_table.UnmapMemory;
    void *pixels=NULL;
    VkResult result=VK_ERROR_INITIALIZATION_FAILED;
    uint32_t status=1;
    if(map && unmap && width==800 && height==600 && pitch==3200 && bytes==1920000){
        result=map(device,memory,0,bytes,0,&pixels);
        if(result==VK_SUCCESS){
            status=cubit_test_gallery_frame(pixels,width,height,pitch);
            for(unsigned wait=0;status==4 && wait<1000;wait++){
                usleep(1000);
                status=cubit_test_gallery_frame(pixels,width,height,pitch);
            }
            unmap(device,memory);
            if(status && status!=2)report("MESA-GALLERY presentation failed stage=%u\n",status);
            result=status==0 ? VK_SUCCESS : (status==2 ? VK_EVENT_SET : VK_ERROR_UNKNOWN);
        }
    }
    ++gallery_frames;
    const uint64_t now=teapot_clock_ns();
    if(!now || now<gallery_rate_last)gallery_rate_valid=0;
    gallery_rate_last=now;
    if(gallery_frames%60==0){
        if(gallery_rate_valid && now>gallery_rate_start)
            cubit_test_gallery_rate((gallery_frames-1)*1000000000000ull/(now-gallery_rate_start));
        else cubit_test_gallery_rate(UINT64_MAX);
    }
    if(result!=VK_SUCCESS || gallery_frames==CUBIT_TEAPOT_FRAME_COUNT){
        report("MESA-GALLERY closing persistent window; retiring CPU frame pair\n");
        finish_gallery_window();
    }
    return result;
}
