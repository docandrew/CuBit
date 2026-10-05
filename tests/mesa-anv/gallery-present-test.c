/* Hosted wrapper/lifetime test with controlled callbacks; NOT GPU execution. */
#include <vulkan/vulkan.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
static unsigned mode,initialized,maps,unmaps,copies,closes,sleeps;
static int mapped;
static uint32_t pixel;
static VkResult VKAPI_CALL map_memory(VkDevice d,VkDeviceMemory m,VkDeviceSize off,
    VkDeviceSize bytes,VkMemoryMapFlags flags,void **out)
{
    (void)d;(void)m;(void)flags;
    assert(initialized==1 && !mapped && off==0 && bytes==1920000);
    ++maps;
    if(mode==4)return VK_ERROR_MEMORY_MAP_FAILED;
    mapped=1;*out=&pixel;return VK_SUCCESS;
}
static void VKAPI_CALL unmap_memory(VkDevice d,VkDeviceMemory m)
{
    (void)d;(void)m;assert(mapped);mapped=0;++unmaps;
}
struct fake_device {struct {struct {PFN_vkMapMemory MapMemory;
    PFN_vkUnmapMemory UnmapMemory;} dispatch_table;} vk;};
#define ANV_FROM_HANDLE(type,name,handle) struct fake_device *name=(struct fake_device *)(handle)
#define CUBIT_TEAPOT_FRAME_COUNT 3
static uint64_t teapot_clock_ns(void){static uint64_t time;++time;return mode==7 && time%3==0?0:time*1000000;}
static void report(const char *format,...){(void)format;}
static int fake_sleep(unsigned usec){assert(usec==1000||usec==100000);++sleeps;return 0;}
#define usleep fake_sleep
#include "native-gallery-present.h"
void mesa_gallery_surface___elabb(void){assert(!initialized);++initialized;}
uint32_t cubit_test_gallery_frame(const void *source,uint32_t w,uint32_t h,uint32_t pitch)
{
    assert(initialized==1 && mapped && source==&pixel && w==800 && h==600 && pitch==3200);
    ++copies;
    if(mode==1)return 2; /* User close. */
    if(mode==2)return 15; /* Publication rejected. */
    if(mode==3 && copies<3)return 4; /* Two pending-retirement replies. */
    if(mode==5)return 4; /* Bounded unavailable writable frame. */
    return 0;
}
uint32_t cubit_test_gallery_close(void)
{
    assert(initialized==1 && !mapped);++closes;
    return closes<3; /* Retirement is polled, not treated as immediate. */
}
void cubit_test_gallery_rate(uint64_t value){(void)value;}
int main(int argc,char **argv)
{
    assert(argc==2);mode=(unsigned)atoi(argv[1]);assert(mode<=7);
    struct fake_device fake={.vk.dispatch_table={map_memory,unmap_memory}};
    VkResult result=present_completed_triangle((VkDevice)&fake,(VkDeviceMemory)1,
        mode==6?0:1920000,800,600,3200);
    if(mode==0||mode==3||mode==7){
        assert(result==VK_SUCCESS && closes==0 && maps==1 && unmaps==1);
        for(unsigned i=0;i<2;i++)assert(present_completed_triangle((VkDevice)&fake,
            (VkDeviceMemory)1,1920000,800,600,3200)==VK_SUCCESS);
        assert(maps==3 && unmaps==3 && copies==(mode==3?5:3));
    }else if(mode==1){assert(result==VK_EVENT_SET && maps==1 && unmaps==1);}
    else if(mode==2){assert(result==VK_ERROR_UNKNOWN && maps==1 && unmaps==1);}
    else if(mode==4){assert(result==VK_ERROR_MEMORY_MAP_FAILED && maps==1 && !unmaps && !copies);}
    else if(mode==5){assert(result==VK_ERROR_UNKNOWN && copies==1001 && maps==1 && unmaps==1);}
    else {assert(result==VK_ERROR_INITIALIZATION_FAILED && !maps && !copies);}
    assert(initialized==1 && !mapped && closes==3);
    if(mode==7)assert(!gallery_rate_valid);
    assert(sleeps==(mode==3?4:mode==5?1002:2));
    finish_gallery_window();assert(closes==4 && !mapped);
    printf("Gallery wrapper mode=%u PASS (host mocks only)\n",mode);
}
