#include "anv_cubit_memory.h"
#include "native_gpu_mapping.h"
#include "native_gpu_presenter.h"
#include <assert.h>
#include <setjmp.h>
#include <stdarg.h>
#include <unistd.h>
static unsigned mode, creates, attaches, presents, destroys, releases, waits;
static bool unhealthy;
static jmp_buf retained;
static void report(const char *format, ...) { (void)format; }
uint64_t cubit_test_render_slot(void);
#include "native-triangle-present.h"
uint64_t cubit_test_render_slot(void) { return 7; }
uint64_t cubit_test_desktop_slot(void) { return 12; }
VkResult anv_cubit_check_status(struct vk_device *device)
{ (void)device; return unhealthy ? VK_ERROR_DEVICE_LOST : VK_SUCCESS; }
uint64_t cubit_test_triangle_create(void) { creates++; return mode==1 ? 0 : 15; }
uint32_t cubit_test_triangle_present(uint64_t surface)
{ assert(surface==15 && attaches==1); presents++; return mode==2 ? 1 : 0; }
uint32_t cubit_test_triangle_destroy(uint64_t surface)
{ assert(surface==15 && attaches==1); destroys++; return mode==3 ? 1 : 0; }
uint32_t cubit_presenter_attach_completed_linear(struct cubit_presenter *p,
   uint64_t render, uint64_t desktop, uint32_t bo, uint64_t surface,
   uint64_t offset, uint32_t width, uint32_t height, uint32_t pitch)
{
   assert(render==7 && desktop==12 && bo==3 && surface==15 && offset==0);
   assert(width==64 && height==64 && pitch==256 && p->state==CUBIT_PRESENTER_EMPTY);
   attaches++;
   p->state=mode==4 ? CUBIT_PRESENTER_RETIRING : CUBIT_PRESENTER_ATTACHED;
   return mode==4 ? 5 : 0;
}
uint32_t cubit_presenter_release(struct cubit_presenter *p)
{
   (void)p;
   assert(destroys==1); releases++;
   if (mode==5) return 5;
   return releases<3 ? 4 : 0;
}
int usleep(useconds_t delay)
{
   if (delay==5000000) assert(presents==1 && !destroys && !releases);
   else { assert(delay==100000 && destroys==1); waits++; }
   if (mode==5 && delay==100000) longjmp(retained,1);
   return 0;
}
int main(void)
{
   struct cubit_cpu_mapping_tracker tracker={.slot=7};
   struct anv_device device={0};
   struct anv_bo bo={.gem_handle=3,.size=16384,.actual_size=16384};
   struct anv_device_memory memory={0};
   device.vk.base.type=VK_OBJECT_TYPE_DEVICE;
   memory.vk.base.type=VK_OBJECT_TYPE_DEVICE_MEMORY;
   device.cubit_cpu_mappings=&tracker;
   memory.bo=&bo; memory.vk.size=16384;
   const VkDevice d=anv_device_to_handle(&device);
   const VkDeviceMemory m=anv_device_memory_to_handle(&memory);
   for (mode=0; mode<6; mode++) {
      creates=attaches=presents=destroys=releases=waits=0;
      if (setjmp(retained)) {
         assert(mode==5 && releases==1 && destroys==1 && waits==1);
         continue; /* Escaped a deliberately retained, non-returning consumer. */
      }
      VkResult result=present_completed_triangle(d,m,16384,64,64,256);
      assert(mode!=5);
      assert(result==(mode==0 ? VK_SUCCESS : mode==1 ?
                      VK_ERROR_INITIALIZATION_FAILED : VK_ERROR_UNKNOWN));
      assert(creates==1);
      if (mode==1) assert(!attaches && !destroys && !releases);
      else assert(attaches==1 && destroys==1 && releases==3 && waits==2);
   }
   mode=0;
   for (unsigned invalid=0; invalid<19; invalid++) {
      creates=attaches=presents=destroys=releases=waits=0;
      unhealthy=invalid==0;
      tracker.lost=invalid==1;
      memory.map=invalid==2 ? (void *)1 : NULL;
      bo.slab_parent=invalid==3 ? &bo : NULL;
      bo.from_host_ptr=invalid==4;
      bo.map=invalid==5 ? (void *)1 : NULL;
      bo.alloc_flags=invalid==6 ? ANV_BO_ALLOC_EXTERNAL : 0;
      device.cubit_cpu_mappings=invalid==7 ? NULL : &tracker;
      tracker.slot=invalid==8 ? 8 : 7;
      memory.bo=invalid==9 ? NULL : &bo;
      memory.vk.size=invalid==10 ? 16383 : 16384;
      bo.gem_handle=invalid==11 ? 0 : 3;
      bo.size=invalid==12 ? 16383 : 16384;
      bo.actual_size=invalid==13 ? 16383 : 16384;
      const uint64_t bytes=invalid==14 ? 16383 : 16384;
      const uint32_t width=invalid==15 ? 0 : invalid==16 ? 63 : 64;
      const uint32_t height=invalid==17 ? 63 : 64;
      const uint32_t pitch=invalid==18 ? 255 : 256;
      assert(present_completed_triangle(d,m,bytes,width,height,pitch)!=VK_SUCCESS);
      /* Reject locally, before creating a Desktop window or borrowing any
       * backing. None of these failures should need asynchronous retirement. */
      assert(!creates && !attaches && !presents && !destroys && !releases && !waits);
   }
   puts("Triangle consumer PASS: cleanup order, pending/uncertain retention, 19 invalid backing/geometry cases rejected without side effects");
}
