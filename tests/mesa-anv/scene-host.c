/* Reuse the exact triangle and consumer oracle; Ada owns program elaboration. */
#define main run_mesa_scene_host
#include "triangle-host-test.c"

#ifdef CUBIT_SCENE_FAULTS
uint32_t __real_cubit_vulkan_record_affine(void *,const struct cubit_mesa_affine *,
    const struct cubit_vulkan_coefficients *,uint32_t,uint32_t,uint32_t,uint32_t);
uint32_t __wrap_cubit_vulkan_record_affine(void *source,const struct cubit_mesa_affine *draw,
    const struct cubit_vulkan_coefficients *coefficients,uint32_t width,uint32_t height,
    uint32_t mask,uint32_t argb)
{
    if(scene_record_failures){--scene_record_failures;return 1;}
    return __real_cubit_vulkan_record_affine(source,draw,coefficients,width,height,mask,argb);
}
#endif
