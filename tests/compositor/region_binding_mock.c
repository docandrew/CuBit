#include "compositor.h"
struct cubit_vulkan_coefficients { int64_t u0,ux,uy,v0,vx,vy,ud,vd; };
struct cubit_vulkan_source_region { uint32_t x,y,width,height,image_width,image_height; };
#include <assert.h>
#include <stddef.h>
_Static_assert(sizeof(struct cubit_vulkan_source_region)==24,"region ABI");
_Static_assert(offsetof(struct cubit_vulkan_source_region,image_height)==20,"last word ABI");
static uint32_t status,calls,fields[9];
void region_mock_reset(uint32_t s){status=s;calls=0;}
uint32_t region_mock_calls(void){return calls;}
uint32_t region_mock_field(uint32_t i){assert(i<9);return fields[i];}
uint32_t cubit_vulkan_record_affine_region(void *context,const struct cubit_mesa_affine *d,
 const struct cubit_vulkan_coefficients *c,uint32_t w,uint32_t h,uint32_t mask,uint32_t tint,
 const struct cubit_vulkan_source_region *r)
{
 assert(context!=NULL&&d&&c&&r&&mask==0&&tint==0);
 assert(c->ud>0&&c->vd>0);
 calls++;fields[0]=r->x;fields[1]=r->y;fields[2]=r->width;fields[3]=r->height;
 fields[4]=r->image_width;fields[5]=r->image_height;fields[6]=w;fields[7]=h;fields[8]=d->over;
 return status;
}
