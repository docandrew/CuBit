#include <stdint.h>
#include <assert.h>
static uint64_t size;
static uint32_t types,prep,bind_result,release_result,binds,releases,selected,refusals;
void image_mock(uint64_t bytes,uint32_t mask,uint32_t p,uint32_t b,uint32_t r)
{size=bytes;types=mask;prep=p;bind_result=b;release_result=r;binds=releases=refusals=0;selected=32;}
/* The next n binds are refused cleanly (1: memory allocation failed, image
 * and backing destroyed), as when the GPU driver refuses the VM bind for
 * capacity (BUG-001). */
void image_mock_refuse_next(uint32_t n){refusals=n;}
uint32_t image_mock_binds(void){return binds;}
uint32_t image_mock_releases(void){return releases;}
uint32_t image_mock_selected(void){return selected;}
uint32_t cubit_vulkan_owned_image_prepare(void *r,uint64_t *b,uint32_t *t)
{(void)r;*b=size;*t=types;return prep;}
uint32_t cubit_vulkan_owned_image_bind(void *r,uint64_t b,uint32_t t)
{(void)r;assert(b==size);assert(t<32 && (types&(UINT32_C(1)<<t)));++binds;selected=t;
 if(refusals){--refusals;return 1;}return bind_result;}
uint32_t cubit_vulkan_owned_image_release(void *r)
{(void)r;++releases;return release_result;}
