#include <stdint.h>
static uint64_t bytes;static uint32_t types,prepare,bind,release,null_map,binds,releases;
void upload_mock_set(uint64_t b,uint32_t t,uint32_t p,uint32_t a,uint32_t r,uint32_t n)
{bytes=b;types=t;prepare=p;bind=a;release=r;null_map=n;binds=releases=0;}
uint32_t upload_mock_binds(void){return binds;}
uint32_t upload_mock_releases(void){return releases;}
uint32_t cubit_vulkan_upload_prepare(void *p,uint32_t c,uint64_t *b,uint32_t *t)
{(void)p;(void)c;*b=bytes;*t=types;return prepare;}
uint32_t cubit_vulkan_upload_bind(void *p,uint64_t b,uint32_t t,void **m)
{(void)p;(void)b;(void)t;++binds;*m=null_map?0:(void *)(uintptr_t)4096;return bind;}
uint32_t cubit_vulkan_upload_release(void *p){(void)p;++releases;return release;}
