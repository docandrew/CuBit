#include <stdint.h>
#include <stddef.h>
/* Link-only stubs for the readback, preview and diagnostic paths that the
 * hosted Desktop Vulkan tests never enter; each reports a clean rejection. */
void *cubit_vulkan_device_readback_prepare(void){return NULL;}
uint32_t cubit_vulkan_readback_prepare(void *p,uint32_t c,uint64_t *b,uint32_t *t)
{(void)p;(void)c;*b=0;*t=0;return 1;}
uint32_t cubit_vulkan_owned_targets_record_readback(void *a,void *b,void *c,void *d)
{(void)a;(void)b;(void)c;(void)d;return 1;}
uint32_t cubit_vulkan_owned_targets_record_readback_regions(void *a,void *b,void *c,void *d,void *e)
{(void)a;(void)b;(void)c;(void)d;(void)e;return 1;}
uint32_t cubit_vulkan_pipeline_last_failure(uint32_t *s,uint32_t *i,int32_t *r)
{*s=0;*i=0;*r=0;return 0;}
uint32_t cubit_vulkan_record_preview(void *a,const void *b){(void)a;(void)b;return 1;}
