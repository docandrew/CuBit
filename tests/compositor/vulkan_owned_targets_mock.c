#include <stdint.h>
static uint32_t failure, prepare_failure;
void owned_target_mock_prepare(uint32_t value){prepare_failure=value;}
void owned_target_mock_bind(uint32_t value){failure=value;prepare_failure=0;}
uint32_t cubit_vulkan_owned_targets_bind(void *d,void *a,void *b,void *c,void *s)
{(void)d;(void)a;(void)b;(void)c;(void)s;return failure;}

uint32_t cubit_vulkan_owned_targets_prepare_frame(void *d,void *s,uint32_t slot,uint32_t w,uint32_t h,uint32_t discard)
{(void)d;(void)s;(void)slot;(void)w;(void)h;(void)discard;return prepare_failure;}
