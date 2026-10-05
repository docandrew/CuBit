#include <stdint.h>
static uint32_t creation,retirement,releases;
void context_mock_set(uint32_t a,uint32_t b){creation=a;retirement=b;releases=0;}
uint32_t context_mock_releases(void){return releases;}
uint32_t cubit_vulkan_context_create(void *request,void **out)
{*out=creation==42?0:request;return creation==42?0:creation;}
uint32_t cubit_vulkan_context_release(void *request){(void)request;releases++;return retirement;}
