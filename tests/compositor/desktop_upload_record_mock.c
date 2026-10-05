#include <stdint.h>
#include <assert.h>
static uint32_t result,calls;
void desktop_upload_record_set(uint32_t value){result=value;calls=0;}
uint32_t desktop_upload_record_calls(void){return calls;}
uint32_t cubit_vulkan_upload_record(void *c,void *u,void *s,const uint32_t *r)
{assert(c&&u&&s&&r&&r[4]>0&&r[5]>0);++calls;return result;}
