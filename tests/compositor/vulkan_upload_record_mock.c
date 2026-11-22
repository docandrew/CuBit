#include <stdint.h>
#include <assert.h>
static uint32_t result,calls;
void upload_record_mock_set(uint32_t r){result=r;calls=0;}
uint32_t upload_record_mock_calls(void){return calls;}
uint32_t cubit_vulkan_upload_record(void *c,void *u,void *s,const uint32_t *r)
{assert(c&&u&&s);assert(r[0]==8&&r[1]==4&&r[2]==0&&r[3]==0&&r[4]==8&&r[5]==4&&r[6]==0&&r[7]==0&&r[8]==0&&r[9]==1);++calls;return result;}
