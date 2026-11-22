#include <stdint.h>
struct result {void *image;uint32_t allowed;};
static uint32_t result,calls;
void source_metadata_mock_set(uint32_t value){result=value;calls=0;}
uint32_t source_metadata_mock_calls(void){return calls;}
uint32_t cubit_vulkan_device_source_prepare(uint32_t slot,uint32_t w,uint32_t h,uint32_t mask,struct result *out)
{
    (void)w;(void)h;(void)mask;++calls;
    /* Deliberately dirty failure output must not escape the Ada boundary. */
    out->image=(void *)(uintptr_t)(result?0xdead:400+slot*8);out->allowed=result?UINT32_MAX:1;
    return result;
}
