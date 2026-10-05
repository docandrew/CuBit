#include "vulkan_checker_request.h"
_Static_assert(sizeof(struct cubit_vulkan_checker_request)==64,"checker request ABI");
static void *last_context;
void *checker_mock_context(void){return last_context;}
#include <assert.h>
static uint32_t status,calls,fail_at;
static struct cubit_vulkan_checker_request last;
void checker_mock_set(uint32_t code){status=code;calls=0;fail_at=0;}
void checker_mock_fail_at(uint32_t index){fail_at=index;}
uint32_t checker_mock_calls(void){return calls;}
int64_t checker_mock_field(uint32_t i)
{
    const int64_t values[]={last.left,last.top,last.right,last.bottom,last.origin_x,last.origin_y,
        last.numerator,last.denominator,last.width,last.height,last.rotation,
        last.clip_x,last.clip_y,last.clip_w,last.clip_h,last.rgb};
    assert(i<16);return values[i];
}
uint32_t cubit_vulkan_device_checker_record(void *context,const struct cubit_vulkan_checker_request *r)
{assert(context);assert(r);last_context=context;last=*r;++calls;return fail_at&&calls<fail_at?0:status;}
