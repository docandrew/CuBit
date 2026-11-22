#include "native_scene_bridge.h"
#include <assert.h>
#include <stddef.h>
extern void submission_mock_reset(void);
extern void submission_mock_set(uint32_t,uint32_t);
extern void image_mock(uint64_t,uint32_t,uint32_t,uint32_t,uint32_t);
/* Real C->Ada ABI call sequence, using deterministic native-call mocks. */
void native_scene_c_abi_test(void)
{
    uint32_t result=99,slot=99;
    submission_mock_reset();image_mock(4096,1,0,0,0);
    cubit_native_scene_open((void *)(uintptr_t)44,(void *)(uintptr_t)1,
        (void *)(uintptr_t)2,(void *)(uintptr_t)3,(void *)(uintptr_t)99,
        (void *)(uintptr_t)55,1,64,64,&result);assert(result==0);
    cubit_native_scene_begin(&slot,&result);assert(result==0&&slot>=1&&slot<=3);
    cubit_native_scene_record(&result);assert(result==0);
    cubit_native_scene_submit(&result);assert(result==0);
    submission_mock_set(3,1);
    cubit_native_scene_poll(&result);assert(result==1);
    cubit_native_scene_cancel(&result);assert(result==2);
    cubit_native_scene_close(&result);assert(result==2);
    submission_mock_set(3,0);
    cubit_native_scene_poll(&result);assert(result==0);
    cubit_native_scene_close(&result);assert(result==2);
    cubit_native_scene_release(&result);assert(result==0);
    cubit_native_scene_close(&result);assert(result==0);
}
