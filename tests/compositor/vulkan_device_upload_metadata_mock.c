#include <stdint.h>
static uint32_t fail,calls;
void upload_metadata_mock_set(uint32_t f){fail=f;calls=0;}
uint32_t upload_metadata_mock_calls(void){return calls;}
void *cubit_vulkan_device_upload_prepare(void){++calls;return fail?0:(void *)(uintptr_t)800;}
