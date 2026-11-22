#include <stdint.h>
static uint32_t create_result,close_result,creates,closes;
void pipeline_mock_set(uint32_t create,uint32_t close)
{create_result=create;close_result=close;creates=closes=0;}
uint32_t pipeline_mock_creates(void){return creates;}
uint32_t pipeline_mock_closes(void){return closes;}
uint32_t cubit_vulkan_device_pipeline_create(void){++creates;return create_result;}
uint32_t cubit_vulkan_device_pipeline_close(void){++closes;return close_result;}

void *cubit_vulkan_device_source_request(uint32_t slot,void *image)
{(void)slot;return image;}
