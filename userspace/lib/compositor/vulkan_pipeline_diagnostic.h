#ifndef CUBIT_VULKAN_PIPELINE_DIAGNOSTIC_H
#define CUBIT_VULKAN_PIPELINE_DIAGNOSTIC_H
#include <stdint.h>
/* Startup-thread-only scalar evidence. Never logs, allocates, or calls Mesa.
 * First failure survives cleanup and later wrapper failures. No pointers. */
extern void cubit_vulkan_pipeline_failure(uint32_t, uint32_t, int32_t)
    __attribute__((weak));
static inline void pipeline_failure(uint32_t stage,uint32_t index,int32_t result)
{
    if(cubit_vulkan_pipeline_failure)
        cubit_vulkan_pipeline_failure(stage,index,result);
}
#ifdef CUBIT_PIPELINE_DIAGNOSTIC_STORAGE
static struct { uint32_t valid,stage,index; int32_t result; } pipeline_diagnostic;
void cubit_vulkan_pipeline_failure(uint32_t stage,uint32_t index,int32_t result)
{
    if(!pipeline_diagnostic.valid && result!=0){
        pipeline_diagnostic.stage=stage;pipeline_diagnostic.index=index;
        pipeline_diagnostic.result=result;pipeline_diagnostic.valid=1;
    }
}
uint32_t cubit_vulkan_pipeline_last_failure(uint32_t *stage,uint32_t *index,int32_t *result)
{
    if(!stage||!index||!result)return 0;
    *stage=pipeline_diagnostic.stage;*index=pipeline_diagnostic.index;
    *result=pipeline_diagnostic.result;return pipeline_diagnostic.valid;
}
#endif
#endif
