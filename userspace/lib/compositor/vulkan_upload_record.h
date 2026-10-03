#ifndef CUBIT_VULKAN_UPLOAD_RECORD_H
#define CUBIT_VULKAN_UPLOAD_RECORD_H
#include "vulkan_upload_buffer.h"
#include "vulkan_owned_image.h"
#include "vulkan_submission.h"
struct cubit_vulkan_upload_region {
    uint32_t image_width,image_height,x,y,width,height,offset,row_pixels,mask,discard;
};
/* Private matching live objects, command recording OUTSIDE any render pass.
 * Producer finished before submit; no CPU writes/release until completion.
 * discard means previous image contents undefined. Partial cold uploads cannot
 * be published until every required pixel is initialized by completed work.
 * No submission/wait/allocation/copy of CPU pixels. 1 rejects before commands. */
uint32_t cubit_vulkan_upload_record(void *,void *,void *,const struct cubit_vulkan_upload_region *);
#endif
