#include "vulkan_backdrop.h"
#include <stddef.h>
/* Same push ABI as the affine engine; mask=2 selects endpoint bilinear mode. */
struct backdrop_push {
    uint32_t origin_x[2],origin_y[2],width[2],height[2];
    int32_t source_size[4];
    float unused_tint[4];
    uint32_t mode, padding[3], region[4];
};
_Static_assert(sizeof(struct cubit_vulkan_backdrop)==56,"SPARK backdrop ABI");
_Static_assert(sizeof(struct backdrop_push)==96,"affine push ABI");
_Static_assert(offsetof(struct backdrop_push,mode)==64,"affine mode ABI");
uint32_t cubit_vulkan_record_backdrop(void *borrowed,
    const struct cubit_vulkan_backdrop *d,uint32_t width,uint32_t height)
{
    const struct cubit_vulkan_affine_draw *b=borrowed;
    if(!b||!d||!b->engine||!b->command||!b->source)return 1;
    const struct cubit_vulkan_affine_engine *e=b->engine;
    if(!e->device||!e->layout||!e->pipeline[0]||!e->bind_pipeline||!e->bind_descriptors||
       !e->viewport||!e->scissor||!e->constants||!e->draw)return 1;
    const uint64_t maximum=UINT64_C(65535)*65535;
    if(!width||width>65535||!height||height>65535||b->width!=width||b->height!=height||
       !d->width||d->width>maximum||!d->height||d->height>maximum||
       d->left<-(int64_t)maximum||d->left>(int64_t)maximum||
       d->top<-(int64_t)maximum||d->top>(int64_t)maximum||
       !d->source_w||d->source_w>65535||!d->source_h||d->source_h>65535||
       d->clip_x>=width||d->clip_y>=height||!d->clip_w||!d->clip_h||
       d->clip_w>width-d->clip_x||d->clip_h>height-d->clip_y)return 1;
    struct backdrop_push push={.mode=2};
    const uint64_t values[]={(uint64_t)d->left,(uint64_t)d->top,d->width,d->height};
    uint32_t *pairs[]={push.origin_x,push.origin_y,push.width,push.height};
    for(unsigned i=0;i<4;i++){pairs[i][0]=(uint32_t)values[i];pairs[i][1]=(uint32_t)(values[i]>>32);}
    push.source_size[0]=(int32_t)d->source_w;push.source_size[1]=(int32_t)d->source_h;
    const VkViewport viewport={0,0,(float)width,(float)height,0,1};
    const VkRect2D clip={{(int32_t)d->clip_x,(int32_t)d->clip_y},{d->clip_w,d->clip_h}};
    e->bind_pipeline(b->command,VK_PIPELINE_BIND_POINT_GRAPHICS,e->pipeline[0]);
    e->bind_descriptors(b->command,VK_PIPELINE_BIND_POINT_GRAPHICS,e->layout,0,1,&b->source,0,NULL);
    e->viewport(b->command,0,1,&viewport);e->scissor(b->command,0,1,&clip);
    e->constants(b->command,e->layout,VK_SHADER_STAGE_VERTEX_BIT|VK_SHADER_STAGE_FRAGMENT_BIT,0,sizeof push,&push);
    e->draw(b->command,6,1,0,0);
    return 0;
}
