#include <stdint.h>
static uint32_t status[14],calls[14];
static void *last_pass;
static int64_t origin[2];
int64_t submission_mock_origin(uint32_t i){return i<2?origin[i]:0;}
static uint32_t geometry[17];
struct affine_geometry { int64_t x,y; uint32_t v[10]; };
uint32_t submission_mock_geometry(uint32_t i){return i<17?geometry[i]:0;}
void *submission_mock_last_pass(void){return last_pass;}
void submission_mock_reset(void){for(unsigned i=0;i<14;i++){status[i]=0;calls[i]=0;}for(unsigned i=0;i<17;i++)geometry[i]=0;}
void submission_mock_set(uint32_t i,uint32_t value){if(i<14)status[i]=value;}
uint32_t submission_mock_calls(uint32_t i){return i<14?calls[i]:0;}
static uint32_t reply(unsigned i){++calls[i];return status[i];}
uint32_t cubit_vulkan_submission_start(void *p){(void)p;return reply(0);}
uint32_t cubit_vulkan_submission_seal(void *p){(void)p;return reply(1);}
uint32_t cubit_vulkan_submission_submit(void *p){(void)p;return reply(2);}
uint32_t cubit_vulkan_submission_poll(void *p){(void)p;return reply(3);}
uint32_t cubit_vulkan_submission_cancel(void *p){(void)p;return reply(4);}
uint32_t cubit_vulkan_submission_matches(void *p,void *d){(void)p;(void)d;return reply(5);}
uint32_t cubit_vulkan_record_affine(void *p,const void *d,const void *c,uint32_t w,uint32_t h,uint32_t m,uint32_t t)
{const struct affine_geometry *a=d;(void)p;(void)c;(void)w;(void)h;(void)m;(void)t;
 geometry[4]=a->v[5];geometry[5]=a->v[6];geometry[6]=a->v[7];geometry[7]=a->v[8];
 geometry[8]=a->v[0];geometry[9]=a->v[1];geometry[11]=a->v[2];geometry[12]=a->v[3];
 geometry[13]=m;geometry[14]=t;geometry[15]=a->v[4];geometry[16]=a->v[9];
 origin[0]=a->x;origin[1]=a->y;return reply(6);}
uint32_t cubit_vulkan_submission_begin_scene(void *p,void *d,uint32_t w,uint32_t h)
{(void)p;last_pass=d;(void)w;(void)h;return reply(7);}
uint32_t cubit_vulkan_submission_end_scene(void *p){(void)p;return reply(8);}

uint32_t cubit_vulkan_source_import(void *description,void **draw)
{uint32_t code=reply(9);*draw=code==42?0:description;return code==42?0:code;}
uint32_t cubit_vulkan_source_release(void *draw)
{(void)draw;return reply(10);}

uint32_t cubit_vulkan_targets_create(void *description,void **a,void **b,void **c)
{
    (void)description;uint32_t code=reply(11);
    *a=code==42?0:(void *)(uintptr_t)101;*b=(void *)(uintptr_t)102;
    *c=code==43?*b:(void *)(uintptr_t)103;return code==42||code==43?0:code;
}
uint32_t cubit_vulkan_targets_release(void *description){(void)description;return reply(12);}

uint32_t cubit_vulkan_submission_fill(void *p,uint32_t w,uint32_t h,uint32_t l,uint32_t t,uint32_t r,uint32_t b,uint32_t rgb)
{(void)p;(void)w;(void)h;geometry[0]=l;geometry[1]=t;geometry[2]=r;geometry[3]=b;geometry[10]=rgb;return reply(13);}
