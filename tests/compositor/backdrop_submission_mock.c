#include <stdint.h>
struct geometry { int64_t x,y; uint64_t w,h; uint32_t cx,cy,cw,ch,sw,sh; };
static uint32_t calls, status;
static struct geometry recorded[8];
static uint32_t fills_before[8];
extern uint32_t submission_mock_calls(uint32_t);
void backdrop_submission_reset(uint32_t value){calls=0;status=value;}
uint32_t backdrop_submission_calls(void){return calls;}
uint64_t backdrop_submission_geometry(uint32_t draw,uint32_t field)
{
    if(draw>=calls||draw>=8)return 0;
    const struct geometry *d=&recorded[draw];
    switch(field){case 0:return (uint64_t)d->x;case 1:return (uint64_t)d->y;
    case 2:return d->w;case 3:return d->h;case 4:return d->cx;case 5:return d->cy;
    case 6:return d->cw;case 7:return d->ch;case 8:return d->sw;case 9:return d->sh;
    case 10:return fills_before[draw];default:return 0;}
}
uint32_t cubit_vulkan_record_backdrop(void *context,const void *description,uint32_t w,uint32_t h)
{
    (void)context;(void)w;(void)h;
    if(calls<8){recorded[calls]=*(const struct geometry *)description;fills_before[calls]=submission_mock_calls(13);}
    ++calls;return status;
}
