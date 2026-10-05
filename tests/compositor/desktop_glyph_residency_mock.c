#include <assert.h>
#include <stdint.h>
struct request {uint32_t face,code,n,d,w,h,pitch,capacity;};
struct metrics {uint32_t advance,height;};
static unsigned calls,fault;
void residency_font_set(unsigned value){calls=0;fault=value;}
unsigned residency_font_calls(void){return calls;}
uint32_t cubit_font_raster_mask(const struct request *r,void *pixels,struct metrics *m)
{
    ++calls;assert(pixels==(void *)(uintptr_t)4096);
    assert(r->face<=1&&r->code>=32&&r->code<=126&&r->pitch>=r->w&&r->capacity==r->pitch*r->h);
    if(fault)return 1;
    *m=(struct metrics){r->w/2,r->h};return 0;
}
