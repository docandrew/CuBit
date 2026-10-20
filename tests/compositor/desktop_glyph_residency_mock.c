#include <assert.h>
#include <stdint.h>
struct request {uint32_t face,code,n,d,w,h,pitch,capacity,phases,grid;};
struct metrics {uint32_t advance,height;};
/* Compositor_Glyph_Layout.Face_Count: Sans, Monospace, Sans_Semibold. */
#define FACE_COUNT 3u
static unsigned calls,fault;
void residency_font_set(unsigned value){calls=0;fault=value;}
unsigned residency_font_calls(void){return calls;}
uint32_t cubit_font_raster_mask(const struct request *r,void *pixels,struct metrics *m)
{
    ++calls;assert(pixels==(void *)(uintptr_t)4096);
    assert(r->face<FACE_COUNT&&r->code>=32&&r->code<=126&&r->pitch>=r->w&&r->phases==4&&r->capacity==r->pitch*r->h*r->phases);
    if(fault)return 1;
    *m=(struct metrics){r->w/2,r->h};return 0;
}
