#include <assert.h>
#include <stdint.h>
struct request {uint32_t font,code,n,d,w,h,pitch,capacity;};
struct metrics {uint32_t advance,height;};
static unsigned fault,calls;
void glyph_upload_mock_set(unsigned value){fault=value;calls=0;}
unsigned glyph_upload_mock_calls(void){return calls;}
uint32_t cubit_font_raster_mask(const struct request *r,void *pixels,struct metrics *m)
{
    ++calls;
    assert(pixels==(void *)(uintptr_t)4096);
    assert(r->font==0&&r->code==65&&r->n==65&&r->d==4);
    assert(r->w==40&&r->h==22&&r->pitch==48&&r->capacity==1056);
    *m=(struct metrics){fault==2?0:fault==7?41:20,fault==3?21:22};
    return fault==1?1:0;
}
