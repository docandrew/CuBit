#include <stdint.h>
#include <string.h>
#include <assert.h>
static unsigned fail;
void fail_code(unsigned code){fail=code;}
struct request {uint32_t font,code,n,d,w,h,pitch,capacity;};
struct metrics {uint32_t advance,height;};
uint32_t cubit_font_raster_mask(const struct request *r,void *pixels,struct metrics *m){
 if(fail==r->code)return 1; assert(r->capacity>=r->pitch*r->h);
 for(unsigned y=0;y<r->h;y++)memset((uint8_t*)pixels+y*r->pitch,127,r->w);
 *m=(struct metrics){r->w,r->h};return 0;}
