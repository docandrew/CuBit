#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
/* CuAlloc's entry points for this hosted fixture (glyph mask storage). */
uint64_t cualloc_allocate(uint64_t bytes,uint64_t alignment){
 void *p=0; if(posix_memalign(&p,alignment,bytes))return 0; return (uint64_t)(uintptr_t)p;}
void cualloc_free(uint64_t item){free((void*)(uintptr_t)item);}
static unsigned fail;
void fail_code(unsigned code){fail=code;}
struct request {uint32_t font,code,n,d,w,h,pitch,capacity,phases,grid;};
struct metrics {uint32_t advance,height;};
uint32_t cubit_font_raster_mask(const struct request *r,void *pixels,struct metrics *m){
 if(fail==r->code)return 1; assert(r->capacity>=r->pitch*r->h*r->phases);
 for(unsigned y=0;y<r->h*r->phases;y++)memset((uint8_t*)pixels+y*r->pitch,127,r->w);
 *m=(struct metrics){r->w,r->h};return 0;}
