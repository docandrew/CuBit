/* Hosted fault fixture, not a renderer. Real Mesa/raster pixels are tested natively. */
#include <assert.h>
#include <stdint.h>
#include <string.h>
#include "../../userspace/lib/compositor/compositor.h"
struct view { struct cubit_mesa_image image; unsigned live, mask; };
static struct view views[140];
static unsigned fault, counts[6];
void glyph_mock_reset(void) { memset(views,0,sizeof views); memset(counts,0,sizeof counts); fault=0; }
void glyph_mock_fault(uint32_t value) { fault=value; }
uint32_t glyph_mock_stat(uint32_t index) { assert(index<6); return counts[index]; }
void *cubit_mesa_create(void) { return counts; }
void cubit_mesa_destroy(void *ctx) { assert(ctx==counts && counts[4]==0); }
void *cubit_mesa_import(void *ctx,const struct cubit_mesa_image *image)
{
    assert(ctx==counts);
    for(unsigned i=0;i<140;i++) if(!views[i].live) {
        views[i]=(struct view){*image,1,0}; counts[4]++; return &views[i];
    }
    return 0;
}
void *cubit_mesa_import_mask(void *ctx,const struct cubit_mesa_image *image,uint64_t capacity)
{
    if(fault==2) return 0;
    assert(capacity >= (uint64_t)image->pitch*image->height);
    struct view *v=cubit_mesa_import(ctx,image); if(v) {v->mask=1;counts[1]++;} return v;
}
uint32_t cubit_mesa_release(void *ctx,void *handle)
{
    struct view *v=handle; assert(ctx==counts && v->live);
    if(fault==6) return 3;
    v->live=0;counts[4]--;counts[2]++;return 0;
}
uint32_t cubit_mesa_draw_mask_batch(void *ctx,void *target,const struct cubit_mesa_mask_command *cmd,uint32_t count)
{
    struct view *t=target;assert(ctx==counts && t->live && !t->mask && count<=32);
    for(unsigned i=0;i<count;i++) {struct view *s=cmd[i].source;
        assert(s->live && s->mask && s->image.pixels!=t->image.pixels);
        assert(((uint8_t*)s->image.pixels)[0]==0x7f);
    }
    counts[3]++;counts[5]+=count;
    return fault==3?1:fault==4?2:fault==5?3:0;
}
uint32_t cubit_mesa_draw(void *a,void *b,void *c,const struct cubit_mesa_draw *d) {(void)a;(void)b;(void)c;(void)d;return 0;}
uint32_t cubit_mesa_draw_mask(void *a,void *b,void *c,const struct cubit_mesa_affine *d,const struct cubit_mesa_quad *q,uint32_t tint)
{(void)a;(void)b;(void)c;(void)d;(void)q;(void)tint;return 0;}
struct request {uint32_t font,code,n,d,w,h,pitch,capacity;};
struct metrics {uint32_t advance,height;};
uint32_t cubit_font_raster_mask(const struct request *r,void *pixels,struct metrics *m)
{
    counts[0]++;if(fault==1) return 1;
    assert(r->capacity >= r->pitch*r->h);
    for(unsigned y=0;y<r->h;y++) memset((uint8_t*)pixels+y*r->pitch,0x7f,r->w);
    *m=(struct metrics){r->w,r->h};return 0;
}

uint32_t cubit_mesa_draw_affine(void *a,void *b,void *c,const struct cubit_mesa_affine *d,const struct cubit_mesa_quad *q)
{(void)a;(void)b;(void)c;(void)d;(void)q;return 0;}
