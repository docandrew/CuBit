/* Mesa ABI adapter only. SPARK owns admission, geometry and failure policy. */
#include "compositor.h"
#include <stdlib.h>
#include <string.h>
#include <cubit/debug.h>
#include "pipe/p_context.h"
#include "pipe/p_screen.h"
#include "pipe/p_state.h"
#include "gallium/drivers/softpipe/sp_public.h"
#include "frontend/sw_winsys.h"
#include "util/u_simple_shaders.h"
#include "util/u_draw.h"
#include "util/u_inlines.h"
#include "util/detect_os.h"
_Static_assert(DETECT_OS_CUBIT && !DETECT_OS_LINUX, "native Mesa ABI required");

struct sw_displaytarget { struct cubit_mesa_image image; unsigned maps; };
struct adapter {
    struct sw_winsys winsys;
    struct pipe_screen *screen;
    struct pipe_context *pipe;
    void *vs, *fs, *blend[2], *raster, *depth, *elements, *sampler;
    unsigned fault;
};
struct imported {
    struct adapter *owner;
    struct pipe_resource *resource;
    struct pipe_sampler_view *view;
    struct sw_displaytarget target;
};
static void failure(const char *stage)
{
    const char *prefix="desktop Mesa FFI: ";
    cubit_debug_write(prefix,strlen(prefix));
    cubit_debug_write(stage,strlen(stage));
    cubit_debug_write("\n",1);
}
static bool supported(struct sw_winsys *w,unsigned bind,enum pipe_format f)
{ (void)w;(void)bind; return f==PIPE_FORMAT_B8G8R8A8_UNORM; }
static struct sw_displaytarget *create_target(struct sw_winsys *w,unsigned bind,
    enum pipe_format f,unsigned width,unsigned height,unsigned align,
    const void *private,unsigned *stride)
{
    (void)bind;(void)align;
    struct sw_displaytarget *t=(void *)private;
    if (!t || !supported(w,bind,f) || width!=t->image.width || height!=t->image.height)
        return NULL;
    *stride=t->image.pitch; return t;
}
static void *map_target(struct sw_winsys *w,struct sw_displaytarget *t,unsigned flags)
{
    if ((flags & PIPE_MAP_WRITE) && !t->image.writable) {
        ((struct adapter *)w)->fault=1; return NULL;
    }
    ++t->maps; return t->image.pixels;
}
static void unmap_target(struct sw_winsys *w,struct sw_displaytarget *t)
{
    if (!t->maps) ((struct adapter *)w)->fault=1;
    else --t->maps;
}
static void destroy_target(struct sw_winsys *w,struct sw_displaytarget *t)
{ if(t->maps) ((struct adapter *)w)->fault=1; }
static void destroy_winsys(struct sw_winsys *w) { (void)w; }

void cubit_mesa_destroy(void *opaque)
{
    struct adapter *a=opaque;
    if(!a) return;
    if(a->pipe) {
        struct pipe_context *p=a->pipe;
        p->bind_vs_state(p,NULL); p->bind_fs_state(p,NULL);
        p->bind_blend_state(p,NULL); p->bind_rasterizer_state(p,NULL);
        p->bind_depth_stencil_alpha_state(p,NULL); p->bind_vertex_elements_state(p,NULL);
        p->bind_sampler_states(p,MESA_SHADER_FRAGMENT,0,1,(void *[]){NULL});
        if(a->vs) p->delete_vs_state(p,a->vs);
        if(a->fs) p->delete_fs_state(p,a->fs);
        for(unsigned i=0;i<2;++i) if(a->blend[i]) p->delete_blend_state(p,a->blend[i]);
        if(a->raster) p->delete_rasterizer_state(p,a->raster);
        if(a->depth) p->delete_depth_stencil_alpha_state(p,a->depth);
        if(a->elements) p->delete_vertex_elements_state(p,a->elements);
        if(a->sampler) p->delete_sampler_state(p,a->sampler);
        p->destroy(p);
    }
    if(a->screen) a->screen->destroy(a->screen);
    free(a);
}
void *cubit_mesa_create(void)
{
#ifdef CUBIT_MESA_FAIL_INIT
    return NULL;
#endif
    struct adapter *a=calloc(1,sizeof *a);
    if(!a) {failure("context allocation failed");return NULL;}
    a->winsys=(struct sw_winsys){.destroy=destroy_winsys,
        .is_displaytarget_format_supported=supported,.displaytarget_create=create_target,
        .displaytarget_map=map_target,.displaytarget_unmap=unmap_target,
        .displaytarget_destroy=destroy_target};
    a->screen=softpipe_create_screen(&a->winsys);
    if(!a->screen) {failure("screen creation failed");goto fail;}
    struct pipe_context *p=a->pipe=a->screen->context_create(a->screen,NULL,0);
    if(!p) {failure("pipe creation failed");goto fail;}
    const enum tgsi_semantic semantics[]={TGSI_SEMANTIC_POSITION,TGSI_SEMANTIC_GENERIC};
    const unsigned indices[]={0,0};
    a->vs=util_make_vertex_passthrough_shader(p,2,semantics,indices,false);
    a->fs=util_make_fragment_tex_shader(p,TGSI_TEXTURE_2D,TGSI_RETURN_TYPE_FLOAT,
                                       TGSI_RETURN_TYPE_FLOAT,false,false);
    struct pipe_blend_state b={0}; b.rt[0].colormask=PIPE_MASK_RGBA;
    a->blend[0]=p->create_blend_state(p,&b);
    b.rt[0].blend_enable=true;
    b.rt[0].rgb_func=b.rt[0].alpha_func=PIPE_BLEND_ADD;
    b.rt[0].rgb_src_factor=b.rt[0].alpha_src_factor=PIPE_BLENDFACTOR_ONE;
    b.rt[0].rgb_dst_factor=b.rt[0].alpha_dst_factor=PIPE_BLENDFACTOR_INV_SRC_ALPHA;
    a->blend[1]=p->create_blend_state(p,&b);
    struct pipe_rasterizer_state r={.scissor=true,.half_pixel_center=true,.line_width=1,.point_size=1};
    struct pipe_depth_stencil_alpha_state d={0};
    struct pipe_vertex_element e[2]={
        {.src_offset=0,.src_format=PIPE_FORMAT_R32G32B32A32_FLOAT,.src_stride=32},
        {.src_offset=16,.src_format=PIPE_FORMAT_R32G32B32A32_FLOAT,.src_stride=32}};
    struct pipe_sampler_state sampler={.wrap_s=PIPE_TEX_WRAP_CLAMP_TO_EDGE,
        .wrap_t=PIPE_TEX_WRAP_CLAMP_TO_EDGE,.wrap_r=PIPE_TEX_WRAP_CLAMP_TO_EDGE,
        .min_img_filter=PIPE_TEX_FILTER_NEAREST,.mag_img_filter=PIPE_TEX_FILTER_NEAREST,
        .min_mip_filter=PIPE_TEX_MIPFILTER_NONE,.unnormalized_coords=false};
    a->raster=p->create_rasterizer_state(p,&r); a->depth=p->create_depth_stencil_alpha_state(p,&d);
    a->elements=p->create_vertex_elements_state(p,2,e); a->sampler=p->create_sampler_state(p,&sampler);
    if(!a->vs||!a->fs||!a->blend[0]||!a->blend[1]||!a->raster||!a->depth||!a->elements||!a->sampler) {
        failure("drawing state creation failed");goto fail;
    }
    p->bind_vs_state(p,a->vs);p->bind_fs_state(p,a->fs);
    p->bind_rasterizer_state(p,a->raster);p->bind_depth_stencil_alpha_state(p,a->depth);
    p->bind_vertex_elements_state(p,a->elements);
    p->bind_sampler_states(p,MESA_SHADER_FRAGMENT,0,1,&a->sampler);
    return a;
fail: cubit_mesa_destroy(a);return NULL;
}
void *cubit_mesa_import(void *opaque,const struct cubit_mesa_image *image)
{
    struct adapter *a=opaque;
    if(!a||!image||!image->pixels||!image->width||!image->height||
        image->width>4096||image->height>4096||image->pitch<image->width*4||
        image->pitch>16384||image->pitch%4) return NULL;
    struct imported *i=calloc(1,sizeof *i);
    if(!i) {failure("import allocation failed");return NULL;}
    i->owner=a;i->target.image=*image;
    struct pipe_resource desc={.target=PIPE_TEXTURE_2D,.format=PIPE_FORMAT_B8G8R8A8_UNORM,
        .width0=image->width,.height0=image->height,.depth0=1,.array_size=1,
        .bind=PIPE_BIND_DISPLAY_TARGET|PIPE_BIND_RENDER_TARGET|PIPE_BIND_SAMPLER_VIEW};
    i->resource=a->screen->resource_create_front(a->screen,&desc,&i->target);
    if(!i->resource) {failure("resource import failed");free(i);return NULL;}
    struct pipe_sampler_view v={.format=desc.format,.target=PIPE_TEXTURE_2D,
        .swizzle_r=PIPE_SWIZZLE_X,.swizzle_g=PIPE_SWIZZLE_Y,
        .swizzle_b=PIPE_SWIZZLE_Z,.swizzle_a=PIPE_SWIZZLE_W};
    i->view=a->pipe->create_sampler_view(a->pipe,i->resource,&v);
    if(!i->view) {failure("sampler view failed");pipe_resource_reference(&i->resource,NULL);free(i);return NULL;}
    return i;
}
uint32_t cubit_mesa_release(void *opaque,void *image)
{
    struct adapter *a=opaque;struct imported *i=image;
    if(!i) return 0;
    /* Quarantined imports must not be freed by a normal release. */
    if(i->owner!=a||i->target.maps) {if(a) a->fault=1;return 3;}
    pipe_sampler_view_reference(&i->view,NULL);
    pipe_resource_reference(&i->resource,NULL);free(i);return 0;
}
uint32_t cubit_mesa_draw(void *opaque,void *dst,void *src,const struct cubit_mesa_draw *d)
{
#ifdef CUBIT_MESA_FAIL_DRAW
    return 2;
#endif
    struct adapter *a=opaque;struct imported *t=dst,*s=src;
    if(!a||!s||!t||!d||s->owner!=a||t->owner!=a||!t->target.image.writable||s==t) return 1;
    if(a->fault||s->target.maps||t->target.maps) return 3;
    struct pipe_context *p=a->pipe;
    const float x0=2.0f*d->dx/t->target.image.width-1, y0=2.0f*d->dy/t->target.image.height-1;
    const float x1=x0+2.0f*d->dw/t->target.image.width, y1=y0+2.0f*d->dh/t->target.image.height;
    const float u0=(float)d->sx/s->target.image.width,v0=(float)d->sy/s->target.image.height;
    const float u1=(float)(d->sx+d->sw)/s->target.image.width,v1=(float)(d->sy+d->sh)/s->target.image.height;
    const float vertices[6][8]={
        {x0,y0,0,1,u0,v0,0,1},{x1,y0,0,1,u1,v0,0,1},{x1,y1,0,1,u1,v1,0,1},
        {x0,y0,0,1,u0,v0,0,1},{x1,y1,0,1,u1,v1,0,1},{x0,y1,0,1,u0,v1,0,1}};
    struct pipe_vertex_buffer vb={.is_user_buffer=true,.buffer.user=vertices};
    struct pipe_framebuffer_state fb={.width=t->target.image.width,.height=t->target.image.height,.nr_cbufs=1};
    fb.cbufs[0]=(struct pipe_surface){.texture=t->resource,.format=PIPE_FORMAT_B8G8R8A8_UNORM};
    struct pipe_viewport_state vp={
        .scale={t->target.image.width/2.0f,t->target.image.height/2.0f,1},
        .translate={t->target.image.width/2.0f,t->target.image.height/2.0f,0}};
    struct pipe_scissor_state clip={.minx=d->clip_x,.miny=d->clip_y,
        .maxx=d->clip_x+d->clip_w,.maxy=d->clip_y+d->clip_h};
    p->bind_blend_state(p,a->blend[d->over?1:0]);
    p->set_vertex_buffers(p,1,&vb);p->set_framebuffer_state(p,&fb);
    p->set_viewport_states(p,0,1,&vp);p->set_scissor_states(p,0,1,&clip);
    p->set_sampler_views(p,MESA_SHADER_FRAGMENT,0,1,0,&s->view);
    util_draw_arrays(p,MESA_PRIM_TRIANGLES,0,6);
    p->flush(p,NULL,0); /* softpipe is synchronous, not a future GPU contract */
    p->set_sampler_views(p,MESA_SHADER_FRAGMENT,0,0,1,NULL);
    p->set_vertex_buffers(p,0,NULL);
    memset(&fb,0,sizeof fb);p->set_framebuffer_state(p,&fb);
    p->flush(p,NULL,0);
    if(s->target.maps||t->target.maps) return 3;
    if(a->fault) failure("draw map/unmap fault");
    return a->fault?2:0;
}
