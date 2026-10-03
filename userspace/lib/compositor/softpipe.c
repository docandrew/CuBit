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
#include "tgsi/tgsi_ureg.h"
_Static_assert(DETECT_OS_CUBIT && !DETECT_OS_LINUX, "native Mesa ABI required");

struct sw_displaytarget { struct cubit_mesa_image image; unsigned maps; };
struct adapter {
    struct sw_winsys winsys;
    struct pipe_screen *screen;
    struct pipe_context *pipe;
    void *vs, *fs, *mask_fs, *blend[3], *raster, *depth, *elements, *sampler;
    unsigned fault;
};
struct imported {
    struct adapter *owner;
    struct pipe_resource *resource;
    struct pipe_sampler_view *view;
    struct sw_displaytarget target;
    bool mask;
};
static void failure(const char *stage)
{
    const char *prefix="desktop Mesa FFI: ";
    cubit_debug_write(prefix,strlen(prefix));
    cubit_debug_write(stage,strlen(stage));
    cubit_debug_write("\n",1);
}
static bool supported(struct sw_winsys *w,unsigned bind,enum pipe_format f)
{ (void)w;(void)bind; return f==PIPE_FORMAT_B8G8R8A8_UNORM || f==PIPE_FORMAT_R8_UNORM; }
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
        if(a->mask_fs) p->delete_fs_state(p,a->mask_fs);
        for(unsigned i=0;i<3;++i) if(a->blend[i]) p->delete_blend_state(p,a->blend[i]);
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
    b.rt[0].rgb_src_factor=PIPE_BLENDFACTOR_SRC_ALPHA;
    a->blend[2]=p->create_blend_state(p,&b);
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
    if(!a->vs||!a->fs||!a->blend[0]||!a->blend[1]||!a->blend[2]||!a->raster||!a->depth||!a->elements||!a->sampler) {
        failure("drawing state creation failed");goto fail;
    }
    p->bind_vs_state(p,a->vs);p->bind_fs_state(p,a->fs);
    p->bind_rasterizer_state(p,a->raster);p->bind_depth_stencil_alpha_state(p,a->depth);
    p->bind_vertex_elements_state(p,a->elements);
    p->bind_sampler_states(p,MESA_SHADER_FRAGMENT,0,1,&a->sampler);
    return a;
fail: cubit_mesa_destroy(a);return NULL;
}
static void *import_image(void *opaque,const struct cubit_mesa_image *image, bool mask)
{
    struct adapter *a=opaque;
    if(!a||!image||!image->pixels||!image->width||!image->height||
        image->width>4096||image->height>4096||image->pitch<image->width*(mask?1:4)||
        image->pitch>16384||image->pitch%4) return NULL;
    struct imported *i=calloc(1,sizeof *i);
    if(!i) {failure("import allocation failed");return NULL;}
    i->owner=a;i->target.image=*image;i->mask=mask;
    struct pipe_resource desc={.target=PIPE_TEXTURE_2D,
        .format=mask?PIPE_FORMAT_R8_UNORM:PIPE_FORMAT_B8G8R8A8_UNORM,
        .width0=image->width,.height0=image->height,.depth0=1,.array_size=1,
        .bind=PIPE_BIND_DISPLAY_TARGET|PIPE_BIND_SAMPLER_VIEW|(mask?0:PIPE_BIND_RENDER_TARGET)};
    i->resource=a->screen->resource_create_front(a->screen,&desc,&i->target);
    if(!i->resource) {failure("resource import failed");free(i);return NULL;}
    struct pipe_sampler_view v={.format=desc.format,.target=PIPE_TEXTURE_2D,
        .swizzle_r=PIPE_SWIZZLE_X,.swizzle_g=PIPE_SWIZZLE_Y,
        .swizzle_b=PIPE_SWIZZLE_Z,.swizzle_a=PIPE_SWIZZLE_W};
    if(mask) v.swizzle_g=v.swizzle_b=v.swizzle_a=PIPE_SWIZZLE_X;
    i->view=a->pipe->create_sampler_view(a->pipe,i->resource,&v);
    if(!i->view) {failure("sampler view failed");pipe_resource_reference(&i->resource,NULL);free(i);return NULL;}
    return i;
}
void *cubit_mesa_import(void *opaque,const struct cubit_mesa_image *image)
{ return import_image(opaque,image,false); }
void *cubit_mesa_import_mask(void *opaque,const struct cubit_mesa_image *image,uint64_t capacity)
{
    if(!image||image->writable||!image->width||image->width>512||
       !image->height||image->height>272||image->pitch<image->width||
       image->pitch>512||image->pitch%16||capacity<(uint64_t)image->pitch*image->height)
        return NULL;
    return import_image(opaque,image,true);
}
/* Fixed coverage * premultiplied tint shader. Geometry and lifetime policy
 * remain in SPARK; this is Mesa shader construction at the foreign boundary. */
static void *create_mask_shader(struct pipe_context *p)
{
    struct ureg_program *u=ureg_create(MESA_SHADER_FRAGMENT);
    if(!u) return NULL;
    struct ureg_src sampler=ureg_DECL_sampler(u,0);
    ureg_DECL_sampler_view(u,0,TGSI_TEXTURE_2D,TGSI_RETURN_TYPE_FLOAT,
        TGSI_RETURN_TYPE_FLOAT,TGSI_RETURN_TYPE_FLOAT,TGSI_RETURN_TYPE_FLOAT);
    struct ureg_src tex=ureg_DECL_fs_input(u,TGSI_SEMANTIC_GENERIC,0,TGSI_INTERPOLATE_LINEAR);
    struct ureg_dst out=ureg_DECL_output(u,TGSI_SEMANTIC_COLOR,0);
    struct ureg_dst coverage=ureg_DECL_temporary(u);
    struct ureg_src tint=ureg_DECL_constant(u,0);
    ureg_TEX(u,coverage,TGSI_TEXTURE_2D,tex,sampler);
    ureg_MUL(u,out,ureg_src(coverage),tint);
    ureg_END(u);
    return ureg_create_shader_and_destroy(u,p);
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
static void begin_vertices(struct adapter *a,struct imported *t,uint32_t over,bool mask)
{
    struct pipe_context *p=a->pipe;
    p->bind_fs_state(p,mask?a->mask_fs:a->fs);
    struct pipe_framebuffer_state fb={.width=t->target.image.width,.height=t->target.image.height,.nr_cbufs=1};
    fb.cbufs[0]=(struct pipe_surface){.texture=t->resource,.format=PIPE_FORMAT_B8G8R8A8_UNORM};
    struct pipe_viewport_state vp={
        .scale={t->target.image.width/2.0f,t->target.image.height/2.0f,1},
        .translate={t->target.image.width/2.0f,t->target.image.height/2.0f,0}};
    p->bind_blend_state(p,a->blend[over]);
    p->set_framebuffer_state(p,&fb);p->set_viewport_states(p,0,1,&vp);
}
static void emit_vertices(struct adapter *a,struct imported *s,
                         const float vertices[6][8],uint32_t clip_x,uint32_t clip_y,
                         uint32_t clip_w,uint32_t clip_h,const float *tint)
{
    struct pipe_context *p=a->pipe;
    struct pipe_constant_buffer cb={.buffer_size=4*sizeof(float),.user_buffer=tint};
    if(tint) p->set_constant_buffer(p,MESA_SHADER_FRAGMENT,0,&cb);
    struct pipe_vertex_buffer vb={.is_user_buffer=true,.buffer.user=vertices};
    struct pipe_scissor_state clip={.minx=clip_x,.miny=clip_y,
        .maxx=clip_x+clip_w,.maxy=clip_y+clip_h};
    p->set_vertex_buffers(p,1,&vb);p->set_scissor_states(p,0,1,&clip);
    p->set_sampler_views(p,MESA_SHADER_FRAGMENT,0,1,0,&s->view);
    util_draw_arrays(p,MESA_PRIM_TRIANGLES,0,6);
}
static void finish_vertices(struct adapter *a)
{
    struct pipe_context *p=a->pipe;
    p->flush(p,NULL,0); /* softpipe is synchronous, not a future GPU contract */
    p->set_sampler_views(p,MESA_SHADER_FRAGMENT,0,0,1,NULL);
    p->set_vertex_buffers(p,0,NULL);
    struct pipe_framebuffer_state fb={0};p->set_framebuffer_state(p,&fb);
    p->flush(p,NULL,0);
    p->set_constant_buffer(p,MESA_SHADER_FRAGMENT,0,NULL);
}
static uint32_t draw_vertices(struct adapter *a, struct imported *t, struct imported *s,
                              const float vertices[6][8], uint32_t clip_x, uint32_t clip_y,
                              uint32_t clip_w, uint32_t clip_h, uint32_t over,
                              const float *tint)
{
    if(over>2 || (tint && over!=1)) return 1;
    if(tint && !a->mask_fs) {
        a->mask_fs=create_mask_shader(a->pipe);
        if(!a->mask_fs) return 2;
    }
    begin_vertices(a,t,over,tint!=NULL);
    emit_vertices(a,s,vertices,clip_x,clip_y,clip_w,clip_h,tint);
    finish_vertices(a);
    if(s->target.maps||t->target.maps) return 3;
    if(a->fault) failure("draw map/unmap fault");
    return a->fault?2:0;
}
uint32_t cubit_mesa_draw(void *opaque,void *dst,void *src,const struct cubit_mesa_draw *d)
{
#ifdef CUBIT_MESA_FAIL_DRAW
    return 2;
#endif
    struct adapter *a=opaque;struct imported *t=dst,*s=src;
    if(!a||!s||!t||!d||s->owner!=a||t->owner!=a||!t->target.image.writable||s==t||s->mask||t->mask||d->over>1) return 1;
    if(a->fault||s->target.maps||t->target.maps) return 3;
    const float x0=2.0f*d->dx/t->target.image.width-1, y0=2.0f*d->dy/t->target.image.height-1;
    const float x1=x0+2.0f*d->dw/t->target.image.width, y1=y0+2.0f*d->dh/t->target.image.height;
    const float u0=(float)d->sx/s->target.image.width,v0=(float)d->sy/s->target.image.height;
    const float u1=(float)(d->sx+d->sw)/s->target.image.width,v1=(float)(d->sy+d->sh)/s->target.image.height;
    const float vertices[6][8]={
        {x0,y0,0,1,u0,v0,0,1},{x1,y0,0,1,u1,v0,0,1},{x1,y1,0,1,u1,v1,0,1},
        {x0,y0,0,1,u0,v0,0,1},{x1,y1,0,1,u1,v1,0,1},{x0,y1,0,1,u0,v1,0,1}};
    return draw_vertices(a,t,s,vertices,d->clip_x,d->clip_y,d->clip_w,d->clip_h,d->over,NULL);
}

_Static_assert(sizeof(struct cubit_mesa_affine)==56, "affine ABI size");
_Static_assert(sizeof(struct cubit_mesa_quad)==88, "quad ABI size");
static uint32_t check_affine(void *opaque,void *dst,void *src,const struct cubit_mesa_affine *d,const struct cubit_mesa_quad *q,bool mask)
{
#ifdef CUBIT_MESA_FAIL_DRAW
    return 2;
#endif
    struct adapter *a=opaque;struct imported *t=dst,*s=src;
    if(!a||!s||!t||!d||!q||s->owner!=a||t->owner!=a||!t->target.image.writable||s==t||t->mask||s->mask!=mask) return 1;
    if(mask && d->over!=1) return 1;
    const uint32_t w=t->target.image.width,h=t->target.image.height;
    if(d->origin_x < -INT64_C(2147483648)||d->origin_x > INT64_C(2147483648)||
       d->origin_y < -INT64_C(2147483648)||d->origin_y > INT64_C(2147483648)||
       !d->logical_w||d->logical_w>UINT32_C(2147483648)||
       !d->logical_h||d->logical_h>UINT32_C(2147483648)||
       !d->numerator||d->numerator>16||!d->denominator||d->denominator>16||
       d->rotation>3||d->over>2||d->clip_x>=w||d->clip_y>=h||
       !d->clip_w||d->clip_w>w-d->clip_x||!d->clip_h||d->clip_h>h-d->clip_y) return 1;
    if(a->fault||s->target.maps||t->target.maps) return 3;
    /* SPARK supplies exact rational corners, including inverse rotation.
     * Only numeric conversion and the fixed two-triangle topology live here. */
    if(q->width!=w||q->height!=h||q->ud<1||q->vd<1||
       q->ud>INT64_C(34359738368)||q->vd>INT64_C(34359738368)) return 1;
    return 0;
}
static void affine_vertices(const struct cubit_mesa_quad *q,float vertices[6][8])
{
    static const unsigned indices[6]={0,1,2,0,2,3};
    static const float positions[4][2]={{-1,-1},{1,-1},{1,1},{-1,1}};
    for(unsigned i=0;i<6;i++) {
        const unsigned c=indices[i];
        const float value[8]={positions[c][0],positions[c][1],0,1,
            (float)((double)q->corners[c].u/q->ud),
            (float)((double)q->corners[c].v/q->vd),0,1};
        memcpy(vertices[i],value,sizeof value);
    }
}
static uint32_t draw_affine(void *opaque,void *dst,void *src,const struct cubit_mesa_affine *d,const struct cubit_mesa_quad *q,const float *tint)
{
    uint32_t status=check_affine(opaque,dst,src,d,q,tint!=NULL);
    if(status) return status;
    float vertices[6][8];affine_vertices(q,vertices);
    return draw_vertices(opaque,dst,src,vertices,d->clip_x,d->clip_y,d->clip_w,d->clip_h,d->over,tint);
}
uint32_t cubit_mesa_draw_affine(void *opaque,void *dst,void *src,const struct cubit_mesa_affine *d,const struct cubit_mesa_quad *q)
{ return draw_affine(opaque,dst,src,d,q,NULL); }
uint32_t cubit_mesa_draw_mask(void *opaque,void *dst,void *src,const struct cubit_mesa_affine *d,const struct cubit_mesa_quad *q,uint32_t argb)
{
    const float alpha=(float)(argb>>24)/255.0f;
    const float tint[4]={((argb>>16)&255)/255.0f*alpha,
        ((argb>>8)&255)/255.0f*alpha,(argb&255)/255.0f*alpha,alpha};
    return draw_affine(opaque,dst,src,d,q,tint);
}

_Static_assert(sizeof(struct cubit_mesa_mask_command)==160, "mask command ABI size");
uint32_t cubit_mesa_draw_mask_batch(void *opaque,void *dst,
    const struct cubit_mesa_mask_command *commands,uint32_t count)
{
    if(count>CUBIT_MESA_MASK_BATCH_MAX||(!commands&&count)) return 1;
    if(!count) return 0;
    struct adapter *a=opaque;struct imported *t=dst;
    /* Every descriptor/handle is checked before the first pixel write. */
    for(uint32_t i=0;i<count;i++) {
        uint32_t status=check_affine(a,t,commands[i].source,
            &commands[i].draw,&commands[i].quad,true);
        if(status) return status;
    }
    if(!a->mask_fs) {
        a->mask_fs=create_mask_shader(a->pipe);
        if(!a->mask_fs) return 2;
    }
    /* User vertex/constant storage survives ALL draws and the final flush.
     * These bounded arrays hold metadata only, never copied glyph pixels. */
    float vertices[CUBIT_MESA_MASK_BATCH_MAX][6][8];
    float tints[CUBIT_MESA_MASK_BATCH_MAX][4];
    for(uint32_t i=0;i<count;i++) {
        const uint32_t c=commands[i].argb;
        const float alpha=(float)(c>>24)/255.0f;
        tints[i][0]=((c>>16)&255)/255.0f*alpha;
        tints[i][1]=((c>>8)&255)/255.0f*alpha;
        tints[i][2]=(c&255)/255.0f*alpha;
        tints[i][3]=alpha;
        affine_vertices(&commands[i].quad,vertices[i]);
    }
    begin_vertices(a,t,1,true);
    for(uint32_t i=0;i<count;i++) {
        const struct cubit_mesa_affine *d=&commands[i].draw;
        emit_vertices(a,commands[i].source,vertices[i],
            d->clip_x,d->clip_y,d->clip_w,d->clip_h,tints[i]);
#ifdef CUBIT_MESA_FAIL_TEXT_PARTIAL
        /* Test-only: commit one glyph, then report a quiescent failure. */
        finish_vertices(a);
        if(t->target.maps||((struct imported *)commands[i].source)->target.maps) return 3;
        return 2;
#endif
    }
    finish_vertices(a);
    if(t->target.maps) return 3;
    for(uint32_t i=0;i<count;i++) {
        struct imported *source=commands[i].source;
        if(source->target.maps) return 3;
    }
    if(a->fault) failure("batch map/unmap fault");
    return a->fault?2:0;
}

uint32_t cubit_mesa_fill(void *opaque,void *dst,uint32_t left,uint32_t top,
                         uint32_t width,uint32_t height,uint32_t color)
{
    struct adapter *a=opaque;struct imported *t=dst;
    if(!a||!t||t->owner!=a||!t->target.image.writable||t->mask||
       left>=t->target.image.width||top>=t->target.image.height||
       !width||width>t->target.image.width-left||!height||height>t->target.image.height-top)return 1;
    if(a->fault||t->target.maps)return 3;
#ifdef CUBIT_MESA_FAIL_DRAW
    return 2;
#endif
    struct pipe_surface surface={.texture=t->resource,.format=PIPE_FORMAT_B8G8R8A8_UNORM};
    const union pipe_color_union value={.f={((color>>16)&255)/255.0f,((color>>8)&255)/255.0f,
        (color&255)/255.0f,((color>>24)&255)/255.0f}};
    a->pipe->clear_render_target(a->pipe,&surface,&value,left,top,width,height,false);
    a->pipe->flush(a->pipe,NULL,0); /* softpipe completion, not an asynchronous GPU contract */
    if(t->target.maps)return 3;
    return a->fault?2:0;
}
