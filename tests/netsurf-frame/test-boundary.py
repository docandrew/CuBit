#!/usr/bin/env python3
"""Compile the production redraw function with observable foreign-library mocks.
This checks the C binding, not NetSurf rendering or actual mapping validity.
"""
from pathlib import Path
import os, subprocess, tempfile
root=Path(__file__).resolve().parents[2]
source=(root/'userspace/c/netsurf/netsurf-embed-cubit.c').read_text()
a=source.index('void cubit_netsurf_redraw('); b=source.index('\n/* kind: 0 move',a)
prefix=r'''
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
typedef struct {int x0,y0,x1,y1;} nsfb_bbox_t;
struct rect {int x0,y0,x1,y1;};
struct redraw_context {bool interactive,background_images; const void *plot;};
static int fb_plotters;
static uint32_t owned[1], pixels[120];
static struct {uint8_t *ptr; int width,height,linelen; nsfb_bbox_t clip;} storage, *view=&storage;
static struct {void *bw; int scroll_x,scroll_y; bool caret;int caret_x,caret_y,caret_h;} state,*window=&state;
static int draws,carets; static bool clip_ok=true;
static bool nsfb_plot_get_clip(void *v, nsfb_bbox_t *b) {(void)v;*b=view->clip;return clip_ok;}
static bool nsfb_plot_set_clip(void *v,const nsfb_bbox_t *b) {(void)v;assert(b);view->clip=*b;return true;}
static bool browser_window_redraw(void *bw,int x,int y,const struct rect *r,const struct redraw_context *ctx) {
 (void)bw;(void)x;(void)y;(void)ctx;draws++;
 assert(view->ptr==(uint8_t*)pixels);assert(r->x0>=0&&r->y0>=0&&r->x1<=view->width&&r->y1<=view->height);
 for(int j=r->y0;j<r->y1;j++) for(int i=r->x0;i<r->x1;i++) pixels[j*(view->linelen/4)+i]=1;
 /* Core rendering may leave a different clip installed. */
 view->clip=(nsfb_bbox_t){0,0,1,1};return false;
}
static bool nsfb_plot_rectangle_fill(void *v,const nsfb_bbox_t *b,uint32_t c) {
 (void)v;carets++;assert(b->x0>=view->clip.x0&&b->y0>=view->clip.y0&&b->x1<=view->clip.x1&&b->y1<=view->clip.y1);
 for(int j=b->y0;j<b->y1;j++)for(int i=b->x0;i<b->x1;i++)pixels[j*(view->linelen/4)+i]=c;
 return true;
}
static void reset(void){
 storage.ptr=(uint8_t*)owned;storage.width=1;storage.height=1;storage.linelen=4;storage.clip=(nsfb_bbox_t){0,0,1,1};
 state.scroll_x=state.scroll_y=0;state.caret=false;draws=carets=0;clip_ok=true;
 for(int i=0;i<120;i++)pixels[i]=0xabcdef;
}
static void restored(void){assert(storage.ptr==(uint8_t*)owned&&storage.width==1&&storage.height==1&&storage.linelen==4);
 assert(storage.clip.x0==0&&storage.clip.y0==0&&storage.clip.x1==1&&storage.clip.y1==1);}
'''
suffix=r'''
int main(void){
 int cases=0;
 for(int cx=0;cx<10;cx++)for(int cy=0;cy<10;cy++) {
  reset();state.caret=true;state.caret_x=5;state.caret_y=-20;state.caret_h=INT_MAX;
  cubit_netsurf_redraw(pixels,10,10,48,cx,cy,INT_MAX,INT_MAX);restored();assert(draws==1);
  for(int y=0;y<10;y++)for(int x=0;x<12;x++) {
   uint32_t expected=0xabcdef;
   if(x>=cx&&x<10&&y>=cy)expected=(x==5?0xff000000:1);
   assert(pixels[y*12+x]==expected);
  } cases++;
 }
 int invalid[][8]={{0,10,48,0,0,1,1,0},{INT_MAX,10,48,0,0,1,1,0},
 {10,INT_MAX,48,0,0,1,1,0},{10,10,39,0,0,1,1,0},{10,10,41,0,0,1,1,0},
 {10,10,48,-1,0,1,1,0},{10,10,48,10,0,1,1,0},{10,10,48,0,10,1,1,0},
 {10,10,48,0,0,0,1,0},{10,10,48,0,0,1,-1,0},{10,10,48,0,0,1,1,INT_MIN}};
 for(unsigned i=0;i<sizeof invalid/sizeof invalid[0];i++) {
  reset();int *q=invalid[i];state.scroll_x=q[7];
  cubit_netsurf_redraw(pixels,q[0],q[1],q[2],q[3],q[4],q[5],q[6]);assert(draws==0);restored();cases++;
 }
 reset();clip_ok=false;cubit_netsurf_redraw(pixels,10,10,48,0,0,10,10);assert(draws==0);restored();cases++;
 reset();cubit_netsurf_redraw(NULL,10,10,48,0,0,10,10);assert(draws==0);restored();cases++;
 window=NULL;cubit_netsurf_redraw(pixels,10,10,48,0,0,10,10);assert(draws==0);restored();window=&state;cases++;
 view=NULL;cubit_netsurf_redraw(pixels,10,10,48,0,0,10,10);assert(draws==0);restored();view=&storage;cases++;
 int edges[]={INT_MIN,INT_MAX,-1,0,9};
 for(unsigned a=0;a<5;a++)for(unsigned b=0;b<5;b++) {
  reset();state.caret=true;state.caret_x=edges[a];state.caret_y=edges[b];state.caret_h=INT_MAX;
  cubit_netsurf_redraw(pixels,10,10,48,0,0,10,10);assert(draws==1);restored();cases++;
 }
 printf("PASS %d redraw-boundary cases: bounded damage/caret, padding, restoration after renderer failure\n",cases);
}
'''
out=Path(tempfile.mkdtemp(prefix='cubit-netsurf-frame-'));(out/'test.c').write_text(prefix+source[a:b]+suffix)
subprocess.run([os.environ.get('CC','gcc'),'-std=c11','-Wall','-Wextra','-Werror','-fsanitize=address,undefined','-fno-omit-frame-pointer','-g',str(out/'test.c'),'-o',str(out/'test')],check=True)
subprocess.run([str(out/'test')],check=True)
print('Artifacts:',out)
