#!/usr/bin/env python3
"""Exercise production NetSurf invalidation against the actual SPARK C entry."""
from pathlib import Path
import subprocess, tempfile
root = Path(__file__).resolve().parents[2]
source = (root/'userspace/c/netsurf/netsurf-embed-cubit.c').read_text()
a = source.index('static nserror embed_invalidate(')
b = source.index('\nstatic bool embed_get_scroll', a)
prefix = r'''
#include <stdint.h>
#include <limits.h>
#include <assert.h>
#include <stdio.h>
typedef int nserror;
#define NSERROR_OK 0
struct gui_window { int scroll_x,scroll_y,width,height; };
struct rect { int x0,y0,x1,y1; };
extern int cubit_ui_clip_edge(int,int,int);
static int calls, observed[4];
static void cubit_browser_invalidate(int x,int y,int w,int h) {
 calls++; observed[0]=x; observed[1]=y; observed[2]=w; observed[3]=h;
}
'''
suffix = r'''
static int edge(int value,int scroll,int limit) {
 int64_t v=value, s=scroll, e=s+limit;
 if(limit<=0 || v<=s) return 0;
 if(v>=e) return limit;
 return (int)(v-s);
}
int main(void) {
 const int values[]={INT_MIN,INT_MIN+1,-65535,-1,0,1,65535,INT_MAX-1,INT_MAX};
 unsigned cases=0;
 for(unsigned x=0;x<9;x++)for(unsigned y=0;y<9;y++)
 for(unsigned z=0;z<9;z++)for(unsigned k=0;k<9;k++) {
  struct gui_window gw={values[z],values[k],800,600};
  struct rect r={values[x],values[y],values[8-x],values[8-y]};
  int x0=edge(r.x0,gw.scroll_x,gw.width),x1=edge(r.x1,gw.scroll_x,gw.width);
  int y0=edge(r.y0,gw.scroll_y,gw.height),y1=edge(r.y1,gw.scroll_y,gw.height);
  calls=0; assert(embed_invalidate(&gw,&r)==NSERROR_OK);
  assert(calls==((x1>x0&&y1>y0)?1:0));
  if(calls)assert(observed[0]==x0&&observed[1]==y0&&observed[2]==x1-x0&&observed[3]==y1-y0);
  cases++;
 }
 struct gui_window gw={0,0,800,600};
 calls=0; assert(embed_invalidate(&gw,NULL)==NSERROR_OK);
 assert(calls==1&&observed[0]==0&&observed[1]==0&&observed[2]==0&&observed[3]==0);
 printf("PASS hosted C/Ada invalidation boundary %u cases\n",cases+1);
}
'''
subprocess.run(['gprbuild','-p','-P',str(root/'tests/compositor/signed_clip.gpr')],check=True)
listing = subprocess.run(['gnatls','-v'], text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, check=True).stdout
runtimes = [Path(line.strip())/'libgnat.a' for line in listing.splitlines() if line.strip().endswith('/adalib')]
runtime = next(path for path in runtimes if path.is_file())
with tempfile.TemporaryDirectory(prefix='cubit-invalidation-') as directory:
    directory=Path(directory)
    c=directory/'test.c'; c.write_text(prefix+source[a:b]+suffix)
    exe=directory/'test'
    subprocess.run(['gcc','-std=c99','-Wall','-Wextra','-Werror','-fsanitize=address,undefined','-fno-omit-frame-pointer',str(c),str(root/'tests/compositor/build/signed-clip/obj/client_signed_clip.o'),str(runtime),'-ldl','-lm','-o',str(exe)],check=True)
    subprocess.run([str(exe)],check=True)
