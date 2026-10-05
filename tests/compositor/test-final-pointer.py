"""Compile Desktop's actual geometry and release assignment; reject stale-preview mutation."""
from pathlib import Path
import subprocess,tempfile
root=Path(__file__).resolve().parents[2]
s=(root/'userspace/services/desktop/main.adb').read_text()
def routine(kind,name):
 a=s.index('   '+kind+' '+name);b=s.index('   end '+name+';',a)+len('   end '+name+';');return s[a:b]
a=s.index('               newBounds :=',s.index('elsif dragMode /= DRAG_NONE and then dragSurfaceId /= 0 then'))
b=s.index(';',a)+1
release=s[a:b]
geometry='\n'.join([routine('procedure','clampSurfaceSize'),routine('function','clampWindowRect'),routine('function','previewRectFromPointer')])
prefix='''with Ada.Text_IO;
procedure Final_Pointer is
 type Rect is record x,y,w,h: Natural; end record;
 type Surface is record x,y,w,h,minW,minH,maxW,maxH: Natural; end record;
 type Pointer_Action is (DRAG_NONE,DRAG_MOVE,DRAG_RESIZE_E,DRAG_RESIZE_S,DRAG_RESIZE_SE,HIT_CLOSE);
 subtype SurfaceIndex is Natural range 0..0;
 surfaces: array(SurfaceIndex) of Surface := (0 => (98,82,808,636,120,80,0,0));
 idx: Integer := 0;
 fbWidth: Natural := 1024; fbHeight: Natural := 768;
 cursorX,cursorY,dragOffsetX,dragOffsetY: Natural := 0;
 dragMode: Pointer_Action := DRAG_NONE;
 dragPreviewRect: Rect := (98,82,808,636);
 newBounds: Rect;
'''
suffix='''
 procedure Release is begin
 RELEASENODE
 end Release;
begin
 -- One final motion arriving on release, without an intermediate preview.
 cursorX:=850;cursorY:=670;dragMode:=DRAG_RESIZE_SE;Release;
 pragma Assert(newBounds=(98,82,752,588));
 dragMode:=DRAG_RESIZE_E;Release;pragma Assert(newBounds=(98,82,752,636));
 dragMode:=DRAG_RESIZE_S;Release;pragma Assert(newBounds=(98,82,808,588));
 -- Release moves must honor final position even after an earlier preview.
 dragMode:=DRAG_MOVE;dragOffsetX:=20;dragOffsetY:=10;
 cursorX:=140;cursorY:=110;Release;pragma Assert(newBounds=(120,100,808,636));
 -- Existing minimum/maximum and screen clamps remain authoritative.
 dragMode:=DRAG_RESIZE_SE;cursorX:=0;cursorY:=0;Release;
 pragma Assert(newBounds=(98,82,120,80));
 surfaces(0).maxW:=900;surfaces(0).maxH:=700;
 cursorX:=2000;cursorY:=2000;Release;pragma Assert(newBounds=(98,68,900,700));
 Ada.Text_IO.Put_Line("FINAL-POINTER: PASS six release geometry cases");
end Final_Pointer;
'''
with tempfile.TemporaryDirectory(prefix='final-pointer-') as td:
 d=Path(td);(d/'test.gpr').write_text('''project Test is
 for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use ".";
 for Main use ("final_pointer.adb");
 package Compiler is for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato","-O2"); end Compiler;
 end Test;''')
 for negative in (False,True):
  node='newBounds := clampWindowRect (surfaces (SurfaceIndex (idx)), dragPreviewRect);' if negative else release
  (d/'final_pointer.adb').write_text(prefix+geometry+suffix.replace('RELEASENODE',node))
  subprocess.run(['gprbuild','-q','-p','-P',str(d/'test.gpr')],check=True)
  r=subprocess.run([str(d/'final_pointer')],capture_output=True,text=True)
  if negative:
   assert r.returncode!=0 and 'ASSERTION_ERROR' in r.stderr,r
   print('FINAL-POINTER: rejected original stale-preview release')
  else:
   assert r.returncode==0,r.stderr
   print(r.stdout,end='')
