"""Exercise the actual Desktop fast redraw gate with mocked draw/clock IO.

The production blit geometry has separate SPARK proof; this checks that the
caller does not promise a complete redraw when a client source cannot fill it.
"""
from pathlib import Path
import subprocess
import tempfile
root=Path(__file__).resolve().parents[2]
source=(root/'userspace/services/desktop/main.adb').read_text()
start=source.index('   function tryFastClientRedraw ')
end=source.index('   end tryFastClientRedraw;',start)+len('   end tryFastClientRedraw;')
helper=source[start:end]
prefix='''with Ada.Text_IO; with Interfaces; use Interfaces;
with System; with System.Storage_Elements;
procedure Check is
 use type System.Address;
 type Rect is record x,y,w,h:Natural:=0; end record;
 function isEmpty(R:Rect) return Boolean is (R.w=0 or R.h=0);
 function clampRect(R:Rect) return Rect is (R);
 function rectContains(A,B:Rect) return Boolean is
  (A.x<=B.x and A.y<=B.y and A.x+A.w>=B.x+B.w and A.y+A.h>=B.y+B.h);
 function rectIntersects(A,B:Rect) return Boolean is
  (A.x<B.x+B.w and B.x<A.x+A.w and A.y<B.y+B.h and B.y<A.y+A.h);
 subtype SurfaceIndex is Positive range 1..3;
 type Surface is record
  used,minimized,bufferAttached,publicationMode:Boolean:=False;
  bufferAddr:System.Address:=System.Null_Address;
  bufferW,bufferH,bufferPitch,bufferLogicalW,bufferLogicalH:Natural:=0;
  bufferFormat,flags:Unsigned_64:=0;
  bounds:Rect:=(10,10,100,100);
 end record;
 surfaces:array(SurfaceIndex) of Surface;
 function clientRect(S:Surface) return Rect is (S.bounds);
 function surfaceRect(S:Surface) return Rect is (S.bounds);
 SURFACE_FLAG_WINDOW:constant Unsigned_64:=1;
 PIXEL_FORMAT_BGRA8888:constant Unsigned_64:=1;
 launchMenuOpen,audioPopupOpen,clipEnabled:Boolean:=False;
 backBufferReady:Boolean:=True;
 backBufferAddr:System.Address:=System.Storage_Elements.To_Address(4096);
 clipRect:Rect;
 statsFastFrames,Draws:Natural:=0;
 type Timing_Stage is (Scene_Draw);
 function timingNow return Unsigned_64 is (0);
 procedure noteTiming(Stage:Timing_Stage;First:Unsigned_64) is null;
 procedure restoreCursorOverlay is null;
 procedure drawCursorOverlay is null;
 procedure noteCursorPresented is null;
 procedure flushBackBufferRect(R:Rect) is null;
 procedure drawClientBuffer(S:Surface;X,Y,W,H:Natural) is
 begin Draws:=Draws+1; end;
'''
suffix='''
 Count:Natural:=0;
 procedure Reset is
 begin
  surfaces:=(others=><>);
  surfaces(1):=(used=>True,bufferAttached=>True,
    bufferAddr=>System.Storage_Elements.To_Address(8192),bufferW=>100,bufferH=>100,
    bufferPitch=>400,bufferFormat=>1,flags=>1,others=><>);
 end;
 procedure Expect(Admitted:Boolean) is
  Before:constant Natural:=Draws;
  Result:Boolean;
 begin
  Result:=tryFastClientRedraw((20,20,20,20));
  pragma Assert(Result=Admitted);
  pragma Assert(Draws=Before+(if Admitted then 1 else 0));
  Count:=Count+1;
 end;
begin
 Reset;Expect(True);
 Reset;surfaces(1).bufferW:=99;Expect(False);
 Reset;surfaces(1).bufferH:=99;Expect(False);
 Reset;surfaces(1).bufferPitch:=399;Expect(False);
 Reset;surfaces(1).bufferAddr:=System.Null_Address;Expect(False);
 Reset;surfaces(1).bufferFormat:=2;Expect(False);
 Reset;surfaces(1).publicationMode:=True;surfaces(1).bufferLogicalW:=100;
 surfaces(1).bufferLogicalH:=100;surfaces(1).bufferW:=200;
 surfaces(1).bufferH:=200;surfaces(1).bufferPitch:=800;Expect(True);
 surfaces(1).bufferLogicalW:=99;Expect(False);
 surfaces(1).bufferLogicalW:=100;surfaces(1).bufferLogicalH:=99;Expect(False);
 surfaces(1).bufferLogicalH:=100;surfaces(1).bufferW:=0;Expect(False);
 surfaces(1).bufferW:=200;surfaces(1).bufferH:=0;Expect(False);
 Reset;surfaces(2):=surfaces(1);surfaces(2).bufferAttached:=False;Expect(False);
 Reset;launchMenuOpen:=True;Expect(False);launchMenuOpen:=False;
 Reset;audioPopupOpen:=True;Expect(False);audioPopupOpen:=False;
 Reset;backBufferAddr:=System.Null_Address;Expect(False);
 Ada.Text_IO.Put_Line("REPAIR-FAST-SOURCE: PASS"&Count'Image&" actual-gate cases");
end Check;
'''
with tempfile.TemporaryDirectory(prefix='cubit-repair-fast-source-') as tmp:
 d=Path(tmp)
 (d/'check.adb').write_text(prefix+helper+suffix)
 (d/'check.gpr').write_text('''project Check is
 for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use ".";
 for Main use ("check.adb");
 package Compiler is for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato"); end Compiler;
end Check;''')
 subprocess.run(['gprbuild','-q','-p','-P',str(d/'check.gpr')],check=True)
 subprocess.run([str(d/'check')],check=True)
