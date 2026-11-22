"""Actual Desktop destroy/goodbye routing with mocked resources and IPC encoding."""
from pathlib import Path
import argparse,hashlib,json,subprocess,tempfile
root=Path(__file__).resolve().parents[2]
ap=argparse.ArgumentParser();ap.add_argument('--source',type=Path,default=root/'userspace/services/desktop/main.adb');args=ap.parse_args()
source=args.source.read_text()
a=source.index('   procedure focusTopmostVisibleWindow (damage : in out Rect) is');b=source.index('   end focusTopmostVisibleWindow;',a)+len('   end focusTopmostVisibleWindow;');helper=source[a:b]
a=source.index('         when OP_SURFACE_DESTROY =>')+len('         when OP_SURFACE_DESTROY =>');b=source.index('         when OP_INPUT_POLL | OP_INPUT_WAIT =>',a);destroy=source[a:b]
a=source.index('         when OP_DESKTOP_BYE =>')+len('         when OP_DESKTOP_BYE =>');b=source.index('         when others =>',a);bye=source[a:b]
routines=helper+'\nprocedure Destroy is begin\n'+destroy+'\nend Destroy;\nprocedure Goodbye is begin\n'+bye+'\nend Goodbye;\n'
routines=routines.replace('CuBit.Desktop_Messages.From_Wire','From_Wire')
out=Path(tempfile.mkdtemp(prefix='desktop-focus-routing-',dir=root/'tests/compositor/build'));print(out,flush=True)
for ext in ('ads','adb'):(out/f'compositor_focus.{ext}').write_bytes((root/f'userspace/lib/compositor/compositor_focus.{ext}').read_bytes())
(out/'test.gpr').write_text('''project Test is
for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("test.adb");
package Compiler is for Default_Switches ("Ada") use ("-gnat2022","-gnata","-gnato","-O1"); end Compiler;
end Test;''')
prefix='''with Ada.Text_IO; with Interfaces; use Interfaces; with Compositor_Focus;
procedure Test is
package Focus_Policy is new Compositor_Focus (8);
subtype SurfaceIndex is Natural range 0..7;
type Rect is record x,y,w,h:Natural:=0;end record;
type Surface is record used,minimized:Boolean:=False;id,owner,flags:Unsigned_64:=0;end record;
type Surface_Array is array(SurfaceIndex) of Surface;
surfaces:Surface_Array;
SURFACE_FLAG_WINDOW:constant Unsigned_64:=1;
focusSurface,pointerSurfaceId,dragSurfaceId:Unsigned_64:=0;
dragMode:Natural:=0;DRAG_NONE:constant:=0;
dragPreviewValid,dragPresentedValid:Boolean:=False;
from:Unsigned_64:=5;
Clears,Releases,Redraws,Display_Releases:Natural:=0;
package DP is
 type Status_Code is (Success,Denied,Bad_Object);
 type Operation is (Destroy_Surface,Goodbye);
 function Encode_Status(Op:Operation;Status:Status_Code) return Status_Code is (Status);
end;
use type DP.Status_Code;
replyMsg:DP.Status_Code;
function From_Wire(S:DP.Status_Code) return DP.Status_Code is (S);
type Destruction_Value is record Surface:Unsigned_64:=4;end record;
type Decoding is record Value:Destruction_Value;end record;
destruction:Decoding;
function findSurface(Id:Unsigned_64) return Integer is
begin for I in surfaces'Range loop if surfaces(I).used and surfaces(I).id=Id then return I;end if;end loop;return -1;end;
function anySurfaceUsed return Boolean is (for some S of surfaces=>S.used);
function surfaceRect(S:Surface) return Rect is ((0,0,64,64));
function taskButtonRect(I:SurfaceIndex) return Rect is ((I,0,16,16));
function inflateRect(R:Rect;Amount:Natural) return Rect is (R);
function unionRect(A,B:Rect) return Rect is (B);
procedure clearInputForTarget(Id:Unsigned_64) is begin Clears:=Clears+1;end;
procedure releaseSurfaceBuffer(S:in out Surface) is begin Releases:=Releases+1;end;
procedure scheduleRedraw is begin Redraws:=Redraws+1;end;
procedure releaseDisplayBuffer is begin Display_Releases:=Display_Releases+1;end;
'''
suffix='''
procedure Reset is begin
 surfaces:=(others=>(others=><>));
 for I in 0..3 loop surfaces(I):=(True,False,Unsigned_64(I+1),5,1);end loop;
 focusSurface:=4;from:=5;destruction.Value.Surface:=4;
 Clears:=0;Releases:=0;Redraws:=0;Display_Releases:=0;
 pointerSurfaceId:=4;dragSurfaceId:=4;dragMode:=1;dragPreviewValid:=True;dragPresentedValid:=True;
end;
begin
 Reset;Destroy;
 pragma Assert(replyMsg=DP.Success and focusSurface=3 and not surfaces(3).used and Releases=1 and Redraws=1);
 pragma Assert(pointerSurfaceId=0 and dragSurfaceId=0 and dragMode=0 and not dragPreviewValid and not dragPresentedValid);
 Reset;destruction.Value.Surface:=2;Destroy;
 pragma Assert(replyMsg=DP.Success and focusSurface=4 and surfaces(3).used);
 Reset;surfaces(2).minimized:=True;surfaces(1).flags:=0;Destroy;
 pragma Assert(focusSurface=1);
 Reset;from:=6;Destroy;
 pragma Assert(replyMsg=DP.Denied and focusSurface=4 and Clears=0 and Releases=0 and Redraws=0);
 Reset;destruction.Value.Surface:=99;Destroy;
 pragma Assert(replyMsg=DP.Bad_Object and focusSurface=4 and Releases=0);
 Reset;for I in 0..2 loop surfaces(I).used:=False;end loop;Destroy;
 pragma Assert(focusSurface=0 and Display_Releases=1);
 Reset;surfaces(0).owner:=6;Goodbye;
 pragma Assert(replyMsg=DP.Success and focusSurface=1 and Releases=3 and Redraws=1);
 Reset;surfaces(3).owner:=6;Goodbye;
 pragma Assert(focusSurface=4 and Releases=3);
 Reset;Goodbye;
 pragma Assert(focusSurface=0 and Releases=4 and Display_Releases=1);
 Ada.Text_IO.Put_Line("DESKTOP FOCUS ROUTING: PASS actual destroy/goodbye authorization, survivor selection, minimized/plain exclusions and final close");
end Test;
'''
variants={'baseline':routines,'missing-restoration':routines.replace('focusTopmostVisibleWindow (Focus_Damage);','focusSurface := 0;'),
 'owner-bypass':routines.replace('surfaces (SurfaceIndex (idx)).owner /= from','False')}
report={'status':'INCOMPLETE','source_sha256':hashlib.sha256(source.encode()).hexdigest(),'variants':{}}
try:
 for name,body in variants.items():
  assert name=='baseline' or body!=routines
  (out/'test.adb').write_text(prefix+body+suffix)
  with (out/(name+'.log')).open('w') as log:
   subprocess.run(['gprbuild','-q','-p','-P',str(out/'test.gpr')],check=True,stdout=log,stderr=subprocess.STDOUT)
   run=subprocess.run([str(out/'test')],stdout=log,stderr=subprocess.STDOUT)
  report['variants'][name]=run.returncode;assert (run.returncode==0)==(name=='baseline'),name
 (out/'test.adb').write_text(prefix+routines+suffix)
 with (out/'restored-baseline.log').open('w') as log:
  subprocess.run(['gprbuild','-q','-p','-P',str(out/'test.gpr')],check=True,stdout=log,stderr=subprocess.STDOUT)
  subprocess.run([str(out/'test')],check=True,stdout=log,stderr=subprocess.STDOUT)
 report['status']='PASS';print('PASS actual focus routing and two rejected negative controls',flush=True)
finally:(out/'result.json').write_text(json.dumps(report,indent=2)+'\n')
