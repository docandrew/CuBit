"""Actual title-bar close dispatch; process, buffer and drawing calls mocked.

This verifies routing and preservation, not native process lifetime or pixels.
"""
from pathlib import Path
import subprocess
import tempfile
root = Path(__file__).resolve().parents[2]
s = (root/'userspace/services/desktop/main.adb').read_text()
a = s.index('   procedure closeSurface (idx : SurfaceIndex; damage : in out Rect) is')
b = s.index('   end closeSurface;', a) + len('   end closeSurface;')
body = s[a:b]
prefix = '''with Interfaces; use Interfaces;
with Ada.Text_IO;
with CuBit.Desktop_Protocol;
procedure Check is
   package DP renames CuBit.Desktop_Protocol;
   subtype SurfaceIndex is Natural range 0 .. 1;
   subtype ProcessID is Unsigned_64;
   NO_PROCESS : constant ProcessID := 0;
   subtype Rect is Natural;
   type Surface is record
      id, owner, windowFlags : Unsigned_64 := 0;
   end record;
   surfaces : array (SurfaceIndex) of Surface;
   pointerSurfaceId, dragSurfaceId, focusSurface : Unsigned_64 := 0;
   dragMode : Natural := 0;
   DRAG_NONE : constant := 0;
   dragPreviewValid, dragPresentedValid : Boolean := False;
   Requests, Releases, Clears, Kills, Focuses : Natural := 0;
   Requested, Killed : Unsigned_64 := 0;
   function surfaceRect (S : Surface) return Rect is (10);
   function taskButtonRect (I : SurfaceIndex) return Rect is (20);
   function inflateRect (R : Rect; N : Natural) return Rect is (R + N);
   function unionRect (A, B : Rect) return Rect is (Natural'Max (A, B));
   function processAlive (P : ProcessID) return Boolean is (P /= 0);
   function killProcess (P : ProcessID) return Unsigned_64 is
   begin Kills := Kills + 1; Killed := P; return 0; end;
   procedure requestClose (Target : Unsigned_64) is
   begin Requests := Requests + 1; Requested := Target; end;
   procedure clearInputForTarget (Target : Unsigned_64) is
   begin Clears := Clears + 1; end;
   procedure releaseSurfaceBuffer (S : in out Surface) is
   begin Releases := Releases + 1; end;
   procedure focusTopmostVisibleWindow (Damage : in out Rect) is
   begin Focuses := Focuses + 1; end;
'''
suffix = '''
   Damage : Rect;
begin
   for Mode in 0 .. 2 loop
      for Target in SurfaceIndex loop
         Requests:=0; Releases:=0; Clears:=0; Kills:=0; Focuses:=0;
         surfaces := [(11,7,0),(22,7,0)];
         if Mode /= 0 then surfaces(Target).windowFlags:=256; end if;
         if Mode = 2 then surfaces(Target).owner:=0; end if;
         pointerSurfaceId:=surfaces(Target).id;
         dragSurfaceId:=pointerSurfaceId; focusSurface:=pointerSurfaceId;
         dragMode:=1; dragPreviewValid:=True; dragPresentedValid:=True;
         Damage:=0;
         declare
            Before : constant Surface := surfaces(Target);
            Other : constant Surface := surfaces(1-Target);
         begin
            closeSurface(Target,Damage);
            pragma Assert(surfaces(1-Target)=Other);
            if Mode=1 then
               pragma Assert(Requests=1 and Requested=Before.id);
               pragma Assert(Releases=0 and Clears=0 and Kills=0 and Focuses=0);
               pragma Assert(surfaces(Target)=Before and Damage=0);
               pragma Assert(pointerSurfaceId=Before.id and dragSurfaceId=Before.id
                 and focusSurface=Before.id and dragMode=1 and dragPreviewValid
                 and dragPresentedValid);
            else
               pragma Assert(Requests=0 and Releases=1 and Clears=1 and Focuses=1);
               pragma Assert(surfaces(Target).id=0 and Damage>0);
               pragma Assert(pointerSurfaceId=0 and dragSurfaceId=0 and focusSurface=0);
               pragma Assert(dragMode=0 and not dragPreviewValid and not dragPresentedValid);
               pragma Assert(Kills=(if Mode=0 then 1 else 0));
               if Mode=0 then pragma Assert(Killed=7); end if;
            end if;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line("PASS actual title-bar dispatch: opt-in preserves surfaces/buffers/process, legacy and internal close, both window slots");
end Check;
'''
with tempfile.TemporaryDirectory(prefix='cubit-close-dispatch-') as tmp:
    d=Path(tmp)
    for name in ('cubit.ads','cubit-grant_references.ads','cubit-desktop_protocol.ads','cubit-desktop_protocol.adb'):
        (d/name).write_bytes((root/'userspace/runtime/gnat'/name).read_bytes())
    (d/'check.gpr').write_text('''project Check is
 for Source_Dirs use ("."); for Object_Dir use "obj"; for Exec_Dir use ".";
 for Main use ("check.adb");
 package Compiler is for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato"); end Compiler;
end Check;''')
    for mutation in (False, True):
        code=body
        if mutation:
            old='         requestClose (oldId);\n         return;'
            assert code.count(old)==1
            code=code.replace(old,'         requestClose (oldId);')
        (d/'check.adb').write_text(prefix+code+suffix)
        subprocess.run(['gprbuild','-q','-p','-P',str(d/'check.gpr')],check=True)
        result=subprocess.run([str(d/'check')],capture_output=True,text=True)
        if mutation:
            assert result.returncode and 'ASSERTION_ERROR' in result.stderr, result
            print('PASS rejected destructive fall-through after graceful request')
        else:
            assert result.returncode==0,result.stderr
            print(result.stdout,end='')
