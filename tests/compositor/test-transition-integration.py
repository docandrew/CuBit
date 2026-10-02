"""Actual Desktop release damage expression, with real pure transition policy.
The old implementation must fail the shrinking-window case. Run inside Nix.
"""
from pathlib import Path
import subprocess,tempfile
root=Path(__file__).resolve().parents[2]
s=(root/'userspace/services/desktop/main.adb').read_text()
def function(name):
    start=s.index('   function '+name)
    end=s.index('   end '+name+';',start)+len('   end '+name+';')
    return s[start:end]
start=s.index('               --  Ensure a final position')
start=s.index('               damage := unionRect',start)
end=s.index('               end if;',start)
release=s[start:end]
helper=function('transitionDamage') if '   function transitionDamage' in s else ''
functions='\n'.join(function(x) for x in ['unionRect','inflateRect','windowVisualRect'])+helper
prefix='''with Ada.Text_IO; with Compositor_Damage; with Compositor_Transition;
procedure Check is
   type Rect is record x,y,w,h : Natural := 0; end record;
   fbWidth,fbHeight : constant Natural := 4096;
   WINDOW_VISUAL_MARGIN : constant Natural := 4;
   function isEmpty(R:Rect) return Boolean is (R.w=0 or R.h=0);
   function damageRectangle(B:Compositor_Damage.Box) return Rect is
     (if Compositor_Damage.Valid(B) then
       (B.Left,B.Top,B.Right-B.Left,B.Bottom-B.Top) else (others=>0));
   function Contains(A,B:Rect) return Boolean is
     (A.x<=B.x and A.y<=B.y and A.x+A.w>=B.x+B.w and A.y+A.h>=B.y+B.h);
'''
suffix='''
   oldBounds,newBounds,dragPresentedRect,damage : Rect;
   dragPresentedValid : Boolean;
begin
   for X in 4 .. 20 loop
      for W in 10 .. 110 loop
         for Has in Boolean loop
            oldBounds:=(X,30,100,100); newBounds:=(X,30,W,W);
            dragPresentedRect:=newBounds; dragPresentedValid:=Has;
            damage:=(0,0,0,0);
RELEASE
            pragma Assert(Contains(damage,windowVisualRect(oldBounds)));
            pragma Assert(Contains(damage,windowVisualRect(newBounds)));
            -- Preview can also be beyond BOTH the final and old window.
            dragPresentedRect:=(500,500,20,20); damage:=(0,0,0,0);
RELEASE
            pragma Assert(Contains(damage,windowVisualRect(oldBounds)));
            pragma Assert(Contains(damage,windowVisualRect(newBounds)));
            pragma Assert(not Has or else Contains(damage,windowVisualRect(dragPresentedRect)));
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line("PASS actual release damage: 6868 old/new/presented combinations including visual margins");
end Check;
'''.replace('RELEASE',release)
with tempfile.TemporaryDirectory(prefix='cubit-transition-glue-') as tmp:
    d=Path(tmp)
    (d/'check.adb').write_text(prefix+functions+suffix)
    (d/'check.gpr').write_text(f'''project Check is
 for Source_Dirs use (".","{root}/userspace/lib/compositor");
 for Source_Files use ("check.adb","compositor_transition.ads","compositor_damage.ads","compositor_damage.adb");
 for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("check.adb");
 package Compiler is for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato"); end Compiler;
end Check;''')
    subprocess.run(['gprbuild','-q','-p','-P',str(d/'check.gpr')],check=True)
    subprocess.run([str(d/'check')],check=True)
