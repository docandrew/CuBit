"""Hosted actual Desktop repair/flush/restore routines with separate scanout.

Run in Nix. Pixel IO and cursor art are mocked; pool, damage and repaint policy
are real. This catches a clean target whose submitted damage leaves scanout stale.
"""
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parents[2]
source = (root / 'userspace/services/desktop/main.adb').read_text()

def routine(name):
    start = source.index('   procedure ' + name)
    end = source.index('   end ' + name + ';', start)
    return source[start:end + len('   end ' + name + ';')]

prefix = '''
with Ada.Text_IO; with Interfaces; use Interfaces;
with Compositor_Damage; with Compositor_Repaint; with Compositor_Pool;
procedure Cursor_Repair_Tests is
   package D renames Compositor_Damage;
   package RP renames Compositor_Repaint;
   package BP renames Compositor_Pool;
   use type BP.Ticket;
   type Rect is record x, y, w, h : Natural := 0; end record;
   function isEmpty (R : Rect) return Boolean is (R.w = 0 or R.h = 0);
   function clampRect (R : Rect) return Rect is (R);
   function damageRectangle (B : D.Box) return Rect is
     (if D.Valid (B) then (B.Left, B.Top, B.Right-B.Left, B.Bottom-B.Top)
      else (others => 0));
   procedure addOutputDamage (S : in out D.State; R : Rect) is
   begin
      if not isEmpty (R) then D.Add (S, (R.x,R.y,R.x+R.w,R.y+R.h)); end if;
   end;
   subtype Output_Index is Natural range 0 .. 0;
   primaryOutput : constant Output_Index := 0;
   type Output_Presentation is record
      Enabled : Boolean := True;
      Pool : BP.State := BP.Open (1);
      Repaint : RP.State := RP.Open ((0,0,32,32));
      Damage : D.State;
      Buffer : BP.Live_Slot := 1;
   end record;
   presentations : array (Output_Index) of Output_Presentation;
   function localDamage (O : Output_Index; R : Rect) return Rect is (R);
   backBufferReady, directOutput : constant Boolean := True;
   nativeScene : constant Boolean := False;
   fbBpp : constant := 32;
   repairingTarget, drawingBackBuffer, clipEnabled : Boolean := False;
   backBufferAddr : BP.Live_Slot := 1;
   clipRect, cursorSaveRect, frameDamage : Rect;
   framePending, dragPresentedValid, dragPreviewValid : Boolean := False;
   cursorRect : Rect := (2,2,2,2);
   cursorSaveValid : Boolean := False;
   CURSOR_SAVE_STRIDE : constant := 2;
   cursorSave : array (0 .. 3) of Unsigned_32;
   type Image is array (0 .. 31, 0 .. 31) of Unsigned_32;
   Scene : Image := (others => (others => 17));
   Scanout : Image := (others => (others => 0));
   Targets : array (BP.Live_Slot) of Image := (others => (others => (others => 999)));
   statsRepairPixels : Unsigned_64 := 0;
   type Timing_Kind is (Scene_Draw);
   function timingNow return Unsigned_64 is (0);
   procedure noteTiming (K : Timing_Kind; T : Unsigned_64) is null;
   procedure exitCompositor (Status : Integer) is
   begin raise Program_Error with "invalid writer"; end;
   procedure writeBackPixel (X,Y : Natural; Pixel : Unsigned_32) is
   begin Targets (backBufferAddr)(Y,X) := Pixel; end;
   procedure drawCurrentScene is
   begin
      pragma Assert (clipEnabled);
      for Y in clipRect.y .. clipRect.y+clipRect.h-1 loop
         for X in clipRect.x .. clipRect.x+clipRect.w-1 loop
            writeBackPixel (X,Y,Scene(Y,X));
         end loop;
      end loop;
   end;
'''
draw = '''
   procedure drawCursorOverlay is
   begin
      for Y in 0 .. cursorRect.h-1 loop
         for X in 0 .. cursorRect.w-1 loop
            cursorSave (Y*2+X) := Targets(backBufferAddr)(cursorRect.y+Y,cursorRect.x+X);
            writeBackPixel (cursorRect.x+X,cursorRect.y+Y,255);
         end loop;
      end loop;
      cursorSaveRect := cursorRect; cursorSaveValid := True;
      flushBackBufferRect (cursorRect);
   end;
'''
suffix = '''
   P : Output_Presentation renames presentations (0);
   T, Held, Next_Writer : BP.Ticket;
   Expected : Image;
   procedure Check_And_Present (Step : Natural) is
      B : constant D.Box := D.Bounds (P.Damage);
   begin
      Expected := Scene;
      for Y in cursorRect.y .. cursorRect.y+cursorRect.h-1 loop
         for X in cursorRect.x .. cursorRect.x+cursorRect.w-1 loop
            Expected(Y,X) := 255;
         end loop;
      end loop;
      pragma Assert (Targets(P.Buffer) = Expected, "target repair failed at frame" & Step'Image);
      -- Display receives only the submitted envelope, not all target pixels.
      pragma Assert (D.Valid(B));
      for Y in B.Top .. B.Bottom-1 loop
         for X in B.Left .. B.Right-1 loop Scanout(Y,X) := Targets(P.Buffer)(Y,X); end loop;
      end loop;
      pragma Assert (Scanout = Expected, "stale scanout cursor at frame" & Step'Image);
      BP.Start_Render(P.Pool,BP.Writer(P.Pool));
      BP.Finish_Render(P.Pool,BP.Writer(P.Pool),BP.Completed);
      BP.Present(P.Pool,Held); pragma Assert(Held /= BP.None);
      D.Clear(P.Damage);
      BP.Acquire(P.Pool,Next_Writer); pragma Assert(Next_Writer /= BP.None);
      P.Buffer := Next_Writer.Buffer; backBufferAddr := P.Buffer;
      cursorSaveValid := False;
      BP.Retire_Display(P.Pool,Held,True);
   end;
begin
   BP.Acquire(P.Pool,T); P.Buffer := T.Buffer;
   repairDirectWriter;
   flushBackBufferRect ((0,0,32,32));
   Check_And_Present (0);
   for Step in 1 .. 1000 loop
      cursorRect := ((Step*13) mod 30,(Step*7) mod 30,2,2);
      -- Frequent complete client redraws, cursor-only work, and conservative
      -- drag admission all run the real repair routine with separate scanout.
      framePending := Step mod 2 = 0;
      dragPreviewValid := Step mod 16 = 0;
      frameDamage := (4,4,24,24);
      if framePending then
         for Y in 4..27 loop
            for X in 4..27 loop Scene(Y,X) := Unsigned_32(Step); end loop;
         end loop;
      elsif Step mod 3 = 0 then
         Scene(31,31) := Unsigned_32(Step);
         RP.Invalidate(P.Repaint,(31,31,32,32));
         addOutputDamage(P.Damage,(31,31,1,1));
      end if;
      pragma Assert(RP.Preparation_Required(P.Repaint,BP.Writer(P.Pool).Buffer,True,False));
      repairDirectWriter;
      restoreCursorOverlay;
      if framePending then
         clipRect := frameDamage; clipEnabled := True;
         drawCurrentScene;
         flushBackBufferRect (frameDamage);
         clipEnabled := False;
      end if;
      drawCursorOverlay;
      Check_And_Present (Step);
      -- An idle loop does not create a new presentation.
      pragma Assert (D.Count(P.Damage)=0);
   end loop;
   Ada.Text_IO.Put_Line("CURSOR-REPAIR: PASS 1000 target+scanout frames, imminent redraws, cursor-only/drag work and idle gaps");
end Cursor_Repair_Tests;
'''
repair = routine('repairDirectWriter')
with tempfile.TemporaryDirectory(prefix='cubit-cursor-repair-') as tmp:
    out = Path(tmp)
    for name in ('compositor_damage','compositor_repaint','compositor_pool'):
        for ext in ('ads','adb'):
            p = root / 'userspace/lib/compositor' / f'{name}.{ext}'
            (out/p.name).write_bytes(p.read_bytes())
    (out/'test.gpr').write_text('''project Test is
      for Source_Dirs use ("."); for Object_Dir use "obj";
      for Exec_Dir use "."; for Main use ("cursor_repair_tests.adb");
      package Compiler is
        for Default_Switches ("Ada") use ("-gnat2022","-gnata","-gnato","-O2");
      end Compiler;
    end Test;''')
    expect_defect = '--expect-defect' in sys.argv
    for negative in (('none',) if expect_defect else ('none','display','history')):
        body = repair
        if negative == 'display':
            needle = 'addOutputDamage (P.Damage, cursorSaveRect);'
            assert body.count(needle)==1
            body = body.replace(needle,'null;')
        elif negative == 'history':
            start = body.index('      if not isEmpty (cursorSaveRect) then')
            end = body.index('      end if;',start)+len('      end if;')
            body = body[:start]+'      null;'+body[end:]
        (out/'cursor_repair_tests.adb').write_text(prefix+routine('flushBackBufferRect')+
            routine('restoreCursorOverlay')+draw+body+suffix)
        subprocess.run(['gprbuild','-q','-p','-P',str(out/'test.gpr')],check=True)
        r = subprocess.run([str(out/'cursor_repair_tests')],capture_output=True,text=True)
        if negative != 'none' or expect_defect:
            expected = 'target repair failed' if negative == 'history' else 'stale scanout cursor'
            assert r.returncode != 0 and expected in r.stderr, r
            print('CURSOR-REPAIR: rejected '+negative+' defect:',r.stderr.strip())
        else:
            assert r.returncode==0,r.stderr
            print(r.stdout,end='')
