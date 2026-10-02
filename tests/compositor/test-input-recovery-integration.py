"""Actual Desktop forced-recovery and close glue with mocked geometry/replies.

The queue policy is real; kernel reply capability semantics remain a native gate.
"""
from pathlib import Path
import subprocess
import tempfile
root=Path(__file__).resolve().parents[2]
s=(root/'userspace/services/desktop/main.adb').read_text()
def procedure(name):
    a=s.index('   procedure '+name+' ',s.index('   procedure forceInputResynchronization is'))
    b=s.index('   end '+name+';',a)+len('   end '+name+';')
    return s[a:b]
constants=s[s.index('   INPUT_NONE '):s.index('   KEYMOD_SHIFT ')]
prefix='''with Ada.Text_IO; with Interfaces; use Interfaces;
with Compositor_Input_Queue;
procedure Recovery_Integration is
   package IQ renames Compositor_Input_Queue;
   use type IQ.Queue;
   subtype SurfaceIndex is Natural range 0 .. 1;
   subtype CapabilitySlot is Unsigned_64;
   subtype ProcessID is Unsigned_64;
   NO_PROCESS : constant ProcessID := 0;
   subtype PendingInputQueue is IQ.Queue;
   type InputSnapshot is record
      pointerPosition,buttons,modifiers,generation : Unsigned_64 := 0;
   end record;
   type InputWaiter is record
      active : Boolean := False;
      owner : ProcessID := 0;
      target,afterSerial,deadline : Unsigned_64 := 0;
      replySlot : CapabilitySlot := 32;
   end record;
   type Channel is record
      target : Unsigned_64 := 0;
      nextSerial : Unsigned_64 := 1;
      pendingClose : Unsigned_64 := 0;
      events : PendingInputQueue;
      snapshot : InputSnapshot;
      waiter : InputWaiter;
   end record;
   inputChannels : array(SurfaceIndex) of Channel;
   type Tag is record label,length,flags,reserved : Unsigned_64 := 0; end record;
   type Words is array(0..3) of Unsigned_64;
   type Message is record tag : Recovery_Integration.Tag; words : Recovery_Integration.Words := (others=>0); end record;
   NULL_MESSAGE : constant Message := (others=><>);
   Last_Reply : Message;
   Replies,Notifications : Natural := 0;
   OP_INPUT_WAIT : constant Unsigned_64 := 42;
   KEYMOD_SHIFT : constant Unsigned_64 := 1;
   KEYMOD_CTRL : constant Unsigned_64 := 2;
   KEYMOD_ALT : constant Unsigned_64 := 4;
   KEYMOD_CAPS : constant Unsigned_64 := 8;
   desktopShiftDown,desktopCtrlDown,desktopCapsLockOn : Boolean := True;
   desktopAltDown : Boolean := False;
   desktopExtendedPrefix,dragPreviewValid,dragPresentedValid : Boolean := True;
   pointerSurfaceId,dragSurfaceId,dragMode : Natural := 1;
   DRAG_NONE : constant := 0;
   cursorX : Natural := 400;
   cursorY : Natural := 500;
   lastButtons : Unsigned_64 := 5;
   titleClicks : Natural := 1;
   package CuBit is
      package Click_Sequences is
         procedure Reset (V : in out Natural);
      end;
   end;
   package body CuBit is
      package body Click_Sequences is
         procedure Reset(V : in out Natural) is begin V:=0; end;
      end;
   end;
   type Rect is record x,y : Natural; end record;
   surfaces : constant array(SurfaceIndex) of Rect := ((100,200),(10,20));
   function clientRect(S : Rect) return Rect is (S);
   function packU32Pair(X,Y : Natural) return Unsigned_64 is
     (Unsigned_64(X) or Shift_Left(Unsigned_64(Y),32));
   function findInputChannel(T : Unsigned_64) return Integer is
     (if T=11 then 0 elsif T=22 then 1 else -1);
   function findSurface(T : Unsigned_64) return Integer is (findInputChannel(T));
   procedure completeInputWaiter(T : Unsigned_64) is
   begin Notifications:=Notifications+1; end;
   function replyCap(Slot : CapabilitySlot; M : Message) return Unsigned_64 is
   begin
      pragma Assert(not inputChannels(SurfaceIndex(Slot-32)).waiter.active);
      Replies:=Replies+1; Last_Reply:=M; return 0;
   end;
   Fatal : exception;
   LF : constant String := (1=>ASCII.LF);
   procedure debugPrint(T : String) is null;
   procedure exitCompositor(Status : Integer) is begin raise Fatal; end;
'''
suffix='''
   Expected_X,Expected_Y : Natural;
   E : IQ.Event;
   Selected : IQ.Selection;
begin
   for Cycle in 1 .. 1000 loop
      inputChannels := (others=>(others=><>)); Notifications:=0; Replies:=0;
      inputChannels(0).target:=11; inputChannels(1).target:=22;
      for I in SurfaceIndex loop
         inputChannels(I).nextSerial:=17;
         inputChannels(I).pendingClose:=15;
         inputChannels(I).events(31):=(True,16,INPUT_TEXT,inputChannels(I).target,99,0);
      end loop;
      forceInputResynchronization;
      pragma Assert(Notifications=2 and titleClicks=0 and pointerSurfaceId=0 and
        dragSurfaceId=0 and dragMode=0 and not desktopExtendedPrefix and
        not dragPreviewValid and not dragPresentedValid);
      for I in SurfaceIndex loop
         pragma Assert(inputChannels(I).pendingClose=15);
         Expected_X:=cursorX-surfaces(I).x; Expected_Y:=cursorY-surfaces(I).y;
         IQ.Pop(inputChannels(I).events,0,Selected,E);
         pragma Assert(Selected=0 and E.Kind=INPUT_RESYNC and E.Serial=17 and
           E.Target=inputChannels(I).target and E.Payload0=packU32Pair(Expected_X,Expected_Y) and
           E.Payload1=(Unsigned_64'(5) or Shift_Left(Unsigned_64'(11),32)));
         pragma Assert(not IQ.Has_After(inputChannels(I).events,0));
         pragma Assert(inputChannels(I).nextSerial=18 and inputChannels(I).snapshot.generation=1);
      end loop;
      inputChannels(0).waiter:=(active=>True,replySlot=>32,others=><>);
      clearInputForTarget(11);
      pragma Assert(Replies=1 and Last_Reply.words(0)=INPUT_RESYNC and Last_Reply.words(1)=18);
      pragma Assert(inputChannels(0).target=0 and not inputChannels(0).waiter.active
        and inputChannels(0).pendingClose=0 and inputChannels(1).pendingClose=15);
      pragma Assert(inputChannels(1).target=22 and inputChannels(1).nextSerial=18);
      clearInputForTarget(22);
      pragma Assert(Replies=1 and inputChannels(1).target=0);
   end loop;
   inputChannels(0).target:=11; inputChannels(0).nextSerial:=Unsigned_64'Last;
   begin forceInputResynchronization; raise Program_Error;
   exception when Fatal=>null; end;
   pragma Assert(inputChannels(0).nextSerial=Unsigned_64'Last);
   inputChannels(0).waiter:=(active=>True,replySlot=>32,others=><>);
   begin clearInputForTarget(11); raise Program_Error;
   exception when Fatal=>null; end;
   pragma Assert(Replies=1 and inputChannels(0).waiter.active);
   Ada.Text_IO.Put_Line("INPUT-RECOVERY: PASS 1000 actual forced-resync/close cycles, state payloads, isolation and exhaustion");
end Recovery_Integration;
'''
with tempfile.TemporaryDirectory(prefix='cubit-input-recovery-') as tmp:
    out=Path(tmp)
    for ext in ('ads','adb'):
        p=root/'userspace/lib/compositor'/('compositor_input_queue.'+ext)
        (out/p.name).write_bytes(p.read_bytes())
    (out/'recovery_integration.adb').write_text(prefix+constants+procedure('forceInputResynchronization')+procedure('clearInputForTarget')+suffix)
    (out/'test.gpr').write_text('''project Test is
      for Source_Dirs use ("."); for Object_Dir use "obj";
      for Exec_Dir use "."; for Main use ("recovery_integration.adb");
      package Compiler is
        for Default_Switches ("Ada") use ("-gnat2022","-gnata","-gnato","-O2");
      end Compiler;
    end Test;''')
    subprocess.run(['gprbuild','-q','-p','-P',str(out/'test.gpr')],check=True)
    subprocess.run([str(out/'recovery_integration')],check=True)
