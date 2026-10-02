"""Actual Desktop enqueue/dequeue/has-input glue with isolated surface channels.

Waiter notification and surface admission are mocked. This is not native IPC,
client rendering throughput, or kernel input-authority validation.
"""
from pathlib import Path
import subprocess
import tempfile

root=Path(__file__).resolve().parents[2]
source=(root/'userspace/services/desktop/main.adb').read_text()

def routine(kind,name,after=0):
    start=source.index('   '+kind+' '+name+'\n',after)
    end=source.index('   end '+name+';',start)+len('   end '+name+';')
    return source[start:end]

constants=source[source.index('   INPUT_NONE '):source.index('   KEYMOD_SHIFT ')]
append=routine('procedure','enqueueInput',source.index('   end queueConfigure;'))
read=routine('procedure','dequeueInput',source.index('   end enqueueInput;',source.index('   end queueConfigure;')))
pending=routine('function','hasInputAfter')
a=source.index('   procedure requestClose (target : Unsigned_64) is')
b=source.index('   end requestClose;',a)+len('   end requestClose;')
close=source[a:b]
prefix='''with Ada.Text_IO; with Interfaces; use Interfaces;
with Compositor_Input_Queue; with Compositor_Input_Trace;
with Compositor_Close_Request; with CuBit.Desktop_Protocol;
procedure Input_Integration is
   package IQ renames Compositor_Input_Queue;
   package Close_Policy renames Compositor_Close_Request;
   package DP renames CuBit.Desktop_Protocol;
   package IT renames Compositor_Input_Trace;
   use type IT.Record_Value;
   inputDequeueTrace : IT.State;
   package Desktop_Timing_Policy is Enabled : Boolean := False; end;
   Trace_Clock : Unsigned_64 := 0;
   Trace_Reads : Natural := 0;
   function timingNow return Unsigned_64 is
   begin Trace_Reads:=Trace_Reads+1; return Trace_Clock; end;
   use type IQ.Queue, IQ.Event;
   subtype PendingInput is IQ.Event;
   subtype InputQueueIndex is IQ.Index;
   subtype PendingInputQueue is IQ.Queue;
   subtype SurfaceIndex is Natural range 0 .. 1;
   type InputSnapshot is record
      pointerPosition, buttons, modifiers, generation : Unsigned_64 := 0;
   end record;
   type Channel is record
      events : PendingInputQueue;
      snapshot : InputSnapshot;
      nextSerial : Unsigned_64 := 1;
      pendingClose : Unsigned_64 := 0;
   end record;
   subtype SurfaceInputChannel is Channel;
   inputChannels : array (SurfaceIndex) of Channel;
   inputQueueOverflows : Unsigned_64 := 0;
   Notifications : Natural := 0;
   Fatal : exception;
   LF : constant String := (1 => ASCII.LF);
   procedure debugPrint (S : String) is null;
   procedure exitCompositor (Status : Integer) is
   begin raise Fatal; end;
   function findInputChannel (Target : Unsigned_64) return Integer is
     (if Target=11 then 0 elsif Target=22 then 1 else -1);
   procedure ensureInputChannel (Target : Unsigned_64; Slot : out Integer) is
   begin Slot := findInputChannel(Target); end;
   procedure completeInputWaiter (Target : Unsigned_64) is
   begin Notifications := Notifications+1; end;
'''
suffix='''
   E : PendingInput;
   Found : Boolean;
   procedure Reset is
   begin
      inputChannels := (others => (others => <>));
      inputQueueOverflows := 0; Notifications := 0;
   end;
begin
   for Cycle in 1 .. 1000 loop
      Reset;
      enqueueInput(INPUT_POINTER_MOVE,11,10,0);
      enqueueInput(INPUT_POINTER_MOVE,11,20,0);
      enqueueInput(INPUT_KEY_DOWN,11,42,4);
      enqueueInput(INPUT_KEY_UP,11,42,0);
      enqueueInput(INPUT_POINTER_MOVE,11,30,0);
      pragma Assert(Notifications=5 and inputChannels(0).nextSerial=5);
      dequeueInput(11,0,Found,E);
      pragma Assert(Found and E.serial=1 and E.payload0=20);
      dequeueInput(11,E.serial,Found,E);
      pragma Assert(Found and E.serial=2 and E.kind=INPUT_KEY_DOWN);
      dequeueInput(11,E.serial,Found,E);
      pragma Assert(Found and E.serial=3 and E.kind=INPUT_KEY_UP);
      dequeueInput(11,E.serial,Found,E);
      pragma Assert(Found and E.serial=4 and E.payload0=30);
      pragma Assert(not hasInputAfter(11,4));
      -- A stalled surface cannot consume another surface's queue capacity.
      Reset;
      enqueueInput(INPUT_TEXT,22,99,0);
      declare Other : constant Channel := inputChannels(1); begin
         enqueueInput(INPUT_POINTER_DOWN,11,100,1);
         enqueueInput(INPUT_KEY_DOWN,11,42,4);
         for I in 1 .. 30 loop enqueueInput(INPUT_TEXT,11,Unsigned_64(I),0); end loop;
         enqueueInput(INPUT_POINTER_UP,11,700,0);
         pragma Assert(inputQueueOverflows=1 and inputChannels(0).snapshot.generation=1);
         pragma Assert(inputChannels(1)=Other);
         dequeueInput(11,0,Found,E);
         pragma Assert(Found and E.kind=INPUT_RESYNC and E.serial=33 and E.target=11);
         pragma Assert(E.payload0=700 and E.payload1=Shift_Left(Unsigned_64'(4),32));
         pragma Assert(not hasInputAfter(11,E.serial));
      end;
      enqueueInput(INPUT_TEXT,11,98,0);
      dequeueInput(11,34,Found,E);
      pragma Assert(not Found and not hasInputAfter(11,0));
      dequeueInput(22,0,Found,E);
      pragma Assert(Found and E.kind=INPUT_TEXT and E.payload0=99);
      -- Counter saturation does not turn a known loss into zero.
      Reset; inputQueueOverflows:=Unsigned_64'Last;
      inputChannels(0).snapshot.generation:=Unsigned_64'Last;
      for I in 1 .. 33 loop enqueueInput(INPUT_TEXT,11,1,0); end loop;
      pragma Assert(inputQueueOverflows=Unsigned_64'Last and
        inputChannels(0).snapshot.generation=Unsigned_64'Last);
   end loop;
   Reset;
   enqueueInput(INPUT_TEXT,0,1,0);
   pragma Assert(Notifications=0);
   inputChannels(0).nextSerial:=Unsigned_64'Last;
   begin
      enqueueInput(INPUT_TEXT,11,1,0);
      raise Program_Error with "accepted exhausted serial";
   exception when Fatal => null; end;
   pragma Assert(Notifications=0 and not hasInputAfter(11,0));
   pragma Assert(Trace_Reads=0 and IT.Count(inputDequeueTrace)=0);
   -- A retained close is a motion-coalescing barrier, even though it is
   -- deliberately stored outside the overflow-prone ordinary queue.
   Reset;
   enqueueInput(INPUT_POINTER_MOVE,11,10,0);
   requestClose(11);
   enqueueInput(INPUT_POINTER_MOVE,11,20,0);
   enqueueInput(INPUT_POINTER_MOVE,11,30,0);
   pragma Assert(inputChannels(0).nextSerial=4);
   dequeueInput(11,0,Found,E);
   pragma Assert(Found and E.serial=1 and E.payload0=10);
   dequeueInput(11,1,Found,E);
   pragma Assert(Found and E.serial=2 and E.kind=10);
   dequeueInput(11,2,Found,E);
   pragma Assert(Found and E.serial=3 and E.payload0=30);
   -- Close redelivery is stable until acknowledgment; overflow cannot erase
   -- it and another channel remains unchanged.
   Reset;
   enqueueInput(INPUT_TEXT,22,99,0);
   requestClose(11);
   for Click in 1 .. 100 loop requestClose(11); end loop;
   pragma Assert(inputChannels(0).nextSerial=2);
   for I in 1 .. 1000 loop enqueueInput(INPUT_TEXT,11,42,0); end loop;
   pragma Assert(hasInputAfter(11,0));
   dequeueInput(11,0,Found,E);
   pragma Assert(Found and E.serial=1 and E.kind=10 and E.target=11);
   dequeueInput(11,0,Found,E);
   pragma Assert(Found and E.serial=1 and E.kind=10);
   dequeueInput(22,0,Found,E);
   pragma Assert(Found and E.serial=1 and E.kind=INPUT_TEXT and E.target=22);
   dequeueInput(11,1,Found,E);
   pragma Assert(Found and E.serial>1 and E.kind/=10);
   pragma Assert(inputChannels(0).pendingClose=0);
   requestClose(99);
   pragma Assert(not hasInputAfter(99,0));
   Reset; Desktop_Timing_Policy.Enabled:=True;
   for N in 1 .. 80 loop
      Trace_Clock:=Unsigned_64(N);
      enqueueInput(INPUT_TEXT,11,42,0);
      dequeueInput(11,0,Found,E);
      pragma Assert(Found);
      if N<=64 then
         pragma Assert(IT.Item(inputDequeueTrace,N)=
           IT.Record_Value'(11,Unsigned_64(N),INPUT_TEXT,Unsigned_64(N)));
      end if;
   end loop;
   pragma Assert(Trace_Reads=80 and IT.Count(inputDequeueTrace)=64 and
                 IT.Lost(inputDequeueTrace)=16);
   dequeueInput(11,0,Found,E); pragma Assert(not Found and Trace_Reads=80);
   dequeueInput(99,0,Found,E); pragma Assert(not Found and Trace_Reads=80);
   IT.Reset(inputDequeueTrace); Trace_Clock:=Unsigned_64'Last;
   enqueueInput(INPUT_TEXT,11,42,0); dequeueInput(11,0,Found,E);
   pragma Assert(Found and Trace_Reads=81 and IT.Invalid(inputDequeueTrace)=1);
   pragma Assert(IT.Count(inputDequeueTrace)=0);
   Reset; IT.Reset(inputDequeueTrace); Trace_Clock:=100;
   requestClose(11); dequeueInput(11,0,Found,E);
   pragma Assert(Found and E.kind=10);
   Trace_Clock:=101; dequeueInput(11,0,Found,E);
   pragma Assert(Found and E.kind=10 and IT.Count(inputDequeueTrace)=2);
   pragma Assert(IT.Item(inputDequeueTrace,1).Serial=IT.Item(inputDequeueTrace,2).Serial);
   pragma Assert(IT.Item(inputDequeueTrace,1).Dequeued=100 and
                 IT.Item(inputDequeueTrace,2).Dequeued=101);
   Ada.Text_IO.Put_Line("INPUT-INTEGRATION: PASS 1000 ordering/overflow/isolation/ack cycles, saturation and exhaustion");
end Input_Integration;
'''
with tempfile.TemporaryDirectory(prefix='cubit-input-integration-') as tmp:
    out=Path(tmp)
    for name in ('compositor_input_queue.ads','compositor_input_queue.adb',
                 'compositor_input_trace.ads','compositor_input_trace.adb','compositor_elapsed.ads',
                 'compositor_close_request.ads','compositor_close_request.adb'):
        p=root/'userspace/lib/compositor'/name
        (out/p.name).write_bytes(p.read_bytes())
    for name in ('cubit.ads','cubit-grant_references.ads','cubit-desktop_protocol.ads','cubit-desktop_protocol.adb'):
        (out/name).write_bytes((root/'userspace/runtime/gnat'/name).read_bytes())
    (out/'test.gpr').write_text('''project Test is
      for Source_Dirs use ("."); for Object_Dir use "obj";
      for Exec_Dir use "."; for Main use ("input_integration.adb");
      package Compiler is
        for Default_Switches ("Ada") use ("-gnat2022","-gnata","-gnato","-O2");
      end Compiler;
    end Test;''')
    for negative in (False,True):
        body=append
        if negative:
            needle='             then INPUT_POINTER_MOVE else INPUT_NONE), Result);'
            assert body.count(needle)==1
            body=body.replace(needle,'             then INPUT_KEY_DOWN else INPUT_NONE), Result);')
        (out/'input_integration.adb').write_text(prefix+constants+close+body+read+pending+suffix)
        subprocess.run(['gprbuild','-q','-p','-P',str(out/'test.gpr')],check=True)
        r=subprocess.run([str(out/'input_integration')],capture_output=True,text=True)
        if negative:
            assert r.returncode!=0 and 'ASSERTION_ERROR' in r.stderr,r
            print('INPUT-INTEGRATION: rejected incorrect coalescing kind mutation')
        else:
            assert r.returncode==0,r.stderr
            print(r.stdout,end='')
