"""Exercise actual Desktop drains and their call order with real SPARK policy.

Only clock, input/request transport, handlers and diagnostic dependencies are
mocked. This is not a native scheduling or handler execution-time measurement.
"""
from pathlib import Path
import re
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = (root / 'userspace/services/desktop/main.adb').read_text()
def extract(start, end):
    a = source.index(start)
    return source[a:source.index(end, a) + len(end)]
clock = extract('   function dispatchNow ', '   end dispatchNow;')
drains = extract('         procedure Drain_Events is', '         end Drain_Requests;')
turn = source[source.index('         collectPresentations;', source.index('   while running loop')):]
turn = turn[:turn.index('         refreshStatus;')]
order = re.findall(r'^\s*(Drain_Events|Drain_Requests);$', turn, re.M)
assert order == ['Drain_Events', 'Drain_Requests', 'Drain_Events'], order
prefix = '''with Ada.Text_IO; with Interfaces; use Interfaces;
with Compositor_Dispatch_Budget;
procedure Dispatch_Integration is
   package DB renames Compositor_Dispatch_Budget;
   use type DB.Tick;
   subtype Message is Natural;
   Fake_Now : Unsigned_64 := 100;
   Clock_Available : Boolean := True;
   Event_Cost, Request_Cost : Unsigned_64 := 0;
   Events_Left, Requests_Left, Event_Calls, Request_Calls : Natural := 0;
   Inject, Pending_At, Stop_At : Natural := 0;
   running, framePending : Boolean := True;
   found, eventFound : Boolean := False;
   eventMsg, from, msg : Natural := 0;
   inputBatch : DB.Input_Batch;
   package CuBit is
      package Monotonic is
         type Reading (Available : Boolean := False) is record
            case Available is
               when True => Microseconds : Unsigned_64;
               when False => null;
            end case;
         end record;
         function Read return Reading;
      end Monotonic;
   end CuBit;
   package body CuBit is
      package body Monotonic is
         function Read return Reading is
           (if Clock_Available then (True, Fake_Now) else (Available=>False));
      end Monotonic;
   end CuBit;
   package Desktop_Timing_Policy is Enabled : constant Boolean := False; end;
   package Presentation_Test_Policy is Enabled : constant Boolean := False; end;
   package CR is
      function Busy (S : Natural) return Boolean is (False);
      function Token (S : Natural) return Unsigned_64 is (0);
   end;
   package CP is
      type Phase is (Ready, In_Flight);
      function Current (S : Natural) return Phase is (Ready);
      function Token (S : Natural) return Unsigned_64 is (0);
   end;
   use type CP.Phase;
   type Presentation is record Transfer : Natural := 0; end record;
   presentations : array (0..0) of Presentation;
   primaryOutput, launchRequest : Natural := 0;
   launchInputAnnounced : Boolean := False;
   LF : constant String := (1=>ASCII.LF);
   function Decimal (V : Unsigned_64) return String is (V'Image);
   procedure debugPrint (V : String) is null;
   type Timing_Stage is (Input_Dispatch, Request_Dispatch);
   function timingNow return Unsigned_64 is (Fake_Now);
   procedure noteTiming (Stage : Timing_Stage; First : Unsigned_64) is null;
   function Poll_Event (M : out Message) return Boolean is
   begin
      M:=1;
      if Events_Left=0 then return False; end if;
      Events_Left:=Events_Left-1; return True;
   end;
   procedure Poll_Service_Request (F,M : out Natural; Have : out Boolean) is
   begin
      F:=1; M:=1; Have:=Requests_Left>0;
      if Have then Requests_Left:=Requests_Left-1; end if;
   end;
   procedure handleEvent (M : Message; Continue : in out Boolean) is
   begin Event_Calls:=Event_Calls+1; Fake_Now:=Fake_Now+Event_Cost; end;
   procedure handleRequest (F,M : Natural) is
   begin
      Request_Calls:=Request_Calls+1; Fake_Now:=Fake_Now+Request_Cost;
      if Request_Calls=1 then Events_Left:=Events_Left+Inject; end if;
      if Request_Calls=Pending_At then framePending:=True; end if;
      if Request_Calls=Stop_At then running:=False; end if;
   end;
'''
suffix = '''
   procedure Scenario
     (Input_N,Request_N : Natural; Input_Us,Request_Us : Unsigned_64;
      Pending,Clock_OK : Boolean; New_Events,New_Frame,Stop : Natural;
      Expected_Events,Expected_Requests : Natural; May_Sleep : Boolean)
   is
   begin
      Fake_Now:=100; Clock_Available:=Clock_OK;
      Events_Left:=Input_N; Requests_Left:=Request_N;
      Event_Calls:=0; Request_Calls:=0; Event_Cost:=Input_Us; Request_Cost:=Request_Us;
      Inject:=New_Events; Pending_At:=New_Frame; Stop_At:=Stop;
      running:=True; framePending:=Pending; found:=False; eventFound:=False;
      inputBatch:=DB.New_Input;
      CALL_ORDER
      pragma Assert(Event_Calls=Expected_Events and Request_Calls=Expected_Requests);
      pragma Assert(DB.Events(inputBatch)=Event_Calls and DB.Phases(inputBatch)=2);
      pragma Assert((not eventFound and not found)=May_Sleep);
      pragma Assert(running=(Stop=0));
   end;
begin
   for Cycle in 1..1000 loop
      Scenario(1000,1000,0,0,True,True,0,0,0,64,32,False);
      Scenario(1000,1000,0,0,False,True,0,0,0,64,96,False);
      Scenario(1000,1000,100,250,False,True,0,0,0,10,4,False);
      Scenario(0,1000,100,250,False,True,4,0,0,4,4,False);
      Scenario(0,0,0,0,False,True,0,0,0,0,0,True);
      Scenario(100,100,0,0,False,False,0,0,0,2,1,False);
      Scenario(100,100,0,0,False,True,0,40,0,64,40,False);
      Scenario(100,100,100,250,False,True,0,0,1,5,1,False);
   end loop;
   Ada.Text_IO.Put_Line("DISPATCH-INTEGRATION: PASS 8000 actual-drain scenarios, fresh post-request arrivals and sleep guards");
end Dispatch_Integration;
'''
with tempfile.TemporaryDirectory(prefix='cubit-dispatch-integration-') as tmp:
    out = Path(tmp)
    for ext in ('ads', 'adb'):
        p = root / 'userspace/lib/compositor' / ('compositor_dispatch_budget.' + ext)
        (out / p.name).write_bytes(p.read_bytes())
    (out / 'test.gpr').write_text('''project Test is
       for Source_Dirs use ("."); for Object_Dir use "obj";
       for Exec_Dir use "."; for Main use ("dispatch_integration.adb");
       package Compiler is
          for Default_Switches ("Ada") use ("-gnat2022","-gnata","-gnato","-O2");
       end Compiler;
    end Test;''')
    for mutation in ('none', 'reset_event_count', 'ignore_pending_frame', 'omit_fresh_drain'):
        body, calls = drains, order[:]
        if mutation == 'reset_event_count':
            body = body.replace('DB.Begin_Input_Phase (inputBatch, dispatchNow);',
                                'inputBatch:=DB.New_Input; DB.Begin_Input_Phase (inputBatch, dispatchNow);')
        elif mutation == 'ignore_pending_frame':
            body = body.replace('DB.Can_Request (Batch, dispatchNow, framePending)',
                                'DB.Can_Request (Batch, dispatchNow, False)')
        elif mutation == 'omit_fresh_drain':
            calls.pop()
        (out / 'dispatch_integration.adb').write_text(prefix + clock + body +
            suffix.replace('CALL_ORDER', '\n'.join(name + ';' for name in calls)))
        subprocess.run(['gprbuild', '-f', '-q', '-p', '-P', str(out / 'test.gpr')], check=True)
        result = subprocess.run([str(out / 'dispatch_integration')], capture_output=True, text=True)
        if mutation == 'none':
            assert result.returncode == 0, result.stderr
            print(result.stdout, end='')
        else:
            assert result.returncode != 0 and 'ASSERTION_ERROR' in result.stderr, result
            print('DISPATCH-INTEGRATION: rejected ' + mutation)
