from pathlib import Path
import subprocess
import hashlib
import json
import tempfile

# Extract the production routine; mock only its external dependencies.
root = Path(__file__).resolve().parents[2]
output_root = root / 'tests/intel-gpu/build'
output_root.mkdir(parents=True, exist_ok=True)
work = Path(tempfile.mkdtemp(prefix='teardown-poll-', dir=output_root))
source = root / 'userspace/services/intel-gpu/main.adb'
original = source.read_bytes()
text = original.decode()
start = text.index('   Next_Teardown_Buffer :')
end = text.index('   end Poll_Teardown_Buffers;', start) + len('   end Poll_Teardown_Buffers;')
body = text[start:end]
prefix = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
procedure Poll_Test is
   Fault, Reads, Starts, Cancels, Finishes : Natural := 0;
   Last_Slot : Positive := 1;
   Buffer_Retirement_Pending, Buffer_Retirement_Session : Unsigned_64 := 0;
   Buffer_Retirement_Has_Reply, Buffer_Retirement_Is_Private,
     Buffer_Retirement_Is_Context, Buffer_Retirement_Is_Closed_Table : Boolean := True;
   Buffer_Retirement_Is_Teardown : Boolean := False;
   Application_Buffer_State, Buffer_Pool : Boolean := True;
   package Intel_GPU_Buffer_Backing is subtype Slot is Positive range 1 .. 4; end;
   package Application_Buffers is
      type Closed_Allocation is record
         Ready : Boolean := False;
         ID, Session : Unsigned_64 := 0;
         Generation : Unsigned_32 := 0;
      end record;
      function Closed_At (Object : Boolean; Slot : Positive) return Closed_Allocation;
      function Committed_Slots (Object : Boolean) return Positive is (4);
   end;
   package body Application_Buffers is
      function Closed_At (Object : Boolean; Slot : Positive) return Closed_Allocation is
      begin
         Reads := Reads + 1; Last_Slot := Slot;
         return (Fault /= 1, 100 + Unsigned_64 (Slot), 101, 7);
      end;
   end;
   function Application_Work_Drained (Session : Unsigned_64) return Boolean is
     (Fault /= 2 and Buffer_Retirement_Pending = 0 and Session = 101);
   function Teardown_Buffer_Owner (Session, Ticket : Unsigned_64) return Boolean is
     (Fault /= 3 and Session = 101 and Ticket = 100 + Unsigned_64 (Last_Slot));
   package Buffer_Memory is
      procedure Retire (Object : in out Boolean; Slot : Positive; Generation : Unsigned_32;
        Evidence : Boolean; Started : out Boolean);
      procedure Cancel (Object : in out Boolean);
   end;
   package body Buffer_Memory is
      procedure Retire (Object : in out Boolean; Slot : Positive; Generation : Unsigned_32;
        Evidence : Boolean; Started : out Boolean) is
      begin
         pragma Assert (Slot = Last_Slot and Generation = 7 and Evidence);
         pragma Assert (Buffer_Retirement_Pending = 100 + Unsigned_64 (Slot));
         pragma Assert (Buffer_Retirement_Session = 101 and Buffer_Retirement_Is_Teardown);
         pragma Assert (not Buffer_Retirement_Has_Reply and not Buffer_Retirement_Is_Private
           and not Buffer_Retirement_Is_Context and not Buffer_Retirement_Is_Closed_Table);
         Starts := Starts + 1; Started := Fault /= 4;
      end;
      procedure Cancel (Object : in out Boolean) is
      begin Cancels := Cancels + 1; end;
   end;
   procedure Finish_Teardown_Buffer_Retirement is
   begin
      pragma Assert (Fault = 4 and Cancels = 1 and Starts = 1);
      Finishes := Finishes + 1; Buffer_Retirement_Pending := 0;
   end;
'''
suffix = '''
begin
   for Case_ID in 0 .. 5 loop
      Fault := Case_ID; Next_Teardown_Buffer := 1;
      for Tick in 1 .. 12 loop
         Reads := 0; Starts := 0; Cancels := 0; Finishes := 0;
         Buffer_Retirement_Pending := (if Fault = 5 then 999 else 0);
         Buffer_Retirement_Has_Reply := True;
         Buffer_Retirement_Is_Private := True;
         Buffer_Retirement_Is_Context := True;
         Buffer_Retirement_Is_Closed_Table := True;
         Buffer_Retirement_Is_Teardown := False;
         Poll_Teardown_Buffers;
         pragma Assert (Reads = 1 and Last_Slot = 1 + (Tick - 1) mod 4);
         pragma Assert (Next_Teardown_Buffer = 1 + Tick mod 4);
         pragma Assert (Starts = (if Fault in 0 | 4 then 1 else 0));
         pragma Assert (Cancels = (if Fault = 4 then 1 else 0) and Finishes = Cancels);
         if Fault = 5 then pragma Assert (Buffer_Retirement_Pending = 999); end if;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Teardown poll PASS72: one slot per tick, wrap, no replacement of pending ticket, failed start completes safely");
end Poll_Test;
'''
(work/'poll_test.adb').write_text(prefix+body+suffix)
(work/'poll.gpr').write_text('''project Poll is
 for Source_Dirs use (".");
 for Object_Dir use "poll-obj";
 for Main use ("poll_test.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
 end Compiler;
end Poll;
''')
subprocess.run(['gprbuild','-p','-P',str(work/'poll.gpr')],check=True)
result=subprocess.run([str(work/'poll-obj/poll_test')],text=True,capture_output=True)
assert result.returncode==0,(result.stdout,result.stderr)
assert source.read_bytes()==original
(work/'poll-result.json').write_text(json.dumps({'source_sha256':hashlib.sha256(original).hexdigest(),'stdout':result.stdout}))
print(result.stdout)
