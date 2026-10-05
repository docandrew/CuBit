from pathlib import Path
import subprocess
import hashlib
import json
import tempfile

# Extract the production routine; mock only its external dependencies.
root = Path(__file__).resolve().parents[2]
output_root = root / 'tests/intel-gpu/build'
output_root.mkdir(parents=True, exist_ok=True)
work = Path(tempfile.mkdtemp(prefix='teardown-completion-', dir=output_root))
source = root / 'userspace/services/intel-gpu/main.adb'
original = source.read_bytes()
text = original.decode()
start = text.index('   procedure Finish_Teardown_Buffer_Retirement is')
end = text.index('   end Finish_Teardown_Buffer_Retirement;', start) + len('   end Finish_Teardown_Buffer_Retirement;')
body = text[start:end]
prefix = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
procedure Completion_Test is
   Fault, Steps, Cancels, Quarantines : Natural := 0;
   Runtime_Fault : Boolean := False;
   Buffer_Retirement_Pending : Unsigned_64 := 55;
   Buffer_Retirement_Session : Unsigned_64 := 101;
   Buffer_Retirement_Is_Teardown : Boolean := True;
   Application_Buffer_State, Buffer_Pool : Boolean := True;
   function Teardown_Buffer_Owner (Session, Ticket : Unsigned_64) return Boolean is
     (Session = 101 and Ticket = 55 and Fault /= 1);
   package CuBit is
      package Log_Records is type Severity is (Debug, Error); end Log_Records;
   end CuBit;
   package Application_Buffers is
      function Ticket_Slot (Ticket : Unsigned_64) return Natural is (3);
      function Ticket_Generation (Ticket : Unsigned_64) return Unsigned_32 is (7);
      procedure Acknowledge_Retirement (Object : in out Boolean; Session, Ticket : Unsigned_64;
        Evidence : Boolean; Accepted : out Boolean);
      procedure Quarantine (Object : in out Boolean);
   end Application_Buffers;
   package body Application_Buffers is
      procedure Acknowledge_Retirement (Object : in out Boolean; Session, Ticket : Unsigned_64;
        Evidence : Boolean; Accepted : out Boolean) is
      begin
         pragma Assert (Steps = 1 and Session = 101 and Ticket = 55 and Evidence);
         Steps := 2; Accepted := Fault /= 3;
      end Acknowledge_Retirement;
      procedure Quarantine (Object : in out Boolean) is
      begin Quarantines := Quarantines + 1; end Quarantine;
   end Application_Buffers;
   package Buffer_Memory is
      function Retirement_Confirmed (Object : Boolean; Slot : Natural; Generation : Unsigned_32) return Boolean;
      procedure Cancel (Object : in out Boolean);
   end Buffer_Memory;
   package body Buffer_Memory is
      function Retirement_Confirmed (Object : Boolean; Slot : Natural; Generation : Unsigned_32) return Boolean is
      begin
         pragma Assert (Steps = 0 and Slot = 3 and Generation = 7);
         Steps := 1; return Fault /= 2;
      end Retirement_Confirmed;
      procedure Cancel (Object : in out Boolean) is
      begin Cancels := Cancels + 1; end Cancel;
   end Buffer_Memory;
   procedure Publish_Snapshot (Text : String; Level : CuBit.Log_Records.Severity) is null;
'''
suffix = '''
begin
   for Case_ID in 0 .. 5 loop
      Fault := Case_ID; Steps := 0; Cancels := 0; Quarantines := 0;
      Runtime_Fault := False; Buffer_Retirement_Is_Teardown := True;
      Buffer_Retirement_Session := (if Fault = 4 then 102 else 101);
      Buffer_Retirement_Pending := (if Fault = 5 then 56 else 55);
      Finish_Teardown_Buffer_Retirement;
      pragma Assert (Buffer_Retirement_Pending = 0 and not Buffer_Retirement_Is_Teardown);
      pragma Assert (Runtime_Fault = (Fault /= 0));
      pragma Assert (Cancels = (if Fault = 0 then 0 else 1) and Quarantines = Cancels);
      pragma Assert (Steps = (if Fault = 0 or Fault = 3 then 2 elsif Fault = 2 then 1 else 0));
   end loop;
   Ada.Text_IO.Put_Line ("Teardown completion PASS6: exact receipt before metadata reuse; owner/ack failure quarantined");
end Completion_Test;
'''
(work/'completion_test.adb').write_text(prefix+body+suffix)
(work/'completion.gpr').write_text('''project Completion is
 for Source_Dirs use (".");
 for Object_Dir use "completion-obj";
 for Main use ("completion_test.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
 end Compiler;
end Completion;
''')
subprocess.run(['gprbuild','-p','-P',str(work/'completion.gpr')],check=True)
result=subprocess.run([str(work/'completion-obj/completion_test')],text=True,capture_output=True)
assert result.returncode==0,(result.stdout,result.stderr)
assert source.read_bytes()==original
(work/'completion-result.json').write_text(json.dumps({'source_sha256':hashlib.sha256(original).hexdigest(),'stdout':result.stdout}))
print(result.stdout)
